package test.task.example

import cats.free.Free
import cats.free.Free._
import org.specs2.Specification
import task.example.TaskCompile._
import task.example.TaskProjections.{Completed, Open, Task, TaskProjection}
import task.example.DoobieCompile._
import task.example.{TaskC, TaskQ}
import TaskQ.composing._
import TaskC.composing._
import scala.language.higherKinds

class DoobieCompileSpec extends Specification { def is = sequential ^ s2"""

 This is a specification to check the Doobie compiler

 The Doobie Compiler should
   read tasks that have been committed to                        $e1
   find all tasks that have been committed to                    $e2
   find completed tasks                                          $e3
                                                                 """

  val program1: Free[CommandOrQuery, Option[TaskProjection]] = {
    for {
      _ <- commitToTask("2", "My first Task").lift
      t <- readTask("2").lift
    } yield t
  }

  def e1 = {
    dbSetUp.run
    val r1 = program1.foldMap(XorInterpreter).foldMap(XorDoobieInterpreter).run
    r1 must_== Some(Task("2", "My first Task", Open))
  }

  def program2: Free[CommandOrQuery, List[TaskProjection]] = {
    for {
      _ <- commitToTask("2", "My first Task").lift
      _ <- commitToTask("3", "My second Task").lift
      tasks <- findAllTasks.lift
    } yield tasks
  }

  def e2 = {
    dbSetUp.run
    val r2 = program2.foldMap(XorInterpreter).foldMap(XorDoobieInterpreter).run
    r2 must_== List(Task("2", "My first Task", Open), Task("3", "My second Task", Open))
  }

  def program3: Free[CommandOrQuery, List[TaskProjection]] = {
    for {
      _ <- commitToTask("2", "My first Task").lift
      _ <- commitToTask("3", "My second Task").lift
      _ <- completeTask("2").lift
      _ <- completeTask("5").lift
      _ <- reopenTask("2").lift
      _ <- completeTask("2").lift
      tasks <- findClosedTasks.lift
    } yield tasks
  }

  def e3 = {
    dbSetUp.run
    val r3 = program3.foldMap(XorInterpreter).foldMap(XorDoobieInterpreter).run
    r3 must_== List(Task("2", "My first Task", Completed))
  }
}
