package test.task.example

import cats.free.Free
import org.specs2.Specification
import task.example.TaskCompile._
import task.example.TaskEvents.EventStream
import task.example.TaskProjections.{Completed, Open, Task}
import task.example._
import TaskQ.composing._
import TaskC.composing._
import cats.free.Free._
import cats.std.all._

class TaskStateQueryCompileSpec extends Specification { def is = s2"""

 This is a specification to check the Task Behaviour and State Compilers

 The Task State Compiler string should
   read tasks that have been committed to                        $e1
  find all tasks that have been committed to                    $e2
  find completed tasks                                          $e3
                                                                 """



  val statingState = (Map[String, EventStream](), Map[String, Option[TaskProjections.Task]]())

  val program1: Free[CommandOrQuery, Option[Task]] = {
    for {
      _ <- commitToTask("2", "My first Task").lift
      t <- readTask("2").lift
    } yield t
  }

  val r1 = program1.foldMap(XorInterpreter).foldMap(XorStateInterpreter).run(statingState).run

  def e1 = r1._2 must_== Some(Task("2", "My first Task", Open))

  def program2: Free[CommandOrQuery, List[Task]] = {
    for {
      _ <- commitToTask("2", "My first Task").lift
      _ <- commitToTask("3", "My second Task").lift
      tasks <- findAllTasks.lift
    } yield tasks
  }

  val r2 = program2.foldMap(XorInterpreter).foldMap(XorStateInterpreter).run(statingState).run

  def e2 = r2._2 must_== List(Task("2", "My first Task", Open), Task("3", "My second Task", Open))

  def program3: Free[CommandOrQuery, List[Task]] = {
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

  val r3 = program3.foldMap(XorInterpreter).foldMap(XorStateInterpreter).run(statingState).run
  def e3 = r3._2 must_== List(Task("2", "My first Task", Completed))
}
