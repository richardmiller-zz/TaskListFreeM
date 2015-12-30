package test.task.example

import org.specs2.Specification
import freetocompose.TransformationOps
import task.example._
import TaskQ.composing._
import TaskC.composing._
import cats.std.function._

class TaskStateCompileSpec extends Specification { def is = s2"""

 This is a specification to check the Task State Compiler

 The Task State Compiler string should
   read tasks that have been committed to                        $e1
   find all tasks that have been committed to                    $e2
   find completed tasks                                          $e3
                                                                 """

  val compiler = TaskCompile.toStateC || TaskCompile.toStateQ

  def program1[F[_] : TaskC : TaskQ] = {
    for {
      _ <- commitToTask("2", "My first Task")
      t <- readTask("2")
    } yield t
  }

  val r1 = program1[compiler.From].foldMap(compiler).run(Map[String, Task]()).run

  def e1 = r1._2 must_== Some(Task("2", "My first Task", Open))

  def program2[F[_] : TaskC : TaskQ] = {
    for {
      _ <- commitToTask("2", "My first Task")
      _ <- commitToTask("3", "My second Task")
      tasks <- findAllTasks
    } yield tasks
  }

  val r2 = program2[compiler.From].foldMap(compiler).run(Map[String, Task]()).run

  def e2 = r2._2 must_== List(Task("2", "My first Task", Open), Task("3", "My second Task", Open))

  def program3[F[_] : TaskC : TaskQ] = {
    for {
      _ <- commitToTask("2", "My first Task")
      _ <- commitToTask("3", "My second Task")
      _ <- completeTask("2")
      _ <- completeTask("5")
      _ <- reopenTask("2")
      _ <- completeTask("2")
      tasks <- findClosedTasks
    } yield tasks
  }

  val r3 = program3[compiler.From].foldMap(compiler).run(Map[String, Task]()).run
  def e3 = r3._2 must_== List(Task("2", "My first Task", Completed))
}
