package task.example

import freetocompose.TransformationOps
import TaskQ.composing._
import TaskC.composing._
import cats.{Monad, Id}
import cats.std.list._
import cats.syntax.foldable._
import cats.std.function._

object TaskExample extends App {

  val compiler = TaskCompile.toStateC || TaskCompile.toStateQ

  def program1[F[_] : TaskC : TaskQ] = {
    for {
      _ <- commitToTask("2", "My first Task")
      t <- readTask("2")
    } yield t
  }

  val lr = program1[compiler.From].foldMap(compiler).run(Map[String, Task]()).run

  println(s"State is ${lr._1}")
  println(s"Result is ${lr._2}")

  def program2[F[_] : TaskC : TaskQ] = {
    for {
      _ <- commitToTask("2", "My first Task")
      _ <- commitToTask("3", "My second Task")
      tasks <- findAllTasks
    } yield tasks
  }

  val lr2 = program2[compiler.From].foldMap(compiler).run(Map[String, Task]()).run

  println(s"State is ${lr2._1}")
  println(s"Result is ${lr2._2}")

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

  val lr3 = program3[compiler.From].foldMap(compiler).run(Map[String, Task]()).run

  println(s"State is ${lr3._1}")
  println(s"Result is ${lr3._2}")
}
