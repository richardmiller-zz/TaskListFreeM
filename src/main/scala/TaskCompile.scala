package task.example

import TaskCommands._
import TaskQueries._
import cats.state._
import cats.~>
import TaskBehaviourC.composing._
import task.example.TaskBehaviours.TaskBehaviour
import task.example.TaskProjections.{Completed, Open, Task}
import scala.language.higherKinds
import cats.free.Free
import cats.data.Xor
import cats.syntax.xor._

object TaskCompile {

  type Tasks = Map[String, Task]

  type MapState[A] = State[Tasks, A]

  type CommandOrQuery[A] = TaskCommand[A] Xor TaskQuery[A]

  type BehaviourOrQuery[A] = Free[TaskBehaviour, A] Xor Free[TaskQuery, A]

  type Result[A] = Free[BehaviourOrQuery, A]

  object toStateC extends (TaskCommand ~> MapState) {

    override def apply[A](fa: TaskCommand[A]) = fa match {
      case CommitToTask(id, text) => State.modify((ts: Tasks) => ts + (id -> Task(id, text, Open))).map(_ => ())
      case CompleteTask(id) => State.modify(updateTask(id, _.copy(status = Completed))).map(_ => ())
      case ReopenTask(id) => State.modify(updateTask(id, _.copy(status = Open))).map(_ => ())
    }

    private def updateTask[A](id: String, f: Task => Task)(ts: Tasks) = {
      ts.get(id).fold(ts)(t => ts + (id -> f(t)))
    }

  }

  object toStateQ extends (TaskQuery ~> MapState) {

    override def apply[A](fa: TaskQuery[A]) = fa match {
      case ReadTask(id) => State.inspect(_.get(id))
      case _: FindAllTasks => State.inspect(asList(_ => true))
      case _: FindOpenTasks => State.inspect(asList(_.status == Open))
      case _: FindClosedTasks => State.inspect(asList(_.status == Completed))
    }

    def asList[A](f: Task => Boolean): (Tasks) => List[Task] = _.map({ case (_, v) => v }).toList.filter(f)
  }

  object toProjectionsStateQ extends (TaskQuery ~> TaskAggCompile.MapState) {

    override def apply[A](fa: TaskQuery[A]) = fa match {
      case ReadTask(id) => State.inspect(_._2.get(id).flatten)
      case _: FindAllTasks => State.inspect(asList(_ => true))
      case _: FindOpenTasks => State.inspect(asList(_.status == Open))
      case _: FindClosedTasks => State.inspect(asList(_.status == Completed))
    }

    def asList[A](f: Task => Boolean): (TaskAggCompile.Tasks) => List[Task] = _._2.flatMap({ case (_, v) => v }).toList.filter(f)
  }

  object identityQ extends (TaskQuery ~> Free[TaskQuery, ?]) {
    override def apply[A](fa: TaskQuery[A]): Free[TaskQuery, A] = Free.liftF(fa)
  }

  object LiftLeft extends (TaskCommand ~> CommandOrQuery) {
    def apply[A](c: TaskCommand[A]): CommandOrQuery[A] = c.left
  }
  object LiftRight extends (TaskQuery ~> CommandOrQuery) {
    def apply[A](q: TaskQuery[A]): CommandOrQuery[A] = q.right
  }

  implicit class LiftLeftOps[A](in: Free[TaskCommand, A]) {
    def lift: Free[CommandOrQuery, A] = in.compile(LiftLeft)
  }
  implicit class LiftRightOps[A](in: Free[TaskQuery, A]) {
    def lift: Free[CommandOrQuery, A] = in.compile(LiftRight)
  }

  def  XorInterpreter = new (CommandOrQuery ~> Result) {
    def apply[A](in: CommandOrQuery[A]) =
      in.fold(
        (c: TaskCommand[A]) => Free.liftF[BehaviourOrQuery, A](toTaskBehaviours(c).left),
        (q: TaskQuery[A]) => Free.liftF[BehaviourOrQuery, A](identityQ(q).right)
      )
  }

  def XorStateInterpreter = new (BehaviourOrQuery ~> TaskAggCompile.MapState) {
    override def apply[A](fa: BehaviourOrQuery[A]) = fa.fold(
      _.foldMap(TaskAggCompile.toState),
      _.foldMap(toProjectionsStateQ)
    )
  }

  def toTaskBehaviours = new (TaskCommand ~> Free[TaskBehaviour, ?]) {

    override def apply[A](fa: TaskCommand[A]) = fa match {
      case CommitToTask(id, text) =>  for {
        t <- commit(id, text)
        _ <- save(t)
        _ <- project(t)
      } yield ()
      case CompleteTask(id) => for {
        t <- find(id)
        t2 <- complete(t)
        _ <- save(t2)
        _ <- project(t2)
      } yield ()
      case ReopenTask(id) => for {
        t <- find(id)
        t2 <- reopen(t)
        _ <- save(t2)
        _ <- project(t2)
      } yield ()
    }

  }
}