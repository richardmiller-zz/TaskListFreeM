package task.example

import TaskCommands._
import TaskQueries._
import cats.state._
import cats.{Functor, ~>}
import TaskBehaviourC.composing._
import monocle.Iso
import monocle.macros.GenLens
import monocle.function.all._
import monocle.std.list._
import monocle.std.tuple2._
import monocle.syntax.all._
import task.example.TaskBehaviours.TaskBehaviour
import task.example.TaskProjections.{TaskProjection, Completed, Open, Task}
import scala.language.higherKinds
import cats.free.Free
import cats.data.Xor
import cats.syntax.xor._
import monocle.std.map._
import cats.std.option._

object TaskCompile {

  type Tasks = Map[String, Task]

  type MapState[A] = State[Tasks, A]

  type CommandOrQuery[A] = TaskCommand[A] Xor TaskQuery[A]

  type BehaviourOrQuery[A] = Free[TaskBehaviour, A] Xor Free[TaskQuery, A]

  type Result[A] = Free[BehaviourOrQuery, A]

  object toStateC extends (TaskCommand ~> MapState) {

    private val taskStatus = GenLens[Task](_.status)
    private val root = Iso.id[Map[String, Task]]

    def insertTask(id: String, t: Task)(ts: Tasks): Tasks =
      (root composeLens at(id)).set(Some(t))(ts)

    def updateTask(f: Task => Task)(id: String)(ts: Tasks): Tasks =
      (root composeLens at(id) modify Functor[Option].lift(f))(ts)

    val openTask = updateTask(taskStatus set Open) _
    val completeTask = updateTask(taskStatus set Completed) _

    def modS[A](f: Tasks => Tasks, g: Unit => A = identity[Unit] _) = State modify f map g

    override def apply[A](fa: TaskCommand[A]) = fa match {
      case CommitToTask(id, text) => modS(insertTask(id, Task(id, text, Open)))
      case CompleteTask(id) => modS(completeTask(id))
      case ReopenTask(id) => modS(openTask(id))
    }
  }

  object toStateQ extends (TaskQuery ~> MapState) {

    def fromS[A](f: Tasks => A) = State.inspect(f)

    def asList(f: Task => Boolean)(ts: Tasks) = ts.map({ case (_, v) => v }).toList.filter(f)

    override def apply[A](fa: TaskQuery[A]) = fa match {
      case ReadTask(id) => fromS(_.get(id))
      case _: FindAllTasks => fromS(asList(_ => true))
      case _: FindOpenTasks => fromS(asList(_.status == Open))
      case _: FindClosedTasks => fromS(asList(_.status == Completed))
    }

  }

  object toProjectionsStateQ extends (TaskQuery ~> TaskAggCompile.MapState) {

    def fromS[A](f: TaskAggCompile.Tasks => A) = State.inspect(f)

    def projections(ts: TaskAggCompile.Tasks) = ts applyLens second

    def findTask(id: String)(ts: TaskAggCompile.Tasks): Option[TaskProjection] =
      (projections(ts) composeLens at(id)).get

    override def apply[A](fa: TaskQuery[A]) = fa match {
      case ReadTask(id) => fromS(findTask(id))
      case _: FindAllTasks => fromS(asList(_ => true))
      case _: FindOpenTasks => fromS(asList(t => t match {case Task(_,_,Open) => true case _ => false}))
      case _: FindClosedTasks => fromS(asList(t => t match {case Task(_,_,Completed) => true case _ => false}))
    }

    def asList(f: TaskProjection => Boolean)(ts: TaskAggCompile.Tasks): List[TaskProjection] =
      (projections(ts) composeTraversal each).getAll.filter(f)
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
        es <- commit(id, text)
        _ <- save(id, es)
        _ <- project(id, es)
      } yield ()
      case CompleteTask(id) => for {
        t <- find(id)
        es <- complete(t)
        _ <- save(id, es)
        _ <- project(id, es)
      } yield ()
      case ReopenTask(id) => for {
        t <- find(id)
        es <- reopen(t)
        _ <- save(id, es)
        _ <- project(id, es)
      } yield ()
    }

  }
}