package task.example

import cats.state._
import cats.~>
import TaskBehaviours._

object TaskAggCompile {

  type Tasks = (Map[String, TaskAgg], Map[String, Option[TaskProjections.Task]])

  type MapState[A] = State[Tasks, A]

  object toState extends (TaskBehaviour ~> MapState) {

    override def apply[A](fa: TaskBehaviour[A]) = fa match {
      case Commit(id, text) => State.get.map(_ => Some(TaskAgg.commit(id, text)))
      case Complete(t) => State.get.map(_ => t.map(TaskAgg.complete))
      case Reopen(t) => State.get.map(_ => t.map(TaskAgg.reopen))
      case Find(id) => State.inspect(_._1.get(id))
      case Save(t) => State.modify((ts: Tasks) => t.fold(ts)(t1 => (ts._1 + (t1.id -> t1), ts._2))).map(_ => ())
      case Project(t) => State.modify((ts: Tasks) =>
        t.fold(ts)(t1 => (ts._1, ts._2 + (t1.id -> TaskProjections.Task.project(None, t1.events))))).map(_ => ())
    }
  }
}