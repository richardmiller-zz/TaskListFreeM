package task.example

import cats.state._
import cats.~>
import TaskBehaviours._

object TaskAggCompile {

  type Tasks = Map[String, TaskAgg]

  type MapState[A] = State[Tasks, A]

  object toState extends (TaskBehaviour ~> MapState) {

    override def apply[A](fa: TaskBehaviour[A]) = fa match {
      case Commit(id, text) => State.get.map(_ => Some(TaskAgg.commit(id, text)))
      case Complete(t) => State.get.map(_ => t.map(TaskAgg.complete))
      case Reopen(t) => State.get.map(_ => t.map(TaskAgg.reopen))
      case Find(id) => State.inspect(_.get(id))
      case Save(t) => State.modify((ts: Tasks) => t.fold(ts)(t1 => ts + (t1.id -> t1))).map(_ => ())
    }
  }
}