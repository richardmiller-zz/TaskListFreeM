package task.example

import cats.state._
import cats.~>
import TaskBehaviours._
import task.example.TaskEvents.EventStream

object TaskAggCompile {

  type Tasks = (Map[String, EventStream], Map[String, Option[TaskProjections.Task]])

  type MapState[A] = State[Tasks, A]

  object toState extends (TaskBehaviour ~> MapState) {

    override def apply[A](fa: TaskBehaviour[A]) = fa match {
      case Commit(id, text) => State.get.map(_ => TaskAgg.commit(id, text))
      case Complete(t) => State.get.map(_ => TaskAgg.complete(t))
      case Reopen(t) => State.get.map(_ => TaskAgg.reopen(t))
      case Find(id) => State.inspect(m => TaskAgg(id, m._1.getOrElse(id, List())))
      case Save(id, es) => State.modify((ts: Tasks) => es match {
        case List() => ts
        case _ => (ts._1 + (id -> (ts._1.getOrElse(id, List()) ++ es)), ts._2)
      }).map(_ => ())
      case Project(id, es) => State.modify((ts: Tasks) =>
        (ts._1, ts._2 + (id -> TaskProjections.Task.project(ts._2.getOrElse(id, None), es)))).map(_ => ())
    }
  }
}