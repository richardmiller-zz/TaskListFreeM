package task.example

import TaskCommands._
import TaskQueries._
import cats.state._
import cats.~>

object TaskCompile {

  type Tasks = Map[String, Task]

  type MapState[A] = State[Tasks, A]

  object toStateC extends (TaskCommand ~> MapState) {

    override def apply[A](fa: TaskCommand[A]) = fa match {
      case CommitToTask(id, text) => State.modify((ts: Tasks) => ts + (id -> Task(id, text))).map(_ => ())
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
}