package task.example

import cats.Monoid
import task.example.TaskEvents._
import cats.implicits._

object TaskProjections {
  sealed trait TaskProjection
  final case class Task(id: String, text: String, status: TaskStatus) extends TaskProjection
  case object EmptyTask extends TaskProjection

  sealed trait TaskStatus
  case object Open extends TaskStatus
  case object Completed extends TaskStatus

  implicit val TaskMonoid = new Monoid[TaskProjection] {
    def combine(a: TaskProjection, b: TaskProjection): TaskProjection = (a, b) match {
      case (EmptyTask, _) => b
      case (_, EmptyTask) => a
      case (Task(ai, _, _), Task(bi, _, _)) if ai != bi => EmptyTask
      case (Task(ai, at, as), Task(bi, bt, bs)) =>
        val t = if (bt == "") at else bt
        Task(bi, t, bs)
    }

    def empty: TaskProjection = EmptyTask
  }

  object Task {

    def fromEvent(e: TaskEvent): TaskProjection = e match {
      case TaskCommitted(i, t) => Task(i, t, Open)
      case TaskCompleted(i) => Task(i, "", Completed)
      case TaskReopened(i) => Task(i, "", Open)
    }

    def project(t: TaskProjection, es: EventStream) = t |+| es.foldMap(fromEvent)
  }

}
