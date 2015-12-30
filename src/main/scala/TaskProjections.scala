package task.example

import cats.Monoid
import task.example.TaskEvents._
import cats.syntax.semigroup._

object TaskProjections {
  final case class Task(id: String, text: String, status: TaskStatus)

  implicit def TaskMonoid = new Monoid[Option[Task]] {
    def combine(a: Option[Task], b: Option[Task]): Option[Task] = (a, b) match {
      case (None, _) => b
      case (_, None) => a
      case (Some(Task(ai, _, _)), Some(Task(bi, _, _))) if ai != bi => None
      case (Some(Task(ai, at, as)), Some(Task(bi, bt, bs))) =>
        val t = if (bt == "") at else bt
        Some(Task(bi, t, bs))
    }

    def empty: Option[Task] = None
  }

  object Task {
    def fromEvent(e: TaskEvent): Task = e match {
      case TaskCommitted(i, t) => Task(i, t, Open)
      case TaskCompleted(i) => Task(i, "", Completed)
      case TaskReopened(i) => Task(i, "", Open)
    }

    val optionFromEvent: (TaskEvent => Option[Task]) = fromEvent _ andThen (t => Some(t))

    def project(t: Option[Task], es: EventStream) = es.map(optionFromEvent).foldLeft(t)(_ |+| _)
  }

}
