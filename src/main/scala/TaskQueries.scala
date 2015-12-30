package task.example

import freetocompose.addComposingFunctions

object TaskQueries {
  sealed trait TaskQuery[+A]
  final case class ReadTask(id: String) extends TaskQuery[Option[Task]]
  final case class FindAllTasks() extends TaskQuery[List[Task]]
  final case class FindOpenTasks() extends TaskQuery[List[Task]]
  final case class FindClosedTasks() extends TaskQuery[List[Task]]
}

object TaskQ {
  @addComposingFunctions[TaskQueries.TaskQuery]('TaskQ) object composing
}

final case class Task(id: String, text: String, status: TaskStatus = Open)

sealed trait TaskStatus
case object Open extends TaskStatus
case object Completed extends TaskStatus