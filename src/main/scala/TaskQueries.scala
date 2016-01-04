package task.example

import freetocompose.addComposingFunctions
import task.example.TaskProjections.Task

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