package task.example

import freetocompose.addComposingFunctions
import task.example.TaskProjections.TaskProjection

object TaskQueries {
  sealed trait TaskQuery[+A]
  final case class ReadTask(id: String) extends TaskQuery[Option[TaskProjection]]
  final case class FindAllTasks() extends TaskQuery[List[TaskProjection]]
  final case class FindOpenTasks() extends TaskQuery[List[TaskProjection]]
  final case class FindClosedTasks() extends TaskQuery[List[TaskProjection]]
}

object TaskQ {
  @addComposingFunctions[TaskQueries.TaskQuery]('TaskQ) object composing
}