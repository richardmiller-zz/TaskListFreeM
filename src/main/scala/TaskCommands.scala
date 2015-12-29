package task.example

import freetocompose.addComposingFunctions

object TaskCommands {
  sealed trait TaskCommand[+A]
  case class CommitToTask(id: String, text: String) extends TaskCommand[Unit]
  case class CompleteTask(id: String) extends TaskCommand[Unit]
  case class ReopenTask(id: String) extends TaskCommand[Unit]
}

object TaskC {
  @addComposingFunctions[TaskCommands.TaskCommand]('TaskC) object composing
}

final case class Task(id: String, text: String, status: TaskStatus = Open)

sealed trait TaskStatus
case object Open extends TaskStatus
case object Completed extends TaskStatus