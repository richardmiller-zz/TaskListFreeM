package task.example

import freetocompose.addComposingFunctions

object TaskCommands {
  sealed trait TaskCommand[+A]
  final case class CommitToTask(id: String, text: String) extends TaskCommand[Unit]
  final case class CompleteTask(id: String) extends TaskCommand[Unit]
  final case class ReopenTask(id: String) extends TaskCommand[Unit]
}

object TaskC {
  @addComposingFunctions[TaskCommands.TaskCommand]('TaskC) object composing
}