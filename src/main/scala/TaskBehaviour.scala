package task.example

import freetocompose.addComposingFunctions

object TaskBehaviours {
  sealed trait TaskBehaviour[+A]
  case class Commit(id: String, text: String) extends TaskBehaviour[Option[TaskAgg]]
  case class Complete(t: Option[TaskAgg]) extends TaskBehaviour[Option[TaskAgg]]
  case class Reopen(t: Option[TaskAgg]) extends TaskBehaviour[Option[TaskAgg]]
  case class Find(id: String) extends TaskBehaviour[Option[TaskAgg]]
  case class Save(t: Option[TaskAgg]) extends TaskBehaviour[Unit]
}

object TaskBehaviourC {
  @addComposingFunctions[TaskBehaviours.TaskBehaviour]('TaskBehaviourC) object composing
}

final case class TaskAgg(id: String, text: String, status: TaskStatus = Open)