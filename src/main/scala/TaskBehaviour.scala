package task.example

import freetocompose.addComposingFunctions
import task.example.TaskEvents.{TaskReopened, TaskCompleted, TaskCommitted, EventStream}

object TaskBehaviours {
  sealed trait TaskBehaviour[+A]
  final case class Commit(id: String, text: String) extends TaskBehaviour[Option[TaskAgg]]
  final case class Complete(t: Option[TaskAgg]) extends TaskBehaviour[Option[TaskAgg]]
  final case class Reopen(t: Option[TaskAgg]) extends TaskBehaviour[Option[TaskAgg]]
  final case class Find(id: String) extends TaskBehaviour[Option[TaskAgg]]
  final case class Save(t: Option[TaskAgg]) extends TaskBehaviour[Unit]
}

object TaskBehaviourC {
  @addComposingFunctions[TaskBehaviours.TaskBehaviour]('TaskBehaviourC) object composing
}

final case class TaskAgg(id: String, events: EventStream)

object TaskAgg {
  def commit(id: String, text: String): TaskAgg = TaskAgg(id, List(TaskCommitted(id, text)))

  def complete(t: TaskAgg): TaskAgg = t.copy(events = t.events :+ TaskCompleted(t.id))

  def reopen(t: TaskAgg): TaskAgg = t.copy(events = t.events :+ TaskReopened(t.id))
}

object TaskEvents {
  type EventStream = List[TaskEvent]
  sealed trait TaskEvent
  final case class TaskCommitted(id: String, text: String) extends TaskEvent
  final case class TaskCompleted(id: String) extends TaskEvent
  final case class TaskReopened(id: String) extends TaskEvent
}

