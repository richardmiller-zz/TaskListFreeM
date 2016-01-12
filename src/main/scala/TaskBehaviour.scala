package task.example

import freetocompose.addComposingFunctions
import task.example.TaskEvents.{TaskReopened, TaskCompleted, TaskCommitted, EventStream}

object TaskBehaviours {
  sealed trait TaskBehaviour[+A]
  final case class Commit(id: String, text: String) extends TaskBehaviour[EventStream]
  final case class Complete(t: TaskAgg) extends TaskBehaviour[EventStream]
  final case class Reopen(t: TaskAgg) extends TaskBehaviour[EventStream]
  final case class Find(id: String) extends TaskBehaviour[TaskAgg]
  final case class Save(id: String, es: EventStream) extends TaskBehaviour[Unit]
  final case class Project(id: String, es: EventStream) extends TaskBehaviour[Unit]
}

object TaskBehaviourC {
  @addComposingFunctions[TaskBehaviours.TaskBehaviour]('TaskBehaviourC) object composing
}

final case class TaskAgg(id: String, events: EventStream)

object TaskAgg {
  def commit(id: String, text: String): EventStream = List(TaskCommitted(id, text))

  def complete(t: TaskAgg): EventStream = emitIfCommitted(t, List(TaskCompleted(t.id)))

  def reopen(t: TaskAgg): EventStream = emitIfCommitted(t, List(TaskReopened(t.id)))

  private def emitIfCommitted(t: TaskAgg, events: EventStream): EventStream = {
    t match {
      case TaskAgg(id, es) if hasBeenCommitted(es) => events
      case _ => List()
    }
  }

  private def hasBeenCommitted(es: EventStream): Boolean = {
    es.exists { case _: TaskCommitted => true case _ => false }
  }
}

object TaskEvents {
  type EventStream = List[TaskEvent]
  sealed trait TaskEvent {
    def id: String
  }
  final case class TaskCommitted(id: String, text: String) extends TaskEvent
  final case class TaskCompleted(id: String) extends TaskEvent
  final case class TaskReopened(id: String) extends TaskEvent
}

