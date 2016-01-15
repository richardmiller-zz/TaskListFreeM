package task.example

object ATaskEvents {
  type EventStream = List[TaskEvent]

  sealed trait TaskEvent {
    def id: String
  }

  final case class TaskCommitted(id: String, text: String) extends TaskEvent
  final case class TaskCompleted(id: String) extends TaskEvent
  final case class TaskReopened(id: String) extends TaskEvent
}
