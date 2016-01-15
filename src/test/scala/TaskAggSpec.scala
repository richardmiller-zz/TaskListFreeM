package test.task.example

import org.specs2.Specification
import task.example.ATaskEvents.{TaskReopened, TaskCompleted, TaskCommitted}
import task.example.TaskAgg

class TaskAggSpec extends Specification { def is = s2"""

 This is a specification to check the TaskAgg

 The TaskAgg should
   allow tasks to be committed to                                $e1
   close tasks                                                   $e2
   repen tasks                                                   $e3
                                                                 """

  def e1 = TaskAgg.commit("5", "Feed the badgers") must_== List(TaskCommitted("5", "Feed the badgers"))

  def e2 = TaskAgg.complete(TaskAgg("5", List(TaskCommitted("5", "Feed the badgers")))) must_== List(TaskCompleted("5"))

  def e3 = TaskAgg.reopen(TaskAgg("5", List(TaskCommitted("5", "Feed the badgers")))) must_== List(TaskReopened("5"))
}
