package test.task.example

import org.specs2.Specification
import task.example.TaskEvents.{TaskEvent, TaskReopened, TaskCompleted, TaskCommitted}
import task.example.TaskProjections._
import cats.syntax.semigroup._
import cats.std.all._
import cats.syntax.foldable._

class TaskProjectionsSpec extends Specification { def is = s2"""

 This is a specification to check the TaskProjections

 The Task Projection should
   be associative                                                $e1
   have left identity                                            $e2
   have right identity                                           $e3
   be creatable from TaskCommited event                          $e4
   be creatable from TaskCompleted event                         $e5
   be creatable from TaskReopened event                          $e6
   be foldable                                                   $e7
   be foldable from events                                       $e8
   be foldable from existing projection and new events           $e9
                                                                 """

  def e1 = {
    val t1: TaskProjection = Task("1", "badger", Open)
    val t2: TaskProjection = Task("1", "potato", Completed)
    val t3: TaskProjection = Task("1", "bananas", Open)
    (t1 |+| t2) |+| t3 must_== t1 |+| (t2 |+| t3)
  }

  def e2 = {
    val t1: TaskProjection = Task("1", "badger", Open)
    TaskMonoid.empty |+| t1 must_== t1
  }

  def e3 = {
    val t1: TaskProjection = Task("1", "badger", Open)
    t1 |+| TaskMonoid.empty must_== t1
  }

  def e4 = Task.fromEvent(TaskCommitted("7", "carrots")) must_== Task("7", "carrots", Open)

  def e5 = Task.fromEvent(TaskCompleted("8")) must_== Task("8", "", Completed)

  def e6 = Task.fromEvent(TaskReopened("10")) must_== Task("10", "", Open)

  def e7 = {
    val ts: List[TaskProjection] = List(
      Task("1", "badger", Open),
      Task("1", "potato", Completed),
      Task("1", "bananas", Open),
      Task("1", "", Open),
      Task("1", "", Completed)
    )

    ts.foldMap(identity) must_== Task("1", "bananas", Completed)
  }

  def e8 = {
    val ts: List[TaskEvent] = List(
      TaskCommitted("7", "bananas"),
      TaskCompleted("7"),
      TaskReopened("7"),
      TaskCompleted("7")
    )

    Task.project(EmptyTask, ts) must_== Task("7", "bananas", Completed)
  }

  def e9 = {
    val t1: TaskProjection = Task("7", "badger", Open)
    val es: List[TaskEvent] = List(
      TaskCompleted("7"),
      TaskReopened("7"),
      TaskCompleted("7")
    )

     Task.project(t1, es) must_== Task("7", "badger", Completed)
  }
}
