package test.task.example

import cats.free.Free
import org.specs2.Specification
import task.example.TaskBehaviours.TaskBehaviour
import task.example.TaskCommands.TaskCommand
import task.example.TaskCompile._
import task.example.TaskEvents.{EventStream, TaskReopened, TaskCompleted, TaskCommitted}
import task.example.TaskProjections.Task
import task.example._
import TaskC.composing._
import cats.std.function._

class TaskBehaviourCompileSpec extends Specification { def is = s2"""

 This is a specification to check the Task State Compiler

 The Task State Compiler string should
   commit, complete and reopen tasks                             $e1
                                                                 """

  val behaviourCompiler = TaskCompile.toTaskBehaviours
  val stateCompiler = TaskAggCompile.toState

  def program: Free[TaskCommand, Unit] = {
    for {
      _ <- commitToTask("2", "My first Task")
      _ <- commitToTask("3", "My second Task")
      _ <- completeTask("2")
      _ <- completeTask("5")
      _ <- reopenTask("2")
      _ <- completeTask("2")
    } yield ()
  }
  val statingState = (Map[String, EventStream](), Map[String, Option[TaskProjections.Task]]())
  val r3 = program.foldMap[Free[TaskBehaviour, ?]](behaviourCompiler).foldMap(stateCompiler).run(statingState).run

  def e1 = r3._1._1 must_== Map(
    "2" -> List(
      TaskCommitted("2", "My first Task"),
      TaskCompleted("2"),
      TaskReopened("2"),
      TaskCompleted("2")
    ),
    "3" -> List(
      TaskCommitted("3", "My second Task")
    )
  )
}
