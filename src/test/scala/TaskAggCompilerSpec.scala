package test.task.example

import org.specs2.Specification
import task.example.TaskEvents.{TaskReopened, TaskCompleted, TaskCommitted}
import task.example._
import TaskBehaviourC.composing._
import cats.std.function._

class TaskAggCompilerSpec extends Specification { def is = s2"""

 This is a specification to check the TaskAgg State Compiler

 The TaskAgg State Compiler string should
   save newly commited tasks                                     $e1
   close tasks                                                   $e2
   reopen tasks                                                  $e3
                                                                 """

  val compiler = TaskAggCompile.toState

  def program1[TaskBehaviourC] = {
    for {
      t <- commit("2", "My first Task")
      _ <- save(t)
    } yield ()
  }

  private val statingState = (Map[String, TaskAgg](), Map[String, Option[TaskProjections.Task]]())

  val r1 = program1.foldMap(compiler).run(statingState).run

  def e1 = r1._1._1 must_== Map("2" -> TaskAgg("2", List(TaskCommitted("2", "My first Task"))))

  def program2[TaskBehaviourC] = {
    for {
      _ <- program1
      t <- TaskBehaviourC.composing.find("2")
      t2 <- complete(t)
      _ <- save(t2)
    } yield ()
  }

  val r2 = program2.foldMap(compiler).run(statingState).run

  def e2 = r2._1._1 must_== Map("2" -> TaskAgg("2", List(TaskCommitted("2", "My first Task"), TaskCompleted("2"))))

  def program3[TaskBehaviourC] = {
    for {
      _ <- program2
      t <- TaskBehaviourC.composing.find("2")
      t2 <- reopen(t)
      _ <- save(t2)
    } yield ()
  }

  val r3 = program3.foldMap(compiler).run(statingState).run

  def e3 = r3._1._1 must_== Map("2" -> TaskAgg("2", List(TaskCommitted("2", "My first Task"), TaskCompleted("2"), TaskReopened("2"))))
}
