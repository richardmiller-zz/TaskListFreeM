package task.example

import java.util.UUID

import cats.data.Xor
import cats.free.Free
import cats.~>
import cats.syntax.xor._
import task.example.IdentityGeneration.{GenerateUuid, IdentityCommand}
import task.example.TaskBehaviours.TaskBehaviour
import task.example.TaskCommands.TaskCommand

import scalaz.concurrent.Task

object IdentityGenCompile {

  type CommandOrIdentity[A] = TaskCommand[A] Xor IdentityCommand[A]

  type BehaviourOrIdentity[A] = Free[TaskBehaviour, A] Xor Free[IdentityCommand, A]

  type Result[A] = Free[BehaviourOrIdentity, A]

  object standard extends (IdentityCommand ~> Task) {

    override def apply[A](fa: IdentityCommand[A]) = fa match {
      case _:GenerateUuid => Task(UUID.randomUUID.toString)
    }
  }

  object identityG extends (IdentityCommand ~> Free[IdentityCommand, ?]) {
    override def apply[A](fa: IdentityCommand[A]): Free[IdentityCommand, A] = Free.liftF(fa)
  }

  object LiftLeftIG extends (TaskCommand ~> CommandOrIdentity) {
    def apply[A](c: TaskCommand[A]): CommandOrIdentity[A] = c.left
  }

  object LiftRightIG extends (IdentityCommand ~> CommandOrIdentity) {
    def apply[A](q: IdentityCommand[A]): CommandOrIdentity[A] = q.right
  }

  implicit class LiftLeftOpsIG[A](in: Free[TaskCommand, A]) {
    def lift: Free[CommandOrIdentity, A] = in.compile(LiftLeftIG)
  }
  implicit class LiftRightOpsIG[A](in: Free[IdentityCommand, A]) {
    def lift: Free[CommandOrIdentity, A] = in.compile(LiftRightIG)
  }

  def  XorInterpreter = new (CommandOrIdentity ~> Result) {
    def apply[A](in: CommandOrIdentity[A]) =
      in.fold(
        (c: TaskCommand[A]) => Free.liftF[BehaviourOrIdentity, A](TaskCompile.toTaskBehaviours(c).left),
        (q: IdentityCommand[A]) => Free.liftF[BehaviourOrIdentity, A](identityG(q).right)
      )
  }
}