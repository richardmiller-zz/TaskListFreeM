package task.example

import freetocompose.addComposingFunctions

object IdentityGeneration {
  sealed trait IdentityCommand[+A]
  final case class GenerateUuid() extends IdentityCommand[String]
}

object IdentityGen {
  @addComposingFunctions[IdentityGeneration.IdentityCommand]('IdentityGen) object composing
}