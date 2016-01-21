package task.example

import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.server.Service
import org.http4s.server.blaze.BlazeBuilder
import task.example.DoobieCompile._

object Main extends App {

  dbSetUp.run

  BlazeBuilder
    .mountService(TaskService.toService)
    .bindLocal(8080)
    .start
    .run
    .awaitShutdown()
}