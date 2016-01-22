package task.example

import JsonConversion._
import org.http4s._
import org.http4s.headers.Location
import org.http4s.rho.RhoService
import org.http4s.rho._
import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.rho.swagger.SwaggerSupport
import shapeless.HNil
import task.example.DoobieCompile._
import task.example.IdentityGenCompile._
import task.example.TaskBehaviours.TaskBehaviour
import task.example.TaskCommands.TaskCommand
import task.example.TaskProjections.{Task, TaskProjection}
import io.circe._
import io.circe.generic.auto._
import io.circe.parse._
import io.circe.syntax._
import TaskQ.composing._
import TaskC.composing._
import IdentityGen.composing._
import cats.free.Free
import cats.free.Free._
import org.http4s.rho.hal.{ResourceObjectBuilder => ResObjBuilder, ResourceObject}
import task.example.TaskQueries.TaskQuery

object TaskService extends RhoService with SwaggerSupport {

  type FFS[A] = Free[CommandOrIdentity, A]

  type HalTaskCollection = ResourceObject[(String, Long), Task]
  val tasksU : TypedPath[HNil]= "tasks"
  val openTasksU: TypedPath[HNil] = tasksU / "open"
  val completedTasksU: TypedPath[HNil] = tasksU / "completed"
  val taskIdU = pathVar[String]

  final case class TaskRequest(text: String)

  GET / "" |>> { (request: Request) => Ok(HalSupport.rootAsResource(request)) }

  GET / tasksU |>> { (request: Request) => Ok(allTasks(request)) }

  GET / openTasksU |>> { (request: Request) => Ok(openTasks(request)) }

  GET / completedTasksU |>> { (request: Request) => Ok(completedTasks(request)) }

  GET / tasksU  / taskIdU |>> { (request: Request, id: String) =>
    runQueryPrg(for {task <- readTask(id)} yield task).map {
      case t: Task => Ok(HalSupport.taskAsResource(request, t))
      case _ => taskNotFound(id)
    } getOrElse taskNotFound(id)
  }

  POST / tasksU ^ EntityDecoder.text |>> { body: String => decode[TaskRequest](body).fold(
    e => BadRequest(s"Invalid task: $body"),
    t => {
      val prg: Free[CommandOrIdentity, String] = for {
        id <- generateUuid.lift
        _ <- commitToTask(id, t.text).lift
      } yield id

      val id = runCommandPrg(prg)

      (for (uri <- Uri.fromString("/tasks/" + id)) yield uri).fold(
        _ => Accepted(""),
        u => Accepted("", Headers(Location(u)))
      )
    }
  )}

  POST / openTasksU  / taskIdU |>> { id: String => {
    runCommandPrg(for {_ <- reopenTask(id).lift} yield ())
    Accepted("")
  }}

  POST / completedTasksU  / taskIdU |>> { id: String => {
    runCommandPrg(for {_ <- completeTask(id).lift} yield ())
    Accepted("")
  }}

  private def allTasks(r: Request): HalTaskCollection = runCollectionQueryPrg(r, for {ts <- findAllTasks} yield ts)

  private def openTasks(r: Request): HalTaskCollection = runCollectionQueryPrg(r, for {ts <- findOpenTasks} yield ts)

  private def completedTasks(r: Request): HalTaskCollection = runCollectionQueryPrg(r, for {ts <- findClosedTasks} yield ts)

  private def runQueryPrg[A](prg: Free[TaskQuery, A]) = prg.foldMap(DoobieCompile.queries).run

  private def runCollectionQueryPrg(r: Request, prg: Free[TaskQuery, List[TaskProjection]]) =
    HalSupport.tasksAsResource(r, runQueryPrg(prg))

  private def runCommandPrg[A](prg: FFS[A]) =
    prg.foldMap(IdentityGenCompile.XorInterpreter).foldMap(DoobieCompile.XorDoobieIdentityGenInterpreter).run

  private def taskNotFound(id: String) = NotFound(s"Task $id not found")

  object HalSupport {
    def tasksAsResource(request: Request, tasks: Seq[TaskProjection]): HalTaskCollection = {
      val ts = tasks.collect({ case t: Task => t })
      tasksLinks(request, new ResObjBuilder[(String, Long), Task]()
        .link("self", request.uri)
        .content("total", ts.length)
        .resources("tasks", ts.map(task => taskAsResourceObject(request, task).build).toList)).build
    }

    def taskAsResource(request: Request, task: Task): ResourceObject[Task, String] =
      tasksLinks(request, taskAsResourceObject(request, task)).build

    def rootAsResource(request: Request): ResourceObject[String, String] =
      tasksLinks(request, new ResObjBuilder[String, String]()).build

    private def taskAsResourceObject(request: Request, task: Task) = {
      val hal = new ResObjBuilder[Task, String]()
      for {
        tpl <- ("tasks" / pathVar[String]("id")).asUriTemplate(request)
      } yield hal.link("self", tpl.expandPath("id", task.id).toUriIfPossible.get)
      hal.content(task)
    }

    private def tasksLinks[A, B](request: Request, hal: ResObjBuilder[A, B]) = {
      def addLink(h: ResObjBuilder[A, B])(path: TypedPath[HNil], name: String, title: String) = {
        for (uri <- path.asUri(request)) yield { if (uri != request.uri) {h.link(name, uri.toString, title)}}
        h
      }

      List(
        (tasksU, "tasks", "Lists tasks"),
        (openTasksU, "open-tasks", "Lists open tasks"),
        (completedTasksU, "completed-tasks", "Lists completed tasks")
      ).foldLeft(hal)((a, c) => (addLink(a) _).tupled(c))
    }
  }
}
