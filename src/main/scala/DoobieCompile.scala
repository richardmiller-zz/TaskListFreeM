package task.example

import scalaz._, Scalaz._
import cats.{Monad, ~>}
import doobie.contrib.h2.h2transactor.H2Transactor
import TaskBehaviours._
import TaskEvents.EventStream
import task.example.TaskCompile.BehaviourOrQuery
import task.example.TaskProjections.{Task => TaskP, _}
import task.example.TaskQueries._
import scalaz.concurrent.Task
import task.example.TaskEvents.TaskEvent
import io.circe._
import io.circe.generic.auto._
import io.circe.parse._
import io.circe.syntax._
import doobie.imports._
import TaskProjections.Task.project

object DoobieCompile {

  implicit val catsTask = new Monad[Task] {
    override def flatMap[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = Task.taskInstance.bind(fa)(f)

    override def pure[A](x: A): Task[A] = Task(x)
  }

  implicit val TaskEventComposite: Composite[TaskEvent] =
    Composite[(String, String)].xmap(
      (t: (String, String)) => decode[TaskEvent](t._2).fold(p => sys.error(p.toString), identity),
      (t: TaskEvent) => (t.id, t.asJson.noSpaces)
    )

  implicit val TaskProjectionComposite: Composite[TaskP] =
    Composite[(String, String, String)].xmap(
      (t: (String, String, String)) => TaskP(t._1, t._2, t._3  match {case "Completed" => Completed case _ => Open}),
      (t: TaskP) => (t.id, t.text, t.status.toString)
    )

  private def runH2[A](f: H2Transactor[Task] => Task[A]) = {
    for {
      xa <- H2Transactor[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")
      _ <- xa.setMaxConnections(10)
      p <- f(xa)
    } yield p
  }
  
  private def runH2T[A](pg: ConnectionIO[A]) = runH2(xa => pg.transact(xa))

  private def findTask(aggId: String): ConnectionIO[Option[TaskP]] =
    sql"select id, text, status from task WHERE id = $aggId".query[TaskP].option

  private def findAllTasks: ConnectionIO[List[TaskP]] =
    sql"select id, text, status from task".query[TaskP].list

  private def findOpenTasks: ConnectionIO[List[TaskP]] =
    sql"select id, text, status from task WHERE status = 'Open'".query[TaskP].list

  private def findClosedTasks: ConnectionIO[List[TaskP]] =
    sql"select id, text, status from task WHERE status = 'Completed'".query[TaskP].list

  object behaviour extends (TaskBehaviour ~> Task) {

    private def insertTaskEvent(e: TaskEvent): ConnectionIO[Int] = {
      sql"insert into events (agg_id, event) values (${e.id}, ${e.asJson.noSpaces})".update.run
    }

    private def liftListO[A](l: List[A]): Option[List[A]] = l match {
      case List() => None
      case _ => Some(l)
    }

    private def liftTaskO(l: TaskProjection): Option[TaskP] = l match {
      case t:TaskP => Some(t)
      case _ => None
    }

    private def insertTaskEvents(es: EventStream) = liftListO(es).map(_.map(insertTaskEvent).reduceLeft(_ *> _))

    private def insertTask(t: TaskProjection) = liftTaskO(t).map(
      t1 => sql"insert into task (id, text, status) values (${t1.id}, ${t1.text}, ${t1.status.toString})".update
    )

    private def updateTask(t: TaskProjection) = liftTaskO(t).map(
      t1 => sql"UPDATE task SET text = ${t1.text}, status = ${t1.status.toString} WHERE id = ${t1.id}".update
    )

    private def findEvents(aggId: String) = sql"select agg_id, event from events WHERE agg_id = $aggId".query[TaskEvent].process.list

    private def projectTask[A](es: EventStream, o: Option[TaskP]) =
      o.fold(insertTask(project(EmptyTask, es)))(t => updateTask(project(t, es)))

    private def runIfExists[A](t: Option[Task[A]]) = t.getOrElse(Task(())).map(_ => ())

    override def apply[A](fa: TaskBehaviour[A]) = fa match {
      case Commit(id, text) => Task(TaskAgg.commit(id, text))
      case Complete(t) => Task(TaskAgg.complete(t))
      case Reopen(t) => Task(TaskAgg.reopen(t))
      case Find(id) => runH2T(findEvents(id)).map(p => TaskAgg(id, p))
      case Save(_, es) => runH2(xa => runIfExists(insertTaskEvents(es).map(_.transact(xa))))
      case Project(id, es) =>
        runH2(xa => runH2T(findTask(id)).flatMap(t => runIfExists(projectTask(es, t).map(_.run.transact(xa)))))
    }
  }

  object queries extends (TaskQuery ~> Task) {

    override def apply[A](fa: TaskQuery[A]) = fa match {
      case ReadTask(id) => runH2T(findTask(id))
      case _: FindAllTasks => runH2T(findAllTasks)
      case _: FindOpenTasks => runH2T(findOpenTasks)
      case _: FindClosedTasks => runH2T(findClosedTasks)
    }
  }

  def XorDoobieInterpreter = new (BehaviourOrQuery ~> Task) {
    override def apply[A](fa: BehaviourOrQuery[A]) = fa.fold(_.foldMap(behaviour), _.foldMap(queries))
  }

  def dbSetUp = {
    val dropE = sql"""DROP TABLE IF EXISTS events""".update.run

    val createE = sql"""
    CREATE TABLE events (
      id BIGINT auto_increment,
      agg_id VARCHAR NOT NULL,
      event TEXT
    )""".update.run

    val dropT = sql"""DROP TABLE IF EXISTS task""".update.run

    val createT = sql"""CREATE TABLE task (
      id   VARCHAR UNIQUE,
      text VARCHAR NOT NULL,
      status VARCHAR
    )""".update.run

    runH2T(dropE *> createE *> dropT *> createT)
  }
}