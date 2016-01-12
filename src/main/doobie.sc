w
import task.example.TaskEvents.{TaskReopened, TaskEvent, TaskCompleted, TaskCommitted}
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import io.circe.syntax._
import io.circe._
import io.circe.generic.auto._
import io.circe.parse._

/*implicit val TaskProjectionComposite: Composite[TaskProjection] =
  Composite[(String, String, String)].xmap(
    (t: (String, String, String)) => TaskProjection(t._1, t._2, t._3  match {case "Completed" => Completed case _ => Open}),
    (t: TaskProjection) => (t.id, t.text, t.status.toString)
  )
val drop =
  sql"""
    DROP TABLE IF EXISTS task
  """.update.run
val create =
  sql"""
    CREATE TABLE task (
      id   VARCHAR UNIQUE,
      text VARCHAR NOT NULL,
      status VARCHAR
    )
  """.update.run
def insertWithDeleteTask(t: TaskProjection) = {
  sql"delete from task WHERE id = ${t.id}".update.run *>
  sql"insert into task (id, text, status) values (${t.id}, ${t.text}, ${t.status.toString})".update.run
}
def findTasks = sql"select id, text, status from task".query[TaskProjection].process.list
val qs1 = drop *>
  create *>
  insertWithDeleteTask(TaskProjection("2", "badger", Open)) *>
  insertWithDeleteTask(TaskProjection("3", "banana", Open)) *>
  insertWithDeleteTask(TaskProjection("3", "banana", Open)) *>
  insertWithDeleteTask(TaskProjection("4", "potato", Completed)) *>
  findTasks
val qs = for {
  xa <- H2Transactor[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")
  _  <- xa.setMaxConnections(10)
  p  <- qs1.transact(xa)
} yield p
qs.run*/

implicit val TaskEventComposite: Composite[TaskEvent] =
  Composite[(String, String)].xmap(
    (t: (String, String)) => decode[TaskEvent](t._2).fold(p => sys.error(p.toString), identity),
    (t: TaskEvent) => (t.id, t.asJson.noSpaces)
  )

val drop =
  sql"""
    DROP TABLE IF EXISTS events
  """.update.run
val create =
  sql"""
    CREATE TABLE events (
      id BIGINT auto_increment,
      agg_id VARCHAR NOT NULL,
      event TEXT
    )
  """.update.run
def insertTaskEvent(e: TaskEvent) = {
    sql"insert into events (agg_id, event) values (${e.id}, ${e.asJson.noSpaces})".update.run
}
def findEvents = sql"select agg_id, event from events".query[TaskEvent].list
val qs1 = drop *>
  create *>
  insertTaskEvent(TaskCommitted("2", "badger")) *>
  insertTaskEvent(TaskCompleted("3")) *>
  insertTaskEvent(TaskReopened("3")) *>
  findEvents
val qs = for {
  xa <- H2Transactor[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")
  _  <- xa.setMaxConnections(10)
  p  <- qs1.transact(xa)
} yield p
qs.run

