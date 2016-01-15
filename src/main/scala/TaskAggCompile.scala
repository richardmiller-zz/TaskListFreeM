package task.example

import cats.state._
import cats.~>
import TaskBehaviours._
import monocle.Iso
import monocle.function.At._
import ATaskEvents.EventStream
import monocle.function.Field1._
import monocle.function.Field2._
import monocle.std.map._
import monocle.std.tuple2._
import monocle.syntax._
import TaskProjections.Task.project
import task.example.TaskProjections.{EmptyTask, TaskProjection}

object TaskAggCompile {

  type TaskProjections = Map[String, TaskProjection]
  type Tasks = (Map[String, EventStream], TaskProjections)

  type MapState[A] = State[Tasks, A]

  object toState extends (TaskBehaviour ~> MapState) {

    private val root = Iso.id[Tasks]
    private val eventLog = root composeLens first
    private val projections = root composeLens second

    def projectEvents(id: String, es: EventStream): Tasks => Tasks = {
      projections composeLens at(id) modify (t => Some(project(t.getOrElse(EmptyTask), es)))
    }

    def appendEvents(id: String, es: EventStream): Tasks => Tasks = {
      eventLog composeLens at(id) modify (t => Some(t.getOrElse(List()) ++ es))
    }

    def appendIfEvents(id: String, es: EventStream): (Tasks => Tasks) = es match {
      case List() => identity
      case _ => appendEvents(id, es)
    }

    def modS[A](f: Tasks => Tasks, g: Unit => A = identity[Unit] _) = State.modify(f).map(g)

    override def apply[A](fa: TaskBehaviour[A]) = fa match {
      case Commit(id, text) => State.pure(TaskAgg.commit(id, text))
      case Complete(t) => State.pure(TaskAgg.complete(t))
      case Reopen(t) => State.pure(TaskAgg.reopen(t))
      case Find(id) => State.inspect(m => TaskAgg(id, m._1.getOrElse(id, List())))
      case Save(id, es) => modS(appendIfEvents(id, es))
      case Project(id, es) => modS(projectEvents(id, es))
    }
  }
}