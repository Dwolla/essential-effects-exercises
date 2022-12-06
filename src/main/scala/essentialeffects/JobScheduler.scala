package essentialeffects

import cats.data.Chain
import cats.effect._
import cats.effect.syntax.all._

trait JobScheduler {
  def schedule(task: IO[_]): IO[Job.Id]
}

object JobScheduler {
  case class State(maxRunning: Int,
                   scheduled: Chain[Job.Scheduled] = Chain.empty,
                   running: Map[Job.Id, Job.Running] = Map.empty,
                   completed: Chain[Job.Completed] = Chain.empty
                  ) {
    def enqueue(job: Job.Scheduled): State =
      copy(scheduled = scheduled :+ job)

    def dequeue: (State, Option[Job.Scheduled]) =
      if (running.size >= maxRunning) this -> None
      else
        scheduled.uncons.map {
          case (head, tail) =>
            copy(scheduled = tail) -> Some(head)
        }
          .getOrElse(this -> None)

    // TODO we invented this, maybe it's wrong
    def onComplete(job: Job.Completed): State =
      copy(completed = completed :+ job)

    // TODO we invented this, maybe it's wrong
    def addRunning(job: Job.Running): State =
      copy(running = running + (job.id -> job))
  }

  def scheduler(stateRef: Ref[IO, State], zzz: Zzz): JobScheduler = new JobScheduler {
    def schedule(task: IO[_]): IO[Job.Id] =
      for {
        job <- Job.create(task)
        _ <- stateRef.update(_.enqueue(job))
        _ <- zzz.wakeUp
      } yield job.id
  }

  def resource(maxRunning: Int): Resource[IO, JobScheduler] =
    for {
      schedulerState <- Ref[IO].of(JobScheduler.State(maxRunning)).toResource
      zzz <- Zzz().toResource
      scheduler = JobScheduler.scheduler(schedulerState, zzz)
      reactor = Reactor(schedulerState)
      onStart = (_: Job.Id) => IO.unit
      onComplete = (_: Job.Id, _: ExitCase[Throwable]) => zzz.wakeUp
      loop = (zzz.sleep *> reactor.whenAwake(onStart, onComplete)).foreverM
      _ <- loop.background
    } yield scheduler
}
