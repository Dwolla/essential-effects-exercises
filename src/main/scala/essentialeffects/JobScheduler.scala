package essentialeffects

import cats.data.Chain
import cats.effect._
import cats.effect.std.Semaphore
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
    def pending: Boolean = scheduled.nonEmpty || running.nonEmpty

    def enqueue(job: Job.Scheduled): State =
      copy(scheduled = scheduled :+ job)

    def dequeue: (State, Option[Job.Scheduled]) =
//      if (running.size >= maxRunning) this -> None
//      else
        scheduled.uncons.map {
          case (head, tail) =>
            copy(scheduled = tail) -> Some(head)
        }
          .getOrElse(this -> None)

    // TODO we invented this, maybe it's wrong
    def onComplete(job: Job.Completed): State =
      copy(running = running - job.id, completed = completed :+ job)

    // TODO we invented this, maybe it's wrong
    def addRunning(job: Job.Running): State =
      copy(running = running + (job.id -> job))
  }

  def scheduler(stateRef: Ref[IO, State], zzz: Zzz): JobScheduler = new JobScheduler {
    def schedule(task: IO[_]): IO[Job.Id] =
      for {
        job <- Job.create(task)
        newState <- stateRef.updateAndGet(_.enqueue(job))
        _ <- IO.println(s"${newState.scheduled.size} jobs scheduled")
        _ <- zzz.wakeUp
      } yield job.id
  }

  def resource(maxRunning: Int): Resource[IO, JobScheduler] =
    for {
      barrier <- Semaphore[IO](maxRunning).toResource
      schedulerState <- Ref[IO].of(JobScheduler.State(maxRunning)).toResource
      zzz <- Zzz().toResource
      scheduler = JobScheduler.scheduler(schedulerState, zzz)
      reactor = Reactor(schedulerState, barrier)
      onStart = (_: Job.Id) => IO.unit
      onComplete = (_: Job.Id, _: ExitCase[Throwable]) => zzz.wakeUp
      loop = (zzz.sleep *> reactor.whenAwake(onStart, onComplete)).foreverM
      _ <- loop.background
      _ <- Resource.onFinalize(IO.println("waitForScheduledTasksToComplete is complete"))
      _ <- Resource.onFinalize(waitForScheduledTasksToComplete(zzz, schedulerState))
      _ <- Resource.onFinalize(IO.println("starting to finalize"))
    } yield scheduler

  def waitForScheduledTasksToComplete(zzz: Zzz, schedulerState: Ref[IO, State]): IO[Unit] =
    schedulerState
      .get
      .map(!_.pending)
      .ifM(
        IO.unit,
        IO.println("things are still scheduled or running, waiting") >>
//        zzz.sleep >> // TODO if we sleep here, we risk a deadlock, but it significantly reduces contention!
          waitForScheduledTasksToComplete(zzz, schedulerState)
      )
}
