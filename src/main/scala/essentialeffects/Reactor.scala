package essentialeffects

import cats.effect._
import cats.effect.std.Semaphore
import cats.syntax.all._

trait Reactor {
  def whenAwake(onStart: Job.Id => IO[Unit],
                onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]): IO[Unit]
}

object Reactor {
  def apply(stateRef: Ref[IO, JobScheduler.State], barrier: Semaphore[IO]): Reactor =
      new Reactor {
        def whenAwake(onStart: Job.Id => IO[Unit],
                      onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]): IO[Unit] = {
          def startNextJob: IO[Option[Job.Running]] =
            for {
              job <- stateRef.modify(_.dequeue)
              running <- job.traverse(barrier.acquire >> startJob(_))
            } yield running

          def startJob(scheduled: Job.Scheduled): IO[Job.Running] =
            for {
              running <- scheduled.start //(jobCompleted)
              _ <- stateRef.update(_.addRunning(running))
              _ <- registerOnComplete(running)
              _ <- onStart(running.id).attempt
            } yield running

          def registerOnComplete(job: Job.Running): IO[_] =
            job.fiber
              .join
              .guaranteeCase {
                case Outcome.Succeeded(fa) =>
                  fa
                    .map(_ => ExitCase.Completed)
                    .map(Job.Completed(job.id, _))
                    .flatMap(jobCompleted)

                case Outcome.Errored(ex) =>
                  jobCompleted(Job.Completed(job.id, ExitCase.Error(ex)))

                case Outcome.Canceled() =>
                  jobCompleted(Job.Completed(job.id, ExitCase.Canceled))
              }
              .flatMap(_ => barrier.release)
              .start

          def jobCompleted(job: Job.Completed): IO[Unit] =
            stateRef
              .update((state: JobScheduler.State) => state.onComplete(job))
              .flatTap(_ => onComplete(job.id, job.exitCase).attempt)

          startNextJob.iterateUntil(_.isEmpty).void
        }
      }

}
