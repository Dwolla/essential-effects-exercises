package essentialeffects

import cats.effect._
import cats.syntax.all._

trait Reactor {
  def whenAwake(onStart: Job.Id => IO[Unit],
                onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]): IO[Unit]
}

object Reactor {
  def apply(stateRef: Ref[IO, JobScheduler.State]): Reactor =
    new Reactor {
      def whenAwake(onStart: Job.Id => IO[Unit],
                    onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]): IO[Unit] = {
        def startNextJob: IO[Option[Job.Running]] =
          for {
            job <- stateRef.modify(_.dequeue)
            running <- job.traverse(startJob)
          } yield running

        def startJob(scheduled: Job.Scheduled): IO[Job.Running] =
          for {
            running <- scheduled.start
            _ <- stateRef.update(_.addRunning(running))
            _ <- registerOnComplete(running)
            _ <- onStart(running.id).attempt
          } yield running

        def registerOnComplete(job: Job.Running) =
          job.await
            .flatMap(jobCompleted)
            .start

        def jobCompleted(job: Job.Completed): IO[Unit] =
          stateRef
            .update((state: JobScheduler.State) => state.onComplete(job))
            .flatTap(_ => onComplete(job.id, job.exitCase).attempt)

        startNextJob.iterateUntil(_.isEmpty).void
      }
    }

}
