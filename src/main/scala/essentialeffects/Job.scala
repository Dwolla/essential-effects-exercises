package essentialeffects

import cats.effect._

import java.util.UUID

sealed trait Job

object Job {
  case class Scheduled(id: Id, task: IO[_]) extends Job {
    def start: IO[Job.Running] =
      for {
        exitCase <- Deferred[IO, ExitCase[Throwable]]
        fiber <- task.void.guaranteeCase {
          case Outcome.Succeeded(fa) =>
            fa.flatMap(_ => exitCase.complete(ExitCase.Completed).void)
          case Outcome.Errored(ex) =>
            exitCase.complete(ExitCase.Error(ex)).void
          case Outcome.Canceled() =>
            exitCase.complete(ExitCase.Canceled).void
        }.start
      } yield Job.Running(id, fiber, exitCase)
  }

  case class Running(id: Id,
                     fiber: Fiber[IO, Throwable, Unit],
                     exitCase: Deferred[IO, ExitCase[Throwable]]) extends Job {
    val await: IO[Completed] = exitCase.get.map(Completed(id, _))
  }

  case class Completed(id: Id, exitCase: ExitCase[Throwable]) extends Job

  case class Id(value: UUID) extends AnyVal

  def create[A](task: IO[A]): IO[Scheduled] = IO(Id(UUID.randomUUID())).map(Scheduled(_, task))
}
