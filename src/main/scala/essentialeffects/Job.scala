package essentialeffects

import cats.effect._

import java.util.UUID

sealed trait Job

object Job {
  case class Scheduled(id: Id, task: IO[_]) extends Job {
    def start: IO[Running] =
      task.void.start.map(Running(id, _))

//    def start(supervisor: Supervisor[IO]): IO[Running] =
//      for {
//        fiber <- supervisor.supervise(task.void)
//      } yield Job.Running(id, fiber)
  }

  case class Running(id: Id,
                     fiber: Fiber[IO, Throwable, Unit]
                    ) extends Job

  case class Completed(id: Id, exitCase: ExitCase[Throwable]) extends Job

  case class Id(value: UUID) extends AnyVal

  def create[A](task: IO[A]): IO[Scheduled] = IO(Id(UUID.randomUUID())).map(Scheduled(_, task))
}
