package essentialeffects

import cats.effect._
import cats.syntax.all._

import scala.concurrent.duration._

object LetsDoThisThing extends IOApp.Simple {
  private val runManyTasks = false

  override def run: IO[Unit] =
    JobScheduler.resource(2).use { scheduler: JobScheduler =>
      val effect =
        if (runManyTasks)
          // TODO this version prints 2 hello worlds, then shuts down. Why only 2?
          (0 until 10).toList.traverse_ { _ =>
            scheduler.schedule(IO.println("hello world"))
          }
        else
          scheduler.schedule(IO.println("hello world"))

      effect >> IO.sleep(1.second)
    }
}
