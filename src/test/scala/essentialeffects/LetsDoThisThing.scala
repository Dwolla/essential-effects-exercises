package essentialeffects

import cats.effect._
import cats.syntax.all._

import scala.concurrent.duration._

object LetsDoThisThing extends IOApp.Simple {
  private val runManyTasks = true

  private def doSomething(i: Int): IO[Unit] =
    IO.println(s"task $i").delayBy(1.second)

  override def run: IO[Unit] =
    JobScheduler.resource(5).use { scheduler: JobScheduler =>
      val effect: IO[Unit] =
        if (runManyTasks)
          (0 until 50).toList.traverse_ { i =>
            scheduler.schedule(doSomething(i))
          }
        else
          scheduler.schedule(IO.println("hello world")).void

      effect
    } //.andWait(3.seconds)

}
