package essentialeffects

import cats.effect._

trait Zzz {
  /**
    * Sleep (semantically block) until wakeUp is invoked.
    *
    * @return an effect that will semantically block until wakeUp invoked
    */
  def sleep: IO[Unit]

  /**
    * Wake up (semantically unblock) any sleepers. No effect if already awake.
    */
  def wakeUp: IO[Unit]
}

object Zzz {
  def apply(): IO[Zzz] =
    Zzz(IO.unit)

  /**
    * @param barrierAwait an effect to run between `sleep` changing the tracked state and starting the semantic blocking. (useful for testing)
    */
  private[essentialeffects] def apply(barrierAwait: IO[Unit]): IO[Zzz] =
    Ref[IO]
      .of[ZzzState](Awake)
      .map { stateCell =>
        new Zzz {

          /**
            * Sleep (semantically block) until wakeUp is invoked.
            *
            * @return an effect that will semantically block until wakeUp invoked
            */
          override def sleep: IO[Unit] =
            Deferred[IO, Unit].flatMap { d2 =>
              stateCell.modify {
                case Asleep(d) => Asleep(d) -> d.get
                case Awake => Asleep(d2) -> d2.get
              }
            }
              .flatTap(_ => barrierAwait)
              .flatten
              .uncancelable

          /** Wake up (semantically unblock) any sleepers. No effect if already awake.
            */
          override def wakeUp: IO[Unit] =
            stateCell.modify {
              case Asleep(d) => Awake -> d.complete(()).void
              case Awake => Awake -> IO.unit
            }
              .flatten
              .uncancelable
        }
      }
}

sealed trait ZzzState
case class Asleep(s: Deferred[IO, Unit]) extends ZzzState
case object Awake extends ZzzState
