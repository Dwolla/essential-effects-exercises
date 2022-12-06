package essentialeffects

import cats.effect._
import cats.effect.std._
import cats.syntax.all._
import munit._
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen, Shrink, Test}

class ZzzSpec
  extends CatsEffectSuite
    with ScalaCheckEffectSuite {

  private val logBetweenTestRuns = false

  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(10000)

  test("calling wakeUp wakes up everything that is sleeping") {
    PropF.forAllF { (numberOfSleeps: NumberOfSleeps) =>
      IO.println {
        s"""********************************************************************************
           |********************************************************************************
           |starting test with numberOfSleeps = $numberOfSleeps""".stripMargin
      }.whenA(logBetweenTestRuns) >>
        Supervisor[IO].use { supervisor =>
          for {
            // create a list of Deferreds to track when each sleep has completed
            sleepTrackers <- (0 until numberOfSleeps.value).toList.traverse { _ =>
              Deferred[IO, Unit].map(SleepTracker)
            }

            barrier <- CyclicBarrier[IO](numberOfSleeps.value + 1)

            // start our concurrent state machine
            zzz <- Zzz(barrier.await)

            // create a deferred per sleep, to complete after the sleep ends via wakeup
            sleepsComplete <- sleepTrackers.parTraverse { case SleepTracker(sleepTracker) =>
              supervisor.supervise {
                zzz.sleep >> sleepTracker.complete(())
              }
            }

            // hold until all the sleeps have been scheduled
            _ <- barrier.await

            blockUntilSleepDeferredAreDone = sleepsComplete.parTraverse(_.join.flatMap {
              case Outcome.Succeeded(fa) => fa
              case Outcome.Errored(ex) => IO.raiseError(ex)
              case Outcome.Canceled() => IO.raiseError(new RuntimeException("a sleep was canceled"))
            })

            race <- IO.race(blockUntilSleepDeferredAreDone, zzz.wakeUp)

            completedSleeps <- blockUntilSleepDeferredAreDone
          } yield {
            // are all the sleeps complete?
            assert(completedSleeps.forall(identity))

            // did wakeUp finish before all the sleeps?
            assertEquals(race, Right(()))
          }
        }
    }
  }
}

case class SleepTracker(sleepHasEnded: Deferred[IO, Unit])

case class NumberOfSleeps(value: Int) {
  override def toString: String = value.toString
}
object NumberOfSleeps {
  implicit val arbSleeps: Arbitrary[NumberOfSleeps] = Arbitrary {
    Gen.chooseNum(0, 100).map(NumberOfSleeps(_))
  }

  implicit val shrinkSleeps: Shrink[NumberOfSleeps] = Shrink.shrinkAny
}
