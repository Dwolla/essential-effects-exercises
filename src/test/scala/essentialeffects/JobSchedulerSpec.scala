package essentialeffects

import cats.effect._
import cats.syntax.all._
import munit._
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen, Shrink, Test}

import scala.concurrent.duration._

class JobSchedulerSpec
  extends CatsEffectSuite
    with ScalaCheckEffectSuite {

  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(10000)

  private implicit val arbInt: Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 100))
  private implicit val shrinkInt: Shrink[Int] = Shrink.shrinkAny

  test("scheduler runs all the jobs that are scheduled") {
    PropF.forAllF { numberOfActions: Int =>
      for {
        _ <- IO.println(s"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nstarting test with $numberOfActions")
        ref <- Ref[IO].of(Set.empty[Int])
        scheduledJobIds <- JobScheduler.resource(1).use { scheduler =>
          (0 until numberOfActions).toList.traverse { i =>
            scheduler.schedule {
              IO.println(s"adding $i") >> ref.update(_ + i)
            }
          }
        }
        _ <- IO.println("JobScheduler.resource is closed. getting Ref[IO, Set[Int]]")
        completed <- ref.get.iterateUntil(_.size == numberOfActions).timeout(1.second)
      } yield {
        assertEquals((0 until 0).toSet, Set.empty[Int])
        assertEquals(scheduledJobIds.length, numberOfActions)
        assertEquals(completed, (0 until numberOfActions).toSet)
      }
    }
  }
}
