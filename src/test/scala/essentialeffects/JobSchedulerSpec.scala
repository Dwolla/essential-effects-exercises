package essentialeffects

import cats.effect._
import cats.syntax.all._
import munit._
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.concurrent.duration._

class JobSchedulerSpec
  extends CatsEffectSuite
    with ScalaCheckEffectSuite {

  private implicit val arbInt: Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 10))
  private implicit val shrinkInt: Shrink[Int] = Shrink.shrinkAny

  override def scalaCheckInitialSeed = "BT0kxuxlft-8IGcDzQPUu_oFLESUXch5O-zMeCXwBtL="

  test("scheduler runs all the jobs that are scheduled") {
    PropF.forAllF { numberOfActions: Int =>
      for {
        _ <- IO.println(s"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nstarting test with $numberOfActions")
        ref <- Ref[IO].of(Set.empty[Int])
        scheduledJobIds <- JobScheduler.resource(numberOfActions).use { scheduler =>
          (0 until numberOfActions).toList.traverse { i =>
            scheduler.schedule {
              IO.println(s"adding $i") >> ref.update(_ + i)
            }
          }
        }
        completed <- ref.get
      } yield {
        assertEquals((0 until 0).toSet, Set.empty[Int])
        assertEquals(scheduledJobIds.length, numberOfActions)
        assertEquals(completed, (0 until numberOfActions).toSet)
      }
    }
  }
}
