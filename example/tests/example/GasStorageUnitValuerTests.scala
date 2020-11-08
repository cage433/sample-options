package example

import example.maths.OptionRight.{Call, Put}
import example.maths.{ApacheMathsPimps, BlackScholes}
import example.time.Day
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GasStorageUnitValuerTests
  extends AnyFreeSpec
  with Matchers
  with ApacheMathsPimps
{

  "Emulate vanilla option value" in {
    /**
     * Create a storage option whose value should match that of a vanilla european
     */
    val firstStorageDay = Day(2021, 1, 1)
    val lastStorageDay = Day(2021, 1, 2)
    val option = GasStorageUnit(
      initialLevel = 0,
      terminalLevel = 0,
      maxLevel = 1,
      firstDay = firstStorageDay,
      lastDay = lastStorageDay
    )
    val F = 100.0
    val K = 101.0
    val vol = 0.2
    val marketData = MarketData(
      forwardPrices = Map(firstStorageDay -> F, lastStorageDay -> K),
      vols = Map(firstStorageDay -> vol, lastStorageDay -> 0.0)
    )
    val marketDay = Day(2020, 1, 1)
    val (mcValues, mcDeltas) = GasStorageUnitValuer(option, marketData, seed=12345, numPaths = 100000, marketDay).monteCarloValuesAndDeltas()
    val mcValue = mcValues.mean
    val se = mcValues.stdError
    val T = (firstStorageDay - 1).timeSince(marketDay)
    val bsValue = BlackScholes(Put, F, K, vol, T).undiscountedValue
    val bsDelta = BlackScholes(Put, F, K, vol, T).analyticDelta
    mcValue shouldBe(bsValue +- 3.0 * se)

    val pathwiseDeltaSamples = mcDeltas.getColumnVector(0)
    val pathwiseDelta = pathwiseDeltaSamples.mean
    val pwdSe = pathwiseDeltaSamples.stdError
    pathwiseDelta shouldBe (bsDelta +- 3.0 * pwdSe)
  }

}
