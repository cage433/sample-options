package example

import java.lang.Math.{PI, sin}

import example.maths.ApacheMathsPimps
import example.time.Day
import org.apache.commons.math3.linear.{RealMatrix, RealVector}

/**
 * Creates a storage unit, then calculates the value/deltas a given number of times
 */
object TimeStorageValuation extends App with ApacheMathsPimps {
  val Array(numDays, numPaths, numRuns) = args.map(_.toInt)
  println(s"Num days $numDays")
  println(s"Num paths $numPaths")
  println(s"Num runs $numRuns")
  val firstStorageDay = Day(2021, 1, 1)
  val option = GasStorageUnit(
    initialLevel = 5,
    terminalLevel = 5,
    maxLevel = 10,
    firstDay = firstStorageDay,
    lastDay = firstStorageDay + (numDays - 1)
  )
  // Use prices that resemble a sin wave, we'd expect the positions to reflect the fact that
  // we inject on cheap days and withdraw on expensive ones - just done as a sanity check,
  // no algorithmic significance
  val forwardPrices = option.storageDays.map {
    d => d -> (100.0 + sin((d - firstStorageDay) / numDays.toDouble * 2.0 * PI))
  }.toMap
  val marketData = MarketData(
    forwardPrices, option.storageDays.map(d => d -> 0.2).toMap
  )
  val marketDay = Day(2020, 11, 1)

  val valuer = GasStorageUnitValuer(option, marketData, 12345, numPaths, marketDay)
  def calculateValuesAndDeltas(): (RealVector, RealMatrix) = {
    valuer.monteCarloValuesAndDeltas()
  }

  var mcValues: RealVector = null
  var mcDeltas: RealMatrix = null

  val t0 = System.currentTimeMillis()
  for (_ <- 0 until numRuns) {
    val foo = calculateValuesAndDeltas()
    mcValues = foo._1
    mcDeltas = foo._2
  }
  val t1 = System.currentTimeMillis()

  val value = mcValues.mean
  val se = mcValues.stdError
  val t = (t1 - t0) / 1000.0
  println(f"Time taken $t (s)")
  println(f"Value $value%1.3f ($se%1.3f)")
//  option.storageDays.zipWithIndex.foreach {
//    case (day, iDay) =>
//      val deltas = mcDeltas.getColumnVector(iDay)
//      val delta = deltas.mean
//      val dse = deltas.stdError
//      println(f"$day. Price ${forwardPrices(day)}%1.3f, Delta $delta%1.3f ($dse%1.3f)")
//  }
}

