package example.maths

import example.time.Day
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector}
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.util.FastMath.sqrt

case class BrownianMotionGenerator(marketDay: Day, observationDays: Seq[Day]) {
  def generate(nPaths: Int, seed: Long): Array2DRowRealMatrix = {
    val rng = new MersenneTwister(seed)
    val brownians = new Array2DRowRealMatrix(observationDays.size, nPaths)
    val times = observationDays.map(_.timeSince(marketDay))
    def brownianShifts(t0: Double, t1: Double) = {
      val t = t1 - t0
      new ArrayRealVector(Array.fill(nPaths)(rng.nextGaussian() * sqrt(t)))
    }
    brownians.setRowVector(0, brownianShifts(0, times(0)))
    for (i <- 1 until times.length) {
      brownians.setRowVector(
        i,
        brownians.getRowVector(i - 1).add(brownianShifts(times(i - 1), times(i)))
      )
    }
    brownians
  }

}
