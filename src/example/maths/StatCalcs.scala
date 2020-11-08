package example.maths

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.exception.OutOfRangeException
import org.apache.commons.math3.linear.{DefaultRealMatrixChangingVisitor, RealMatrix}
import org.apache.commons.math3.util.FastMath

object StatCalcs extends ApacheMathsPimps {

  private val standardNormal = new NormalDistribution(0, 1)

  def ND(x: Double): Double = standardNormal.density(x)

  /**
   * From http://www.codeplanet.eu/files/download/accuratecumnorm.pdf
   * and https://stackoverflow.com/a/23119456/190452
   *
   * Tested against Apache Maths' CND function in StatCalcsTests. More than
   * 20 times faster than Apache's implementation.
   */
  def CND(x: Double): Double = {
    val RT2PI = 2.5066282746310002 // sqrt(4.0 * acos(0.0))

    val SPLIT = 7.07106781186547

    val N0 = 220.206867912376
    val N1 = 221.213596169931
    val N2 = 112.079291497871
    val N3 = 33.912866078383
    val N4 = 6.37396220353165
    val N5 = 0.700383064443688
    val N6 = 3.52624965998911e-02
    val M0 = 440.413735824752
    val M1 = 793.826512519948
    val M2 = 637.333633378831
    val M3 = 296.564248779674
    val M4 = 86.7807322029461
    val M5 = 16.064177579207
    val M6 = 1.75566716318264
    val M7 = 8.83883476483184e-02

    val z = FastMath.abs(x)
    var c = 0.0

    if (z <= 37.0) {
      val e = FastMath.exp(-z * z / 2.0)
      if (z < SPLIT) {
        val n = (((((N6 * z + N5) * z + N4) * z + N3) * z + N2) * z + N1) * z + N0
        val d = ((((((M7 * z + M6) * z + M5) * z + M4) * z + M3) * z + M2) * z + M1) * z + M0
        c = e * n / d
      } else {
        val f = z + 1.0 / (z + 2.0 / (z + 3.0 / (z + 4.0 / (z + 13.0 / 20.0))))
        c = e / (RT2PI * f)
      }
    }
    if (x <= 0.0) c else 1 - c
  }

  def uniformToNormal(_u: Double): Double = {
    if (_u < 0.0 || _u > 1.0) throw new OutOfRangeException(_u, 0, 1)
    // Apache's inverseCumulativeProbability doesn't handle very small numbers or numbers too close to 1
    val u = _u max 1e-16 min (1 - 1e-16)
    standardNormal.inverseCumulativeProbability(u)
  }

  def convertUniformToNormals(M: RealMatrix): Unit = {
    M.walkInOptimizedOrder(new DefaultRealMatrixChangingVisitor{
      override def visit(row: Int, column: Int, value: Double): Double = uniformToNormal(value)
    })
  }

}
