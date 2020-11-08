package example.maths

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, DefaultRealMatrixChangingVisitor, RealMatrix, RealVector}
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.util.FastMath
import org.apache.commons.math3.util.FastMath.sqrt

trait ApacheMathsPimps {

  implicit class PimpedRealVector(vec: RealVector) {
    def -(rhs: RealVector): RealVector = vec.subtract(rhs)
    def +(rhs: RealVector): RealVector = vec.add(rhs)
    def *(rhs: RealVector): RealVector = vec.ebeMultiply(rhs)
    def *(c: Double): RealVector = vec.mapMultiply(c)
    def /(c: Double): RealVector = vec.mapMultiply(1.0 / c)
    def /(rhs: RealVector): RealVector = vec.ebeDivide(rhs)
    def +(c: Double): RealVector = vec.mapAdd(c)
    def -(c: Double): RealVector = vec.mapAdd(-c)
    def exp: RealVector = {
      vec.map(FastMath.exp(_))
    }
    def length: Int = vec.getDimension
    def asColumnMatrix: RealMatrix = {
      val M = new Array2DRowRealMatrix(vec.length, 1)
      M.setColumnVector(0, vec)
      M
    }

    def maxIndex: Int = {
      var maxIndex = 0
      var maxValue = vec.getEntry(0)
      val N = vec.length
      var i = 1
      while (i < N) {
        val v = vec.getEntry(i)
        if (v > maxValue) {
          maxIndex = i
          maxValue = v
        }
        i += 1
      }
      maxIndex
    }
    def mean: Double = StatUtils.mean(vec.toArray)
    def variance: Double = StatUtils.variance(vec.toArray) max 0.0
    def stdDev: Double = sqrt(variance)
    def stdError: Double = stdDev / sqrt(vec.length)
    def apply(i: Int): Double = vec.getEntry(i)
  }

  implicit class PimpedApacheMatrix(mat: RealMatrix) {
    def + (rhs: RealMatrix) = mat.add(rhs)
    def apply(iRow: Int, iCol: Int): Double = mat.getEntry(iRow, iCol)
    def update(iRow: Int, iCol: Int, v: Double): Unit = {
      mat.setEntry(iRow, iCol, v)
    }
    def setAll(x: Double): RealMatrix = {
      mat.walkInOptimizedOrder(new DefaultRealMatrixChangingVisitor {
        override def visit(row: Int, column: Int, value: Double): Double = x
      })
      mat
    }
  }
}
