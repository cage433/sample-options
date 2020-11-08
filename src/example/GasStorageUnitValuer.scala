package example

import example.maths.{ApacheMathsPimps, BrownianMotionGenerator}
import example.time.Day
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, RealMatrix, RealVector, SingularValueDecomposition}
import org.apache.commons.math3.util.FastMath.sqrt

/**
 * American Monte Carlo valuer of a gas storage unit
 */
case class GasStorageUnitValuer(
  option: GasStorageUnit,
  marketData: MarketData,
  seed: Long,
  numPaths: Int,
  marketDay: Day
) extends ApacheMathsPimps{
  import option.{numLevels, numDays}

  private val brownians: Array2DRowRealMatrix = BrownianMotionGenerator(
    marketDay, option.exerciseDays
  ).generate(numPaths, seed)

  /**
   * Returns the monte carlo prices for the given storage day observed on the preceding day (i.e. the exercise day)
   */
  private def monteCarloForwardPrices(iStorageDay: Int): RealVector = {
    val bs = brownians.getRowVector(iStorageDay)
    val storageDay = option.storageDays(iStorageDay)
    val vol = marketData.vols(storageDay)
    val F = marketData.forwardPrices(storageDay)
    val t = option.exerciseDays(iStorageDay).timeSince(marketDay)
    (bs * vol * sqrt(t) - 0.5 * vol * vol * t).exp * F
  }

  def monteCarloValuesAndDeltas(): (RealVector, RealMatrix) = {

    var preExerciseValues =  new Array2DRowRealMatrix(numLevels, numPaths)
    var postExerciseValues =  new Array2DRowRealMatrix(numLevels, numPaths).setAll(0.0).asInstanceOf[Array2DRowRealMatrix]
    var preExerciseDeltas = Array.fill(numLevels, numPaths, numDays)(0.0)
    var postExerciseDeltas = Array.fill(numLevels, numPaths, numDays)(0.0)
    val conditionalExpectations =  new Array2DRowRealMatrix(numLevels, numPaths)
    val designMatrix = new Array2DRowRealMatrix(numPaths, 3)

    option.storageDays.zipWithIndex.reverse.foreach {
      case (storageDay, iStorageDay) =>
        val (postLow, postHigh) = option.postExerciseLevelRange(storageDay)

        val bs = brownians.getRowVector(iStorageDay)
        designMatrix.setColumn(0, Array.fill(numPaths)(1.0))
        designMatrix.setColumnVector(1, bs)
        designMatrix.setColumnVector(2, bs)
        val solver = new SingularValueDecomposition(designMatrix).getSolver

        for (postExerciseLevel <- postLow to postHigh) {
          val P = postExerciseValues.getRowVector(postExerciseLevel)
          val coeffs = solver.solve(P)
          conditionalExpectations.setRowVector(
            postExerciseLevel,
            designMatrix.multiply(coeffs.asColumnMatrix).getColumnVector(0)
          )
        }

        val prices = monteCarloForwardPrices(iStorageDay)
        val (preExerciseMinLevel, preExerciseMaxLevel) = option.preExerciseLevelRange(storageDay)
        for (preExerciseLevel <- preExerciseMinLevel to preExerciseMaxLevel) {
          val exerciseDecisions = option.possibleExerciseDecisions(storageDay, preExerciseLevel)
          val possiblePostExerciseLevels = exerciseDecisions.map(preExerciseLevel + _.changeInState)
          val payoffs = {
            val P = new Array2DRowRealMatrix(exerciseDecisions.length, numPaths)
            exerciseDecisions.zipWithIndex.foreach {
              case (exerciseDecision, iExerciseDecision) =>
                P.setRowVector(
                  iExerciseDecision,
                  prices * (exerciseDecision.changeInState * -1.0)
                )
            }
            P
          }
          val conditionalPostExValues = payoffs + new Array2DRowRealMatrix(conditionalExpectations.getDataRef.slice(possiblePostExerciseLevels.head, possiblePostExerciseLevels.head + possiblePostExerciseLevels.length), false)
          for (iPath <- 0 until numPaths) {
            val iBestExercise = conditionalPostExValues.getColumnVector(iPath).maxIndex
            val bestExercise = exerciseDecisions(iBestExercise)
            val postExerciseLevel = preExerciseLevel + bestExercise.changeInState
            preExerciseValues(preExerciseLevel, iPath) = postExerciseValues(postExerciseLevel, iPath) + payoffs(iBestExercise, iPath)
            preExerciseDeltas(preExerciseLevel)(iPath) = postExerciseDeltas(postExerciseLevel)(iPath).clone()
            preExerciseDeltas(preExerciseLevel)(iPath)(iStorageDay) = prices(iPath) / marketData.forwardPrices(storageDay) * -bestExercise.changeInState

          }
        }

        val tmp = postExerciseValues
        postExerciseValues = preExerciseValues
        preExerciseValues = tmp
        val tmp2 = postExerciseDeltas
        postExerciseDeltas = preExerciseDeltas
        preExerciseDeltas = tmp2

    }
    (postExerciseValues.getRowVector(option.initialLevel), new Array2DRowRealMatrix(postExerciseDeltas(option.initialLevel)))

  }
}
