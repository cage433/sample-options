package example

import example.time.Day

/**
 * Idealised version of a storage unit
 *
 * We have control of the unit between the first and last days, inclusive. Call these
 * the 'storage' days
 *
 * On the day preceding each storage day an exercise decision is made as to whether we
 *  a) Inject a unit of gas
 *  b) Do nothing
 *  c) Withdraw a unit of gas
 *
 * Subject to the constraint that we must return the unit at the terminal state at the end of the last day
 *
 * This constraint limits the range of states we are permitted to be in in the run up to the last day
 *
 */
case class GasStorageUnit(
  initialLevel: Int,
  terminalLevel: Int,
  maxLevel: Int,
  firstDay: Day,
  lastDay: Day
) {

  val numLevels: Int = 1 + maxLevel
  val numDays: Int = (lastDay - firstDay) + 1
  def postExerciseLevelRange(storageDay: Day): (Int, Int) = {
    val numStorageDaysLeftToExercise = lastDay - storageDay
    val low = Math.max(terminalLevel - numStorageDaysLeftToExercise, 0)
    val high = Math.min(terminalLevel + numStorageDaysLeftToExercise, maxLevel)
    (low, high)
  }

  def preExerciseLevelRange(storageDay: Day): (Int, Int) = {
    val (postLow, postHigh) = postExerciseLevelRange(storageDay)
    val preLow = Math.max(postLow - 1, 0)
    val preHigh = Math.min(postHigh + 1, maxLevel)
    (preLow, preHigh)
  }


  val storageDays: Seq[Day] = Day.daysBetween(firstDay, lastDay)

  val exerciseDays: Seq[Day] = Day.daysBetween(firstDay - 1, lastDay - 1)

  def possibleExerciseDecisions(storageDay: Day, preExerciseState: Int) : Seq[ExerciseDecision] = {
    val (preLow, preHigh) = preExerciseLevelRange(storageDay)
    require(
      preLow <= preExerciseState && preExerciseState <= preHigh,
      s"Invalid pre exercise state $preExerciseState for $storageDay"
    )
    ExerciseDecisions.values.filter {
      ex =>
        val postExerciseState = preExerciseState + ex.changeInState
        val (postLow, postHigh) = postExerciseLevelRange(storageDay)
        postLow <= postExerciseState && postExerciseState <= postHigh
    }
  }
}
