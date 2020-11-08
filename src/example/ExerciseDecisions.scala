package example

import enumeratum.values.{IntEnum, IntEnumEntry}

sealed abstract class ExerciseDecision(val value: Int) extends IntEnumEntry {
  def changeInState: Int
}

object ExerciseDecisions extends IntEnum[ExerciseDecision] {
  override def values: IndexedSeq[ExerciseDecision] = findValues
  case object Withdraw extends ExerciseDecision(2) {
    def changeInState: Int = -1
  }
  case object DoNothing extends ExerciseDecision(1) {
    def changeInState: Int = 0
  }
  case object Inject extends ExerciseDecision(0) {
    def changeInState: Int = 1
  }
}
