package example.maths

import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.collection.immutable

sealed abstract class OptionRight(val value: Int, val name: String) extends IntEnumEntry {
  override def toString = name
  def payoff(S: Double, K: Double): Double
  def toChar = toString.head.toLower
  def isInTheMoney(S: Double, K: Double): Boolean = payoff(S, K) > 0
  def exerciseMultiplier(S: Double, K: Double): Int = {
    if (payoff(S, K) > 0)
      if (S > K) 1 else -1
    else
      0
  }
}


object OptionRight extends IntEnum[OptionRight] {
  val values: immutable.IndexedSeq[OptionRight] = findValues

  case object Call extends OptionRight(1, "Call") {
    def payoff(S: Double, K: Double): Double = math.max(S - K, 0)
  }

  case object Put extends OptionRight(2, "Put") {
    def payoff(S: Double, K: Double): Double = math.max(K - S, 0)
  }

  case object Straddle extends OptionRight(3, "Straddle") {
    def payoff(S: Double, K: Double): Double = math.max(K - S, S - K)
  }

}
