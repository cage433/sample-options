package example.maths

import example.maths.OptionRight.{Call, Put, Straddle}
import example.maths.StatCalcs.{CND, ND}

import scala.math.{log, sqrt}

case class BlackScholes(right: OptionRight, F: Double, K: Double, vol: Double, T: Double) {
  require(vol >= 0, s"Black-Scholes vol cannot be negative - got $vol")

  private lazy val d1 = (log(F / K) + vol * vol / 2 * T) / (vol * sqrt(T))
  private lazy val d2 = d1 - vol * sqrt(T)
  private lazy val N1 = CND(d1)
  private lazy val Nm1 = CND(-d1)
  private lazy val N2 = CND(d2)
  private lazy val Nm2 = CND(-d2)

  val isWorthIntrinsic: Boolean = vol == 0.0 || T <= 0.0

  def undiscountedValue: Double = {
    if (isWorthIntrinsic)
      right.payoff(F, K)
    else
      right match {
        case Call     => F * N1 - K * N2
        case Put      => K * Nm2 - F * Nm1
        case Straddle => F * (N1 - Nm1) - K * (N2 - Nm2)
      }
  }

  def analyticDelta: Double = if (isWorthIntrinsic) {
    val callDelta = if (Call.isInTheMoney(F, K)) 1.0 else 0.0
    val putDelta = if (Put.isInTheMoney(F, K)) -1.0 else 0.0
    right match {
      case Call => callDelta
      case Put => putDelta
      case Straddle => callDelta + putDelta
    }
  } else {
    right match {
      case Call => N1
      case Put => N1 - 1.0
      case Straddle => 2 * N1 - 1.0
    }
  }

  def numericDelta(dF: Double): Double = {
    if (isWorthIntrinsic)
      analyticDelta
    else {
      val dF_ = dF min F / 4.0
      val up = copy(F = F + dF_).undiscountedValue
      val dn = copy(F = F - dF_).undiscountedValue
      (up - dn) / (2.0 * dF_)

    }
  }
  def analyticGamma: Double = if (isWorthIntrinsic)
    0.0
  else {
    val callOrPutGamma = ND(d1) / (F * vol * sqrt(T))
    right match {
      case Straddle => 2.0 * callOrPutGamma
      case _ => callOrPutGamma
    }
  }

  def numericGamma(dF: Double): Double = {
    if (isWorthIntrinsic)
      0.0
    else {
      val dF_ = dF min F / 4.0
      val up = copy(F = F + dF_).undiscountedValue
      val mid = undiscountedValue
      val dn = copy(F = F - dF_).undiscountedValue
      (up - 2.0 * mid + dn) / (dF_ * dF_)
    }
  }

  def analyticVega: Double = if (isWorthIntrinsic)
    0.0
  else {
    val callOrPutVega = F * ND(d1) * sqrt(T)
    right match {
      case Straddle => 2.0 * callOrPutVega
      case _ => callOrPutVega
    }
  }

  def numericVega(dVol: Double): Double = {
    if (isWorthIntrinsic)
      0.0
    else {
      val dV = dVol min vol / 4.0
      val up = copy(vol = vol + dV).undiscountedValue
      val dn = copy(vol = vol - dV).undiscountedValue
      (up - dn) / (2.0 * dV)
    }
  }

}
