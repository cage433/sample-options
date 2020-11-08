package example.time

case class Day(y: Int, m: Int, d: Int) extends Ordered[Day] {

  lazy val julianDayNumber: Int = {
    var (d1, m1, y1) = (d, m, y)
    if (m1 > 2) {
      m1 -= 3
    } else {
      m1 += 9
      y1 -= 1
    }
    val c: Int = y1 / 100
    val ya: Int = y1 - 100 * c
    ((146097 * c) >> 2) + ((1461 * ya) >> 2) + (153 * m1 + 2) / 5 + d1 + 1721119
  }

  override def compare(other: Day): Int = julianDayNumber - other.julianDayNumber
  val containingMonth: Month = Month(y, m)

  def +(n: Int) = Day.fromJulianDayNumber(julianDayNumber + n)
  def -(n: Int) = this + (-n)
  def -(other: Day): Int = julianDayNumber - other.julianDayNumber
  def timeSince(other: Day): Double = (this - other) / 365.0
}

object Day {

  def daysBetween(from: Day, to: Day): IndexedSeq[Day] = {
    Range.inclusive(from.julianDayNumber, to.julianDayNumber).map(fromJulianDayNumber)
  }

  def fromJulianDayNumber(n: Int) = {
    var j = n - 1721119
    var y = (4 * j - 1) / 146097
    j = 4 * j - 1 - 146097 * y
    var d = j / 4
    j = (4 * d + 3) / 1461
    d = 4 * d + 3 - 1461 * j
    d = (d + 4) / 4
    var m = (5 * d - 3) / 153
    d = 5 * d - 3 - 153 * m
    d = (d + 5) / 5
    y = 100 * y + j

    if (m < 10)
      m = m + 3
    else {
      m = m - 9
      y = y + 1
    }
    Day(y, m, d)
  }
}