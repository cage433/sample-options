package example.time

case class Month(y: Int, m: Int) extends Ordered[Month] {
  def firstDay = Day(y, m, 1)

  override def compare(other: Month): Int = firstDay.compare(other.firstDay)
}
