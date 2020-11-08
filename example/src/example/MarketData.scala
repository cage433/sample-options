package example

import example.time.Day

case class MarketData(
  forwardPrices: Map[Day, Double],
  vols: Map[Day, Double],
)
