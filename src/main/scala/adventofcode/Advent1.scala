package adventofcode

object Advent1:
  // part one
  def countIncreasedMeasurments(measurments: List[Int]): Int =
    measurments
      .sliding(2)
      .filter { case x :: y :: Nil => x < y }
      .length

  // part two
  def countIncreasedMeasurments2(measurments: List[Int]): Int =
    measurments
      .sliding(3)
      .map(_.sum)
      .toList
      .sliding(2)
      .filter { case x :: y :: Nil => x < y }
      .length
