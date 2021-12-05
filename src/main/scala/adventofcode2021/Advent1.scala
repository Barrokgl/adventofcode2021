package adventofcode2021

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

  def run(input: List[String]) =
    val inputInts = input.map(_.toInt)
    (countIncreasedMeasurments(inputInts), countIncreasedMeasurments2(inputInts))
