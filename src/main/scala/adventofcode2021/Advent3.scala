package adventofcode2021

import scala.annotation.tailrec

object Advent3:
  val example = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  type Ones = Int
  type Zeros = Int
  type Gamma = String
  type Epsilon = String
  // part one
  def binaryDiagnostic(input: List[String]) =
    input
      .map(_.split("").toList)
      .transpose
      .map(binariesRow =>
        binariesRow.foldRight[(Ones, Zeros)]((0, 0))((num, acc) => countBinary(num, acc))
      )
      .foldLeft[(Gamma, Epsilon)](("", ""))((acc, a) =>
        (a, acc) match
          case ((ones, zeros), (gamma, epsilon)) if ones > zeros => (gamma + "1", epsilon + "0")
          case ((ones, zeros), (gamma, epsilon))                 => (gamma + "0", epsilon + "1")
      ) match
      case (gammaStr, epsilonStr) =>
        Integer.parseInt(gammaStr, 2) * Integer.parseInt(epsilonStr, 2)

  def countBinary(num: String, acc: (Ones, Zeros)): (Ones, Zeros) = (num, acc) match
    case ("1", (ones, zeros)) => (ones + 1, zeros)
    case ("0", (ones, zeros)) => (ones, zeros + 1)

  // part two
  def binaryDiagnostic2(input: List[String]) =
    val inputTransposed = input.map(_.split("").toList).transpose
    @tailrec
    def helper(
        in: List[List[String]],
        filterFun: ((n: Ones, t: Zeros, row: List[String]) => Boolean),
        position: Int
    ): String =
      val (ones, zeros) = in
        .drop(position)
        .headOption
        .map(_.foldRight[(Ones, Zeros)]((0, 0))((num, acc) => countBinary(num, acc)))
        .getOrElse((0, 0))
      in.transpose.filter(row => filterFun(ones, zeros, row.drop(position))) match
        case x :: Nil => x.transpose.reduceLeft(_ ++ _).mkString
        case y        => helper(y.transpose, filterFun, position + 1)

    val oxygen = helper(
      inputTransposed,
      {
        case (ones: Ones, zeros: Zeros, row: List[String]) if ones < zeros =>
          row.headOption.exists(_ == "0")
        case (ones: Ones, zeros: Zeros, row: List[String]) => row.headOption.exists(_ == "1")
      },
      0
    )
    val co2 = helper(
      inputTransposed,
      {
        case (ones: Ones, zeros: Zeros, row: List[String]) if ones < zeros =>
          row.headOption.exists(_ == "1")
        case (ones: Ones, zeros: Zeros, row: List[String]) => row.headOption.exists(_ == "0")
      },
      0
    )
    Integer.parseInt(oxygen, 2) * Integer.parseInt(co2, 2)

  def run(input: List[String]) =
    (binaryDiagnostic(input), binaryDiagnostic2(input))
