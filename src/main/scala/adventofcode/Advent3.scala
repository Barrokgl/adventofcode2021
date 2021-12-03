package adventofcode

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
  assert(binaryDiagnostic(example) == 198)
  assert(binaryDiagnostic2(example) == 230)
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
          case ((ones, zeros), (gamma, epsilon)) if ones > zeros => (epsilon + "1", gamma + "0")
          case ((ones, zeros), (gamma, epsilon))                 => (epsilon + "0", gamma + "1")
      ) match
      case (gammaStr, epsilonStr) =>
        Integer.parseInt(gammaStr, 2) * Integer.parseInt(epsilonStr, 2)

  def countBinary(num: String, acc: (Ones, Zeros)): (Ones, Zeros) = (num, acc) match
    case ("1", (ones, zeros)) => (ones + 1, zeros)
    case ("0", (ones, zeros)) => (ones, zeros + 1)
