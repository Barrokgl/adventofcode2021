package adventofcode

object Advent2:
  // part one
  case class Position(horizontal: Int, depth: Int)
  def followRoute1(route: List[String]): Position =
    def moveSubmarine(direction: String, position: Position) = direction match
      case s"forward $x" => position.copy(horizontal = position.horizontal + x.toInt)
      case s"down $x"    => position.copy(depth = position.depth + x.toInt)
      case s"up $x"      => position.copy(depth = position.depth - x.toInt)
    route.foldLeft(Position(0, 0))((position, direction) => moveSubmarine(direction, position))

  // part two
  case class PositionWithAim(horizontal: Int, depth: Int, aim: Int)
  def followRoute2(route: List[String]): PositionWithAim =
    def moveSubmarine(direction: String, position: PositionWithAim) = direction match
      case s"forward $x" =>
        position.copy(
          horizontal = position.horizontal + x.toInt,
          depth = position.depth + (position.aim * x.toInt)
        )
      case s"down $x" => position.copy(aim = position.aim + x.toInt)
      case s"up $x"   => position.copy(aim = position.aim - x.toInt)
    route.foldLeft(PositionWithAim(0, 0, 0))((position, direction) =>
      moveSubmarine(direction, position)
    )

  // part one alternative
  def followRoute3(route: List[String]): (Int, Int) =
    route.foldLeft((0, 0))((position, direction) =>
      (direction, position) match
        case (s"forward $x", (horizontal, depth)) => (horizontal + x.toInt, depth)
        case (s"down $x", (horizontal, depth))    => (horizontal, depth + x.toInt)
        case (s"up $x", (horizontal, depth))      => (horizontal, depth - x.toInt)
    )

  // part two alternative
  def followRoute4(route: List[String]): (Int, Int, Int) =
    route.foldLeft((0, 0, 0))((position, direction) =>
      (direction, position) match
        case (s"forward $x", (horizontal, depth, aim)) =>
          (horizontal + x.toInt, depth + (aim * x.toInt), aim)
        case (s"down $x", (horizontal, depth, aim)) => (horizontal, depth, aim + x.toInt)
        case (s"up $x", (horizontal, depth, aim))   => (horizontal, depth, aim - x.toInt)
    )
  def run(input: List[String]) =
    (followRoute1(input), followRoute2(input))
