package adventofcode2021

import scala.annotation.tailrec

object Advent6:
  val example = List("3,4,3,1,2")

  def checkSpawn(fishes: Map[Int, Long]): Map[Int, Long] =
    fishes.get(0) match
      case Some(n) =>
        (
          (
            fishes + (7 -> (fishes.getOrElse(7, 0L) + n))
          ) + (9 -> (fishes.getOrElse(9, 0L) + n))
        ) - 0
      case None => fishes

  def tickTimer(fishes: Map[Int, Long]): Map[Int, Long] =
    fishes.map[Int, Long]((d, n) => (d - 1 -> n))

  @tailrec
  def simulateLanternfish(fishes: Map[Int, Long], day: Int): Long =
    day match
      case n if n <= 0 => fishes.values.sum
      case _ =>
        simulateLanternfish(checkSpawn.andThen(tickTimer)(fishes), day - 1)

  def parseInput2(input: List[String]) =
    input.head
      .split(",")
      .map(_.toInt)
      .foldLeft(Map.empty[Int, Long])((acc, n) => acc + (n -> (acc.getOrElse(n, 0L) + 1L)))

  def run(input: List[String]) =
    val fishes = parseInput2(input)
    (simulateLanternfish(fishes, 80), simulateLanternfish(fishes, 256))
