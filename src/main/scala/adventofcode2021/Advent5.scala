package adventofcode2021

import scala.annotation.tailrec
import scala.util.Try

object Advent5:
  val example = """
  0,9 -> 5,9
  8,0 -> 0,8
  9,4 -> 3,4
  2,2 -> 2,1
  7,0 -> 7,4
  6,4 -> 2,0
  0,9 -> 2,9
  3,4 -> 1,4
  0,0 -> 8,8
  5,5 -> 8,2
  """.linesIterator.map(_.trim).filter(_.nonEmpty).toList

  case class Point(x: Int, y: Int)
  case class PointLine(from: Point, to: Point)
  type Grid = Map[Int, Map[Int, Int]] // grid X x Y

  def isPointLineVertical(line: PointLine): Boolean = line.from.y == line.to.y
  def isPointLineHorizontal(line: PointLine): Boolean = line.from.x == line.to.x
  def getSlope(line: PointLine) = Try((line.from.y - line.to.y) / (line.from.x - line.to.x))
  def isPointLineDiagonal(line: PointLine): Boolean =
    math.abs(getSlope(line).getOrElse(0)) == 1

  def getLineRange(n1: Int, n2: Int) =
    if (n1 < n2) n1 to n2
    else n1 to n2 by -1

  def markOpaqueLine(grid: Grid, line: Seq[(Int, Int)]): Grid =
    line.foldLeft(grid)((acc, nums) =>
      val (x, y) = nums
      acc + (acc.get(x) match
        case Some(r) => (x -> (r + (y -> (r.getOrElse(y, 0) + 1))))
        case None    => (x -> Map(y -> 1))
      )
    )

  def markOpaque(grid: Grid, line: PointLine): Grid =
    (isPointLineVertical(line), isPointLineHorizontal(line)) match
      case (true, false) =>
        val xs = getLineRange(line.from.x, line.to.x).toSeq
        markOpaqueLine(grid, xs.zip(Seq.fill(xs.length)(line.from.y)))
      case (false, true) =>
        val ys = getLineRange(line.from.y, line.to.y).toSeq
        markOpaqueLine(grid, Seq.fill(ys.length)(line.from.x).zip(ys))
      case _ => grid

  @tailrec
  def hydrothermalVenture(lines: Seq[PointLine], grid: Grid = Map.empty): Int =
    lines match
      case Nil =>
        grid.flatMap(_._2.toList.map(_._2)).count(_ > 1)
      case x +: ys =>
        hydrothermalVenture(ys, markOpaque(grid, x))

  def markOpaqueWithDiagonal(grid: Grid, line: PointLine): Grid =
    (isPointLineVertical(line), isPointLineHorizontal(line), isPointLineDiagonal(line)) match
      case (true, false, false) =>
        val xs = getLineRange(line.from.x, line.to.x).toSeq
        markOpaqueLine(grid, xs.zip(Seq.fill(xs.length)(line.from.y)))
      case (false, true, false) =>
        val ys = getLineRange(line.from.y, line.to.y).toSeq
        markOpaqueLine(grid, Seq.fill(ys.length)(line.from.x).zip(ys))
      case (false, false, true) =>
        val lineRange =
          getLineRange(line.from.x, line.to.x).zip(getLineRange(line.from.y, line.to.y))
        markOpaqueLine(grid, lineRange)
      case _ => grid

  @tailrec
  def hydrothermalVentureDiagonal(lines: Seq[PointLine], grid: Grid = Map.empty): Int =
    lines match
      case Nil =>
        grid.flatMap(_._2.toList.map(_._2)).count(_ > 1)
      case x +: ys =>
        hydrothermalVentureDiagonal(ys, markOpaqueWithDiagonal(grid, x))

  def printGrid(grid: Grid, x: Int, y: Int) =
    def transpose(grid: Grid): Seq[Seq[String]] =
      (0 to x).map { i =>
        (0 to y).map(j => s" ${grid.getOrElse(i, Map.empty).getOrElse(j, ".")} ")
      }.transpose
    println("---" * 9)
    transpose(grid).foreach(s => println(s.mkString))
    println("---" * 9)

  def parseInput(input: List[String]): Seq[PointLine] =
    input.map(line =>
      line match
        case s"$x1,$y1 -> $x2,$y2" =>
          PointLine(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    )

  def run(input: List[String]) =
    val lines = parseInput(input)
    (
      hydrothermalVenture(lines),
      hydrothermalVentureDiagonal(lines)
    )
