package adventofcode2021

import scala.annotation.tailrec

object Advent4:
  val example = """
  7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19

   3 15  0  2 22
   9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7
""".trim().split("\n").toList

  type GridState = IndexedSeq[IndexedSeq[Boolean]]
  type GridNumbers = IndexedSeq[IndexedSeq[Int]]
  case class Grid(grid: GridNumbers, state: GridState)

  def markNumber(number: Int, grid: GridNumbers, state: GridState): GridState =
    grid
      .zip(state)
      .map((gridrow, staterow) => gridrow.zip(staterow).map((g, s) => s || g == number))

  def checkWin(grid: Grid): Boolean =
    grid.state.exists(_.forall(identity)) || grid.state.transpose.exists(_.forall(identity))

  def unmarkedSum(grid: Grid): Int =
    grid.grid
      .zip(grid.state)
      .flatMap((gridrow, staterow) => gridrow.zip(staterow).filter((_, s) => !s).map((g, _) => g))
      .sum

  // part one
  @tailrec
  def giantSquid(numbers: Seq[Int], grids: Seq[Grid]): Int =
    val currentNumber = numbers.head
    val newStates = grids.map(gr => gr.copy(state = markNumber(currentNumber, gr.grid, gr.state)))
    newStates.find(checkWin) match
      case Some(grid) => unmarkedSum(grid) * currentNumber
      case _          => giantSquid(numbers.tail, newStates)

  // part two
  @tailrec
  def giantSquidWin(numbers: Seq[Int], grids: Seq[Grid], winner: Option[Int] = None): Int =
    numbers match
      case x +: ys =>
        val newStates =
          grids.map(gr => gr.copy(state = markNumber(x, gr.grid, gr.state)))
        val newWinner = grids
          .zip(newStates)
          .find((previous, current) => !checkWin(previous) && checkWin(current)) match
          case Some(_, grid) => Some(unmarkedSum(grid) * x)
          case _             => winner
        giantSquidWin(ys, newStates, newWinner)
      case _ => winner.get

  def parseInput(input: List[String]): (IndexedSeq[Int], IndexedSeq[List[IndexedSeq[Int]]]) =
    val gridSize = 5
    val numbers = input.head.split(",").toIndexedSeq.map(_.toInt)
    val grids = input.tail
      .grouped(gridSize + 1)
      .toIndexedSeq
      .map(_.tail.map(_.trim.split(" +").toIndexedSeq.map(_.toInt)))
    (numbers, grids)

  def run(input: List[String]) =
    val (numbers, grids) = parseInput(input)
    val gridsMapped = grids.map(gr => Grid(gr.toIndexedSeq, gr.map(_.map(_ => false)).toIndexedSeq))
    (
      giantSquid(numbers, gridsMapped),
      giantSquidWin(numbers, gridsMapped)
    )
