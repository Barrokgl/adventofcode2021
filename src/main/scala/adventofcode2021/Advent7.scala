package adventofcode2021

object Advent7:
  val example = List("16,1,2,0,4,2,7,1,2,14")

  def parseInput(input: List[String]) =
    input.head.split(",").toSeq.map(_.toInt)

  def moveCrabs(crabs: Seq[Int], targetLevel: Int) =
    crabs.map(crabLevel => math.abs(crabLevel - targetLevel)).sum

  def moveCrabs2(crabs: Seq[Int], targetLevel: Int) =
    crabs.map(crabLevel => (1 to math.abs(crabLevel - targetLevel)).sum).sum

  def theTreacheryOfWhales(crabs: Seq[Int], moveFun: (Seq[Int], Int) => Int): Int =
    val (min, max) = (crabs.min, crabs.max)
    (min to max).map(targetLevel => moveFun(crabs, targetLevel)).min

  def run(input: List[String]) =
    (
      theTreacheryOfWhales(parseInput(input), moveCrabs),
      theTreacheryOfWhales(parseInput(input), moveCrabs2)
    )
