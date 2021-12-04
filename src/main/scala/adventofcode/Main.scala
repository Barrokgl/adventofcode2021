package adventofcode

@main def hello: Unit =
  println(s"task 1: ${Advent1.run(getInput("./input1.txt"))}")
  println(s"task 2: ${Advent2.run(getInput("./input2.txt"))}")
  println(s"task 3: ${Advent3.run(getInput("./input3.txt"))}")

def getInput(file: String): List[String] =
  scala.io.Source.fromFile(file).getLines.toList
