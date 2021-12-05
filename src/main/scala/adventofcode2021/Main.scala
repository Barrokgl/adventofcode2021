package adventofcode2021

@main def hello: Unit =
  println(s"task 1: ${Advent1.run(getInput("./input1.txt"))}")
  println(s"task 2: ${Advent2.run(getInput("./input2.txt"))}")
  println(s"task 3: ${Advent3.run(getInput("./input3.txt"))}")
  println(s"task 4: ${Advent4.run(getInput("./input4.txt"))}")
  println(s"task 5: ${Advent5.run(getInput("./input5.txt"))}")

def getInput(file: String): List[String] =
  scala.io.Source.fromFile(file).getLines.toList
