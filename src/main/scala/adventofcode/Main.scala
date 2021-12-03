package adventofcode

@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

def getInput(file: String): List[String] =
  scala.io.Source.fromFile(file).getLines.toList
