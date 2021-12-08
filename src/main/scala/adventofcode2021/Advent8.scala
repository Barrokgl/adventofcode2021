package adventofcode2021

import scala.collection.immutable.ListMap

object Advent8:
  val example = """
  be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
  edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
  fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
  fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
  aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
  fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
  dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
  bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
  egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
  gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
  """.trim.split("\n").toList.filter(_.nonEmpty)

  val segmentsToNum = Map(
    2 -> 1,
    7 -> 8,
    3 -> 7,
    4 -> 4
  )
  val signalsPriority =
    ListMap(1 -> 2, 4 -> 4, 7 -> 3, 8 -> 7, 6 -> 6, 3 -> 5, 5 -> 5, 9 -> 6, 0 -> 6, 2 -> 5)

  type DecodedSegments = Map[Int, String]
  case class Display(signal: Seq[String], output: Seq[String], decoded: DecodedSegments)

  def sevenSegmentSearch(displays: Seq[Display]): Int =
    displays.foldLeft(0)((sum, display) =>
      sum + display.output.filter(num => segmentsToNum.contains(num.length)).length
    )

  def decodeDisplay(display: Display): Seq[Int] =
    val decoded = signalsPriority.foldLeft[DecodedSegments](display.decoded)((res, n) =>
      decodeSignal(n._1, display.signal, res)
    )
    val decodedReversed = decoded.map((k, v) => (v.toSet -> k)).toMap
    display.output
      .flatMap(seg => decodedReversed.find((decodedSeg, i) => decodedSeg == seg.toSet))
      .reverse
      .zipWithIndex
      .map((v, i) => v._2 * math.pow(10, i).toInt)

  def decodeSignal(n: Int, signal: Seq[String], decoded: DecodedSegments): DecodedSegments =
    val undecodedSegments = signal
      .filter(seg => !decoded.toSeq.map(_._2).contains(seg))
    decoded + (n -> (signalsPriority.get(n) match
      case Some(segLen) if segLen == 6 && n == 6 =>
        undecodedSegments
          .find(seg => seg.length == segLen && !decoded.get(1).exists(_.toSet.subsetOf(seg.toSet)))

      case Some(segLen) if segLen == 5 && n == 3 =>
        undecodedSegments
          .find(seg => seg.length == segLen && decoded.get(1).exists(_.toSet.subsetOf(seg.toSet)))

      case Some(segLen) if segLen == 5 && n == 5 =>
        undecodedSegments
          .find(seg => seg.length == segLen && seg.toSet.subsetOf(decoded.getOrElse(6, "").toSet))

      case Some(segLen) if segLen == 6 && n == 9 =>
        undecodedSegments.find(seg =>
          seg.length == segLen
            && decoded.get(5).exists(_.toSet.subsetOf(seg.toSet))
            && decoded.get(1).exists(_.toSet.subsetOf(seg.toSet))
        )

      case Some(segLen) =>
        undecodedSegments.find(seg => seg.length == segLen)

      case _ => ???
    ).get)

  def sevenSegmentSearch2(displays: Seq[Display]): Int =
    displays.foldLeft(0)((sum, display) => sum + decodeDisplay(display).sum)

  def parseInput(input: List[String]) =
    input.map { case s"$l | $r" =>
      Display(
        l.split(" ").toSeq.filter(_.nonEmpty),
        r.split(" ").toSeq.filter(_.nonEmpty),
        Map.empty
      )
    }

  def run(input: List[String]) =
    (
      sevenSegmentSearch(parseInput(input)),
      sevenSegmentSearch2(parseInput(input))
    )
