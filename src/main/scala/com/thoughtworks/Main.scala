import java.io.FileNotFoundException

import scala.collection.mutable
import scala.io.Source

/**
  * Where our Application bootstraps
  *
  * extends App where the actual main method is
  */
object Main extends App {

  val galaxyObj       = new Galaxy
  val obj             = new RomanNumeral()
  val InputFile       = "input.txt"
  val ValidRomanChars = List("I", "V", "X", "L", "C", "D", "M")
  var alienToRomanMap = mutable.Map[String, String]()

  val HowManyCredits  = "how many Credits is "
  val HowMuch         = "how much is "
  val Question        = " ?"

  val metalMap        = mutable.Map[String, Int]()

  def getInputFileLines(fileName: String): Iterator[String] =
    Source.fromResource(fileName).getLines()

  def isAlienCodeValid(string: String): Boolean = {
    string.split(" ").foreach ( word => if (!alienToRomanMap.contains(word)) {println("---->", word); return false } )
    true
  }

  def getRomanFromAlien(str: String): String =
    str.split(" ").map(alienToRomanMap).mkString

  for (line <- getInputFileLines(InputFile)) {

    val words           = line.split(" ")
    val curLineWords    = line.split(" ")
    val lineLength      = line.length
    line match {
      case someInput: String if curLineWords.length == 3 &&
                                curLineWords(1) == "is" &&
                                ValidRomanChars.contains(curLineWords.last) =>
        alienToRomanMap += (curLineWords(0) -> curLineWords(2))
      case someInput: String if someInput.endsWith("Credits") =>
        // Do Nothing
      case someInput: String if(someInput.startsWith(HowManyCredits) || someInput.startsWith(HowMuch) ) &&
                                someInput.endsWith(Question) =>
        val startIndex = if(someInput.startsWith(HowManyCredits)) HowManyCredits.length else HowMuch.length
        val alienCurrency = someInput.substring(startIndex, lineLength-Question.length)
        println(alienCurrency)
        println(isAlienCodeValid(alienCurrency))
        println("------------------------------")
        if(isAlienCodeValid(alienCurrency))
          if(!galaxyObj.isRomanValid(getRomanFromAlien(alienCurrency)))
            println("Not a valid alien currency")
          else
            galaxyObj.getActualValue(getRomanFromAlien(alienCurrency)) match {
              case -1 =>
                println("Not a valid alien currency")
              case value =>
                println(s"$alienCurrency is $value${if(someInput.startsWith(HowManyCredits))" Credits"}")
            }
        else
          println("Cannot decode the alien currency with the given inputs")
      case someInput: String if someInput.endsWith(Question) =>
        println("I have no idea what you are talking about")
println(alienToRomanMap)
    }

  }

  //dont forget to close the file

//  (1 to 4000).foreach(i => galaxyObj.isRomanValid(obj.toRomanNumerals(i)))

}

class RomanNumeral {
  def toRomanNumerals( number: Int) : String = {
    toRomanNumerals( number, List( ("M", 1000),("CM", 900), ("D", 500), ("CD", 400), ("C", 100), ("XC", 90),
      ("L", 50), ("XL",40), ("X", 10), ("IX", 9), ("V", 5), ("IV", 4), ("I", 1) ))
  }

  private def toRomanNumerals( number: Int, digits: List[(String, Int)] ) : String = digits match {
    case Nil => ""
    case h :: t => h._1 * ( number / h._2 ) + toRomanNumerals( number % h._2, t )
  }

}