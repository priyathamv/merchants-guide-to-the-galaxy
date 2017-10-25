package com.thoughtworks

import com.thoughtworks.galaxy.Galaxy
import scala.io.Source

/** Application starting point,
  * takes input from input.txt file
  * located in resources directory
  */
object Main extends App {

  def getInputFileLines(fileName: String): Iterator[String] =
    Source.fromResource(fileName).getLines()

  val galaxyObj = new Galaxy

  for (line <- getInputFileLines("input.txt"))
    galaxyObj.intergalaxy(line)

}