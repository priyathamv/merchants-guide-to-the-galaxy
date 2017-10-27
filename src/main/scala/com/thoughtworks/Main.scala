package com.thoughtworks

import java.io.{FileNotFoundException, IOException}

import com.thoughtworks.galaxy.Galaxy

import scala.io.Source

/**
  * Application starting point
  */
@throws(classOf[FileNotFoundException])
object Main extends App {

  /** Get file name from args if provided */
  val fileName = args.length match {
    case 0 => "input.txt"
    case _ => args.head
  }

  val galaxyObj: Galaxy = new Galaxy

  try {
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines())
      galaxyObj.decryptAlienCode(line)

    /** Closing the file when finished with the file */
    bufferedSource.close
  } catch {
    case e: FileNotFoundException => println(e.getMessage)
    case e: IOException           => println(e.getMessage)
  }

}

