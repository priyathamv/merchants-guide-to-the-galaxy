package com.thoughtworks.galaxy

import scala.util.control.Breaks._

/**
  * Factory for Roman numerals
  *
  * majorly contains functions to convert
  * Roman numerals to Decimal values and its utils
  *
  * @author Vinnakota Priyatam
  */
object  RomanNumerals {

  /** List of valid Roman numerals */
  val ValidRomanNumerals = List('I', 'V', 'X', 'L', 'C', 'D', 'M')

  /**
    * @return Returns Decimal value of the given Roman numeral
    * @param char Roman numeral
    * */
  def getDecimalValue(char: Char): Int =
    char match {
      case 'I'  => 1
      case 'V'  => 5
      case 'X'  => 10
      case 'L'  => 50
      case 'C'  => 100
      case 'D'  => 500
      case 'M'  => 1000
      case _    => -1
    }

  /**
    * @return Returns true, if the given character is a valid Roman numeral
    * @param char Roman numeral to be validated
    * */
  def isRomanCharValid(char: Char): Boolean = ValidRomanNumerals.contains(char)

  /**
    * @return Returns true, if the given String is a valid Roman numeral
    *         all the conditions are explained where required
    * @param romanNumeral Roman number to be validated
    */
  def isRomanValid(romanNumeral: String): Boolean = {

    val romanLength = romanNumeral.length

    /** Condition 1:
      *  Checking if the `romanNumeral` is empty
      */
    if(romanNumeral.isEmpty)
      return false

    /** Condition 2:
      *  Checking if the `romanNumeral` contains
      *  any Non-Roman symbol
      */
    if (!romanNumeral.forall(isRomanCharValid))
      return false

    /** Condition 3:
      *  D, L and V can never be repeated and
      *  no character can repeat more than 4 times
      */
    if (romanNumeral.count(_ == 'D') > 1 ||
        romanNumeral.count(_ == 'L') > 1 ||
        romanNumeral.count(_ == 'V') > 1 ||
        romanNumeral.count(_ == 'I') > 4 ||
        romanNumeral.count(_ == 'X') > 4 ||
        romanNumeral.count(_ == 'C') > 4 ||
        romanNumeral.count(_ == 'M') > 4 )
      return false

    /** Condition 4:
      *  The symbols "I", "X", "C", and "M" can be
      *  repeated no more than three times in succession
      */
    for (i <- 0 to romanLength-4) {
      val curSubString = romanNumeral.substring(i, i+4)
      if (curSubString.count(_ == curSubString.head) == 4)
        return false
    }

    /** Condition 5:
      *  The symbols "I", "X", "C", and "M" may appear four times
      *  if the third and fourth are separated by a smaller value
      */
    for (i <- 0 to romanLength-5) {
      val curSubString = romanNumeral.substring(i, i+5)
      val curCharFreq = curSubString.count(_ == curSubString(0))
      if (curCharFreq == 4 && (getDecimalValue(curSubString(0)) < getDecimalValue(curSubString(3))))
        return false
    }

    /** Condition 6:
      *  V, L and D can never be subtracted.
      *  I, X and C can only be subtracted from
      *  V and X, L and C, D and M respectively.
      */
    for(i <- 0 until romanLength-1){
      if(getDecimalValue(romanNumeral(i)) < getDecimalValue(romanNumeral(i+1))){
        if ((romanNumeral(i) == 'V' || romanNumeral(i) == 'L' || romanNumeral(i) == 'D') ||
          (romanNumeral(i) == 'I' && (romanNumeral(i+1) != 'V' && romanNumeral(i+1) != 'X')) ||
          (romanNumeral(i) == 'X' && (romanNumeral(i+1) != 'L' && romanNumeral(i+1) != 'C')) ||
          (romanNumeral(i) == 'C' && (romanNumeral(i+1) != 'D' && romanNumeral(i+1) != 'M')) ) {
          return false
        }
      }
    }

    /** Return true, when all invalid conditions fail */
    true
  }

  /**
    * @return Returns Decimal value of the given `romanNumeral`,
    *         None if the Roman numeral is invalid
    * @param romanNumeralOpt Roman numeral to be converted to decimal
    */
  def romanToDecimal(romanNumeralOpt: Option[String]): Option[Int] = {

    if (romanNumeralOpt.isEmpty || !isRomanValid(romanNumeralOpt.get))
      return None

    val romanNumeral    = romanNumeralOpt.get
    val romanNumLength  = romanNumeral.length
    var decimalValue    = 0

    var i = 0
    breakable {
      while (i < romanNumLength) {
        if (i == romanNumLength - 1) {
          decimalValue += getDecimalValue(romanNumeral(i))
          i += 1
        }else if (getDecimalValue(romanNumeral(i)) < getDecimalValue(romanNumeral(i+1))) {
          decimalValue += getDecimalValue(romanNumeral(i+1)) - getDecimalValue(romanNumeral(i))
          i += 2
        }else if (getDecimalValue(romanNumeral(i)) >= getDecimalValue(romanNumeral(i+1))) {
          decimalValue += getDecimalValue(romanNumeral(i))
          i += 1
        }
      }
    }
    Some(decimalValue)
  }

}