package com.thoughtworks.testgalaxy

import scala.util.control.Breaks._

/**
  * Factory for Roman numerals
  *
  * majorly contains functions to convert
  * Roman numerals to Decimal values and its utils
  */
object  RomanNumerals {

  /** returns Decimal value of the given Roman numeral */
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

  /** List of valid Roman numerals */
  val ValidRomanNumerals = List('I', 'V', 'X', 'L', 'C', 'D', 'M')

  /** returns true, if the given character is a valid Roman numeral */
  def isValidRomanChar(char: Char): Boolean = ValidRomanNumerals.contains(char)

  /**
    * Returns true, if the given String is a valid Roman numeral
    * all the conditions are explained where required
    */
  def isRomanValid(str: String): Boolean = {

    val strLength = str.length

    /** Condition 1:
      *  filtering the roman characters in the given string
      *  and check if any non Roman numeral exists
      */
    if (str.filter(isValidRomanChar).length != strLength)
      return false

    /** Condition 2:
      *  "D", "L", and "V" can never be repeated and
      *  no character can repeat more than 4 times
      */
    if (str.count(_ == 'D') > 1 ||
        str.count(_ == 'L') > 1 ||
        str.count(_ == 'V') > 1 ||
        str.count(_ == 'I') > 4 ||
        str.count(_ == 'X') > 4 ||
        str.count(_ == 'C') > 4 ||
        str.count(_ == 'M') > 4 )
      return false

    /** Condition 3:
      *  The symbols "I", "X", "C", and "M" can be
      *  repeated no more than three times in succession
      */
    for (i <- 0 to strLength-4) {
      val curSubString = str.substring(i, i+4)
      if(curSubString.count(_ == curSubString(0)) == 4)
        return false
    }

    /** Condition 4:
      *  The symbols "I", "X", "C", and "M" may appear four times
      *  if the third and fourth are separated by a smaller value
      */
    for (i <- 0 to strLength-5) {
      val curSubString = str.substring(i, i+5)
      val curCharFreq = curSubString.count(_ == curSubString(0))
      if (curCharFreq == 4 && (getDecimalValue(curSubString(0)) < getDecimalValue(curSubString(3))))
        return false
    }

    /** Return true, when all invalid conditions fail */
    true
  }

  /**
    * Converts given Roman numeral into Decimal value
    * returns -1 if the Roman numeral is invalid
    */
  def romanToDecimal(romanNumeral: String): Int = {
    if (romanNumeral.isEmpty) return -1

    val romanNumLength  = romanNumeral.length
    var decimalValue    = 0
    var isValid         = true

    var i = 0
    breakable {
      while (i < romanNumLength) {
        if (i == romanNumLength-1) {
          decimalValue += getDecimalValue(romanNumeral(i))
          i += 1
        }else if (getDecimalValue(romanNumeral(i)) < getDecimalValue(romanNumeral(i+1))) {
          /** Condition 5:
            *  V, L and D can never be subtracted.
            *  I, X and C can only be subtracted from
            *  V and X, L and C, D and M respectively.
            */
          if ((romanNumeral(i) == 'V' || romanNumeral(i) == 'L' || romanNumeral(i) == 'D') ||
            (romanNumeral(i) == 'I' && (romanNumeral(i+1) != 'V' && romanNumeral(i+1) != 'X')) ||
            (romanNumeral(i) == 'X' && (romanNumeral(i+1) != 'L' && romanNumeral(i+1) != 'C')) ||
            (romanNumeral(i) == 'C' && (romanNumeral(i+1) != 'D' && romanNumeral(i+1) != 'M')) ) {
            isValid = false
            break
          }
          decimalValue += getDecimalValue(romanNumeral(i+1)) - getDecimalValue(romanNumeral(i))
          i += 2
        }else if (getDecimalValue(romanNumeral(i)) >= getDecimalValue(romanNumeral(i+1))) {
          decimalValue += getDecimalValue(romanNumeral(i))
          i += 1
        }
      }
    }
    if (isValid) decimalValue else -1
  }

}