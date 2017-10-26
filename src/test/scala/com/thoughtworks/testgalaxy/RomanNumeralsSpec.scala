package com.thoughtworks.testgalaxy

import org.scalatest.FlatSpec

class RomanNumeralsSpec extends FlatSpec {

  "getDecimalValue" should "return the decimal value of the given Roman numeral" in {
    val decimalValue = RomanNumerals.getDecimalValue('X')
    assert(decimalValue === 10)
  }

  "getDecimalValue" should "return -1 if the Roman numeral given is invalid" in {
    val decimalValue = RomanNumerals.getDecimalValue('Z')
    assert(decimalValue === -1)
  }

  "isValidRomanChar" should "return true if the Roman numeral is valid" in {
    val isValid = RomanNumerals.isValidRomanChar('M')
    assert(isValid === true)
  }

  "isValidRomanChar" should "return false if the Roman numeral is invalid" in {
    val isValid = RomanNumerals.isValidRomanChar('S')
    assert(isValid === false)
  }

  "isRomanValid" should "return true if the Roman numeral is valid" in {
    val isValid = RomanNumerals.isRomanValid("MCMIII")
    assert(isValid === true)
  }

  "isRomanValid" should "return false if the Roman numeral contains non-roman numeral" in {
    val isValid = RomanNumerals.isRomanValid("MCMIZI")
    assert(isValid === false)
  }

  "isRomanValid" should "return false if the D, L or V are repeated" in {
    val isValid = RomanNumerals.isRomanValid("DD")
    assert(isValid === false)
  }

  "isRomanValid" should "return false if the I, X, C or M are repeated " +
    "more than 4 times" in {
    val isValid = RomanNumerals.isRomanValid("XXXXX")
    assert(isValid === false)
  }

  "isRomanValid" should "return false if I, X, C or M repeated " +
    "no more than three times in succession" in {
    val isValid = RomanNumerals.isRomanValid("CCCC")
    assert(isValid === false)
  }

  "isRomanValid" should "return false if I, X, C or M repeated 4 times and " +
    "third and fourth are seperated by a bigger value" in {
    val isValid = RomanNumerals.isRomanValid("XXXDX")
    assert(isValid === false)
  }

  "isRomanValid" should "return true if the given Roman numeral is valid" in {
    val isValid = RomanNumerals.isRomanValid("MCMIII")
    assert(isValid === true)
  }

  "romanToDecimal" should "return 289 if the given Roman numeral is CCLXXXIX" in {
    val decimalValue = RomanNumerals.romanToDecimal("CCLXXXIX")
    assert(decimalValue === 289)
  }

  "romanToDecimal" should "return 1903 if the given Roman numeral is MCMIII" in {
    val decimalValue = RomanNumerals.romanToDecimal("MCMIII")
    assert(decimalValue === 1903)
  }

  "romanToDecimal" should "return -1 if the given Roman numeral is not valid" in {
    val decimalValue = RomanNumerals.romanToDecimal("ID")
    assert(decimalValue === -1)
  }

}