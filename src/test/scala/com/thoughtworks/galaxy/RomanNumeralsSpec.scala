package com.thoughtworks.galaxy

import org.scalatest.FlatSpec

/**
  * RomanNumeralsSpec has all the unit tests covering
  * different kinds of Roman inputs and check for
  * its validity and conversion of Roman to Decimal
  */
class RomanNumeralsSpec extends FlatSpec {

  "getDecimalValue" should "return the decimal value of the given Roman numeral" in {
    val decimalValue = RomanNumerals.getDecimalValue('X')
    assert(decimalValue === 10)
  }

  it should "return -1 if the Roman numeral given is invalid" in {
    val decimalValue = RomanNumerals.getDecimalValue('Z')
    assert(decimalValue === -1)
  }

  "isRomanCharValid" should "return true if the Roman numeral is valid" in {
    val isValid = RomanNumerals.isRomanCharValid('M')
    assert(isValid === true)
  }

  it should "return false if the Roman numeral is invalid" in {
    val isValid = RomanNumerals.isRomanCharValid('S')
    assert(isValid === false)
  }

  "isRomanValid" should "return true if the Roman numeral is valid" in {
    val isValid = RomanNumerals.isRomanValid("MCMIII")
    assert(isValid === true)
  }

  it should "return false if the Roman numeral contains non-roman numeral" in {
    val isValid = RomanNumerals.isRomanValid("MCMIZI")
    assert(isValid === false)
  }

  it should "return false if the D, L or V are repeated" in {
    val isValid = RomanNumerals.isRomanValid("DD")
    assert(isValid === false)
  }

  it should "return false if the I, X, C or M are repeated " +
    "more than 4 times" in {
    val isValid = RomanNumerals.isRomanValid("XXXXX")
    assert(isValid === false)
  }

  it should "return false if I, X, C or M repeated " +
    "no more than three times in succession" in {
    val isValid = RomanNumerals.isRomanValid("CCCC")
    assert(isValid === false)
  }

  it should "return false if I, X, C or M repeated 4 times and " +
    "third and fourth are seperated by a bigger value" in {
    val isValid = RomanNumerals.isRomanValid("XXXDX")
    assert(isValid === false)
  }

  it should "return true if the given Roman numeral is valid" in {
    val isValid = RomanNumerals.isRomanValid("MCMIII")
    assert(isValid === true)
  }

  it should "return false if the given Roman numeral is empty" in {
    val isValid = RomanNumerals.isRomanValid("")
    assert(isValid === false)
  }

  it should "return false if the given Roman numeral is decimal number" in {
    val isValid = RomanNumerals.isRomanValid("12345")
    assert(isValid === false)
  }

  it should "return false if the given Roman numeral has special chars" in {
    val isValid = RomanNumerals.isRomanValid("MMCMLX VII")
    assert(isValid === false)
  }

  "romanToDecimal" should "return 289 if the given Roman numeral is CCLXXXIX" in {
    val decimalValue = RomanNumerals.romanToDecimal(Some("CCLXXXIX"))
    assert(decimalValue === Some(289))
  }

  it should "return 1903 if the given Roman numeral is MCMIII" in {
    val decimalValue = RomanNumerals.romanToDecimal(Some("MCMIII"))
    assert(decimalValue === Some(1903))
  }

  it should "return None if the given Roman numeral is not valid" in {
    val decimalValue = RomanNumerals.romanToDecimal(Some("ID"))
    assert(decimalValue === None)
  }

  it should "return None if the given Roman numeral is empty" in {
    val decimalValue = RomanNumerals.romanToDecimal(None)
    assert(decimalValue === None)
  }

}