package com.thoughtworks.testgalaxy

import com.thoughtworks.testgalaxy.mocks.TestMocks
import org.scalatest._
import org.scalatest.mockito._
import org.mockito.Mockito._

/**
  * RomanNumeralsSpec has all the unit tests covering
  * different kinds of Roman inputs to check its
  * validity and conversions to Decimal values
  */

trait DataService {
  def findData: String
}

class GalaxySpec extends FlatSpec with TestMocks with Matchers with MockitoSugar {

  "isAlienCurrencyValid" should "return true if the alien currency given is valid" in {
    val isValid = testGalaxyObj.isAlienCurrencyValid("prok")
    assert(isValid === true)
  }

  it should "return false if the alien currency given is invalid" in {
    val isValid = testGalaxyObj.isAlienCurrencyValid("blah")
    assert(isValid === false)
  }

  "getRomanFromAlien" should "return roman numeral for the alien value given" in {
    when(testGalaxyMock.isAlienCurrencyValid("tegjl")).thenReturn(true)
    val romanNumeral = testGalaxyObj.getRomanFromAlien("tegj")
    assert(romanNumeral === "L")
  }

  it should "return empty string if the alien value given is invalid" in {
    when(testGalaxyMock.isAlienCurrencyValid("tegjldsa")).thenReturn(false)
    val romanNumeral = testGalaxyObj.getRomanFromAlien("tegjldsa")
    assert(romanNumeral === "")
  }

  "updateAlienToRomanMap" should "update the alienToRomanMap with the new values" in {
    testGalaxyObj.updateAlienToRomanMap("testAlien", "M")
    assert(testGalaxyObj.alienToRomanMap contains "testAlien")
  }

  "updateMetalValue" should "update the metalValuesMap with the new values" in {
    testGalaxyObj.updateMetalValue("glob prok Gold is 57800 Credits")
    assert(testGalaxyObj.metalValuesMap("Gold") === 14450.0)
  }

  "printGalaxyValue" should "print alien currency in decimal value" in {
    val input = "how much is pish tegj glob glob ?"
    val galaxyValue = testGalaxyObj.getGalaxyValue(input)
    assert(galaxyValue === "pish tegj glob glob is 42")
  }

  "printGalaxyValue" should "print 'No idea what you are talking " +
    "if the given input is invalid" in {
    val input = "how much wood could a woodchuck chuck if a woodchuck could chuck wood ?"
    val galaxyValue = testGalaxyObj.getGalaxyValue(input)
    assert(galaxyValue === "I have no idea what you are talking about")
  }

  "getGalaxyCredits" should "print alien currency in decimal credits" in {
    val input = "how many Credits is glob prok Gold ?"
    val galaxyValue = testGalaxyObj.getGalaxyCredits(input)
    assert(galaxyValue === "glob prok Gold is 57800 Credits")
  }

  "getGalaxyCredits" should "print 'No idea what you are talking " +
    "if the given input is invalid" in {
    val input = "how much wood could a woodchuck chuck if a woodchuck could chuck wood ?"
    val galaxyValue = testGalaxyObj.getGalaxyCredits(input)
    assert(galaxyValue === "I have no idea what you are talking about")
  }

  "interGalaxy" should "call printGalaxyValue function if the given input" +
        " starts with 'how much' and ends with ' ?'" in {
    doNothing().when(galaxySpy).updateAlienToRomanMap("glob", "I")
    galaxySpy.interGalaxy("glob is I")
    verify(galaxySpy, times(1)).updateAlienToRomanMap("glob", "I")
  }

  it should "call updateMetalValue function if the given input" +
    " ends with Credits" in {
    val input = "pish pish Iron is 3910 Credits"
    doNothing().when(galaxySpy).updateMetalValue(input)
    galaxySpy.interGalaxy(input)
    verify(galaxySpy, times(1)).updateMetalValue(input)
  }

  it should "call getGalaxyValue function if the given input" +
    " starts with 'how much' ends with ' ?'" in {
    val input = "how much is pish tegj glob glob ?"
    when(galaxySpy.getGalaxyValue(input)).thenReturn("pish tegj glob glob is 42")
    testGalaxyObj.interGalaxy(input)
    verify(galaxySpy, times(1)).getGalaxyValue(input)
  }

  it should "call getGalaxyValue function if the given input" +
    " starts with 'how many Credits ' ends with ' ?'" in {
    val input = "how many Credits is glob prok Silver ?"
    when(galaxySpy.getGalaxyCredits(input)).thenReturn("glob prok Silver is 68 Credits")
    testGalaxyObj.interGalaxy(input)
    verify(galaxySpy, times(1)).getGalaxyCredits(input)
  }

}