package com.thoughtworks.galaxy

import com.thoughtworks.galaxy.mocks.TestMocks
import org.scalatest._
import org.scalatest.mockito._
import org.mockito.Mockito._

/**
  * RomanNumeralsSpec has all the unit tests covering
  * different kinds of Roman inputs to check its
  * validity and conversions to Decimal values
  */
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
    assert(romanNumeral === Some("L"))
  }

  it should "return None if the alien value given is invalid" in {
    when(testGalaxyMock.isAlienCurrencyValid("tegjldsa")).thenReturn(false)
    val romanNumeral = testGalaxyObj.getRomanFromAlien("tegjldsa")
    assert(romanNumeral === None)
  }

  "updateAlienToRomanMap" should "update the alienToRomanMap with the new values" in {
    testGalaxyObj.updateAlienToRomanMap("testAlien", "M")
    assert(testGalaxyObj.alienToRomanMap contains "testAlien")
  }

  "getMetalFromCode" should "return the metal name from the given Alien code" in {
    val metal = testGalaxyObj.getMetalFromCode("glob prok Gold is 57800 Credits")
    assert(metal === Some("Gold"))
  }

  it should "return None if the no valid metals exists" in {
    val metal = testGalaxyObj.getMetalFromCode("glob prok Random metal is 57800 Credits")
    assert(metal === None)
  }

  "isDouble" should "return true if the given string is Double parsable" in {
    val isValid = testGalaxyObj.isDouble("20.45")
    assert(isValid === true)
  }

  it should "return false if the given string is not Double parsable" in {
    val isValid = testGalaxyObj.isDouble("2a45")
    assert(isValid === false)
  }

  "getCreditsFromCode" should "return Credit from the given alien code" in {
    val credits = testGalaxyObj.getCreditsFromCode("pish pish Iron is 3910 Credits")
    assert(credits === 3910)
  }

  it should "return 0.0 if no valid Credit exists in the given alien code" in {
    val credits = testGalaxyObj.getCreditsFromCode("pish pish Iron is blah blah Credits")
    assert(credits === 0.0)
  }

  "getDecimalFromAlien" should "return Decimal value from the given alien code" in {
    val decimal = testGalaxyObj.getDecimalFromAlien("how many Credits is glob prok Silver ?")
    assert(decimal === Some(4))
  }

  it should "return None if the given alien code not a valid one" in {
    val decimal = testGalaxyObj.getDecimalFromAlien("how many Credits is something random string Silver ?")
    assert(decimal === None)
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

  it should "print 'No idea what you are talking " +
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

  it should "print 'No idea what you are talking " +
    "if the given input is invalid" in {
    val input = "how much wood could a woodchuck chuck if a woodchuck could chuck wood ?"
    val galaxyValue = testGalaxyObj.getGalaxyCredits(input)
    assert(galaxyValue === "I have no idea what you are talking about")
  }

  "interGalaxy" should "call printGalaxyValue function if the given input" +
        " starts with 'how much' and ends with ' ?'" in {
    doNothing().when(galaxySpy).updateAlienToRomanMap("glob", "I")
    galaxySpy.decryptAlienCode("glob is I")
    verify(galaxySpy, times(1)).updateAlienToRomanMap("glob", "I")
  }

  it should "call updateMetalValue function if the given input" +
    " ends with Credits" in {
    val input = "pish pish Iron is 3910 Credits"
    doNothing().when(galaxySpy).updateMetalValue(input)
    galaxySpy.decryptAlienCode(input)
    verify(galaxySpy, times(1)).updateMetalValue(input)
  }

  it should "call getGalaxyValue function if the given input" +
    " starts with 'how much' ends with ' ?'" in {
    val input = "how much is pish tegj glob glob ?"
    when(galaxySpy.getGalaxyValue(input)).thenReturn("pish tegj glob glob is 42")
    testGalaxyObj.decryptAlienCode(input)
    verify(galaxySpy, times(1)).getGalaxyValue(input)
  }

  it should "call getGalaxyValue function if the given input" +
    " starts with 'how many Credits ' ends with ' ?'" in {
    val input = "how many Credits is glob prok Silver ?"
    when(galaxySpy.getGalaxyCredits(input)).thenReturn("glob prok Silver is 68 Credits")
    testGalaxyObj.decryptAlienCode(input)
    verify(galaxySpy, times(1)).getGalaxyCredits(input)
  }

}