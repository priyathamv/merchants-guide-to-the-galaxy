package com.thoughtworks.galaxy

/**
  * A class to represent a Galaxy
  *
  * Includes methods to decrypt alien code
  * to Human understandable form
  *
  * @author Vinnakota Priyatam
  */
class Galaxy {

  /** Constants */
  val HowManyCredits: String    = "how many Credits is "
  val HowMuch: String           = "how much is "
  val Question: String          = " ?"
  val Credits: String           = "Credits"
  val NoIdeaWhatYouTalk: String = "I have no idea what you are talking about"

  var alienToRomanMap: Map[String, String]  = Map[String, String]()
  var metalValuesMap: Map[String, Double]   = Map[String, Double]()

  /**
    * @return Returns true, if the given string is a valid Alien currency
    * @param alienCurrency Alien currency to be checked for validation
    * */
  def isAlienCurrencyValid(alienCurrency: String): Boolean = {
    val wordsInCode = alienCurrency.split(" ")
    if (alienCurrency.nonEmpty &&
        wordsInCode.count(alienToRomanMap.contains(_)) == wordsInCode.length)
      true
    else
      false
  }

  /**
    * @return Returns Roman numeral for the given
    *         Alien currency if valid else an empty string
    * @param alienCurrency Alien currency to be converted to Roman equivalent
    * */
  def getRomanFromAlien(alienCurrency: String): Option[String] =
    if (isAlienCurrencyValid(alienCurrency))
      Some(
        alienCurrency.split(" ")
                     .map(alienToRomanMap)
                     .mkString
      )
    else None

  /** sets Alien currency -> Roman numeral in alienToRomanMap map */
  def updateAlienToRomanMap(alienCurrency: String, romanNumeral: String): Unit =
    if (!alienToRomanMap.contains(alienCurrency))
      alienToRomanMap = alienToRomanMap + (alienCurrency -> romanNumeral)

  /**
    * @return Returns the metal name from the given input,
    *         None if no metal exists
    * @param alienCode Alien code from which the metals name to be extracted
    * */
  def getMetalFromCode(alienCode: String): Option[String] = {
    val metalWordsInCode = alienCode.split(" is ")
                                    .head
                                    .split(" ")
                                    .filterNot(str => alienToRomanMap.contains(str))
    if (metalWordsInCode.length == 1)
      Some(metalWordsInCode.head)
    else None
  }

  /**
    * @return Returns true if the given string is Double parsable
    * @param str Given any string
    * */
  def isDouble(str: String): Boolean =
    str.forall(char => char.isDigit || char == '.')

  /**
    * @return Returns Credits from the given input
    * @param alienCode Alien code from which the Credits to be extracted
    * */
  def getCreditsFromCode(alienCode: String): Double = {
    val creditsInCode = alienCode.split(" is ")
                                 .last
                                 .split(" ")

    if (creditsInCode.length > 1 && isDouble(creditsInCode.head))
      creditsInCode.head.toDouble
    else 0.0
  }

  /**
    * @return Returns the Decimal equivalent of the given Alien code
    *         None, if the Alien code is invalid
    * @param alienCode Alien code which has to be converted to Decimal
    * */
  def getDecimalFromAlien(alienCode: String): Option[Int] = {
    RomanNumerals.romanToDecimal(
      getRomanFromAlien(alienCode.split(" ")
                                 .filter(alienToRomanMap.contains(_))
                                 .mkString(" ")
                       )
    )
  }

  /** extracts metal from given input and sets it in the metalValuesMap map
    * @param givenInput Alien code from which the metal and its value to be extracted
    * */
  def updateMetalValue(givenInput: String): Unit = {

    val currentMetal        = getMetalFromCode(givenInput)
    val currentCredits      = getCreditsFromCode(givenInput)
    val currentDecimalValue = getDecimalFromAlien(givenInput)

    if (currentMetal.nonEmpty &&
        currentCredits != 0.0 &&
        currentDecimalValue.nonEmpty &&
        !metalValuesMap.contains(currentMetal.get)) {
      metalValuesMap = metalValuesMap + (currentMetal.get -> currentCredits/currentDecimalValue.get)
    } else
      println(NoIdeaWhatYouTalk)
  }

  /** @return Returns Alien currency in decimal credits
    * @param givenInput Alien code to be converted to decimal Credits
    * */
  def getGalaxyCredits(givenInput: String): String = {

    val (startIndex, endIndex) = (HowManyCredits.length, givenInput.length - Question.length)
    if (startIndex >= endIndex)
      return NoIdeaWhatYouTalk

    val alienValue          = givenInput.substring(startIndex, endIndex)
    val currentMetal        = alienValue.split(" ").last
    val alienCurrency       = alienValue.substring(0, alienValue.length - currentMetal.length - 1)
    val alienCurrencyValue  = RomanNumerals.romanToDecimal(getRomanFromAlien(alienCurrency))

    if (alienCurrencyValue.nonEmpty && metalValuesMap.contains(currentMetal)){
      val finalValue = (alienCurrencyValue.get * metalValuesMap(currentMetal)).toInt
      s"$alienCurrency $currentMetal is $finalValue $Credits"
    } else
      NoIdeaWhatYouTalk

  }

  /**
    * @return Returns Alien currency in decimal value
    * @param givenInput Alien code to be converted to decimal value
    * */
  def getGalaxyValue(givenInput: String): String = {

    val (startIndex, endIndex) = (HowMuch.length, givenInput.length - Question.length)
    if (startIndex >= endIndex)
      return NoIdeaWhatYouTalk
    val alienCurrency       = givenInput.substring(startIndex, endIndex)
    val alienCurrencyValue  = RomanNumerals.romanToDecimal(getRomanFromAlien(alienCurrency))
    if (alienCurrencyValue.nonEmpty)
      s"$alienCurrency is ${alienCurrencyValue.get}"
    else
      NoIdeaWhatYouTalk

  }

  /**
    * Takes Alien code as input and routes
    * the data to the corresponding method
    * @param alienCode The alien statement to be decrypted
    */
  def decryptAlienCode(alienCode: String): Unit = {

    val curLineWords: Array[String] = alienCode.split(" ")
    alienCode match {

      case _: String if curLineWords.length == 3  &&
                        curLineWords(1) == "is"   &&
                        RomanNumerals.ValidRomanNumerals.contains(curLineWords.last.last) =>
        updateAlienToRomanMap(curLineWords(0), curLineWords(2))

      case givenInput: String if givenInput.endsWith(Credits) =>
        updateMetalValue(givenInput)

      case givenInput: String if givenInput.startsWith(HowMuch) && givenInput.endsWith(Question) =>
        val galaxyValue = getGalaxyValue(givenInput)
        println(galaxyValue)

      case givenInput: String if givenInput.startsWith(HowManyCredits) && givenInput.endsWith(Question) =>
        val galaxyCredits = getGalaxyCredits(givenInput)
        println(galaxyCredits)

      case _ =>
        println(NoIdeaWhatYouTalk)
    }

  }

}
