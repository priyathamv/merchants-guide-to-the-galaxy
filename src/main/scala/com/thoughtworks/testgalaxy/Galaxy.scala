package com.thoughtworks.testgalaxy

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
  def isAlienCurrencyValid(alienCurrency: String): Boolean =
    if(alienCurrency.split(" ").count(alienToRomanMap.contains(_)) == alienCurrency.split(" ").length) true else false

  /**
    * @return Returns Roman numeral for the given
    *         Alien currency if valid else an empty string
    * @param alienCurrency Alien currency to be converted to Roman equivalent
    * */
  def getRomanFromAlien(alienCurrency: String): String =
    if (isAlienCurrencyValid(alienCurrency))
      alienCurrency.split(" ").map(alienToRomanMap).mkString
    else
      ""

  /** sets Alien currency -> Roman numeral in alienToRomanMap map */
  def updateAlienToRomanMap(alienCurrency: String, romanNumeral: String): Unit =
    if(!alienToRomanMap.contains(alienCurrency))
      alienToRomanMap = alienToRomanMap + (alienCurrency -> romanNumeral)

  /** extracts metal from given input and sets it in the metalValuesMap map
    * @param givenInput Alien code from which the metal and its value to be extracted
    * */
  def updateMetalValue(givenInput: String): Unit = {

    val metal = givenInput.split(" is ").head.split(" ").filterNot(str => alienToRomanMap.contains(str)).head
    val curCredits = givenInput.split(" is ").last.split(" ").head.toDouble
    val curArabicValue = RomanNumerals.romanToDecimal(
                          getRomanFromAlien(givenInput.split(" ")
                                                      .filter(alienToRomanMap.contains(_))
                                                      .mkString(" ")))
    if(!metalValuesMap.contains(metal))
      metalValuesMap = metalValuesMap + (metal -> curCredits/curArabicValue)

  }

  /** prints Alien currency in decimal credits
    * @param givenInput Alien code to be converted to decimal Credits
    * */
  def getGalaxyCredits(givenInput: String): String = {

    val alienValue          = givenInput.substring(HowManyCredits.length, givenInput.length - Question.length)
    val currentMetal        = alienValue.split(" ").last
    val alienCurrency       = alienValue.substring(0, alienValue.length - currentMetal.length - 1)
    val alienCurrencyValue  = RomanNumerals.romanToDecimal(getRomanFromAlien(alienCurrency))

    if(alienCurrencyValue != -1){
      val finalValue = (alienCurrencyValue * metalValuesMap(currentMetal)).toInt
      s"$alienCurrency $currentMetal is $finalValue $Credits"
    }else
      NoIdeaWhatYouTalk

  }

  /** prints Alien currency in decimal
    * @param givenInput Alien code to be converted to decimal value
    * */
  def getGalaxyValue(givenInput: String): String = {
    val (startIndex, endIndex) = (HowMuch.length, givenInput.length - Question.length)
    if(startIndex >= endIndex)
      return NoIdeaWhatYouTalk
    val alienCurrency       = givenInput.substring(startIndex, endIndex)
    val alienCurrencyValue  = RomanNumerals.romanToDecimal(getRomanFromAlien(alienCurrency))
    if(alienCurrencyValue != -1)
      s"$alienCurrency is $alienCurrencyValue"
    else
      NoIdeaWhatYouTalk

  }

  /** gets the input line from file and routes
    * the data to the corresponding method
    * @param inputLine The alien statement to be decrypted
    */
  def interGalaxy(inputLine: String): Any = {

    val curLineWords: Array[String] = inputLine.split(" ")
    inputLine match {

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