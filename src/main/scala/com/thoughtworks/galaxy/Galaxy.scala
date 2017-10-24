import scala.util.control.Breaks._

object Galaxy extends App{

  def romanValue(str: Char): Int =
    str match {
      case 'I'  => 1
      case 'V'  => 5
      case 'X'  => 10
      case 'L'  => 50
      case 'C'  => 100
      case 'D'  => 500
      case 'M'  => 1000
      case _    => -1
    }

  val validRomanChars = List('I', 'V', 'X', 'L', 'C', 'D', 'M')

  def isValidRomanChar(char: Char): Boolean = validRomanChars.contains(char)

  def isRomanValid(str: String): Boolean = {

    val strLength = str.length

    if(str.filter(isValidRomanChar).length != strLength)
      return false

    if( str.count(_ == 'D') > 1 ||
        str.count(_ == 'L') > 1 ||
        str.count(_ == 'V') > 1 ||
        str.count(_ == 'I') > 4 ||
        str.count(_ == 'X') > 4 ||
        str.count(_ == 'C') > 4 ||
        str.count(_ == 'M') > 4 )
      return false

    for(i <- 0 to strLength-4){
      val curSubString = str.substring(i, i+4)
      if(curSubString.count(_ == curSubString(0)) == 4)
        return false
    }

    for(i <- 0 to strLength-5){
      val curSubString = str.substring(i, i+5)
      val curCharFreq = curSubString.count(_ == curSubString(0))
      if(curCharFreq == 4 && romanValue(curSubString(0)) > romanValue(curSubString(4)))
        return false
    }

    true
  }

  def getActualValue(str: String): Int = {
    val strLen      = str.length
    var finalValue  = 0
    var isValid     = true

    var i = 0
    breakable{
      while(i < strLen){
        if(i == strLen-1){
          finalValue += romanValue(str(i))
          i += 1
        }else if(romanValue(str(i)) < romanValue(str(i+1))){
          if( (str(i) == 'V' || str(i) == 'L' || str(i) == 'D') ||
            (str(i) == 'I' && (str(i+1) != 'V' && str(i+1) != 'X')) ||
            (str(i) == 'X' && (str(i+1) != 'L' && str(i+1) != 'C')) ||
            (str(i) == 'C' && (str(i+1) != 'D' && str(i+1) != 'M')) ){
            isValid = false
            break
          }
          finalValue += romanValue(str(i+1)) - romanValue(str(i))
          i += 2
        }else if(romanValue(str(i)) >= romanValue(str(i+1))){
          finalValue += romanValue(str(i))
          i += 1
        }
      }
    }
    if(isValid) finalValue else -1
  }

}

