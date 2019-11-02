package honey

object AddressUtils {

  def getAddress(input: String): Address = {
    val string = input.replaceAll("\\(.*?\\)", "")
    val allParts = string.split(", ")

    def getStreetDeets(sLine: String): (String, String) = {
      if (sLine.charAt(0).isDigit) {
        val sParts = sLine.split(" ")
        if (sParts.length == 1) ("null", sParts(0))
        else if (sParts.length > 1) (sParts(0), sParts.tail.mkString(" "))
        else throw new Exception(s"Could not parse street line: $sLine")
      } else {
        ("null", sLine)
      }
    }

    def parseFullAddress(): Address = {
      val sydneyPartIdx = allParts.indexWhere(_.startsWith("Sydney"))
      if (sydneyPartIdx < 0) throw new Exception(s"Could not find Sydney index: $string")
      val b = allParts.slice(0, sydneyPartIdx)
      if (b.length < 2) throw new Exception(s"Address has insufficient parts: $string")
      val suburb = b(b.length - 1)

      val streetIdx = {
        val kiwi = b.indexWhere(p => p.charAt(0).isDigit && hasValidSuffix(p))
        if (kiwi >= 0) kiwi else b.indexWhere(p => hasValidSuffix(p))
      }
      val c = b.slice(0, streetIdx)
      val (streetNumber, streetName) = {
        val sLine = b(streetIdx)
        getStreetDeets(sLine)
      }
      val line1 = if (c.length >= 1) c(0) else "null"
      val line2 = if (c.length >= 2) c(1) else "null"

      Address(line1, line2, streetNumber, streetName, suburb)
    }

    def parseTwoPartAddress(): Address = {
      val (streetNumber, streetName) = getStreetDeets(allParts(0))
      val suburb = allParts(1)
      Address(null, null, streetNumber, streetName, suburb)
    }

    def parseThreePartAddress(): Address = {
      val (streetNumber, streetName) = getStreetDeets(allParts(0))
      val suburb = {
        val kiwi = allParts.drop(1).filterNot(s => s == "Sydney").headOption
        if (kiwi.isEmpty) throw new Exception("Could not find suburb")
        kiwi.get
      }
      Address(null, null, streetNumber, streetName, suburb)
    }

    def parseOnePartAddress(): Address = {
      if (hasValidSuffix(string)) {
        val (streetNumber, streetName) = getStreetDeets(allParts(0))
        Address(null, null, streetNumber, streetName, "Sydney")
      } else {
        val sParts = string.split(" ")
        try {
          val sLine = sParts.drop(1).mkString(" ")
          val suburb = sParts.dropRight(1).mkString(" ")
          if (!hasValidSuffix(sLine)) throw new Exception("Could not parse one part address")
          val (streetNumber, streetName) = getStreetDeets(sLine)
          Address(null, null, streetNumber, streetName, suburb)
        } catch {
          case _: Exception =>
            val suffixIdx = sParts.indexWhere(s => hasValidSuffix(s))
            if (suffixIdx < 0) throw new Exception("Could not find word with valid suffix")
            val sLine = sParts.slice(0, suffixIdx + 1).mkString(" ")
            val suburb = try {
              sParts(suffixIdx + 1)
            } catch {
              case _: Exception => "Sydney"
            }
            val (streetNumber, streetName) = getStreetDeets(sLine)
            Address(null, null, streetNumber, streetName, suburb)
        }
      }
    }

    def parseByStreetAddress(): Address = {
      val streetIdx = {
        val kiwi = allParts.indexWhere(p => p.charAt(0).isDigit && hasValidSuffix(p))
        if (kiwi >= 0) kiwi else allParts.indexWhere(p => hasValidSuffix(p))
      }
      val c = allParts.slice(0, streetIdx)
      val (streetNumber, streetName) = {
        val sLine = allParts(streetIdx)
        getStreetDeets(sLine)
      }
      val suburb = allParts(streetIdx + 1)
      val line2 = if (c.length >= 1) allParts(1) else "null"
      val line1 = if (c.length >= 2) allParts(0) else "null"
      Address(line1, line2, streetNumber, streetName, suburb)
    }

    def parseNaively(): Address = {
      val line1 = allParts(0)
      val line2 = allParts(1)
      val (streetNumber, streetName) = getStreetDeets(allParts(2))
      val suburb = allParts(3)
      Address(line1, line2, streetNumber, streetName, suburb)
    }

    try {
      parseFullAddress()
    } catch {
      case _: Exception =>
        try {
          if (allParts.size == 2) parseTwoPartAddress()
          else if (allParts.size == 3) parseThreePartAddress()
          else if (allParts.size == 1) parseOnePartAddress()
          else try {
            parseByStreetAddress()
          } catch {
            case _: Exception => parseNaively()
          }
        } catch {
          case e: Exception =>
            throw new Exception(s"Could not parse address: $string", e)
        }
    }
  }

  val ValidStreetSuffixes = "Road" :: "Street" :: "Way" :: "Avenue" :: "Avenue" :: "Drive" :: "Lane" :: "Place" ::
      "Crescent" :: "Court" :: "Boulevard" :: "Rd" :: "Ave" :: "Kingsway" :: "Walk" :: "Plaza" :: "Parade" ::
      "Road South" :: "Road North" :: "Broadway" :: "Highway" :: "St" :: "Dr" :: "street" :: "Roads" ::
      "Avenue of Europe" :: "Ln" :: "Promenade" :: "Street East" :: "Parkway" :: "Terrace" :: Nil

  private def hasValidSuffix(s: String): Boolean = {
    ValidStreetSuffixes.exists(suffix => s.endsWith(suffix))
  }

  case class Address(line1: String, line2: String, streetNumber: String, streetName: String, suburb: String)

}
