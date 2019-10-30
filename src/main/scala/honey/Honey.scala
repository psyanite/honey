package honey

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.{Files, Paths}

import net.liftweb.json.{DefaultFormats, _}
import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat
import org.jsoup.Jsoup
import scalaj.http._

import scala.jdk.CollectionConverters._

object Honey {

  implicit val formats: DefaultFormats.type = DefaultFormats

  def main(args: Array[String]): Unit = {
    try {
      start()
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      Utils.declareComplete()
    }
  }

  def start(): Unit = {

    val fmt  = DateTimeFormat.forPattern("yyyy-MM-dd_HH-mm")
    val time = fmt.print(new LocalDateTime())
    val summaryFile = Utils.createFile("logs", time, "summary")

    var successCount = 0
    var failCount = 0

    def parseZid(zId: String): Unit = {

      val zUrl = s"https://www.zomato.com/sydney/$zId"

      val html = Http(zUrl)
          .option(HttpOptions.readTimeout(10000))
          .asString
          .body

      val doc = Jsoup.parse(html)
      val name = doc.select("h1 a").text()
      if (name == null) throw new Exception("Name is null")


      val closedText = doc.select("div.closed-label.tooltip_formatted.ui.big.red.label.ml0").text()
      if (closedText != null && closedText.contains("Permanently closed")) return

      val coverImage = doc.select("div#progressive_image").attr("data-url")
      val cuisines = doc.select("div.res-info-cuisines a").eachText().asScala.mkString(",")
      val moreInfo = doc.select("div.res-info-feature-text").eachText().asScala.mkString(",")

      val phone = {
        val p = doc.select("span.tel").attr("aria-label")
        if (p.startsWith("02 ")) s"(02) ${p.substring(3)}" else p
      }

      val avgCost = {
        val s = doc.select("div.res-info-detail span").eachText().asScala.mkString("")
        try {
          s.split("""A\$""")(1).split(" ")(0)
        } catch {
          case _: Exception => "null"
        }
      }

      val (lat, lng) = {
        val mapUrl = Option(doc.select("div.resmap-img").attr("data-url")) getOrElse ""
        try {
          val coords = mapUrl.split("center=")(1).split("&")(0).split(",")
          (coords(0), coords(1))
        } catch {
          case _: Exception => throw new Exception(s"Could not parse lat and lng from $mapUrl")
        }
      }

      val hours = {
        val hours = doc.select("div.res-week-timetable table tr").asScala
        hours.map(h => {
          val tds = h.select("td").asScala
          if (tds.length == 2) s"${tds(0).text()}~${tds(1).text()}"
          else throw new Exception(s"Could not parse hours: $tds")
        }).mkString("$$$")
      }

      val addressStr = Option(doc.select("div.resinfo-icon span").text()) getOrElse ""
      val addr = getAddress(addressStr)

      val mutation = {
        def a(s: String): String = if (s == "null") s else s"""\\"$s\\""""
        s"""
           |mutation {
           |  upsertStore(
           |    zId: ${a(zId)},
           |    zUrl: ${a(zUrl)},
           |    name: ${a(name)},
           |    phoneCountry: \\"+61\\",
           |    phoneNumber: ${a(phone)},
           |    coverImage: ${a(coverImage)},
           |    addressFirstLine: ${a(addr.line1)},
           |    addressSecondLine: ${a(addr.line2)},
           |    addressStreetNumber: ${a(addr.streetNumber)},
           |    addressStreetName: ${a(addr.streetName)},
           |    cuisines: ${a(cuisines)},
           |    location: null,
           |    suburb: ${a(addr.suburb)},
           |    city: \\"Sydney\\",
           |    lat: $lat,
           |    lng: $lng,
           |    moreInfo: ${a(moreInfo)},
           |    avgCost: $avgCost,
           |    hours: ${a(hours.toString)}
           |  ) {
           |    id,
           |    name,
           |  }
           |}""".stripMargin
      }

      val response = Toaster.query(mutation)

      try {
        if (!response.isSuccess) {
          throw new Exception(s"Response was not success, response code: ${response.code}")
        }
        val body = parse(response.body)
        val errors = (body \ "errors").children
        if (errors.nonEmpty) {
          println(s"Response had errors: $mutation \n ${errors.map(prettyRender).mkString("\n")}")
          throw new Exception(s"Response had errors")
        }
        val savedName = (body \ "data" \ "upsertStore" \ "name").extractOpt[String] getOrElse ""
        if (name != savedName) throw new Exception(s"Response name mismatch, name: $name, responseName: $savedName")
      } catch {
        case e: Exception =>
          throw new Exception(s"Toaster request failed", e)
      }
    }

    def processFile(file: File): Unit = {
      val logFile = Utils.createFile("logs", time,s"${file.getName}-log")
      val errorDetailsFile = Utils.createFile("logs", time, s"${file.getName}-errors-details")
      val errorsFailedZidsFile = Utils.createFile("logs", time, s"${file.getName}-errors-failed-zIds")

      def processStore(zId: String): Unit = {
        println(s"\nProcess $zId")
        try {
          if (alreadySaved(zId)) {
            println(s"Process $zId skipped")
            logFile.write(s"$zId, https://www.zomato.com/sydney/$zId, skipped")
          } else {
            parseZid(zId)
            println(s"Process $zId skipped")
            logFile.write(s"$zId, https://www.zomato.com/sydney/$zId, success")
          }
          logFile.newLine()
          successCount += 1
        } catch {
          case e: Exception =>
            failCount += 1
            println(s"Process $zId failed")
            logFile.write(s"$zId,https://www.zomato.com/sydney/$zId,fail")
            logFile.newLine()
            errorDetailsFile.write(s"$zId;https://www.zomato.com/sydney/$zId;${e.getCause} ${e.getMessage}")
            errorDetailsFile.newLine()
            errorDetailsFile.newLine()
            errorDetailsFile.newLine()
            errorsFailedZidsFile.write(s"$zId")
            errorsFailedZidsFile.newLine()
        }
      }

      println(s"============================ Processing File: $file")
      val br = new BufferedReader(new FileReader(file))
      val totalLineCount = Files.lines(Paths.get(file.getPath)).count()
      var zId: String = null
      var lineCount = 0
      while ( {
        zId = br.readLine
        zId != null
      }) {
        if (zId.nonEmpty) processStore(zId.toLowerCase)
        lineCount += 1
        println(s"Processed $lineCount / $totalLineCount")
        val total = successCount + failCount
        if (total % 50 == 0) println(s"Processed $total total, $successCount succeeded, $failCount failed")
      }

      br.close()
      logFile.flush()
      logFile.close()
      errorDetailsFile.flush()
      errorDetailsFile.close()
      errorsFailedZidsFile.flush()
      errorsFailedZidsFile.close()
    }

    println(
      """
        |============================
        |  START    START    START
        |  (\__/)   (\__/)  (\__/)
        |  (•ㅅ•)   (•ㅅ•)   (•ㅅ•)
        |  / 　 づ  / 　 づ  / 　 づ
        |============================
        |""".stripMargin)

    val dir = "farm/test"
    val files = Utils.getFiles(dir)

    files.foreach(processFile)

    val summary =
      s"""
         |============================
         |Number of files processed: ${files.length}
         |Files processed\n${files.map(_.getName).mkString("\n")}
         |
         |ZId Total Count:   ${successCount + failCount}
         |ZId Success Count: $successCount
         |ZId Fail Count:    $failCount
         |""".stripMargin
    summaryFile.write(summary)
    summaryFile.flush()
    summaryFile.close()

    println(summary)
    println(
      """============================
        |   DONE     DONE     DONE
        |  (\__/)   (\__/)  (\__/)
        |  (•ㅅ•)   (•ㅅ•)   (•ㅅ•)
        |  / 　 づ  / 　 づ  / 　 づ
        |============================
        |""".stripMargin)
  }

  val ValidStreetSuffixes = "Road" :: "Street" :: "Way" :: "Avenue" :: "Avenue" :: "Drive" :: "Lane" :: "Place" ::
      "Crescent" :: "Court" :: "Boulevard" :: "Rd" :: "Ave" :: "Kingsway" :: "Walk" :: "Plaza" :: "Parade" ::
      "Road South" :: "Road North" :: "Broadway" :: "Highway" :: "Street" :: "St" :: "Dr" :: "street" :: "Roads" ::
      "Avenue of Europe" :: "Ln" :: "Promenade" :: "Street East" :: "Parkway" :: "Terrace" :: Nil

  private def hasValidSuffix(s: String) = s.endsWith(ValidStreetSuffixes)

  case class Address(line1: String, line2: String, streetNumber: String, streetName: String, suburb: String)

  private def getAddress(input: String): Address = {
    val string = input.replaceAll("\\(.*?\\)","")
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
      val line2 = if (c.length >= 1) b(1) else "null"
      val line1 = if (c.length >= 2) b(0) else "null"

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
        val sLine = sParts.drop(1).mkString(" ")
        val suburb = sParts.dropRight(1).mkString(" ")
        if (!hasValidSuffix(sLine)) throw new Exception("Could not parse one part address")
        val (streetNumber, streetName) = getStreetDeets(sLine)
        Address(null, null, streetNumber, streetName, suburb)
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

    try {
      parseFullAddress()
    } catch {
      case _: Exception =>
        try {
          if (allParts.size == 2) parseTwoPartAddress()
          else if (allParts.size == 3) parseThreePartAddress()
          else if (allParts.size == 1) parseOnePartAddress()
          else parseByStreetAddress()
        } catch {
          case e: Exception =>
            throw new Exception(s"Could not parse address: $string", e)
        }
    }
  }

  private def alreadySaved(zId: String): Boolean = {
    val query = {
      s"""
         |query {
         |  storeByZid(zid: \\"$zId\\") {
         |    z_id
         |  }
         |}""".stripMargin
    }

    val response = Toaster.query(query)

    if (!response.isSuccess) return false

    val body = parse(response.body)
    val savedZid = (body \ "data" \ "storeByZid" \ "z_id").extractOpt[String] getOrElse ""
    zId == savedZid
  }
}
