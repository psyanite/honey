package honey

import java.io.{BufferedWriter, File, FileWriter}

import org.joda.time.LocalDateTime
import org.joda.time.format._
import org.jsoup.Jsoup
import scalaj.http._

import scala.jdk.CollectionConverters._

object Honey {

  def main(args: Array[String]): Unit = {

    val fmt  = DateTimeFormat.forPattern("yyyy-MM-dd_HH:mm")
    val time = fmt.print(new LocalDateTime())
    val file = new File(s"src/main/resources/logs/$time.csv")
    file.createNewFile()
    val bw   = new BufferedWriter(new FileWriter(file))

    def parse(zId: String): Unit = {

      val zUrl = s"https://www.zomato.com/sydney/$zId"

      val html = Http(zUrl)
          .option(HttpOptions.readTimeout(10000))
          .asString
          .body

      val doc        = Jsoup.parse(html)
      val coverImage = doc.select("div#progressive_image").attr("data-url")
      val name       = doc.select("h1 a").text()
      val cuisines   = doc.select("div.res-info-cuisines a").eachText().asScala.mkString(",")
      val moreInfo   = doc.select("div.res-info-feature-text").eachText().asScala.mkString(",")

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
        try {
          hours.map(h => {
            val tds = h.select("td").asScala
            if (tds.length == 2) s"${tds(0).text()}~${tds(1).text()}"
            else throw new Exception(s"Could not parse hours: $tds")
          }).mkString("$$$")
        } catch {
          case _: Exception => "null"
        }
      }

      val address = Option(doc.select("div.resinfo-icon span").text()) getOrElse ""
      val allParts = address.split(", ")
      val sydneyPartIdx = allParts.lastIndexWhere(_.startsWith("Sydney"))
      if (sydneyPartIdx < 0) throw new Exception(s"Could not find Sydney index: $address")
      val b = allParts.slice(0, sydneyPartIdx)
      if (b.length < 2) throw new Exception(s"Address has insufficient parts: $address")
      val suburb = b(b.length - 1)

      val streetIdx = {
        val kiwi = b.indexWhere(p => p.charAt(0).isDigit && hasValidSuffix(p))
        if (kiwi >= 0) kiwi else b.indexWhere(p => hasValidSuffix(p))
      }
      val c = b.slice(0, streetIdx)
      val (streetNumber, streetName) = {
        val sLine = b(streetIdx)
        if (sLine.charAt(0).isDigit) {
          val sParts = sLine.split(" ")
          if (sParts.length == 1) ("null", sParts(0))
          else if (sParts.length > 1) (sParts(0), sParts.tail.mkString(" "))
          else throw new Exception(s"Could not parse street line: $sLine")
        } else {
          ("null", sLine)
        }
      }
      val line2 = if (c.length >= 1) b(1) else "null"
      val line1 = if  (c.length >= 2) b(0) else "null"

      //  static Future<Map<String, dynamic>> get(String body, {Map<String, dynamic> variables}) async {
      //    var requestBody = json.encode({'query': body });
      //    var response = await http.post(url, body: requestBody, headers: headers);
      //    var responseBody = json.decode(response.body);
      //    if (response.statusCode != 200 || responseBody['errors'] != null) {
      //      print('Toaster request failed: {\n${body.trimRight()}\n}');
      //      print('${response.statusCode} response: ${response.body}');
      //      return Map<String, dynamic>();
      //    }
      //    return responseBody['data'];
      //  }

      def a(s: String): String = if (s == "null") s else s""""$s""""

//      Http("http://localhost:3000/graphql")
//          .postData(s"{ query: $mutation }")
//          .header("Content-Type", "application/json")
//          .header("Accept", "application/json")
//          .asString
//          .code

      // Verify response
    }

    def processStore(zId: String): Unit = {
      try {
        parse(zId)
      } catch {
        case e: Exception =>
          println("==============")
          println(s"https://www.zomato.com/sydney/$zId")
          println(e)
          println("==============")
      }
    }

    println("============== START")

    processStore("meet-mica-surry-hills")
    processStore("shortstop-coffee-donuts-barangaroo")
    processStore("sushi-hotaru-1-cbd")
    processStore("yayoi-japanese-teishoku-restaurant-westfield-sydney-cbd")
    processStore("dera-uma-cbd")
    processStore("sakura-japanese-kitchen-cbd")
    processStore("yayoi-japanese-teishoku-restaurant-westfield-sydney-cbd")
    processStore("yama-japanese-cafe-restaurant-cbd")
    processStore("kura-cbd")
    processStore("sushi-hon-cbd")
    processStore("umi-sushi-bar-haymarket")
    processStore("condor-japanese-restaurant-the-york-hotel-cbd")

    bw.flush()
    bw.close()

    println("============== DONE")
  }

  val ValidStreetSuffixes = "Road" ::
      "Street" ::
      "Way" ::
      "Avenue" ::
      "Avenue" ::
      "Drive" ::
      "Lane" ::
      "Place" ::
      "Crescent" ::
      "Court" ::
      "Boulevard" ::
      Nil

  private def hasValidSuffix(s: String) = ValidStreetSuffixes.contains(s.split(" ").last)
}
