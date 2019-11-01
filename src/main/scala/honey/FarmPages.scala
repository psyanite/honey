package honey

import org.jsoup.Jsoup
import scalaj.http.HttpResponse

import util.control.Breaks._
import scala.jdk.CollectionConverters._

object FarmPages {

  def main(args: Array[String]): Unit = {
//    farmCategory("lunch", "https://www.zomato.com/sydney/lunch", None)
//    farmCategory("breakfast", "https://www.zomato.com/sydney/breakfast", None)
//    farmCategory("delivery", "https://www.zomato.com/sydney/delivery", None)
    farmCategory("take-away", "https://www.zomato.com/sydney/take-away", None)
    farmCategory("pubs-bars", "https://www.zomato.com/sydney/pubs-bars", None)
    farmCategory("asian", "https://www.zomato.com/sydney/restaurants/asian", None)
    farmCategory("italian", "https://www.zomato.com/sydney/restaurants/italian", None)
    farmCategory("french", "https://www.zomato.com/sydney/restaurants/french", None)
    farmCategory("japanese", "https://www.zomato.com/sydney/restaurants/japanese", None)
    farmCategory("chinese", "https://www.zomato.com/sydney/restaurants/chinese", None)
    farmCategory("desserts-bakes", "https://www.zomato.com/sydney/restaurants", Some("desserts-bakes=1"))
    farmCategory("bubble-tea", "https://www.zomato.com/sydney/restaurants/bubble-tea", None)
  }

  private def farmCategory(pageName: String, url: String, queryOpt: Option[String]): Unit = {

    val bw = Utils.createFile("farm-input", pageName, pageName)

    def parse(s: String): Unit = {
      val doc = Jsoup.parse(s)
      val urls = doc.select("a.result-title.hover_feedback.zred.bold.ln24.fontsize0").eachAttr("href").asScala
      urls.foreach(u => {
        val parts = u.split("/")
        val pathname = parts(parts.length - 1)
        bw.write(pathname)
        bw.newLine()
        bw.flush()
      })
    }

    var i = 1
    breakable {
      while (i < 10000) {
        var response: Option[HttpResponse[String]] = None
        var tryCount = 0
        while (response == null && tryCount < 4) try {
          response = Some(doGetPage(url, queryOpt, i))
        } catch {
          case _: Exception => tryCount += 1
        }
        response.foreach(r => if (r.code == 200) parse(r.body))
        i += 1
      }
    }

    bw.close()

    Utils.declareComplete()
  }

  private def doGetPage(path: String, queryOpt: Option[String], pageNumber: Int) = {
    val pageParam = s"page=$pageNumber"
    val query = queryOpt.map(query => s"$query&$pageParam").getOrElse(pageParam)
    Utils.doGet(path, query)
  }

}
