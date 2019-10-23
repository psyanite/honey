package honey

import org.jsoup.Jsoup
import util.control.Breaks._
import scala.jdk.CollectionConverters._

object FarmPages {

  def main(args: Array[String]): Unit = {

//    farmCategory("lunch", "https://www.zomato.com/sydney/lunch", None)
//    farmCategory("breakfast", "https://www.zomato.com/sydney/breakfast", None)
//    farmCategory("delivery", "https://www.zomato.com/sydney/delivery", None)
//    farmCategory("take-away", "https://www.zomato.com/sydney/take-away", None)
//    farmCategory("pubs-bars", "https://www.zomato.com/sydney/pubs-bars", None)
//    farmCategory("asian", "https://www.zomato.com/sydney/restaurants/asian", None)
//    farmCategory("italian", "https://www.zomato.com/sydney/restaurants/italian", None)
//    farmCategory("french", "https://www.zomato.com/sydney/restaurants/french", None)
//    farmCategory("japanese", "https://www.zomato.com/sydney/restaurants/japanese", None)
//    farmCategory("chinese", "https://www.zomato.com/sydney/restaurants/chinese", None)
//    farmCategory("desserts-bakes", "https://www.zomato.com/sydney/restaurants", Some("desserts-bakes=1"))
//    farmCategory("bubble-tea", "https://www.zomato.com/sydney/restaurants/bubble-tea", None)

  }

  private def farmCategory(pageName: String, url: String, queryOpt: Option[String]): Unit = {

    val bw = Utils.createFile("farm", s"$pageName-")

    def parse(s: String): Unit = {
      val doc = Jsoup.parse(s)
      val url = doc.select("a.result-title.hover_feedback.zred.bold.ln24.fontsize0").eachAttr("href").asScala
      val str = url.map(u => {
        val parts = u.split("/")
        parts(parts.length - 1)
      }).mkString("\n")
      bw.write(str)
    }

    var i = 1
    breakable {
      while (i < 10000) {
        val response = doGetPage(url, queryOpt, i)
        if (response.code != 200) break else {
          parse(response.body)
        }
        i += 1
      }
    }

    bw.flush()
    bw.close()

  }

  private def doGetPage(path: String, queryOpt: Option[String], pageNumber: Int) = {
    val pageParam = s"page=$pageNumber"
    val query = queryOpt.map(query => s"${query}&${pageParam}").getOrElse(pageParam)
    Utils.doGet(path, query)
  }

}
