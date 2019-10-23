package honey

import java.io.{BufferedWriter, File, FileWriter}

import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat
import scalaj.http.Http

object Utils {

  def createFile(dir: String, suffix: String = "") = {
    val fmt  = DateTimeFormat.forPattern("yyyy-MM-dd_HH-mm")
    val time = fmt.print(new LocalDateTime())
    val file = new File(s"src/main/resources/$dir/$time-$suffix.csv")
    file.createNewFile()
    new BufferedWriter(new FileWriter(file))
  }

  def doGet(path: String, query: String = "") = {
    val url = s"$path?$query"
    println(s"Doing get: $url")
    Http(url)
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .asString
  }

}
