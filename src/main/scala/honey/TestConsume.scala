package honey

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.{Files, Paths}

import honey.AddressUtils.Address
import net.liftweb.json.{DefaultFormats, parse, prettyRender}
import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat

object TestConsume {

  implicit val formats: DefaultFormats.type = DefaultFormats

  def main(args: Array[String]): Unit = {
    val dir = "farm-output/test"
    val files = Utils.getFiles(dir)

    val fmt = DateTimeFormat.forPattern("yyyy-MM-dd_HH-mm")
    val time = fmt.print(new LocalDateTime())
    val errorFile = Utils.createFile("logs", s"$time-consume", s"errors")
    val summaryFile = Utils.createFile("logs", s"$time-consume", "summary")

    var successCount = 0
    var failCount = 0

    def processLine(line: String): Unit = {
      val parts = line.split("~~~")
      if (parts.length != 3) throw new Exception("Line consists of less or more than 3 parts")

      val zId = parts(0)
      val addressStr = parts(1)
      val mutation = parts(2)
      println(s"Processing $zId")
      val address = AddressUtils.getAddress(addressStr)
      val populated = populate(mutation, address)
      upsert(populated)
      println("Success")
    }

    def processFile(file: File): Unit = {
      val br = new BufferedReader(new FileReader(file))
      val totalLineCount = Files.lines(Paths.get(file.getPath)).count()

      var line: String = null
      var lineCount = 0
      while ( {
        line = br.readLine
        line != null
      }) {
        try {
          if (line.nonEmpty) processLine(line)
          successCount += 1
        } catch {
          case e: Exception =>
            e.printStackTrace()
            errorFile.write(line)
            errorFile.newLine()
            failCount += 1
        }
        lineCount += 1
        println()
        println(s"Processed $lineCount / $totalLineCount")
        val total = successCount + failCount
        if (total % 50 == 0) println(s"Processed $total total, $successCount succeeded, $failCount failed")
      }

      br.close()
    }

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

    errorFile.flush()
    errorFile.close()
  }

  private def populate(mutation: String, addr: Address): String = {
    mutation.replaceAllLiterally(":line1:", a(addr.line1))
        .replaceAllLiterally(":line2:", a(addr.line2))
        .replaceAllLiterally(":streetNumber:", a(addr.streetNumber))
        .replaceAllLiterally(":streetName:", a(addr.streetName))
        .replaceAllLiterally(":suburb:", a(addr.suburb))
  }

  private def a(s: String): String = if (s == "null") s else s"""\\"$s\\""""

  private def upsert(mutation: String): Unit = {
    val response = Toaster.query(mutation)
    val mutStr = mutation.replaceAllLiterally("\\", "")
    try {
      if (!response.isSuccess) {
        println(s"Response was not success, response code: ${response.code} \n $mutStr \n $response")
        throw new Exception(s"Response was not success, response code: ${response.code}")
      }
      val body = parse(response.body)
      val errors = (body \ "errors").children
      if (errors.nonEmpty) {
        println(s"Response had errors: \n $mutStr \n ${errors.map(prettyRender).mkString("\n")}")
        throw new Exception(s"Response had errors")
      }
      val savedName = (body \ "data" \ "upsertStore" \ "name").extractOpt[String] getOrElse ""
      if (savedName.isEmpty) {
        println(s"Response savedName is empty \n $mutStr: \n ")
        throw new Exception("Response name was empty")
      }
    } catch {
      case e: Exception =>
        throw new Exception(s"Toaster request failed", e)
    }
  }
}
