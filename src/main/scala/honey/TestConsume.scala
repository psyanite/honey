package honey

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.{Files, Paths}

import honey.AddressUtils.Address
import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat

object TestConsume {

  def main(args: Array[String]): Unit = {

    val dir = "consume"
    val files = Utils.getFiles(dir)

    val fmt = DateTimeFormat.forPattern("yyyy-MM-dd_HH-mm")
    val time = fmt.print(new LocalDateTime())
    val errorFile = Utils.createFile("logs", time, s"consume")

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
      println(populated)
      println("============================")
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
          case _: Exception =>
            errorFile.write(line)
            errorFile.newLine()
            failCount += 1
        }
        lineCount += 1
        println(s"Processed $lineCount / $totalLineCount")
        val total = successCount + failCount
        if (total % 50 == 0) println(s"Processed $total total, $successCount succeeded, $failCount failed")
      }

      br.close()
      errorFile.flush()
      errorFile.close()
    }

    files.foreach(processFile)
  }

  private def populate(mutation: String, addr: Address): String = {
    mutation.replaceAllLiterally(":line1:", a(addr.line1))
        .replaceAllLiterally(":line2:", a(addr.line2))
        .replaceAllLiterally(":streetNumber:", a(addr.streetNumber))
        .replaceAllLiterally(":streetName:", a(addr.streetName))
        .replaceAllLiterally(":suburb:", a(addr.suburb))
  }

  private def a(s: String): String = if (s == "null") s else s"""\\"$s\\""""
}
