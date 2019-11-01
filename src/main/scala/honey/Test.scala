package honey

import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat

object Test {

  def main(args: Array[String]): Unit = {

    val fmt = DateTimeFormat.forPattern("yyyy-MM-dd_HH-mm")
    val time = fmt.print(new LocalDateTime())
    val failedAddrFile = Utils.createFile("logs", time, "failed-addr-stores")
    val zId = "inferno-grill-cafe-maroubra"
    val addressStr = "44 Baker Street, Sydney, NSW"

    val mutation = {
      s"""
         |mutation {
         |  upsertStore(
         |    addressFirstLine: :line1:,
         |    addressSecondLine: :line2:,
         |    addressStreetNumber: :streetNumber:,
         |    addressStreetName: :streetName:,
         |    location: null,
         |    suburb: :suburb:,
         |    city: \\"Sydney\\",
         |  ) {
         |    id,
         |    name,
         |  }
         |}""".stripMargin
    }
    val stripped = mutation.replaceAllLiterally("\n", "")
    failedAddrFile.write(s"$zId~~~$addressStr~~~$stripped")
    failedAddrFile.newLine()

    failedAddrFile.flush()
    failedAddrFile.close()
  }
}
