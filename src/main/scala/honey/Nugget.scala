package honey

import java.sql.{Connection, DriverManager}


class Nugget {

  var Con: Connection = _

  connect()

  private def connect(): Unit = {
    Class.forName("org.postgresql.Driver")
    val url = "jdbc:postgresql://localhost:5432/burntoast"
    val username = "postgres"
    val password = "meow"
    Con = DriverManager.getConnection(url, username, password)
  }

  private def insert(sql: String): Int = {
    try {
      val statement = Con.createStatement
      statement.executeUpdate(sql)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        0
    }
  }

  def close() : Unit = {
    Con.close()
  }

}
