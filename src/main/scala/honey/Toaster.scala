package honey

import scalaj.http.{Http, HttpResponse}

object Toaster {

  def query(queryStr: String): HttpResponse[String] = {
    val data = s"""{ "query": "$queryStr" }"""
    Http("http://localhost:3000/graphql")
      .postData(data)
      .header("Authorization", "Bearer breadcat")
      .header("Content-Type", "application/json")
      .header("Accept", "application/json")
      .asString
  }

}
