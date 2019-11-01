package honey

import java.io.{BufferedWriter, File, FileWriter}

import org.openqa.selenium.{By, Keys}
import org.openqa.selenium.chrome.ChromeDriver
import scalaj.http.Http

object Utils {

  def createFile(parentDir: String, subDir: String, fileName: String) = {
    val dir = s"src/main/resources/$parentDir/$subDir"
    val dirFile = new File(dir)
    if (!dirFile.exists()) dirFile.mkdirs()
    val file = new File(s"$dir/$fileName.csv")
    file.createNewFile()
    new BufferedWriter(new FileWriter(file))
  }

  def getFiles(dir: String) = {
    val pathname = s"src/main/resources/$dir"
    val d = new File(pathname)
    if (d.exists && d.isDirectory) d.listFiles.filter(_.isFile).toList
    else Nil
  }

  def doGet(path: String, query: String = "") = {
    val url = s"$path?$query"
    println(s"Doing get: $url")
    Http(url)
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .asString
  }

  def declareComplete(): Unit = {
    System.setProperty("webdriver.chrome.driver", "C:\\Apps\\chrome\\chromedriver.exe")
    val chrome = new ChromeDriver()
    chrome.get("https://www.youtube.com/watch?v=CDW3dqfCW7c")
    val body = chrome.findElement(By.tagName("body"))
    body.sendKeys(Keys.SPACE)
  }

}
