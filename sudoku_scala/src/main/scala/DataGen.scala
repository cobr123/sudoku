import com.github.plokhotnyuk.jsoniter_scala.core.writeToString

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object DataGen {

  def main(args: Array[String]): Unit = {
    val dir = "./data"
    var count = Option(new File(dir).list).map(_.count(_.endsWith(".json"))).getOrElse(0)
    var prevTimeMillis = System.currentTimeMillis()
    while (count < 1_000_000) {
      if (count % 100 == 0) {
        println(s"$count, ${(System.currentTimeMillis() - prevTimeMillis) / 1000} sec")
        prevTimeMillis = System.currentTimeMillis()
      }
      val grid = Grid.solve(Grid().cells, printIntermediateGrids = false)
      val fileName = s"$dir/${grid.cells.mkString}.json"
      if (Files.notExists(Paths.get(fileName))) {
        val fileContent = writeToString(grid)
        Files.write(Paths.get(fileName), fileContent.getBytes(StandardCharsets.UTF_8))
        count += 1
      }
    }
  }

}
