
import cats.effect.IO
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import fs2.{Pure, Stream}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import cats.effect.unsafe.implicits.global

object DataGen {

  val dir = "./data"

  val infiniteSolvedGrids: Stream[IO, Grid] = Stream.eval(IO(Grid.solve(Grid().cells, printIntermediateGrids = false))).repeat

  def main(args: Array[String]): Unit = {
    val count = AtomicInteger(Option(new File(dir).list).map(_.count(_.endsWith(".json"))).getOrElse(0))
    val prevTimeMillis = AtomicLong(System.currentTimeMillis())
    val maxConcurrent = Runtime.getRuntime.availableProcessors() / 2

    val stream = (1 to maxConcurrent).foldLeft(infiniteSolvedGrids) {
      case (s, _) =>
        s.merge(infiniteSolvedGrids)
    }

    stream
      .evalFilterAsync(maxConcurrent) { grid =>
        IO {
          val fileName = s"$dir/${grid.cells.mkString}.json"
          Files.notExists(Paths.get(fileName))
        }
      }
      .parEvalMapUnordered(maxConcurrent) { grid =>
        IO {
          val fileName = s"$dir/${grid.cells.mkString}.json"
          val fileContent = writeToString(grid)
          Files.write(Paths.get(fileName), fileContent.getBytes(StandardCharsets.UTF_8))
          if (count.incrementAndGet() % 100 == 0) {
            prevTimeMillis.getAndUpdate { prevTimeMillis =>
              val current = System.currentTimeMillis()
              println(s"${Thread.currentThread().getName}: $count, ${(current - prevTimeMillis) / 1000} sec")
              current
            }
          }
        }
      }
      .compile
      .drain
      .unsafeRunSync()
  }

}
