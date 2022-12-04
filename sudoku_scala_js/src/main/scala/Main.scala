
import org.scalajs.dom
import org.scalajs.dom.{document, window}

object Main {

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { (_: dom.Event) =>
      val state = Option(window.localStorage.getItem(GameState.STORAGE_KEY_NAME))
        .filter(_.nonEmpty)
        .map(json => GameState(json))
        .getOrElse(InMenuState())

      draw(state)
    })
  }

  private def draw(gameState: GameState): Unit = gameState match {
    case InGameState() => ???
    case InMenuState() => ???
  }

}