
import org.scalajs.dom
import org.scalajs.dom.{document, window}

object Main {

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { (_: dom.Event) =>
      drawInMenuState()
    })
  }

  private def clearScreen(): Unit = {
    val body = document.body

    while (body.hasChildNodes()) {
      body.removeChild(body.lastChild)
    }
  }

  private def drawInMenuState(): Unit = {
    clearScreen()
    Option(window.localStorage.getItem(InGameState.STORAGE_KEY_NAME))
      .filter(_.nonEmpty)
      .flatMap(json => InGameState(json))
      .foreach(addContinueBtn)

    addNewGameBtn()
  }

  private def addContinueBtn(inGameState: InGameState): Unit = {
    val inputStyle = "padding-left: 7px; padding-right: 7px; padding-top: 2px; padding-bottom: 2px; margin: 5px;"
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", "Continue")
    btn.setAttribute("style", inputStyle)
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      drawInGameState(inGameState)
    })
    document.body.appendChild(btn)
    document.body.appendChild(document.createElement("br"))
  }

  private def addNewGameBtn(): Unit = {
    val inputStyle = "padding-left: 7px; padding-right: 7px; padding-top: 2px; padding-bottom: 2px; margin: 5px;"
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", "New Game")
    btn.setAttribute("style", inputStyle)
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      drawInGameState(InGameState())
    })
    document.body.appendChild(btn)
    document.body.appendChild(document.createElement("br"))
  }

  private def drawInGameState(inGameState: InGameState): Unit = {

  }

}