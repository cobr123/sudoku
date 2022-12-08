
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
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", s"Continue: ${inGameState.complexity.entryName}")
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      drawInGameState(inGameState)
    })
    document.body.appendChild(btn)
    document.body.appendChild(document.createElement("br"))
  }

  private def addNewGameBtn(): Unit = {
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", "New Game")
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      drawInMenuChooseComplexityState()
    })
    document.body.appendChild(btn)
    document.body.appendChild(document.createElement("br"))
  }

  private def drawInMenuChooseComplexityState(): Unit = {
    clearScreen()
    Complexity.values.foreach { complexity =>
      val btn = document.createElement("input")
      btn.setAttribute("type", "button")
      btn.setAttribute("value", complexity.entryName)
      btn.addEventListener("click", { (_: dom.MouseEvent) =>
        btn.setAttribute("value", s"${btn.getAttribute("value")}. Loading...")
        btn.setAttribute("disabled", "true")
        initAndDrawGameState(complexity)
      })
      document.body.appendChild(btn)
      document.body.appendChild(document.createElement("br"))
    }
  }

  private def initAndDrawGameState(complexity: Complexity): Unit = {
    val state = InGameState(complexity)
    window.localStorage.setItem(InGameState.STORAGE_KEY_NAME, InGameState.toJson(state))
    drawInGameState(state)
  }

  private def drawInGameState(inGameState: InGameState): Unit = {
    clearScreen()
    Grid.printGrid(inGameState.grid.cells)
    drawGameTable(inGameState)
    drawControls(inGameState)
    drawNumbers(inGameState)
  }

  private var selectedCell: Option[Int] = None

  private def drawGameTable(inGameState: InGameState): Unit = {
    val table = document.createElement("table")
    val tr = document.createElement("tr")
    table.append(tr)

    inGameState.grid.cells.zipWithIndex.foldLeft(tr) {
      case (tr, (cell, idx)) =>
        val td = document.createElement("td")
        td.setAttribute("id", s"cell_$idx")
        if (cell == 0) {
          td.append("")
        } else {
          td.append(s"$cell")
        }
        td.addEventListener("click", { (_: dom.MouseEvent) =>
          selectedCell.foreach { idx =>
            val td = document.getElementById(s"cell_$idx")
            td.setAttribute("style", "")
          }
          selectedCell = Some(idx)
          td.setAttribute("style", "background-color: #e2e2e2;")
        })
        tr.append(td)

        if ((idx + 1) % 9 == 0 && idx < inGameState.grid.cells.length) {
          val tr = document.createElement("tr")
          table.append(tr)
          tr
        } else {
          tr
        }
    }
    document.body.appendChild(table)
    document.body.appendChild(document.createElement("br"))
  }

  private def drawControls(inGameState: InGameState): Unit = {

  }

  private def drawNumbers(inGameState: InGameState): Unit = {

  }

}