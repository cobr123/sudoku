
import org.scalajs.dom
import org.scalajs.dom.{Element, document, window}

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

  private var selectedIdx: Option[Int] = None

  private def drawGameTable(inGameState: InGameState): Unit = {
    val table = document.createElement("table")
    val tr = document.createElement("tr")
    table.append(tr)

    inGameState.grid.cells.zipWithIndex.foldLeft(tr) {
      case (tr, (number, idx)) =>
        val td = document.createElement("td")
        td.setAttribute("id", s"cell_$idx")
        if (number == 0) {
          td.append("")
        } else {
          td.append(s"$number")
        }
        td.addEventListener("click", { (_: dom.MouseEvent) =>
          toggleCellSelection(idx)
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

  private def toggleCellSelection(idx: Int): Unit = {
    selectedIdx.foreach { prevIdx =>
      val td = document.getElementById(s"cell_$prevIdx")
      val classStr = Option(td.getAttribute("class"))
        .map(_.split(" ").toSet.removedAll(Seq("selected")).mkString(" "))
        .getOrElse("")
      td.setAttribute("class", classStr)
    }
    if (selectedIdx.isEmpty || selectedIdx.get != idx) {
      selectedIdx = Some(idx)
      val td = document.getElementById(s"cell_$idx")
      val classStr = (Option(td.getAttribute("class"))
        .map(_.split(" ").toSet)
        .getOrElse(Set.empty) ++ Set("selected")
        ).mkString(" ")
      td.setAttribute("class", classStr)
    } else {
      selectedIdx = None
    }
  }

  private def clearCell(td: Element): Unit = {
    while (td.hasChildNodes()) {
      td.removeChild(td.lastChild)
    }
  }

  private def makeMove(inGameState: InGameState, idx: Int, number: Int): Unit = {
    val td = document.getElementById(s"cell_$idx")
    clearCell(td)
    td.append(s"$number")
    if (Grid.placeNumber(inGameState.grid.cells, idx, number)) {
      td.setAttribute("class", "selected")
    } else {
      td.setAttribute("class", "selected error")
    }
  }

  private def drawNumbers(inGameState: InGameState): Unit = {
    (1 to 9).foreach { number =>
      val btn = document.createElement("input")
      btn.setAttribute("type", "button")
      btn.setAttribute("value", s"$number")
      btn.addEventListener("click", { (_: dom.MouseEvent) =>
        selectedIdx match {
          case Some(idx) =>
            makeMove(inGameState, idx, number)
          case None =>

        }
      })
      document.body.appendChild(btn)
    }
    document.body.appendChild(document.createElement("br"))
  }

}