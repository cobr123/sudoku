
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
    saveGameState(state)
    drawInGameState(state)
  }

  private def saveGameState(inGameState: InGameState): Unit = {
    println(InGameState.toJsonDebug(inGameState))
    window.localStorage.setItem(InGameState.STORAGE_KEY_NAME, InGameState.toJson(inGameState))
  }

  private def drawInGameState(inGameState: InGameState): Unit = {
    clearScreen()
    drawGameTable(inGameState)
    drawControls(inGameState)
    drawNumbers(inGameState)
  }

  private def drawGameTable(inGameState: InGameState): Unit = {
    val errorIdxs = inGameState.moveHistory.filter(_.isError).map(_.idx).toSet
    val lastMoveIdx = inGameState.moveHistory.lastOption.map(_.idx).getOrElse(-1)
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
          toggleCellSelection(inGameState, idx)
        })
        tr.append(td)
        if (lastMoveIdx == idx && errorIdxs.contains(idx)) {
          td.setAttribute("class", "selected error")
        } else if (errorIdxs.contains(idx)) {
          td.setAttribute("class", "error")
        } else if (lastMoveIdx == idx) {
          td.setAttribute("class", "selected")
        }

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

  private def addUndoBtn(inGameState: InGameState): Unit = {
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", "Undo")
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      undoMove(inGameState)
    })
    document.body.appendChild(btn)
  }

  private def drawControls(inGameState: InGameState): Unit = {
    // undo
    addUndoBtn(inGameState)
    // erase
    // drawAllGhosts
    // toggleGhostMode
    // showHint
    document.body.appendChild(document.createElement("br"))
  }

  private def markSelected(td: Element): Unit = {
    val classStr = (Option(td.getAttribute("class"))
      .map(_.split(" ").toSet)
      .getOrElse(Set.empty) ++ Set("selected")
      ).mkString(" ")
    td.setAttribute("class", classStr)
  }

  private def unMarkSelected(td: Element): Unit = {
    val classStr = Option(td.getAttribute("class"))
      .map(_.split(" ").toSet.removedAll(Seq("selected")).mkString(" "))
      .getOrElse("")
    td.setAttribute("class", classStr)
  }

  private def toggleCellSelection(inGameState: InGameState, idx: Int): Unit = {
    inGameState.selectedIdx.foreach { prevIdx =>
      val td = document.getElementById(s"cell_$prevIdx")
      unMarkSelected(td)
    }
    if (inGameState.selectedIdx.isEmpty || inGameState.selectedIdx.get != idx) {
      inGameState.selectedIdx = Some(idx)
      val td = document.getElementById(s"cell_$idx")
      markSelected(td)
    } else {
      inGameState.selectedIdx = None
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
    val isError = if (Grid.placeNumber(inGameState.grid.cells, idx, number)) {
      td.setAttribute("class", "selected")
      false
    } else {
      td.setAttribute("class", "selected error")
      true
    }
    inGameState.moveHistory += Move(idx, isError, number)
    saveGameState(inGameState)
  }

  private def undoMove(inGameState: InGameState): Unit = {
    if (inGameState.moveHistory.nonEmpty) {
      val lastMove = inGameState.moveHistory.last

      inGameState.moveHistory.dropRightInPlace(1)
      inGameState.grid.cells(lastMove.idx) = 0

      val td = document.getElementById(s"cell_${lastMove.idx}")
      clearCell(td)
    }
    if (inGameState.moveHistory.nonEmpty) {
      val prevMove = inGameState.moveHistory.last
      if (inGameState.selectedIdx.getOrElse(-1) == prevMove.idx) {
        inGameState.grid.cells(prevMove.idx) = prevMove.number

        val td = document.getElementById(s"cell_${prevMove.idx}")
        td.append(s"${prevMove.number}")
      } else {
        toggleCellSelection(inGameState, prevMove.idx)
      }
    }
    saveGameState(inGameState)
  }

  private def drawNumbers(inGameState: InGameState): Unit = {
    (1 to 9).foreach { number =>
      val btn = document.createElement("input")
      btn.setAttribute("type", "button")
      btn.setAttribute("value", s"$number")
      btn.addEventListener("click", { (_: dom.MouseEvent) =>
        inGameState.selectedIdx match {
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