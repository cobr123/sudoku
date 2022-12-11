
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
    btn.setAttribute("value", s"Continue: ${inGameState.complexity.toString}")
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
      btn.setAttribute("value", complexity.toString)
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
    val errorIdxs = inGameState.moveHistory.filter {
      case RealMove(_, isError, _) => isError
      case _ => false
    }.map(_.idx).toSet
    val lastMoveIdx = inGameState.moveHistory.lastOption.map(_.idx).getOrElse(-1)
    val table = document.createElement("table")
    val tr = document.createElement("tr")
    table.append(tr)

    inGameState.grid.cells.zipWithIndex.foldLeft(tr) {
      case (tr, (number, idx)) =>
        val td = document.createElement("td")
        td.setAttribute("id", s"cell_$idx")
        val guesses = inGameState.guesses.get(idx)
        if (guesses.isDefined) {
          addGhostTable(td, guesses.get)
        } else if (number == 0) {
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

  private def addToggleGhostModeBtn(inGameState: InGameState): Unit = {
    val cb = document.createElement("input")
    cb.setAttribute("type", "checkbox")
    if (inGameState.ghostMode) {
      cb.setAttribute("checked", "true")
    }
    cb.id = "toggleGhostModeCheckbox"
    cb.addEventListener("change", { (_: dom.MouseEvent) =>
      inGameState.ghostMode = !inGameState.ghostMode
      saveGameState(inGameState)
    })
    val label = document.createElement("label")
    label.innerText = "Ghost mode"
    label.setAttribute("for", cb.id)
    document.body.appendChild(label)
    document.body.appendChild(cb)
  }

  private def drawControls(inGameState: InGameState): Unit = {
    // undo
    addUndoBtn(inGameState)
    // erase
    // drawAllGhosts
    // toggleGhostMode
    addToggleGhostModeBtn(inGameState)
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
    inGameState.moveHistory += RealMove(idx, isError, number)
    inGameState.guesses.remove(idx)
    saveGameState(inGameState)
  }

  private def addGhostTable(td: Element, guesses: Set[Int]): Unit = {
    val table = document.createElement("table")
    table.setAttribute("class", "guess")
    for (row <- 0 to 2) {
      val tr = document.createElement("tr")
      for (col <- 0 to 2) {
        val td = document.createElement("td")
        td.setAttribute("class", "guess")
        val guessIdx = row * 3 + col
        val number = guessIdx + 1
        if (guesses.contains(number)) {
          td.append(s"$number")
        }
        tr.append(td)
      }
      table.append(tr)
    }
    clearCell(td)
    td.append(table)
  }

  private def makeGhostMove(inGameState: InGameState, idx: Int, number: Int): Unit = {
    val newGuesses = inGameState.guesses.getOrElse(idx, Set.empty) ++ Set(number)
    inGameState.guesses += (idx -> newGuesses)
    inGameState.moveHistory += GhostMove(idx, inGameState.guesses(idx))

    val td = document.getElementById(s"cell_$idx")
    addGhostTable(td, newGuesses)

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
        prevMove match {
          case RealMove(idx, _, number) =>
            inGameState.grid.cells(idx) = number

            val td = document.getElementById(s"cell_$idx")
            td.append(s"$number")
          case GhostMove(idx, guesses) =>
            inGameState.guesses += (idx -> guesses)

            val td = document.getElementById(s"cell_$idx")
            addGhostTable(td, guesses)
        }
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
            if (inGameState.ghostMode) {
              makeGhostMove(inGameState, idx, number)
            } else {
              makeMove(inGameState, idx, number)
            }
          case None =>

        }
      })
      document.body.appendChild(btn)
    }
    document.body.appendChild(document.createElement("br"))
  }

}