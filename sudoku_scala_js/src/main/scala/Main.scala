
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
    if (window.location.host.contains("localhost")) {
      println(InGameState.toJsonDebug(inGameState))
    }
    window.localStorage.setItem(InGameState.STORAGE_KEY_NAME, InGameState.toJson(inGameState))
  }

  private def drawInGameState(inGameState: InGameState): Unit = {
    clearScreen()
    drawGameTable(inGameState)
    drawControls(inGameState)
    drawNumbers(inGameState)
  }

  private def drawGameTable(inGameState: InGameState): Unit = {
    val errorIdxs = inGameState.moveHistory.flatMap {
      case RealMove(idx, isError, _) if isError => Set(idx)
      case _ => Set.empty
    }
    val lastMoveIdx = inGameState.moveHistory.lastOption.map {
      case RealMove(idx, _, _) => idx
      case GhostMove(idx, _) => idx
      case _ => -1
    }.getOrElse(-1)
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
          markSelected(td)
          markError(td)
        } else if (errorIdxs.contains(idx)) {
          markError(td)
        } else if (lastMoveIdx == idx) {
          markSelected(td)
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

  private def addEraseBtn(inGameState: InGameState): Unit = {
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", "Erase")
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      inGameState.selectedIdx.foreach { idx =>
        if (inGameState.grid.cells(idx) != 0 || inGameState.guesses.contains(idx)) {
          makeMove(inGameState, idx, 0)
        }
      }
    })
    document.body.appendChild(btn)
  }

  private def addDrawAllGhostsBtn(inGameState: InGameState): Unit = {
    val btn = document.createElement("input")
    btn.setAttribute("type", "button")
    btn.setAttribute("value", "Draw all ghosts")
    btn.addEventListener("click", { (_: dom.MouseEvent) =>
      drawAllGhosts(inGameState)
    })
    document.body.appendChild(btn)
  }

  private def drawAllGhosts(inGameState: InGameState): Unit = {
    val batchGhostMoves = inGameState.grid.cells.zipWithIndex.filter(_._1 == 0).map {
      case (_, idx) =>
        val oldGuesses = inGameState.guesses.getOrElse(idx, Set.empty)
        val newGuesses = Grid.getGuesses(inGameState.grid.cells, idx)
        drawBatchGhostMoves(inGameState, Array((idx, newGuesses)))
        BatchGhostMove(idx, oldGuesses, newGuesses)
    }
    inGameState.moveHistory += BatchAllGhostMoves(batchGhostMoves)
    saveGameState(inGameState)
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
    addUndoBtn(inGameState)
    addEraseBtn(inGameState)
    addDrawAllGhostsBtn(inGameState)
    addToggleGhostModeBtn(inGameState)
    // TODO: showHint
    document.body.appendChild(document.createElement("br"))
  }

  private def editClass(td: Element)(f: Set[String] => Set[String]): Unit = {
    val oldClasses = Option(td.getAttribute("class")).map(_.split(" ").toSet).getOrElse(Set.empty)
    val newClasses = f(oldClasses)
    td.setAttribute("class", newClasses.mkString(" "))
  }

  private def addClass(td: Element, classNames: Set[String]): Unit = {
    editClass(td) { oldClasses =>
      oldClasses ++ classNames
    }
  }

  private def removeClass(td: Element, classNames: Set[String]): Unit = {
    editClass(td) { oldClasses =>
      oldClasses -- classNames
    }
  }

  private def markSelected(td: Element, replaceAll: Boolean = false): Unit = {
    if (replaceAll) {
      td.setAttribute("class", "selected")
    } else {
      addClass(td, Set("selected"))
    }
  }

  private def unMarkSelected(td: Element): Unit = {
    removeClass(td, Set("selected"))
  }

  private def markError(td: Element): Unit = {
    addClass(td, Set("error"))
  }

  private def unMarkError(td: Element): Unit = {
    removeClass(td, Set("error"))
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
    val isSameMove = inGameState.moveHistory.lastOption.exists {
      case RealMove(lastMoveIdx, _, lastMoveNumber) if lastMoveIdx == idx && lastMoveNumber == number => true
      case _ => false
    }
    if (!isSameMove) {
      val td = document.getElementById(s"cell_$idx")
      clearCell(td)
      if (number > 0) {
        td.append(s"$number")
      }
      val isError = if (number == 0 || Grid.placeNumber(inGameState.grid.cells, idx, number)) {
        unMarkError(td)
        false
      } else {
        markError(td)
        true
      }
      inGameState.moveHistory += RealMove(idx, isError, number)
      inGameState.guesses.remove(idx)
      saveGameState(inGameState)
    }
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
    val isSameMove = inGameState.moveHistory.lastOption.exists {
      case GhostMove(lastMoveIdx, lastMoveGuesses) if lastMoveIdx == idx && lastMoveGuesses.contains(number) => true
      case _ => false
    }
    if (!isSameMove) {
      val newGuesses = inGameState.guesses.getOrElse(idx, Set.empty) ++ Set(number)
      inGameState.guesses += (idx -> newGuesses)
      inGameState.moveHistory += GhostMove(idx, inGameState.guesses(idx))

      val td = document.getElementById(s"cell_$idx")
      addGhostTable(td, newGuesses)

      saveGameState(inGameState)
    }
  }

  private def clearGridCell(inGameState: InGameState, idx: Int): Unit = {
    inGameState.grid.cells(idx) = 0

    val td = document.getElementById(s"cell_${idx}")
    clearCell(td)
  }

  private def undoMove(inGameState: InGameState): Unit = {
    if (inGameState.moveHistory.nonEmpty) {
      val lastMove = inGameState.moveHistory.last

      inGameState.moveHistory.dropRightInPlace(1)

      lastMove match {
        case RealMove(idx, _, _) =>
          clearGridCell(inGameState, idx)
        case GhostMove(idx, _) =>
          clearGridCell(inGameState, idx)
        case BatchAllGhostMoves(batchGhostMoves) =>
          val oldGhostMoves = batchGhostMoves.map(m => (m.idx, m.oldGuesses))
          drawBatchGhostMoves(inGameState, oldGhostMoves)
      }
    }
    if (inGameState.moveHistory.nonEmpty) {
      val prevMove = inGameState.moveHistory.last
      prevMove match {
        case RealMove(idx, isError, number) =>
          if (inGameState.selectedIdx.getOrElse(-1) == idx) {
            inGameState.grid.cells(idx) = number

            val td = document.getElementById(s"cell_$idx")
            clearCell(td)
            td.append(s"$number")
            if (isError) {
              markError(td)
            } else {
              unMarkError(td)
            }
          } else {
            toggleCellSelection(inGameState, idx)
          }
        case GhostMove(idx, guesses) =>
          if (inGameState.selectedIdx.getOrElse(-1) == idx) {
            inGameState.guesses += (idx -> guesses)

            val td = document.getElementById(s"cell_$idx")
            addGhostTable(td, guesses)
          } else {
            toggleCellSelection(inGameState, idx)
          }
        case BatchAllGhostMoves(batchGhostMoves) =>
          val newGhostMoves = batchGhostMoves.map(m => (m.idx, m.newGuesses))
          drawBatchGhostMoves(inGameState, newGhostMoves)
      }
    }
    saveGameState(inGameState)
  }

  private def drawBatchGhostMoves(inGameState: InGameState, ghostMoves: Array[(Int, Set[Int])]): Unit = {
    ghostMoves.foreach {
      case (idx, guesses) =>
        if (guesses.isEmpty) {
          inGameState.guesses.remove(idx)

          val td = document.getElementById(s"cell_$idx")
          clearCell(td)
        } else {
          inGameState.guesses += (idx -> guesses)

          val td = document.getElementById(s"cell_$idx")
          addGhostTable(td, guesses)
        }
    }
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