

enum SubGridLine {
  case Horizontal(row: Int) extends SubGridLine
  case Vertical(column: Int) extends SubGridLine
}