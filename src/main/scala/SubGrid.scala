import scala.util.Random

case class SubGrid(nums: Array[Int]) {
  assert(nums.length == 3 * 3)
  assert(nums.forall(num => num >= 0 && num <= 9))
}

object SubGrid {
  def apply(nums: Int*): SubGrid = this (nums.toArray)

  def isFilledCorrect(sg: SubGrid): Boolean = {
    isCorrect(sg.nums)
  }

  def isCorrect(nums: Array[Int]): Boolean = {
    nums.filter(_ > 0).distinct.length == 9
  }

  def getCol(nums: Array[Int])(col: Int): Array[Int] = {
    (col, nums) match {
      case (0, Array(
      a, _, _,
      b, _, _,
      c, _, _
      )) => Array(a, b, c)
      case (1, Array(
      _, a, _,
      _, b, _,
      _, c, _
      )) => Array(a, b, c)
      case (2, Array(
      _, _, a,
      _, _, b,
      _, _, c
      )) => Array(a, b, c)
    }
  }

  def getRow(nums: Array[Int])(row: Int): Array[Int] = {
    (row, nums) match {
      case (0, Array(
      a, b, c,
      _, _, _,
      _, _, _
      )) => Array(a, b, c)
      case (1, Array(
      _, _, _,
      a, b, c,
      _, _, _
      )) => Array(a, b, c)
      case (2, Array(
      _, _, _,
      _, _, _,
      a, b, c
      )) => Array(a, b, c)
    }
  }

  def generateCompleted: SubGrid = {
    val nums = Random.shuffle((1 to 9).toList).toArray
    SubGrid(nums)
  }

  def generateRowPart(filter: Set[Int]): Array[Int] = {
    Random.shuffle((1 to 9).filterNot(filter.contains))
      .take(3)
      .toArray
  }

  def printSubGrid(nums: Array[Int]): Unit = {
    println("------------------------------------------------------")
    (0 to 2).map(getRow(nums)(_).mkString("", " ", "")).foreach(println)
  }
}