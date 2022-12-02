import scala.util.Random

case class Row(nums: Array[Int]) {
  assert(nums.length == 3 * 3)
  assert(nums.forall(num => num >= 0 && num <= 9))
}

object Row {
  def apply(nums: Int*): Row = this (nums.toArray)

  def generateCompleted: Row = {
    val nums = Random.shuffle((1 to 9).toList).toArray
    Row(nums)
  }
}