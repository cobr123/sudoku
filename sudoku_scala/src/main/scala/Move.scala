

sealed trait Move(val idx: Int)

final case class RealMove(override val idx: Int, isError: Boolean, number: Int) extends Move(idx)

final case class GhostMove(override val idx: Int, guesses: Set[Int]) extends Move(idx)
