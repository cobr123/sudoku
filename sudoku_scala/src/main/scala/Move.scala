

sealed trait Move

final case class RealMove(idx: Int, isError: Boolean, number: Int) extends Move

final case class GhostMove(idx: Int, guesses: Set[Int]) extends Move

final case class BatchGhostMove(idx: Int, oldGuesses: Set[Int], newGuesses: Set[Int])

final case class BatchAllGhostMoves(batchGhostMoves: Array[BatchGhostMove]) extends Move
