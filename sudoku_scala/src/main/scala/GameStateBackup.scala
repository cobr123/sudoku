

case class GameStateBackup(grid: Grid,
                           selectedIdx: Option[Int],
                           ghostMode: Boolean,
                           moveHistory: Array[Move],
                           guesses: Map[Int, Set[Int]]
                          )
