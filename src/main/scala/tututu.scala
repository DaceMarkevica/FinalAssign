object tututu extends App {import scala.annotation.tailrec
  import scala.io.StdIn
  import scala.util.{Success, Failure, Random,Try}

  /**
   * A simple expirement in tic tac toe the idea
   * is to try to have something that contains state
   * without having vars.
   */
  object Player extends Enumeration {
    val X, O = Value
    type Player = Value
  }

  import Player._
  type Grid = Seq[Option[Player]]

  val grid: Grid = IndexedSeq.fill(9)(None)

  val winningLines =
    IndexedSeq(
      IndexedSeq(0, 1, 2),
      IndexedSeq(3, 4, 5),
      IndexedSeq(6, 7, 8),
      IndexedSeq(0, 3, 6),
      IndexedSeq(1, 4, 7),
      IndexedSeq(2, 5, 8),
      IndexedSeq(1, 4, 8),
      IndexedSeq(2, 4, 6))

  def play(next: Player.Value, grid: Grid): String = {
    def sliceHorizontal(g: Grid) =
      IndexedSeq(
        g.slice(0, 3),
        g.slice(3, 6),
        g.slice(6, 9))

    def playerRepr(p: Option[Player]) = p.map(v => s" $v ").getOrElse("   ")
    def gridRepr(g: Seq[Grid]) = g.map(_.map(playerRepr).mkString("|")).mkString("", "\n---+---+---\n", "\n\n")

    def printGrid(g: Grid) = println(gridRepr(sliceHorizontal(g)))
    printGrid(grid)

    def emptyCells[T](g: Seq[Option[T]]) =
      g.zipWithIndex.collect {
        case (value, index) if value.isEmpty => index
      }

    lazy val randomCell = Random.shuffle(emptyCells(grid)).head

      def playerChoice(g: Grid): Int = {
      print("Please enter selection: ")
      Try(StdIn.readInt()) match {
        case Success(v) =>
          if (emptyCells(g).contains(v)) v
          else {
            println("Position already occupied, please try a different one")
            playerChoice(g)
          }
        case Failure(_) =>
          println("Invalid Position - try a value between 0 and 8")
          playerChoice(g)
      }
    }

    if (emptyCells(grid).isEmpty) {
      "Draw"
    } else {
      val newGrid = if (next == O) {
        grid.updated(randomCell, Some(O))
      } else {
        grid.updated(playerChoice(grid), Some(X))
      }

      def allEqual[A](first: A, seq: Seq[A]): Boolean = seq.forall(_ == first)

      def hasWon(p: Player) = winningLines.foldLeft(false)((i, s) => i || allEqual(Some(p), s.map(newGrid(_))))

      if (hasWon(X)) {
        printGrid(newGrid)
        "X Wins"
      } else if (hasWon(O)) {
        printGrid(newGrid)
        "O Wins"
      } else {
        play(if (next == O) X else O, newGrid)
      }
    }
  }

  println(play(Player.X, grid))
}