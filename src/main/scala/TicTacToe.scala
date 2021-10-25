object TicTacToe extends App {
  // Scala Training 08-2021
  // Final project
  // Team : project performed in in close cooperation by Liva Leimane & Dace Markevica

  import scala.io.StdIn.readLine

  var player1 = "Player 1"
  var player2 = "Player 2"
  var whoseTurn = "O"
  var winner = " "
  var gameOver = false
  var grid: Array[Array[String]] = Array(Array(" ", " ", " "), Array(" ", " ", " "), Array(" ", " ", " "))
  var startNewGame = true

  /**
   * Main function that runs the game. First, prints out instruction of game, secondly ask names of players.
   * Starts the game and sets possibility for more rounds if players would like to continue playing the game.
   */
  def mainGameLoop(): Unit = {
    printInstructions()
    initPlayerSettings()
    while (startNewGame) {
      singleGameLoop()
      val response = readLine("New game Y/N ?")
      if (response.toLowerCase.startsWith("y")) {
        resetGameState()
      } else startNewGame = false
    }
  }
  mainGameLoop()

  /**
   * Defines game algorithm
   */
  def singleGameLoop(): Unit = {
    while (!gameOver) {
      printGrid(grid)
      playerMove(whoseTurn, grid)
    }
  }

  /**
   * Reset the state of game. The game board is empty and there is no winner in the beginning of the next game.
   */
  def resetGameState() = {
    grid = Array(Array(" ", " ", " "), Array(" ", " ", " "), Array(" ", " ", " "))
    whoseTurn = "O"
    winner = " "
    gameOver = false
  }

  /**
   * The following method prints out instruction of the game on screen
   */
  def printInstructions(): Unit = {
    println("INSTRUCTIONS:")
    println("The grid is of the form")
    println("|-----|-----|-----|")
    for (i <- 0 to 2; j <- 0 to 2) {
      if (j == 0) {
        print("|")
      }
      print(s"($i,$j)")
      if (j == 2) {
        println("|")
        println("|-----|-----|-----|")
      }
      else {
        print("|")
      }
    }
    println("To place your symbol, you must enter the coordinate where you wish to place your symbol.")
    println("Input must be given in the form x,y with no space or brackets.")
    println("Example: Where would you like to place your O? 1,1")
    println("In case of incorrect placement, the system will ask you to try again.")
    println("That's all. Have fun!")
    println()
    println()
    println("GAME BEGINS")
  }

  /**
   * The following method prints the grid after every move
   * @param grid - Array of String; formation of the board of game
   */
  def printGrid(grid: Array[Array[String]]): Unit = {
    println("|---|---|---|")
    for (i <- 0 to 2; j <- 0 to 2) {
      if (j == 0) {
        print("| ")
      }
      print(grid(i)(j))
      if (j == 2) {
        println(" | ")
        println("|---|---|---|")
      }
      else {
        print(" | ")
      }
    }
  }

  /**
   * The following method finds out names of players. Players input.
   */
  def initPlayerSettings(): Unit = {
    player1 = readLine("What is your name dear Player 1? You will be playing O.")
    player2 = readLine("What's your name dear Player 2? You will be playing X.")
  }

  /**
   * The following method makes a player move.
   * Check if player's move has been made in the range of game,
   * as well if the appropriate field has  been already taken, ask for another move.
   * Also provides player's change after every turn and check for result after each turn.
   * @param turn - player's move; string format
   * @param grid - returns grid - formation of board of game; array of string
   */
  def playerMove(turn: String, grid: Array[Array[String]]): Unit = {

    val coordinate_input = readLine(s"Where would you like to place your $turn?")
    val coordinate = coordinate_input.split(",").map(_.toInt)

    if (((coordinate(0) < 0 || coordinate(0) > 2) && (coordinate(1) < 0 || coordinate(1) > 2)) ||
      ((coordinate(0) < 0 || coordinate(0) > 2) && (coordinate(1) >= 0 && coordinate(1) <= 2)) ||
      ((coordinate(0) >= 0 && coordinate(0) <= 2) && (coordinate(1) < 0 || coordinate(1) > 2)))
    {
      println("Oops! No - no - no. This turn is not allowed! Out of game range! Try again!")
      playerMove(turn, grid)
    } else {
      if (grid(coordinate(0))(coordinate(1)) == " ") {
        grid(coordinate(0))(coordinate(1)) = turn
        switchTurn()
        checkForWin()
        checkIfTie()
      }
      else {
        println("Oops! Incorrect move! This field is not available anymore! Try again! :)")
        playerMove(turn, grid)
      }
    }
  }

  /**
   * Switch between players ("O" vs "X")
   */
  def switchTurn(): Unit = {
    if (whoseTurn == "O") {
      whoseTurn = "X"
    }
    else {
      whoseTurn = "O"
    }
  }

  /**
   * Figure out which of player is a winner
   */
  def checkForWin(): Unit = {
    if (grid(0)(0) == "O" && grid(0)(1) == "O" && grid(0)(2) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(1)(0) == "O" && grid(1)(1) == "O" && grid(1)(2) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(2)(0) == "O" && grid(2)(1) == "O" && grid(2)(2) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(0)(0) == "O" && grid(1)(0) == "O" && grid(2)(0) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(0)(1) == "O" && grid(1)(1) == "O" && grid(2)(1) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(0)(2) == "O" && grid(1)(2) == "O" && grid(2)(2) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(0)(0) == "O" && grid(1)(1) == "O" && grid(2)(2) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(0)(2) == "O" && grid(1)(1) == "O" && grid(2)(0) == "O") {
      winner = player1
      thereIsAWinner()
    }
    else if (grid(0)(0) == "X" && grid(0)(1) == "X" && grid(0)(2) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(1)(0) == "X" && grid(1)(1) == "X" && grid(1)(2) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(2)(0) == "X" && grid(2)(1) == "X" && grid(2)(2) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(0)(0) == "X" && grid(1)(0) == "X" && grid(2)(0) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(0)(1) == "X" && grid(1)(1) == "X" && grid(2)(1) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(0)(2) == "X" && grid(1)(2) == "X" && grid(2)(2) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(0)(0) == "X" && grid(1)(1) == "X" && grid(2)(2) == "X") {
      winner = player2
      thereIsAWinner()
    }
    else if (grid(0)(2) == "X" && grid(1)(1) == "X" && grid(2)(0) == "X") {
      winner = player2
      thereIsAWinner()
    }
  }

  /**
   * The following method check if there is a tie, and if so, ends the game
   */
  def checkIfTie(): Unit = {
    if (grid(0)(0) != " " && grid(0)(1) != " " && grid(0)(2) != " " && grid(1)(0) != " " && grid(1)(1) != " "
      && grid(1)(2) != " " && grid(2)(0) != " " && grid(2)(1) != " " && grid(2)(2) != " " && !gameOver) {
      gameOver = true
      printGrid(grid)
      println("GAME OVER")
      println("Well played! It is a tie!")
    }
  }

  /**
   *  The following method congratulates the winner and ends the game
   */
  def thereIsAWinner(): Unit = {
    printGrid(grid)
    println("GAME OVER")
    println(s"$winner is the winner!")
    gameOver = true
  }
}
