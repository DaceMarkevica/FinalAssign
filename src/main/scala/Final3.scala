import scala.io.StdIn.readLine

object Final3 extends App{
  //assignment of global variables
  var player1 = " Player 1"
  var player2 = "Player 2"
  var whoseTurn = ""
  //  var whoseTurn = "O"
  var winner = " "
  var gameOver = false
  var grid: Array[Array[String]] = Array(Array(" ", " ", " "), Array(" ", " ", " "), Array(" ", " ", " "))
  var startNewGame = true

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

  def singleGameLoop(): Unit = {
    //loop that continues the game while the game is not over
    while (!gameOver) {
      printGrid(grid)
      playerMove(whoseTurn, grid)
    }
  }

  def resetGameState() = {
    grid = Array(Array(" ", " ", " "), Array(" ", " ", " "), Array(" ", " ", " "))
    whoseTurn = "Player1"
    //    whoseTurn = "O"
    winner = " "
    gameOver = false
  }

  //This function prints out instructions at the beginning
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
    println("GAME BEGINS")
  }

  //this function prints the grid after every move
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

  def initPlayerSettings(): Unit = {
    player1 = readLine("What is your name Player 1, you will be playing O.")
    player2 = readLine("You will play X, what's your name?")
  }


  //this function makes a player's move
  def playerMove(turn: String, grid: Array[Array[String]]): Unit = {
    val coordinateInput = readLine(s"Where would you like to place your $turn? ")
    val coordinate = coordinateInput.split(",").map(_.toInt)

    if ((((coordinate(0) < 0) || (coordinate(0) > 2)) &&
      ((coordinate(1) < 0) || (coordinate(1) > 2))) ||
      (((coordinate(0) < 0) || (coordinate(0) > 2)) &&
        ((coordinate(1) >= 0) && (coordinate(1) <= 2))) ||
      ((coordinate(0) >= 0) && (coordinate(0) <= 2)) &&
        ((coordinate(1) < 0) || (coordinate(1) > 2))) {
      println("Oops! no - no - no.  Try again!")
      playerMove(turn, grid)
    } else {
      if (grid(coordinate(0))(coordinate(1)) == " ") {
        grid(coordinate(0))(coordinate(1)) = turn
        switchTurn()
        checkForWin()
        checkIfTie()
      }
      else {
        println("Oops! Incorrect move. Try again!")
        playerMove(turn, grid)
      }
    }

    //this function switches the players' turns
    def switchTurn(): Unit = {
      //      if (whoseTurn == "O") {
      //        whoseTurn = "X"
      //      }
      //      else {
      //        whoseTurn = "O"
      //      }
      if (whoseTurn == "Player2") {
        whoseTurn = "Player1"
      }
      else {
        whoseTurn = "Player2"
      }
    }

    //this function checks if any player has won
    def checkForWin(): Unit = {
      if (grid(0)(0) == "O" && grid(0)(1) == "O" && grid(0)(2) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(1)(0) == "O" && grid(1)(1) == "O" && grid(1)(2) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(2)(0) == "O" && grid(2)(1) == "O" && grid(2)(2) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(0)(0) == "O" && grid(1)(0) == "O" && grid(2)(0) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(0)(1) == "O" && grid(1)(1) == "O" && grid(2)(1) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(0)(2) == "O" && grid(1)(2) == "O" && grid(2)(2) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(0)(0) == "O" && grid(1)(1) == "O" && grid(2)(2) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(0)(2) == "O" && grid(1)(1) == "O" && grid(2)(0) == "O") {
        winner = "O"
        thereIsAWinner()
      }
      else if (grid(0)(0) == "X" && grid(0)(1) == "X" && grid(0)(2) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(1)(0) == "X" && grid(1)(1) == "X" && grid(1)(2) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(2)(0) == "X" && grid(2)(1) == "X" && grid(2)(2) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(0)(0) == "X" && grid(1)(0) == "X" && grid(2)(0) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(0)(1) == "X" && grid(1)(1) == "X" && grid(2)(1) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(0)(2) == "X" && grid(1)(2) == "X" && grid(2)(2) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(0)(0) == "X" && grid(1)(1) == "X" && grid(2)(2) == "X") {
        winner = "X"
        thereIsAWinner()
      }
      else if (grid(0)(2) == "X" && grid(1)(1) == "X" && grid(2)(0) == "X") {
        winner = "X"
        thereIsAWinner()
      }
    }

    //this function checks if there is a tie ,and if so, ends the game
    def checkIfTie(): Unit = {
      if (grid(0)(0) != " " && grid(0)(1) != " " && grid(0)(2) != " " && grid(1)(0) != " " && grid(1)(1) != " "
        && grid(1)(2) != " " && grid(2)(0) != " " && grid(2)(1) != " " && grid(2)(2) != " " && !gameOver) {
        gameOver = true
        printGrid(grid)
        println("GAME OVER")
        println("Well played! It is a tie!")
      }
    }

    //this function congratulates the winner and ends the game
    def thereIsAWinner(): Unit = {
      printGrid(grid)
      println("GAME OVER")
      println(s"$winner is the winner!")
      gameOver = true
    }
  }
}
