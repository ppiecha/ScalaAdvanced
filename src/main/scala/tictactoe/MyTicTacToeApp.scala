package tictactoe

import cats.data.State
import cats.*
import cats.data.*
import cats.implicits.*

import scala.io.StdIn

object MyTicTacToeApp extends App {
  
  sealed trait Player
  object Player {
    case object P1 extends Player
    case object P2 extends Player
    given Show[Player] = Show.show {
      case P1 => "X"
      case P2 => "O"
    }
    given showOptPlayer: Show[Option[Player]] = Show.show {
      case Some(player) => player.show
      case None => "-"
    }
  }
  
  case class GameState(turn: Player, board: Board)
  object GameState {

  }
  type TrackedGameState = Writer[List[String], GameState]
  type TrackedGame[A] = State[TrackedGameState, A]
  type Game[A] = State[GameState, A]
  
  class Board(sz: Int, board: Map[Board.Cell, Player]) {
    val size: Int = sz
    def updated(row: Int, col: Int, player: Player): Board = new Board(sz, board = board.updated((row, col), player))
    def readCell(row: Int, col: Int): Option[Player] = board.get((row, col))
    def readCol(col: Int): List[Option[Player]] = List.range(0, sz).map(row => readCell(row, col))
    def readRow(row: Int): List[Option[Player]] = List.range(0, sz).map(col => readCell(row, col))
    def readDiags: (List[Option[Player]], List[Option[Player]]) = {
      val idx = List.range(0, sz)
      (idx.map(n => readCell(n, n)), idx.map(n => readCell(sz-n-1, n)))
    }
  }
  
  object Board {
    type Cell = (Int, Int)
    given showBoard: Show[Board] = Show.show { b =>
      (0 until b.size).map(row => b.readRow(row).map(Player.showOptPlayer.show).mkString(" ")).mkString("\n")
    }
    def emptyBoard(size: Int): Board = new Board(size, Map.empty[Board.Cell, Player])
  }
  
  case class Play(row: Int, col: Int)

  class TicTacToe(size: Int) {

    def switchTurns: TrackedGame[Unit] =
      State.modify(trackedGameState =>
        trackedGameState.map {
          gameState => gameState.copy(turn = if gameState.turn == Player.P1 then Player.P2 else Player.P1)
        })
    
    def currentPlayerWon: TrackedGame[Boolean] =
      State.get[TrackedGameState].map(gameState => {
        val diags = gameState.value.board.readDiags
        val rowsCols = List.range(0, gameState.value.board.size)
          .flatMap(n => List(gameState.value.board.readRow(n), gameState.value.board.readCol(n)))
        rowsCols.appendedAll(diags.toList).exists(line => line.forall(c => c.isDefined && c.get == gameState.value.turn))
      })

    def doPlay(play: Play): TrackedGame[Unit] =
      State.modify { trackedGameState =>
        trackedGameState.tell(List(play.toString)).map(gameState => {
          if gameState.board.readCell(play.row, play.col).isDefined then throw new RuntimeException("Cell already defined")
          else gameState.copy(board = gameState.board.updated(play.row, play.col, gameState.turn))
        })
      }

    def readMove(turn: Player): Play = {
      println(s"What's your next move $turn:")
      val n = StdIn.readInt()
      Play(n / 10, n % 10)
    }

    def game: TrackedGame[Player] =
      for {
        gameState <- State.get[TrackedGameState]
        _ = println(gameState.value.board.show)
        play = readMove(gameState.value.turn) // read move
        _ <- doPlay(play)
        weHaveAWinner <- currentPlayerWon // check winner
        winner <- if weHaveAWinner then gameState.value.turn.pure[TrackedGame]
                  else switchTurns >> game // switch players
      } yield winner
  }

  val ticTacToeGame = new TicTacToe(3)
  val (finalState, winner) = ticTacToeGame.game.run(GameState(Player.P1, Board.emptyBoard(3)).writer(Nil)).value
  val (logs, state) = finalState.run
  println(logs)
  println(state.board.show)
  println(s"The winner is $winner")
}

/*
Exercises:
- Keep track of the plays and modify playGame to return both the winner and the
  list of plays
- Write a show instance for GameState in the companion object
- Write a def replay(plays: List[Play]): GameState function inside TicTacToe that
  will execute the plays and return the final GameState
*/
