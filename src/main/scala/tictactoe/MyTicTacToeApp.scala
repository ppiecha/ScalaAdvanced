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
      case None         => "-"
    }
  }

  case class Play(row: Int, col: Int)

  sealed trait Movable {
    def getMove(turn: Player): TrackedGame[Play]
  }

  case object ConsoleMover extends Movable {
    override def getMove(turn: Player): TrackedGame[Play] =
      State { trackedGameState =>
        {
          println(s"What's your next move $turn:")
          val n    = StdIn.readInt()
          val play = Play(n / 10, n % 10)
          (trackedGameState, play)
        }
      }
  }

  case class ListMover(plays: List[Play]) extends Movable {
    override def getMove(turn: Player): TrackedGame[Play] =
      State { trackedGameState =>
        val tracked = trackedGameState.map { gameState =>
          plays match {
            case ::(head, next) =>
              (gameState.copy(mover = ListMover(next)), head)
            case Nil => throw new RuntimeException("No more moves")
          }
        }
        val (gameState, play) = tracked.value
        (gameState.writer(Nil), play)
      }
  }

  case class GameState(turn: Player, board: Board, mover: Movable)
  object GameState {
    given Show[GameState] = Show.show { gs =>
      s"Turn ${gs.turn}\n${gs.board.show}"
    }
  }
  type TrackedGameState = Writer[List[String], GameState]
  type TrackedGame[A]   = State[TrackedGameState, A]
  type Game[A]          = State[GameState, A]

  class Board(sz: Int, board: Map[Board.Cell, Player]) {
    val size: Int                                          = sz
    def updated(row: Int, col: Int, player: Player): Board = new Board(sz, board = board.updated((row, col), player))
    def readCell(row: Int, col: Int): Option[Player]       = board.get((row, col))
    def readCol(col: Int): List[Option[Player]]            = List.range(0, sz).map(row => readCell(row, col))
    def readRow(row: Int): List[Option[Player]]            = List.range(0, sz).map(col => readCell(row, col))
    def readDiags: (List[Option[Player]], List[Option[Player]]) = {
      val idx = List.range(0, sz)
      (idx.map(n => readCell(n, n)), idx.map(n => readCell(sz - n - 1, n)))
    }
  }

  object Board {
    type Cell = (Int, Int)
    given showBoard: Show[Board] = Show.show { b =>
      (0 until b.size)
        .map(row => b.readRow(row).map(Player.showOptPlayer.show).mkString(" "))
        .mkString("\n")
    }
    def emptyBoard(size: Int): Board =
      new Board(size, Map.empty[Board.Cell, Player])
  }

  class TicTacToe(size: Int) {

    val initTrackedGameState: TrackedGameState =
      GameState(Player.P1, Board.emptyBoard(size), ConsoleMover).writer(Nil)

    def switchTurns: TrackedGame[Unit] =
      State.modify(trackedGameState =>
        trackedGameState.map { gameState =>
          gameState.copy(turn = if gameState.turn == Player.P1 then Player.P2 else Player.P1)
        }
      )

    def currentPlayerWon: TrackedGame[Boolean] =
      State
        .get[TrackedGameState]
        .map(gameState => {
          val diags = gameState.value.board.readDiags
          val rowsCols = List
            .range(0, gameState.value.board.size)
            .flatMap(n =>
              List(
                gameState.value.board.readRow(n),
                gameState.value.board.readCol(n)
              )
            )
          rowsCols
            .appendedAll(diags.toList)
            .exists(line => line.forall(c => c.isDefined && c.get == gameState.value.turn))
        })

    def doPlay(play: Play): TrackedGame[Unit] =
      State.modify { trackedGameState =>
        trackedGameState
          .tell(List(play.toString))
          .map(gameState => {
            if gameState.board.readCell(play.row, play.col).isDefined then
              throw new RuntimeException("Cell already defined")
            else gameState.copy(board = gameState.board.updated(play.row, play.col, gameState.turn))
          })
      }

    // List(Play(0,0), Play(0,1), Play(1,1), Play(0,2), Play(2,2))
    def replay(plays: List[Play]): GameState =
      val (trackedGameState, winner) = game
        .run(
          GameState(
            Player.P1,
            Board.emptyBoard(3),
            ListMover(plays)
          ).writer(Nil)
        )
        .value
      trackedGameState.value  

    def game: TrackedGame[Player] =
      for {
        trackedGameState <- State.get[TrackedGameState]
        gameState = trackedGameState.value
        _         = println(gameState.show)
        play          <- gameState.mover.getMove(gameState.turn)
        _             <- doPlay(play)
        weHaveAWinner <- currentPlayerWon // check winner
        winner <-
          if weHaveAWinner then trackedGameState.value.turn.pure[TrackedGame]
          else switchTurns >> game // switch players
      } yield winner
  }

  val ticTacToeGame = new TicTacToe(3)
  val gameState = ticTacToeGame.replay(List(Play(0, 0), Play(0, 1), Play(1, 1), Play(0, 2), Play(2, 2)))
  //import GameState.given_Show_GameState
  println(gameState.show)
  // val (finalState, winner) =
  //   ticTacToeGame.game.run(ticTacToeGame.initTrackedGameState).value
  // val (logs, state) = finalState.run
  // println(logs)
  // println(state.board.show)
  // println(s"The winner is $winner")
}

/*
Exercises:
- Keep track of the plays and modify playGame to return both the winner and the
  list of plays
- Write a show instance for GameState in the companion object
- Write a def replay(plays: List[Play]): GameState function inside TicTacToe that
  will execute the plays and return the final GameState
 */
