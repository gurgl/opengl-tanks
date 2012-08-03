package se.bupp.lek.server

import se.bupp.lek.common.model.Competitor
import se.bupp.lek.server.Server.GameMatchSettings
import se.bupp.lek.server.Server.GameMatchSettings.{ScoreReached, NumOfRoundsPlayed, WhenNumOfConnectedPlayersCriteria}
import se.bupp.lek.server.GameLogicFactory.{ScoreStrategy, GameLogicListener}
import se.bupp.lek.server.GameLogic.Kill


/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 21:01
 * To change this template use File | Settings | File Templates.
 */

object GameLogic {
  class Kill(victim:Int)
}

class GameLogic(var gameSettings:GameMatchSettings, var listener:GameLogicListener, val scoreStrategy:ScoreStrategy) {

  var roundCount = 0

  var competitors = collection.mutable.ArrayBuffer[Competitor]()

  private def gameStart() {
    scoreStrategy.init()
    listener.onGameStart()
    startRound()
  }

  def addCompetitor(pjr: Competitor) {
    competitors += pjr

    queryStartGame()
  }

  def queryStartGame() {
    gameSettings.startCriteria match {
      case WhenNumOfConnectedPlayersCriteria(s) =>
        if(competitors.size == s) {
          gameStart()
        }
      case _ =>
    }
  }

  def isGameStarted = false
  def startRound() = {
    println("roundCount roundCount roundCount roundCount " + roundCount)
    if(roundCount > 0) {
      scoreStrategy.newRound
      listener.onIntermediateRoundStart()
    }
  }

  def competitorScored(scorerComepetitorId:Int) {
    listener.onCompetetitorScored(null)
    gameSettings.roundEndCriteria match {
      case ScoreReached(n) =>
        if (scoreStrategy.getCompetitorScore(scorerComepetitorId) >= n) {
          roundEnded()
        }
      case _ =>
    }
  }


  def removePlayer(playerId:Int) {
    competitors = competitors.filterNot(_.playerId == playerId)
  }

  def roundEnded() {
    //listener.onIntermediateRoundEnd(null,null)
    roundCount = roundCount + 1
    gameSettings.gameEndCriteria match {
      case NumOfRoundsPlayed(r) =>
        if(roundCount >= r) {
          gameEnded()
        } else {
          listener.onIntermediateRoundEnd(null,null)
        }
      case _ =>

    }
  }

  def gameEnded() {
    listener.onGameEnd(null)
  }


/*
  def onGameStart()

  def onIntermediateRoundStart()

  def onIntermediateRoundEnd()

  def onGameEnd()*/

}
