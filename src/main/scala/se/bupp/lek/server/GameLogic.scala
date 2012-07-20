package se.bupp.lek.server

import se.bupp.lek.common.model.Competitor
import se.bupp.lek.server.Server.GameMatchSettings
import se.bupp.lek.server.Server.GameMatchSettings.{NumOfRoundsPlayed, NumOfKills, WhenNumOfConnectedPlayersCriteria}
import se.bupp.lek.server.GameLogicFactory.GameLogicListener
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

class GameLogic(var gameSettings:GameMatchSettings, var listener:GameLogicListener) {

  var roundCount = 0
  var playerKills = collection.mutable.HashMap[Int,List[Kill]]()
  var competitorKills = collection.mutable.HashMap[Int,List[Kill]]()

  var competitors = collection.mutable.ArrayBuffer[Competitor]()

  def addCompetitor(pjr: Competitor) {
    competitors += pjr

    gameSettings.startCriteria match {
      case WhenNumOfConnectedPlayersCriteria(s) =>
        if(competitors.size == s) {
          listener.onGameStart()
        }
      case _ =>
    }
  }

  def isGameStarted = false
  def startRound() = {

  }



  def removePlayer(playerId:Int) {

  }

  def roundEnded() {
    listener.onRoundEnd()
    roundCount = roundCount + 1
    gameSettings.gameEndCriteria match {
      case NumOfRoundsPlayed(r) =>
        if(roundCount >= r) {
          gameEnded()
        }
      case _ =>

    }
  }

  def gameEnded() {
    listener.onGameEnd()
  }

  def playerKilledByPlayer(offender:Int,victim:Int) {

    val offenderCompetitor = competitors.find(_.playerId == offender).get
    val victimCompetitor = competitors.find(_.playerId == victim).get


    if (victimCompetitor.teamId == offenderCompetitor.teamId) {

    } else {
      playerKills += (offender -> (playerKills.get(offenderCompetitor.teamId).flatten.toList :+ new Kill(victim)) )
      competitorKills += (offenderCompetitor.teamId-> (competitorKills.get(offenderCompetitor.teamId).flatten.toList :+ new Kill(victim)) )
    }

    gameSettings.roundEndCriteria match {
      case NumOfKills(n) =>
        if (competitorKills(offenderCompetitor.teamId).size >= n) {
          roundEnded()
        }
      case _ =>
    }

  }
/*
  def onGameStart()

  def onRoundStart()

  def onRoundEnd()

  def onGameEnd()*/

}
