package se.bupp.lek.server

import se.bupp.lek.server.Server.GameMatchSettings
import se.bupp.lek.common.Model.PlayerJoinRequest
import se.bupp.lek.common.model.Competitor
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.GameLogic.Kill
import se.bupp.lek.server.GameLogicFactory.KillBasedStrategy.PlayerKill

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 02:19
 * To change this template use File | Settings | File Templates.
 */

object GameLogicFactory {

  class AbstractScoringControllables(val scorePerTic:Map[Int, Int]) {

  }

  trait ScoreStrategy {

    var gameLogic:GameLogic = _

    def init()
    def playerKilledByPlayer(offender:Int,victim:Int)
    def newRound
    def controllablesChanged(keep:AbstractScoringControllables)

    def keepsTic()

    def getCompetitorScore(competitorId:Int) : Int
  }

  object KillBasedStrategy {
    class PlayerKill(val of:Int,val vi:Int) extends AbstractScoreDescription
  }

  class KillBasedStrategy extends ScoreStrategy {



    class RoundScore() {
      var playerKills = collection.mutable.HashMap[Int,List[Kill]]()
      var competitorKills = collection.mutable.HashMap[Int,List[Kill]]()
    }


    var roundResults = List[RoundScore]()
    var currentRound:RoundScore = _

    def init() {
      roundResults = List[RoundScore]()
      currentRound = new RoundScore
    }

    def newRound = {
      roundResults = roundResults :+ currentRound
      currentRound = new RoundScore
    }

    def keepsTic() {}

    def controllablesChanged(keep: AbstractScoringControllables) {}

    def playerKilledByPlayer(offender:Int,victim:Int) {

      val offenderCompetitor = gameLogic.competitors.find(_.playerId == offender).get
      val victimCompetitor = gameLogic.competitors.find(_.playerId == victim).get


      if (victimCompetitor.teamId == offenderCompetitor.teamId) {

      } else {
        currentRound.playerKills += (offender -> (currentRound.playerKills.get(offenderCompetitor.teamId).flatten.toList :+ new Kill(victim)) )
        currentRound.competitorKills += (offenderCompetitor.teamId-> (currentRound.competitorKills.get(offenderCompetitor.teamId).flatten.toList :+ new Kill(victim)) )

        gameLogic.competitorScored(new PlayerKill(offenderCompetitor.playerId, victimCompetitor.playerId), offenderCompetitor.teamId)
      }
    }

    def getCompetitorScore(competitorId:Int) : Int = {
      currentRound.competitorKills(competitorId).size
    }

  }

  class ControllablesScoringStrategy(var currentKeeps:AbstractScoringControllables) extends ScoreStrategy {

    var competitorScore = collection.mutable.HashMap.empty[Int,Int]

    def init() {
      gameLogic.competitors.groupBy(_.teamId).keys.foreach {
        comp => competitorScore += (comp -> 0)
      }
    }

    def playerKilledByPlayer(offender: Int, victim: Int) {}

    def controllablesChanged(keep: AbstractScoringControllables) {
      currentKeeps = keep
    }

    def newRound {

    }

    def keepsTic() {
        currentKeeps.scorePerTic.foreach {
          case (competitorId, increase) =>

            competitorScore += ( competitorId -> (competitorScore(competitorId) + increase))
            if (increase > 0) gameLogic.competitorScored(null, competitorId)

        }
    }

    def getCompetitorScore(competitorId: Int) = competitorScore(competitorId)
  }



  trait GameLogicListener {
    def onGameStart()

    def onIntermediateRoundStart()

    def onIntermediateRoundEnd(roundResults:RoundResults, standing:GameTotalResults)

    def onGameEnd(totals:GameTotalResults)

    def onCompetetitorScored(scoreDescription:AbstractScoreDescription)
  }

  class AbstractScoreDescription()

  class GameTotalResults()

  class RoundResults()


  def create(settings:GameMatchSettings, gameLogicListener:GameLogicListener, scoreStrategy:ScoreStrategy) : GameLogic = {
    val gl = new GameLogic(settings, gameLogicListener, scoreStrategy)
    scoreStrategy.gameLogic = gl
    //scoreStrategy.init()
    gl

  }



}
