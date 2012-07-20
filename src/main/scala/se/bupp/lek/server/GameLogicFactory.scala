package se.bupp.lek.server

import se.bupp.lek.server.Server.GameMatchSettings
import se.bupp.lek.server.Model.PlayerJoinRequest
import se.bupp.lek.common.model.Competitor
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.GameLogic.Kill

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
    def controllablesChanged(keep:AbstractScoringControllables)

    def keepsTic()

    def getCompetitorScore(competitorId:Int) : Int
  }

  class KillBasedStrategy extends ScoreStrategy {

    var playerKills = collection.mutable.HashMap[Int,List[Kill]]()
    var competitorKills = collection.mutable.HashMap[Int,List[Kill]]()


    def init() {}

    def keepsTic() {}

    def controllablesChanged(keep: AbstractScoringControllables) {}

    def playerKilledByPlayer(offender:Int,victim:Int) {

      val offenderCompetitor = gameLogic.competitors.find(_.playerId == offender).get
      val victimCompetitor = gameLogic.competitors.find(_.playerId == victim).get


      if (victimCompetitor.teamId == offenderCompetitor.teamId) {

      } else {
        playerKills += (offender -> (playerKills.get(offenderCompetitor.teamId).flatten.toList :+ new Kill(victim)) )
        competitorKills += (offenderCompetitor.teamId-> (competitorKills.get(offenderCompetitor.teamId).flatten.toList :+ new Kill(victim)) )

        gameLogic.competitorScored(offenderCompetitor.teamId)
      }
    }

    def getCompetitorScore(competitorId:Int) : Int = {
      competitorKills(competitorId).size
    }

  }

  class TimedKeepsScoringStrategy(var currentKeeps:AbstractScoringControllables) extends ScoreStrategy {

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

    def keepsTic() {
        currentKeeps.scorePerTic.foreach {
          case (competitorId, increase) =>
            competitorScore += ( competitorId -> (competitorScore(competitorId) + increase))
            gameLogic.competitorScored(competitorId)

        }
    }

    def getCompetitorScore(competitorId: Int) = competitorScore(competitorId)
  }



  trait GameLogicListener {
    def onGameStart() {}

    def onRoundStart() {}

    def onRoundEnd() {}

    def onGameEnd() {}
  }


  def create(settings:GameMatchSettings, gameLogicListener:GameLogicListener, scoreStrategy:ScoreStrategy) : GameLogic = {
    val gl = new GameLogic(settings, gameLogicListener, scoreStrategy)
    scoreStrategy.gameLogic = gl
    //scoreStrategy.init()
    gl

  }



}
