package se.bupp.lek.server

import se.bupp.lek.server.Server._
import se.bupp.lek.common.Model.{ScoreMessage, StartRoundRequest, PlayerJoinRequest}
import se.bupp.lek.common.model.Competitor
import se.bupp.lek.common.model.Model._
import se.bupp.lek.server.Server.GameMatchSettings.{NumOfRoundsPlayed, WhenNumOfConnectedPlayersCriteria, ScoreReached}
import se.bupp.lek.server.GameLogic.Kill
import se.bupp.lek.server.GameLogicFactory.KillBasedStrategy.PlayerKill
import se.bupp.cs3k.example.ExampleScoreScheme.{JavaTuple2, ExContestScore}
import collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import se.bupp.cs3k.example.ExampleScoreScheme.JavaTuple2
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.Server.FreeForAll
import se.bupp.lek.server.Server.GameMatchSettings.WhenNumOfConnectedPlayersCriteria
import se.bupp.lek.server.Server.TeamDeathmatch
import se.bupp.lek.server.Server.GameMatchSettings.NumOfRoundsPlayed

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 02:19
 * To change this template use File | Settings | File Templates.
 */

object GameLogicFactory {


  trait ScoreSerializer {
    def serialize(s:AnyRef) : String
  }



  class AbstractScoringControllables(val scorePerTic:Map[Long, Int]) {

  }

  trait ScoreStrategy {

    var gameLogic:GameLogic = _

    def init()
    def playerKilledByPlayer(offender:PlayerId,victim:PlayerId)
    def newRound
    def endRound
    def controllablesChanged(keep:AbstractScoringControllables)

    def keepsTic()

    def getCompetitorScore(competitorId:Long) : Int

    def getEndGameResult() : AbstractGameResult = null
  }

  object KillBasedStrategy {
    class PlayerKill(val of:PlayerId,val vi:PlayerId) extends AbstractScoreDescription
    class EndGameResult(val s:ExContestScore) extends AbstractGameResult {
      def getSerializedResult(serializer: ScoreSerializer) = serializer.serialize(s)
    }
  }

  class KillBasedStrategy extends ScoreStrategy {

    class RoundScore(competitors:ArrayBuffer[Competitor]) {
      var playerKills = collection.mutable.HashMap[PlayerId,List[Kill]]()
      var competitorKills = competitors.map( c => c.teamId -> List[Kill]()).toMap//collection.mutable.HashMap[Int,List[Kill]]()
      println("New round created " + competitorKills)
      println("Competitors" + competitors)
    }


    var roundResults = List[RoundScore]()
    var currentRound:RoundScore = _

    def init() {
      roundResults = List[RoundScore]()
      //createRound
    }

    def endRound = {
      roundResults = roundResults :+ currentRound
    }

    def newRound = {
      createRound
    }

    def createRound() {
      currentRound = new RoundScore(gameLogic.competitors)
    }

    def keepsTic() {}

    def controllablesChanged(keep: AbstractScoringControllables) {}

    def playerKilledByPlayer(offender:Int,victim:Int) {

      val offenderCompetitor = gameLogic.competitors.find(_.playerId == offender).get
      val victimCompetitor = gameLogic.competitors.find(_.playerId == victim).get


      if (victimCompetitor.teamId == offenderCompetitor.teamId) {

      } else {
        currentRound.playerKills += (offender -> (currentRound.playerKills.get(offenderCompetitor.playerId).toList.flatten :+ new Kill(victim)) )
        currentRound.competitorKills += (offenderCompetitor.teamId-> (currentRound.competitorKills.get(offenderCompetitor.teamId).toList.flatten :+ new Kill(victim)) )

        gameLogic.competitorScored(new PlayerKill(offenderCompetitor.playerId, victimCompetitor.playerId), offenderCompetitor.teamId)
      }
    }

    def getCompetitorScore(competitorId:Long) : Int = {
      currentRound.competitorKills(competitorId).size
    }

    override def getEndGameResult() = {


      val res = gameLogic.competitors.map( c => c.teamId -> roundResults.foldLeft(new JavaTuple2(0,0)) {
        case (t,round)=>
          println(round.competitorKills)
          println(c.teamId)
          t.a = t.a + round.competitorKills(c.teamId).size
          t
      }
      )


      import scala.collection.JavaConversions.mapAsJavaMap
      val m:Map[Long,JavaTuple2] = res.toMap
      new KillBasedStrategy.EndGameResult(new ExContestScore(m.map(e => (e._1.toLong,e._2))))

    }
  }

  class ControllablesScoringStrategy(var currentKeeps:AbstractScoringControllables) extends ScoreStrategy {

    var competitorScore = collection.mutable.HashMap.empty[Long,Int]

    def init() {
      gameLogic.competitors.groupBy(_.teamId).keys.foreach {
        comp => competitorScore += (comp -> 0)
      }
    }

    def playerKilledByPlayer(offender: Int, victim: Int) {}

    def controllablesChanged(keep: AbstractScoringControllables) {
      currentKeeps = keep
    }

    def endRound {

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

    def getCompetitorScore(competitorId: Long) = competitorScore(competitorId)
  }



  trait GameLogicListener {

    val log = LoggerFactory.getLogger(classOf[GameLogicListener])

    def onGameStart()

    def onIntermediateRoundStart()

    def onIntermediateRoundEnd(roundResults:RoundResults, standing:AbstractGameResult)

    def onGameEnd(totals:AbstractGameResult)

    def onCompetetitorScored(scoreDescription:AbstractScoreDescription)
  }

  class AbstractScoreDescription()

  trait AbstractGameResult {
    def getSerializedResult(serializer:ScoreSerializer) : String
  }

  class RoundResults()


  def create(settings:GameMatchSettings, gameLogicListener:GameLogicListener, scoreStrategy:ScoreStrategy) : GameLogic = {
    val gl = new GameLogic(settings, gameLogicListener, scoreStrategy)
    scoreStrategy.gameLogic = gl
    //scoreStrategy.init()
    gl

  }

  def createGameLogic(gameType:AbstractGameDescription, server:Server) = {

    val startCriteria = gameType match {
      case FreeForAll(i) => WhenNumOfConnectedPlayersCriteria(i)
      case TeamDeathmatch(t,np) => WhenNumOfConnectedPlayersCriteria(t*np)
    }

    val gameSettings: GameMatchSettings = new GameMatchSettings(
      startCriteria = startCriteria,
      roundEndCriteria = ScoreReached(2),
      gameEndCriteria = NumOfRoundsPlayed(2)
    )

    var gameLogicListener = createListener(gameType, server, gameSettings)
    GameLogicFactory.create(gameSettings, gameLogicListener, new KillBasedStrategy())
  }


  def createListener(gameType:AbstractGameDescription, server:Server, gameSettings: GameMatchSettings ) = {
    var gameLogicListener = new GameLogicListener() {
      def onGameStart() {

        // send countdown message
        // add timer to start round
        // leave lobby mode
        // enter game mode
        log.info("onGameStart")
        server.appendToQueue(GameStarted())

      }

      def onIntermediateRoundStart() {
        // send round started message
        log.info("Round Started")
        server.networkState.sendToAllClients(new StartRoundRequest)
        server.worldSimulator.spawnAllParticipants()
      }

      def onCompetetitorScored(scoreDescription: AbstractScoreDescription) {

        log.info("Someone scored")
        val playState: PlayState = server.getStateManager.getState(classOf[PlayState])

        var kill: PlayerKill = scoreDescription.asInstanceOf[PlayerKill]
        playState.postMessage(new ScoreMessage(kill.of, kill.vi))
        // send displayable score modification
      }

      def onIntermediateRoundEnd(roundResults: RoundResults, standing: AbstractGameResult) {
        // send countdown message
        // add timer to start round
        server.onUpdateSentMessageQueue.enqueue(RoundEnded())
      }

      def onGameEnd(totals: AbstractGameResult) {
        log.debug("Scheduling Game End")

        server.onUpdateSentMessageQueue.enqueue(GameEnded(totals))
      }
    }

    gameLogicListener

  }



}
