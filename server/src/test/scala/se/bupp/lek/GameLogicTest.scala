package se.bupp.lek

import common.model.Competitor
import org.specs2.mutable.Specification
import server.GamePhaseOrchestratorFactory
import server.GamePhaseOrchestratorFactory.{AbstractScoringControllables, ControllablesScoringStrategy, KillBasedStrategy, GameLogicListener}
import server.Server.GameMatchSettings
import server.Server.GameMatchSettings.{ScoreReached, NumOfRoundsPlayed, WhenNumOfConnectedPlayersCriteria}

import org.specs2.mock._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.specs2.mock.Mockito
import collection.mutable
//import org.mockito.Mockito
import org.specs2.mock.Mockito


/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 19:35
 * To change this template use File | Settings | File Templates.
 */

class GameLogicTest extends Specification with Mockito {

  "kill based games" should {

     "handle simple 1 on 1 free for all, 1 round" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(2),
        roundEndCriteria = ScoreReached(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      val listener = mock[GameLogicListener]
      val gameLogic = GamePhaseOrchestratorFactory.create(settings, listener, new KillBasedStrategy())

      gameLogic.addCompetitor(new Competitor(1,1))

      there was no(listener).onGameStart()
      //gameLogic.isGameStarted should be equalTo(false)
      gameLogic.addCompetitor(new Competitor(2,2))
      there was one(listener).onGameStart()

      gameLogic.scoreStrategy.playerKilledByPlayer(1,2)
      there was one(listener).onCompetetitorScored(any)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      there was two(listener).onCompetetitorScored(any)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      there was three(listener).onCompetetitorScored(any)
      //there was one(listener).onIntermediateRoundEnd(any,any)
      there was one(listener).onGameEnd(any)
      there were noMoreCallsTo(listener)
    }


    "handle simple 1 on 1 free for all, multiple rounds" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(2),
        roundEndCriteria = ScoreReached(1),
        gameEndCriteria = NumOfRoundsPlayed(2)
      )

      val listener = mock[GameLogicListener]
      val gameLogic = GamePhaseOrchestratorFactory.create(settings, listener, new KillBasedStrategy())

      gameLogic.addCompetitor(new Competitor(1,1))

      there was no(listener).onGameStart()
      //gameLogic.isGameStarted should be equalTo(false)
      gameLogic.addCompetitor(new Competitor(2,2))
      there was one(listener).onGameStart()
      //Mockito.reset(gameLogic)
      gameLogic.scoreStrategy.playerKilledByPlayer(1,2)
      there was one(listener).onCompetetitorScored(any)
      there was one(listener).onIntermediateRoundEnd(any,any)
      there was no(listener).onGameEnd(any)

      gameLogic.startRound()
      there was one(listener).onIntermediateRoundStart()
      gameLogic.scoreStrategy.playerKilledByPlayer(1,2)
      there was two(listener).onCompetetitorScored(any)
      there was one(listener).onGameEnd(any)
      there were noMoreCallsTo(listener)

    }

    "handle simple 2 on 2 team match" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(4),
        roundEndCriteria = ScoreReached(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      val listener = mock[GameLogicListener]
      val gameLogic = GamePhaseOrchestratorFactory.create(settings, listener, new KillBasedStrategy())

      gameLogic.addCompetitor(new Competitor(1,1))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(2,1))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(3,2))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(4,2))
      there was one(listener).onGameStart()

      gameLogic.scoreStrategy.playerKilledByPlayer(1,4)
      there was one(listener).onCompetetitorScored(any)
      gameLogic.scoreStrategy.playerKilledByPlayer(3,2)
      there was two(listener).onCompetetitorScored(any)
      gameLogic.scoreStrategy.playerKilledByPlayer(4,1)
      there was three(listener).onCompetetitorScored(any)
      //there was one(listener).onIntermediateRoundEnd(any,any)
      there was one(listener).onGameEnd(any)
      there were noMoreCallsTo(listener)
    }
  }

  "tic based games" should {

    "handle simple 1 on 1 tic based game" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(2),
        roundEndCriteria = ScoreReached(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      val listener = mock[GameLogicListener]
      val gameLogic = GamePhaseOrchestratorFactory.create(settings, listener, new ControllablesScoringStrategy(new AbstractScoringControllables(Map(1L->0,2L->0))))


      gameLogic.addCompetitor(new Competitor(1,1))

      there was no(listener).onGameStart()
      //gameLogic.isGameStarted should be equalTo(false)
      gameLogic.addCompetitor(new Competitor(2,2))
      there was one(listener).onGameStart()

      val score: mutable.HashMap[Long, Int] = gameLogic.scoreStrategy.asInstanceOf[ControllablesScoringStrategy].competitorScore
      score should be equalTo((mutable.HashMap.empty ++= Map(1L->0, 2L->0)))


      gameLogic.scoreStrategy.playerKilledByPlayer(1,2)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      gameLogic.scoreStrategy.keepsTic()

      gameLogic.scoreStrategy.controllablesChanged(new AbstractScoringControllables(Map(1L->1,2L->0)))
      gameLogic.scoreStrategy.keepsTic()
      there was one(listener).onCompetetitorScored(any)
      gameLogic.scoreStrategy.controllablesChanged(new AbstractScoringControllables(Map(1L->0,2L->1)))
      gameLogic.scoreStrategy.keepsTic()
      there was two(listener).onCompetetitorScored(any)
      gameLogic.scoreStrategy.keepsTic()
      there was three(listener).onCompetetitorScored(any)
      //there was one(listener).onIntermediateRoundEnd(any,any)
      there was one(listener).onGameEnd(any)
      there were noMoreCallsTo(listener)
    }
  }

}
