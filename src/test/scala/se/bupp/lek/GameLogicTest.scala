package se.bupp.lek

import common.model.Competitor
import org.specs2.mutable.Specification
import server.GameLogicFactory
import server.GameLogicFactory.{AbstractKeep, TimedKeepsScoringStrategy, KillBasedStrategy, GameLogicListener}
import server.Server.GameMatchSettings
import server.Server.GameMatchSettings.{ScoreReached, NumOfRoundsPlayed, WhenNumOfConnectedPlayersCriteria}

import org.specs2.mock._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.specs2.mock.Mockito
import collection.mutable


/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 19:35
 * To change this template use File | Settings | File Templates.
 */

class GameLogicTest extends Specification with Mockito {

  "kill based games" should {

      "handle simple 1 on 1 free for all" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(2),
        roundEndCriteria = ScoreReached(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      val listener = mock[GameLogicListener]
      val gameLogic = GameLogicFactory.create(settings, listener, new KillBasedStrategy())

      gameLogic.addCompetitor(new Competitor(1,1))

      there was no(listener).onGameStart()
      //gameLogic.isGameStarted should be equalTo(false)
      gameLogic.addCompetitor(new Competitor(2,2))
      there was one(listener).onGameStart()

      gameLogic.scoreStrategy.playerKilledByPlayer(1,2)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      there was one(listener).onRoundEnd()
      there was one(listener).onGameEnd()
    }

    "handle simple 2 on 2 team match" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(4),
        roundEndCriteria = ScoreReached(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      val listener = mock[GameLogicListener]
      val gameLogic = GameLogicFactory.create(settings, listener, new KillBasedStrategy())

      gameLogic.addCompetitor(new Competitor(1,1))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(2,1))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(3,2))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(4,2))
      there was one(listener).onGameStart()

      gameLogic.scoreStrategy.playerKilledByPlayer(1,4)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(3,2)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(4,1)
      there was one(listener).onRoundEnd()
      there was one(listener).onGameEnd()
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
      val gameLogic = GameLogicFactory.create(settings, listener, new TimedKeepsScoringStrategy(new AbstractKeep(Map(1->0,2->0))))


      gameLogic.addCompetitor(new Competitor(1,1))

      there was no(listener).onGameStart()
      //gameLogic.isGameStarted should be equalTo(false)
      gameLogic.addCompetitor(new Competitor(2,2))
      there was one(listener).onGameStart()

      val score: mutable.HashMap[Int, Int] = gameLogic.scoreStrategy.asInstanceOf[TimedKeepsScoringStrategy].competitorScore
      score should be equalTo((mutable.HashMap.empty ++= Map(1->0, 2->0)))


      gameLogic.scoreStrategy.playerKilledByPlayer(1,2)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.playerKilledByPlayer(2,1)
      gameLogic.scoreStrategy.keepsTic()

      gameLogic.scoreStrategy.keepsChanged(new AbstractKeep(Map(1->1,2->0)))
      gameLogic.scoreStrategy.keepsTic()
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.keepsChanged(new AbstractKeep(Map(1->0,2->1)))
      gameLogic.scoreStrategy.keepsTic()
      there was noCallsTo(listener)
      gameLogic.scoreStrategy.keepsTic()
      there was one(listener).onRoundEnd()
      there was one(listener).onGameEnd()
    }
  }

}
