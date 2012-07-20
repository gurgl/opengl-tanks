package se.bupp.lek

import common.model.Competitor
import org.specs2.mutable.Specification
import server.GameLogicFactory
import server.GameLogicFactory.GameLogicListener
import server.Server.GameMatchSettings
import server.Server.GameMatchSettings.{NumOfRoundsPlayed, NumOfKills, WhenNumOfConnectedPlayersCriteria}

import org.specs2.mock._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.specs2.mock.Mockito

//import org.mockito.Matchers._

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
        roundEndCriteria = NumOfKills(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      /*class ClassGameLogicListener extends GameLogicListener {

      }*/
      val listener = mock[GameLogicListener]
      val gameLogic = GameLogicFactory.create(settings, listener)

      //org.mockito.Mockito.doNot().when(listener).onGameStart()

      /*doAnswer(new Answer[Unit]() {
        def answer(im:InvocationOnMock) {
           ()
        }
      })*/
      gameLogic.addCompetitor(new Competitor(1,1))

      there was no(listener).onGameStart()
      //gameLogic.isGameStarted should be equalTo(false)
      gameLogic.addCompetitor(new Competitor(2,2))
      there was one(listener).onGameStart()

      gameLogic.playerKilledByPlayer(1,2)
      there was noCallsTo(listener)
      gameLogic.playerKilledByPlayer(2,1)
      there was noCallsTo(listener)
      gameLogic.playerKilledByPlayer(2,1)
      there was one(listener).onRoundEnd()
      there was one(listener).onGameEnd()
    }
  }

  "kill based games" should {

    "handle simple 2 on 2 team match" in {
      val settings = new GameMatchSettings(
        startCriteria = WhenNumOfConnectedPlayersCriteria(4),
        roundEndCriteria = NumOfKills(2),
        gameEndCriteria = NumOfRoundsPlayed(1)
      )

      /*class ClassGameLogicListener extends GameLogicListener {

      }*/
      val listener = mock[GameLogicListener]
      val gameLogic = GameLogicFactory.create(settings, listener)

      //org.mockito.Mockito.doNot().when(listener).onGameStart()

      /*doAnswer(new Answer[Unit]() {
        def answer(im:InvocationOnMock) {
           ()
        }
      })*/
      gameLogic.addCompetitor(new Competitor(1,1))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(2,1))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(3,2))
      there was no(listener).onGameStart()
      gameLogic.addCompetitor(new Competitor(4,2))
      there was one(listener).onGameStart()

      gameLogic.playerKilledByPlayer(1,4)
      there was noCallsTo(listener)
      gameLogic.playerKilledByPlayer(3,2)
      there was noCallsTo(listener)
      gameLogic.playerKilledByPlayer(4,1)
      there was one(listener).onRoundEnd()
      there was one(listener).onGameEnd()
    }
  }
}
