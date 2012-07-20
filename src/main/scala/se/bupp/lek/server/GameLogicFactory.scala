package se.bupp.lek.server

import se.bupp.lek.server.Server.GameMatchSettings
import se.bupp.lek.server.Model.PlayerJoinRequest
import se.bupp.lek.common.model.Competitor

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 02:19
 * To change this template use File | Settings | File Templates.
 */

object GameLogicFactory {

  trait GameLogicListener {
    def onGameStart() {}

    def onRoundStart() {}

    def onRoundEnd() {}

    def onGameEnd() {}
  }


  def create(settings:GameMatchSettings, gameLogicListener:GameLogicListener) : GameLogic = {
    new GameLogic(settings, gameLogicListener) {

      override def removePlayer(playerId: Int) {
        super.removePlayer(playerId)
      }

      override def playerKilledByPlayer(offender: Int, victim: Int) {
        super.playerKilledByPlayer(offender, victim)
      }

    }
  }



}
