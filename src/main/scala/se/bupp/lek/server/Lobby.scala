package se.bupp.lek.server

import se.bupp.lek.server.Model.{PlayerConnection, GameParticipant, PlayerGO, PlayerJoinRequest}
import com.jme3.math.{Quaternion, Vector3f}

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-09-13
 * Time: 19:59
 * To change this template use File | Settings | File Templates.
 */
class Lobby() {

  var connectionSequence = 0

  var connectedPlayers = List[PlayerConnection]()

  def removePlayer(playerId:Int) {

    val (pcOpt, newConnectedPlayers) = connectedPlayers.partition(_.playerId == playerId)
    connectedPlayers = newConnectedPlayers
  }

  def addPlayer(pjr: PlayerJoinRequest) = {

    var playerId = -1


      playerId = connectionSequence
      val player = {

        var ps = new PlayerConnection
        ps.playerId = playerId
        ps.teamIdentifier = if(pjr.teamIdentifier == -1) playerId else pjr.teamIdentifier

        //ps.lastUpdate = None
        ps
      }

      connectedPlayers = connectedPlayers :+ player
      //world.spawnPlayer(player)

      connectionSequence += 1

    player
  }

}
