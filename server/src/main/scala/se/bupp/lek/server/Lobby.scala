package se.bupp.lek.server

import se.bupp.lek.common.Model._
import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.common.model.Model._
import se.bupp.cs3k.api.SimplePlayerInfo

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

  def findByPlayerId(pid:PlayerId) = connectedPlayers.find(_.playerId == pid)

  def removePlayer(playerId:Int) {

    val (pcOpt, newConnectedPlayers) = connectedPlayers.partition(_.playerId == playerId)
    connectedPlayers = newConnectedPlayers
  }

  def addPlayer(pjr: PlayerJoinRequest, masterProvidedPlayerInfoOpt:Option[PlayerInfoServerLobby]) = {

    val player = masterProvidedPlayerInfoOpt match {
      case Some(masterProvidedPlayerInfo) =>
        var ps = new PlayerConnection
        ps.playerId = masterProvidedPlayerInfo.getReportableId.toLong.asInstanceOf[PlayerId]
        ps.teamIdentifier = Option(masterProvidedPlayerInfo.getTeam).map(_.getReportableId.toLong).getOrElse(masterProvidedPlayerInfo.getReportableId.toLong)
        ps.name = masterProvidedPlayerInfo.getName
        ps
      //ps.lastUpdate = None
      case None =>
        var ps = new PlayerConnection
        ps.playerId = connectionSequence
        ps.teamIdentifier = if(pjr.teamIdentifier == -1) ps.playerId else pjr.teamIdentifier
        connectionSequence += 1
        ps.name = "Fixme - should be given by client if server doesnt provide"
        ps
        //ps.lastUpdate = None
    }

    connectedPlayers = connectedPlayers :+ player
    player
  }

}
