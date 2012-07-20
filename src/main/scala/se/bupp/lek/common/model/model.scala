package se.bupp.lek.common.model

import se.bupp.lek.server.Model.PlayerGO

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-06-24
 * Time: 07:28
 * To change this template use File | Settings | File Templates.
 */


class Competitor(val playerId:Int, val teamId:Int)

sealed abstract class PlayerConnectionState {

}

case class Playing() extends PlayerConnectionState {

}
case class Dead(time:Long) extends PlayerConnectionState {

}

class ServerPlayerGO extends PlayerGO {

}

