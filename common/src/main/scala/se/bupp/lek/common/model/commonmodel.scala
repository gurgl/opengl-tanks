package se.bupp.lek.common.model

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-06-24
 * Time: 07:28
 * To change this template use File | Settings | File Templates.
 */



object Model {
  type PlayerId = Int
}


import se.bupp.lek.common.Model._
import Model._

class Competitor(val playerId:PlayerId, val teamId:TeamId) {

  override def toString = "(playerId = " + playerId + ", teamId = " + teamId + ")"
}

sealed abstract class PlayerConnectionState {

}

case class NotPlaying() extends PlayerConnectionState
case class Playing() extends PlayerConnectionState {

}
case class Dead(time:Long) extends PlayerConnectionState {

}

class ServerPlayerGO extends PlayerGO {

}

