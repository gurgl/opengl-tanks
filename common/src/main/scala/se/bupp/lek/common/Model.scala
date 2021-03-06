package se.bupp.lek.common

import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.math.{Quaternion, Vector3f}
import scala.collection.JavaConversions.asScalaBuffer
import se.bupp.lek.common.model.{ServerPlayerGO, Playing, PlayerConnectionState}
import se.bupp.lek.common.model.Model._
import com.jme3.scene.Node
import se.bupp.cs3k.api.SimplePlayerInfo

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 6/13/12
 * Time: 12:01 AM
 * To change this template use File | Settings | File Templates.
 */




trait SceneGraphAccessors {
  def projectNodeChildrenByData[U](nodeKey: String, userDataKey: String) = {
    getNode(nodeKey).getChildren.map(x => (x.getUserData[U](userDataKey), x))
  }
  def getNode(str: String): Node

}


object Model {

  type TeamId = Long

  type TimeStamp = Long

  type PlayerInfoServerLobby = SimplePlayerInfo

  val getNetworkMessages = List[Class[_ <: AnyRef]](
    classOf[PlayerJoinRequest],
    classOf[PlayerJoinResponse],
    classOf[PlayerActionRequest],
    classOf[GameWorldResponse],
    classOf[Vector3f],
    classOf[Quaternion],
    classOf[PlayerGO],
    classOf[ProjectileGO],
    classOf[MotionGO],
    classOf[OrientationGO],
    classOf[ProjectileFireGO],
    classOf[java.util.ArrayList[PlayerGO]],
    classOf[ServerGameWorld],
    classOf[RoundOverRequest],
    classOf[StartRoundRequest],
    classOf[StartGameRequest],
    classOf[GameOverRequest],
    classOf[SpawnPlayer],
    classOf[KillPlayer],
    classOf[ScoreMessage],
    classOf[ServerWorldStateChange],
    classOf[OrderedMessage]


  )

  class PlayerConnection extends Savable {
    var playerId: PlayerId = _

    var teamIdentifier: Long = -1

    var name: String = _

    override def read(reader: JmeImporter) {}

    override def write(writer: JmeExporter) {}
  }

  class GameParticipant extends Savable {
    var gameState: PlayerGO = _

    var serverClientClock:Long = 0
    var state:PlayerConnectionState = Playing()

    var seqId : Int = _

    var updates:Seq[MotionGO] = Seq.empty

    var lastSimulationServerTime:Long = _

    var playerId: PlayerId = _

    var teamIdentifier: Long = -1

    var name: String = _


    override def read(reader: JmeImporter) {}

    override def write(writer: JmeExporter) {}
  }

  type Reorientation = (Vector3f,Quaternion)

  class Orientation(or:Orientation) {

    var position: Vector3f = _
    var direction: Quaternion = _

    if(or != null) {
      position = or.position.clone()
      direction = or.direction.clone()
    }

    def this() = this(null)
    def this(p:Vector3f,r:Quaternion) = {
      this()
      position = p.clone()
      direction = r.clone()
    }

    //def this(o:Orientation) = this(o.position,o.direction)

    def orientation = this

    def reorientate(reorientation:Reorientation) = {
      new Orientation(position.add(reorientation._1),reorientation._2.mult(orientation.direction))
    }

    def orientation_=(o: Orientation): Unit = {
      position = o.position.clone()
      direction = o.direction.clone()
    }
    override def toString() = "(" + position + ", " + direction + ")"
  }


  class OrientationGO(
    or:OrientationGO
  ) extends Orientation(or) {

    //def this() = this(null, null)

    def this() = this(null)

    def this(p:Vector3f,r:Quaternion) = {
      this()
      position = p
      direction = r
    }
  }

  class AbstractGameObject(ago:AbstractGameObject) extends OrientationGO(ago) {
    var clientSeqId: Int = _
    if(ago != null) {
      clientSeqId = ago.clientSeqId
    }
    //def this(clientSeqId: Int, or:OrientationGO)
    def this() = this(null)

  }

  /*trait IsUniquelyIdentifieble {
    def id:OwnedGameObjectId
  }*/
  type OwnedGameObjectId = (Int, Int)

  class AbstractOwnedGameObject(aogo:AbstractOwnedGameObject) extends AbstractGameObject(aogo) {
    /* Only applic to player really */
    var sentToServerByClient: Long = -1
    var playerId: PlayerId = -1

    if(aogo != null) {
      sentToServerByClient = aogo.sentToServerByClient
      playerId = aogo.playerId

    }

    def this() = {
      this(null)
    }
    def id: OwnedGameObjectId = (clientSeqId, playerId)

    def id_=(goid: OwnedGameObjectId) = {
      clientSeqId = goid._1; playerId = goid._2
    }
  }


  class ProjectileGO(pgo: ProjectileGO) extends AbstractOwnedGameObject(pgo) with Savable {

    var speed: Float = _
    var timeSpawned: Long = _

    if(pgo != null) {
      speed = pgo.speed
      timeSpawned = pgo.timeSpawned
    }

    override def read(reader: JmeImporter) {}

    override def write(writer: JmeExporter) {}

    def this() = this(null)
  }

  class ServerWorldStateChange {}

  class ScoreMessage(val offender:PlayerId, val victim:PlayerId) extends ServerWorldStateChange {
    def this() = this(-1,-1)
  }

  class KillPlayer(val playerId: PlayerId) extends ServerWorldStateChange {
    def this() = this(-1)
  }

  class SpawnPlayer(val playerId: PlayerId , val name:String, val teamId: TeamId) extends ServerWorldStateChange {
    override def toString() = name + " with playerid = " + playerId + " representing " + teamId
    def this() = this(-1,"",-1)
  }
  class PlayerGO(pgo: PlayerGO) extends AbstractOwnedGameObject(pgo) with Savable {
    clientSeqId = -1

    def this() = this(null)


    //override def getClassTag = classOf[PlayerGO]
    override def read(reader: JmeImporter) {
      throw new RuntimeException("dont")
    }

    override def write(writer: JmeExporter) {
      throw new RuntimeException("dont")
    }
  }

  abstract class OrderedMessage(var seqId:Int) {
    def this() = this(-1)
  }


  class ServerGameWorld(
    var timeStamp: Long,
    seqId:Int,
    //var deadPlayers:java.util.ArrayList[Int],
    var alivePlayers: java.util.ArrayList[PlayerGO],
    var projectiles: java.util.ArrayList[ProjectileGO],
    var explodedProjectiles: java.util.ArrayList[ProjectileGO],

    var stateChanges: java.util.ArrayList[ServerWorldStateChange]
                         ) extends OrderedMessage(seqId) {

    def this() = this(0,0,null,null,null,null)
    def all = alivePlayers.toList ++ projectiles.toList
  }

  class PlayerJoinRequest(val connectMessage:String) {
    // TODO : What is this used for?
    def this() = this("")
    var clientLabel: String = _
    var teamIdentifier : Int = -1
  }

  class PlayerJoinResponse {
    var playerId: PlayerId = _
  }

  class GameOverRequest(seqId:Int, var serverShutDown:Boolean) extends OrderedMessage(seqId) {
    def this() = this(-1,false)
  }

  class StartGameRequest(seqId:Int) extends OrderedMessage(seqId) {
    def this() = this(-1)
  }

  class RoundOverRequest(seqId:Int) extends OrderedMessage(seqId) {
    def this() = this(-1)
  }

  class StartRoundRequest(seqId:Int) extends OrderedMessage(seqId) {
    def this() = this(-1)
  }

  class MotionGO(
                  var translation: Vector3f,
                  var rotation: Quaternion,
                  var sentToServer:Long
                  ) {
    def this() = this(null, null, -1)
    override def toString = "(" + translation + ", " + rotation + ", " + sentToServer + ")"
  }

  class ProjectileFireGO(
                          var from: OrientationGO,
                          var speed: Float,
                          //var timeStamp: Long,
                          var clientSeqId: Int
                          ) {
    def this() = this(null, 0f, 0)
  }

  class PlayerActionRequest() {
    var text: String = _
    var playerId: PlayerId = _
    var motion: MotionGO = _
    var timeStamp: Long = _
    var elapsed: Long = _
    var projectilesFired: java.util.ArrayList[ProjectileFireGO] = _

    var seqId:Int = _
  }

  class GameWorldResponse() {
    var text: String = _
  }
}