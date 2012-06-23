package se.bupp.lek.server

import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.math.{Quaternion, Vector3f}
import scala.collection.JavaConversions.asScalaBuffer

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 6/13/12
 * Time: 12:01 AM
 * To change this template use File | Settings | File Templates.
 */


object Model {

  class PlayerStatus extends Savable {
    var state: PlayerGO = _

    var seqId : Int = _
    var reorientation:Seq[MotionGO] = Seq.empty

    var lastSimulation:Long = _
    //var lastUpdate: Option[PlayerActionRequest] = None
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

  type OwnedGameObjectId = (Int, Int)

  class AbstractOwnedGameObject(aogo:AbstractOwnedGameObject) extends AbstractGameObject(aogo) {
    /* Only applic to player really */
    var sentToServerByClient: Long = -1
    var playerId: Int = -1

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

  class PlayerGO(pgo: PlayerGO) extends AbstractOwnedGameObject(pgo) with Savable {


    def this() = this(null)


    //override def getClassTag = classOf[PlayerGO]
    override def read(reader: JmeImporter) {
      throw new RuntimeException("dont")
    }

    override def write(writer: JmeExporter) {
      throw new RuntimeException("dont")
    }
  }


  class ServerGameWorld(
    var timeStamp: Long,
    var players: java.util.ArrayList[PlayerGO],
    var projectiles: java.util.ArrayList[ProjectileGO],
    var explodedProjectiles: java.util.ArrayList[ProjectileGO]
                         ) {

    def this() = this(0,null,null,null)
    def all = players.toList ++ projectiles.toList
  }

  class PlayerJoinRequest {
    var clientLabel: String = _
  }

  class PlayerJoinResponse {
    var playerId: Int = _
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
                          var timeStamp: Long,
                          var clientSeqId: Int
                          ) {
    def this() = this(null, 0f, 0, 0)
  }

  class PlayerActionRequest() {
    var text: String = _
    var playerId: Int = _
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