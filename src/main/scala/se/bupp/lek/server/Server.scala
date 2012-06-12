package se.bupp.lek.server

import com.esotericsoftware.kryonet.{Connection, Listener, Server => KryoServer}
import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.system.JmeContext
import collection.JavaConversions
import se.bupp.lek.server.Server._

import JavaConversions.asScalaBuffer
import collection.mutable.{HashMap, ArrayBuffer}
import se.bupp.lek.server.Server._
import scala.None
import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.bullet.{PhysicsSpace, PhysicsTickListener, BulletAppState}
import com.jme3.scene.{Node, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */


class Server extends SimpleApplication with PhysicsTickListener {



  import Server._

  override def prePhysicsTick(p1: PhysicsSpace, p2: Float) {}

  override def physicsTick(p1: PhysicsSpace, p2: Float) {}

  var updateMaxTimeStamp = System.currentTimeMillis()

  var worldSimulator: WorldSimulator = _

  var server:KryoServer = _

  override def simpleInitApp() {

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))
    val bulletAppState = new BulletAppState();

    stateManager.attach(bulletAppState);
    flyCam.setEnabled(false)

    val leRoot = new Node()
    val serverWorld = new ServerWorld(leRoot,assetManager,bulletAppState)
    serverWorld.initEmpty()
    worldSimulator = new WorldSimulator(serverWorld)

    server = new KryoServer();
    server.start();
    server.bind(54555, 54777);

    val kryo = server.getKryo();
    getNetworkMessages.foreach(kryo.register(_))

    server.addListener(new Listener() {
      override def received(connection: Connection, obj: Object) {
        //println("rec " + obj.getClass.getName)
        obj match {
          case request: PlayerActionRequest =>
            //println(request.playerId + " " + request.position);
            request.ensuring(request.motion.translation != null && request.motion.rotation != null)

            //updateMaxTimeStamp = if(updateMaxTimeStamp < request.timeStamp) request.timeStamp else updateMaxTimeStamp
            worldSimulator.addPlayerAction(request)


          /*val response = new GameWorldResponse();
          response.text = "Thanks!";
          connection.sendUDP(response);*/
          case req: PlayerJoinRequest =>
            println("rec " + obj.getClass.getName)
            val resp = new PlayerJoinResponse
            resp.playerId = worldSimulator.addPlayer(req)

            connection.sendTCP(resp)
          case _ =>
        }
      }
    });

  }


  var lastSentUpdate = 0L
  override def simpleUpdate(tpf: Float) {

    if(System.currentTimeMillis() - lastSentUpdate > 1000/16) {


        val gameWorld = worldSimulator.getGameWorld
        //println("" + players.size)
        server.sendToAllUDP(gameWorld)
        lastSentUpdate = System.currentTimeMillis()

    }
  }
}

object Server {

  class PlayerStatus extends Savable {
    var state: PlayerGO = _

    var seqId : Int = _
    var reorientation:Seq[MotionGO] = Seq.empty
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


  class ServerGameWorld {
    var timeStamp: Long = _
    var players: java.util.ArrayList[PlayerGO] = _
    var projectiles: java.util.ArrayList[ProjectileGO] = _

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
                  var rotation: Quaternion
                  ) {
    def this() = this(null, null)
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
    classOf[ServerGameWorld]
  )

  def main(args: Array[String]) {
    new Server().start(JmeContext.Type.Headless)
  }
}

