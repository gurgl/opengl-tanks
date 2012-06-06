package se.bupp.lek.server

import com.esotericsoftware.kryonet.{Connection, Listener, Server => KryoServer}
import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.app.SimpleApplication
import com.jme3.system.JmeContext
import collection.JavaConversions
import se.bupp.lek.server.Server._

import JavaConversions.asScalaBuffer
import collection.mutable.{HashMap, ArrayBuffer}
import se.bupp.lek.server.Server._
import com.jme3.scene.Geometry
import scala.None

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */

class WorldSimulator {
  var connectionSequence = 0

  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  var players = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  var projectiles = List[ProjectileGO]()


  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()

  def getGameWorld(): ServerGameWorld = {
    val gameWorld = new ServerGameWorld
    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {
      val playerState = players.map {
        p =>
          p.state
      }

      //println("pos " + playerState.map ( p => p.playerId + " " + p.position).mkString(", "))
      s = playerState.map(x => " | " + x.playerId + " " + x.position + " " + x.direction).mkString(",")


      val simTime: Long = System.currentTimeMillis()
      val maxAgeProjectiles = simTime - 5 * 1000


      val newProjectiles = firedProjectiles.flatMap {
        case (pid, list) =>
          list.map {
            pf =>

              val p = new ProjectileGO()
              p.orientation = pf.from
              p.timeSpawned = pf.timeStamp
              p.playerId = pid
              p.clientSeqId = pf.clientSeqId
              p.speed = pf.speed
              p
          }
      }


      firedProjectiles = HashMap.empty
      projectiles = projectiles ++ newProjectiles

      projectiles = projectiles.filter(_.timeSpawned > maxAgeProjectiles)


      lastWorldSimTimeStamp.foreach { lastSimTime =>
      projectiles.foreach {
        pf =>
        //pf.
          val translate = pf.orientation.direction.getRotationColumn(0).mult(pf.speed * (simTime - lastSimTime).toFloat / 1000f)
          //println("translate " + translate + translate.length)
          pf.orientation.position = pf.orientation.position.add(translate)
      }
      }

      //println("projectiles.size" + projectiles.size + " newProjectiles " + newProjectiles.size)

      /*

      firedProjectiles = firedProjectiles.map { case (k,vl) =>
       val remaining = vl.filter { pf => pf.timeStamp > maxAgeProjectiles }
       (k, remaining)
      }.filter {
        case (k,vl) => vl.size > 0
      }
      */

      gameWorld.players = new java.util.ArrayList[PlayerGO](playerState)
      gameWorld.projectiles = new java.util.ArrayList[ProjectileGO](projectiles)
      gameWorld.timeStamp = simTime
      lastWorldSimTimeStamp = Some(simTime)
    }

    //println(s)
    gameWorld
  }

  def addPlayer(pjr: PlayerJoinRequest): Int = {

    var playerId = -1

    lock.synchronized {

      playerId = connectionSequence
      val player = {
        val pd = new PlayerGO
        pd.playerId = playerId
        pd.position = Vector3f.ZERO.clone()
        pd.direction = Quaternion.DIRECTION_Z.clone()
        pd
        var ps = new PlayerStatus
        ps.state = pd
        ps.lastUpdate = None
        ps
      }
      players += player

      connectionSequence += 1
    }
    playerId
  }

  def addPlayerAction(request: PlayerActionRequest) {
    lock.synchronized {


      players.find(p => p.state.playerId == request.playerId).foreach {
        x => {
          x.lastUpdate = Some(request)
          //x.processedUpdate = false

          x.state.sentToServerByClient = request.timeStamp
          x.state.position = x.state.position.add(request.motion.translation)
          x.state.direction = request.motion.rotation.mult(x.state.direction)


          firedProjectiles.get(request.playerId) match {
            case Some(existing) => firedProjectiles(request.playerId) = existing ++ request.projectilesFired
            case None => firedProjectiles(request.playerId) = request.projectilesFired.toList
          }

          //println("Rec " + request.translation + " " + request.rotation)
          //x.position = x.position.add(request.translation)
          //x.direction = request.rotation.mult(x.direction)

          /*

          val state = p.state
                     if(!p.processedUpdate) {
                       p.lastUpdate.foreach { r =>
                         state.position = state.position.add(r.translation)
                         state.direction = r.rotation.mult(state.direction)
                         p.processedUpdate = true
                       }
                     }

          */

        } //.ensuring(x.position != null && x.direction != null)
      }

    }
  }
}

class Server extends SimpleApplication {


  import Server._

  var updateMaxTimeStamp = System.currentTimeMillis()

  var worldSimulator: WorldSimulator = _

  override def simpleInitApp() {

    worldSimulator = new WorldSimulator()

    val server = new KryoServer();
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

    while (true) {


      try {

        Thread.sleep(1000 / 10)

        val gameWorld = worldSimulator.getGameWorld
        //println("" + players.size)
        server.sendToAllUDP(gameWorld)
      } catch {
        case e: InterruptedException =>
      }
    }
  }
}

object Server {

  class PlayerStatus {
    var state: PlayerGO = _
    var lastUpdate: Option[PlayerActionRequest] = None
    var processedUpdate = true
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

