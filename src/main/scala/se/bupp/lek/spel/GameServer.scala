package se.bupp.lek.spel

import com.esotericsoftware.kryonet.{Connection, Listener, Server}
import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.app.SimpleApplication
import com.jme3.system.JmeContext
import collection.JavaConversions
import se.bupp.lek.spel.GameServer._

import JavaConversions.asScalaBuffer
import collection.mutable.{HashMap, ArrayBuffer}

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */

class WorldSimulator {
  var connectionSequence = 0

  var lock : AnyRef = new Object()

  var lastWorldSimTimeStamp = null
  var players = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()


  class FlyingProjectile {
    //val created:Long = _

    
  }
  var projectiles = List[ProjectileGO]()



  //var projectiles = new ArrayBuffer[ProjectileGO]()

  def getGameWorld() : ServerGameWorld = {
    val gameWorld = new ServerGameWorld
    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {
      val playerState = players.map { p =>
       p.state
      }
      
      s = playerState.map(x => " | " + x.playerId + " " + x.position + " " + x.direction).mkString(",")


      val simTime: Long = System.currentTimeMillis()
      val maxAgeProjectiles = simTime - 5 * 1000
      
      
      val newProjectiles = firedProjectiles.flatMap {
        case (pid, list) => 
        list.map { pf =>

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


      projectiles.foreach { pf =>
        //pf.
        pf.orientation.position = pf.position.add(pf.orientation.direction.getRotationColumn(0).mult(pf.speed * (simTime - pf.timeSpawned).toFloat/1000f))
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
    }
    //println(s)
    gameWorld
  }
  
  def addPlayer(pjr:PlayerJoinRequest) : Int = {

    var playerId = -1

    lock.synchronized {

      playerId = connectionSequence
      val player = {
        val pd = new PlayerGO
        pd.playerId = playerId
        pd.position = Vector3f.ZERO
        pd.direction = Quaternion.DIRECTION_Z
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
  
  def addPlayerAction(request:PlayerActionRequest) {
    lock.synchronized {



      players.find( p => p.state.playerId == request.playerId).foreach {
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

        }//.ensuring(x.position != null && x.direction != null)
      }

    }
  }
}

class GameServer extends SimpleApplication {


  import GameServer._

  var updateMaxTimeStamp = System.currentTimeMillis()

  var worldSimulator:WorldSimulator = _

  override def simpleInitApp() {

    worldSimulator = new WorldSimulator()

    val server = new Server();
    server.start();
    server.bind(54555, 54777);

    val kryo = server.getKryo();
    getNetworkMessages.foreach( kryo.register(_))

    server.addListener(new Listener() {
       override def received(connection:Connection , obj:Object ) {
         //println("rec " + obj.getClass.getName)
          obj match {
            case request:PlayerActionRequest =>
             //println(request.playerId + " " + request.position);
              request.ensuring(request.motion.translation != null && request.motion.rotation != null)
              
               //updateMaxTimeStamp = if(updateMaxTimeStamp < request.timeStamp) request.timeStamp else updateMaxTimeStamp
               worldSimulator.addPlayerAction(request)

              
             /*val response = new GameWorldResponse();
             response.text = "Thanks!";
             connection.sendUDP(response);*/
            case req:PlayerJoinRequest =>
              println("rec " + obj.getClass.getName)
              val resp = new PlayerJoinResponse
              resp.playerId = worldSimulator.addPlayer(req)

              connection.sendTCP(resp)
            case _ =>
          }
       }
    });

    while(true) {



      try {

        Thread.sleep(1000 / 10)

        val gameWorld = worldSimulator.getGameWorld
        //println("" + players.size)
        server.sendToAllUDP(gameWorld)
      } catch {
        case e:InterruptedException => 
      }
    }
  }
}

object GameServer {

  class PlayerStatus {
    var state:PlayerGO = _
    var lastUpdate:Option[PlayerActionRequest] = None
    var processedUpdate = true
  }


  class OrientationGO(
    var position:Vector3f,
    var direction:Quaternion
  )  {
    def this() = this(null, null)
    def orientation = this
    def orientation_=(o:OrientationGO) : Unit = {
      position = o.position.clone()
      direction = o.direction.clone()
    }
  }
  class AbstractGameObject extends OrientationGO {
    var clientSeqId:Int = _
  }

  type OwnedGameObjectId = (Int,Int)

  class AbstractOwnedGameObject extends AbstractGameObject {
    /* Only applic to player really */
    var sentToServerByClient:Long = _
    var playerId:Int = _
    def id:OwnedGameObjectId = (clientSeqId,playerId)
    def id_=(goid:OwnedGameObjectId) = { clientSeqId = goid._1 ; playerId = goid._2 }
  }


  class ProjectileGO() extends AbstractOwnedGameObject with Savable {

    var speed:Float = _
    var timeSpawned:Long = _
    override def read(reader:JmeImporter) {}
    override def write(writer:JmeExporter) {}

    def this(pgo:ProjectileGO) = {
      this()
      sentToServerByClient = pgo.sentToServerByClient
      speed = pgo.speed
      timeSpawned = pgo.timeSpawned
      id = pgo.id
      orientation = pgo
    }
  }
  class PlayerGO() extends AbstractOwnedGameObject with Savable {


    def this(pgo:PlayerGO) = {
      this()
      id = pgo.id
      orientation = pgo
      sentToServerByClient = pgo.sentToServerByClient
    }
    

    //override def getClassTag = classOf[PlayerGO]
    override def read(reader:JmeImporter) { throw new RuntimeException("dont")}
    override def write(writer:JmeExporter) { throw new RuntimeException("dont")}
  }


  class ServerGameWorld {
    var timeStamp:Long = _
    var players:java.util.ArrayList[PlayerGO] = _
    var projectiles:java.util.ArrayList[ProjectileGO] = _
    def all = players.toList ++ projectiles.toList
  }

  class PlayerJoinRequest {
    var clientLabel:String = _
  }

  class PlayerJoinResponse {
    var playerId:Int = _
  }

  class MotionGO(
    var translation:Vector3f,
    var rotation:Quaternion
  ) {
    def this() = this(null,null)
  }

  class ProjectileFireGO(
    var from:OrientationGO,
    var speed:Float,
    var timeStamp:Long,
    var clientSeqId:Int
    ) {
    def this() = this(null,0f,0,0)
  }

  class PlayerActionRequest() {
    var text:String = _
    var playerId:Int = _
    var motion:MotionGO = _
    var timeStamp:Long = _
    var elapsed:Long = _
    var projectilesFired:java.util.ArrayList[ProjectileFireGO] = _

  }
  class GameWorldResponse() {
    var text:String = _
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

  def main(args:Array[String]) {
    new GameServer().start(JmeContext.Type.Headless)
  }
}

