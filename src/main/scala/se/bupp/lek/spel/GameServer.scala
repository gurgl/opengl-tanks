package se.bupp.lek.spel

import com.esotericsoftware.kryonet.{Connection, Listener, Server}
import collection.mutable.ArrayBuffer
import scala.Predef._
import management.ManagementFactory
import se.bupp.lek.spel.GameServer.{ServerGameWorld, PlayerGO, PlayerJoinResponse}
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.app.SimpleApplication
import com.jme3.system.JmeContext
import collection.JavaConversions


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */

class GameServer extends SimpleApplication {

  import GameServer._
  var connectionSequence = 0
  var players = new ArrayBuffer[PlayerStatus]()


  var lock : AnyRef = new Object()

  var updateMaxTimeStamp = System.currentTimeMillis()
  
  override def simpleInitApp() {

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
              request.ensuring(request.translation != null && request.rotation != null)
              
              lock.synchronized {
               updateMaxTimeStamp = if(updateMaxTimeStamp < request.timeStamp) request.timeStamp else updateMaxTimeStamp
               players.find( p => p.state.playerId == request.playerId).foreach {
                 x => {
                   x.lastUpdate = Some(request)
                   //x.processedUpdate = false
                   
                   x.state.position = x.state.position.add(request.translation)
                   x.state.direction = request.rotation.mult(x.state.direction)
                   
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
              
             /*val response = new GameWorldResponse();
             response.text = "Thanks!";
             connection.sendUDP(response);*/
            case req:PlayerJoinRequest =>
              println("rec " + obj.getClass.getName)
              val resp = new PlayerJoinResponse
              lock.synchronized {
                resp.playerId = connectionSequence

                players += {
                  val pd = new PlayerGO
                  pd.playerId = connectionSequence
                  pd.position = Vector3f.ZERO
                  pd.direction = Quaternion.DIRECTION_Z
                  pd
                  var ps = new PlayerStatus
                  ps.state = pd
                  ps.lastUpdate = None
                  ps
                }

                connectionSequence += 1
              }
              connection.sendTCP(resp)
            case _ =>
          }
       }
    });

    while(true) {



      try {

        Thread.sleep(1000 / 8)
        val gameWorld = new ServerGameWorld
        import scala.collection.JavaConversions.seqAsJavaList

        lock.synchronized {
          val playerState = players.map { p =>


            p.state
          }

          gameWorld.players = new java.util.ArrayList[PlayerGO](playerState)
          gameWorld.projectiles = new java.util.ArrayList[ProjectileGO]()
          gameWorld.timeStamp = System.currentTimeMillis()
        }
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

  class PlayerAction {

  }
  class ObjectOrientation {
    var position:Vector3f = _
    var direction:Quaternion = _

    def orientation = this
    def orientation_=(o:ObjectOrientation) : Unit = {
      position = o.position
      direction = o.direction
    }
  }
  class AbstractGameObject extends ObjectOrientation {
    var clientId:Int = _
  }

  type OwnedGameObjectId = (Int,Int)

  class AbstractOwnedGameObject extends AbstractGameObject {
    var playerId:Int = _
    def id:OwnedGameObjectId = (clientId,playerId)
    def id_=(goid:OwnedGameObjectId) = { clientId = goid._1 ; playerId = goid._2 }


  }


  class ProjectileGO() extends AbstractOwnedGameObject with Savable {

    override def read(reader:JmeImporter) {}
    override def write(writer:JmeExporter) {}

    def this(pgo:ProjectileGO) = {
      this()
      id = pgo.id
      orientation = pgo
    }
  }
  class PlayerGO() extends AbstractOwnedGameObject with Savable {

    def this(pgo:PlayerGO) = {
      this()
      id = pgo.id
      orientation = pgo
    }

    //override def getClassTag = classOf[PlayerGO]
    override def read(reader:JmeImporter) {}
    override def write(writer:JmeExporter) {}
  }

  import JavaConversions.asScalaBuffer
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

  class PlayerActionRequest() {
    var text:String = _
    var playerId:Int = _
    var translation:Vector3f = _
    var rotation:Quaternion = _
    var timeStamp:Long = _
    var elapsed:Long = _

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
    classOf[java.util.ArrayList[PlayerGO]],
    classOf[ServerGameWorld]
  )

  def main(args:Array[String]) {
    new GameServer().start(JmeContext.Type.Headless)
  }
}

