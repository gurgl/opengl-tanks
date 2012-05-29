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
  var players = new ArrayBuffer[PlayerGO]()

  var lock : AnyRef = new Object()

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
               players.find( p => p.playerId == request.playerId).foreach {
                 x => {
                   println("Rec " + request.translation + " " + request.rotation)
                   x.position = x.position.add(request.translation)
                   x.direction = request.rotation.mult(x.direction)

                 }.ensuring(x.position != null && x.direction != null)
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
        Thread.sleep(1000 / 15)
        val gameWorld = new ServerGameWorld
        import scala.collection.JavaConversions.seqAsJavaList

        lock.synchronized {
          gameWorld.players = new java.util.ArrayList[PlayerGO](players)
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

  class AbstractGameObject {
    var id:Int = _
    var position:Vector3f = _
    var direction:Quaternion = _
  }

  class AbstractOwnedGameObject extends AbstractGameObject {
    var playerId:Int = _

  }


  class ProjectileGO() extends AbstractOwnedGameObject with Savable {

    override def read(reader:JmeImporter) {}
    override def write(writer:JmeExporter) {}
  }
  class PlayerGO() extends AbstractOwnedGameObject with Savable {

    //override def getClassTag = classOf[PlayerGO]
    override def read(reader:JmeImporter) {}
    override def write(writer:JmeExporter) {}
  }

  class ServerGameWorld {
    var players:java.util.ArrayList[PlayerGO] = _
    var projectiles:java.util.ArrayList[ProjectileGO] = _
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

