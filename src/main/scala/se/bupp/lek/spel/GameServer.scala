package se.bupp.lek.spel

import com.esotericsoftware.kryonet.{Connection, Listener, Server}
import collection.mutable.ArrayBuffer
import scala.Predef._
import management.ManagementFactory
import se.bupp.lek.spel.GameServer.{ServerGameWorld, PlayerDetails, PlayerJoinResponse}
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */

object GameServer {

  var connectionSequence = 0
  var players = new ArrayBuffer[PlayerDetails]()

  val getNetworkMessages = List[Class[_ <: AnyRef]](
    classOf[PlayerJoinRequest],
    classOf[PlayerJoinResponse],
    classOf[PlayerActionRequest],
    classOf[GameWorldResponse],
    classOf[Vector3f],
    classOf[Quaternion],
    classOf[PlayerDetails],
    classOf[java.util.ArrayList[PlayerDetails]],
    classOf[ServerGameWorld]
  )

  class PlayerDetails() extends Savable {
    var playerId:Int = _
    var position:Vector3f = _
    var direction:Quaternion = _

    //override def getClassTag = classOf[PlayerDetails]
    override def read(reader:JmeImporter) {}
    override def write(writer:JmeExporter) {}
  }

  class ServerGameWorld {
    var players:java.util.ArrayList[PlayerDetails] = _
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
    var position:Vector3f = _
    var direction:Quaternion = _

  }
  class GameWorldResponse() {
    var text:String = _
  }

  var lock : AnyRef = new Object()

  def main(args:Array[String]) {

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
              request.ensuring(request.position != null && request.direction != null)
              lock.synchronized {
               players.find( p => p.playerId == request.playerId).foreach {
                 x => {
                   x.position = request.position
                   x.direction = request.direction

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
                  val pd = new PlayerDetails
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
          gameWorld.players = new java.util.ArrayList[PlayerDetails](players)
        }
        //println("" + players.size)
        server.sendToAllUDP(gameWorld)
      } catch {
        case e:InterruptedException => 
      }
    }
  }
}

