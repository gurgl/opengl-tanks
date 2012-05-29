package se.bupp.lek.spel

import com.esotericsoftware.kryonet.{Connection, Listener, Client}
import management.ManagementFactory
import se.bupp.lek.spel.GameServer._


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 9:05 AM
 * To change this template use File | Settings | File Templates.
 */

object TestClient {
  var playerIdOpt:Option[Int] = None
  
  def main(arg:Array[String]) {
    val gameClient = new Client();


    println("Tjo")
      val kryo = gameClient.getKryo();

      GameServer.getNetworkMessages.foreach( kryo.register(_))

      gameClient.addListener(new Listener() {
         override def received (connection:Connection , obj:Object ) {
           println("rec " + obj.getClass.getName)
           obj match {
              case response:ServerGameWorld=>
                //syncGameWorld(response)


              case response:PlayerJoinResponse =>
                println("join resp " + response.playerId)
                playerIdOpt = Some(response.playerId)
              case _ =>
            }
         }
      });
    gameClient.start();
    gameClient.connect(5000, "localhost", 54555, 54777);
      val playerJoinRequest = new PlayerJoinRequest()
      playerJoinRequest.clientLabel = ManagementFactory.getRuntimeMXBean().getName()
      println("sending joing")
      gameClient.sendTCP(playerJoinRequest);

      val request = new PlayerActionRequest()
      request.text = "Here is the request!"
      gameClient.sendTCP(request);

    while(true) {
      try {
        Thread.sleep(1000 / 15)
        if(playerIdOpt.isDefined) {

          val gameWorld = new ServerGameWorld
          import scala.collection.JavaConversions.seqAsJavaList
          //gameWorld.players = new java.util.ArrayList[PlayerDetails](players)
          println("sendin update")
          val p = new PlayerActionRequest()

          p.playerId = playerIdOpt.get

          gameClient.sendUDP(p)
        }
      } catch {
        case e:InterruptedException =>
      }
    }

  }

}
