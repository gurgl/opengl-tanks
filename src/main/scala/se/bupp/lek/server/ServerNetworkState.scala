package se.bupp.lek.server

import com.esotericsoftware.kryonet.{Connection, Listener, Server => KryoServer}
import se.bupp.lek.server.Model.{ServerGameWorld, PlayerJoinResponse, PlayerJoinRequest, PlayerActionRequest}
import se.bupp.lek.server.Server.PortSettings
import collection.immutable.HashMap

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 02:04
 * To change this template use File | Settings | File Templates.
 */

abstract class ServerNetworkState(portSettings:PortSettings) {

  val server = new KryoServer();
  server.start();
  server.bind(portSettings.tcpPort, portSettings.udpPort);

  val kryo = server.getKryo();
  Server.getNetworkMessages.foreach(kryo.register(_))

  var connectionIdToPlayerIds = HashMap.empty[Int,Int]

  server.addListener(new Listener() {

    override def disconnected(con: Connection) {
      super.disconnected(con)
      val playerId = connectionIdToPlayerIds(con.getID)
      playerLeave(playerId)
    }

    override def received(connection: Connection, obj: Object) {
      //println("rec " + obj.getClass.getName)
      obj match {
        case request: PlayerActionRequest =>
          addPlayerAction(request)



        case req: PlayerJoinRequest =>
          println("rec " + obj.getClass.getName)

          val resp = playerJoined(req)
          connectionIdToPlayerIds += (connection.getID -> resp.playerId)
          connection.sendTCP(resp)
        case _ =>
      }
    }
  });

  var lastSentUpdate = 0L

  var worldSeqId = 0
  def update(genGameWorld : () => ServerGameWorld) {
    if(System.currentTimeMillis() - lastSentUpdate > 1000/16) {
      worldSeqId = worldSeqId + 1
      val gameWorld = genGameWorld.apply()
      gameWorld.seqId = worldSeqId
      //println("" + getPlayers.size)
      server.sendToAllUDP(gameWorld)
      lastSentUpdate = System.currentTimeMillis()
    }
  }

  def addPlayerAction(pa:PlayerActionRequest)

  def playerJoined(pjr:PlayerJoinRequest) : PlayerJoinResponse

  def playerLeave(playerId:Int)


}
