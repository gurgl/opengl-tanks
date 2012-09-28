package se.bupp.lek.server

import com.esotericsoftware.kryonet.{Connection, Listener, Server => KryoServer}
import se.bupp.lek.server.Model._
import se.bupp.lek.server.Server.PortSettings
import collection.immutable.HashMap
import org.apache.log4j.Logger
import se.bupp.lek.common.FuncUtil.RateProbe

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-19
 * Time: 02:04
 * To change this template use File | Settings | File Templates.
 */



abstract class ServerNetworkState(portSettings:PortSettings) {

  type TimeStamp = Long

  val server = new KryoServer();
  server.start();
  server.bind(portSettings.tcpPort, portSettings.udpPort);

  val kryo = server.getKryo();
  Server.getNetworkMessages.foreach(kryo.register(_))

  var connectionIdToPlayerIds = HashMap.empty[Int,Int]

  val log = Logger.getLogger(classOf[ServerNetworkState])

  val actionReqProbe = new RateProbe("ActionReq",1000L, log)
  val serverSentProbe = new RateProbe("serverSentProbe",1000L, log)

  var orderedChannelBuffer:Seq[(TimeStamp,_ <: OrderedMessage)] = Nil

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

          //actionReqProbe.tick()
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
  def querySendUpdate(genGameWorld : () => ServerGameWorld) {
    if(System.currentTimeMillis() - lastSentUpdate > 1000/16) {
      worldSeqId = worldSeqId + 1
      val gameWorld = genGameWorld.apply()
      gameWorld.seqId = worldSeqId
      //println("" + getPlayers.size)
      server.sendToAllUDP(gameWorld)
      lastSentUpdate = System.currentTimeMillis()
      //serverSentProbe.tick()
    }
  }

  def addPlayerAction(pa:PlayerActionRequest)

  def playerLeave(playerId:Int)

  def playerJoined(pjr:PlayerJoinRequest) : PlayerJoinResponse


  def sendRoundOver() {
    worldSeqId = worldSeqId + 1
    val me = new RoundOverRequest(worldSeqId)
    server.sendToAllUDP(me)
  }


  def createSimple(m:OrderedMessage) = {
    worldSeqId = worldSeqId + 1
    val me = m match {
      case go:GameOverRequest => new GameOverRequest(worldSeqId)
      case go:StartGameRequest => new StartGameRequest(worldSeqId)
      case go:StartRoundRequest => new StartRoundRequest(worldSeqId)
    }
    /*
    orderedChannelBuffer.synchronized {
      orderedChannelBuffer = orderedChannelBuffer :+ (System.currentTimeMillis(), me)
    }
    */
    server.sendToAllUDP(me)
  }

  /*def run() {
    while(true) {
      try {
        Thread.currentThread().sleep(10)
        var time: Long = System.currentTimeMillis()
        orderedChannelBuffer.synchronized {
          orderedChannelBuffer.foreach {
            case (lastSent,me) if lastSent - time > 100 =>
          }
        }
      } catch {
        case ie:InterruptedException =>
        case e => e.printStackTrace()
      }
    }

  }*/

}
