package se.bupp.lek.server

import com.esotericsoftware.kryonet.{Connection, Listener, Server => KryoServer}
import se.bupp.lek.common.Model._
import se.bupp.lek.server.Server.PortSettings
import collection.immutable.HashMap
import org.slf4j.LoggerFactory
import se.bupp.lek.common.FuncUtil.RateProbe
import java.util.{TimerTask, Timer}
import com.esotericsoftware.kryonet.Listener.LagListener
import se.bupp.lek.common.{Model, Tmp}
import se.bupp.lek.common.model.Model._

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
  Model.getNetworkMessages.foreach(kryo.register(_))

  var connectionIdToPlayerIds = HashMap.empty[Int,PlayerId]

  val log = LoggerFactory.getLogger(classOf[ServerNetworkState])

  val actionReqProbe = new RateProbe("ActionReq",1000L, log)
  val serverSentProbe = new RateProbe("serverSentProbe",1000L, log)

  var orderedChannelBuffer:Seq[(TimeStamp,_ <: OrderedMessage)] = Nil


  /*

  val disconnected = (con: Connection) => {
    //super.disconnected(con)
    val playerId = connectionIdToPlayerIds(con.getID)
    playerLeave(playerId)
  }

  val received = (connection: Connection, obj: Object) => {
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
  }*/

  val listener = new Listener() {

    override def disconnected(con: Connection) {
      super.disconnected(con)
      val playerId = connectionIdToPlayerIds.apply(con.getID)
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

          new Thread(new Runnable() {
            def run() {

              log.info("req.connectMessage " + req.connectMessage)
              var masterPlayerId = Option.empty[Long]
              var masterTeamId = Option.empty[Long]
              var mess: String = new java.lang.String(req.connectMessage.getBytes())
              val playerInfoOpt = Server.settings.masterServer.gameSessionIdOpt.map { occ =>
                val absPlayerInfo = Server.gameServerFacade.evaluateGamePass(mess,occ)
                if (absPlayerInfo != null) {
                  log.info("" +absPlayerInfo.getName)

                  //absPlayerInfo.getName
                  Some(absPlayerInfo)
                } else {
                  log.info("No master server available : " + req.connectMessage)
                  //"n/a"
                  None
                }

                absPlayerInfo
              }



              val resp = playerJoined(req, playerInfoOpt)
              connectionIdToPlayerIds = connectionIdToPlayerIds + (connection.getID -> resp.playerId)
              connection.sendTCP(resp)
            }
          }).start()

        case _ =>
      }
    }
  }
  server.addListener(Tmp.decorateListener(listener))

  var lastSentUpdate = 0L

  var worldSeqId = 0
  def querySendUpdate(genGameWorld : () => ServerGameWorld) = {
    if(Server.clock() - lastSentUpdate > 1000/16) {
      worldSeqId = worldSeqId + 1
      val gameWorld = genGameWorld.apply()
      gameWorld.seqId = worldSeqId
      //println("" + getPlayers.size)

       server.sendToAllUDP(gameWorld)

      lastSentUpdate = Server.clock()
      //serverSentProbe.tick()
      true
    } else false

  }

  def addPlayerAction(pa:PlayerActionRequest)

  def playerLeave(playerId:Int)

  def playerJoined(pjr:PlayerJoinRequest, masterPlayerInfoOpt:Option[PlayerInfoServerLobby]) : PlayerJoinResponse


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
      orderedChannelBuffer = orderedChannelBuffer :+ (Server.clock(), me)
    }
    */
    server.sendToAllUDP(me)
  }

  /*def run() {
    while(true) {
      try {
        Thread.currentThread().sleep(10)
        var time: Long = Server.clock()
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
