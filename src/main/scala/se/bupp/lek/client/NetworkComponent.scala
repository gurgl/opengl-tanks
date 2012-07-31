package se.bupp.lek.client

import com.esotericsoftware.kryonet.{Connection, Listener, Client => KryoClient}
import se.bupp.lek.server.{Model, Server}
import se.bupp.lek.server.Model._
import management.ManagementFactory
import com.jme3.app.state.{AbstractAppState, AppStateManager}
import collection.immutable.Queue
import com.jme3.app.Application

import collection.JavaConversions
import JavaConversions.asScalaBuffer
import scala.Some
import com.jme3.math.Vector3f
import se.bupp.lek.client.MathUtil._
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-31
 * Time: 01:17
 * To change this template use File | Settings | File Templates.
 */

object MessageQueue {

  var accTranslation = Vector3f.ZERO.clone()
  var accRotation = noRotation

  def accumulate(reorientation:Reorientation) {
    accTranslation = accTranslation.add(reorientation._1)
    accRotation = reorientation._2.mult(accRotation)
  }

  def flushAccumulated() : Reorientation = {
    val r = (accTranslation,accRotation)
    accTranslation = Vector3f.ZERO.clone()
    accRotation = noRotation
    r
  }


}

class ClientConnectSettings(val host:String, val tcpPort: Int, val udpPort: Int)


class NetworkComponent(clientConnectSettings:ClientConnectSettings) extends AbstractAppState {

  var gameClient:KryoClient = _

  var gameApp:Client = _

  var buffer = new StringBuffer()

  val GW_UPDATES_SIZE = 4

  var gameWorldUpdatesQueue:Queue[Model.ServerGameWorld] = Queue()

  var lastSentUpdate = 0L

  def initClient() {
    gameClient = new KryoClient();

    val kryo = gameClient.getKryo();

    Server.getNetworkMessages.foreach( kryo.register(_))

    gameClient.addListener(new Listener() {
      override def received (connection:Connection , obj:Object ) {
        obj match {
          case response:ServerGameWorld=>
            if(gameApp.playerIdOpt.isDefined) {
              //                lock.synchronized {
              handleWorldUpdate(response)
              //              }
            } else {
              println("Getting world wo player received.")
            }

          case response:PlayerJoinResponse =>
            println("join resp received " + response.playerId)
            gameApp.playerIdOpt = Some(response.playerId)

          case _ =>
        }
      }
    });

    gameClient.start();
    println("tcpPort " + clientConnectSettings.tcpPort + ",  updPort " + clientConnectSettings.udpPort)
    gameClient.connect(5000, clientConnectSettings.host, clientConnectSettings.tcpPort, clientConnectSettings.udpPort);

    val playerJoinRequest = new PlayerJoinRequest()
    playerJoinRequest.clientLabel = ManagementFactory.getRuntimeMXBean().getName()
    gameClient.sendTCP(playerJoinRequest);
  }

  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Client]
    initClient()
  }


  def sendClientUpdate(simTime: Long) {
    val reorientation = MessageQueue.flushAccumulated()
    val projectiles = gameApp.visualWorldSimulation.flushFired()
    val request = gameApp.createPlayerActionRequest(simTime, reorientation, projectiles)
    gameClient.sendUDP(request)

  }

  override def cleanup() {
    println(buffer.toString)
    gameClient.close();
  }

  def handleWorldUpdate(serverUpdate: Model.ServerGameWorld) {
    gameWorldUpdatesQueue =
      Option(gameWorldUpdatesQueue).map(
        x => if (x.size >= GW_UPDATES_SIZE) {
          x.dequeue._2
        } else {
          x
        }
      ).head.enqueue(serverUpdate);

    val toKill = serverUpdate.deadPlayers.toList
    if (toKill.size > 0) {
      println("handle deaths")
      gameApp.visualWorldSimulation.handleKilledPlayers(toKill)
    }

    if (gameApp.visualWorldSimulation.playerDead) {
      serverUpdate.alivePlayers.find( p => p.playerId == gameApp.playerIdOpt.get).foreach {
        p =>
          println("You respawned")
          gameApp.visualWorldSimulation.playerDead = false
          gameApp.visualWorldSimulation.rootNode.attachChild(gameApp.visualWorldSimulation.player)
      }
    }
  }


  def handleNeworkGameClientToServerUpdate(simTime: Long) {
    if ((System.currentTimeMillis() - lastSentUpdate).toFloat > 1000f / 15f) {

      sendClientUpdate(simTime)

      lastSentUpdate = System.currentTimeMillis()
    }
  }

  def handleInputForNetworking(input: PlayerInput.Reorientation,lastUpdate:Option[(Long,Reorientation)]) {
    lastUpdate.foreach {
      case (lastSimTime, lastInput) =>
        gameApp.visualWorldSimulation.storePlayerLastInputAndOutput(lastSimTime, lastInput)
    }

    MessageQueue.accumulate(input)
  }

  var currentGameWorldUpdates:Queue[ServerGameWorld] = null
  def generateNetworkModeGameWorld(simTime: Long) = {
    currentGameWorldUpdates = Queue(gameWorldUpdatesQueue: _*)

    gameApp.visualWorldSimulation.generateLocalGameWorld(simTime, currentGameWorldUpdates)
  }

}
