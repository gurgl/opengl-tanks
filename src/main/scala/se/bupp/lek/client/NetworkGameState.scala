package se.bupp.lek.client

import com.esotericsoftware.kryonet.{Connection, Listener, Client => KryoClient}
import se.bupp.lek.server.{Model, Server}
import se.bupp.lek.server.Model._
import management.ManagementFactory
import com.jme3.app.state.{AbstractAppState, AppStateManager}
import collection.immutable.Queue
import com.jme3.app.Application
import VisualWorldSimulation._

import collection.JavaConversions
import JavaConversions.asScalaBuffer
import scala.Some
import com.jme3.math.Vector3f
import se.bupp.lek.client.MathUtil._
import scala.Some
import org.apache.log4j.Logger


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

trait WorldUpdater {
  def postUpdate(simTime: Long)
  def processInput(input: PlayerInput.Reorientation,lastUpdate:Option[(Long,Reorientation)])
  def generateGameWorld(simTime: Long) : Option[VisualGameWorld]
}
class NetworkGameState(clientConnectSettings:ClientConnectSettings) extends AbstractAppState {

  var gameClient:KryoClient = _

  var gameApp:Client = _

  var buffer = new StringBuffer()

  val GW_UPDATES_SIZE = 4

  val log = Logger.getLogger(classOf[NetworkGameState])

  var gameWorldUpdatesQueue:Queue[Model.ServerGameWorld] = Queue()

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
              log.warn("Getting world wo player received.")
            }
          case response:RoundOverRequest =>
            gameApp.postMessage(response)
          case response:StartRoundRequest =>
            gameApp.postMessage(response)
          case response:GameOverRequest =>
            gameApp.postMessage(response)
            gameWorldUpdatesQueue = Queue()
          case response:StartGameRequest =>
            gameApp.postMessage(response)

          case response:PlayerJoinResponse =>
            log.info("join resp received " + response.playerId)
            gameApp.playerIdOpt = Some(response.playerId)

          case _ =>
        }
      }
    });

    gameClient.start();
    log.info("tcpPort " + clientConnectSettings.tcpPort + ",  updPort " + clientConnectSettings.udpPort)
    gameClient.connect(5000, clientConnectSettings.host, clientConnectSettings.tcpPort, clientConnectSettings.udpPort);

    val playerJoinRequest = new PlayerJoinRequest()
    playerJoinRequest.clientLabel = ManagementFactory.getRuntimeMXBean().getName()
    gameClient.sendTCP(playerJoinRequest);
  }

  def getPlayState = Option(gameApp.getStateManager.getState(classOf[PlayState]))

  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Client]
    initClient()
  }


  def sendClientUpdate(simTime: Long, visualWorldSimulation:VisualWorldSimulation) {
    val reorientation = MessageQueue.flushAccumulated()
    val projectiles = visualWorldSimulation.flushFired()
    val request = gameApp.createPlayerActionRequest(simTime, reorientation, projectiles)
    gameClient.sendUDP(request)

  }

  override def cleanup() {
    log.debug(buffer.toString)
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

    getPlayState.foreach {
      playState =>
        if(playState.isInitialized) {
          //println("ITS INITIALIZED")
          val toKill = serverUpdate.deadPlayers.toList
          if (toKill.size > 0) {
            log.info("handle deaths")
            playState.visualWorldSimulation.handleKilledPlayers(toKill)
          }

          if (playState.visualWorldSimulation.playerDead) {
            serverUpdate.alivePlayers.find( p => p.playerId == gameApp.playerIdOpt.get).foreach {
              p =>
                log.info("You respawned")
                playState.visualWorldSimulation.playerDead = false
                playState.visualWorldSimulation.rootNode.attachChild(playState.visualWorldSimulation.player)
            }
          }
        }
    }
  }

  class ServerWorldUpdater(visualWorldSimulation:VisualWorldSimulation) extends WorldUpdater {

    var lastSentUpdate = 0L

    def postUpdate(simTime: Long) {

      if ((System.currentTimeMillis() - lastSentUpdate).toFloat > 1000f / 15f) {
          sendClientUpdate(simTime,visualWorldSimulation)

          lastSentUpdate = System.currentTimeMillis()

      }
    }

    def processInput(input: PlayerInput.Reorientation,lastUpdate:Option[(Long,Reorientation)]) {
      lastUpdate.foreach {
        case (lastSimTime, lastInput) =>
          visualWorldSimulation.storePlayerLastInputAndOutput(lastSimTime, lastInput)
      }

      MessageQueue.accumulate(input)
    }

    var currentGameWorldUpdates:Queue[ServerGameWorld] = null
    def generateGameWorld(simTime: Long) : Option[VisualGameWorld] = {
      currentGameWorldUpdates = Queue(gameWorldUpdatesQueue: _*)

      visualWorldSimulation.generateLocalGameWorld(simTime, currentGameWorldUpdates)
    }
  }
}
