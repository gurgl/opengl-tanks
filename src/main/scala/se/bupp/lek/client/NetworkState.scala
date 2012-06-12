package se.bupp.lek.client

import com.esotericsoftware.kryonet.{Connection, Listener, Client => KryoClient}
import management.ManagementFactory
import com.jme3.app.state.{AppStateManager, AbstractAppState}
import com.jme3.app.Application
import collection.immutable.Queue
import collection.immutable.Queue._
import MathUtil._
import com.jme3.app.SimpleApplication._
import com.jme3.scene.Node
import com.jme3.export.Savable
import com.jme3.math.{Quaternion, Vector3f}
import collection.{JavaConversions, immutable}
import scala.Option
import se.bupp.lek.server.Server
import se.bupp.lek.server.Server._


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/29/12
 * Time: 12:32 PM
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

class NetworkState extends AbstractAppState {

  var gameClient:KryoClient = _

  val GW_UPDATES_SIZE = 4

  var lastSentUpdate = 0L


  var gameWorldUpdatesQueue:Queue[Server.ServerGameWorld] = Queue()


  //var predictions:Map[Long,Map[OwnedGameObjectId,Vector3f]] = Map[Long,Map[OwnedGameObjectId,Vector3f]]()


  var gameApp:Client = _


  var buffer = new StringBuffer()
  
  override def cleanup() {
    println(buffer.toString)
  }



  var lastUpdate:Option[(Long,Reorientation)] = None

  override def update(tpf: Float) {
    if(gameApp.playerIdOpt.isEmpty) return

    val simTime = System.currentTimeMillis()

    val input = gameApp.playerInput.pollInput()

    lastUpdate.foreach { case (lastSimTime, lastInput) =>
      gameApp.visualWorldSimulation.storePlayerLastInputAndOutput(lastSimTime, lastInput)
    }
    //gameApp.visualWorldSimulation.saveReorientation(simTime, input)
    MessageQueue.accumulate(input)

    //saveReorientation(timeStamp, lastInput)

    var currentGameWorldUpdates:Queue[ServerGameWorld] = null
    //    }

//    lock.synchronized {
    currentGameWorldUpdates = gameWorldUpdatesQueue


    if(currentGameWorldUpdates.size > 0) {
      gameApp.visualWorldSimulation.update(simTime,currentGameWorldUpdates, gameApp.playerIdOpt.get, input)

    }


    if(System.currentTimeMillis() - lastSentUpdate > 1000/15 ) {

      val reorientation = MessageQueue.flushAccumulated()
      val projectiles = gameApp.visualWorldSimulation.flushFired()
      val request = gameApp.createPlayerActionRequest(simTime, reorientation, projectiles)
      gameClient.sendUDP(request)
      lastSentUpdate = System.currentTimeMillis()
    }

    lastUpdate = Some((simTime, input))

  }

  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Client]
    initClient()
  }

  val lock : AnyRef = new Object()
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
                  gameWorldUpdatesQueue =
                    Option(gameWorldUpdatesQueue).map(
                      x => if(x.size >= GW_UPDATES_SIZE) {x.dequeue._2} else {x}
                    ).head.enqueue(response)
  //              }
              } else {
                println("Getting world wo player received")
              }

            case response:PlayerJoinResponse =>
              println("join resp received" + response.playerId)
              gameApp.playerIdOpt = Some(response.playerId)

            case _ =>
          }
       }
    });

    gameClient.start();
    gameClient.connect(5000, "localhost", 54555, 54777);

    val playerJoinRequest = new PlayerJoinRequest()
    playerJoinRequest.clientLabel = ManagementFactory.getRuntimeMXBean().getName()
    gameClient.sendTCP(playerJoinRequest);
  }
}
