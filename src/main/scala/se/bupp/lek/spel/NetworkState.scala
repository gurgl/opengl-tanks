package se.bupp.lek.spel

import com.esotericsoftware.kryonet.{Connection, Listener, Client}
import management.ManagementFactory
import com.jme3.app.state.{AppStateManager, AbstractAppState}
import com.jme3.app.Application
import com.jme3.math.Vector3f
import collection.immutable.Queue
import collection.immutable.Queue._
import MathUtil._
import com.jme3.app.SimpleApplication._
import com.jme3.scene.Node
import se.bupp.lek.spel.GameServer._


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/29/12
 * Time: 12:32 PM
 * To change this template use File | Settings | File Templates.
 */

class NetworkState extends AbstractAppState {

  var gameClient:Client = _

  val GW_UPDATES_SIZE = 4

    var lastSentUpdate = 0L


  var gameWorldUpdates:Queue[GameServer.ServerGameWorld] = Queue()
  var hasUnProcessedWorldUpdate = false

  var accTranslation = Vector3f.ZERO
  var accRotation = noRotation
  
  var gameApp:Spel = _


  override def update(tpf: Float) {
    if(gameApp.playerIdOpt.isEmpty) return
    if(hasUnProcessedWorldUpdate) {
      gameApp.syncGameWorld(gameWorldUpdates.last)
      hasUnProcessedWorldUpdate = false
    }

    accTranslation = accTranslation.add(gameApp.playerInput._1)
    accRotation = gameApp.playerInput._2.mult(accRotation)

    if(System.currentTimeMillis() - lastSentUpdate > 1000/15 ) {
      val request: PlayerActionRequest = new PlayerActionRequest

      request.playerId = gameApp.playerIdOpt.get
      request.translation = accTranslation
      request.rotation = accRotation
      gameClient.sendUDP(request)
      lastSentUpdate = System.currentTimeMillis()
      accTranslation = Vector3f.ZERO
      accRotation = noRotation
    }
  }


  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Spel]
    initClient()
  }

  def initClient() {
    gameClient = new Client();

    val kryo = gameClient.getKryo();

    GameServer.getNetworkMessages.foreach( kryo.register(_))

    gameClient.addListener(new Listener() {
       override def received (connection:Connection , obj:Object ) {
         obj match {
            case response:ServerGameWorld=>
              if(gameApp.playerIdOpt.isDefined) {
                gameWorldUpdates =
                  Option(gameWorldUpdates).map(
                    x => if(x.size >= GW_UPDATES_SIZE) {x.dequeue._2} else {x}
                  ).head.enqueue(response)
                hasUnProcessedWorldUpdate = true
              } else {
                println("Getting world wo player received")
              }

            case response:PlayerJoinResponse =>
              println("join resp received")
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
