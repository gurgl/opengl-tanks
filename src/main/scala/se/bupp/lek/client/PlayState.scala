package se.bupp.lek.client


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
import JavaConversions.asScalaBuffer
import scala.Option
import se.bupp.lek.server.Model._
import se.bupp.lek.server.{Server, Model}
import com.jme3.bullet.{PhysicsSpace, PhysicsTickListener}
import java.util


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/29/12
 * Time: 12:32 PM
 * To change this template use File | Settings | File Templates.
 */

class PlayState(clientConnectSettings:ClientConnectSettings) extends AbstractAppState with PhysicsTickListener {






  //var predictions:Map[Long,Map[OwnedGameObjectId,Vector3f]] = Map[Long,Map[OwnedGameObjectId,Vector3f]]()


  var gameApp:Client = _

  var networkStateOpt:Option[NetworkComponent] = None

  var lastUpdate:Option[(Long,Reorientation)] = None

  override def update(tpf: Float) {

    if(gameApp.playerIdOpt.isEmpty) return

    if(!isEnabled) return
    val simTime = System.currentTimeMillis()


    var input = gameApp.playerInput.pollInput()

    networkStateOpt.foreach(_.handleInputForNetworking(input,lastUpdate))

    val worldToPaintOpt = networkStateOpt match {
      case Some(networkState) => networkState.generateNetworkModeGameWorld(simTime)
      case None => throw new IllegalStateException("Not implemented")
    }

    worldToPaintOpt.foreach( gameApp.visualWorldSimulation.updateGameWorld(_ , input) )

    networkStateOpt.foreach(networkState => lastUpdate.foreach {
        case (lastSimTime, _) =>
          networkState.handleNeworkGameClientToServerUpdate(simTime)
    })

    lastUpdate = Some((simTime, input))

  }


  /*def generateLocalGameWorld(simTime: Long,currentGameWorldUpdates:Queue[Model.ServerGameWorld]): (Set[Model.AbstractOwnedGameObject with Savable] , Model.ServerGameWorld) = {
    val prediction: Set[AbstractOwnedGameObject with Savable] = gameApp.visualWorldSimulation.calculatePrediction(simTime, currentGameWorldUpdates, gameApp.playerIdOpt.get)

    (prediction, currentGameWorldUpdates.last)
  }*/



  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Client]

    val state: NetworkComponent = gameApp.getStateManager.getState(classOf[NetworkComponent])
    networkStateOpt = Option.apply(state)
  }

  val lock : AnyRef = new Object()




  def prePhysicsTick(p1: PhysicsSpace, tpf: Float) {

  }

  def physicsTick(p1: PhysicsSpace, p2: Float) {

  }
}
