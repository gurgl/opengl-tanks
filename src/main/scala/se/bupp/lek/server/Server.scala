package se.bupp.lek.server


import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.system.JmeContext
import collection.JavaConversions
import se.bupp.lek.server.Model._

import JavaConversions.asScalaBuffer
import collection.mutable.{HashMap, ArrayBuffer}
import se.bupp.lek.server.Model._
import scala.None
import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.bullet.{PhysicsSpace, PhysicsTickListener, BulletAppState}
import com.jme3.scene.{Node, Geometry}
import com.jme3.light.DirectionalLight
import java.util.logging.{Logger, Level}
import se.bupp.lek.server.Server.PortSettings
import com.jme3.system.JmeContext.Type
import se.bupp.lek.server.Server.GameMatchSettings.{WhenNumOfConnectedPlayersCriteria, AbstractStartCriteria}

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */




class Server(portSettings:PortSettings) extends SimpleApplication with PhysicsTickListener {



  import Model._
  import Server._

  override def prePhysicsTick(p1: PhysicsSpace, p2: Float) {}

  override def physicsTick(p1: PhysicsSpace, p2: Float) {}

  var updateMaxTimeStamp = System.currentTimeMillis()

  var worldSimulator: WorldSimulator = _

  var networkState: ServerNetworkState = _

  var gameLogic:GameLogic = _

  var leRoot:Node = null

  override def simpleInitApp() {


    //val bulletAppState = new BulletAppState();

    //bulletAppState.startPhysics()
    //stateManager.attach(bulletAppState);


    if (getContext.getType != JmeContext.Type.Headless) {
      createDebug()
      leRoot = rootNode
      setPauseOnLostFocus(false)
    } else {
      stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))
      flyCam.setEnabled(false)
      leRoot = new Node()
    }

    val physicsSpace = new PhysicsSpace()
    val serverWorld = new ServerWorld(leRoot, assetManager, physicsSpace)
    serverWorld.initEmpty()
    worldSimulator = new WorldSimulator(serverWorld)


    gameLogic = new GameLogic(new GameMatchSettings(
      startCriteria = WhenNumOfConnectedPlayersCriteria(2)
    )) {

    }

    networkState = new ServerNetworkState(portSettings) {
      def addPlayerAction(pa: PlayerActionRequest) {
          pa.ensuring(pa.motion.translation != null && pa.motion.rotation != null)
          worldSimulator.addPlayerAction(pa)
      }

      override def playerJoined(pjr: PlayerJoinRequest) = {
        val resp = new PlayerJoinResponse
        val playerId = worldSimulator.addPlayer(pjr)
        resp.playerId = playerId
        gameLogic.addPlayer(playerId)
        resp
      }

      def playerLeave(playerId: Int) {
        println("Player disconnected")
        worldSimulator.removePlayer(playerId)
        gameLogic.removePlayer(playerId)
      }
    }



    println("Game Launch Complete")
 }


  def createDebug() {
    //val rot = Quaternion.IDENTITY.clone()
    //rot.lookAt(dir, new Vector3f(0, 1, 0))

    val sun = new DirectionalLight();
    //sun.setColor(ColorRGBA.White)
    val sunDirection: Vector3f = new Vector3f(-1, -1, -1).normalizeLocal()
    sun.setDirection(sunDirection);
    sun.setColor(ColorRGBA.Green)
    rootNode.addLight(sun);
  }


  override def simpleUpdate(tpf: Float) {

    worldSimulator.world.simulateToLastUpdated()

    worldSimulator.handleStateLogic()

    networkState.update(worldSimulator.getGameWorld())

    leRoot.updateLogicalState(tpf);

    leRoot.updateGeometricState();



  }
}

object Server {

  class PortSettings(val tcpPort:Int, val udpPort:Int)

  val getNetworkMessages = List[Class[_ <: AnyRef]](
      classOf[PlayerJoinRequest],
      classOf[PlayerJoinResponse],
      classOf[PlayerActionRequest],
      classOf[GameWorldResponse],
      classOf[Vector3f],
      classOf[Quaternion],
      classOf[PlayerGO],
      classOf[ProjectileGO],
      classOf[MotionGO],
      classOf[OrientationGO],
      classOf[ProjectileFireGO],
      classOf[java.util.ArrayList[PlayerGO]],
      classOf[ServerGameWorld]
    )

  object GameMatchSettings {

    sealed abstract class AbstractStartCriteria()

    case class WhenNumOfConnectedPlayersCriteria(num:Int) extends AbstractStartCriteria()
    case class AlwaysOn() extends AbstractStartCriteria()

  }

  class GameMatchSettings(startCriteria:AbstractStartCriteria) {

  }

  def main(args: Array[String]) {

    val portSettings = args.toList match {
      case tcpPort :: udpPort :: rest => new PortSettings(tcpPort.toInt, udpPort.toInt)
      case _ => new PortSettings(54555, 54777)
    }
    println(portSettings.tcpPort + " " + portSettings.udpPort)
    //Logger.getLogger("com.jme3").setLevel(Level.SEVERE)
    new Server(portSettings).start(JmeContext.Type.Headless)
  }
}

