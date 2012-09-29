package se.bupp.lek.server


import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.system.JmeContext
import collection.{mutable, JavaConversions}
import se.bupp.lek.server.Model._

import JavaConversions.asScalaBuffer
import collection.mutable.{HashMap, ArrayBuffer}
import scala.None
import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.bullet.{PhysicsSpace, PhysicsTickListener, BulletAppState}
import com.jme3.scene.{Node, Geometry}
import com.jme3.light.DirectionalLight
import se.bupp.lek.server.Server.PortSettings
import com.jme3.system.JmeContext.Type
import se.bupp.lek.server.Server.GameMatchSettings._
import se.bupp.lek.server.Server.GameMatchSettings.WhenNumOfConnectedPlayersCriteria
import se.bupp.lek.common.model.Competitor
import se.bupp.lek.server.GameLogicFactory._
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.Server.GameMatchSettings.WhenNumOfConnectedPlayersCriteria
import se.bupp.lek.server.Server.GameMatchSettings.NumOfRoundsPlayed
import java.util.{TimerTask, Timer}
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.Server.GameMatchSettings.WhenNumOfConnectedPlayersCriteria
import se.bupp.lek.server.Server.GameMatchSettings.NumOfRoundsPlayed

import org.apache.log4j.Logger
import se.bupp.lek.common.FuncUtil.RateProbe
import se.bupp.lek.server.GameLogicFactory.KillBasedStrategy.PlayerKill

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */




class Server(portSettings:PortSettings) extends SimpleApplication with PlayStateListener
//with PhysicsTickListener
{



  import Model._
  import Server._

  /*override def prePhysicsTick(p1: PhysicsSpace, p2: Float) {}

  override def physicsTick(p1: PhysicsSpace, p2: Float) {}
    */
  var updateMaxTimeStamp = System.currentTimeMillis()

  var worldSimulator: WorldSimulator = _

  var networkState: ServerNetworkState = _

  var gameLogic:GameLogic = _

  var leRoot:Node = null

  var lobby = new Lobby()

  val log = Logger.getLogger(classOf[Server])


  override def simpleInitApp() {

    //java.util.logging.Logger.getLogger("com.jme3").setLevel(java.util.logging.Level.OFF);
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

    //val (ws, gl) = createWorldSimulator()
    //worldSimulator = ws
    gameLogic = createGameLogic()

    networkState = new ServerNetworkState(portSettings) {
      def addPlayerAction(pa: PlayerActionRequest) {
        pa.ensuring(pa.motion.translation != null && pa.motion.rotation != null)
        Option(getStateManager.getState(classOf[PlayState])).foreach { ps =>
          if (ps.isEnabled && ps.isInitialized) {
            ps.addPlayerAction(pa)
          }
        }
      }

      override def playerJoined(pjr: PlayerJoinRequest) = {
        val resp = new PlayerJoinResponse

        val ps = lobby.addPlayer(pjr)

        resp.playerId = ps.playerId

        //worldSimulator.addParticipant(ps)
        gameLogic.addCompetitor(new Competitor(ps.playerId,ps.teamIdentifier))
        resp
      }

      def playerLeave(playerId: Int) {
        log.info("Player disconnected")
        // TOOD: Make update loop message out of me
        lobby.removePlayer(playerId)
        worldSimulator.removeParticipant(playerId)
        gameLogic.removePlayer(playerId)
      }
    }
    log.info("Game Launch Complete")
  }
  //sealed abstract class PlayStateUpdateMessage
  case class RoundEnded() extends AbstractServerMessage

  sealed abstract class AbstractServerMessage
  case class GameStarted() extends AbstractServerMessage

  case class GameEnded() extends AbstractServerMessage

  var serverMessageQueue = mutable.Queue.empty[AbstractServerMessage]
  var onUpdateSentMessageQueue = mutable.Queue.empty[AbstractServerMessage]


  def onUpdateSent() {
    if(onUpdateSentMessageQueue.size > 0) {
      serverMessageQueue.enqueue(onUpdateSentMessageQueue:_*)
      onUpdateSentMessageQueue = mutable.Queue.empty
    }

  }

  override def simpleUpdate(tpf: Float) : Unit = try {


    val toHandle = serverMessageQueue.dequeueAll(p => true)
    if (toHandle.size > 0 ) {
      log.debug("Server messages to handle " + toHandle.size)
    }
    serverMessageQueue = mutable.Queue.empty[AbstractServerMessage]
    toHandle.foreach {
      case RoundEnded() =>

          log.info("Round ended")
          new Timer().schedule(new TimerTask {
            def run() {
              gameLogic.startRound()
            }
          },3000L)
          worldSimulator.unspawnAllGameObjects()
          networkState.sendRoundOver()

      case GameStarted()  =>
        log.info("Game Started")

        worldSimulator = createWorldSimulator()
        getStateManager.attach(new PlayState(Server.this))

        lobby.connectedPlayers.foreach {
          ps => worldSimulator.addParticipant(ps)
        }
        //gameLogic = gl

        networkState.createSimple(new StartGameRequest)
      case GameEnded() =>
        worldSimulator.unspawnAllGameObjects()

        var playState: PlayState = getStateManager.getState(classOf[PlayState])
        playState.setEnabled(false)
        getStateManager.detach(playState)
        worldSimulator.destroy()
        worldSimulator = null
        //gameLogic = null
        networkState.createSimple(new GameOverRequest())
        log.info("Game ended")
        new Timer().schedule(new TimerTask {
          def run() {
            gameLogic.queryStartGame()

            /*lobby.connectedPlayers.foreach {
              p =>
                gameLogic.addCompetitor(new Competitor(p.playerId,p.teamIdentifier))
            }*/
            //gameLogic.queryStartGame()
          }
        },5000L)
      // lobby mode
    }
    /*updateProbe.tick()
    worldSimulator.world.simulateToLastUpdated()

    worldSimulator.handleStateLogic()

    val simTime: Long = System.currentTimeMillis()

    networkState.querySendUpdate(() => worldSimulator.generateGameWorldChanges(simTime))


    leRoot.updateLogicalState(tpf);

    leRoot.updateGeometricState();*/
  } catch { case e:Exception => e.printStackTrace() ; log.error(e) }

  def createGameLogic() = {

    val settings: GameMatchSettings = new GameMatchSettings(
      startCriteria = WhenNumOfConnectedPlayersCriteria(2),
      roundEndCriteria = ScoreReached(2),
      gameEndCriteria = NumOfRoundsPlayed(2)
    )

    var gameLogicListener = new GameLogicListener() {
      def onGameStart() {

        // send countdown message
        // add timer to start round
        // leave lobby mode
        // enter game mode
        serverMessageQueue.enqueue(GameStarted())

      }

      def onIntermediateRoundStart() {
        // send round started message
        log.info("Round Started")
        networkState.createSimple(new StartRoundRequest)
        worldSimulator.spawnAllParticipants()
      }

      def onCompetetitorScored(scoreDescription: AbstractScoreDescription) {

        log.info("Someone scored")
        val playState: PlayState = getStateManager.getState(classOf[PlayState])

        var kill: PlayerKill = scoreDescription.asInstanceOf[PlayerKill]
        playState.postMessage(new ScoreMessage(kill.of, kill.vi))
        // send displayable score modification
      }

      def onIntermediateRoundEnd(roundResults: RoundResults, standing: GameTotalResults) {
        // send countdown message
        // add timer to start round
        onUpdateSentMessageQueue.enqueue(RoundEnded())
      }

      def onGameEnd(totals: GameTotalResults) {
        log.debug("Scheduling Game End")
        onUpdateSentMessageQueue.enqueue(GameEnded())
      }
    }

    GameLogicFactory.create(settings, gameLogicListener, new KillBasedStrategy())
  }

  def createWorldSimulator() : WorldSimulator = {
    val physicsSpace = new PhysicsSpace()
    val serverWorld = new ServerWorld(leRoot, assetManager, physicsSpace)
    serverWorld.initEmpty()
    worldSimulator = new WorldSimulator(serverWorld) {
      def playerKilledPlayer(killer: Int, victim: Int) {
        gameLogic.scoreStrategy.playerKilledByPlayer(killer,victim)
      }
    }

    worldSimulator
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

  var updateProbe = new RateProbe("App Update", 3000L,log)


}

object Server {

  val log = Logger.getLogger(classOf[Server])

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
      classOf[ServerGameWorld],
      classOf[RoundOverRequest],
      classOf[StartRoundRequest],
      classOf[StartGameRequest],
      classOf[GameOverRequest],
      classOf[SpawnPlayer],
      classOf[KillPlayer],
      classOf[ScoreMessage],
      classOf[ServerWorldStateChange],
      classOf[OrderedMessage]


    )

  object GameMatchSettings {

    sealed abstract class AbstractStartCriteria()

    case class WhenNumOfConnectedPlayersCriteria(num:Int) extends AbstractStartCriteria()
    case class AlwaysOn() extends AbstractStartCriteria()

    sealed abstract class RoundEndCriteria()
    //case class KillsReached(value:Int) extends RoundEndCriteria()
    case class ScoreReached(value:Int) extends RoundEndCriteria()
    case class TimeLimitReached(value:Long) extends RoundEndCriteria()

    sealed abstract class GameEndCriteria()
    case class NumOfRoundsPlayed(value:Int) extends GameEndCriteria()

  }

  class GameMatchSettings(val startCriteria:AbstractStartCriteria,val roundEndCriteria:RoundEndCriteria,val gameEndCriteria:GameEndCriteria) {

  }

  def main(args: Array[String]) {


    val portSettings = args.toList match {
      case tcpPort :: udpPort :: rest => new PortSettings(tcpPort.toInt, udpPort.toInt)
      case _ => new PortSettings(54555, 54777)
    }
    log.info(portSettings.tcpPort + " " + portSettings.udpPort)
    //Logger.getLogger("com.jme3").setLevel(Level.SEVERE)
    new Server(portSettings).start(JmeContext.Type.Headless)
  }
}

