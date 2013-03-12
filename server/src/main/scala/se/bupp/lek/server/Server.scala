package se.bupp.lek.server


import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.system.JmeContext
import collection.{mutable, JavaConversions}
import se.bupp.lek.common.Model._

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
import se.bupp.lek.server.GamePhaseOrchestratorFactory._
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.Server.GameMatchSettings.WhenNumOfConnectedPlayersCriteria
import se.bupp.lek.server.Server.GameMatchSettings.NumOfRoundsPlayed
import java.util.{TimerTask, Timer}
import se.bupp.lek.server.Server.GameMatchSettings.ScoreReached
import se.bupp.lek.server.Server.GameMatchSettings.WhenNumOfConnectedPlayersCriteria
import se.bupp.lek.server.Server.GameMatchSettings.NumOfRoundsPlayed

import java.util.logging.{Logger => JUJME3Logger, Level => JULevel, LogManager}
import org.apache.log4j.{PatternLayout, FileAppender, Level, Logger}
import se.bupp.lek.common.FuncUtil.{Int, RateProbe}
import se.bupp.lek.server.GamePhaseOrchestratorFactory.KillBasedStrategy.PlayerKill
import scala.util.{Failure, Success, Random}
import java.rmi.registry.{Registry, LocateRegistry}
import java.lang.Exception
import java.rmi.{ConnectException, Naming, Remote, RMISecurityManager}
import java.security.Permission
import java.lang.reflect.Method
import java.{util, lang}
import java.io.File
import se.bupp.cs3k.api.GameServerFacade
import org.slf4j.LoggerFactory
import util.logging.LogManager
import org.slf4j.bridge.SLF4JBridgeHandler
import com.fasterxml.jackson.databind.ObjectMapper

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

  import Server._

  /*override def prePhysicsTick(p1: PhysicsSpace, p2: Float) {}

  override def physicsTick(p1: PhysicsSpace, p2: Float) {}
    */
  var worldSimulator: WorldSimulator = _

  var networkState: ServerNetworkState = _

  var gameLogic:GamePhaseOrchestrator = _

  var serverMode:ServerMode = _

  var leRoot:Node = null

  var lobby = new Lobby()

  val log = LoggerFactory.getLogger(classOf[Server])

  var objectMapper = new ObjectMapper


  override def simpleInitApp() {

    //java.util.logging.LoggerFactory.getLogger("com.jme3").setLevel(java.util.logging.Level.OFF);
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
    gameLogic = GamePhaseOrchestratorFactory.createGameLogic(Server.settings.gameSetup,this)

    networkState = new ServerNetworkState(portSettings) {
      def addPlayerAction(pa: PlayerActionRequest) {
        pa.ensuring(pa.motion.translation != null && pa.motion.rotation != null)
        Option(getStateManager.getState(classOf[PlayState])).foreach { ps =>
          if (ps.isEnabled && ps.isInitialized) {
            ps.addPlayerAction(pa)
          }
        }
      }

      def playerJoined(pjr:PlayerJoinRequest, masterPlayerInfoOpt:Option[PlayerInfoServerLobby]) = {
        val resp = new PlayerJoinResponse

        val ps = lobby.addPlayer(pjr, masterPlayerInfoOpt)

        resp.playerId = ps.playerId

        //worldSimulator.addParticipant(ps)
        gameLogic.addCompetitor(new Competitor(ps.playerId,ps.teamIdentifier))
        resp
      }

      def playerLeave(playerId: Int) {
        log.info("Player disconnected")
        // TOOD: Make update loop message out of me
        lobby.removePlayer(playerId)
        // TODO: Ugly check
        if(worldSimulator != null) worldSimulator.removeParticipant(playerId)
        gameLogic.removePlayer(playerId)
      }
    }
    log.info("Game Launch Complete")
  }



  private var serverMessageQueue = mutable.Queue.empty[AbstractServerMessage]
  var onUpdateSentMessageQueue = mutable.Queue.empty[AbstractServerMessage]

  def appendToQueue(elems:AbstractServerMessage*) {
    serverMessageQueue.synchronized {
      serverMessageQueue.enqueue(elems:_*)
    }
  }
  def onUpdateSent() {
    if(onUpdateSentMessageQueue.size > 0) {
      appendToQueue(onUpdateSentMessageQueue:_*)
      onUpdateSentMessageQueue = mutable.Queue.empty
    }
  }

  override def simpleUpdate(tpf: Float) : Unit = try {


    val toHandle = serverMessageQueue.synchronized {
      val th = serverMessageQueue.dequeueAll(p => true)
      if (th.size > 0 ) {
        log.debug("Server messages to handle " + th.size)
      }
      serverMessageQueue = mutable.Queue.empty[AbstractServerMessage]
      th
    }
    if(toHandle.size > 0 ) log.info("To handle : " + toHandle.map(_.getClass.getSimpleName).mkString(","))



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

        networkState.sendToAllClients(new StartGameRequest)
      case GameEnded(totals) =>

        import scala.concurrent.ExecutionContext.Implicits.global

        Server.settings.masterServer.gameSessionIdOpt.foreach { o =>
          val f = scala.concurrent.future {
            val serializer = new ScoreSerializer {
              def serialize(s: AnyRef) = server.objectMapper.writeValueAsString(s)
            }
            Server.gameServerFacade.endGame(o.toInt,totals.getSerializedResult(serializer))
          }
          f onComplete {
            case Success(_) =>
            case Failure(e) => log.error("ERROR contacting master " + e)
          }
        }

        worldSimulator.unspawnAllGameObjects()

        var playState: PlayState = getStateManager.getState(classOf[PlayState])
        playState.setEnabled(false)
        getStateManager.detach(playState)
        worldSimulator.destroy()
        worldSimulator = null
        //gameLogic = null

        log.info("Game ended")

        val sendShutdown = Server.settings.serverMode match {
          case ServerMode(true,false) =>
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
            false
          //case ServerMode(false,true) =>
          case _ =>
            true
        }

        networkState.sendGameOver(sendShutdown)


        //appendToQueue(LeaveGameMode())

      //case LeaveGameMode()
    }

    updateProbe.tick()

  } catch { case e:Exception => e.printStackTrace() ; log.debug(String.valueOf(e)) }


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

  sealed abstract class AbstractServerMessage

  case class RoundEnded() extends AbstractServerMessage
  case class GameStarted() extends AbstractServerMessage
  case class GameEnded(gameResult:AbstractGameResult) extends AbstractServerMessage
  //case class LeaveGameMode() extends AbstractServerMessage
  //case class EnterGameMode() extends AbstractServerMessage


  case class PortSettings(var tcpPort:Int, var udpPort:Int)
  case class MasterServerSettings(var host:String, var port:Int, var gameSessionIdOpt:Option[Long])

  trait IsContinous {
    self : AbstractGameDescription =>
  }

  class Settings(var ports:PortSettings, var masterServer:MasterServerSettings, var log:Option[File], var gameSetup:AbstractGameDescription)  {
    def serverMode = gameSetup match {
      case s:IsContinous => ServerMode(true,false)
      case _ => ServerMode(false, true)
    }
  }
  sealed abstract class AbstractGameDescription()
  case class FreeForAll(val numOfPlayers:Int) extends AbstractGameDescription()
  case class TeamDeathmatch(val numOfTeams:Int, val numOfPlayersPerTeam:Int) extends AbstractGameDescription() {
    if(numOfTeams < 2) throw new IllegalArgumentException("Gah")
  }



  case class ServerMode(val restartOnIdle:Boolean, val quitOnGameOver:Boolean)

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

  class GameMatchSettings(val startCriteria:AbstractStartCriteria,val roundEndCriteria:RoundEndCriteria,val gameEndCriteria:GameEndCriteria) {  }


  val log = LoggerFactory.getLogger(classOf[Server])

  var server:Server = _

  var occassionIdOpt = Option.empty[Int]
  var gameServerFacade:GameServerFacade = _

  var settings:Settings = _


  def clock() = System.currentTimeMillis()



  def initMasterServerConnection(mss:MasterServerSettings) = {
    if (System.getSecurityManager() == null) {
      System.setSecurityManager(new SecurityManager() {
        override def checkPermission(perm: Permission) {

        }
      } );
    }
    try {
      //val name = "Compute";
      val registry = LocateRegistry.getRegistry(mss.host, mss.port);

      var lookup: Remote = registry.lookup("game-server-facade")
      //lookup.getClass.getInterfaces.toList.foreach(println)

      gameServerFacade = lookup.asInstanceOf[GameServerFacade]
      log.info("gameServerFacade" + gameServerFacade)
    //Pi task = new Pi(Integer.parseInt(args[1]));
    //BigDecimal pi = comp.executeTask(task);
    //System.out.println(pi);
    } catch {
      case e:ConnectException =>
        log.info("Not able to connecto master server - continueing wo")
        //System.err.println("ComputePi exception:");

      case e:Exception => e.printStackTrace()
    } finally {
      if(gameServerFacade == null) {
        gameServerFacade = new GameServerFacade {

          def startGame(p1: lang.Long, p2: util.Map[Integer, Integer]) {}

          def startGame(p1: lang.Long, p2: util.List[Integer]) {}

          def endGame(p1: lang.Long, p2: String) {}

          def evaluateGamePass(p1: String, l:java.lang.Long) = null
        }
      }
    }
  }

  def createDefaultSettings = new Settings(new PortSettings(54555, 54777), new MasterServerSettings("localhost", 1199, None), None, FreeForAll(2))

  val usage = """
    Usage: Server [options]
      options :
         --udp-port <port>
         --tcp-port <port>
         --master-host <host>
         --master-port <port>
         --log <path>
         --game-sessiond-id <id>
         --game-setup <name>
              """

  type OptionMap = Map[Symbol, Any]
  def handleCommandLine(args: Array[String]) : Settings = {
    if (args.length == 0) println(usage)
    val arglist = args.toList



    val defaultSettings = createDefaultSettings

    def nextOption(map : Settings, list: List[String]) : Settings = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        //case "--udp-port" :: tail => nextOption(map ++ Map('useyeah -> true), tail)
        case "--udp-port" :: value :: tail =>
          map.ports.udpPort = value.toInt
          nextOption(map , tail)
        case "--tcp-port" :: value :: tail =>
          map.ports.tcpPort = value.toInt
          nextOption(map , tail)
        case "--master-host" :: value :: tail =>
          map.masterServer.host = value
          nextOption(map , tail)
        case "--master-port" :: value :: tail =>
          map.masterServer.port = value.toInt
          nextOption(map, tail)
        case "--log" :: value :: tail =>
          map.log = Some(new File(value))
          nextOption(map, tail)

        case "--game-session-id" :: value :: tail =>
          map.masterServer.gameSessionIdOpt = Some(value.toLong)
          nextOption(map, tail)
        case "--game-setup" :: value :: tail =>
          val setup = value match {
            case "ffa2cont" => new FreeForAll(2) with IsContinous
            case "ffa2" => FreeForAll(2)
            case "2vs2" => TeamDeathmatch(2,2)
            case "1vs1" => TeamDeathmatch(2,1) // REMOVE ME
            case _ => println("Unknown game setup "+value)
              sys.exit(1)
              throw new RuntimeException("bad option")
          }

          map.gameSetup = setup
          nextOption(map, tail)

        case option :: tail => println("Unknown option "+option)
        exit(1)
      }
    }
    val options = nextOption(defaultSettings,arglist)
    options
  }

  def main(args: Array[String]) {

    JUJME3Logger.getLogger("com.jme3").setLevel(JULevel.INFO)

    // Optionally remove existing handlers attached to j.u.l root logger
    SLF4JBridgeHandler.removeHandlersForRootLogger();  // (since SLF4J 1.6.5)

    // add SLF4JBridgeHandler to j.u.l's root logger, should be done once during
    // the initialization phase of your application
    SLF4JBridgeHandler.install();

    try {
      // TODO: Remvoe me - RAndom wait to fix some error?
      Thread.sleep(new Random().nextInt(2000))
    } catch { case e:InterruptedException =>  }

    settings = handleCommandLine(args)
    settings.log.foreach(
      l => {
        if(l.exists()) throw new IllegalArgumentException("log file must not exist")
        var rootLogger: Logger = Logger.getRootLogger

        rootLogger.removeAllAppenders()
        var fa = new FileAppender()

        fa.setFile(l.getAbsolutePath)
        fa.setLayout(new PatternLayout("%d %-5p [%c{1}] %m%n"));
        fa.setThreshold(Level.DEBUG);
        fa.setAppend(true);
        fa.activateOptions();
        rootLogger.addAppender(fa)
      }
    )

    log.info("CommandLine " + args.toList.mkString(" "))
    initMasterServerConnection(settings.masterServer)
    log.info(settings.ports.tcpPort + " " + settings.ports.udpPort + ", gameOccassionId " + occassionIdOpt)

    /*
    // root looger to set slf4j handler on

    Logger rootLogger = LogManager.getLogManager().getLogger("");
            // remove old handlers
            for (Handler handler : rootLogger.getHandlers()) {
                oldHandlers.add(handler);
                rootLogger.removeHandler(handler);
            }
            // add our own
            activeHandler = new JuliToLog4jHandler();
            activeHandler.setLevel(handlerLevel);
            rootLogger.addHandler(activeHandler);
            rootLogger.setLevel(rootLevel);
            // done, let's check it right away!!!

     */

    server = new Server(settings.ports)
    server.start(JmeContext.Type.Headless)
  }
}

