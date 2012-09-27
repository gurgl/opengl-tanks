package se.bupp.lek.client

import com.esotericsoftware.kryonet.{Connection, Listener, Client => KryoClient}
import se.bupp.lek.server.{Model, Server}
import se.bupp.lek.server.Model._
import management.ManagementFactory
import com.jme3.app.state.{AbstractAppState, AppStateManager}
import collection.immutable.{TreeSet, SortedSet, Queue}
import com.jme3.app.Application
import VisualWorldSimulation._

import collection.{mutable, JavaConversions}
import JavaConversions.asScalaBuffer
import scala.{None, Some}
import com.jme3.math.Vector3f
import se.bupp.lek.client.MathUtil._
import org.apache.log4j.Logger
import se.bupp.lek.common.FuncUtil.RateProbe
import com.jme3.export.Savable


/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-31
 * Time: 01:17
 * To change this template use File | Settings | File Templates.
 */

object PlayerActionQueue {

  var accTranslation = Vector3f.ZERO.clone()
  var accRotation = noRotation

  var fired = mutable.Stack[ProjectileFireGO]()

  def accumulateMotion(reorientation:Reorientation) {
    accTranslation = accTranslation.add(reorientation._1)
    accRotation = reorientation._2.mult(accRotation)
  }

  def flushMotion() : Reorientation = {
    val r = (accTranslation,accRotation)
    accTranslation = Vector3f.ZERO.clone()
    accRotation = noRotation
    r
  }

  def accumulateProjectile(p: ProjectileFireGO) {
    fired = fired.push(p)
  }

  def flushProjectiles() : List[ProjectileFireGO] = {
    val res = fired.toList
    fired = mutable.Stack[ProjectileFireGO]()
    res
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
  var gameWorldStateChangeQueue:Queue[Model.ServerGameWorld] = Queue()

  var polledUpUntilToOpt = Option.empty[Int]

  var serverUpdProbe = new RateProbe("Server Upd",1000L,log)
  var pollProbe = new RateProbe("PollProbe",1000L,log)


  implicit object OrderedMessageOrdering extends Ordering[OrderedMessage] {
    def compare(x: OrderedMessage, y: OrderedMessage) = if(x.seqId < y.seqId) -1 else if(x.seqId > y.seqId) 1 else 0
  }
  var orderedMessageBuffer = SortedSet.empty[OrderedMessage]




  //implicit def orderingOnConstrainedType[G <: OrderedMessage]: Ordering[G]

  def bufferMessage(om:OrderedMessage) {
    orderedMessageBuffer = insertMessage(orderedMessageBuffer, om)
  }

  def insertMessage[T](l:SortedSet[T], i:T)(implicit ordering:Ordering[T]) : SortedSet[T] = {
    import ordering.mkOrderingOps
    if(l.apply(i)) {
      log.error("duplicate received")
      l
    } else {
      l + i
    }
  }

  /*
    l.toSeq match {
      case Seq() => Seq(i)
      case nonEmpty =>
        val (less, moreOrEqual) = nonEmpty.partition(e => e < i)
        val res = moreOrEqual match {
          case potentialDuplicate :: tail if(potentialDuplicate == i) => less ++: moreOrEqual
          case goodTail => less ++ (i +: goodTail)
        }
        res
    }
  }.toSet*/

  def buffersssMessage(om:OrderedMessage) {

  /*orderedMessageBuffer.synchronized {
  orderedMessageBuffer match {
    case Nil =>
    case nonEmpty =>
      val (less, moreOrEqual) = nonEmpty.partition(e => e.seqId < om.seqId)
      moreOrEqual match {
        case x :: tail if(x.seqId == om.seqId) => orderedMessageBuffer
        case goodTail => om +: moreOrEqual
      }
  }
}

def bupp(l:SortedSet[Int], i:Int) : SortedSet[Int] = SortedSet.empty[Int] ++ {
  l.toSeq match {
    case Seq() => Seq.apply(i)
    case nonEmpty =>
      val (less, moreOrEqual) = nonEmpty.partition(e => e < i)
      val res = moreOrEqual match {
        case x :: tail if(x == i) => less ++: moreOrEqual
        case goodTail => less ++ (i +: goodTail)
      }
      res
  }
}.toSet*/

  }

  def handleOrderedMessageDetailed(om:OrderedMessage) = om match {
    case response:ServerGameWorld=>
      //serverUpdProbe.tick()
           //log.info("Tja")
      //if(gameApp.playerIdOpt.isDefined) {
        //                lock.synchronized {
        /*if(response.seqId != lastReceiveSeqId + 1) {
        log.error("bad sequence id. Last " + lastReceiveSeqId + ", new " + response.seqId )
      }
      lastReceiveSeqId = response.seqId*/
        handleWorldUpdate(response)
        //              }
      /*} else {
        log.warn("Getting world wo player received.")
      }*/
    case response:RoundOverRequest =>
      gameApp.postMessage(response)
    case response:StartRoundRequest =>
      gameApp.postMessage(response)
    case response:GameOverRequest =>
      gameApp.postMessage(response)
      gameWorldUpdatesQueue = Queue()
    case response:StartGameRequest =>
      gameApp.postMessage(response)
    case x => log.info(x.getClass)
  }

  def handleOrderedMessage(om:OrderedMessage) {
    serverUpdProbe.tick()
    orderedMessageBuffer.synchronized {
      polledUpUntilToOpt match {
        case Some(polledUpUntil) =>
          if(om.seqId <= polledUpUntil) {
            // discard
          } else {
            bufferMessage(om)
            if(orderedMessageBuffer.head.seqId == polledUpUntil + 1) {
              var nextExpected = polledUpUntil + 1

              val toDeliver = orderedMessageBuffer.takeWhile {
                e =>

                  val r = nextExpected == e.seqId
                  nextExpected = nextExpected + 1
                  r
              }

              if (toDeliver.size > 0) polledUpUntilToOpt = Some(toDeliver.last.seqId)

              orderedMessageBuffer = orderedMessageBuffer.dropWhile {
                e => e.seqId < nextExpected
              }


              toDeliver.foreach { m => {pollProbe.tick() ; handleOrderedMessageDetailed(m)}  }


            } /*else {

                    }*/
          }
        case None =>
          polledUpUntilToOpt = Some(om.seqId)
          handleOrderedMessageDetailed(om)
      }
    }

  }

  def initClient() {
    gameClient = new KryoClient();

    val kryo = gameClient.getKryo();

    Server.getNetworkMessages.foreach( kryo.register(_))

    gameClient.addListener(new Listener() {
      override def received (connection:Connection , obj:Object ) = try {
        obj match {
          case om:OrderedMessage =>
            //log.info(om.getClass + " " + gameApp.playerIdOpt.isDefined)
            if(gameApp.playerIdOpt.isDefined) {
              handleOrderedMessage(om)
            }

            /*
          case response:ServerGameWorld=>
            //serverUpdProbe.tick()

            if(gameApp.playerIdOpt.isDefined) {
              //                lock.synchronized {
              /*if(response.seqId != lastReceiveSeqId + 1) {
                log.error("bad sequence id. Last " + lastReceiveSeqId + ", new " + response.seqId )
              }
              lastReceiveSeqId = response.seqId*/
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
              */
          case response:PlayerJoinResponse =>
            log.info("join resp received " + response.playerId)
            gameApp.playerIdOpt = Some(response.playerId)

          case _ =>
        }
      } catch { case e:Exception => e.printStackTrace()}
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

  // TODO: Remove unused variable
  def sendClientUpdate(simTime: Long, visualWorldSimulation:VisualWorldSimulation) {
    val projectiles = PlayerActionQueue.flushProjectiles()
    val reorientation = PlayerActionQueue.flushMotion()

    /*if (lastReorDebug != null && lastReorDebug._1 != reorientation._1) {
      log.debug(if(reorientation._1 == Vector3f.ZERO) "stop" else "move")
    }
    lastReorDebug = reorientation*/

    val request = gameApp.createPlayerActionRequest(simTime, reorientation, projectiles)
    gameClient.sendUDP(request)

  }

  override def cleanup() {
    log.debug("cleanup " + buffer.toString)
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

    gameWorldStateChangeQueue = gameWorldStateChangeQueue.enqueue(serverUpdate)

    /*
    getPlayState.foreach {
      playState =>
        if(playState.isInitialized) {
          applyWorldUpdate(playState, serverUpdate)
        }
    }*/

  }

  /**
   * TODO: Evaluate why this "copy" should be done separate of movement etc
   */
  def applyWorldUpdate(playState: PlayState, serverUpdate: Model.ServerGameWorld) {
    //println("ITS INITIALIZED")
    /*val toKill = serverUpdate.deadPlayers.toList
    if (toKill.size > 0) {
      log.info("handle deaths")
      playState.visualWorldSimulation.handleKilledPlayers(toKill)
    }

    if (playState.visualWorldSimulation.playerDead) {
      serverUpdate.alivePlayers.find( p => p.playerId == gameApp.playerIdOpt.get).foreach {
        p =>
          playState.visualWorldSimulation.respawnLocalPlayer()
      }
    }*/
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

      PlayerActionQueue.accumulateMotion(input)
    }

    var currentGameWorldUpdates:Queue[ServerGameWorld] = null
    def generateGameWorld(simTime: Long) : Option[VisualGameWorld] = {
      currentGameWorldUpdates = Queue(gameWorldUpdatesQueue: _*)

      val worldStateChangeToHandle = Seq(gameWorldStateChangeQueue:_*)
      gameWorldStateChangeQueue = gameWorldStateChangeQueue.companion.empty

      var stateChanges = Seq.empty[ServerStateChanges]


      worldStateChangeToHandle.foreach { u =>
        val toKill = u.deadPlayers.toList
        if (toKill.size > 0) {
          log.info("handle deaths")
          stateChanges = stateChanges :+ KillPlayers(toKill)
        }

        //if (playState.visualWorldSimulation.playerDead) {
        //val playerId = visualWorldSimulation.playerIdOpt.apply().get
        val list = u.newAlivePlayersInfo.map { pi =>

          log.info("respawn mess")
            val p = u.alivePlayers.find( p => p.playerId == pi.playerId).get
            (pi,p)
            /*pi =>
              log.debug("Spawning player")
              val p = lastGameWorldUpdate.alivePlayers.find(p => pi.playerId == p.playerId).getOrElse(throw new IllegalStateException("bad"))
              playState.visualWorldSimulation.respawnLocalPlayer(p,pi)*/

        //}

        }.toList
        if (list.size > 0) {
          stateChanges = stateChanges :+ SpawnPlayers(list)
        }
      }
      /*
      val toKill = lastGameWorldUpdate.deadPlayers.toList
      if (toKill.size > 0) {
        log.info("handle deaths")
        playState.visualWorldSimulation.handleKilledPlayers(toKill)
      }

      if (playState.visualWorldSimulation.playerDead) {
        lastGameWorldUpdate.newAlivePlayersInfo.find( pi => pi.playerId == playerIdOpt.apply().get).foreach {
          //lastGameWorldUpdate.alivePlayers.find( p => p.playerId == playerIdOpt.apply().get).foreach {
          pi =>
            log.debug("Spawning player")
            val p = lastGameWorldUpdate.alivePlayers.find(p => pi.playerId == p.playerId).getOrElse(throw new IllegalStateException("bad"))
            playState.visualWorldSimulation.respawnLocalPlayer(p,pi)
        }
      }
       */

      var buppOpt: Option[(Set[AbstractOwnedGameObject with Savable], ServerGameWorld)] = visualWorldSimulation.generateLocalGameWorld(simTime, currentGameWorldUpdates)
      buppOpt.map(bupp => (bupp._1,bupp._2, stateChanges))

    }
  }
}
