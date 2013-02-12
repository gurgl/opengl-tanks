package se.bupp.lek.client

import com.esotericsoftware.kryonet.{Connection, Listener, Client => KryoClient}
import se.bupp.lek.common.Model._
import management.ManagementFactory
import com.jme3.app.state.{AbstractAppState, AppStateManager}
import collection.immutable.{TreeSet, SortedSet, Queue}
import com.jme3.app.Application
import VisualWorldSimulation._

import collection.{mutable, JavaConversions}
import JavaConversions.asScalaBuffer
import scala.{None, Some}
import com.jme3.math.Vector3f
import org.slf4j.LoggerFactory
import se.bupp.lek.common.FuncUtil.RateProbe
import com.jme3.export.Savable
import com.esotericsoftware.kryonet.Listener.LagListener
import se.bupp.lek.common.{MathUtil, Tmp}


/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-07-31
 * Time: 01:17
 * To change this template use File | Settings | File Templates.
 */

object PlayerActionQueue {

  var accTranslation = Vector3f.ZERO.clone()
  var accRotation = MathUtil.noRotation

  var fired = mutable.Stack[ProjectileFireGO]()

  def accumulateMotion(reorientation:Reorientation) {
    accTranslation = accTranslation.add(reorientation._1)
    accRotation = reorientation._2.mult(accRotation)
  }

  def flushMotion() : Reorientation = {
    val r = (accTranslation,accRotation)
    accTranslation = Vector3f.ZERO.clone()
    accRotation = MathUtil.noRotation
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

case class ClientConnectSettings(var host:String, var tcpPort: Int, var udpPort: Int) {
  //println(connectMessage)
}

trait WorldUpdater {
  def postUpdate(simTime: Long)
  def processInput(input: PlayerInput.Reorientation,lastUpdate:Option[(Long,Reorientation)])
  def generateGameWorldToRender(simTime: Long) : Option[VisualGameWorld]
}
class NetworkGameState(val clientSettings:Client.Settings) extends AbstractAppState {

  var gameClient:KryoClient = _

  var gameApp:Client = _

  var buffer = new StringBuffer()

  val GW_UPDATES_SIZE = 20

  val log = LoggerFactory.getLogger(classOf[NetworkGameState])

  var gameWorldUpdatesQueue:Queue[(Long,ServerGameWorld)] = Queue()
  var gameWorldStateEventQueue:Queue[ServerGameWorld] = Queue()

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
      //gameWorldUpdatesQueue = Queue()
    case response:StartGameRequest =>
      gameApp.postMessage(response)
    case x => log.info("handleOrderedMessageDetailed " + x.getClass.getSimpleName)
  }

  def handleOrderedMessage(om:OrderedMessage) {
    //serverUpdProbe.tick()
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


              toDeliver.foreach { m => { handleOrderedMessageDetailed(m)}  }


            } else {
              log.debug("recieiving out of order " + polledUpUntilToOpt.get + " " + om.seqId + " " + orderedMessageBuffer.headOption.map(_.seqId))
            }
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

    getNetworkMessages.foreach( kryo.register(_))

    val listener = new Listener() {
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
    }
    gameClient.addListener(Tmp.decorateListener(listener));

    gameClient.start();
    log.info("tcpPort " + clientSettings.connect.tcpPort + ",  updPort " + clientSettings.connect.udpPort)
    gameClient.connect(5000, clientSettings.connect.host, clientSettings.connect.tcpPort, clientSettings.connect.udpPort);

    val playerJoinRequest = new PlayerJoinRequest(clientSettings.playerInfo)
    clientSettings.teamIdOpt.foreach( tid => playerJoinRequest.teamIdentifier = tid)
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

  def handleWorldUpdate(serverUpdate: ServerGameWorld) {
    gameWorldUpdatesQueue =
      Option(gameWorldUpdatesQueue).map(
        x => if (x.size >= GW_UPDATES_SIZE) {
          x.dequeue._2
        } else {
          x
        }
      ).head.enqueue((Client.clock(),serverUpdate));

    gameWorldStateEventQueue = gameWorldStateEventQueue.enqueue(serverUpdate)

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
  //def applyWorldUpdate(playState: PlayState, serverUpdate: Model.ServerGameWorld) {
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
  //}

  class ServerWorldUpdater(visualWorldSimulation:VisualWorldSimulation) extends WorldUpdater {

    var lastSentUpdate = 0L

    def postUpdate(simTime: Long) {

      if ((Client.clock() - lastSentUpdate).toFloat > 1000f / 15f) {
        sendClientUpdate(simTime,visualWorldSimulation)
        lastSentUpdate = Client.clock()
      }
    }

    def processInput(input: PlayerInput.Reorientation,lastUpdate:Option[(Long,Reorientation)]) {
      lastUpdate.foreach {
        case (lastSimTime, lastInput) =>
          visualWorldSimulation.storePlayerLastInputAndOutput(lastSimTime, lastInput)
      }

      PlayerActionQueue.accumulateMotion(input)
    }



    def generateGameWorldToRender(simTime: Long) : Option[VisualGameWorld] = {
      var currentGameWorldUpdates = Queue(gameWorldUpdatesQueue: _*)

      if(currentGameWorldUpdates.size > 0) {
        val worldStateEventsToHandle = Seq(gameWorldStateEventQueue:_*)
        gameWorldStateEventQueue = gameWorldStateEventQueue.companion.empty

        val stateEvents = convertStateChanges(worldStateEventsToHandle)

        val (lastReceivedTS, lastGameWorld) = currentGameWorldUpdates.last
        val serverRelativeSimTime = lastGameWorld.timeStamp + (simTime - lastReceivedTS)

          /*if (list.size > 0) {
            stateChanges = stateChanges :+ SpawnPlayers(list)
          }*/

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

        var buppOpt: Option[(Set[AbstractOwnedGameObject with Savable], ServerGameWorld)] = visualWorldSimulation.generateLocalGameWorld(serverRelativeSimTime, currentGameWorldUpdates.map(_._2))
        buppOpt.map(bupp => (bupp._1,bupp._2, stateEvents))
      } else None
    }
  }

  def convertStateChanges(worldStateChangeToHandle: Seq[ServerGameWorld]) : Seq[ServerStateChanges] = {
    var stateChanges = Seq.empty[ServerStateChanges] ++ worldStateChangeToHandle.flatMap {
      u =>
        val ut = u.stateChanges.toList
        val kills = ut.collect {
          case pi: KillPlayer =>
            //if (toKill.size > 0) {
            log.info("handle deaths")
            pi.playerId
          //}
        }

        //if (playState.visualWorldSimulation.playerDead) {
        //val playerId = visualWorldSimulation.playerIdOpt.apply().get
        val spawns = ut.collect {
          case pi: SpawnPlayer =>
            log.info("respawn mess")
            val p = u.alivePlayers.find(p => p.playerId == pi.playerId).get
            (pi, p)
        }
        val scores = ut.collect {
          case pi: ScoreMessage =>
            log.debug("Score received")
            PlayerScore(pi.offender, pi.victim)

        }

        val l = List(if (kills.size > 0) Some(KillPlayers(kills)) else None, if (spawns.size > 0) Some(SpawnPlayers(spawns)) else None)
        l.flatMap(p => p) ++ scores

    }
    stateChanges
  }
}
