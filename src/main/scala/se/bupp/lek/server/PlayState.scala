package se.bupp.lek.server

import com.jme3.app.state.{AppStateManager, AbstractAppState, AppState}
import se.bupp.lek.common.FuncUtil.RateProbe
import org.apache.log4j.Logger
import com.jme3.app.Application
import se.bupp.lek.server.Model.{ScoreMessage, PlayerActionRequest}
import collection.mutable
import java.util.{TimerTask, Timer}

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-09-22
 * Time: 18:57
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class ServerPlayStateMessage
case class PlayerKillPlayerMessage(player: Model.GameParticipant, killer:Int) extends ServerPlayStateMessage

trait PlayStateListener {
  def onUpdateSent() : Unit
}

class PlayState(val server:Server) extends AbstractAppState {
  var listeners = List.empty[PlayStateListener]

  val log = Logger.getLogger(classOf[PlayState])
  var updateProbe = new RateProbe("App Update", 3000L,log)

  var playMessageQueue = mutable.Queue.empty[ScoreMessage]

  var roundEnded = false

  addUpdateSentLister(server)

  def addUpdateSentLister(psl:PlayStateListener) {
    listeners = listeners :+ psl
  }

  override def update(tpf: Float) : Unit = try {

    //updateProbe.tick()
    server.worldSimulator.world.simulateAllUpdates(tpf)

    server.worldSimulator.handleStateLogic()

    // TODO: Make a better criteria for "no game running"
    if (server.worldSimulator == null || server.worldSimulator.destroyed) return

    val simTime: Long = Server.clock()

    if(server.networkState.querySendUpdate(() => server.worldSimulator.generateGameWorldChanges(simTime))) {
       listeners.foreach(_.onUpdateSent())
    }

    server.leRoot.updateLogicalState(tpf);

    server.leRoot.updateGeometricState();
  } catch { case e:Exception => e.printStackTrace() ; throw e }

  override def initialize(stateManager: AppStateManager, app: Application) {
    super.initialize(stateManager, app)
    log.debug("Playstate init done")
  }

  def postMessage(ref:ScoreMessage) {
    //playMessageQueue.enqueue(ref)
    ref match {
      case sm: ScoreMessage => server.worldSimulator.scoreSinceLastUpdate = server.worldSimulator.scoreSinceLastUpdate :+ sm
      //case re: RoundEndMessage => playMessageQueue.enqueue(re)
    }


  }

  def addPlayerAction(request: PlayerActionRequest) {
    server.worldSimulator.addPlayerAction(request)
  }
}
