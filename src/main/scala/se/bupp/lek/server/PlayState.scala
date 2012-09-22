package se.bupp.lek.server

import com.jme3.app.state.{AppStateManager, AbstractAppState, AppState}
import se.bupp.lek.common.FuncUtil.RateProbe
import org.apache.log4j.Logger
import com.jme3.app.Application
import se.bupp.lek.server.Model.PlayerActionRequest

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-09-22
 * Time: 18:57
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class ServerPlayStateMessage
case class PlayerKillPlayerMessage(player: Model.GameParticipant, killer:Int) extends ServerPlayStateMessage


class PlayState(val server:Server) extends AbstractAppState {
  val log = Logger.getLogger(classOf[PlayState])
  var updateProbe = new RateProbe("App Update", 3000L,log)

  override def update(tpf: Float) : Unit = try {

    updateProbe.tick()
    server.worldSimulator.world.simulateToLastUpdated()

    server.worldSimulator.handleStateLogic()

    val simTime: Long = System.currentTimeMillis()

    server.networkState.querySendUpdate(() => server.worldSimulator.generateGameWorldChanges(simTime))


    server.leRoot.updateLogicalState(tpf);

    server.leRoot.updateGeometricState();
  } catch { case e:Exception => e.printStackTrace() ; throw e }

  override def initialize(stateManager: AppStateManager, app: Application) {
    super.initialize(stateManager, app)
    log.debug("Playstate init done")
  }

  def addPlayerAction(request: PlayerActionRequest) {
    server.worldSimulator.addPlayerAction(request)
  }
}
