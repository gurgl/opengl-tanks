package se.bupp.lek.server

import com.jme3.app.state.{AppStateManager, AbstractAppState, AppState}
import se.bupp.lek.common.FuncUtil.RateProbe
import org.apache.log4j.Logger
import com.jme3.app.Application

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-09-22
 * Time: 18:57
 * To change this template use File | Settings | File Templates.
 */
class PlayState(val server:Server) extends AbstractAppState {
  val log = Logger.getLogger(classOf[PlayState])
  var updateProbe = new RateProbe("App Update", 3000L,log)

  override def update(tpf: Float) : Unit = try {

    updateProbe.tick()
    server.worldSimulator.world.simulateToLastUpdated()

    server.worldSimulator.handleStateLogic()

    val simTime: Long = System.currentTimeMillis()

    server.networkState.update(() => server.worldSimulator.generateGameWorldChanges(simTime))


    server.leRoot.updateLogicalState(tpf);

    server.leRoot.updateGeometricState();
  } catch { case e:Exception => e.printStackTrace() }

  override def initialize(stateManager: AppStateManager, app: Application) {
    super.initialize(stateManager, app)
    log.debug("Playstate init done")
  }
}
