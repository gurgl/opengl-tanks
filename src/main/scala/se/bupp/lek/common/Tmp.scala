package se.bupp.lek.common

import com.esotericsoftware.kryonet.Listener
import com.esotericsoftware.kryonet.Listener.LagListener

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-10-08
 * Time: 19:25
 * To change this template use File | Settings | File Templates.
 */
object Tmp {
  val doSimulateLag = true
  val doSimulateClockIndependence = false
  def decorateListener(l:Listener) : Listener = {
    if(doSimulateLag) new LagListener(1000,2000,l) else l
  }
}
