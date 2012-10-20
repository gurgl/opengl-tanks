package se.bupp.lek.common

import com.jme3.math.{Vector3f, Quaternion}
import se.bupp.lek.common.Model._

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/16/12
 * Time: 11:49 PM
 * To change this template use File | Settings | File Templates.
 */


object MathUtil {
  def noRotation = Quaternion.ZERO.clone().fromAngleNormalAxis(0f,Vector3f.UNIT_XYZ.clone())

  def noMotion:Reorientation = (Vector3f.ZERO.clone(), noRotation)
}
