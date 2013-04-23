package se.bupp.lek

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import se.bupp.lek.server.Server.GameMatchSettings
import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.common.MathUtil

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2013-04-08
 * Time: 23:29
 * To change this template use File | Settings | File Templates.
 */
class MathTest extends Specification with Mockito {

  "do math" should {

    "IDENTITY" in {
      var v  = new Vector3f(1, 3, 4)
      val res1 = Quaternion.IDENTITY.clone().mult(v)
      res1 === new Vector3f(1, 3, 4)

      val res2 = MathUtil.noRotation.mult(v)
      res2 === new Vector3f(1, 3, 4)

      val res3 = MathUtil.noRotation.getRotationColumn(0)
      res3 === new Vector3f(1, 0, 0)

      val res4 = Quaternion.IDENTITY.clone().getRotationColumn(0)
      res4 === new Vector3f(1, 0, 0)

      val res5 = Quaternion.IDENTITY.clone().getRotationColumn(2)
      res5 === new Vector3f(0, 0, 1)


      Quaternion.IDENTITY.clone() === MathUtil.noRotation
    }

    "IDENTITY" in {
      var v  = new Vector3f(1, 3, 4)
      val res1 = Quaternion.IDENTITY.clone().mult(v)
      res1 === new Vector3f(1, 3, 4)

      val res2 = MathUtil.noRotation.mult(v)
      res2 === new Vector3f(1, 3, 4)

      val res3 = MathUtil.noRotation.getRotationColumn(0)
      res3 === new Vector3f(1, 0, 0)

      val res4 = Quaternion.IDENTITY.clone().getRotationColumn(0)
      res4 === new Vector3f(1, 0, 0)

      Quaternion.IDENTITY.clone() === MathUtil.noRotation
    }

    "oth" in {
      var axis: Quaternion = new Quaternion().fromAngleNormalAxis(Math.PI.toFloat / 6.0f, Vector3f.UNIT_Y)
      var mult: Vector3f = axis.mult(new Vector3f(1, 0, 0))
      mult.y === 0.0f
      var column: Vector3f = axis.getRotationColumn(0)
      column === new Vector3f(0.8660254f, 0.0f, -0.5f)


    }
  }
}
