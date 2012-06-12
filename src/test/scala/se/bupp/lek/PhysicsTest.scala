package se.bupp.lek

import com.jme3.asset.{DesktopAssetManager, AssetManager, ModelKey}
import com.jme3.bullet.PhysicsSpace
import com.jme3.bullet.PhysicsSpace.BroadphaseType
import com.jme3.math.Vector3f
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import com.jme3.scene.{Spatial, Node}
import org.specs2.mutable.Specification
import se.bupp.lek.client.MathUtil
import se.bupp.lek.server.Model.Orientation
import com.jme3.system.{JmeContext, AppSettings, JmeSystem}
import com.jme3.bullet.control.CharacterControl
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/12/12
 * Time: 7:33 PM
 * To change this template use File | Settings | File Templates.
 */

class PhysicsTest extends Specification {

  "lal" should {
    val broadphaseType = BroadphaseType.DBVT;
    val worldMin = new Vector3f(-10000f, -10000f, -10000f);
    val worldMax = new Vector3f(10000f, 10000f, 10000f);

    "lol" in {


      val settings = new AppSettings(true);
      val context = JmeSystem.newContext(settings, JmeContext.Type.Headless);
          //context.setSystemListener(this);
      context.create(false);

      val node = new Node()

      val pSpace = new PhysicsSpace(worldMin, worldMax, broadphaseType);

      val pd = new Orientation(new Vector3f(0, 2.5f, 0), MathUtil.noRotation)
      //val assetManager = new DesktopAssetManager()
      val assetManager = JmeSystem.newAssetManager(
        Thread.currentThread().getContextClassLoader()
          .getResource("com/jme3/asset/Desktop.cfg"))
      val player = assetManager.loadModel(new ModelKey("tank2.blend"))
      //enemy.setMaterial(mat_default)


      player.setLocalScale(0.5f)
      //tank.setLocalTranslation(pd.position)
      player.setLocalRotation(pd.direction)

      val capsuleShape = new CapsuleCollisionShape(0.05f, 0.05f, 1)
      val playerControl = new CharacterControl(capsuleShape, 0.1f)
      playerControl.setUseViewDirection(false)
      player.addControl(playerControl)

      pSpace.add(playerControl)


      playerControl.setJumpSpeed(0);
      playerControl.setFallSpeed(0.0f);
      playerControl.setGravity(0.0f);
      playerControl.setPhysicsLocation(pd.position.clone());


      node.attachChild(player)


      playerControl.setWalkDirection(new Vector3f(.3f,0f,0f))

      val tpf = 1.0f / 60.0f
      pSpace.update(tpf * 1.0f)
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.3f,0f,0f))


      pSpace.update(tpf * 1.0f)
            pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.6f,0f,0f))

      playerControl.setWalkDirection(new Vector3f(.0f,0f,8f))
      pSpace.update(tpf * 1.0f)
            pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.6f,0f,8f))


      1 should be equalTo (1)
    }
  }
}
