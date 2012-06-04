package se.bupp.lek.client

import com.jme3.scene.shape.Box
import com.jme3.material.Material
import com.jme3.font.BitmapText
import com.jme3.light.DirectionalLight
import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.input.KeyInput
import com.jme3.input.controls.{AnalogListener, ActionListener, KeyTrigger}
import com.jme3.renderer.Camera
import com.jme3.math.{Matrix3f, Matrix4f, Quaternion, Vector3f}
import com.jme3.asset.ModelKey
import com.jme3.scene.{Node, Mesh, Spatial, Geometry}
import com.jme3.bounding.{BoundingSphere, BoundingBox}
import com.esotericsoftware.kryonet.{Connection, Listener, Client}
import management.ManagementFactory
import se.bupp.lek.server.Server
import MathUtil._
import com.jme3.system.AppSettings
import collection.immutable.{HashSet, Queue}
import com.jme3.export.Savable
import se.bupp.lek.client.ClientWorld
import se.bupp.lek.server.Server.{ProjectileGO, PlayerGO, AbstractOwnedGameObject}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/16/12
 * Time: 11:49 PM
 * To change this template use File | Settings | File Templates.
 */


object MathUtil {
  val noRotation = Quaternion.ZERO.fromAngleNormalAxis(0f,Vector3f.UNIT_XYZ)

}



class Client extends SimpleApplication {
  import Client._


  //val speed = 1.0f
  val rotSpeed = 2.0f

  var playerIdOpt:Option[Int] = None

  val noPlayerInput = Pair(Vector3f.ZERO, noRotation)
  var playerInput:(Vector3f,Quaternion) = noPlayerInput

  var gameWorld:ClientWorld = _


  val actionListener = new AnalogListener() with ActionListener {

    def onAction(name:String, value:Boolean, tpf:Float) {

      if (name.equals("Fire")) {
        if(value == true) {
          val p = gameWorld.projectileHandler.fireProjectile(gameWorld.player.getLocalTranslation,gameWorld.player.getLocalRotation)
          //rootNode.attachChild(p)
        }
      }
    }

    def onAnalog(name:String, value:Float, tpf:Float) {

      name match {
        case "Left" =>

          playerInput = (playerInput._1, new Quaternion().fromAngleNormalAxis(rotSpeed * tpf,Vector3f.UNIT_Y))

        case "Right" =>
          //player.rotate(0, -rotSpeed * tpf, 0);
          //val rot = new Quaternion(0f, math.sin(angle/2d).toFloat, 0f, math.cos(angle/2d).toFloat)

          playerInput= (playerInput._1, new Quaternion().fromAngleNormalAxis(-rotSpeed * tpf,Vector3f.UNIT_Y))

        case "Forward" =>

          val v = gameWorld.player.getLocalRotation.toRotationMatrix;
          playerInput= (v.getColumn(0).mult(speed*tpf), playerInput._2)

        case "Back" =>

          val v = gameWorld.player.getLocalRotation.toRotationMatrix;
          playerInput= (v.getColumn(0).mult(-speed*tpf), playerInput._2)
        case "Fire" =>
      }
    }
  };

  def setupInput() {

    //import collection.JavaConversions.
    inputManager.addListener(actionListener, List("Left","Right", "Forward", "Back", "Fire"):_*)

    inputManager.addMapping("Left",   new KeyTrigger(KeyInput.KEY_A));
    inputManager.addMapping("Right",  new KeyTrigger(KeyInput.KEY_D));
    inputManager.addMapping("Forward", new KeyTrigger(KeyInput.KEY_W));
    inputManager.addMapping("Back",  new KeyTrigger(KeyInput.KEY_S));
    inputManager.addMapping("Fire",  new KeyTrigger(KeyInput.KEY_SPACE));
  }

  /*def attachStatsDisplay() {
    guiNode.detachAllChildren();
    guiFont = assetManager.loadFont("Interface/Fonts/Default.fnt");
    val helloText = new BitmapText(guiFont, false);
    helloText.setSize(guiFont.getCharSet().getRenderedSize());
    helloText.setText("Hello World");
    helloText.setLocalTranslation(300, helloText.getLineHeight(), 0);
    guiNode.attachChild(helloText);
  }*/

  override def simpleInitApp() {

    settings.setTitle("Tank Showdown")
    setPauseOnLostFocus(false)
    setShowSettings(false)

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))
    stateManager.attach(new NetworkState)

    gameWorld = new ClientWorld(rootNode,assetManager,() => playerIdOpt)
    gameWorld.init

    setupInput()
  }
  

  override def simpleUpdate(tpf: Float) {

    val (pos,rot) = gameWorld.getCamPosition
    getCamera.setFrame(pos,rot)

    playerInput = noPlayerInput
  }


}

object Client {

  def main(arguments: Array[String]): Unit = {
    val spel = new Client()
    val settings = new AppSettings(true);
    settings.setResolution(640,480)
    settings.setTitle("Tank Showdown")
    spel.setSettings(settings)
    spel.setShowSettings(false)
    spel.start()
  }
}