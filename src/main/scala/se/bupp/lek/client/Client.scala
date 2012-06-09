package se.bupp.lek.client

import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.input.KeyInput
import com.jme3.input.controls.{AnalogListener, ActionListener, KeyTrigger}
import com.jme3.scene.{Node, Mesh, Spatial, Geometry}
import com.jme3.bounding.{BoundingSphere, BoundingBox}
import se.bupp.lek.server.Server
import MathUtil._
import com.jme3.system.AppSettings
import collection.immutable.{HashSet, Queue}
import se.bupp.lek.client.VisualWorldSimulation._
import collection.JavaConversions
import se.bupp.lek.server.Server._
import com.jme3.math._
import com.jme3.bullet.BulletAppState
import com.jme3.bullet.control.CharacterControl


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/16/12
 * Time: 11:49 PM
 * To change this template use File | Settings | File Templates.
 */


object MathUtil {
  def noRotation = Quaternion.ZERO.fromAngleNormalAxis(0f,Vector3f.UNIT_XYZ.clone())

  def noMotion:Reorientation = (Vector3f.ZERO.clone(), noRotation)

}


object PlayerInput {
  val noPlayerInput = noMotion
  type Reorientation = (Vector3f,Quaternion)
  val LocalInputLogSize = 20
}

class PlayerInput(startPosition:Orientation) {
  import PlayerInput._
  var translation:Vector3f = noPlayerInput._1
  var rotation:Quaternion = noPlayerInput._2

  def lastInput:Reorientation = (translation,rotation)
  var accTranslation = Vector3f.ZERO.clone()
  var accRotation = noRotation

  var saved = Queue.empty[(Long, Orientation, Reorientation)] + (System.currentTimeMillis(),startPosition, noMotion)

  val lock:AnyRef = new Object
  def flushAccumulated() : Reorientation = {
    val r = (accTranslation,accRotation)
    accTranslation = Vector3f.ZERO.clone()
    accRotation = noRotation
    r
  }

  def resetInput() {
    translation = noPlayerInput._1
    rotation = noPlayerInput._2
  }

  def saveReorientation(timeStamp:Long, reorientation:Reorientation) {
    lock.synchronized {
      while(saved.size >= LocalInputLogSize) {
        saved = saved.dequeue._2
      }

      val orientation = saved.last._2.reorientate(reorientation)
      saved =  saved + (timeStamp, orientation, reorientation)

      accTranslation = accTranslation.add(reorientation._1)
      accRotation = reorientation._2.mult(accRotation)
    }
  }
  def saveInput(timeStamp:Long) {
    saveReorientation(timeStamp, lastInput)
    resetInput()

  }
}

class Client extends SimpleApplication {
  import Client._

  var playerIdOpt:Option[Int] = None

  var playerInput:PlayerInput = _

  var gameWorld:VisualWorldSimulation = _

  val actionListener = new AnalogListener() with ActionListener {

    def onAction(name:String, value:Boolean, tpf:Float) {

      if (name.equals("Fire")) {
        if(value == true) {
          val p = gameWorld.fireProjectile(gameWorld.player.getLocalTranslation,gameWorld.player.getLocalRotation)
          //rootNode.attachChild(p)
        }
      }
    }

    def onAnalog(name:String, value:Float, tpf:Float) {

      name match {
        case "Left" =>

          playerInput.rotation = new Quaternion().fromAngleNormalAxis(rotSpeed * tpf,Vector3f.UNIT_Y)

        case "Right" =>
          //player.rotate(0, -rotSpeed * tpf, 0);
          //val rot = new Quaternion(0f, math.sin(angle/2d).toFloat, 0f, math.cos(angle/2d).toFloat)

          playerInput.rotation = new Quaternion().fromAngleNormalAxis(-rotSpeed * tpf,Vector3f.UNIT_Y)

        case "Forward" =>

          val v = gameWorld.player.getLocalRotation.toRotationMatrix;
          playerInput.translation = v.getColumn(0).mult(speed*tpf)

        case "Back" =>

          val v = gameWorld.player.getLocalRotation.toRotationMatrix;
          playerInput.translation = v.getColumn(0).mult(-speed*tpf)
        case "Fire" =>
      }
    }
  };

  var seqId = 0
  def createPlayerActionRequest(lastRecordedActionTime:Long, reorientation:Reorientation,projectiles:List[ProjectileFireGO]): Server.PlayerActionRequest = {
      val request: PlayerActionRequest = new PlayerActionRequest


      import JavaConversions.seqAsJavaList
      request.projectilesFired = new java.util.ArrayList[ProjectileFireGO](projectiles)//gameWorld.purgeFired)
      request.timeStamp = lastRecordedActionTime
      request.playerId = playerIdOpt.get
      request.seqId = seqId
      seqId += 1
      request.motion = new MotionGO(reorientation._1, reorientation._2)
      request
    }

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

    val bulletAppState = new BulletAppState();
    stateManager.attach(bulletAppState);
    gameWorld = new VisualWorldSimulation(rootNode,assetManager,() => playerIdOpt,playerInput, viewPort, bulletAppState);

    val playerStartPosition = new Orientation(Vector3f.ZERO.clone().setY(0.5f), Quaternion.IDENTITY.clone())
    gameWorld.init(playerStartPosition)

    playerInput = new PlayerInput(playerStartPosition)


    setupInput()
  }
  

  override def simpleUpdate(tpf: Float) {

    val (pos,rot) = gameWorld.getCamPosition
    getCamera.setFrame(pos,rot)

  }


  override def destroyInput() {}

  override def destroy() {
    super.destroy()

    println("destroy " + Client.buffer.toString())
  }
}

object Client {

  val rotSpeed = 2.0f

  var buffer = new StringBuilder
  var spel:Client = _

  def main(arguments: Array[String]): Unit = {
    spel = new Client()
    val settings = new AppSettings(true);
    settings.setResolution(640,480)
    settings.setTitle("Tank Showdown")
    spel.setSettings(settings)
    spel.setShowSettings(false)
    spel.start()
  }
}