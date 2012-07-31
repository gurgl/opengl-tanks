package se.bupp.lek.client

import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.input.KeyInput
import com.jme3.input.controls.{AnalogListener, ActionListener, KeyTrigger}
import com.jme3.scene.{Node, Mesh, Spatial, Geometry}
import com.jme3.bounding.{BoundingSphere, BoundingBox}
import se.bupp.lek.server.Model
import MathUtil._
import com.jme3.system.AppSettings
import collection.immutable.{HashSet, Queue}
import se.bupp.lek.client.VisualWorldSimulation._
import collection.JavaConversions
import se.bupp.lek.server.Model._
import com.jme3.math._
import com.jme3.bullet.BulletAppState
import com.jme3.bullet.control.CharacterControl
import com.jme3.renderer.RenderManager
import com.jme3.bullet.BulletAppState.ThreadingType
import com.jme3.audio.AudioNode


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


object PlayerInput {
  val noPlayerInput = noMotion
  type Reorientation = (Vector3f,Quaternion)
}

class PlayerInput(startPosition:Orientation) {
  import PlayerInput._

  var translation:Vector3f = noPlayerInput._1
  var rotation:Quaternion = noPlayerInput._2

  def lastInput:Reorientation = (translation,rotation)


  def resetInput() {
    translation = noPlayerInput._1
    rotation = noPlayerInput._2
  }


  def pollInput() = {
    val r = lastInput
    resetInput()
    r
  }
}

class Client(clientConnectSettings:ClientConnectSettings) extends SimpleApplication {
  import Client._

  var playerIdOpt:Option[Int] = None


  var visualWorldSimulation:VisualWorldSimulation = _

  var audio_gun:AudioNode = _

  def getSpeed = speed

  var seqId = 0
  def createPlayerActionRequest(lastRecordedActionTime:Long, reorientation:Reorientation,projectiles:List[ProjectileFireGO]): Model.PlayerActionRequest = {
      val request: PlayerActionRequest = new PlayerActionRequest


      import JavaConversions.seqAsJavaList
      request.projectilesFired = new java.util.ArrayList[ProjectileFireGO](projectiles)//gameWorld.flushFired)
      request.timeStamp = lastRecordedActionTime
      request.playerId = playerIdOpt.get
      request.seqId = seqId
      seqId += 1
      request.motion = new MotionGO(reorientation._1, reorientation._2, lastRecordedActionTime)
      request
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




  def initAudio() {
    /* gun shot sound is to be triggered by a mouse click. */
    audio_gun = new AudioNode(assetManager, "39023__wildweasel__dual-neutron-disruptor.wav", false);
    audio_gun.setLooping(false);
    audio_gun.setVolume(2);
    rootNode.attachChild(audio_gun);

    /*
    val audio_nature = new AudioNode(assetManager, "33703__yewbic__ambience02.wav", false);
    audio_nature.setLooping(true);  // activate continuous playing
    audio_nature.setPositional(true);
    audio_nature.setLocalTranslation(Vector3f.ZERO.clone());
    audio_nature.setVolume(3);
    rootNode.attachChild(audio_nature);
    audio_nature.play(); // play continuously!

    */
  }


  override def simpleInitApp() {

    settings.setTitle("Tank Showdown")
    setPauseOnLostFocus(false)
    setShowSettings(false)

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))

    val networkState: NetworkGameState = new NetworkGameState(clientConnectSettings)
    stateManager.attach(networkState)

    val playState: PlayState = new PlayState()
    stateManager.attach(playState)



    val bulletAppState = new BulletAppState() {
      override
      def render(rm:RenderManager) {
        if (!active) {

        } else if (threadingType == ThreadingType.PARALLEL) {
          //physicsFuture = executor.submit(parallelPhysicsUpdate);
        } else if (threadingType == ThreadingType.SEQUENTIAL) {
          pSpace.update(tpf * 1.00000001f, 1);
        } else {
        }
      }
    }
    stateManager.attach(bulletAppState);

    bulletAppState.getPhysicsSpace.addTickListener(playState)



    initAudio()

    //playState.setupInput()
  }
  



  override def simpleUpdate(tpf: Float) {


  }


  override def destroyInput() {}

  override def destroy() {
    super.destroy()

    val playState: PlayState = stateManager.getState(classOf[PlayState])


    stateManager.detach( playState)
    playState.cleanup()



    println("destroy " + Client.buffer.toString())
  }
}

object Client {


  var buffer = new StringBuilder
  var spel:Client = _

  def main(arguments: Array[String]): Unit = {
    spel = new Client(new ClientConnectSettings("localhost", 54555, 54777))
    val settings = new AppSettings(true);
    settings.setFrameRate(58)
    settings.setResolution(640,480)
    settings.setTitle("Tank Showdown")
    spel.setSettings(settings)
    spel.setShowSettings(false)
    spel.start()
  }
}