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

class Client(tcpPort:Int, udpPort:Int) extends SimpleApplication {
  import Client._

  var playerIdOpt:Option[Int] = None

  var playerInput:PlayerInput = _

  var visualWorldSimulation:VisualWorldSimulation = _

  var audio_gun:AudioNode = _

  val actionListener = new AnalogListener() with ActionListener {

    def onAction(name:String, value:Boolean, tpf:Float) {

      if (name.equals("Fire")) {
        if(value == true) {
          val p = visualWorldSimulation.fireProjectile(visualWorldSimulation.player.getControl(classOf[CharacterControl]).getPhysicsLocation.clone(),visualWorldSimulation.player.getLocalRotation)
          audio_gun.playInstance()
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

          val v = visualWorldSimulation.player.getLocalRotation.toRotationMatrix;
          playerInput.translation = v.getColumn(0).mult(speed*tpf)

        case "Back" =>

          val v = visualWorldSimulation.player.getLocalRotation.toRotationMatrix;
          playerInput.translation = v.getColumn(0).mult(-speed*tpf)
        case "Fire" =>
      }
    }
  };

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
    val networkState: NetworkState = new NetworkState(tcpPort, udpPort)
    stateManager.attach(networkState)


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

    bulletAppState.getPhysicsSpace.addTickListener(networkState)

    visualWorldSimulation = new VisualWorldSimulation(rootNode,assetManager,() => playerIdOpt,playerInput, viewPort, bulletAppState);

    val playerStartPosition = new Orientation(Vector3f.ZERO.clone().setY(0.5f), Quaternion.IDENTITY.clone())
    visualWorldSimulation.init(playerStartPosition)

    playerInput = new PlayerInput(playerStartPosition)

    initAudio()

    setupInput()
  }
  



  override def simpleUpdate(tpf: Float) {

    val (pos,rot) = visualWorldSimulation.getCamPosition
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
    spel = new Client(54555, 54777)
    val settings = new AppSettings(true);
    settings.setFrameRate(58)
    settings.setResolution(640,480)
    settings.setTitle("Tank Showdown")
    spel.setSettings(settings)
    spel.setShowSettings(false)
    spel.start()
  }
}