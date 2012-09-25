package se.bupp.lek.client

import com.jme3.app.{Application, FlyCamAppState, SimpleApplication}
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
import com.jme3.app.state.{AppStateManager, AbstractAppState}
import com.jme3.font.{BitmapFont, BitmapText}

import org.apache.log4j.{Logger, PropertyConfigurator}


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

  val log = Logger.getLogger(classOf[Client])

  //var visualWorldSimulation:VisualWorldSimulation = _

  var audio_gun:AudioNode = _

  def getSpeed = speed

  var seqId = 0

  def getSettings = settings
  def createPlayerActionRequest(lastRecordedActionTime:Long, reorientation:Reorientation,projectiles:List[ProjectileFireGO]): Model.PlayerActionRequest = {
      val request: PlayerActionRequest = new PlayerActionRequest


      import JavaConversions.seqAsJavaList
      request.projectilesFired = new java.util.ArrayList[ProjectileFireGO](projectiles)//gameWorld.flushProjectiles)
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



  var messages:Option[AnyRef] = None
  def postMessage(b:AnyRef) {
    messages = Some(b)
  }

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
    java.util.logging.Logger.getLogger("com.jme3").setLevel(java.util.logging.Level.WARNING);
    settings.setTitle("Tank Showdown")
    setPauseOnLostFocus(false)
    setShowSettings(false)

    //setDisplayStatView(false)
    //setDisplayFps(false)

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))

    val networkState: NetworkGameState = new NetworkGameState(clientConnectSettings)
    stateManager.attach(networkState)


    val bulletAppState = createBulletAppState
    stateManager.attach(bulletAppState);

    stateManager.attach(new MessageState("Tja"))
    //val playState: PlayState = new PlayState()
    //stateManager.attach(playState)

    //bulletAppState.getPhysicsSpace.addTickListener(playState)

    initAudio()

    //playState.setupInput()
  }


  def createBulletAppState: BulletAppState {def render(rm: RenderManager): Unit} = {
    new BulletAppState() {
      override
      def render(rm: RenderManager) {
        if (!active) {

        } else if (threadingType == ThreadingType.PARALLEL) {
          //physicsFuture = executor.submit(parallelPhysicsUpdate);
        } else if (threadingType == ThreadingType.SEQUENTIAL) {
          pSpace.update(tpf * 1.00000001f, 1);
        } else {
        }
      }
    }
  }

  override def simpleUpdate(tpf: Float) {

    //if (gotGO ) println("Start please")
    messages.foreach  {
      m =>
        log.debug("MESS " + m.getClass)
        m match {
        case x:RoundOverRequest =>
          val state: PlayState = getStateManager.getState(classOf[PlayState])
          if (state != null) {
            state.unspawnAllPlayers()
            log.info("Round over received")
            state.setEnabled(false)
          }
          messages = None
          getStateManager.attach(new MessageState("Round Over"))
        case x:StartGameRequest =>

          var state: PlayState = getStateManager.getState(classOf[PlayState])
          log.info("Start game received - checking")

          if (state != null) {
            log.info("Waiting for state to initialize")
            if (state.isInitialized) {
              getStateManager.detach(state)
            }
            //state.setEnabled(false)
          } else {
            log.info("Start game received - starting new game")
            state = new PlayState()
            getStateManager.attach(state)
            getStateManager.attach(createBulletAppState)
            messages = None

          }

          var mstate = getStateManager.getState(classOf[MessageState])
          if (mstate != null) {
            getStateManager.detach(mstate)
          }

        case x:GameOverRequest =>
          val state: PlayState = getStateManager.getState(classOf[PlayState])
          if (state != null) {
            log.info("GAme over received")
            state.setEnabled(false)
            log.info("detaching")
            getStateManager.detach(state)

            val bState = getStateManager.getState(classOf[BulletAppState])
            getStateManager.detach(bState)
          }
          //if (mState == null) {
          getStateManager.attach(new MessageState("Game Over"))

          log.info("GameOver done")

          //}

          messages = None
          //getStateManager.attach(new MessageState("Game Over"))

        case x:StartRoundRequest =>
          val state: PlayState = getStateManager.getState(classOf[PlayState])
          if (state != null) {
            log.info("Start round received")
            state.setEnabled(true)
          }

          var mstate = getStateManager.getState(classOf[MessageState])
          if (mstate != null) {
            getStateManager.detach(mstate)
          }
          messages = None
        case _ => println("WTF")
      }
    }


  }


  override def destroyInput() {}

  override def destroy() {
    super.destroy()

    val playState: PlayState = stateManager.getState(classOf[PlayState])

    if (playState != null) {
      stateManager.detach( playState)
      //playState.cleanup()
    }


    println("destroy " + Client.buffer.toString())
  }

  def getGuiFont = guiFont
}


class MessageState(s:String) extends AbstractAppState {
  var content:Node = _

  var application:Client = _

  var refreshed = false

  var settings:AppSettings = _

  val NODE_NAME = "messageNodeName"

  override def initialize(stateManager: AppStateManager, app: Application) {
    super.initialize(stateManager, app)
    //app.asInstanceOf[SimpleApplication].getGuiNode.attachChild()
    application = app.asInstanceOf[Client]
    settings = application.getSettings
  }

  import JavaConversions.asScalaBuffer
  override def update(tpf: Float) {
    super.update(tpf)

    if (!refreshed ) {
      val contentNode:Node = Option(application.getGuiNode.getChild(NODE_NAME).asInstanceOf[Node]).getOrElse {
          content = new Node(NODE_NAME)
          application.getGuiNode.attachChild(content)
          content
      }
      contentNode.getChildren.toList.foreach(_.removeFromParent())

      val hudText = new BitmapText(application.getGuiFont, false);
      hudText.setSize(application.getGuiFont.getCharSet().getRenderedSize());      // font size
      hudText.setColor(ColorRGBA.White);                             // font color
      hudText.setBox(new  com.jme3.font.Rectangle(0, 0, settings.getWidth, settings.getHeight));
      //hudText.setBox(new com.jme3.font.Rectangle(0,0,settings.getWidth , settings.getHeight))
      hudText.setAlignment(BitmapFont.Align.Center);
      hudText.setVerticalAlignment(BitmapFont.VAlign.Center);
      hudText.setText(s);             // the text
      //hudText.setLocalTranslation(settings.getWidth/2 , settings.getHeight/2 , 0); // position
      hudText.setLocalTranslation(0 , settings.getHeight , 0); // position
      contentNode.attachChild(hudText)
      refreshed = true
    }

  }

  override def cleanup() {
    super.cleanup()

    application.getGuiNode.detachChild(content)
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