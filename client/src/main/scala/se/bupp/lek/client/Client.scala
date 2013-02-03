package se.bupp.lek.client

import com.jme3.app.{Application, FlyCamAppState, SimpleApplication}
import com.jme3.input.KeyInput
import com.jme3.input.controls.{AnalogListener, ActionListener, KeyTrigger}
import com.jme3.scene.{Node, Mesh, Spatial, Geometry}
import com.jme3.bounding.{BoundingSphere, BoundingBox}
import com.jme3.system.AppSettings
import collection.immutable.{HashSet, Queue}
import se.bupp.lek.client.VisualWorldSimulation._
import collection.JavaConversions
import se.bupp.lek.common.Model._
import com.jme3.math._
import com.jme3.bullet.BulletAppState
import com.jme3.bullet.control.CharacterControl
import com.jme3.renderer.RenderManager
import com.jme3.bullet.BulletAppState.ThreadingType
import com.jme3.audio.AudioNode
import com.jme3.app.state.{AppStateManager, AbstractAppState}
import com.jme3.font.{BitmapFont, BitmapText}



import se.bupp.lek.common.FuncUtil.{Int, RateProbe}
import java.util.{TimerTask, Timer}
import com.jme3.font.plugins.BitmapFontLoader

import util.Random
import se.bupp.lek.common.{MathUtil, Tmp}
import org.slf4j.LoggerFactory
import se.bupp.lek.common.Model._
import se.bupp.lek.common.model.Model._



object PlayerInput {
  val noPlayerInput = MathUtil.noMotion
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

  var playerIdOpt:Option[PlayerId] = None

  val log = LoggerFactory.getLogger(classOf[Client])

  //var visualWorldSimulation:VisualWorldSimulation = _

  var audio_gun:AudioNode = _
  var audio_spawn:AudioNode = _
  var audio_score:AudioNode = _
  var audio_explosion:AudioNode = _

  def getSpeed = speed

  var seqId = 0


  def getSettings = settings

  var lastLastRecordedActionTime:Long = 0

  def createPlayerActionRequest(lastRecordedActionTime:Long, reorientation:Reorientation,projectiles:List[ProjectileFireGO]): PlayerActionRequest = {
      val request: PlayerActionRequest = new PlayerActionRequest



      import JavaConversions.seqAsJavaList
      request.projectilesFired = new java.util.ArrayList[ProjectileFireGO](projectiles)//gameWorld.flushProjectiles)
      request.timeStamp = lastRecordedActionTime
      request.elapsed = if(lastLastRecordedActionTime != 0) lastRecordedActionTime - lastLastRecordedActionTime else 100
      request.playerId = playerIdOpt.get
      request.seqId = seqId
      seqId += 1
      request.motion = new MotionGO(reorientation._1, reorientation._2, 0)

      lastLastRecordedActionTime = lastRecordedActionTime
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


  var serverUpdProbe = new RateProbe("Messages ",1000L,log)

  var messages = collection.mutable.Queue[AnyRef]()

  def postMessage(b:OrderedMessage) {

    //log.info("MESSA")
    serverUpdProbe.tick()
    messages.enqueue(b)
  }

  def initAudio() {
    /* gun shot sound is to be triggered by a mouse click. */
    audio_gun = new AudioNode(assetManager, "gun.wav", false);
    audio_gun.setLooping(false);
    audio_gun.setVolume(2);
    rootNode.attachChild(audio_gun);
    audio_explosion = new AudioNode(assetManager, "explosion.wav", false);
    audio_explosion.setLooping(false);
    audio_explosion.setVolume(0.3f);
    rootNode.attachChild(audio_explosion);

    audio_spawn = new AudioNode(assetManager, "spawn.wav", false);
    audio_spawn.setLooping(false);
    audio_spawn.setVolume(2);
    rootNode.attachChild(audio_spawn);
    audio_score = new AudioNode(assetManager, "score.wav", false);
    audio_score.setLooping(false);
    audio_score.setVolume(10);
    rootNode.attachChild(audio_score);


    val audio_nature = new AudioNode(assetManager, "music2.ogg", true);

    //audio_nature.setLooping(true);  // activate continuous playing
    //audio_nature.setPositional(true);
    //audio_nature.setLocalTranslation(Vector3f.ZERO.clone());
    audio_nature.setVolume(1);
    rootNode.attachChild(audio_nature);
    audio_nature.play(); // play continuously!


  }

  var gameFont:BitmapFont = _

  override def simpleInitApp() : Unit = try {
    log.debug("Init appp start")
    java.util.logging.Logger.getLogger("com.jme3").setLevel(java.util.logging.Level.WARNING);
    settings.setTitle("Tank Showdown")
    setPauseOnLostFocus(false)
    setShowSettings(false)
    setDisplayStatView(false)
    setDisplayFps(false)


    try {
      gameFont = assetManager.loadFont("c64font.fnt");

    } catch {
      case ex =>
      log.error("Unable to load font: " + ex);
      System.exit(1);
    }

    //setDisplayStatView(false)
    //setDisplayFps(false)

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))

    val networkState: NetworkGameState = new NetworkGameState(clientConnectSettings)
    stateManager.attach(networkState)


    val bulletAppState = createBulletAppState
    stateManager.attach(bulletAppState);

    stateManager.attach(new MessageState("COMMODORE 64 press play on tape"))
    //val playState: PlayState = new PlayState()
    //stateManager.attach(playState)

    //bulletAppState.getPhysicsSpace.addTickListener(playState)


    initAudio()
    //playState.setupInput()
    log.debug("App initialized")
  } catch { case e:Exception => e.printStackTrace }


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
  class DoRoundOver()
  class PostGameOver()
  class DoGameOver()

  override def simpleUpdate(tpf: Float) {


    //if (gotGO ) println("Start please")
    val toHandle  = messages.dequeueFirst( p => true)

    toHandle.foreach {
      m =>
        log.debug("MESS " + m.getClass)
        m match {
          case x:DoRoundOver =>
            val state: PlayState = getStateManager.getState(classOf[PlayState])
            if (state != null) {

              state.cleanForReuse()
              log.info("Round over received")
              state.setEnabled(false)
            }
            getStateManager.attach(new MessageState("Round Over"))

        case x:RoundOverRequest =>
          new DoRoundOver() +=: messages

        case x:StartGameRequest =>

          var state: PlayState = getStateManager.getState(classOf[PlayState])
          log.info("Start game received - checking")

          if (state != null) {
            log.info("Waiting for state to initialize")
            if (state.isInitialized) {
              getStateManager.detach(state)
            }
            x +=: messages
            //state.setEnabled(false)
          } else {
            log.info("Start game received - starting new game")
            state = new PlayState()
            getStateManager.attach(state)
            getStateManager.attach(createBulletAppState)

          }

          var mstate = getStateManager.getState(classOf[MessageState])
          if (mstate != null) {
            getStateManager.detach(mstate)
          }

        case x:PostGameOver =>

          val state: PlayState = getStateManager.getState(classOf[PlayState])
          log.info("detaching")
          getStateManager.detach(state)

          val bState = getStateManager.getState(classOf[BulletAppState])
          getStateManager.detach(bState)
          log.info("GameOver done")


        case x:DoGameOver =>
            val state: PlayState = getStateManager.getState(classOf[PlayState])
            if (state != null) {
              log.info("GAme over received")
              state.setEnabled(false)
            }
            //if (mState == null) {
            getStateManager.attach(new MessageState("Game Over"))

            new Timer().schedule(new TimerTask {
              def run() {
                new PostGameOver() +=: messages
              }
            },3000L)

        case x:GameOverRequest =>
          new DoGameOver() +=: messages

          //log.info("GameOver done")

          //}
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
        case _ => println("WTF")
      }
    }
    /*messages = messages match {
      case x :: tail => if (keepHead) messages else tail
      case Nil => Nil
    }*/


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

      val hudText = new BitmapText(application.gameFont, false);
      hudText.setSize(application.gameFont.getCharSet().getRenderedSize());      // font size
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

  val clockOffset = if(Tmp.doSimulateClockIndependence) 1000* 50 - Random.nextInt(1000*100) else 0
  def clock() = System.currentTimeMillis() + clockOffset

  def getHostSettings(args: Array[String]) = {

    (System.getProperty("gameHost"),System.getProperty("gamePortTCP"), System.getProperty("gamePortUDP"), System.getProperty("playerInfo")) match {
      case (h:String,Int(u),Int(t), p:String) => (h,u,t,p)
      case _ => args match {
        case Array(h,Int(u),Int(t),p) => (h,u,t,p)
        case _ => ("localhost", 54555, 54777, "none set")
      }

    }
  }
  def main(arguments: Array[String]): Unit = {

    spel = new Client((ClientConnectSettings.apply _).tupled(getHostSettings(arguments)))
    val settings = new AppSettings(true);
    settings.setFrameRate(58)
    settings.setResolution(640,480)
    settings.setTitle("Tank Showdown")



    spel.setSettings(settings)
    spel.setShowSettings(false)
    spel.start()
  }
}