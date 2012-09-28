package se.bupp.lek.client


import management.ManagementFactory
import com.jme3.app.state.{AppStateManager, AbstractAppState}
import com.jme3.app.Application
import collection.immutable.Queue
import collection.immutable.Queue._
import MathUtil._
import com.jme3.app.SimpleApplication._
import com.jme3.scene.{Spatial, SceneGraphVisitor, Node}
import com.jme3.export.Savable
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import collection.{JavaConversions, immutable}
import JavaConversions.asScalaBuffer
import scala.Option
import se.bupp.lek.server.Model._
import se.bupp.lek.server.{Server, Model}
import com.jme3.bullet.{BulletAppState, PhysicsSpace, PhysicsTickListener}
import java.util
import com.jme3.input.controls.{KeyTrigger, ActionListener, AnalogListener}
import com.jme3.bullet.control.CharacterControl
import com.jme3.input.KeyInput
import se.bupp.lek.client.SceneGraphWorld.SceneGraphNodeKeys
import org.apache.log4j.Logger
import se.bupp.lek.client.SceneGraphWorld.SceneGraphNodeKeys._
import scala.Some
import se.bupp.lek.client.VisualWorldSimulation.PlayerScore
import com.jme3.font.{Rectangle, BitmapFont, BitmapText}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/29/12
 * Time: 12:32 PM
 * To change this template use File | Settings | File Templates.
 */

class PlayState() extends AbstractAppState with PhysicsTickListener {


  val log = Logger.getLogger(classOf[Client])

  var visualWorldSimulation:VisualWorldSimulation = _



  val rotSpeed = 2.0f

  //var predictions:Map[Long,Map[OwnedGameObjectId,Vector3f]] = Map[Long,Map[OwnedGameObjectId,Vector3f]]()


  var playerInput:PlayerInput = _

  var gameApp:Client = _

  lazy val worldUpdater:WorldUpdater = {
    val state: NetworkGameState = gameApp.getStateManager.getState(classOf[NetworkGameState])
    new state.ServerWorldUpdater(visualWorldSimulation)
  }

  var lastUpdate:Option[(Long,Reorientation)] = None
  var contentNode:Node = _
  var needsReenableRefresh = false


  override def setEnabled(enabled: Boolean) {
    super.setEnabled(enabled)
    if(enabled) needsReenableRefresh = true
  }

  override def update(tpf: Float) {

    // MOVE to after?
    val (pos,rot) = visualWorldSimulation.getCamPosition
    gameApp.getCamera.setFrame(pos,rot)

    if(gameApp.playerIdOpt.isEmpty) return

    if(!isEnabled) return
    if (needsReenableRefresh) {
      logMessageQueue = Queue.empty
      contentNode.detachAllChildren()
      needsReenableRefresh = false
    }

    if (logNeedsRepaint) paintLog(logMessageQueue.toList)

    val simTime = System.currentTimeMillis()

    var input = playerInput.pollInput()

    worldUpdater.processInput(input,lastUpdate)

    val worldToPaintOpt = worldUpdater.generateGameWorld(simTime)


    //visualWorldSimulation.handleServerStateReplication()
    worldToPaintOpt.foreach( world => visualWorldSimulation.updateGameWorld(this, world , input) )

    lastUpdate.foreach {
        case _ => worldUpdater.postUpdate(simTime)
    }

    lastUpdate = Some((simTime, input))

  }


  /*def generateLocalGameWorld(simTime: Long,currentGameWorldUpdates:Queue[Model.ServerGameWorld]): (Set[Model.AbstractOwnedGameObject with Savable] , Model.ServerGameWorld) = {
    val prediction: Set[AbstractOwnedGameObject with Savable] = gameApp.visualWorldSimulation.calculatePrediction(simTime, currentGameWorldUpdates, gameApp.playerIdOpt.get)

    (prediction, currentGameWorldUpdates.last)
  }*/



  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Client]

    val state: NetworkGameState = gameApp.getStateManager.getState(classOf[NetworkGameState])

    contentNode = new Node(NODE_NAME)
    gameApp.getGuiNode.attachChild(contentNode)


    val playerStartPosition = new Orientation(Vector3f.ZERO.clone().setY(0.5f), Quaternion.IDENTITY.clone())

    val bulletAppState = gameApp.getStateManager.getState(classOf[BulletAppState])
    visualWorldSimulation = new VisualWorldSimulation(gameApp.getRootNode,gameApp.getAssetManager,() => gameApp.playerIdOpt,playerInput, gameApp.getViewPort, bulletAppState);
    visualWorldSimulation.init(playerStartPosition)
    playerInput = new PlayerInput(playerStartPosition)

    setupInput()
    initialized = true
    printSceneGraph(visualWorldSimulation.rootNode)
  }

  val lock : AnyRef = new Object()




  def prePhysicsTick(p1: PhysicsSpace, tpf: Float) {

  }

  def physicsTick(p1: PhysicsSpace, p2: Float) {

  }

  val actionListener = new AnalogListener() with ActionListener {

    def onAction(name:String, value:Boolean, tpf:Float) {

      if (name.equals("Fire")) {
        if(value == true) {
          val p = visualWorldSimulation.fireProjectile()
          gameApp.audio_gun.playInstance()
          //rootNode.attachChild(p)
        }
      } else if (name.equals("Pause")) {
        if(value == true) {
          val state: PlayState = gameApp.getStateManager.getState(classOf[PlayState])
          state.setEnabled(!state.isEnabled)
          log.info("Paused " + state.isEnabled)
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
          playerInput.translation = v.getColumn(0).mult(gameApp.getSpeed *tpf)

        case "Back" =>

          val v = visualWorldSimulation.player.getLocalRotation.toRotationMatrix;
          playerInput.translation = v.getColumn(0).mult(-gameApp.getSpeed *tpf)
        case _ =>


      }
    }
  };


  val mappings = Map("Left" -> new KeyTrigger(KeyInput.KEY_A),
    "Right" -> new KeyTrigger(KeyInput.KEY_D),
    "Forward" -> new KeyTrigger(KeyInput.KEY_W),
    "Back" ->  new KeyTrigger(KeyInput.KEY_S),
    "Fire" -> new KeyTrigger(KeyInput.KEY_SPACE),
    "Pause" ->  new KeyTrigger(KeyInput.KEY_P)
  )

  def setupInput() {

    //import collection.JavaConversions.

    gameApp.getInputManager.addListener(actionListener, mappings.keys.toList:_*)

    mappings.foreach {
      case (key, trigger) => gameApp.getInputManager.addMapping(key,trigger)
    }
  }
  def cleanForReuse() {
    unspawnAllPlayers()
    //contentNode.detachAllChildren()
  }
  def unspawnAllPlayers() {
    visualWorldSimulation.playerDead = true
    visualWorldSimulation.cleanNodes(List(Player,Projectiles,Enemies,Effects))

  }

  /*def unspawnAllGameObject() {
    import SceneGraphNodeKeys._
    visualWorldSimulation.removeNodes(List(Player,Projectiles,Enemies,Effects))
  }*/

  override def cleanup() {
    //printSceneGraph(visualWorldSimulation.rootNode)
    /*visualWorldSimulation.rootNode.depthFirstTraversal(new SceneGraphVisitor {
      def visit(p1: Spatial) {

        println("aftercleanup " + p1.getName + " " + p1.getClass.getName)
      }
    })*/

    super.cleanup()
    contentNode.detachAllChildren()
    gameApp.getGuiNode.detachChild(contentNode)

    visualWorldSimulation.destroy()
    gameApp.getInputManager.removeListener(actionListener)
    mappings.foreach {
      case (key, trigger) => gameApp.getInputManager.deleteMapping(key)
    }
    //printSceneGraph(visualWorldSimulation.rootNode)
    /*visualWorldSimulation.rootNode.depthFirstTraversal(new SceneGraphVisitor {
      def visit(p1: Spatial) {

        println("aftercleanup " + p1.getName + " " + p1.getClass.getName)
      }
    })*/
  }

  def printSceneGraph(s:Spatial, level:Int = 0, indent:Int = 0, first:Boolean = true) {
    var pCnt = 0

    //def ident() = { val res = if(pCnt == 0) "" else (" " * indent) ; pCnt = pCnt + 1 ; res }

    def printIt(pre:String, str:String) = {if(first) print(pre + str) else println(" " * indent + pre + str)}
    s match {
      case n:Node =>
        print((if(first) "" else " " * indent ) + "+" +  n.getName)
        Option(n.getChildren).getOrElse(new java.util.ArrayList).toList match {
        case Nil => println("")
        case head :: tail =>
          //print((if(first) "" else " " * indent ) + "+" +  n.getName)

          var nextLevel: Int = level + 1
          var indentNext: Int = indent + n.getName.size + 1
          printSceneGraph(head,nextLevel, indentNext,true)
          tail.foreach( c => printSceneGraph(c,nextLevel, indentNext,false) )
      }
        //val max = children.foldLeft(0) { case (m, c) => math.max(m,if(c.getName !=null) c.getName.length else 0) }




        //println("")
      case s:Spatial => {if(first) println("-" + s.getName) else println(" " * indent + "-" + s.getName)}
    }
    ()
  }

  var logMessageQueue = Queue.empty[String]
  var logNeedsRepaint = false
  def handleScoreMessage(sm:PlayerScore) {
    logMessageQueue = logMessageQueue.enqueue(sm.of + " killed " + sm.victim)
    if (logMessageQueue.size > 3) {
      logMessageQueue = logMessageQueue.drop(1)
      //logMessageQueue = newQueue
    }

    logNeedsRepaint = true
  }

  val LOG_BOX_WIDTH = 200

  val LOG_BOX_HEIGHT = 150

  val NODE_NAME = "GuiNode"
  //var content:Node = _

  def paintLog(str:List[String]) {
    val s = str.reverse.mkString("\n")
    log.debug("updating scores : " + s)

    contentNode.detachAllChildren()

    val hudText = new BitmapText(gameApp.getGuiFont, false);
    hudText.setSize(gameApp.getGuiFont.getCharSet().getRenderedSize());      // font size
    hudText.setColor(ColorRGBA.White);                             // font color
    var logBox: Rectangle = new com.jme3.font.Rectangle(0, 0, LOG_BOX_WIDTH, LOG_BOX_HEIGHT)
    hudText.setBox(logBox);
    //hudText.setBox(new com.jme3.font.Rectangle(0,0,settings.getWidth , settings.getHeight))
    hudText.setAlignment(BitmapFont.Align.Left);
    hudText.setVerticalAlignment(BitmapFont.VAlign.Top);
    hudText.setText(s);             // the text
    //hudText.setLocalTranslation(settings.getWidth/2 , settings.getHeight/2 , 0); // position
    hudText.setLocalTranslation(gameApp.getSettings.getWidth - LOG_BOX_WIDTH, LOG_BOX_HEIGHT, 0); // position
    contentNode.attachChild(hudText)
    logNeedsRepaint = false
  }

}
