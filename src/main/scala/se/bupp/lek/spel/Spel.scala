package se.bupp.lek.spel

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
import com.jme3.bullet.util.CollisionShapeFactory
import com.jme3.bullet.collision.shapes.{BoxCollisionShape, CollisionShape}
import com.jme3.bounding.{BoundingSphere, BoundingBox}
import com.esotericsoftware.kryonet.{Connection, Listener, Client}
import management.ManagementFactory
import se.bupp.lek.spel.GameServer._
import collection.immutable.Queue
import MathUtil._
import com.jme3.system.AppSettings


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



class Spel extends SimpleApplication {

  var player:Spatial = _

  //val speed = 1.0f
  val rotSpeed = 2.0f

  var playerIdOpt:Option[Int] = None

  //var enemies:Spatial = _
  var projectileHandler : ProjectileHandler = _
  var mat_default : Material = _

  val noPlayerInput = Pair(Vector3f.ZERO, noRotation)
  var playerInput:(Vector3f,Quaternion) = noPlayerInput


  val actionListener = new AnalogListener() with ActionListener {

    def onAction(name:String, value:Boolean, tpf:Float) {

      if (name.equals("Fire")) {
        if(value == true) {
          val p = projectileHandler.fire(player.getLocalTranslation,player.getLocalRotation.getRotationColumn(0).normalize())
          rootNode.attachChild(p)
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

          val v = player.getLocalRotation.toRotationMatrix;
          playerInput= (v.getColumn(0).mult(speed*tpf), playerInput._2)

        case "Back" =>

          val v = player.getLocalRotation.toRotationMatrix;
          playerInput= (v.getColumn(0).mult(-speed*tpf), playerInput._2)
        case "Fire" =>
      }
    }
  };


  
  def getCamPosition() : (Vector3f,Quaternion) = {

    val pos = player.getLocalTranslation
    val camPos = pos.add(Vector3f.UNIT_XYZ.mult(5))
    //val camPos = pos.add(Vector3f.UNIT_Z.mult(5))
    val dir = pos.subtract(camPos).normalize()
    //Quaternion.IDENTITY.fromAxes(,,dir)

    val rot = Quaternion.IDENTITY
    rot.lookAt(dir, new Vector3f(0, 1, 0))
    (camPos,rot)
  }

  def materializeLevel() {
    // Create a wall with a simple texture from test_data
    val box = new Box(Vector3f.ZERO, 2.5f, 2.5f, 1.0f);
    val wall = new Geometry("Box", box);
    val mat_brick = new Material(
      assetManager, "Common/MatDefs/Misc/Unshaded.j3md");
    mat_brick.setTexture("ColorMap",
      assetManager.loadTexture("Textures/Terrain/BrickWall/BrickWall.jpg"));
    wall.setMaterial(mat_brick);
    wall.setLocalTranslation(2.0f, -2.5f, 0.0f);
    rootNode.attachChild(wall);

  }

  def materializeStats() {
    guiNode.detachAllChildren();
    guiFont = assetManager.loadFont("Interface/Fonts/Default.fnt");
    val helloText = new BitmapText(guiFont, false);
    helloText.setSize(guiFont.getCharSet().getRenderedSize());
    helloText.setText("Hello World");
    helloText.setLocalTranslation(300, helloText.getLineHeight(), 0);
    guiNode.attachChild(helloText);
  }
  
  def materializePlayer() {
    mat_default = new Material(assetManager, "Common/MatDefs/Misc/ShowNormals.j3md");

    player = assetManager.loadModel("Models/Teapot/Teapot.obj")

    player.setLocalRotation(Quaternion.IDENTITY)
    player.setMaterial(mat_default);

    rootNode.attachChild(player);
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

  override def simpleInitApp() {

    settings.setTitle("Tank Showdown")
    setPauseOnLostFocus(false)
    setShowSettings(false)

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))
    stateManager.attach(new NetworkState)


    materializePlayer()

    materializeLevel()

    materializeStats()

    var enemyNodes = new Node("Enemies")
    rootNode.attachChild(enemyNodes)

    // You must add a light to make the model visible
    val sun = new DirectionalLight();
    sun.setDirection(new Vector3f(-0.1f, -0.7f, -1.0f));
    rootNode.addLight(sun);

    projectileHandler = new ProjectileHandler(mat_default)
    projectileHandler.init()

    setupInput()
  }

  override def simpleUpdate(tpf: Float) {

    //player.move(playerInput._1)
    //player.rotate(playerInput._2)

    projectileHandler.update(rootNode, tpf)

    //projectileHandler.collidesWith(enemy, rootNode)

    val (pos,rot) = getCamPosition
    getCamera.setFrame(pos,rot)


    playerInput = noPlayerInput
  }


  def syncGameWorld(all:List[_ <: AbstractGameObject]) {

    import scala.collection.JavaConversions.asScalaBuffer
    val enemyNodes = rootNode.getChild("Enemies").asInstanceOf[Node].getChildren
    val enemyMap = enemyNodes.map( e => (e.getUserData[PlayerGO]("PlayerGO"),e) ).toMap

    var i=0
    all.foreach {
      case p:ProjectileGO =>

      case p:PlayerGO =>
        if(p.playerId == playerIdOpt.get) {
          //println("Getting server state" + p.position + " " + p.direction)

          player.setLocalTranslation(p.position)
          player.setLocalRotation(p.direction)
        } else {
          val enemyOpt = enemyMap.find { case (pd, spatial ) => pd.playerId == p.playerId }

          enemyOpt match {
            case Some((enemyPd, spatial)) =>
              //println("updated " + p.position)
              //println("updating existing " + enemyPd.position)
              spatial.setUserData("PlayerGO",enemyPd)
              spatial.setLocalTranslation(p.position)
              spatial.setLocalRotation(p.direction)
            case None => materializeEnemy(p)
          }
        }
      case _ =>
    }

  }

  def materializeEnemy(pd:PlayerGO) {
    val enemy = assetManager.loadModel(new ModelKey("Models/Teapot/Teapot.obj"))
    enemy.setMaterial(mat_default)

    println("Materialize enemy " + pd.position + " " + pd.direction)
    enemy.setUserData("PlayerGO",pd)
    enemy.setLocalTranslation(pd.position)
    enemy.setLocalRotation(pd.direction)

    enemy.setModelBound(new BoundingSphere())
    enemy.updateModelBound()

    println(enemy.getClass.getName)
    import scala.collection.JavaConversions.asScalaBuffer
    //enemy.asInstanceOf[Node].getChildren.toList.foreach( x => println(x.getClass.getName))
    println("........................")
    println(enemy.asInstanceOf[Geometry].getModelBound)
    rootNode.getChild("Enemies").asInstanceOf[Node].attachChild(enemy)

  }


}

object Spel {

  def main(arguments: Array[String]): Unit = {
    val spel = new Spel()
    val settings = new AppSettings(true);
    settings.setResolution(640,480)
    settings.setTitle("Tank Showdown")
    spel.setSettings(settings)
    spel.setShowSettings(false)
    spel.start()
  }
}