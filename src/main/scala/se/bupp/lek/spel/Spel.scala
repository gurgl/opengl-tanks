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

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/16/12
 * Time: 11:49 PM
 * To change this template use File | Settings | File Templates.
 */

class Spel extends SimpleApplication {

  var player:Spatial = _

  //val speed = 1.0f
  val rotSpeed = 2.0f

  var lastAcc = 0f
  var currentSpeed = 0f
  val MAX_SPEED = 1.0f
  val ACC = 0.1f

  
  
  
  //var enemies:Spatial = _
  var projectileHandler : ProjectileHandler = _
  var mat_default : Material = _

  var gameClient:Client = _
  var playerIdOpt:Option[Int] = None

  var lastGameWorldUpdate:Option[GameServer.ServerGameWorld] = None

  val actionListener = new AnalogListener() with ActionListener {

    def onAction(name:String, value:Boolean, tpf:Float) {
      if (name.equals("Forward")) {
        if(value == false) {
         lastAcc = 0
        }
      }

      if (name.equals("Fire")) {
        if(value == true) {
          val p = projectileHandler.fire(player.getLocalTranslation,player.getLocalRotation.getRotationColumn(0).normalize())
          rootNode.attachChild(p)
        }
      }
    }

    def onAnalog(name:String, value:Float, tpf:Float) {
      if (name.equals("Left")) {
        val v = player.getLocalTranslation();
        //teapot.setLocalTranslation(v.x + speed * tpf, v.y, v.z);
        player.rotate(0, rotSpeed * tpf, 0);
      }


      if (name.equals("Right")) {

        val v = player.getLocalTranslation();
        //teapot.setLocalTranslation(v.x - speed * tpf, v.y, v.z);
        player.rotate(0, -rotSpeed * tpf, 0);
        //teapot.setLocalTranslation(v.x,v.y + speed * tpf, v.z);
      }

      if (name.equals("Forward")) {

        val v = player.getLocalRotation.toRotationMatrix;
        val t = player.getLocalTranslation();
        player.setLocalTranslation(t.add(v.getColumn(0).mult(speed*tpf)));



      }

      if (name.equals("Back")) {

        val v = player.getLocalRotation.toRotationMatrix;
        val t = player.getLocalTranslation();
        player.setLocalTranslation(t.add(v.getColumn(0).mult(-speed*tpf)));
      }
    }
  };
  
  def createYawPitchRoll() = {

    val PIf = math.Pi.toFloat
    Quaternion.IDENTITY.fromAngles(PIf / 2.0f, PIf/ 2.0f, 0f)
  }
  
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



  override def simpleInitApp() {

    setPauseOnLostFocus(false)
    setShowSettings(false)


    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))
    //import collection.JavaConversions.
    inputManager.addListener(actionListener, List("Left","Right", "Forward", "Back", "Fire"):_*)
    mat_default = new Material(
          assetManager, "Common/MatDefs/Misc/ShowNormals.j3md");
    
    val tankGeometry = assetManager.loadModel("Models/Teapot/Teapot.obj") //.asInstanceOf[Mesh]
    player = tankGeometry //new Geometry("Player", tankGeometry)

    //player.setLocalTranslation(Vector3f.ZERO)
    //player.setLocalRotation(Quaternion.ZERO)
    player.setMaterial(mat_default);
    rootNode.attachChild(player);


    //val enemyShape = CollisionShapeFactory.createMeshShape((Node) enemy);


    projectileHandler = new ProjectileHandler(mat_default)
    projectileHandler.init()

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

    var enemyNodes = new Node("Enemies")
    rootNode.attachChild(enemyNodes)

    // Display a line of text with a default font
    guiNode.detachAllChildren();
    guiFont = assetManager.loadFont("Interface/Fonts/Default.fnt");
    val helloText = new BitmapText(guiFont, false);
    helloText.setSize(guiFont.getCharSet().getRenderedSize());
    helloText.setText("Hello World");
    helloText.setLocalTranslation(300, helloText.getLineHeight(), 0);
    guiNode.attachChild(helloText);

    // You must add a light to make the model visible
    val sun = new DirectionalLight();
    sun.setDirection(new Vector3f(-0.1f, -0.7f, -1.0f));
    rootNode.addLight(sun);

    inputManager.addMapping("Left",   new KeyTrigger(KeyInput.KEY_A));
    inputManager.addMapping("Right",  new KeyTrigger(KeyInput.KEY_D));
    inputManager.addMapping("Forward", new KeyTrigger(KeyInput.KEY_W));
    inputManager.addMapping("Back",  new KeyTrigger(KeyInput.KEY_S));
    inputManager.addMapping("Fire",  new KeyTrigger(KeyInput.KEY_SPACE));


    //inputManager.addMapping("Rotate", new KeyTrigger(KeyInput.KEY_SPACE));

    initClient()
  }

  var lastSentUpdate = 0L

  override def simpleUpdate(tpf: Float) {
    if(playerIdOpt.isEmpty) return
    //ninja.rotate(0, 2 * tpf, 0);

    lastGameWorldUpdate.foreach( gw => syncGameWorld(gw))
    lastGameWorldUpdate = None

    projectileHandler.update(rootNode, tpf)

    //projectileHandler.collidesWith(enemy, rootNode)

    val (pos,rot) = getCamPosition
    getCamera.setFrame(pos,rot)

    if(System.currentTimeMillis() - lastSentUpdate > 1000/15 ) {
      val request: PlayerActionRequest = new PlayerActionRequest
      player.ensuring(player.getLocalTranslation != null && player.getLocalRotation != null)
      request.playerId = playerIdOpt.get
      request.position = player.getLocalTranslation
      request.direction = player.getLocalRotation
      gameClient.sendUDP(request)
      lastSentUpdate = System.currentTimeMillis()
    }
    
  }
  
  def syncGameWorld(gw:ServerGameWorld) {

    import scala.collection.JavaConversions.asScalaBuffer
    val enemyNodes = rootNode.getChild("Enemies").asInstanceOf[Node].getChildren
    val enemyMap = enemyNodes.map( e => (e.getUserData[PlayerDetails]("PlayerDetails"),e) ).toMap

    gw.players.foreach { p =>
        if(p.playerId != playerIdOpt.get) {
          val enemyOpt = enemyMap.find { case (pd, spatial ) => pd.playerId == p.playerId }

          enemyOpt match {
            case Some((enemyPd, spatial)) => 
              //println("updating existing " + enemyPd.position)
              spatial.setUserData("PlayerDetails",enemyPd)
              spatial.setLocalTranslation(p.position)
              spatial.setLocalRotation(p.direction)
            case None => materializeEnemy(p)
          }
        }
    }
  }

  def materializeEnemy(pd:PlayerDetails) {
    val enemy = assetManager.loadModel(new ModelKey("Models/Teapot/Teapot.obj"))
    enemy.setMaterial(mat_default)

    println("Materialize enemy " + pd.position + " " + pd.direction)
    enemy.setUserData("PlayerDetails",pd)
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

  def initClient() {
    gameClient = new Client();

    val kryo = gameClient.getKryo();

    GameServer.getNetworkMessages.foreach( kryo.register(_))
    
    gameClient.addListener(new Listener() {
       override def received (connection:Connection , obj:Object ) {
          obj match {
            case response:ServerGameWorld=>
              if(playerIdOpt.isDefined) {
                lastGameWorldUpdate = Some(response)
              } else {
                println("Getting world wo player received")
              }

            case response:PlayerJoinResponse =>
              println("join resp received")
              playerIdOpt = Some(response.playerId)

            case _ =>
          }
       }
    });

    gameClient.start();
    gameClient.connect(5000, "localhost", 54555, 54777);

    val playerJoinRequest = new PlayerJoinRequest()
    playerJoinRequest.clientLabel = ManagementFactory.getRuntimeMXBean().getName()
    gameClient.sendTCP(playerJoinRequest);
  }
}

object Spel {

  def main(arguments: Array[String]): Unit = {
    val spel = new Spel()
    spel.setShowSettings(false)
    spel.start()
  }
}