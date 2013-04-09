package se.bupp.lek.common

import com.jme3.asset.{ModelKey, AssetManager}
import com.jme3.scene.{Geometry, Spatial, Node}
import com.jme3.bullet.PhysicsSpace
import com.jme3.bullet.control.{BetterCharacterControl, RigidBodyControl, CharacterControl}
import com.jme3.material.Material
import com.jme3.scene.shape.{Cylinder, Box}
import com.jme3.math.{ColorRGBA, Vector3f}
import com.jme3.bullet.util.CollisionShapeFactory
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import se.bupp.lek.common.Model.{AbstractOwnedGameObject, ProjectileGO, PlayerGO, Orientation}
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape
import com.jme3.bounding.BoundingSphere
import collection.immutable.HashSet
import com.jme3.export.Savable
import org.slf4j.LoggerFactory
import com.jme3.bullet.collision.PhysicsCollisionObject

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-06-24
 * Time: 11:48
 * To change this template use File | Settings | File Templates.
 */

object SceneGraphWorld {
  object SceneGraphUserDataKeys {
    val Projectile= "ProjectileGO"
    val Player = "PlayerGO"
  }
  object SceneGraphNodeKeys {
    type SceneGraphNodeKey = String
    val Projectiles = "Projectiles"
    val Enemies= "Enemies"
    //val Particles = "Particles"
    val Effects = "Effects"
    val Statics = "Statics"
    val Player = "Player"
    val Gui = "Gui"
  }



  def setMatch[A,B](left:Set[A],right:Set[B],comp:Function2[A,B,Boolean]) : Tuple3[Set[A],Set[B],Set[(A,B)]] = {
    var leftLeft = new HashSet[A]()
    var rightLeft = new HashSet[B]() ++ right
    var matched = new HashSet[(A,B)]()

    left.foreach { l =>
      rightLeft.find(r => comp(l,r)) match {
        case Some(rr) =>
          rightLeft -= rr
          matched = matched + Pair(l, rr)
          ()
        case None =>
          leftLeft += l
          ()
      }
    }
    (leftLeft,rightLeft, matched)
  }

  type GOWithSavable = AbstractOwnedGameObject with Savable
}


abstract class SceneGraphWorld(val isHeadLess:Boolean, assetManager:AssetManager, rootNode:Node) {

  private val log = LoggerFactory.getLogger(classOf[SceneGraphWorld])
  def getPhysicsSpace : PhysicsSpace

  var gravity: Vector3f = new Vector3f(0, -9.8f, 0)

  var playerControl:BetterCharacterControl = _
  var player:Spatial = _

  var mat_default : Material = _
  var mat_default_red : Material = _
  var mat_default_blue: Material = _
  var mat_default_lgt : Material = _
  var mat_default_ush : Material = _

  var projectileGeometry:Box = _
  var flagGeometry : Cylinder = _

  def tankForward = new Vector3f(1.0f,0,0)

  import SceneGraphWorld._

  def getNode(key:String) = rootNode.getChild(key).asInstanceOf[Node]

  def init() {
    projectileGeometry = new Box(Vector3f.ZERO.clone(), 0.1f, 0.1f, 0.1f)
    flagGeometry = new Cylinder(10,10,0.10f,1.0f,true);
    //projectileGeometry.setBound(new BoundingSphere())
    //projectileGeometry.updateBound()


    if(!isHeadLess) {
      mat_default = new Material(assetManager, "Common/MatDefs/Misc/ShowNormals.j3md");
      mat_default_red  = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");
      mat_default_blue  = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");
      mat_default_lgt = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");

      //mat_default_red.setBoolean("UseMaterialColors",false);  // Set some parameters, e.g. blue.
      //mat_default_red.setBoolean("m_UseMaterialColors", false);
      ///mat_default_red.setColor("Color",  ColorRGBA.Orange);
      mat_default_red.setBoolean("UseMaterialColors", true);
      mat_default_red.setColor("Ambient",  new ColorRGBA(1f,0f,0f,1.0f));
      mat_default_red.setColor("Diffuse",  new ColorRGBA(1f,0.5f,0.5f,0.5f));
      mat_default_red.setColor("Specular", ColorRGBA.White);
      mat_default_red.setFloat("Shininess", 128);

      mat_default_blue.setBoolean("UseMaterialColors", true);
      mat_default_blue.setColor("Ambient",  new ColorRGBA(0f,0f,1f,1f));
      mat_default_blue.setColor("Diffuse",  new ColorRGBA(0.5f,0.5f,1f,0.5f));
      mat_default_blue.setColor("Specular", ColorRGBA.White);
      mat_default_blue.setFloat("Shininess", 128);
      //mat_default_blue.setBoolean("UseMaterialColors",false);  // Set some parameters, e.g. blue.
      //mat_default_blue.setBoolean("m_UseMaterialColors", false);
      //mat_default_blue.setColor("Color",  ColorRGBA.Cyan);

      //mat_default_lgt.setBoolean("UseMaterialColors",true);  // Set some parameters, e.g. blue.
      /*mat_default_lgt.setBoolean("m_UseMaterialColors", true);
      mat_default_lgt.setColor("m_Ambient",  ColorRGBA.Orange);
      mat_default_lgt.setColor("m_Diffuse",  ColorRGBA.Orange);
      mat_default_lgt.setColor("m_Specular", ColorRGBA.White);
      mat_default_lgt.setFloat("m_Shininess", 12);
      */


      mat_default_ush = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");

      mat_default_ush.setColor("Color", ColorRGBA.Blue);
      //mat_default_ush.setColor("Diffuse", ColorRGBA.Blue ); // with Lighting.j3md
      //mat_default_ush.setColor("Ambient", ColorRGBA.White);
    }

    var levelItems = new Node(SceneGraphNodeKeys.Statics)
    rootNode.attachChild(levelItems)


    var enemyNodes = new Node(SceneGraphNodeKeys.Enemies)
    rootNode.attachChild(enemyNodes)

    var projectileNodes = new Node(SceneGraphNodeKeys.Projectiles)
    rootNode.attachChild(projectileNodes)

    var effects = new Node(SceneGraphNodeKeys.Effects)
    rootNode.attachChild(effects)

    var playerContainer = new Node(SceneGraphNodeKeys.Player)
    rootNode.attachChild(playerContainer)

    materializeLevel()


    //val flag = materializeFlag(new Orientation(new Vector3f(-2.0f,0.5f,-2.0f),MathUtil.noRotation.fromAngles(math.Pi.toFloat/2,0f,0f)))
    //levelItems.attachChild(flag)

  }

  def materializeLevel() {
    // Create a wall with a simple texture from TEST_DATA
    val level = assetManager.loadModel("level.blend")//new Box(Vector3f.ZERO, 2.5f, 2.5f, 1.0f);

    //assetManager.registerLocator("town.zip", classOf[ZipLocator]);
    //val level = assetManager.loadModel("main.scene");
    //level.setLocalScale(0.2f)
    val sceneCollisionShape = CollisionShapeFactory.createMeshShape(level) //.asInstanceOf[Spatial]
    val landscape = new RigidBodyControl(sceneCollisionShape, 0)
    landscape.setCollisionGroup(PhysicsCollisionObject.COLLISION_GROUP_01)
    landscape.setCollideWithGroups(0)
    //landscape.setCollisionGroup(PhysicsCollisionObject.COLLISION_GROUP_03)
    level.addControl(landscape);

    getPhysicsSpace.add(landscape)

    //getPhysicsSpace.setGravity(new Vector3f(0,-0.00001f,0))

    //val wall = new Geometry("Box", box);
    //val matLevel = new MaterialList()
    //val materialList = assetManager.loadAsset("level.mtl").asInstanceOf[MaterialList]


    /*wall.asInstanceOf[Node].getChildren.foreach {
      case x:Geometry =>TangentBinormalGenerator.generate(x.getMesh, true)
      case _ =>
    }*/

    //wall.setMaterial(mat_default_lgt)
    /*mat_brick.setTexture("ColorMap",
      assetManager.loadTexture("level.mtl"));*/
    //wall.asInstanceOf[Geometry].
    level.setLocalTranslation(0.0f, -0.5f, 0.0f);
    if(!isHeadLess) {
      level.setShadowMode(ShadowMode.Receive)
    }
    getNode(SceneGraphNodeKeys.Statics).attachChild(level);
  }

  def materializeTank(pd: Orientation): Spatial = {
    log.debug("creating tank")
    val tank = new Node("Tank")
    var model: Spatial = assetManager.loadModel(new ModelKey("tank2.blend"))
    model.setLocalScale(0.5f)
    //val model = assetManager.loadModel("Models/Jaime/Jaime.j3o");
    //model.setLocalScale(1.5f)

    tank.attachChild(model)
    model.setLocalTranslation(new Vector3f(0,0.4f,0))
    //enemy.setMaterial(mat_default)

    //tank.setLocalTranslation(pd.position)
    tank.setLocalRotation(pd.direction)
    if(!isHeadLess) {
      tank.setShadowMode(ShadowMode.Off)
    }
    tank
  }

  /*def materializeTankServer(pd: Orientation): Spatial = {
    val tank = new Node("Tank")

    var model: Spatial = assetManager.loadModel(new ModelKey("tank2.blend"))
    tank.setLocalScale(0.5f)
    //val model = assetManager.loadModel("Models/Jaime/Jaime.j3o");
    //model.setLocalScale(1.5f)
    tank.attachChild(model)


    if(!isHeadLess) {
      tank.setShadowMode(ShadowMode.Off)
    }
    tank
  }*/



  def materializeFlag(pd: Orientation): Spatial = {
    val instance = new Geometry("flag", flagGeometry)
    //enemy.setMaterial(mat_default)

    //tank.setLocalScale(0.5f)
    instance.setLocalTranslation(pd.position)
    instance.setLocalRotation(pd.direction)
    instance.setShadowMode(ShadowMode.Off)
    if(!isHeadLess) {
      instance.setMaterial(mat_default)
    }
    instance
  }

  def tankCollisionShape = new CapsuleCollisionShape(0.35f, 0.8f, 0) // below 0.6 = woobly, 0.15 = jumpy

  def materializeEnemy(pd:PlayerGO) = {
    val tank = materializeTank(pd)

    log.info("materialzie enemy" + pd.playerId)

    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    tank.setUserData(SceneGraphUserDataKeys.Player, pd)

    getNode(SceneGraphNodeKeys.Enemies).attachChild(tank)
    tank
  }

  def materializePlayer(orientation:Orientation) {

    //player = assetManager.loadModel("Models/Teapot/Teapot.obj")
    player = materializeTank(orientation)
    //player.setMaterial(if(teamId % 2 == 0) mat_default_blue else mat_default_red)
    player.setLocalTranslation(new Vector3f(0, 2.5f, 0))

    val capsuleShape = tankCollisionShape
    //val playerControl = new BetterCharacterControl(capsuleShape.getRadius,capsuleShape.getHeight, 8f)
    val playerControl = new BetterCharacterControl(tankCollisionShape.getRadius,tankCollisionShape.getHeight, 8f)
    //playerControl.setUseViewDirection(false)
    player.addControl(playerControl)

    //playerControl.setViewDirection(orientation.direction.getRotationColumn(0))
    playerControl.setGravity(gravity.clone());

    getPhysicsSpace.add(playerControl)


    //playerControl.setJumpSpeed(0);
    //playerControl.setFallSpeed(0.3f);
    //playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));



    /*val capsuleShape = new CapsuleCollisionShape(0.5f, 0.51f, 1)
    playerControl = new CharacterControl(capsuleShape, 0.05f)
    player.addControl(playerControl)
    bulletAppState.getPhysicsSpace.add(playerControl)


    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0);
    playerControl.setGravity(0);
    playerControl.setPhysicsLocation(new Vector3f(0, 0.5f, 0));
    */
    //player.setMaterial(mat_default);


  }



  def materializeProjectileServer(p:ProjectileGO) = {

    log.info("adding projectile " + p.position + "" + p.id)
    val instance = new Geometry("Box", projectileGeometry);
    instance.setLocalTranslation(p.position.clone())

    instance.setUserData(SceneGraphUserDataKeys.Projectile,p)

    if(!isHeadLess) {
      instance.setShadowMode(ShadowMode.Off)
      instance.setMaterial(mat_default)
    }

    rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].attachChild(instance)

    instance
  }

  def materializeProjectile(p:ProjectileGO) {

    //println("adding projectile " + p.position + "" + p.id)
    val instance = new Geometry("Box", projectileGeometry);
    instance.setModelBound(new BoundingSphere())
    instance.updateModelBound()
    instance.setLocalTranslation(p.position)
    instance.setUserData(SceneGraphUserDataKeys.Projectile,p)

    if(!isHeadLess) {
      instance.setShadowMode(ShadowMode.Off)
      instance.setMaterial(mat_default)
    }

    rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].attachChild(instance)
  }

  def destroy() {
    val toRemove = List(
      SceneGraphNodeKeys.Enemies,
      SceneGraphNodeKeys.Statics,
      SceneGraphNodeKeys.Projectiles,
      SceneGraphNodeKeys.Effects,
      SceneGraphNodeKeys.Player
    )
    removeNodes(toRemove)
  }

  def cleanNodes(toRemove:List[SceneGraphNodeKeys.SceneGraphNodeKey]) {
    import collection.JavaConversions.asScalaBuffer
    toRemove.foreach( x => Option(getNode(x)) match {
      case Some(y) => y.getChildren.toList.foreach ( s => y.detachChild(s))
      case None => log.debug("Cannot remove " + x)
    })
  }

  def removeNodes(toRemove:List[SceneGraphNodeKeys.SceneGraphNodeKey]) {
    toRemove.foreach( x => Option(getNode(x)) match {
      case Some(y) => log.debug("Detatching " + y.getName) ; rootNode.detachChild(y)
      case None => log.debug("Cannot remove " + x)
    })
  }

}
