package se.bupp.lek.client

import com.jme3.asset.{ModelKey, AssetManager}
import com.jme3.scene.{Geometry, Spatial, Node}
import com.jme3.bullet.PhysicsSpace
import com.jme3.bullet.control.{RigidBodyControl, CharacterControl}
import com.jme3.material.Material
import com.jme3.scene.shape.Box
import com.jme3.math.{ColorRGBA, Vector3f}
import com.jme3.bullet.util.CollisionShapeFactory
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import se.bupp.lek.server.Model.{AbstractOwnedGameObject, ProjectileGO, PlayerGO, Orientation}
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape
import com.jme3.bounding.BoundingSphere
import collection.immutable.HashSet
import com.jme3.export.Savable

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
    val Projectiles = "Projectiles"
    val Enemies= "Enemies"
    val Particles = "Particles"
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

  def getPhysicsSpace : PhysicsSpace

  var playerControl:CharacterControl = _
  var player:Spatial = _

  var mat_default : Material = _
  var mat_default_lgt : Material = _
  var mat_default_ush : Material = _

  var projectileGeometry:Box = _

  import SceneGraphWorld._

  def getNode(key:String) = rootNode.getChild(key).asInstanceOf[Node]

  def init() {
    projectileGeometry = new Box(Vector3f.ZERO.clone(), 0.1f, 0.1f, 0.1f)
    //projectileGeometry.setBound(new BoundingSphere())
    //projectileGeometry.updateBound()


    if(!isHeadLess) {
      mat_default = new Material(assetManager, "Common/MatDefs/Misc/ShowNormals.j3md");
      mat_default_lgt = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");

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

    materializeLevel()

    var enemyNodes = new Node(SceneGraphNodeKeys.Enemies)
    rootNode.attachChild(enemyNodes)
    var projectileNodes = new Node(SceneGraphNodeKeys.Projectiles)
    rootNode.attachChild(projectileNodes)

  }

  def materializeLevel() {
    // Create a wall with a simple texture from test_data
    val level = assetManager.loadModel("level.blend")//new Box(Vector3f.ZERO, 2.5f, 2.5f, 1.0f);

    //assetManager.registerLocator("town.zip", classOf[ZipLocator]);
    //val level = assetManager.loadModel("main.scene");
    //level.setLocalScale(0.2f)
    val sceneCollisionShape = CollisionShapeFactory.createMeshShape(level.asInstanceOf[Node])
    val landscape = new RigidBodyControl(sceneCollisionShape, 0)
    level.addControl(landscape);

    getPhysicsSpace.add(landscape)

    getPhysicsSpace.setGravity(Vector3f.ZERO.clone());

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
    level.setLocalTranslation(0.0f, 0.0f, 0.0f);
    if(!isHeadLess) {
      level.setShadowMode(ShadowMode.Receive)
    }
    rootNode.attachChild(level);
  }

  def materializeTank(pd: Orientation): Spatial = {
    val tank = assetManager.loadModel(new ModelKey("tank2.blend"))
    //enemy.setMaterial(mat_default)

    tank.setLocalScale(0.5f)
    //tank.setLocalTranslation(pd.position)
    tank.setLocalRotation(pd.direction)
    if(!isHeadLess) {
      tank.setShadowMode(ShadowMode.Off)
    }
    tank
  }

  def tankCollisionShape = new CapsuleCollisionShape(0.35f, 0.45f, 0)

  def materializeTank2(pd: Orientation): Spatial = {
    val tank = assetManager.loadModel(new ModelKey("tank2.blend"))
    //enemy.setMaterial(mat_default)

    tank.setLocalScale(0.5f)
    if(!isHeadLess) {
      tank.setShadowMode(ShadowMode.Off)
    }
    tank
  }


  def materializeEnemy(pd:PlayerGO) {
    val tank = materializeTank(pd)

    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    tank.setUserData(SceneGraphUserDataKeys.Player, pd)

    rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].attachChild(tank)
  }

  def materializePlayer(orientation:Orientation) {

    //player = assetManager.loadModel("Models/Teapot/Teapot.obj")
    player = materializeTank(orientation)


    val capsuleShape = tankCollisionShape
    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    playerControl.setUseViewDirection(false)
    player.addControl(playerControl)

    getPhysicsSpace.add(playerControl)


    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0.3f);
    playerControl.setGravity(0.3f);
    playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));



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

    rootNode.attachChild(player);
  }



  def materializeProjectile2(p:ProjectileGO) = {

    println("adding projectile " + p.position + "" + p.id)
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

}
