package se.bupp.lek.client

import com.jme3.scene.shape.Box
import com.jme3.bounding.BoundingSphere
import com.jme3.scene.{Spatial, Node, Geometry}
import com.jme3.asset.{ModelKey, AssetManager}
import com.jme3.export.Savable
import scala.collection.JavaConversions.asScalaBuffer
import com.jme3.material.{MaterialDef, MaterialList, Material}
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.shadow.BasicShadowRenderer
import com.jme3.renderer.ViewPort
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import com.jme3.light.{AmbientLight, DirectionalLight}
import com.jme3.post.FilterPostProcessor
import com.jme3.post.ssao.SSAOFilter
import com.jme3.util.TangentBinormalGenerator
import se.bupp.lek.server.Server._
import collection.immutable.{Stack, HashSet}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/4/12
 * Time: 3:57 PM
 * To change this template use File | Settings | File Templates.
 */


object ClientWorld {

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

  object SceneGraphUserDataKeys {
    val Projectile= "ProjectileGO"
    val Player = "PlayerGO"
  }
  object SceneGraphNodeKeys {
    val Projectiles = "Projectiles"
    val Enemies= "Enemies"
  }
}

class ClientWorld(val rootNode:Node,val assetManager:AssetManager, playerIdOpt:() => Option[Int],playerInput:PlayerInput, viewPort:ViewPort) {
  import ClientWorld._

  var mat_default : Material = _
  var mat_default_lgt : Material = _
  var mat_default_ush : Material = _

  var projectileGeometry:Box = _



  var player:Spatial = _

  var projectileSeqId = 0

  var fired = Stack[ProjectileFireGO]()



  def init(playerPosition:Orientation) {


    rootNode.setShadowMode(ShadowMode.Off)
    projectileGeometry = new Box(Vector3f.ZERO.clone(), 0.1f, 0.1f, 0.1f)
    projectileGeometry.setBound(new BoundingSphere())
    projectileGeometry.updateBound()

    mat_default = new Material(assetManager, "Common/MatDefs/Misc/ShowNormals.j3md");
    mat_default_lgt = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");

    //mat_default_lgt.setBoolean("UseMaterialColors",true);  // Set some parameters, e.g. blue.
    mat_default_lgt.setBoolean("m_UseMaterialColors", true);
    mat_default_lgt.setColor("m_Ambient",  ColorRGBA.Orange);
    mat_default_lgt.setColor("m_Diffuse",  ColorRGBA.Orange);
    mat_default_lgt.setColor("m_Specular", ColorRGBA.White);
    mat_default_lgt.setFloat("m_Shininess", 12);



    mat_default_ush = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");

    mat_default_ush.setColor("Color", ColorRGBA.Blue);
    //mat_default_ush.setColor("Diffuse", ColorRGBA.Blue ); // with Lighting.j3md
    //mat_default_ush.setColor("Ambient", ColorRGBA.White);


    materializePlayer(playerPosition)
    materializeLevel()
    //materializeStats()

    var enemyNodes = new Node(SceneGraphNodeKeys.Enemies)
    rootNode.attachChild(enemyNodes)
    var projectileNodes = new Node(SceneGraphNodeKeys.Projectiles)
    rootNode.attachChild(projectileNodes)

    // You must add a light to make the model visible
    val sun = new DirectionalLight();
    //sun.setColor(ColorRGBA.White)
    val sunDirection: Vector3f = new Vector3f(-1, -1, -1).normalizeLocal()
    sun.setDirection(sunDirection);
    sun.setColor(ColorRGBA.Green)
    rootNode.addLight(sun);

    val al = new AmbientLight();
    al.setColor(ColorRGBA.White.mult(1.3f));
    rootNode.addLight(al);

    val bsr = new BasicShadowRenderer(assetManager, 1024);
    bsr.setDirection(sunDirection.clone()); // light direction
    viewPort.addProcessor(bsr);


    /*
    val fpp = new FilterPostProcessor(assetManager);
    val ssaoFilter = new SSAOFilter(12.94f, 43.92f, 0.33f, 0.61f);
    fpp.addFilter(ssaoFilter)
    viewPort.addProcessor(fpp);
    */
    //rootNode.setShadowMode(ShadowMode.CastAndReceive);
  }

  def purgeFired() : List[ProjectileFireGO] = {

    val res = fired.toList
    fired = Stack[ProjectileFireGO]()
    res
  }

  def fireProjectile(pos:Vector3f, dir:Quaternion) = {

      fired = fired.push(
        new ProjectileFireGO(
          new OrientationGO(pos.add(0f,0.7f,0.0f).add(dir.getRotationColumn(0).mult(0.5f)),dir.clone()),
          4.5f,
          System.currentTimeMillis(),
          projectileSeqId
        ))
      projectileSeqId += 1
    }

  def getCamPosition() : (Vector3f,Quaternion) = {

    val pos = player.getLocalTranslation
    val camPos = pos.add(Vector3f.UNIT_XYZ.clone().mult(5))

    val dir = pos.subtract(camPos).normalize()

    val rot = Quaternion.IDENTITY.clone()
    rot.lookAt(dir, new Vector3f(0, 1, 0))

    (camPos,rot)
  }

  def materializeLevel() {
    // Create a wall with a simple texture from test_data
    val wall = assetManager.loadModel("level.blend")//new Box(Vector3f.ZERO, 2.5f, 2.5f, 1.0f);
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
    wall.setLocalTranslation(0.0f, 0.0f, 0.0f);
    wall.setShadowMode(ShadowMode.Receive)
    rootNode.attachChild(wall);

  }

  def materializePlayer(orientation:Orientation) {


    //player = assetManager.loadModel("Models/Teapot/Teapot.obj")
    player = materializeTank(orientation)

    //player.setMaterial(mat_default);

    rootNode.attachChild(player);
  }

  def materializeProjectile(p:ProjectileGO) {

    //println("adding projectile " + p.position + "" + p.id)
    val instance = new Geometry("Box", projectileGeometry);
    instance.setModelBound(new BoundingSphere())
    instance.updateModelBound()
    instance.setMaterial(mat_default)
    instance.setLocalTranslation(p.position)
    instance.setUserData("ProjectileGO",p)
    instance.setShadowMode(ShadowMode.Off)

    rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].attachChild(instance)
  }

  def materializeTank(pd: Orientation): Spatial = {
    val tank = assetManager.loadModel(new ModelKey("tank2.blend"))
    //enemy.setMaterial(mat_default)

    tank.setLocalScale(0.5f)
    tank.setLocalTranslation(pd.position)
    tank.setLocalRotation(pd.direction)
    tank.setShadowMode(ShadowMode.Off)
    tank
  }

  def materializeEnemy(pd:PlayerGO) {
    val tank = materializeTank(pd)

    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    tank.setUserData(SceneGraphUserDataKeys.Player, pd)

    rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].attachChild(tank)
  }

  def syncGameWorld(allUpdates:Set[_ <: AbstractOwnedGameObject with Savable]) {

    import scala.collection.JavaConversions.asScalaBuffer
    val enemyNodes = rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].getChildren
    val projectileNodes = rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].getChildren
    val enemyMap = enemyNodes.map( e => (e.getUserData[PlayerGO](SceneGraphUserDataKeys.Player),e).ensuring(_._1 != null) ).toMap
    val projectileMap = projectileNodes.map( e => (e.getUserData[ProjectileGO](SceneGraphUserDataKeys.Projectile),e).ensuring(_._1 != null) ).toMap

    var allExisting = enemyMap.toSet ++ projectileMap.toSet

    def doMatch(l:GOWithSavable,r:(GOWithSavable,Spatial)) : Boolean = {
      (l, r._1) match {
        case (l:PlayerGO, rr:PlayerGO) =>
          l.playerId == rr.playerId
        case (l:ProjectileGO, rr:ProjectileGO) =>
          l.id == rr.id
        case _ => false
      }
    }
    var (newInUpdateOrPlayer, noUpdate, matched) = setMatch(allUpdates, allExisting, doMatch)

    if(false && (System.currentTimeMillis()) % 10 == 3) {
      /*println("enemies" + enemyNodes.size +
        "noUpdate" + noUpdate.size +
        "newInUpdate " + newInUpdateOrPlayer.size +
        "projectileMap " + projectileMap.size +
        "matched " + matched.size +
        "allU "+allUpdates.size
      )*/
      enemyMap.foreach {  case (k,v) => println( "pos " +  k.position + " " + v.getLocalTranslation()) }

    }

    newInUpdateOrPlayer.foreach {
      case p:PlayerGO =>
        if(p.playerId == playerIdOpt.apply().get) {

          //player.move(playerInput.translation)
          //player.rotate(playerInput.rotation)
          player.setLocalTranslation(p.position)
          player.setLocalRotation(p.direction)
        } else {
          materializeEnemy(p)
        }
      case p:ProjectileGO =>
        materializeProjectile(p)
    }

    matched.foreach {
      case (u:AbstractOwnedGameObject with Savable,(o,s:Spatial)) =>
        //println("upd match" + s.getLocalTranslation + " " + u.position + " " +u.direction)
        s.setLocalTranslation(u.position.clone())
        s.setLocalRotation(u.direction.clone())
        u match {
          case p:ProjectileGO => s.setUserData(SceneGraphUserDataKeys.Projectile, p)
          case p:PlayerGO => s.setUserData(SceneGraphUserDataKeys.Player, p)
        }
    }
    noUpdate.foreach {
      case (o, spatial) =>
        o match {
          case p:ProjectileGO => rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].detachChild(spatial)
          case p:PlayerGO => rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].detachChild(spatial)
        }
    }
  }
}

