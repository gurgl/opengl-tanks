package se.bupp.lek.client

import com.jme3.scene.shape.Box
import com.jme3.bounding.BoundingSphere
import com.jme3.scene.{Spatial, Node, Geometry}
import com.jme3.asset.{ModelKey, AssetManager}
import com.jme3.export.Savable
import scala.collection.JavaConversions.asScalaBuffer
import com.jme3.material.{MaterialDef, MaterialList, Material}
import com.jme3.shadow.BasicShadowRenderer
import com.jme3.renderer.ViewPort
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import com.jme3.light.{AmbientLight, DirectionalLight}
import com.jme3.post.FilterPostProcessor
import com.jme3.post.ssao.SSAOFilter
import com.jme3.util.TangentBinormalGenerator
import se.bupp.lek.server.Model._
import com.jme3.bullet.util.CollisionShapeFactory
import com.jme3.bullet.BulletAppState
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape
import com.jme3.bullet.control.{CharacterControl, RigidBodyControl}
import com.jme3.asset.plugins.ZipLocator
import com.jme3.math.{FastMath, ColorRGBA, Quaternion, Vector3f}
import collection.immutable.Queue._
import collection.immutable.{Queue, Stack, HashSet}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/4/12
 * Time: 3:57 PM
 * To change this template use File | Settings | File Templates.
 */


object VisualWorldSimulation {


}

object SceneGraphWorld {
  object SceneGraphUserDataKeys {
    val Projectile= "ProjectileGO"
    val Player = "PlayerGO"
  }
  object SceneGraphNodeKeys {
    val Projectiles = "Projectiles"
    val Enemies= "Enemies"
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

class SceneGraphWorld(val isHeadLess:Boolean, assetManager:AssetManager, bulletAppState:BulletAppState, rootNode:Node) {

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
    projectileGeometry.setBound(new BoundingSphere())
    projectileGeometry.updateBound()


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

    bulletAppState.getPhysicsSpace.add(landscape)

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


    val capsuleShape = new CapsuleCollisionShape(0.05f, 0.05f, 1)
    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    playerControl.setUseViewDirection(false)
    player.addControl(playerControl)

    bulletAppState.getPhysicsSpace.add(playerControl)


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

class VisualWorldSimulation(val rootNode:Node,val assetManager:AssetManager, playerIdOpt:() => Option[Int],val playerInput:PlayerInput, viewPort:ViewPort, val bulletAppState:BulletAppState) extends SceneGraphWorld(false,assetManager,bulletAppState,rootNode) {
  import VisualWorldSimulation._
  import SceneGraphWorld._


  var saved = Queue.empty[(Long, Orientation, Reorientation)] //:+ ((System.currentTimeMillis(),startPosition, MathUtil.noMotion))


  var projectileSeqId = 0

  var fired = Stack[ProjectileFireGO]()

  val LocalInputLogSize = 20

  val lock:AnyRef = new Object


  def storePlayerLastInputAndOutput(simTime:Long, input:Reorientation) = {

    val physicalPosition = player.getLocalTranslation
    val physicalRotation = player.getLocalRotation //.subtract(gameApp.playerInput.saved.last._2.position)
    saveReorientation(simTime, new Orientation(physicalPosition, physicalRotation), input)
  }

  def saveReorientation(timeStamp:Long, orientation: Orientation, reorientation:Reorientation) {
    lock.synchronized {
      while(saved.size >= LocalInputLogSize) {
        saved = saved.dequeue._2
      }

      //val orientation = saved.last._2.reorientate(reorientation)
      saved =  saved + (timeStamp, orientation, reorientation)

    }
  }


  def init(playerPosition:Orientation) {
    super.init()


    rootNode.setShadowMode(ShadowMode.Off)


    materializePlayer(playerPosition)

    //materializeStats()


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

    /*val bsr = new BasicShadowRenderer(assetManager, 1024);
    bsr.setDirection(sunDirection.clone()); // light direction
    viewPort.addProcessor(bsr);
    */


    /*
    val fpp = new FilterPostProcessor(assetManager);
    val ssaoFilter = new SSAOFilter(12.94f, 43.92f, 0.33f, 0.61f);
    fpp.addFilter(ssaoFilter)
    viewPort.addProcessor(fpp);
    */
    //rootNode.setShadowMode(ShadowMode.CastAndReceive);
  }

  def flushFired() : List[ProjectileFireGO] = {

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




  def syncNonPlayerGameWorld(allUpdates:Set[_ <: AbstractOwnedGameObject with Savable]) {

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
    var (newInUpdate, noUpdate, matched) = setMatch(allUpdates, allExisting, doMatch)

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

    newInUpdate.foreach {
      case p:PlayerGO =>
        if(p.playerId == playerIdOpt.apply().get) {

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

  def update(simTime: Long,currentGameWorldUpdates:Queue[ServerGameWorld], playerId:Int, reorientation:Reorientation) {
    val predictor: VisualSimulationPrediction = new VisualSimulationPrediction(currentGameWorldUpdates, playerId)
    val nonPlayerPredictons = predictor.interpolateNonPlayerObjects(simTime)

    syncNonPlayerGameWorld(nonPlayerPredictons.distinct.toSet)

    applyPlayerInput(currentGameWorldUpdates,reorientation);
  }


  def diff(client:Orientation,server:Orientation) = {
    val transDiff = client.position.subtract(server.position)

    //println(client.position + " " + server.position + " " + transDiff + " " + transDiff.length())
    //new Quaternion(client.direction).subtract(server.direction)

    //val rotDiff = Quaternion.IDENTITY.clone().slerp(client.direction,server.direction,1.0f)
    //transDiff.length() < 0.1 && rotDiff.getW < FastMath.PI / 80
    //math.sqrt(client.direction.dot(server.direction))
    val deltaQ: Quaternion = client.direction.subtract(server.direction)
    val sqrt = math.sqrt(deltaQ.dot(deltaQ))
    (transDiff.length(),math.abs(sqrt))
  }

  def applyCorrectionIfDiffers(serverSnapshotSentByPlayerTime:Long,serverSimTime:Long, server:Orientation) {

    lock.synchronized {

      val(discarded, newSaved) = saved.partition ( _._1 < serverSnapshotSentByPlayerTime)
      saved = newSaved
      //saved = if(discarded.size > 0) discarded.last +: newSaved else newSaved
      if(saved.size == 0) {
        //server
        println("Inga ")

      } else {

        val diffHeur = diff(saved.head._2,server)
        //val newPos =
        if(diffHeur._1 > 1.0 || diffHeur._2 > FastMath.PI / 45) {

          val newSavedPos = saved.foldLeft(Queue(server)) {
            case (recalculatedPositions,(time,orientationBeforeReorientation, reorient)) =>
              recalculatedPositions :+ recalculatedPositions.last.reorientate(reorient)
          }
          println("Bad " + saved.head._2.position + " " + server.position + " " + serverSimTime + " " + diffHeur._1 + " " + serverSnapshotSentByPlayerTime)
          saved = newSavedPos.tail.zip(saved).map {case (np, (ts, _ , reor)) => (ts, np, reor) }
          //println("Bad " + saved.head._2.position+ " " + server.position + " " + diffHeur._1) // + " " + newSavedPos.last)
          //println("Bad " + diffHeur)
          //newSavedPos.last
            val control: CharacterControl = player.getControl(classOf[CharacterControl])
            control.setPhysicsLocation(saved.last._2.position)
            player.setLocalRotation(saved.last._2.direction)
            //Client.spel.gameWorld.player.setLocalRotation(saved.last._2.direction)
            //control.setViewDirection(saved.last._2.direction.getRotationColumn(0))
        } /*else {
          //println("Good " + diffHeur)
          //println("using " + saved.last._2)
          saved.last._2
        } */
        //saved.map{ case (a,b,c) => a + " p" + b._1 + " t" + a._1 }.
        //newPos
        //Client.spel.playerInput.saved = saved
        //new Orientation(saved.last._3._1,saved.last._3.direction)
      }
    }
  }

  def applyPlayerInput(currentGameWorldUpdates:Queue[ServerGameWorld], reorientation:Reorientation) {


    currentGameWorldUpdates.last.players.find(_.playerId == playerIdOpt().get).foreach {
      x => 
      applyCorrectionIfDiffers(x.sentToServerByClient, currentGameWorldUpdates.last.timeStamp, x)

      //player.move(playerinput.translation)
      //player.rotate(playerinput.rotation)

      val control = player.getControl(classOf[CharacterControl])

      //println(direction + " " + bulletAppState.getSpeed + " " + bulletAppState.getPhysicsSpace.getAccuracy)
      control.setWalkDirection(reorientation._1)
      //control.setviewdirection(player.setlocalrotation(p.direction))
      //control.setViewDirection(p.direction.getRotationColumn(0))
      player.rotate(reorientation._2)


      //player.setlocaltranslation(p.position)
      //player.setLocalRotation(p.direction)
    }
  }

}

