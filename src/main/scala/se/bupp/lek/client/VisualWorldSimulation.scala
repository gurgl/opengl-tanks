package se.bupp.lek.client

import com.jme3.scene.shape.Box
import com.jme3.bounding.BoundingSphere
import com.jme3.scene.{Spatial, Node, Geometry}
import com.jme3.asset.{ModelKey, AssetManager}
import com.jme3.export.Savable
import scala.collection.JavaConversions.asScalaBuffer
import com.jme3.material.{MaterialDef, MaterialList, Material}
import com.jme3.shadow.BasicShadowRenderer
import com.jme3.renderer.{RenderManager, ViewPort}
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import com.jme3.light.{AmbientLight, DirectionalLight}
import com.jme3.post.FilterPostProcessor
import com.jme3.post.ssao.SSAOFilter
import com.jme3.util.TangentBinormalGenerator
import se.bupp.lek.server.Model._
import com.jme3.bullet.util.CollisionShapeFactory
import com.jme3.bullet.{PhysicsSpace, BulletAppState}
import com.jme3.bullet.collision.shapes.{SphereCollisionShape, CapsuleCollisionShape}
import com.jme3.bullet.control.{CharacterControl, RigidBodyControl}
import com.jme3.asset.plugins.ZipLocator
import com.jme3.math.{FastMath, ColorRGBA, Quaternion, Vector3f}
import collection.immutable.Queue._
import collection.immutable.{Queue, Stack, HashSet}
import com.jme3.effect.{ParticleMesh, ParticleEmitter}
import scala.Tuple3
import scala.Some
import com.jme3.scene.control.AbstractControl
import se.bupp.lek.server.{Model, SceneGraphAccessors}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/4/12
 * Time: 3:57 PM
 * To change this template use File | Settings | File Templates.
 */


object VisualWorldSimulation {

  type VisualGameWorld = (Set[Model.AbstractOwnedGameObject with Savable] , Model.ServerGameWorld)
}


abstract class LogicalSimulation() {


}

class VisualWorldSimulation(val rootNode:Node,val assetManager:AssetManager, playerIdOpt:() => Option[Int],val playerInput:PlayerInput, viewPort:ViewPort, val bulletAppState:BulletAppState) extends SceneGraphWorld(false,assetManager,rootNode) with SceneGraphAccessors {
  import VisualWorldSimulation._
  import SceneGraphWorld._

  override def getPhysicsSpace = bulletAppState.getPhysicsSpace

  var playerDead = false

  var projectileSeqId = 0

  var fired = Stack[ProjectileFireGO]()

  val lock:AnyRef = new Object

  val LocalInputLogSize = 20

  var saved = Queue.empty[(Long, Orientation, Reorientation)] //:+ ((System.currentTimeMillis(),startPosition, MathUtil.noMotion))


  var sun:DirectionalLight = _
  var al:AmbientLight = _


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

      //val orientation = saved.last._2.reorientate(input)
      saved =  saved + (timeStamp, orientation, reorientation)
    }
  }

  def init(playerPosition:Orientation) {
    super.init()


    rootNode.setShadowMode(ShadowMode.Off)


    materializePlayer(playerPosition)

    //materializeStats()


    // You must add a light to make the model visible
    sun = new DirectionalLight();

    //sun.setColor(ColorRGBA.White)
    val sunDirection: Vector3f = new Vector3f(-1, -1, -1).normalizeLocal()
    sun.setDirection(sunDirection);
    sun.setColor(ColorRGBA.Green)
    rootNode.addLight(sun);

    al = new AmbientLight();
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
          new OrientationGO(pos.add(0f,0.33f,0.0f).add(dir.getRotationColumn(0).mult(0.7f)),dir.clone()),
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

  def handleKilledPlayers(playersToKill:List[Int]) {

    playersToKill.foreach {
      playerId =>
        if (playerId == playerIdOpt().get) {
          rootNode.detachChild(player)
          playerDead = true
          println("You died")
        } else {
          val enemies = projectNodeChildrenByData[PlayerGO](SceneGraphNodeKeys.Enemies, SceneGraphUserDataKeys.Player).toMap
          enemies.foreach {
            case (p, s) => if (p.playerId == playerId) {
              println("Player " + playerId + " died")
              getNode(SceneGraphNodeKeys.Enemies).detachChild(s)
            }
          }
        }
    }
  }

  def syncNonPlayerGameWorld(allUpdates:Set[_ <: AbstractOwnedGameObject with Savable]) {

    import scala.collection.JavaConversions.asScalaBuffer
    //val enemyNodes = rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].getChildren
    //val projectileNodes = rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].getChildren
    val enemyMap = projectNodeChildrenByData[PlayerGO](SceneGraphNodeKeys.Enemies, SceneGraphUserDataKeys.Player).toMap //enemyNodes.map( e => (e.getUserData[PlayerGO](SceneGraphUserDataKeys.Player),e).ensuring(_._1 != null) ).toMap
    val projectileMap = projectNodeChildrenByData[ProjectileGO](SceneGraphNodeKeys.Projectiles, SceneGraphUserDataKeys.Projectile).toMap  //projectileNodes.map( e => (e.getUserData[ProjectileGO](SceneGraphUserDataKeys.Projectile),e).ensuring(_._1 != null) ).toMap

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

    // if (k.position.getX == Float.NaN)
    //enemyMap.foreach {  case (k,v) => println( "pos " +  k.position + " " + v.getLocalTranslation()) }


    newInUpdate.foreach {
      case p:PlayerGO =>
        if(p.playerId == playerIdOpt.apply().get) {
          // TODO (player spawn etc)
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

  var dbgLastPlayerPos:Vector3f = _

  def generateLocalGameWorld(simTime: Long,currentGameWorldUpdates:Queue[Model.ServerGameWorld]): Option[VisualGameWorld] = {
    if(currentGameWorldUpdates.size > 0) {

      val prediction: Set[AbstractOwnedGameObject with Savable] = calculatePrediction(simTime, currentGameWorldUpdates, playerIdOpt().get)

      Some((prediction, currentGameWorldUpdates.last))
    } else None
  }

  def calculatePrediction(simTime: Long,currentGameWorldUpdates:Queue[ServerGameWorld], playerId:Int) = {

    val predictor: VisualSimulationPrediction = new VisualSimulationPrediction(currentGameWorldUpdates, playerId)
    val nonPlayerPredictons = predictor.interpolateNonPlayerObjects(simTime)

    nonPlayerPredictons.distinct.toSet
  }

  def updateGameWorld(visualGameWorld:VisualGameWorld, reorientation:Reorientation) {

    val (nonPlayerPredictons:Set[AbstractOwnedGameObject with Savable], lastGameWorldUpdate: ServerGameWorld) = visualGameWorld
    syncNonPlayerGameWorld(nonPlayerPredictons)

    if(lastSynchedGameWorldUpdate != lastGameWorldUpdate) {
      applyServerWorld(lastGameWorldUpdate)
      lastSynchedGameWorldUpdate = lastGameWorldUpdate
    }


    applyPlayerInput(lastGameWorldUpdate,reorientation)

    val dbgPlayerPos = player.getControl(classOf[CharacterControl]).getPhysicsLocation.clone()

    /*
    if (dbgLastPlayerPos != null) {
      val dist: Float = dbgLastPlayerPos.subtract(dbgPlayerPos).length()
      if (math.abs(dist) < 0.001 ) {
        println("Zero dist" + dist + dbgLastPlayerPos + " " + dbgPlayerPos)
      }
    }*/
    dbgLastPlayerPos = dbgPlayerPos

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
        if(diffHeur._1 > 0.2 || diffHeur._2 > FastMath.PI / 90) {

          val newSavedPos = saved.foldLeft(Queue(server)) {
            case (recalculatedPositions,(time,orientationBeforeReorientation, reorient)) =>
              recalculatedPositions :+ recalculatedPositions.last.reorientate(reorient)
          }
          println("Bad " + saved.head._2.position + " " + server.position + " " + serverSimTime + " " + diffHeur + " " + serverSnapshotSentByPlayerTime)
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

  var lastSynchedGameWorldUpdate:ServerGameWorld = _

  def applyServerWorld(newServerState:ServerGameWorld) {

    newServerState.explodedProjectiles.foreach {
      p => println("explosiion")
      explosion(p.position.clone())
    }

    // Only check if player alive (TODO: Rewrite for readablity)
    newServerState.alivePlayers.find(_.playerId == playerIdOpt().get).foreach {
      x =>

      // TODO: Might be able to do this when server state arrives instead of on update
        applyCorrectionIfDiffers(x.sentToServerByClient, newServerState.timeStamp, x)

      //player.move(playerinput.translation)
      //player.rotate(playerinput.rotation)

    }
  }
  def applyPlayerInput(lastGameWorldUpdate: ServerGameWorld, input:Reorientation) {

      // Only check if player alive (TODO: Rewrite for readablity)
    lastGameWorldUpdate.alivePlayers.find(_.playerId == playerIdOpt().get).foreach {
      x =>
        val control = player.getControl(classOf[CharacterControl])


      //println(direction + " " + bulletAppState.getSpeed + " " + bulletAppState.getPhysicsSpace.getAccuracy)
      control.setWalkDirection(input._1)
      //control.setviewdirection(player.setlocalrotation(p.direction))
      //control.setViewDirection(p.direction.getRotationColumn(0))
      player.rotate(input._2)


      //player.setlocaltranslation(p.position)
      //player.setLocalRotation(p.direction)
    }


  }

  class DestroyControl extends AbstractControl
  {
    val time = System.currentTimeMillis();
    var killed = false

    override def controlUpdate(tpf:Float)
    {
      if(!killed && System.currentTimeMillis() - time < 500)
      {
        val emitter: ParticleEmitter = this.getSpatial().asInstanceOf[ParticleEmitter]
        //println("emit all")
        emitter.emitAllParticles()
      }
      else if(!killed && (System.currentTimeMillis() - time) > 1000)
      {
        val emitter: ParticleEmitter = this.getSpatial().asInstanceOf[ParticleEmitter]
        emitter.killAllParticles()
        //println("kill")
        killed = true
      } else if(killed && System.currentTimeMillis() - time > 3000)
      {
        //println("detach")
        this.getSpatial().getParent.detachChild(this.getSpatial())
      }
    }

    def cloneForSpatial(p1: Spatial) = null

    def controlRender(p1: RenderManager, p2: ViewPort) {}
  }


  def explosion(pos:Vector3f) {
    val fire =
      new ParticleEmitter("Emitter", ParticleMesh.Type.Triangle, 30);
    val mat_red = new Material(assetManager,
      "Common/MatDefs/Misc/Particle.j3md");
    mat_red.setTexture("Texture", assetManager.loadTexture(
      "Effects/Explosion/flame.png"));
    fire.setMaterial(mat_red);
    fire.setImagesX(2);
    fire.setImagesY(2); // 2x2 texture animation
    fire.setEndColor(  new ColorRGBA(1f, 0f, 0f, 1f));   // red
    fire.setStartColor(new ColorRGBA(1f, 1f, 0f, 0.5f)); // yellow
    fire.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 2, 0));
    fire.setStartSize(0.3f);
    fire.setEndSize(0.1f);
    fire.setGravity(0, 0, 0);
    fire.setLowLife(1f);
    fire.setHighLife(3f);
    fire.getParticleInfluencer().setVelocityVariation(0.3f);
    fire.setLocalTranslation(pos)
    fire.emitAllParticles()
    getNode(SceneGraphNodeKeys.Effects).attachChild(fire)
    fire.killAllParticles()
    fire.addControl(new DestroyControl)
    fire.setParticlesPerSec(0)

    /*
    val explosion = new ParticleEmitter(
      "My explosion effect", ParticleMesh.Type.Triangle, 30);
    //val the emitter to the rootNode and position it in the scene:
    rootNode.attachChild(explosion);

    val mat_red = new Material(assetManager,"Common/MatDefs/Misc/Particle.j3md")
    mat_red.setTexture("Texture", assetManager.loadTexture("Effects/Explosion/flame.png"));
    explosion.setMaterial(mat_red)
    explosion.setLocalTranslation(pos);
    //Trigger the effect by calling
    explosion.emitAllParticles()
    //End the effect by calling
    explosion.killAllParticles
    //rootNode.attachChild(explosion)
    */

  }

  override def destroy() {
    super.destroy()
    rootNode.removeLight(al)
    rootNode.removeLight(sun)

  }

}

