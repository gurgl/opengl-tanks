package se.bupp.lek.server

import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Model._
import collection.JavaConversions
import JavaConversions.asScalaBuffer
import se.bupp.lek.client.SceneGraphWorld
import com.jme3.asset.AssetManager
import com.jme3.bullet.control.{RigidBodyControl, CharacterControl}
import se.bupp.lek.client.SceneGraphWorld.SceneGraphUserDataKeys
import com.jme3.bullet.collision.shapes.{CollisionShape, SphereCollisionShape, CapsuleCollisionShape}
import util.Random
import com.jme3.scene.{Spatial, Node}
import scalaz.NonEmptyList
import collection.mutable.{Buffer, HashMap, ArrayBuffer}
import com.jme3.bullet.{PhysicsSpace, BulletAppState}
import com.jme3.scene.control.Control
import com.jme3.bullet.collision.{PhysicsCollisionEvent, PhysicsCollisionListener}
import com.jme3.bounding.BoundingSphere


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/6/12
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */


class ProjectileCollisionControl(cs:CollisionShape) extends RigidBodyControl(cs) with PhysicsCollisionListener {

  def extractUserData(s:Spatial) : Option[_] = {
    var proj = (Option[ProjectileGO](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Projectile)),Option[PlayerStatus](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Player)))
    proj match {
      case (Some(pro),None) => Some(pro)
      case (None, Some(pro))  => Some(pro)
      case _ => None
    }
  }

  def collision(p1: PhysicsCollisionEvent) {
    //println("collision")
    (extractUserData(p1.getNodeA),extractUserData(p1.getNodeB)) match {
      case (Some(proj:ProjectileGO),Some(player:PlayerStatus)) => println("unknown collision p")
      case (Some(player:PlayerStatus),Some(proj:ProjectileGO)) => println("unknown collision p ")
      case (Some(proj:ProjectileGO),None) => println("Projectile lvl")
      case (None, Some(proj:ProjectileGO)) => println("Projectile lvl")
      case _ => //println("unknown collision")
    }
  }
}

class ServerWorld(rootNode: Node, assetManager:AssetManager, physicsSpace:PhysicsSpace) extends SceneGraphWorld(true,assetManager,rootNode) with PhysicsSpaceSimAdapter {

  var simCurrentTime:Long = System.currentTimeMillis()

  def initEmpty() {
    super.init()
  }

  def getProjectiles() = projectNodeChildrenByData[ProjectileGO](SceneGraphWorld.SceneGraphNodeKeys.Projectiles, SceneGraphWorld.SceneGraphUserDataKeys.Projectile)

  def addProjectile(pr:ProjectileGO) {
    val instance = materializeProjectile2(pr)

    //val ctrl = new ProjectileCollisionControl()


    val sphereShape =
      new SphereCollisionShape(0.1f)
    val control = new RigidBodyControl(sphereShape)
    instance.setModelBound(new BoundingSphere())
    instance.updateModelBound()
    control.setLinearVelocity(pr.direction.getRotationColumn(0).mult(pr.speed))
    //control.setPhysicsRotation(p.direction);
    //control.setAngularVelocity(Vector3f.ZERO.clone())
    control.setMass(0.1f)
    control.setGravity(Vector3f.ZERO.clone())

    //control.setLinearDamping(0f)
    control.setKinematic(false)

    instance.addControl(control)
    getPhysicsSpace.add(control)


    //getPhysicsSpace.addCollisionListener(control)
  }


  def addPlayer(ps:PlayerStatus) {
    val tank = materializeTank2(ps.state)
    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    //val tank = new Node("Bupp")
    tank.setUserData(SceneGraphUserDataKeys.Player, ps)

    //tank.attachChild(tankModel)
    val capsuleShape = new CapsuleCollisionShape(0.05f, 0.05f, 1)
    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    tank.addControl(playerControl)

    getPhysicsSpace.add(playerControl)

    playerControl.setUseViewDirection(false)

    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0.3f);
    playerControl.setGravity(0.3f);
    playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));

    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).attachChild(tank)
  }

  def getPhysicsSpace = physicsSpace
}



class WorldSimulator(world:ServerWorld) extends  PhysicsCollisionListener {

  world.getPhysicsSpace.addCollisionListener(this)

  def extractUserData(s:Spatial) : Option[_] = {
    var proj = (Option[ProjectileGO](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Projectile)),Option[PlayerStatus](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Player)))
    proj match {
    case (Some(pro),None) => Some(pro)
    case (None, Some(pro))  => Some(pro)
    case _ => None
    }
  }

  def collision(p1: PhysicsCollisionEvent) {
  //println("collision")
    (extractUserData(p1.getNodeA),extractUserData(p1.getNodeB)) match {
    case (Some(proj:ProjectileGO),Some(player:PlayerStatus)) => println("unknown collision p")
    case (Some(player:PlayerStatus),Some(proj:ProjectileGO)) => println("unknown collision p ")
    case (Some(proj:ProjectileGO), None) => explodeProjectile(p1.getNodeA,proj)
    case (None, Some(proj:ProjectileGO)) => explodeProjectile(p1.getNodeB,proj)

    case _ => //println("unknown collision")
    }
  }

  var exloadedSinceLastUpdate = Seq.empty[ProjectileGO]

  def explodeProjectile(s:Spatial,proj:ProjectileGO) {
    world.getNode(SceneGraphWorld.SceneGraphNodeKeys.Projectiles).detachChild(s)
    proj.position = s.getControl(classOf[RigidBodyControl]).getPhysicsLocation
    exloadedSinceLastUpdate = exloadedSinceLastUpdate :+ proj
  }

  var connectionSequence = 0

  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  //var getPlayers = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  //var projectiles = List[ProjectileGO]()

  var simulatedUntil:Option[Long] = None

  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()



  def getGameWorld() :  ServerGameWorld = {
    val simTime: Long = System.currentTimeMillis()

    getGameWorld(simTime)
  }


  def getGameWorld(simTime:Long):  ServerGameWorld = {

    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {
      


      world.simulateToLastUpdated()



      val players = world.getPlayers
      players.foreach {
        case (ps,s) =>
          ps.state.position = s.getControl(classOf[CharacterControl]).getPhysicsLocation.clone()
          ps.state.direction = s.getLocalRotation
      }


      val playerState = players.map {
        case (ps, s)  =>
          val p = new PlayerGO(ps.state)
          p.sentToServerByClient = ps.lastSimulation
          p
      }
      /*val playerState = getPlayers.map {
        p =>
          p.state
      }*/
      
      

      //println("pos " + playerState.map ( p => p.playerId + " " + p.position).mkString(", "))
      s = playerState.map(x => " | " + x.playerId + " " + x.position + " " + x.direction).mkString(",")

      val maxAgeProjectiles = simTime - 5 * 1000

      val newProjectiles = firedProjectiles.flatMap {
        case (pid, list) =>
          list.map {
            pf =>

              val p = new ProjectileGO()
              p.orientation = pf.from
              p.timeSpawned = pf.timeStamp
              p.playerId = pid
              p.clientSeqId = pf.clientSeqId
              p.speed = pf.speed
              p
          }
      }

      newProjectiles.foreach {
        world.addProjectile(_)
      }


      firedProjectiles = HashMap.empty


      //projectiles = projectiles ++ newProjectiles

      val projectiles: Buffer[(ProjectileGO, Spatial)] = world.getProjectiles()

      val (toPreserve, toRemove)  = projectiles.partition { _._1.timeSpawned > maxAgeProjectiles }

      toRemove.foreach {
        case (p,s) => world.getNode(SceneGraphWorld.SceneGraphNodeKeys.Projectiles).detachChild(s)
      }





      /*lastWorldSimTimeStamp.foreach { lastSimTime =>
        projectiles.foreach {
          pf =>
          //pf.
            val translate = pf.orientation.direction.getRotationColumn(0).mult(pf.speed * (simTime - lastSimTime).toFloat / 1000f)
            //println("translate " + translate + translate.length)
            pf.orientation.position = pf.orientation.position.add(translate)
        }
      }*/

      //println("projectiles.size" + projectiles.size + " newProjectiles " + newProjectiles.size)

      /*

      firedProjectiles = firedProjectiles.map { case (k,vl) =>
       val remaining = vl.filter { pf => pf.timeStamp > maxAgeProjectiles }
       (k, remaining)
      }.filter {
        case (k,vl) => vl.size > 0
      }
      */

      toPreserve.foreach {
        case (p,s) =>

          p.position = s.getControl(classOf[RigidBodyControl]).getPhysicsLocation
          //println(p.position)
      }
      val simulatedProjectiles = toPreserve.map {
        case (p,s) =>
          p
      }

      if(exloadedSinceLastUpdate.size > 0) {
        println(exloadedSinceLastUpdate.size + " exploaded ")
      }
      val exploaded = exloadedSinceLastUpdate
      exloadedSinceLastUpdate = Seq.empty[ProjectileGO]

      lastWorldSimTimeStamp = Some(world.simCurrentTime)
      new ServerGameWorld(
        players = new java.util.ArrayList[PlayerGO](playerState),
        projectiles = new java.util.ArrayList[ProjectileGO](simulatedProjectiles),
        explodedProjectiles = new java.util.ArrayList[ProjectileGO](exploaded),
        timeStamp = simTime
      )
    }
  }

  def addPlayer(pjr: PlayerJoinRequest): Int = {

    var playerId = -1

    lock.synchronized {

      playerId = connectionSequence
      val player = {
        val pd = new PlayerGO
        pd.playerId = playerId
        pd.position = Vector3f.ZERO.clone().setY(0.13499954f)
        pd.direction = Quaternion.DIRECTION_Z.clone()
        pd
        var ps = new PlayerStatus
        ps.state = pd
        //ps.lastUpdate = None
        ps
      }
      world.addPlayer(player)

      connectionSequence += 1
    }
    playerId
  }

  def addPlayerAction(request: PlayerActionRequest) {
    lock.synchronized {


      world.findPlayer(request.playerId).foreach {
        x => {
          //x.lastUpdate = Some(request)
          //x.processedUpdate = false

          x.state.sentToServerByClient = request.timeStamp
          request.motion.sentToServer = request.timeStamp
          x.reorientation = x.reorientation :+ request.motion

          x.seqId = request.seqId


          firedProjectiles.get(request.playerId) match {
            case Some(existing) => firedProjectiles(request.playerId) = existing ++ request.projectilesFired
            case None => firedProjectiles(request.playerId) = request.projectilesFired.toList
          }
        } //.ensuring(x.position != null && x.direction != null)
      }
    }
  }
}
