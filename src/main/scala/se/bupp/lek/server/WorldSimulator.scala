package se.bupp.lek.server

import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Model._
import collection.JavaConversions
import JavaConversions.asScalaBuffer
import se.bupp.lek.client.SceneGraphWorld
import com.jme3.asset.AssetManager
import com.jme3.bullet.control.{GhostControl, RigidBodyControl, CharacterControl}
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
import se.bupp.lek.common.model.{Playing, Dead}
import java.util


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/6/12
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */


class ProjectileCollisionControl(cs:CollisionShape) extends RigidBodyControl(cs) with PhysicsCollisionListener {

  def extractUserData(s:Spatial) : Option[_] = {
    var proj = (Option[ProjectileGO](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Projectile)),Option[PlayerConnection](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Player)))
    proj match {
      case (Some(pro),None) => Some(pro)
      case (None, Some(pro))  => Some(pro)
      case _ => None
    }
  }

  def collision(p1: PhysicsCollisionEvent) {
    //println("collision")
    (extractUserData(p1.getNodeA),extractUserData(p1.getNodeB)) match {
      case (Some(proj:ProjectileGO),Some(player:PlayerConnection)) => println("Player " + player.gameState.playerId + " died.")
      case (Some(player:PlayerConnection),Some(proj:ProjectileGO)) => println("Player " + player.gameState.playerId + " died.")
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

  def spawnProjectile(pr:ProjectileGO) {
      val instance = materializeProjectile2(pr)

    //val ctrl = new ProjectileCollisionControl()


    val sphereShape =
      new SphereCollisionShape(0.3f)
    val control = new RigidBodyControl(sphereShape)
    instance.setModelBound(new BoundingSphere())
    instance.updateModelBound()
    control.setLinearVelocity(pr.direction.getRotationColumn(0).mult(pr.speed))
    //control.setPhysicsRotation(p.direction);
    //control.setAngularVelocity(Vector3f.ZERO.clone())
    control.setMass(1.0f)
    control.setGravity(Vector3f.ZERO.clone())

    //control.setLinearDamping(0f)
    control.setKinematic(false)

    instance.addControl(control)
    getPhysicsSpace.add(control)


    //getPhysicsSpace.addCollisionListener(control)
  }

  def unspawnPlayer(s: Spatial, p:PlayerConnection) = {
    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).detachChild(s)
    val characterControl = s.getControl(classOf[CharacterControl])
    getPhysicsSpace.remove(characterControl)
    val ghostControl= s.getControl(classOf[GhostControl])
    getPhysicsSpace.remove(ghostControl)
  }

  def unspawnProjectile(s: Spatial, p:ProjectileGO) = {
    getNode(SceneGraphWorld.SceneGraphNodeKeys.Projectiles).detachChild(s)
    val rigidBodyControl = s.getControl(classOf[RigidBodyControl])
    getPhysicsSpace.remove(rigidBodyControl)
  }


  def spawnPlayer(ps:PlayerConnection) {
    println("Spawn player")
    val tankGeo = materializeTank2(ps.gameState)
    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    //val tank = new Node("Bupp")
    val tank = new Node("Tank")


    tank.attachChild(tankGeo)
    tankGeo.setLocalTranslation(Vector3f.ZERO.setY(-0.3f))
    tank.setUserData(SceneGraphUserDataKeys.Player, ps)

    //tank.attachChild(tankModel)
    val capsuleShape = new CapsuleCollisionShape(0.3f, 0.3f, 1)
    val capsuleShapeGhost = new CapsuleCollisionShape(0.5f, 0.5f, 1)

    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    tank.addControl(playerControl)

    getPhysicsSpace.add(playerControl)

    playerControl.setUseViewDirection(false)

    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0.3f);
    playerControl.setGravity(0.3f);
    playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));

    val ghost: GhostControl = new GhostControl(capsuleShapeGhost) /*{
      override def update(tpf:Float) {

        if (!enabled) {
          return;
        }
        val vectorf: Vector3f = {
          if (applyLocal) {
            spatial.getLocalTranslation()
          } else spatial.getWorldTranslation()

        }
        setPhysicsLocation(vectorf);
        val quaternion: Quaternion = {
          if (applyLocal) {
            spatial.getLocalRotation();
          } else spatial.getWorldRotation()
        }
        setPhysicsRotation(quaternion);

        //println("ghost update" + vectorf + " " + quaternion)
      }
    }*/

    //ghost.setApplyPhysicsLocal(true)
    tank.addControl(ghost);
    //getPhysicsSpace.add(ghost)

    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).attachChild(tank)
  }

  def getPhysicsSpace = physicsSpace
}



class WorldSimulator(world:ServerWorld) extends  PhysicsCollisionListener {

  world.getPhysicsSpace.addCollisionListener(this)

  def extractUserData(s:Spatial) : Option[_] = {
    var proj = (Option[ProjectileGO](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Projectile)),Option[PlayerConnection](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Player)))
    proj match {
    case (Some(pro),None) => Some(pro)
    case (None, Some(pro))  => Some(pro)
    case _ => None
    }
  }

  def collision(p1: PhysicsCollisionEvent) {
  //println("collision")
    (extractUserData(p1.getNodeA),extractUserData(p1.getNodeB)) match {
    case (Some(proj:ProjectileGO),Some(player:PlayerConnection)) => playerDied(p1.getNodeB, player)
    case (Some(player:PlayerConnection),Some(proj:ProjectileGO)) => playerDied(p1.getNodeA, player)
    case (Some(proj:ProjectileGO), None) => explodeProjectile(p1.getNodeA,proj)
    case (None, Some(proj:ProjectileGO)) => explodeProjectile(p1.getNodeB,proj)

    case _ => //println("unknown collision")
    }
  }


  def playerDied(s: Spatial, player: Model.PlayerConnection) {
    player.state match {
      case Dead(since) =>
      case Playing() =>
        println("Player " + player.gameState.playerId + " died.")

        deadSinceLastUpdate = deadSinceLastUpdate :+ player.playerId
        player.state = Dead(System.currentTimeMillis())
        world.unspawnPlayer(s,player)
    }
  }

  var exloadedSinceLastUpdate = Seq.empty[ProjectileGO]
  var deadSinceLastUpdate = Seq.empty[Int]


  def explodeProjectile(s:Spatial,proj:ProjectileGO) {

    if (exloadedSinceLastUpdate.forall(_.id != proj.id)) {
      world.unspawnProjectile(s,proj)
      proj.position = s.getControl(classOf[RigidBodyControl]).getPhysicsLocation
      exloadedSinceLastUpdate = exloadedSinceLastUpdate :+ proj
    }
  }

  var connectionSequence = 0

  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  //var getPlayers = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  var connectedPlayers = List[PlayerConnection]()

  //var projectiles = List[ProjectileGO]()

  var simulatedUntil:Option[Long] = None

  val RespawnTime = 1000L * 10L
  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()



  def getGameWorld() :  ServerGameWorld = {
    val simTime: Long = System.currentTimeMillis()

    getGameWorld(simTime)
  }

  def handleStateLogic() {
    connectedPlayers.foreach {
      cp => cp.state match {
        case Dead(since) => if(System.currentTimeMillis() - since > RespawnTime) {
          respawnDeadPlayer(cp)
        }
        case Playing() =>
      }
    }

  }


  def respawnDeadPlayer(cp: Model.PlayerConnection) {
    println("Respawning dead player " + cp.playerId)

    cp.state = Playing()
    world.spawnPlayer(cp)
  }

  def getGameWorld(simTime:Long):  ServerGameWorld = {

    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {
      


      world.simulateToLastUpdated()



      val players = world.getPlayers
      players.foreach {
        case (ps,s) =>
          ps.gameState.position = s.getControl(classOf[CharacterControl]).getPhysicsLocation.clone()
          ps.gameState.direction = s.getLocalRotation
      }


      val playerState = players.map {
        case (ps, s)  =>
          val p = new PlayerGO(ps.gameState)
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
              p.orientation  = pf.from
              p.timeSpawned = pf.timeStamp
              p.playerId = pid
              p.clientSeqId = pf.clientSeqId
              p.speed = pf.speed
              p
          }
      }

      newProjectiles.foreach {
        world.spawnProjectile(_)
      }


      firedProjectiles = HashMap.empty


      //projectiles = projectiles ++ newProjectiles

      val projectiles: Buffer[(ProjectileGO, Spatial)] = world.getProjectiles()

      val (toPreserve, toRemove)  = projectiles.partition { _._1.timeSpawned > maxAgeProjectiles }

      toRemove.foreach {
        case (p,s) => world.unspawnProjectile(s,p)
      }

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
      val deadPlayers = purgeDeadPlayersSinceLastUpdate
      val res: ServerGameWorld = new ServerGameWorld(

        deadPlayers = new java.util.ArrayList[Int](deadPlayers),
        alivePlayers = new util.ArrayList[PlayerGO](playerState),
        projectiles = new java.util.ArrayList[ProjectileGO](simulatedProjectiles),
        explodedProjectiles = new java.util.ArrayList[ProjectileGO](exploaded),
        timeStamp = simTime
      )
      res
    }
  }

  def purgeDeadPlayersSinceLastUpdate: List[Int] = {
    val res = deadSinceLastUpdate
    deadSinceLastUpdate = Seq.empty[Int]
    res.toList
  }

  def connectPlayer(pjr: PlayerJoinRequest): Int = {

    var playerId = -1

    lock.synchronized {

      playerId = connectionSequence
      val player = {
        val pd = new PlayerGO
        pd.playerId = playerId
        pd.position = Vector3f.ZERO.clone().setY(0.13499954f)
        pd.direction = Quaternion.DIRECTION_Z.clone()
        pd
        var ps = new PlayerConnection
        ps.gameState = pd
        //ps.lastUpdate = None
        ps
      }

      connectedPlayers = connectedPlayers :+ player
      world.spawnPlayer(player)

      connectionSequence += 1
    }
    playerId
  }

  def addPlayerAction(request: PlayerActionRequest) {
    lock.synchronized {


      connectedPlayers.find(_.playerId == request.playerId).map(_.state) match {
        case Some(Dead(since)) => //println("Discarding dead player " + request.playerId + " update")
        case Some(Playing()) =>

        world.findPlayer(request.playerId) match {
          case Some(x) =>
            //x.lastUpdate = Some(request)
            //x.processedUpdate = false

            x.gameState.sentToServerByClient = request.timeStamp
            request.motion.sentToServer = request.timeStamp
            x.updates = x.updates :+ request.motion

            x.seqId = request.seqId


            firedProjectiles.get(request.playerId) match {
              case Some(existing) => firedProjectiles(request.playerId) = existing ++ request.projectilesFired
              case None => firedProjectiles(request.playerId) = request.projectilesFired.toList
            }
           //.ensuring(x.position != null && x.direction != null)
          case None => throw new IllegalStateException("Player not found but connected and not dead")
        }
        case None => println("Player not connected")
      }
    }
  }
}
