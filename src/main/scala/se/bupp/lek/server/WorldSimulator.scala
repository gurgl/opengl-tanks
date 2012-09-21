package se.bupp.lek.server

import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Model._
import collection.{immutable, mutable, JavaConversions}
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
import org.apache.log4j.Logger


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




abstract class WorldSimulator(val world:ServerWorld) extends  PhysicsCollisionListener {

  val log = Logger.getLogger(classOf[WorldSimulator])

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
    case (Some(proj:ProjectileGO),Some(player:PlayerConnection)) => playerDied(p1.getNodeB, player, proj.playerId) ; explodeProjectile(p1.getNodeA,proj)
    case (Some(player:PlayerConnection),Some(proj:ProjectileGO)) => playerDied(p1.getNodeA, player, proj.playerId) ; explodeProjectile(p1.getNodeB,proj)
    case (Some(proj:ProjectileGO), None) => explodeProjectile(p1.getNodeA,proj)
    case (None, Some(proj:ProjectileGO)) => explodeProjectile(p1.getNodeB,proj)

    case _ => //println("unknown collision")
    }
  }

  def playerKilledPlayer(killer:Int, victim:Int)

  def playerDied(s: Spatial, player: Model.PlayerConnection, killer:Int) {
    player.state match {
      case Dead(since) =>
      case Playing() =>
        log.info("Player " + player.gameState.playerId + " died.")



        deadSinceLastUpdate = deadSinceLastUpdate :+ player.playerId
        player.state = Dead(System.currentTimeMillis())
        //world.unspawnPlayer(s,player)
        spatialsToRemoveInUpdatePhase = spatialsToRemoveInUpdatePhase :+ (s, player)
        playerKilledPlayer(killer,player.playerId)

    }
  }

  var exloadedSinceLastUpdate = Seq.empty[ProjectileGO]
  var deadSinceLastUpdate = Seq.empty[Int]

  var spatialsToRemoveInUpdatePhase = immutable.Seq.empty[(Spatial,_ <: ScalaObject)]


  def explodeProjectile(s:Spatial,proj:ProjectileGO) {

    if (exloadedSinceLastUpdate.forall(_.id != proj.id)) {
      //world.unspawnProjectile(s,proj)
      proj.position = s.getControl(classOf[RigidBodyControl]).getPhysicsLocation
      exloadedSinceLastUpdate = exloadedSinceLastUpdate :+ proj

      spatialsToRemoveInUpdatePhase = spatialsToRemoveInUpdatePhase :+ (s, proj)
    }
  }


  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  //var getPlayers = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  //var projectiles = List[ProjectileGO]()

  var participatingPlayers = List[PlayerConnection]()

  var simulatedUntil:Option[Long] = None

  val RespawnTime = 1000L * 10L
  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()




  //getGameWorld(simTime)

  def handleStateLogic() {

    if (spatialsToRemoveInUpdatePhase.size > 0) {
      log.info("spatialsToRemoveInUpdatePhase " + spatialsToRemoveInUpdatePhase.size)
    }

    spatialsToRemoveInUpdatePhase.foreach {
      case (s:Spatial,pc:PlayerConnection) =>
        val bef = world.getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).getChildren.size
        world.unspawnPlayer(s,pc)
        log.info(world.getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).getChildren.size + " " + bef )

      case (s:Spatial,pc:ProjectileGO) => world.unspawnProjectile(s,pc)
      case _ => throw new IllegalStateException("Unhandled object in remove queue")

    }

    spatialsToRemoveInUpdatePhase = spatialsToRemoveInUpdatePhase.companion.empty

    participatingPlayers.foreach {
      cp => cp.state match {
        case Dead(since) => if(System.currentTimeMillis() - since > RespawnTime) {
          respawnDeadPlayer(cp)
        }
        case Playing() =>
      }
    }

  }


  def respawnDeadPlayer(cp: Model.PlayerConnection) {
    log.info("Respawning dead player " + cp.playerId)

    cp.state = Playing()
    world.spawnPlayer(cp)
  }

  def getGameWorld(simTime:Long):  ServerGameWorld = {

    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {

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


      val exploaded = Seq.empty ++: exloadedSinceLastUpdate
      exloadedSinceLastUpdate = Seq.empty[ProjectileGO]



      //if(simulatedProjectiles.size > 0) simulatedProjectiles.foreach { p => print(p.id) }
      lastWorldSimTimeStamp = Some(world.simCurrentTime)
      val deadPlayers = purgeDeadPlayersSinceLastUpdate.ensuring(_.forall( dp => !playerState.exists(_.playerId == dp)))


      if(deadPlayers.size > 0 ) {
        log.info(deadPlayers.size + "DDDDDDDDDDDDDDDDDDDDDDEAD")
        deadPlayers.foreach( p => log.info(p))
      }
      val res: ServerGameWorld = new ServerGameWorld(

        deadPlayers = new java.util.ArrayList[Int](deadPlayers),
        alivePlayers = new util.ArrayList[PlayerGO](playerState),
        projectiles = new java.util.ArrayList[ProjectileGO](simulatedProjectiles),
        explodedProjectiles = new java.util.ArrayList[ProjectileGO](exploaded),
        timeStamp = simTime,
        seqId = -1
      )
      if(exploaded.size > 0) {
        log.info(exploaded.size + " exploaded ")
      }

      res
    }
  }

  def purgeDeadPlayersSinceLastUpdate: List[Int] = {
    val res = deadSinceLastUpdate
    deadSinceLastUpdate = Seq.empty[Int]
    res.toList
  }


  def removeParticipant(playerId:Int) {

    val (pcOpt, newParticipatingPlayers) = participatingPlayers.partition(_.playerId == playerId)
    participatingPlayers = newParticipatingPlayers
    pcOpt match {
      case Nil =>
      case pc :: rest => world.unspawnPlayer(pc)
    }
  }

  def addParticipant(pjr: PlayerConnection) = {

    lock.synchronized {

      val pd = new PlayerGO
      pd.playerId = pjr.playerId
      pd.position = Vector3f.ZERO.clone().setY(0.13499954f)
      pd.direction = Quaternion.DIRECTION_Z.clone()

      pjr.gameState = pd

      participatingPlayers = participatingPlayers :+ pjr

      world.spawnPlayer(pjr)
    }

  }



  def addPlayerAction(request: PlayerActionRequest) {
    lock.synchronized {


      participatingPlayers.find(_.playerId == request.playerId).map(_.state) match {
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
          case None => log.error("BUG: Player not found but connected and not dead")
        }
        case None => log.warn("Player not connected")
      }
    }
  }

  def unspawnAllGameObjects() {
    participatingPlayers.foreach { pc => world.unspawnPlayer(pc) }
    world.getProjectiles().foreach { case (pgo,s) => world.unspawnProjectile(s,pgo) }

  }

  def spawnAllParticipants() {
    participatingPlayers.foreach{
      ps =>

        val pd = new PlayerGO
        pd.playerId = ps.playerId
        pd.position = Vector3f.ZERO.clone().setY(0.13499954f)
        pd.direction = Quaternion.DIRECTION_Z.clone()

        ps.gameState = pd

        ps.state = Playing()

        world.spawnPlayer(ps)
    }
  }

  def removeAndRespawnAll() {

  }
}
