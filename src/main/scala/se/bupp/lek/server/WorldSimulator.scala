package se.bupp.lek.server

import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Model._
import collection.{immutable, mutable, JavaConversions}
import JavaConversions.asScalaBuffer
import se.bupp.lek.client.SceneGraphWorld
import com.jme3.asset.AssetManager
import com.jme3.bullet.control.{GhostControl, RigidBodyControl, CharacterControl}
import se.bupp.lek.client.SceneGraphWorld.{SceneGraphNodeKeys, SceneGraphUserDataKeys}
import com.jme3.bullet.collision.shapes.{CollisionShape, SphereCollisionShape, CapsuleCollisionShape}
import util.Random
import com.jme3.scene.{Spatial, Node}
import scalaz.NonEmptyList
import collection.mutable.{Buffer, HashMap, ArrayBuffer}
import com.jme3.bullet.{PhysicsSpace, BulletAppState}
import com.jme3.scene.control.Control
import com.jme3.bullet.collision.{PhysicsCollisionObject, PhysicsCollisionGroupListener, PhysicsCollisionEvent, PhysicsCollisionListener}
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
    var proj = (Option[ProjectileGO](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Projectile)),Option[GameParticipant](s.getUserData(SceneGraphWorld.SceneGraphUserDataKeys.Player)))
    proj match {
      case (Some(pro),None) => Some(pro)
      case (None, Some(pro))  => Some(pro)
      case _ => None
    }
  }

  def collision(p1: PhysicsCollisionEvent) {
    //println("collision")
    (extractUserData(p1.getNodeA),extractUserData(p1.getNodeB)) match {
      case (Some(proj:ProjectileGO),Some(player:GameParticipant)) => println("Player " + player.gameState.playerId + " died.")
      case (Some(player:GameParticipant),Some(proj:ProjectileGO)) => println("Player " + player.gameState.playerId + " died.")
      case (Some(proj:ProjectileGO),None) => println("Projectile lvl")
      case (None, Some(proj:ProjectileGO)) => println("Projectile lvl")
      case _ => //println("unknown collision")
    }
  }
}




abstract class WorldSimulator(val world:ServerWorld) extends PhysicsCollisionListener {

  var destroyed = false

  val log = Logger.getLogger(classOf[WorldSimulator])

  //world.getPhysicsSpace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_02 | PhysicsCollisionObject.COLLISION_GROUP_03)
  world.getPhysicsSpace.addCollisionListener(this)

  def extractUserData(s:Spatial) : Option[_] = {
    var projOpt = s.getUserData[ProjectileGO](SceneGraphWorld.SceneGraphUserDataKeys.Projectile)
    var playOpt = s.getUserData[GameParticipant](SceneGraphWorld.SceneGraphUserDataKeys.Player)
    var proj = (Option(projOpt),Option(playOpt))
    proj match {
    case (Some(pro),None) => Some(pro)
    case (None, Some(pro))  => Some(pro)
    case _ => None
    }
  }

  val levelProjColl = PhysicsCollisionObject.COLLISION_GROUP_01 | PhysicsCollisionObject.COLLISION_GROUP_03
  val tankProjColl = PhysicsCollisionObject.COLLISION_GROUP_02 | PhysicsCollisionObject.COLLISION_GROUP_03

  def collision(p1: PhysicsCollisionEvent) {
    val a = p1.getObjectA.getCollisionGroup
    val b = p1.getObjectB.getCollisionGroup

    val colType = a | b
    //log.debug("collision " + colType)


    if (colType == levelProjColl || colType == tankProjColl) {
      log.debug("spat " + p1.getNodeA + " " + p1.getNodeB)
      (extractUserData(p1.getNodeA),extractUserData(p1.getNodeB)) match {
      case (Some(proj:ProjectileGO),Some(player:GameParticipant)) => playerDied(p1.getNodeB, player, proj.playerId) ; explodeProjectile(p1.getNodeA,proj)
      case (Some(player:GameParticipant),Some(proj:ProjectileGO)) => playerDied(p1.getNodeA, player, proj.playerId) ; explodeProjectile(p1.getNodeB,proj)
      case (Some(proj:ProjectileGO), None) => explodeProjectile(p1.getNodeA,proj)
      case (None, Some(proj:ProjectileGO)) => explodeProjectile(p1.getNodeB,proj)

      case _ => log.error("Collision with unknown objects " + colType + " " + p1.getNodeA.getName + " " + p1.getNodeB.getName)
      }
    }
  }
  def collides(p1: PhysicsCollisionObject, p2: PhysicsCollisionObject) = {

    log.debug("collision group")
    (p1.getUserObject,p2.getUserObject) match {
      case (Some(proj:ProjectileGO),Some(player:GameParticipant)) => playerDied(null, player, proj.playerId) ; explodeProjectile(null,proj); true
      case (Some(player:GameParticipant),Some(proj:ProjectileGO)) => playerDied(null, player, proj.playerId) ; explodeProjectile(null,proj); true
      case (Some(proj:ProjectileGO), None) => explodeProjectile(null ,proj) ; true
      case (None, Some(proj:ProjectileGO)) => explodeProjectile(null ,proj) ; true

      case _ => //log.error("Collision with unknown objects")
        false
    }
  }

  def playerKilledPlayer(killer:Int, victim:Int)

  def playerDied(s: Spatial, player: Model.GameParticipant, killer:Int) {
    updateLoopMessages = updateLoopMessages :+ PlayerKillPlayerMessage(player,killer)
  }

  var updateLoopMessages = mutable.Seq.empty[ServerPlayStateMessage]



  def handlePlayerDeathMessage(pkpm: PlayerKillPlayerMessage) {

    pkpm.player.state match {
      case Dead(since) =>
      case Playing() =>
        world.findPlayerInfo(pkpm.player.playerId) match {
          case Some((_,s)) =>
            log.info("Player " + pkpm.player.gameState.playerId + " died.")



            deadSinceLastUpdate = deadSinceLastUpdate :+ pkpm.player.playerId
            pkpm.player.state = Dead(System.currentTimeMillis())
            //world.unspawnPlayer(s,player)
            spatialsToRemoveInUpdatePhase = spatialsToRemoveInUpdatePhase :+ (s, pkpm.player)
            playerKilledPlayer(pkpm.killer,pkpm.player.playerId)
          case None => throw new IllegalStateException("Bad")
        }
    }


  }

  type ServerUpdateLoopMessage = (Spatial,_ <: ScalaObject)


  var exloadedSinceLastUpdate = Seq.empty[ProjectileGO]
  var deadSinceLastUpdate = Seq.empty[Int]

  var spatialsToRemoveInUpdatePhase = immutable.Seq.empty[ServerUpdateLoopMessage]


  def explodeProjectile(s:Spatial,proj1:ProjectileGO) {

    world.find[ProjectileGO](SceneGraphNodeKeys.Projectiles,SceneGraphUserDataKeys.Projectile,proj1).foreach {
    //world.getNode(SceneGraphNodeKeys.Projectiles).
      case (proj,s) =>
      if (exloadedSinceLastUpdate.forall(_.id != proj.id)) {
        //world.unspawnProjectile(s,proj)
        proj.position = s.getControl(classOf[RigidBodyControl]).getPhysicsLocation
        exloadedSinceLastUpdate = exloadedSinceLastUpdate :+ proj

        spatialsToRemoveInUpdatePhase = spatialsToRemoveInUpdatePhase :+ (s, proj)
      }
    }
  }


  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  //var getPlayers = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  //var projectiles = List[ProjectileGO]()

  var participatingPlayers = List[GameParticipant]()

  var simulatedUntil:Option[Long] = None

  val RespawnTime = 1000L * 3L
  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()

  var lastGeneratedUpdate:Option[ServerGameWorld] = None


  //generateGameWorldChanges(simTime)

  def handleStateLogic() {



    updateLoopMessages.synchronized {
      val s = Seq.empty ++: updateLoopMessages
      updateLoopMessages = updateLoopMessages.companion.empty
      s
    }.foreach { mm => mm match {
        case m : PlayerKillPlayerMessage => handlePlayerDeathMessage(m)
      }
      if(destroyed) return
    }

    // Deal with the fact that game can have ended
    if(destroyed) return

    if (spatialsToRemoveInUpdatePhase.size > 0) {
      log.info("spatialsToRemoveInUpdatePhase " + spatialsToRemoveInUpdatePhase.size)
    }

    spatialsToRemoveInUpdatePhase.foreach {
      case (s:Spatial,pc:GameParticipant) =>
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


  def respawnDeadPlayer(cp: Model.GameParticipant) {
    log.info("Respawning dead player " + cp.playerId)

    cp.state = Playing()
    world.spawnPlayer(cp)
  }

  def generateGameWorldChanges(simTime:Long):  ServerGameWorld = {

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
      // TODO: Fix this sometime failing assert
      val deadPlayers = purgeDeadPlayersSinceLastUpdate.ensuring(_.forall( dp => !playerState.exists(_.playerId == dp)))


      if(deadPlayers.size > 0 ) {
        log.info(deadPlayers.size + "DDDDDDDDDDDDDDDDDDDDDDEAD")
        deadPlayers.foreach( p => log.info(p))
      }

      log.debug("playerState " + playerState.size + " alivePlayers " + deadPlayers.size)

      val wastInLastUpdate = lastGeneratedUpdate match {
        case Some(l) => var lastAlivePlayers = l.alivePlayers.toList
          val (_ , newPlayers) = playerState.partition( p => lastAlivePlayers.exists( p2 => p.playerId == p2.playerId))
          newPlayers
        case None => playerState
      }

      val newAlivePlayers:List[PlayerInfo] = wastInLastUpdate.flatMap(p => participatingPlayers.find(p2 => p.playerId == p2.playerId))
        .map(p => new PlayerInfo(p.playerId, "Player " + p.playerId, p.teamIdentifier)).toList


      newAlivePlayers.foreach( n => log.info(n + " entered"))

      val res: ServerGameWorld = new ServerGameWorld(

        deadPlayers = new java.util.ArrayList[Int](deadPlayers),
        alivePlayers = new util.ArrayList[PlayerGO](playerState),
        projectiles = new java.util.ArrayList[ProjectileGO](simulatedProjectiles),
        explodedProjectiles = new java.util.ArrayList[ProjectileGO](exploaded),
        timeStamp = simTime,
        newAlivePlayersInfo = new util.ArrayList(newAlivePlayers ),
        seqId = -1
      )
      if(exploaded.size > 0) {
        log.info(exploaded.size + " exploaded ")
      }

      lastGeneratedUpdate = Some(res)
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

  def addParticipant(pc: PlayerConnection) = {

    lock.synchronized {

      val pd = new PlayerGO
      val pp = new GameParticipant
      pd.playerId = pc.playerId
      pp.playerId = pc.playerId
      pp.teamIdentifier = pc.teamIdentifier
      pd.position = Vector3f.ZERO.clone().setY(0.13499954f)
      pd.direction = Quaternion.DIRECTION_Z.clone()

      pp.gameState = pd

      participatingPlayers = participatingPlayers :+ pp
      //TODO: Really from here? Should be handled by game logic
      world.spawnPlayer(pp)
    }

  }



  def addPlayerAction(request: PlayerActionRequest) {
    lock.synchronized {


      participatingPlayers.find(_.playerId == request.playerId).map(_.state) match {
        case Some(Dead(since)) => //println("Discarding dead player " + request.playerId + " querySendUpdate")
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

  def destroy() {
    world.getPhysicsSpace.removeCollisionListener(this)
    //world.getPhysicsSpace.removeCollisionGroupListener(PhysicsCollisionObject.COLLISION_GROUP_02)
    //world.getPhysicsSpace.removeCollisionGroupListener(PhysicsCollisionObject.COLLISION_GROUP_03)
    destroyed = true
    world.destroy()
  }
}
