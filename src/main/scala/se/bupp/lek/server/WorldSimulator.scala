package se.bupp.lek.server

import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Model._
import collection.JavaConversions
import JavaConversions.asScalaBuffer
import se.bupp.lek.client.SceneGraphWorld
import com.jme3.asset.AssetManager
import com.jme3.bullet.control.CharacterControl
import se.bupp.lek.client.SceneGraphWorld.SceneGraphUserDataKeys
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape
import util.Random
import com.jme3.scene.{Spatial, Node}
import scalaz.NonEmptyList
import collection.mutable.{Buffer, HashMap, ArrayBuffer}
import com.jme3.bullet.{PhysicsSpace, BulletAppState}


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/6/12
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */


class ServerWorld(rootNode: Node, assetManager:AssetManager, physicsSpace:PhysicsSpace) extends SceneGraphWorld(true,assetManager,rootNode) with Lallers {

  var simCurrentTime:Long = System.currentTimeMillis()

  def initEmpty() {
    super.init()
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

trait Lallers {

  var simCurrentTime:Long
  def addPlayer(ps:PlayerStatus) : Unit
  def getNode(str:String) : Node

  def getPlayers() = projectNodeChildrenByData[PlayerStatus](SceneGraphWorld.SceneGraphNodeKeys.Enemies, SceneGraphWorld.SceneGraphUserDataKeys.Player)

  def projectNodeChildrenByData[U](nodeKey:String, userDataKey:String) = {
    getNode(nodeKey).getChildren.map(x => (x.getUserData[U](userDataKey),x))
  }
    
  def findPlayer(playerId:Int) = {
    getPlayers().find {
      case (p, s) => p.state.playerId == playerId
    }.map(_._1)
  }

  def findPlayerInfo(playerId:Int) = {
    getPlayers().find {
      case (p, s) => p.state.playerId == playerId
    }
  }

  def simulateToLastUpdated() : Long = {

    import scalaz._
    import Scalaz._

    val players = getPlayers
    getOldestUpdateTime(players) match {

      case Some(oldestPlayerUpdate) =>

        val playerSpatials = players.map {
          case (playerStatus, s) => s
        }

        if (simCurrentTime < oldestPlayerUpdate) {
          val updates = popPlayerUpdatesLessOrEqualToTimeSorted(players, oldestPlayerUpdate)

          simulateUpdatesUntil(updates, playerSpatials)
        }
      case None => println("No oldest update")
    }

    simCurrentTime
  }


  def simulateUpdatesUntil(updates: scala.Seq[(Long, NonEmptyList[(Model.PlayerStatus, Spatial, Model.MotionGO)])], playerSpatials:Seq[Spatial]) {
    updates.foreach {
      case (time, playerUpdatesAtTime) =>
        val nextSimDuration =  time - simCurrentTime
        val changedFromSimClockUntilNextPause = playerUpdatesAtTime.map {
          case (ps, s, u) =>
            (s, u)
        }

        playerSpatials.foreach { s =>
          val control = s.getControl(classOf[CharacterControl])
          control.setWalkDirection(Vector3f.ZERO.clone())
        }

        simulateStepSequence(changedFromSimClockUntilNextPause.list, nextSimDuration)

        playerUpdatesAtTime.list.foreach {
          case (ps, s, u) => ps.lastSimulation = time
        }
        simCurrentTime = time
    }
  }

  def popPlayerUpdatesLessOrEqualToTimeSorted(players: Buffer[(Model.PlayerStatus, Spatial)], oldestPlayerUpdate: Long): Seq[(Long, NonEmptyList[(Model.PlayerStatus, Spatial, Model.MotionGO)])] = {
    import se.bupp.lek.common.FuncUtil._
    players.map {
      case (playerStatus, s) =>
        val (toExecute, left) = playerStatus.reorientation.partition(_.sentToServer <= oldestPlayerUpdate)
        playerStatus.reorientation = left
        (playerStatus, s, toExecute)
      //p._1.state.sentToServerByClient <= oldestPlayerUpdate && p._1.state.sentToServerByClient > simCurrentTime
    }.flatMap {
      case (ps, s, ups) => ups.map(u => (ps, s, u))
    }.map {
      case (ps, s, u) => (u.sentToServer, (ps, s, u))
    }.toListMap.toSeq.sortWith(_._1 < _._1)
  }

  def getOldestUpdateTime(players: Buffer[(Model.PlayerStatus, Spatial)]): Option[Long] = {
    if(players.forall(_._1.reorientation.size > 0)) {
      val oldestPlayerUpdate = players.foldLeft(Long.MaxValue) {
        case (least, (st, sp)) =>
          if (st.reorientation.size == 0) least else {
            math.min(st.reorientation.last.sentToServer, least)
          }
      }
      if(oldestPlayerUpdate == Long.MaxValue) None else Some(oldestPlayerUpdate)
    } else None
  }

  /**
   * @param updates Changes to be applied at current simulation time
   * @param duration How long to run sim until next stop
   */
  def simulateStepSequence(updates:List[(Spatial, MotionGO)], duration:Long) {

    val timeSlots:Int = math.round((duration.toFloat/1000f) / getPhysicsSpace.getAccuracy).toInt
    val simSteps = if(timeSlots > 1) timeSlots else 1

    updates.foreach { case (s,u) =>
      val ctrl: CharacterControl = s.getControl(classOf[CharacterControl])
      ctrl.setWalkDirection(u.translation.divide(simSteps.toFloat))
      s.setLocalRotation(u.rotation.mult(s.getLocalRotation))
      val status: PlayerStatus = s.getUserData[PlayerStatus](SceneGraphWorld.SceneGraphUserDataKeys.Player)
      println("Setting trans " + u.translation + " p " + status.state.playerId + " time " + simCurrentTime + ctrl.getWalkDirection + " " + simSteps)
    }


    //println("Sim " + " aaaaaaaaaaaaaaaa " + duration.toFloat + " " + simSteps)
    getPhysicsSpace.update(duration.toFloat/1000f, simSteps)
  }
  def getPhysicsSpace : PhysicsSpace
}

class WorldSimulator(world:Lallers) {
  var connectionSequence = 0

  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  //var getPlayers = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  var projectiles = List[ProjectileGO]()

  var simulatedUntil:Option[Long] = None

  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()



  def getGameWorld() :  ServerGameWorld = {
    val simTime: Long = System.currentTimeMillis()

    getGameWorld(simTime)
  }


  def getGameWorld(simTime:Long):  ServerGameWorld = {
    val gameWorld = new ServerGameWorld
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


      firedProjectiles = HashMap.empty
      projectiles = projectiles ++ newProjectiles

      projectiles = projectiles.filter(_.timeSpawned > maxAgeProjectiles)


      lastWorldSimTimeStamp.foreach { lastSimTime =>
        projectiles.foreach {
          pf =>
          //pf.
            val translate = pf.orientation.direction.getRotationColumn(0).mult(pf.speed * (simTime - lastSimTime).toFloat / 1000f)
            //println("translate " + translate + translate.length)
            pf.orientation.position = pf.orientation.position.add(translate)
        }
      }

      //println("projectiles.size" + projectiles.size + " newProjectiles " + newProjectiles.size)

      /*

      firedProjectiles = firedProjectiles.map { case (k,vl) =>
       val remaining = vl.filter { pf => pf.timeStamp > maxAgeProjectiles }
       (k, remaining)
      }.filter {
        case (k,vl) => vl.size > 0
      }
      */

      gameWorld.players = new java.util.ArrayList[PlayerGO](playerState)
      gameWorld.projectiles = new java.util.ArrayList[ProjectileGO](projectiles)
      gameWorld.timeStamp = simTime
      lastWorldSimTimeStamp = Some(world.simCurrentTime)
    }

    //println(s)
    gameWorld
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
