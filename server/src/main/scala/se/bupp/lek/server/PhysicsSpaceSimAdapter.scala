package se.bupp.lek.server

import se.bupp.lek.common.Model.{AbstractOwnedGameObject, MotionGO, GameParticipant}
import com.jme3.scene.{Spatial, Node}

import scalaz.NonEmptyList
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.bullet.control.{BetterCharacterControl, CharacterControl}
import collection.mutable
import com.jme3.bullet.PhysicsSpace
import scala.collection.JavaConversions.asScalaBuffer
import se.bupp.lek.common.SceneGraphWorld.SceneGraphNodeKeys
import se.bupp.lek.common.{MathUtil, SceneGraphWorld, SceneGraphAccessors}
import se.bupp.lek.common.model.Model._

trait PhysicsSpaceSimAdapter extends SceneGraphAccessors {

  var simCurrentTime: Long

  lazy val minSimDuration = math.ceil(getPhysicsSpace.getAccuracy * 1000).toLong
  def isTest:Boolean = false

  def spawnPlayer(ps: GameParticipant): Unit


  def getPlayers() = projectNodeChildrenByData[GameParticipant](SceneGraphWorld.SceneGraphNodeKeys.Enemies, SceneGraphWorld.SceneGraphUserDataKeys.Player)


  def findPlayer(playerId: PlayerId) = {
    getPlayers().find {
      case (p, s) => p.gameState.playerId == playerId
    }.map(_._1)
  }

  def findPlayerInfo(playerId: PlayerId) = {
    getPlayers().find {
      case (p, s) => p.gameState.playerId == playerId
    }
  }

  def find[T <: AbstractOwnedGameObject](k:SceneGraphNodeKeys.SceneGraphNodeKey, udKey:String, t:T) = {
    projectNodeChildrenByData[T](k,udKey).find { case (tt,s) => tt.id == t.id}
  }


  def simulateAllUpdates(tpf:Float) {
    val players = getPlayers
    val simTime = Server.clock()
    /*val control = s.getControl(classOf[CharacterControl])
    control.setWalkDirection(Vector3f.ZERO.clone())


    ctrl.setWalkDirection(u.translation.divide(simSteps.toFloat))
    */

    val timeSlots = (tpf ) / getPhysicsSpace.getAccuracy
    val simSteps = if (timeSlots > 1.0f) math.floor(timeSlots).toInt else 1


    players.foreach {
      case (pi,s) =>
        val anyUpdates = Seq(pi.updates:_*)
        if(anyUpdates.size > 0) {
          pi.updates = Nil

        val control = s.getControl(classOf[BetterCharacterControl]).ensuring(_ != null)

        val (movement,rot) = anyUpdates match {
          case Nil => (Vector3f.ZERO.clone(),MathUtil.noRotation)
          case updates =>

            val rot = updates.foldLeft(MathUtil.noRotation) { case (a,u) => u.rotation.mult(a) }
            val move = updates.foldLeft(Vector3f.ZERO.clone()) { case (a,u) => (u.translation.add(a)) }
            (move.divide(updates.size.toFloat), rot)

        }
        //
        if(pi.playerId % 2 == 0) {
          println("++++ " + pi.playerId + " " + rot + " " + s.getLocalRotation + " " + movement + " " + s.getWorldRotation)
        }

        var local: Vector3f = rot.mult(control.getViewDirection)

        control.setViewDirection(local)
        control.setWalkDirection(movement)
        pi.lastSimulationServerTime = simTime

      }

      case _ =>
    }

    //println("tpf " + tpf + " simSteps " + simSteps)

    getPhysicsSpace.update(tpf,simSteps)
    getPhysicsSpace.distributeEvents()

    /*playerSpatials.foreach {
      s =>
        val control = s.getControl(classOf[CharacterControl])
        control.setWalkDirection(Vector3f.ZERO.clone())
    }*/


  }


  def simulateToLastUpdated(): Long = {

    import scalaz._
    import Scalaz._

    val players = getPlayers
    getOldestUpdateTimeLastReceived(players) match {

      case Some(oldestPlayerUpdate) =>

        val playerSpatials = players.map {
          case (playerStatus, s) => s
        }

        if (simCurrentTime < oldestPlayerUpdate) {
          val updates = popPlayerUpdatesLessOrEqualToTimeSorted(players, oldestPlayerUpdate)

          simulateUpdatesUntil(updates, playerSpatials)
        }
      case None => //println("No oldest querySendUpdate")
    }

    simCurrentTime
  }

  var timeBank = 0L

  def simulateUpdatesUntil(updates: scala.Seq[(Long, NonEmptyList[(GameParticipant, Spatial, MotionGO)])], playerSpatials: Seq[Spatial]) {
    updates.foreach {
      case (time, playerUpdatesAtTime) =>
        var nextSimDuration = time - simCurrentTime

        if (nextSimDuration < minSimDuration) {
          timeBank += nextSimDuration - minSimDuration
          nextSimDuration = minSimDuration
        } else if (nextSimDuration > minSimDuration && timeBank < 0) {
          val deposit = (nextSimDuration % minSimDuration)
          timeBank += deposit
          nextSimDuration -= deposit
        }

        val changedFromSimClockUntilNextPause = playerUpdatesAtTime.map {
          case (ps, s, u) =>
            (s, u)
        }

        playerSpatials.foreach {
          s =>
            val control = s.getControl(classOf[BetterCharacterControl])
            control.setWalkDirection(Vector3f.ZERO.clone())
        }

        simulateStepSequence(changedFromSimClockUntilNextPause.list, nextSimDuration)

        simCurrentTime += nextSimDuration

        playerUpdatesAtTime.list.foreach {
          case (ps, s, u) => ps.lastSimulationServerTime = time
        }

    }
  }

  def popPlayerUpdatesLessOrEqualToTimeSorted(players: mutable.Buffer[(GameParticipant, Spatial)], oldestPlayerUpdate: Long): Seq[(Long, NonEmptyList[(GameParticipant, Spatial, MotionGO)])] = {
    import se.bupp.lek.common.FuncUtil._
    players.map {
      case (playerStatus, s) =>
        val (toExecute, left) = playerStatus.updates.partition(_.sentToServer <= oldestPlayerUpdate)
        playerStatus.updates = left
        (playerStatus, s, toExecute)
      //p._1.state.sentToServerByClient <= oldestPlayerUpdate && p._1.state.sentToServerByClient > simCurrentTime
    }.flatMap {
      case (ps, s, ups) => ups.map(u => (ps, s, u))
    }.map {
      case (ps, s, u) => (u.sentToServer, (ps, s, u))
    }.toListMap.toSeq.sortWith(_._1 < _._1)
  }

  def getOldestUpdateTimeLastReceived(players: mutable.Buffer[(GameParticipant, Spatial)]): Option[Long] = {
    if (players.forall(_._1.updates.size > 0)) {
      val oldestPlayerUpdate = players.foldLeft(Long.MaxValue) {
        case (least, (st, sp)) =>
          if (st.updates.size == 0) least
          else {
            math.min(st.updates.last.sentToServer, least)
          }
      }
      if (oldestPlayerUpdate == Long.MaxValue) None else Some(oldestPlayerUpdate)
    } else None
  }

  /**
   * @param updates Changes to be applied at current simulation time
   * @param duration How long to run sim until next stop
   */
  def simulateStepSequence(updates: List[(Spatial, MotionGO)], duration: Long) {

    val timeSlots = (duration.toFloat / 1000f) / getPhysicsSpace.getAccuracy
    val simSteps = if (timeSlots > 1.0f) math.floor(timeSlots).toInt else 1
    var str = mutable.Map.empty[Int,String]
    var preMap = mutable.Map.empty[Int,Vector3f]
    updates.foreach {
      case (s, u) =>
        val ctrl = s.getControl(classOf[BetterCharacterControl])

        ctrl.setWalkDirection(u.translation.divide(simSteps.toFloat))
        s.setLocalRotation(u.rotation.mult(s.getLocalRotation))
        if (isTest) {
          val status: GameParticipant = s.getUserData[GameParticipant](SceneGraphWorld.SceneGraphUserDataKeys.Player)
          preMap = preMap + (status.gameState.playerId -> s.getLocalTranslation.clone())
          val ss = "Setting trans " + u.translation + " p " + status.gameState.playerId + " " + duration + " time " + simCurrentTime + ctrl.getWalkDirection + " " + simSteps
          str  = str + (status.gameState.playerId ->  ss)
        }
    }

    //println("Sim " + " aaaaaaaaaaaaaaaa " + duration.toFloat + " " + simSteps)
    getPhysicsSpace.update(duration.toFloat / 1000f, simSteps)
    getPhysicsSpace.distributeEvents()

    if (isTest)
    updates.foreach {
      case (s, u) =>

        val ctrl = s.getControl(classOf[BetterCharacterControl])

        val status: GameParticipant = s.getUserData[GameParticipant](SceneGraphWorld.SceneGraphUserDataKeys.Player)
        var post = s.getLocalTranslation
        var pre = preMap(status.gameState.playerId)
        var actual = post.subtract(pre).length()
        val ss = str(status.gameState.playerId) + ( " " + actual + " " + u.translation.length() + " " + timeBank + " " + (if (math.abs(u.translation.length() - actual) > 0.001) "***" else "---"))
        //println(ss)
    }
  }

  def getPhysicsSpace: PhysicsSpace
}
