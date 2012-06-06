package se.bupp.lek.server

import collection.mutable.{HashMap, ArrayBuffer}
import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Server._
import collection.JavaConversions
import JavaConversions.asScalaBuffer
import com.jme3.scene.Node

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/6/12
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */

class WorldSimulator(rootNode:Node) {
  var connectionSequence = 0

  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  var players = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  var projectiles = List[ProjectileGO]()


  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()

  def getGameWorld():  ServerGameWorld = {
    val gameWorld = new ServerGameWorld
    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {
      val playerState = players.map {
        p =>
          p.state
      }

      //println("pos " + playerState.map ( p => p.playerId + " " + p.position).mkString(", "))
      s = playerState.map(x => " | " + x.playerId + " " + x.position + " " + x.direction).mkString(",")


      val simTime: Long = System.currentTimeMillis()
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
      lastWorldSimTimeStamp = Some(simTime)
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
        pd.position = Vector3f.ZERO.clone()
        pd.direction = Quaternion.DIRECTION_Z.clone()
        pd
        var ps = new PlayerStatus
        ps.state = pd
        ps.lastUpdate = None
        ps
      }
      players += player

      connectionSequence += 1
    }
    playerId
  }

  def addPlayerAction(request: PlayerActionRequest) {
    lock.synchronized {


      players.find(p => p.state.playerId == request.playerId).foreach {
        x => {
          x.lastUpdate = Some(request)
          //x.processedUpdate = false

          x.state.sentToServerByClient = request.timeStamp
          x.state.position = x.state.position.add(request.motion.translation)
          x.state.direction = request.motion.rotation.mult(x.state.direction)


          firedProjectiles.get(request.playerId) match {
            case Some(existing) => firedProjectiles(request.playerId) = existing ++ request.projectilesFired
            case None => firedProjectiles(request.playerId) = request.projectilesFired.toList
          }
        } //.ensuring(x.position != null && x.direction != null)
      }

    }
  }
}
