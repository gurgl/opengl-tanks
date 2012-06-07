package se.bupp.lek.server

import collection.mutable.{HashMap, ArrayBuffer}
import com.jme3.math.{Quaternion, Vector3f}
import se.bupp.lek.server.Server._
import collection.JavaConversions
import JavaConversions.asScalaBuffer
import com.jme3.scene.Node
import se.bupp.lek.client.SceneGraphWorld
import com.jme3.asset.AssetManager
import com.jme3.bullet.BulletAppState
import com.jme3.bullet.control.CharacterControl
import se.bupp.lek.client.SceneGraphWorld.SceneGraphUserDataKeys
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/6/12
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */


class ServerWorld(rootNode: Node, assetManager:AssetManager, bulletAppState:BulletAppState) extends SceneGraphWorld(true,assetManager,bulletAppState,rootNode) {
  def initEmpty() {
    super.init()
  }

  
  
  def players() = projectNodeChildrenByData[PlayerStatus](SceneGraphWorld.SceneGraphNodeKeys.Enemies, SceneGraphWorld.SceneGraphUserDataKeys.Player)

  def projectNodeChildrenByData[U](nodeKey:String, userDataKey:String) = {
    getNode(nodeKey).getChildren.map(x => (x.getUserData[U](userDataKey),x))
  }
    
  def findPlayer(playerId:Int) = {
    players().find {
      case (p, s) => p.state.playerId == playerId
    }.map(_._1)
  }

  def addPlayer(ps:PlayerStatus) = {
    val tank = materializeTank2(ps.state)
    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    //val tank = new Node("Bupp")
    tank.setUserData(SceneGraphUserDataKeys.Player, ps)

    //tank.attachChild(tankModel)
    val capsuleShape = new CapsuleCollisionShape(0.05f, 0.05f, 1)
    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    tank.addControl(playerControl)
    
    bulletAppState.getPhysicsSpace.add(playerControl)


    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0.3f);
    playerControl.setGravity(0.3f);
    playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));

    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).attachChild(tank)

  }
}

class WorldSimulator(world:ServerWorld) {
  var connectionSequence = 0

  var lock: AnyRef = new Object()

  var lastWorldSimTimeStamp:Option[Long] = None
  //var players = new ArrayBuffer[PlayerStatus]()
  var firedProjectiles = new HashMap[Int, List[ProjectileFireGO]]()

  var projectiles = List[ProjectileGO]()


  //enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)

  //var projectiles = new ArrayBuffer[ProjectileGO]()

  def getGameWorld():  ServerGameWorld = {
    val gameWorld = new ServerGameWorld
    import scala.collection.JavaConversions.seqAsJavaList

    var s = ""
    lock.synchronized {
      val players = world.players
      
      players.foreach {
        case (d,s) => 
          val ctrl = s.getControl(classOf[CharacterControl])
          d.reorientation.foreach { m => 

            ctrl.setWalkDirection(m.translation)
            d.state.direction = m.rotation.mult(d.state.direction)
            
            d.state.position = ctrl.getPhysicsLocation
            //println("phy " + ctrl.getPhysicsLocation + " reor " + m.translation)
          }
      }
      val playerState = players.map {
        case (ps, s)  => ps.state

      }
      /*val playerState = players.map {
        p =>
          p.state
      }*/
      
      

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
        pd.position = Vector3f.ZERO.clone().setY(1.5f)
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
          x.reorientation = Some(request.motion)



          firedProjectiles.get(request.playerId) match {
            case Some(existing) => firedProjectiles(request.playerId) = existing ++ request.projectilesFired
            case None => firedProjectiles(request.playerId) = request.projectilesFired.toList
          }
        } //.ensuring(x.position != null && x.direction != null)
      }
    }
  }
}
