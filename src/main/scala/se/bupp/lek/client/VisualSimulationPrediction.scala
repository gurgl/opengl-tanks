package se.bupp.lek.client

import collection.immutable.Queue
import se.bupp.lek.server.Server
import collection.immutable
import com.jme3.export.Savable
import se.bupp.lek.server.Server._
import com.jme3.math.{FastMath, Vector3f, Quaternion}
import com.jme3.bullet.control.CharacterControl

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/4/12
 * Time: 5:13 PM
 * To change this template use File | Settings | File Templates.
 */


class VisualSimulationPrediction(gameWorldUpdates:Queue[Server.ServerGameWorld], val playerId:Int) {
  def projectGameHistoryByGameObjectId() : immutable.Map[OwnedGameObjectId,List[ _ <: AbstractOwnedGameObject with Savable]] = {
    val slots = gameWorldUpdates.last.all.map(_.id).toSet

    slots.map { s =>
      val sUpdates = gameWorldUpdates.flatMap { upd =>
         upd.all.find( _.id == s )
      }.toList

      (s,sUpdates)
    }.toMap
  }

  def simulatePlayer(p:PlayerGO, simTime:Long, snapshots:List[AbstractOwnedGameObject with Savable]) : PlayerGO = {

    var start = snapshots.reverse.tail.head
    var end = snapshots.last

    var snapDeltaTime = (end.sentToServerByClient - start.sentToServerByClient)
    val timeSinceLast = simTime - end.sentToServerByClient
    //println("ugl " + end.position + " " + end.direction + " " + end.sentToServerByClient + " " + end.id + " " + snapDeltaTime + " " + timeSinceLast)
    if(snapDeltaTime == 0) {
      p
    }


    var velocity = end.position.subtract(start.position).divide(snapDeltaTime)

    val extrapolTranslation = velocity.mult(timeSinceLast)

    var startAngle = start.direction
    var endAngle = snapshots.last.direction


    val interpolationFactor: Float = (timeSinceLast + snapDeltaTime).toFloat / snapDeltaTime.toFloat

    val exptrapolRotation = Quaternion.IDENTITY.clone().slerp(startAngle,endAngle, interpolationFactor)

    val pNew = new PlayerGO(p)
    pNew.direction = exptrapolRotation
    pNew.position = p.position.add(extrapolTranslation)
    pNew

  }
  def simulateProjectile(lastServerSimToSimTimes:Seq[Long],snapshots:List[AbstractOwnedGameObject with Savable]) = {

    //buffer.append(snapshots.map(_.position).mkString(", ") + "\n")
    val (p:ProjectileGO,lastServerSimToSimTime) =  if(lastServerSimToSimTimes.last > 25) {
      (snapshots.last,lastServerSimToSimTimes.last)
    } else {
      (snapshots.reverse.tail.head, lastServerSimToSimTimes.reverse.tail.head)
    }
    val pp = new ProjectileGO(p)

    val translation = pp.direction.getRotationColumn(0).mult(pp.speed * lastServerSimToSimTime.toFloat/1000f)

    pp.position = pp.position.add(translation)

    //Client.buffer.append("moving " + translation + " " + translation.length() + " " + lastServerSimToSimTime + " " + p.speed + " " + pp.position + System.currentTimeMillis() + "\n")
    pp
  }

  def interpolate(simTime:Long, playerInput:PlayerInput) : List[AbstractOwnedGameObject with Savable] = {
    val lastServerSimInstants = gameWorldUpdates.map(_.timeStamp).toSeq
    val lastServerSimToClientSimDurations = lastServerSimInstants.map(simTime - _)
    //println(gameWorldUpdates.last.all.map(_.position).mkString(","))
    val res = projectGameHistoryByGameObjectId.toList.map {
      case (id,snapshotsUT) =>

        val orderedObjectSnapshots = snapshotsUT.asInstanceOf[List[AbstractOwnedGameObject with Savable]]

        val estimate:Option[AbstractOwnedGameObject with Savable] = if(orderedObjectSnapshots.size < 2) {
          //println("unable to interpolate")
          None
        } else {
            orderedObjectSnapshots.last match {
            case p:PlayerGO =>

              if(id._2 == playerId) {
                val newP = new PlayerGO(p)

                newP.orientation = recalculateFrom(p.sentToServerByClient,lastServerSimInstants.last,p)
                Some(newP)
                //Some(p)
              } else {
                Some(simulatePlayer(p,simTime, orderedObjectSnapshots))
              }

            case p:ProjectileGO =>
              Some(simulateProjectile(lastServerSimToClientSimDurations, orderedObjectSnapshots))
          }
        }
        estimate.getOrElse(orderedObjectSnapshots.last)
    }
    res
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

  def recalculateFrom(serverSnapshotSentByPlayerTime:Long,serverSimTime:Long, server:Orientation) : Orientation = {


    Client.spel.playerInput.lock.synchronized {

      val(discarded, newSaved) = Client.spel.playerInput.saved.partition ( _._1 < serverSnapshotSentByPlayerTime)
      var saved = newSaved
      //saved = if(discarded.size > 0) discarded.last +: newSaved else newSaved
      if(saved.size == 0) {
        //server
        new Orientation(Vector3f.ZERO.clone(), MathUtil.noRotation)
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
            val control: CharacterControl = Client.spel.gameWorld.player.getControl(classOf[CharacterControl])
            control.setPhysicsLocation(saved.last._2.position)
            //Client.spel.gameWorld.player.setLocalRotation(saved.last._2.direction)
            //control.setViewDirection(saved.last._2.direction.getRotationColumn(0))
        } /*else {
          //println("Good " + diffHeur)
          //println("using " + saved.last._2)
          saved.last._2
        } */
        //saved.map{ case (a,b,c) => a + " p" + b._1 + " t" + a._1 }.
        //newPos
        Client.spel.playerInput.saved = saved
        new Orientation(saved.last._3._1,saved.last._2.direction)
      }
    }
  }
}
