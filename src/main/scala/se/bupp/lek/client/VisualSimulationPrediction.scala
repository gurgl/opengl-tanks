package se.bupp.lek.client

import collection.immutable.Queue
import se.bupp.lek.server.Model
import collection.immutable
import com.jme3.export.Savable
import se.bupp.lek.server.Model._
import com.jme3.math.{FastMath, Vector3f, Quaternion}
import com.jme3.bullet.control.CharacterControl
import java.lang

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/4/12
 * Time: 5:13 PM
 * To change this template use File | Settings | File Templates.
 */


class VisualSimulationPrediction(gameWorldUpdates:Queue[Model.ServerGameWorld], val playerId:Int) {
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

    var previous = snapshots.reverse.tail
    var end = snapshots.last

    val startOpt = previous.find( prev => (end.sentToServerByClient - prev.sentToServerByClient) > 0)

    startOpt match {
      case Some(start) =>
        var snapDeltaTime = (end.sentToServerByClient - start.sentToServerByClient)
        val timeSinceLast = simTime - end.sentToServerByClient
        //println("ugl " + end.position + " " + end.direction + " " + end.sentToServerByClient + " " + end.id + " " + snapDeltaTime + " " + timeSinceLast)



        var velocity = end.position.subtract(start.position).divide(snapDeltaTime)

        val extrapolTranslation = velocity.mult(timeSinceLast)

        var startAngle = start.direction
        var endAngle = snapshots.last.direction




        val interpolationFactor: Float = (timeSinceLast + snapDeltaTime).toFloat / snapDeltaTime.toFloat

        val exptrapolRotation = Quaternion.IDENTITY.clone().slerp(startAngle,endAngle, interpolationFactor)

        val pNew = new PlayerGO(p)
        pNew.direction = exptrapolRotation
        pNew.position = p.position.add(extrapolTranslation)

        if(pNew.position.getX.equals(Float.NaN)) {
          println("snapDeltaTime" +    snapDeltaTime +
            "timeSinceLast" +   timeSinceLast+
            "interpolationFactor" +  interpolationFactor +
            "startAngle" + startAngle +
            "endAngle" +   endAngle+
            "timeSinceLast" +   timeSinceLast+
            "end.position" + end.position +
            "start.position" + start.position
          )

        }

        pNew
      case None => p
    }
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

  def interpolateNonPlayerObjects(simTime:Long) : List[AbstractOwnedGameObject with Savable] = {
    val lastServerSimInstants = gameWorldUpdates.map(_.timeStamp).toSeq
    val lastServerSimToClientSimDurations = lastServerSimInstants.map(simTime - _)
    //println(gameWorldUpdates.last.all.map(_.position).mkString(","))
    val res = projectGameHistoryByGameObjectId.toList.map {
      case (id,snapshotsUT) =>

        val orderedObjectSnapshots = snapshotsUT.asInstanceOf[List[AbstractOwnedGameObject with Savable]]

        val estimate:Option[AbstractOwnedGameObject with Savable] = if(orderedObjectSnapshots.size < 2) {
          //println("unable to interpolateNonPlayerObjects")
          None
        } else {
            orderedObjectSnapshots.last match {
            case p:PlayerGO =>

              if(id._2 == playerId) {
                None
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
}
