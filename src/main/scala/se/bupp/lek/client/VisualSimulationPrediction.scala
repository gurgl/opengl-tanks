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

object VisualSimulationPrediction {
  type Blaj = (Long,AbstractOwnedGameObject with Savable)
}

class VisualSimulationPrediction(val gameWorldUpdates:Queue[Model.ServerGameWorld], val playerId:Int) {
  import VisualSimulationPrediction._
  def projectGameHistoryByGameObjectId() : immutable.Map[OwnedGameObjectId,List[(Long, _ <: AbstractOwnedGameObject with Savable)]] = {
    val slots = gameWorldUpdates.last.all.map(_.id).toSet
    slots.map { s =>
      val sUpdates = gameWorldUpdates.flatMap { upd =>
         upd.all.find( _.id == s ).map((upd.timeStamp,_))
      }.toList

      (s,sUpdates)
    }.toMap //.ensuring(_.size == gameWorldUpdates.last.all.size)
  }

  def simulatePlayer(p:PlayerGO, simTime:Long, snapshots:List[Blaj]) : PlayerGO = {

    var previous = snapshots.reverse.tail
    var endP = snapshots.last

    val startOpt = previous.find( prev => (endP._1 - prev._1) > 0)

    startOpt match {
      case Some(startP) =>
        var snapDeltaTime = (endP._1 - startP._1)
        val timeSinceLast = simTime - endP._1
        val (_,end) = endP
        val (_,start) = startP

        //println("ugl " + end.position + " " + end.direction + " " + end.sentToServerByClient + " " + end.id + " " + snapDeltaTime + " " + timeSinceLast)



        var velocity = end.position.subtract(start.position).divide(snapDeltaTime)

        val extrapolTranslation = velocity.mult(timeSinceLast)

        var startAngle = start.direction
        var endAngle = snapshots.last._2.direction




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
  def simulateProjectile(lastServerSimToSimTimes:Seq[Long],snapshots:List[AbstractOwnedGameObject with Savable], reversedServerSimTimes:Seq[Long]) = {

    //buffer.append(snapshots.map(_.position).mkString(", ") + "\n")
    val (sequence,lastServerSimToSimTime, simDelta) = if(lastServerSimToSimTimes.last > 25) {
      (snapshots.reverse, lastServerSimToSimTimes.last, reversedServerSimTimes.head)
    } else {
      (snapshots.reverse.tail, lastServerSimToSimTimes.reverse.tail.head, reversedServerSimTimes.tail.head)
    }



    val p:ProjectileGO = sequence match {
        case List(end:ProjectileGO,start:ProjectileGO,_*)  =>

          /*

          val velocity = dist.clone().divide(simDelta.toFloat)

          //println(velocity.length() + " " + end.speed * lastServerSimToSimTime.toFloat/1000f + end.position)
          val translation = velocity.mult(lastServerSimToSimTime.toFloat/1000f)

           */

          val dist = end.position.subtract(start.position)

          val dir = dist.clone().normalizeLocal()

          //println(end.speed + " " + end.speed * lastServerSimToSimTime.toFloat/1000f + end.position)
          val translation = dir.mult(end.speed * lastServerSimToSimTime.toFloat/1000f)

          val n = new ProjectileGO(end)
          n.position = end.position.add(translation)
          n

        case  List(end:ProjectileGO) => new ProjectileGO(end)
        case _ => throw new RuntimeException("fan")
      //Client.buffer.append("moving " + translation + " " + translation.length() + " " + lastServerSimToSimTime + " " + p.speed + " " + pp.position + Client.clock() + "\n")
    }
    p
  }

  def interpolateNonPlayerObjects(simTime:Long) : List[AbstractOwnedGameObject with Savable] = {
    val lastServerSimInstants = gameWorldUpdates.map(_.timeStamp).toSeq

    val reversedLastServerSimInstants = lastServerSimInstants.reverse
    val serverSimsDelta = reversedLastServerSimInstants.zip(reversedLastServerSimInstants.tail).map( t => t._1 - t._2)

    val lastServerSimToClientSimDurations = lastServerSimInstants.map(simTime - _)
    //println(gameWorldUpdates.last.all.map(_.position).mkString(","))
    //if(gameWorldUpdates.last.projectiles.size > 0) println(gameWorldUpdates.last.projectiles.size)
    //gameWorldUpdates.last.all.find( _.isInstanceOf[ProjectileGO]).foreach( p => println("PRO"))
    //projectGameHistoryByGameObjectId.find( _._2.last.isInstanceOf[ProjectileGO]).foreach( p => println("PRO"))
    val res = projectGameHistoryByGameObjectId.toList.flatMap {
      case (id,snapshotsUT) =>

        val orderedObjectSnapshots = snapshotsUT.asInstanceOf[List[Blaj]]

        val estimate:Option[AbstractOwnedGameObject with Savable] =
          if(orderedObjectSnapshots.size < 2) {
            //println("unable to interpolateNonPlayerObjects")
            Some(orderedObjectSnapshots.last._2)
          } else {
              orderedObjectSnapshots.last match {
              case (_,p:PlayerGO) =>

                if(id._2 == playerId) {

                  None
                  //Some(p)
                } else {
                  Some(simulatePlayer(p,simTime, orderedObjectSnapshots))
                }

              case (_,p:ProjectileGO) =>

                Some(simulateProjectile(lastServerSimToClientSimDurations, orderedObjectSnapshots.map(_._2), serverSimsDelta))
            }
          }
      estimate
    }
    res
  }
}
