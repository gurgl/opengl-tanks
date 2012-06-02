package se.bupp.lek.spel

import com.esotericsoftware.kryonet.{Connection, Listener, Client}
import management.ManagementFactory
import com.jme3.app.state.{AppStateManager, AbstractAppState}
import com.jme3.app.Application
import collection.immutable.Queue
import collection.immutable.Queue._
import MathUtil._
import com.jme3.app.SimpleApplication._
import com.jme3.scene.Node
import com.jme3.export.Savable
import com.jme3.math.{Quaternion, Vector3f}
import collection.{JavaConversions, immutable}
import scala.Option
import se.bupp.lek.spel.GameServer._


/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/29/12
 * Time: 12:32 PM
 * To change this template use File | Settings | File Templates.
 */


class NetworkState extends AbstractAppState {


  var gameClient:Client = _

  val GW_UPDATES_SIZE = 4

    var lastSentUpdate = 0L


  var gameWorldUpdatesQueue:Queue[GameServer.ServerGameWorld] = Queue()
  var hasUnProcessedWorldUpdate = false

  var accTranslation = Vector3f.ZERO
  var accRotation = noRotation

  var predictions:Map[Long,Map[OwnedGameObjectId,Vector3f]] = Map[Long,Map[OwnedGameObjectId,Vector3f]]()


  var gameApp:Spel = _

  class InstantSimulation(gameWorldUpdates:Queue[GameServer.ServerGameWorld]) {

    def projectGameHistoryByGameObjectId() : immutable.Map[OwnedGameObjectId,List[ _ <: AbstractOwnedGameObject with Savable]] = {
      val slots = gameWorldUpdates.last.all.map(_.id).toSet
      //val gameWorldUpdatesRev = gameWorldUpdates.reverse
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
      //println("adf " + interpolationFactor)
      val exptrapolRotation = Quaternion.IDENTITY.slerp(startAngle,endAngle, interpolationFactor)

      val pNew = new PlayerGO(p)
      pNew.direction = exptrapolRotation
      pNew.position = p.position.add(extrapolTranslation)
      pNew

    }
    def simulateProjectile(p:ProjectileGO,lastServerSimToSimTime:Long) = {
      val pp = new ProjectileGO(p)
      val translation = pp.direction.getRotationColumn(0).mult(pp.speed * lastServerSimToSimTime.toFloat/1000f)

      pp.position = pp.position.add(translation)

      //println("moving " + translation + " " + lastServerSimToSimTime + " " + p.speed + pp.position)
      pp
    }

    def interpolate(simTime:Long) : List[AbstractOwnedGameObject with Savable] = {
      val lastServerSimTime = gameWorldUpdates.last.timeStamp
      val lastServerSimToClientSim= simTime - lastServerSimTime
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

                if(id._2 == gameApp.playerIdOpt.get) {
                  None
                } else {
                  Some(simulatePlayer(p,simTime, orderedObjectSnapshots))
                }
                
              case p:ProjectileGO =>
                Some(simulateProjectile(p,lastServerSimToClientSim))
            }
          }
          estimate.getOrElse(orderedObjectSnapshots.last)

      }

      res
    }
  }

  override def update(tpf: Float) {
    if(gameApp.playerIdOpt.isEmpty) return
    if(hasUnProcessedWorldUpdate) {
      //println("upd s " +  gameWorldUpdates.last.projectiles.size)
      //gameApp.syncGameWorld(gameWorldUpdates.last.all)
      hasUnProcessedWorldUpdate = false
      //println(gameWorldUpdatesQueue.last.timeStamp + " " + gameWorldUpdatesQueue.last.all.size)
      //println("last updd " + gameWorldUpdatesQueue.last.all.map(_.position).mkString(","))
    }

    var currentGameWorldUpdates:Queue[ServerGameWorld] = null
//    lock.synchronized {
      currentGameWorldUpdates = gameWorldUpdatesQueue
//    }

    if(currentGameWorldUpdates.size > 0) {

      val simTime = System.currentTimeMillis()

      val updates = new InstantSimulation(currentGameWorldUpdates).interpolate(simTime)
      gameApp.syncGameWorld(updates.distinct.toSet)
    }

    accTranslation = accTranslation.add(gameApp.playerInput._1)
    accRotation = gameApp.playerInput._2.mult(accRotation)

    if(System.currentTimeMillis() - lastSentUpdate > 1000/16 ) {
      val request: PlayerActionRequest = new PlayerActionRequest

      import JavaConversions.seqAsJavaList
      request.projectilesFired = new java.util.ArrayList[ProjectileFireGO](gameApp.projectileHandler.purgeFired)
      request.timeStamp = System.currentTimeMillis()
      request.playerId = gameApp.playerIdOpt.get
      request.motion = new MotionGO(accTranslation,accRotation)
      gameClient.sendUDP(request)
      lastSentUpdate = request.timeStamp
      accTranslation = Vector3f.ZERO
      accRotation = noRotation
    }
  }


  override def initialize(stateManager: AppStateManager, app: Application) {
    gameApp = app.asInstanceOf[Spel]
    initClient()
  }

  val lock : AnyRef = new Object()
  def initClient() {
    gameClient = new Client();

    val kryo = gameClient.getKryo();

    GameServer.getNetworkMessages.foreach( kryo.register(_))

    gameClient.addListener(new Listener() {
       override def received (connection:Connection , obj:Object ) {
         obj match {
            case response:ServerGameWorld=>
              if(gameApp.playerIdOpt.isDefined) {
//                lock.synchronized {
                  gameWorldUpdatesQueue =
                    Option(gameWorldUpdatesQueue).map(
                      x => if(x.size >= GW_UPDATES_SIZE) {x.dequeue._2} else {x}
                    ).head.enqueue(response)
                  hasUnProcessedWorldUpdate = true
  //              }
              } else {
                println("Getting world wo player received")
              }

            case response:PlayerJoinResponse =>
              println("join resp received")
              gameApp.playerIdOpt = Some(response.playerId)

            case _ =>
          }
       }
    });

    gameClient.start();
    gameClient.connect(5000, "localhost", 54555, 54777);

    val playerJoinRequest = new PlayerJoinRequest()
    playerJoinRequest.clientLabel = ManagementFactory.getRuntimeMXBean().getName()
    gameClient.sendTCP(playerJoinRequest);
  }
}
