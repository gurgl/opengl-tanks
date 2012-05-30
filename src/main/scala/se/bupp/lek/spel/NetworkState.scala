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
import se.bupp.lek.spel.GameServer._
import collection.immutable
import com.jme3.export.Savable
import com.jme3.math.{Quaternion, Vector3f}


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


  var gameWorldUpdates:Queue[GameServer.ServerGameWorld] = Queue()
  var hasUnProcessedWorldUpdate = false

  var accTranslation = Vector3f.ZERO
  var accRotation = noRotation
  
  var gameApp:Spel = _


  def projectGameHistoryByGameObjectId() : immutable.Map[OwnedGameObjectId,List[ _ <: AbstractOwnedGameObject with Savable]] = {
    val elapsedTime = gameWorldUpdates.last.timeStamp - gameWorldUpdates.head.timeStamp
    val slots = gameWorldUpdates.last.all.map(_.id).toSet
    //val gameWorldUpdatesRev = gameWorldUpdates.reverse
    slots.map { s => 
      val sUpdates = gameWorldUpdates.flatMap { upd =>
         upd.all.find( _.id == s )
      }.toList

      (s,sUpdates)
    }.toMap
  }

  var predictions:Map[Long,Map[OwnedGameObjectId,Vector3f]] = Map[Long,Map[OwnedGameObjectId,Vector3f]]()

  def interpolate(timeSinceLast:Long) : List[AbstractOwnedGameObject with Savable] = {
    val res = projectGameHistoryByGameObjectId.toList.map {
      case (id,snapshotsUT) =>

        val snapshots = snapshotsUT.asInstanceOf[List[AbstractOwnedGameObject with Savable]]

        val estimate = if(snapshots.size < 2 || id._2 == gameApp.playerIdOpt.get) {
          //println("unable to interpolate")
          None
        } else {

          var start = snapshots.reverse.tail.head
          var end = snapshots.last
          var startPos = start.position

          var snapDeltaTime = (snapshots.last.sentToServer - start.sentToServer)
          var velocity = end.position.subtract(start.position).divide(snapDeltaTime)
          /*val velocities = snapshots.tail.map { x =>
            val r = x._2.position.subtract(startPos).divide(x._1 - startTime) ; startTime = x._1 ; startPos = x._2.position ; r
          }

          val sumVelocity = velocities.tail.foldLeft(velocities.head)( (a,b) => a.add(b))
          val avgVelocity = sumVelocity.divide(velocities.size.toFloat)
          val extrapolTranslation = avgVelocity.mult(timeSinceLast)
            */

          val extrapolTranslation = velocity.mult(timeSinceLast)

          

          var startAngle = start.direction
          var endAngle = snapshots.last.direction


          val interpolationFactor: Float = (timeSinceLast + snapDeltaTime).toFloat / snapDeltaTime.toFloat
          //println("adf " + interpolationFactor)
          val exptrapolRotation = Quaternion.IDENTITY.slerp(startAngle,endAngle, interpolationFactor)


          val go = snapshots.last match {
            case p:PlayerGO => 
              new PlayerGO(p)
            case p:ProjectileGO =>
              new ProjectileGO(p)
          }
          //println("Interpolating " + avgVelocity.mult(timeSinceLast) + go.playerId )

          go.direction = exptrapolRotation
          go.position = go.position.add(extrapolTranslation)

          if(gameApp.playerIdOpt.get % 2 == 1) {
            //println("a " + velocity + " " + extrapolTranslation + " " + timeSinceLast + " " + snapDeltaTime + " " + go.position)
          }
          Some(go)
        }
        val r:AbstractOwnedGameObject with Savable = estimate.getOrElse(snapshots.last)

        r
    }

    res
  }   
  
  override def update(tpf: Float) {
    if(gameApp.playerIdOpt.isEmpty) return
    if(hasUnProcessedWorldUpdate) {
      //gameApp.syncGameWorld(gameWorldUpdates.last.all)
      hasUnProcessedWorldUpdate = false
    }
    if(gameWorldUpdates.size > 0) {

      val updates = interpolate(System.currentTimeMillis()-gameWorldUpdates.last.timeStamp)
      gameApp.syncGameWorld(updates)
    }

    accTranslation = accTranslation.add(gameApp.playerInput._1)
    accRotation = gameApp.playerInput._2.mult(accRotation)

    if(System.currentTimeMillis() - lastSentUpdate > 1000/15 ) {
      val request: PlayerActionRequest = new PlayerActionRequest

      request.timeStamp = System.currentTimeMillis()
      request.playerId = gameApp.playerIdOpt.get
      request.translation = accTranslation
      request.rotation = accRotation
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

  def initClient() {
    gameClient = new Client();

    val kryo = gameClient.getKryo();

    GameServer.getNetworkMessages.foreach( kryo.register(_))

    gameClient.addListener(new Listener() {
       override def received (connection:Connection , obj:Object ) {
         obj match {
            case response:ServerGameWorld=>
              if(gameApp.playerIdOpt.isDefined) {
                gameWorldUpdates =
                  Option(gameWorldUpdates).map(
                    x => if(x.size >= GW_UPDATES_SIZE) {x.dequeue._2} else {x}
                  ).head.enqueue(response)
                hasUnProcessedWorldUpdate = true
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
