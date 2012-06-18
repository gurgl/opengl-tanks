package se.bupp.lek.server

import com.esotericsoftware.kryonet.{Connection, Listener, Server => KryoServer}
import scala.Predef._
import management.ManagementFactory
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.export.{JmeExporter, JmeImporter, Savable}
import com.jme3.system.JmeContext
import collection.JavaConversions
import se.bupp.lek.server.Model._

import JavaConversions.asScalaBuffer
import collection.mutable.{HashMap, ArrayBuffer}
import se.bupp.lek.server.Model._
import scala.None
import com.jme3.app.{FlyCamAppState, SimpleApplication}
import com.jme3.bullet.{PhysicsSpace, PhysicsTickListener, BulletAppState}
import com.jme3.scene.{Node, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/28/12
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */


class Server extends SimpleApplication with PhysicsTickListener {



  import Model._
  import Server._

  override def prePhysicsTick(p1: PhysicsSpace, p2: Float) {}

  override def physicsTick(p1: PhysicsSpace, p2: Float) {}

  var updateMaxTimeStamp = System.currentTimeMillis()

  var worldSimulator: WorldSimulator = _

  var server:KryoServer = _

  override def simpleInitApp() {

    stateManager.detach( stateManager.getState(classOf[FlyCamAppState]))
    //val bulletAppState = new BulletAppState();

    //bulletAppState.startPhysics()
    //stateManager.attach(bulletAppState);
    flyCam.setEnabled(false)

    val leRoot = new Node()
    val physicsSpace = new PhysicsSpace()
    val serverWorld = new ServerWorld(leRoot, assetManager, physicsSpace)
    serverWorld.initEmpty()
    worldSimulator = new WorldSimulator(serverWorld)

    server = new KryoServer();
    server.start();
    server.bind(54555, 54777);

    val kryo = server.getKryo();
    getNetworkMessages.foreach(kryo.register(_))

    server.addListener(new Listener() {
      override def received(connection: Connection, obj: Object) {
        //println("rec " + obj.getClass.getName)
        obj match {
          case request: PlayerActionRequest =>
            //println(request.playerId + " " + request.position);
            request.ensuring(request.motion.translation != null && request.motion.rotation != null)

            //updateMaxTimeStamp = if(updateMaxTimeStamp < request.timeStamp) request.timeStamp else updateMaxTimeStamp
            worldSimulator.addPlayerAction(request)


          /*val response = new GameWorldResponse();
          response.text = "Thanks!";
          connection.sendUDP(response);*/
          case req: PlayerJoinRequest =>
            println("rec " + obj.getClass.getName)
            val resp = new PlayerJoinResponse
            resp.playerId = worldSimulator.addPlayer(req)

            connection.sendTCP(resp)
          case _ =>
        }
      }
    });

  }


  var lastSentUpdate = 0L
  override def simpleUpdate(tpf: Float) {

    if(System.currentTimeMillis() - lastSentUpdate > 1000/16) {


        val gameWorld = worldSimulator.getGameWorld
        //println("" + getPlayers.size)
        server.sendToAllUDP(gameWorld)
        lastSentUpdate = System.currentTimeMillis()

    }
  }
}

object Server {

  val getNetworkMessages = List[Class[_ <: AnyRef]](
      classOf[PlayerJoinRequest],
      classOf[PlayerJoinResponse],
      classOf[PlayerActionRequest],
      classOf[GameWorldResponse],
      classOf[Vector3f],
      classOf[Quaternion],
      classOf[PlayerGO],
      classOf[ProjectileGO],
      classOf[MotionGO],
      classOf[OrientationGO],
      classOf[ProjectileFireGO],
      classOf[java.util.ArrayList[PlayerGO]],
      classOf[ServerGameWorld]
    )
  def main(args: Array[String]) {
    new Server().start(JmeContext.Type.Headless)
  }
}

