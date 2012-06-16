package se.bupp.lek

import client.SceneGraphWorld.SceneGraphUserDataKeys
import client.{SceneGraphWorld, MathUtil}
import com.jme3.asset.{DesktopAssetManager, AssetManager, ModelKey}
import com.jme3.bullet.PhysicsSpace
import com.jme3.bullet.PhysicsSpace.BroadphaseType
import com.jme3.renderer.queue.RenderQueue.ShadowMode
import com.jme3.scene.{Spatial, Node}
import org.specs2.mutable.Specification
import com.jme3.system.{JmeContext, AppSettings, JmeSystem}
import com.jme3.bullet.control.CharacterControl
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape
import server.Model._
import server.{Lallers, WorldSimulator}
import com.jme3.math.{Vector3f, Quaternion}
import scalaz.NonEmptyList

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/12/12
 * Time: 7:33 PM
 * To change this template use File | Settings | File Templates.
 */

class PhysicsTest extends Specification {

  val broadphaseType = BroadphaseType.DBVT;
  val worldMin = new Vector3f(-10000f, -10000f, -10000f);
  val worldMax = new Vector3f(10000f, 10000f, 10000f);


  object Clock {
    def round(f:Float) = {
      math.floor(f*1000f) / 1000f
    }.toFloat
  }
  class Clock(var time:Long) {


    def add(millis:Long) = {
      time += millis
      time
    }
  }

  class SeqGen(var seqId:Int) {

    def next = {
      seqId += 1
      seqId
    }
  }


  def addPlayerBare(pSpace:PhysicsSpace, pd:Orientation, node:Node, assetManager:AssetManager) = {
    val player = assetManager.loadModel(new ModelKey("tank2.blend"))
    //enemy.setMaterial(mat_default)


    player.setLocalScale(0.5f)
    //tank.setLocalTranslation(pd.position)
    player.setLocalRotation(pd.direction)

    val capsuleShape = new CapsuleCollisionShape(0.05f, 0.05f, 1)
    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    playerControl.setUseViewDirection(false)
    player.addControl(playerControl)

    pSpace.add(playerControl)


    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0.0f);
    playerControl.setGravity(0.0f);
    playerControl.setPhysicsLocation(pd.position.clone());


    node.attachChild(player)
    player

  }


  "lal" should {

    "lil" in {
      Clock.round(1f/60f).shouldEqual(0.016f)
    }

    "lol" in {


      val settings = new AppSettings(true);
      val context = JmeSystem.newContext(settings, JmeContext.Type.Headless)
      //context.setSystemListener(this);
      context.create(false)

      val node = new Node()
      val pSpace = new PhysicsSpace(worldMin, worldMax, broadphaseType);

      val pd = new Orientation(new Vector3f(0, 2.5f, 0), MathUtil.noRotation)
      //val assetManager = new DesktopAssetManager()
      val assetManager = JmeSystem.newAssetManager(
        Thread.currentThread().getContextClassLoader()
          .getResource("com/jme3/asset/Desktop.cfg"))


      val player = addPlayerBare(pSpace,pd,node,assetManager)

      val playerControl = player.getControl(classOf[CharacterControl])
      playerControl.setWalkDirection(new Vector3f(.3f,0f,0f))

      /*pSpace.update( 0.016f)
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.3f,0f,0f))
        */


      val tpf = 1.0f / 60.0f
      //println("Tjooo" + (tpf * 3.0f))
      pSpace.update(Clock.round(tpf * 1.0f))
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.3f,0f,0f))


      pSpace.update(Clock.round(tpf * 3.0f))
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(1.2f,0f,0f))

      playerControl.setWalkDirection(new Vector3f(.0f,0f,8f))
      pSpace.update(Clock.round(tpf * 1.0f))
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(1.2f,0f,8f))
    }

    "lul" in {


      val settings = new AppSettings(true);
      val context = JmeSystem.newContext(settings, JmeContext.Type.Headless);
      //context.setSystemListener(this);
      context.create(false);

      val node = new Node()
      val pSpace = new PhysicsSpace(worldMin, worldMax, broadphaseType);

      val pd = new Orientation(new Vector3f(0, 2.5f, 0), MathUtil.noRotation)
      //val assetManager = new DesktopAssetManager()
      val assetManager = JmeSystem.newAssetManager(
        Thread.currentThread().getContextClassLoader()
          .getResource("com/jme3/asset/Desktop.cfg"))


      val player = addPlayerBare(pSpace,pd,node,assetManager)

      val playerControl = player.getControl(classOf[CharacterControl])
      playerControl.setWalkDirection(new Vector3f(.3f,0f,0f))

      //val tpf = 1.0f / 60.0f
      val tpf = 1.0f / 60.0f

      pSpace.update(Clock.round(tpf*1f))
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.3f,0f,0f))

      playerControl.setWalkDirection(new Vector3f(.3f,0f,0f))
      pSpace.update(Clock.round(tpf * 3.0f))
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(1.2f,0f,0f))

      pSpace.update(Clock.round(tpf * 6.0f))
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(3.0f,0f,0f))
    }
  }


  class SimpleWorld extends Lallers {

    val settings = new AppSettings(true);
    val context = JmeSystem.newContext(settings, JmeContext.Type.Headless);
    //context.setSystemListener(this);
    context.create(false);

    val rootNode = new Node()
    rootNode.attachChild(new Node(SceneGraphWorld.SceneGraphNodeKeys.Enemies))
    val pSpace = new PhysicsSpace(worldMin, worldMax, broadphaseType);

    val pd = new Orientation(new Vector3f(0, 2.5f, 0), MathUtil.noRotation)
    //val assetManager = new DesktopAssetManager()
    val assetManager = JmeSystem.newAssetManager(
      Thread.currentThread().getContextClassLoader()
        .getResource("com/jme3/asset/Desktop.cfg"))


    def addPlayer(ps: PlayerStatus) = {
      val p = addPlayerBare(pSpace,ps.state, rootNode, assetManager)
      p.setUserData(SceneGraphUserDataKeys.Player, ps)
      getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).attachChild(p)

    }

    def getNode(key: String) = rootNode.getChild(key).asInstanceOf[Node]

    def addPlayerAction(playerId:Int, motion:MotionGO, seqId:Int) {
      findPlayer(playerId).foreach {
        x => {

          x.state.sentToServerByClient = motion.sentToServer
          x.reorientation = x.reorientation :+ motion

          x.seqId = seqId

        }
      }
    }
    override def getPhysicsSpace = pSpace
  }

  "jing" should {
    "jang" in {

      val myWorld = new SimpleWorld



      var clock: Long = 10000

      val ps1 = new PlayerStatus()
      val p1 = new PlayerGO()
      ps1.state = p1
      p1.position = new Vector3f(0f,1f,0f)
      p1.direction = Quaternion.DIRECTION_Z.clone()
      p1.sentToServerByClient = clock
      p1.playerId = 1
      p1.clientSeqId = 2


      myWorld.addPlayer(ps1)
      val step: Int = 16
      clock += step
      myWorld.addPlayerAction(p1.playerId,new MotionGO(new Vector3f(0.4f,0f,0f),MathUtil.noRotation,clock), 2)

      val su = myWorld.popPlayerUpdatesLessOrEqualToTimeSorted(myWorld.getPlayers(),10000L + step)
      su.size should be equalTo(1)
      //myWorld.simulateToLastUpdated(clock-100)

      su.toList match {
        case x :: xs => xs.shouldEqual(Nil)
          x match {
            case (t, l) =>
              l.list match {
                case y :: ys => ys.shouldEqual(Nil)
                  y match {
                    case (ps,s,m) => println("O " + t + ", " + ps  + ", " + s  + ", " + m)
                  }
                case _ => failure("bad")
              }
            case _ => failure("should contain one update")
          }
        case _ => failure("should be list")
      }
      myWorld.simulateToLastUpdated(10000)
      myWorld.getPlayers().head._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))


      1 should be equalTo (1)
    }



    "handle multi player" in {

      val myWorld = new SimpleWorld

      var seqGen = new SeqGen(1)
      var clock = new Clock(10000)

      val ps1 = new PlayerStatus()
      val p1 = new PlayerGO()
      ps1.state = p1
      p1.position = new Vector3f(0f,1f,0f)
      p1.direction = Quaternion.DIRECTION_Z.clone()
      p1.sentToServerByClient = clock.time
      p1.playerId = 1
      p1.clientSeqId = 2

      val ps2 = new PlayerStatus()
      val p2 = new PlayerGO()
      ps2.state = p2
      p2.position = new Vector3f(0f,1f,0f)
      p2.direction = Quaternion.DIRECTION_Z.clone()
      p2.sentToServerByClient = clock.time
      p2.playerId = 2
      p2.clientSeqId = 2


      myWorld.addPlayer(ps1)
      myWorld.addPlayer(ps2)
      val step: Int = 16
      //clock.add(step)
      myWorld.addPlayerAction(p1.playerId,new MotionGO(new Vector3f(0.4f,0f,0f),MathUtil.noRotation,clock.add(15)), seqGen.next)
      myWorld.addPlayerAction(p2.playerId,new MotionGO(new Vector3f(0.4f,0f,0f),MathUtil.noRotation,clock.add(15)), seqGen.next)

      ps1.reorientation.size.shouldEqual(1)
      ps2.reorientation.size.shouldEqual(1)



      //val su = myWorld.popPlayerUpdatesLessOrEqualToTimeSorted(myWorld.getPlayers(),10030L) // + step)
      //su.size should be equalTo(2)

      val oldestUpdate = myWorld.getOldestUpdate(myWorld.getPlayers)
      oldestUpdate.shouldEqual(10015)

      val newTime = myWorld.simulateToLastUpdated(10000)

      newTime.shouldEqual(10015)


      myWorld.findPlayerInfo(1).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))
      myWorld.findPlayerInfo(2).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))

      1.shouldEqual(1)
    }
  }
}
