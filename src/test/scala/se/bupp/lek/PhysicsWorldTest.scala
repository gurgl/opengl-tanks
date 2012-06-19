package se.bupp.lek

import client.SceneGraphWorld.SceneGraphUserDataKeys
import client.{SceneGraphWorld, MathUtil}
import server.PhysicsSpaceSimAdapter
import server.Model.{Orientation, MotionGO, PlayerGO, PlayerStatus}
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.bullet.control.CharacterControl
import com.jme3.system.{JmeContext, JmeSystem, AppSettings}
import com.jme3.scene.Node
import com.jme3.bullet.PhysicsSpace
import com.jme3.asset.{ModelKey, AssetManager}
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape
import org.specs2.mutable.Specification
import com.bulletphysics.linearmath

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-06-16
 * Time: 22:26
 * To change this template use File | Settings | File Templates.
 */

object Clock {
  def round(f:Float) = {
    math.floor(f*1000f) / 1000f
  }.toFloat
}

class PhysicsWorldTest extends Specification {



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


  class SimpleWorld(accuracy:Float = 1f/60f) extends PhysicsSpaceSimAdapter {

    var simCurrentTime:Long = _

    val settings = new AppSettings(true);
    val context = JmeSystem.newContext(settings, JmeContext.Type.Headless);
    //context.setSystemListener(this);
    context.create(false);

    val rootNode = new Node()
    rootNode.attachChild(new Node(SceneGraphWorld.SceneGraphNodeKeys.Enemies))
    val pSpace = new PhysicsSpace(Physics.worldMin, Physics.worldMax, Physics.broadphaseType);
    pSpace.setAccuracy(accuracy)

    val pd = new Orientation(new Vector3f(0, 2.5f, 0), MathUtil.noRotation)
    //val assetManager = new DesktopAssetManager()
    val assetManager = JmeSystem.newAssetManager(
      Thread.currentThread().getContextClassLoader()
        .getResource("com/jme3/asset/Desktop.cfg"))


    def addPlayer(ps: PlayerStatus) = {
      val p = Physics.addPlayerBare(pSpace,ps.state, rootNode, assetManager)
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
    override def isTest = true
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
      myWorld.simCurrentTime = 10000
      myWorld.simulateToLastUpdated()
      myWorld.getPlayers().head._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))


      1 should be equalTo (1)
    }


    "handle multi player" in {

      val myWorld = new SimpleWorld(1f/100f)

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
      val step = 10L
      //clock.add(step)

      myWorld.findPlayerInfo(1).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.0f,1f,0f))
      myWorld.findPlayerInfo(2).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.0f,1f,0f))




      myWorld.addPlayerAction(p1.playerId,new MotionGO(new Vector3f(0.4f,0f,0f),MathUtil.noRotation,clock.add(step)), seqGen.next)
      myWorld.addPlayerAction(p2.playerId,new MotionGO(new Vector3f(0.5f,0f,0f),MathUtil.noRotation,clock.add(step)), seqGen.next)

      ps1.reorientation.size.shouldEqual(1)
      ps2.reorientation.size.shouldEqual(1)

      //val su = myWorld.popPlayerUpdatesLessOrEqualToTimeSorted(myWorld.getPlayers(),10030L) // + step)
      //su.size should be equalTo(2)

      //myWorld.pSpace.update(1f/100f)
      myWorld.pSpace.update(1f/100f)

      val oldestUpdate = myWorld.getOldestUpdateTimeLastReceived(myWorld.getPlayers)
      oldestUpdate.shouldEqual(10000 + step)
      myWorld.simCurrentTime = 10000
      val newTime = myWorld.simulateToLastUpdated()

      newTime.shouldEqual(oldestUpdate)

      myWorld.simCurrentTime.shouldEqual(oldestUpdate)


      myWorld.findPlayerInfo(1).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))
      myWorld.findPlayerInfo(2).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.0f,1f,0f))

      myWorld.addPlayerAction(p2.playerId,new MotionGO(new Vector3f(0.0f,0f,3f),MathUtil.noRotation,clock.add(step*2)), seqGen.next)
      myWorld.addPlayerAction(p1.playerId,new MotionGO(new Vector3f(0.0f,4f,0f),MathUtil.noRotation,clock.add(step)), seqGen.next)

      ps1.reorientation.size.shouldEqual(1)
      ps2.reorientation.size.shouldEqual(2)

      val oldestUpdateRnd2 = myWorld.getOldestUpdateTimeLastReceived(myWorld.getPlayers)
      oldestUpdateRnd2.shouldEqual(10000 + 4 * step)


      val newTimeRnd2 = myWorld.simulateToLastUpdated()

      newTimeRnd2.shouldEqual(oldestUpdateRnd2)

      myWorld.findPlayerInfo(1).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))
      myWorld.findPlayerInfo(2).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.5f,1f,3f))

    }

    /*
        1 2 3 4 5 6 7 8 9 0 1 2 3 4
    A     x         x       x
    B         x   x   x




    all movement in cycle => choppy

    stepped simulation

    linear : (simtime - simtime)

     */
  }

  "time independence" should {
    "handle multi player" in {


      val tpf = 1f/60f

      val myWorld = new SimpleWorld(tpf)

      var seqGen = new SeqGen(1)
      var clock = new Clock(10000)

      val ps1 = new PlayerStatus()
      val p1 = new PlayerGO()
      ps1.state = p1
      p1.position = new Vector3f(0f,0f,0f)
      p1.direction = Quaternion.DIRECTION_Z.clone()
      p1.sentToServerByClient = clock.time
      p1.playerId = 1
      p1.clientSeqId = 2

      val ps2 = new PlayerStatus()
      val p2 = new PlayerGO()
      ps2.state = p2
      p2.position = new Vector3f(0f,0f,0f)
      p2.direction = Quaternion.DIRECTION_Z.clone()
      p2.sentToServerByClient = clock.time
      p2.playerId = 2
      p2.clientSeqId = 2


      myWorld.addPlayer(ps1)
      myWorld.addPlayer(ps2)
      val step = 10L
      //clock.add(step)

      myWorld.findPlayerInfo(1).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.0f,0f,0f))
      myWorld.findPlayerInfo(2).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.0f,0f,0f))


      val startTime = System.currentTimeMillis()

      myWorld.simCurrentTime = startTime
      val rnd = new scala.util.Random()



      val span = tpf * 8
      val min = span / 2f

      var playerSimData = Map.empty[Int,(Long,Vector3f)] ++ myWorld.getPlayers.map( x =>  (x._1.state.playerId, (startTime, Vector3f.ZERO.clone())))


      //val nextSim = Map[Int,Long]()
      for( i <- 0 until 100) {
        playerSimData = playerSimData.map { case (p, (t, a)) =>
          val additionSeconds: Float = (rnd.nextFloat * span) + min
          val additionMillis: Long = math.round((additionSeconds * 1000).toDouble)
          println(additionMillis)
          val newTime = t + additionMillis
          (p, (newTime, a)) }

        try {

        }
        playerSimData.toList.sortWith { case (a,b) => a._2._1 < b._2._1 }.foreach {

          case (pid, (t, acc)) =>
          val translation: Vector3f = new Vector3f(0.4f, 0f, 0f)
          myWorld.addPlayerAction(pid,new MotionGO(translation,MathUtil.noRotation,t), seqGen.next)
          acc.set(acc.add(translation))

        }


      }
      var loops = 0
      var lastSimTime = 0L
      do {
        lastSimTime = myWorld.simCurrentTime
        myWorld.simulateToLastUpdated()
        loops += 1
      } while(lastSimTime < myWorld.simCurrentTime)

      //loops.shouldEqual(100)

      //startTime.shouldEqual(myWorld.simCurrentTime)
      myWorld.findPlayerInfo(1).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(playerSimData(1)._2)
      myWorld.findPlayerInfo(2).get._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(playerSimData(2)._2)

    }
  }
}
