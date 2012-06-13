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

    "lol" in {


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

      val tpf = 1.0f / 60.0f
      pSpace.update(tpf * 1.0f)
      pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.3f,0f,0f))


      pSpace.update(tpf * 1.0f)
            pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.6f,0f,0f))

      playerControl.setWalkDirection(new Vector3f(.0f,0f,8f))
      pSpace.update(tpf * 1.0f)
            pSpace.distributeEvents()

      playerControl.getPhysicsLocation should be equalTo(pd.position.add(0.6f,0f,8f))



    }
  }

  "jing" should {
    "jang" in {

      val myWorld = new Lallers {

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
      clock += 100
      myWorld.addPlayerAction(p1.playerId,new MotionGO(new Vector3f(0.4f,0f,0f),MathUtil.noRotation,clock), 2)

      val su = myWorld.getSortedUpdates(myWorld.getPlayers(),10100L)
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
      myWorld.simulateUpdatesUntil(su, 10000L)
      myWorld.getPlayers().head._2.getControl(classOf[CharacterControl]).getPhysicsLocation should be equalTo(new Vector3f(0.4f,1f,0f))


      1 should be equalTo (1)
    }
  }
}
