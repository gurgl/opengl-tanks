package se.bupp.lek.client

import com.jme3.material.Material
import com.jme3.scene.shape.Box
import com.jme3.app.Application._
import com.jme3.font.BitmapText
import com.jme3.math.{Quaternion, Vector3f}
import com.jme3.bounding.BoundingSphere
import com.jme3.app.SimpleApplication._
import com.jme3.light.DirectionalLight
import se.bupp.lek.spel.ProjectileHandler
import com.jme3.scene.{Spatial, Node, Geometry}
import com.jme3.asset.{ModelKey, AssetManager}
import collection.immutable.HashSet
import se.bupp.lek.server.Server.{AbstractOwnedGameObject, PlayerGO, ProjectileGO}
import com.jme3.export.Savable

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 6/4/12
 * Time: 3:57 PM
 * To change this template use File | Settings | File Templates.
 */


object ClientWorld {
  object SceneGraphUserDataKeys {
    val Projectile= "ProjectileGO"
    val Player = "PlayerGO"
  }
  object SceneGraphNodeKeys {
    val Projectiles = "Projectiles"
    val Enemies= "Enemies"
  }
}

class ClientWorld(val rootNode:Node,val assetManager:AssetManager, playerIdOpt:() => Option[Int]) {
  import ClientWorld._

  var mat_default : Material = _

  var player:Spatial = _

  var projectileHandler : ProjectileHandler = _


  def init() {

    materializePlayer()
    materializeLevel()
    //materializeStats()

    var enemyNodes = new Node(SceneGraphNodeKeys.Enemies)
    rootNode.attachChild(enemyNodes)
    var projectileNodes = new Node(SceneGraphNodeKeys.Projectiles)
    rootNode.attachChild(projectileNodes)

    // You must add a light to make the model visible
    val sun = new DirectionalLight();
    sun.setDirection(new Vector3f(-0.1f, -0.7f, -1.0f));
    rootNode.addLight(sun);

    projectileHandler = new ProjectileHandler(mat_default)
    projectileHandler.init()

  }

  def getCamPosition() : (Vector3f,Quaternion) = {

    val pos = player.getLocalTranslation
    val camPos = pos.add(Vector3f.UNIT_XYZ.mult(5))

    val dir = pos.subtract(camPos).normalize()

    val rot = Quaternion.IDENTITY
    rot.lookAt(dir, new Vector3f(0, 1, 0))

    (camPos,rot)
  }



  def materializeLevel() {
    // Create a wall with a simple texture from test_data
    val box = new Box(Vector3f.ZERO, 2.5f, 2.5f, 1.0f);
    val wall = new Geometry("Box", box);
    val mat_brick = new Material(
      assetManager, "Common/MatDefs/Misc/Unshaded.j3md");
    mat_brick.setTexture("ColorMap",
      assetManager.loadTexture("Textures/Terrain/BrickWall/BrickWall.jpg"));
    wall.setMaterial(mat_brick);
    wall.setLocalTranslation(2.0f, -2.5f, 0.0f);
    rootNode.attachChild(wall);

  }

  def materializePlayer() {
    mat_default = new Material(assetManager, "Common/MatDefs/Misc/ShowNormals.j3md");

    player = assetManager.loadModel("Models/Teapot/Teapot.obj")

    player.setLocalRotation(Quaternion.IDENTITY)
    player.setMaterial(mat_default);

    rootNode.attachChild(player);
  }

  def materializeProjectile(p:ProjectileGO) {

    println("adding projectile " + p.position + "" + p.id)
    val instance = new Geometry("Box", projectileHandler.projectileGeometry);
    instance.setModelBound(new BoundingSphere())
    instance.updateModelBound()
    instance.setMaterial(mat_default)
    instance.setLocalTranslation(p.position)
    instance.setUserData("ProjectileGO",p)

    rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].attachChild(instance)
  }

  def materializeEnemy(pd:PlayerGO) {
    val enemy = assetManager.loadModel(new ModelKey("Models/Teapot/Teapot.obj"))
    enemy.setMaterial(mat_default)

    println("Materialize enemy " + pd.position + " " + pd.direction)
    enemy.setUserData(SceneGraphUserDataKeys.Player,pd)
    enemy.setLocalTranslation(pd.position)
    enemy.setLocalRotation(pd.direction)

    enemy.setModelBound(new BoundingSphere())
    enemy.updateModelBound()

    println(enemy.getClass.getName)
    import scala.collection.JavaConversions.asScalaBuffer
    //enemy.asInstanceOf[Node].getChildren.toList.foreach( x => println(x.getClass.getName))
    println("........................")
    println(enemy.asInstanceOf[Geometry].getModelBound)
    rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].attachChild(enemy)

  }

  def setMatch[A,B](left:Set[A],right:Set[B],comp:Function2[A,B,Boolean]) : Tuple3[Set[A],Set[B],Set[(A,B)]] = {
      var leftLeft = new HashSet[A]()
      var rightLeft = new HashSet[B]() ++ right
      var matched = new HashSet[(A,B)]()

      left.foreach { l =>
        rightLeft.find(r => comp(l,r)) match {
          case Some(rr) =>
            rightLeft -= rr
            matched = matched + Pair(l, rr)
            ()
          case None =>
            leftLeft += l
            ()
        }
      }
      (leftLeft,rightLeft, matched)
    }


    type L = AbstractOwnedGameObject with Savable
    def syncGameWorld(allUpdates:Set[_ <: AbstractOwnedGameObject with Savable]) {

      import scala.collection.JavaConversions.asScalaBuffer
      val enemyNodes = rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].getChildren
      val projectileNodes = rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].getChildren
      val enemyMap = enemyNodes.map( e => (e.getUserData[PlayerGO](SceneGraphUserDataKeys.Player),e).ensuring(_._1 != null) ).toMap
      val projectileMap = projectileNodes.map( e => (e.getUserData[ProjectileGO](SceneGraphUserDataKeys.Projectile),e).ensuring(_._1 != null) ).toMap

      var allExisting = enemyMap.toSet ++ projectileMap.toSet

      def doMatch(l:L,r:(L,Spatial)) : Boolean = {
        (l, r._1) match {
          case (l:PlayerGO, rr:PlayerGO) =>
            l.playerId == rr.playerId
          case (l:ProjectileGO, rr:ProjectileGO) =>
            l.id == rr.id
          case _ => false
        }
      }
      var (newInUpdateOrPlayer, noUpdate, matched) = setMatch(allUpdates, allExisting, doMatch)

      if(false && (System.currentTimeMillis()) % 10 == 3) {
        println("enemies" + enemyNodes.size +
          "noUpdate" + noUpdate.size +
          "newInUpdate " + newInUpdateOrPlayer.size +
          "projectileMap " + projectileMap.size +
          "matched " + matched.size +
          "allU "+allUpdates.size
        )
        enemyMap.foreach {  case (k,v) => println( "pos " +  k.position + " " + v.getLocalTranslation()) }

      }

      newInUpdateOrPlayer.foreach {
        case p:PlayerGO =>
          if(p.playerId == playerIdOpt.apply().get) {
            player.setLocalTranslation(p.position)
            player.setLocalRotation(p.direction)
          } else {
            materializeEnemy(p)
          }
        case p:ProjectileGO =>
          materializeProjectile(p)
      }

      matched.foreach {
        case (u:AbstractOwnedGameObject with Savable,(o,s:Spatial)) =>
          //println("upd match" + s.getLocalTranslation + " " + u.position + " " +u.direction)
          s.setLocalTranslation(u.position.clone())
          s.setLocalRotation(u.direction.clone())
          u match {
            case p:ProjectileGO => s.setUserData(SceneGraphUserDataKeys.Projectile, p)
            case p:PlayerGO => s.setUserData(SceneGraphUserDataKeys.Player, p)
          }
      }
      noUpdate.foreach {
        case (o, spatial) =>
          println("removing " + o.id + " " + o.getClass)
          o match {
            case p:ProjectileGO => rootNode.getChild(SceneGraphNodeKeys.Projectiles).asInstanceOf[Node].detachChild(spatial)
            case p:PlayerGO => rootNode.getChild(SceneGraphNodeKeys.Enemies).asInstanceOf[Node].detachChild(spatial)
          }
      }



      /*allUpdates.foreach {
        case p:ProjectileGO =>
          val projectileOpt = projectileMap.find { case (pd, spatial ) => pd.id == p.id }
          projectileOpt match {
            case Some((proj, spatial)) =>
              spatial.setLocalTranslation(p.position)
              spatial.setLocalRotation(p.direction)
            case None =>
              materializeProjectile()
          }
        case p:PlayerGO =>
          if(p.playerId == playerIdOpt.get) {
            //println("Getting server state" + p.position + " " + p.direction)

            player.setLocalTranslation(p.position)
            player.setLocalRotation(p.direction)
          } else {
            val enemyOpt = enemyMap.find { case (pd, spatial ) => pd.playerId == p.playerId }

            enemyOpt match {
              case Some((enemyPd, spatial)) =>
                //println("updated " + p.position)
                //println("updating existing " + enemyPd.position)
                spatial.setUserData("PlayerGO",enemyPd)
                spatial.setLocalTranslation(p.position)
                spatial.setLocalRotation(p.direction)
              case None => materializeEnemy(p)
            }
          }
        case _ =>
      }*/

    }


}

