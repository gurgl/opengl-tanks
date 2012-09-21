package se.bupp.lek.server

import com.jme3.scene.{Spatial, Node}
import com.jme3.asset.AssetManager
import com.jme3.bullet.PhysicsSpace
import se.bupp.lek.client.SceneGraphWorld
import se.bupp.lek.server.Model.{PlayerConnection, ProjectileGO}
import com.jme3.bullet.collision.shapes.{CapsuleCollisionShape, SphereCollisionShape}
import com.jme3.bullet.control.{GhostControl, CharacterControl, RigidBodyControl}
import com.jme3.bounding.BoundingSphere
import com.jme3.math.Vector3f
import se.bupp.lek.client.SceneGraphWorld.SceneGraphUserDataKeys
import org.apache.log4j.Logger

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-06-24
 * Time: 11:47
 * To change this template use File | Settings | File Templates.
 */

class ServerWorld(rootNode: Node, assetManager:AssetManager, physicsSpace:PhysicsSpace) extends SceneGraphWorld(true,assetManager,rootNode) with PhysicsSpaceSimAdapter {

  val log = Logger.getLogger(classOf[ServerWorld])

  var simCurrentTime:Long = System.currentTimeMillis()

  def initEmpty() {
    super.init()
  }

  def getProjectiles() = projectNodeChildrenByData[ProjectileGO](SceneGraphWorld.SceneGraphNodeKeys.Projectiles, SceneGraphWorld.SceneGraphUserDataKeys.Projectile)

  def spawnProjectile(pr:ProjectileGO) {
    val instance = materializeProjectile2(pr)

    //val ctrl = new ProjectileCollisionControl()


    val sphereShape =
      new SphereCollisionShape(0.1f)
    val control = new RigidBodyControl(sphereShape)
    instance.setModelBound(new BoundingSphere())
    instance.updateModelBound()
    control.setLinearVelocity(pr.direction.getRotationColumn(0).mult(pr.speed))
    //control.setPhysicsRotation(p.direction);
    //control.setAngularVelocity(Vector3f.ZERO.clone())
    control.setMass(1.0f)
    control.setGravity(Vector3f.ZERO.clone())

    //control.setLinearDamping(0f)
    control.setKinematic(false)

    instance.addControl(control)
    getPhysicsSpace.add(control)


    //getPhysicsSpace.addCollisionListener(control)
  }

  def unspawnPlayer(p:PlayerConnection) {
    findPlayerInfo(p.playerId).foreach { case (_,s) => unspawnPlayer(s, p) }
  }

  def unspawnPlayer(s: Spatial, p:PlayerConnection) = {
    log.info("Unspawning player " + p.playerId)

    val characterControl = s.getControl(classOf[CharacterControl])
    getPhysicsSpace.remove(characterControl)
    val ghostControl= s.getControl(classOf[GhostControl])
    getPhysicsSpace.remove(ghostControl)
    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).detachChild(s)
  }

  def unspawnProjectile(s: Spatial, p:ProjectileGO) = {
    log.info("Unspawning projectile" + p.id)

    val rigidBodyControl = s.getControl(classOf[RigidBodyControl])
    getPhysicsSpace.remove(rigidBodyControl)
    getNode(SceneGraphWorld.SceneGraphNodeKeys.Projectiles).detachChild(s)
  }


  def spawnPlayer(ps:PlayerConnection) {
    log.info("Spawn player")
    val tankGeo = materializeTank2(ps.gameState)
    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    //val tank = new Node("Bupp")
    val tank = new Node("Tank")


    tank.attachChild(tankGeo)
    //tankGeo.setLocalTranslation(Vector3f.ZERO.setY(0.5f))
    tank.setUserData(SceneGraphUserDataKeys.Player, ps)


    //tank.attachChild(tankModel)
    val capsuleShape = tankCollisionShape

    val capsuleShapeGhost = new CapsuleCollisionShape(0.4f, 0.4f, 1)

    val playerControl = new CharacterControl(capsuleShape, 0.1f)
    tank.addControl(playerControl)

    getPhysicsSpace.add(playerControl)

    playerControl.setUseViewDirection(false)

    playerControl.setJumpSpeed(0);
    playerControl.setFallSpeed(0.3f);
    playerControl.setGravity(0.3f);
    playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));

    val ghost: GhostControl = new GhostControl(capsuleShapeGhost) /*{
      override def update(tpf:Float) {

        if (!enabled) {
          return;
        }
        val vectorf: Vector3f = {
          if (applyLocal) {
            spatial.getLocalTranslation()
          } else spatial.getWorldTranslation()

        }
        setPhysicsLocation(vectorf);
        val quaternion: Quaternion = {
          if (applyLocal) {
            spatial.getLocalRotation();
          } else spatial.getWorldRotation()
        }
        setPhysicsRotation(quaternion);

        //println("ghost update" + vectorf + " " + quaternion)
      }
    }*/

    //ghost.setApplyPhysicsLocal(true)
    tank.addControl(ghost);

    //getPhysicsSpace.add(ghost)

    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).attachChild(tank)
  }

  def getPhysicsSpace = physicsSpace
}
