package se.bupp.lek.server

import com.jme3.scene.{Spatial, Node}
import com.jme3.asset.AssetManager
import com.jme3.bullet.PhysicsSpace
import se.bupp.lek.common.SceneGraphWorld
import se.bupp.lek.common.Model.{GameParticipant, ProjectileGO}
import com.jme3.bullet.collision.shapes.{CapsuleCollisionShape, SphereCollisionShape}
import com.jme3.bullet.control.{BetterCharacterControl, GhostControl, CharacterControl, RigidBodyControl}
import com.jme3.bounding.BoundingSphere
import com.jme3.math.Vector3f
import se.bupp.lek.common.SceneGraphWorld.SceneGraphUserDataKeys
import org.slf4j.LoggerFactory
import com.jme3.bullet.collision.PhysicsCollisionObject

/**
 * created with intellij idea.
 * user: karlw
 * date: 2012-06-24
 * time: 11:47
 * to change this template use file | settings | file templates.
 */

class ServerWorld(rootNode: Node, assetManager:AssetManager, physicsSpace:PhysicsSpace) extends SceneGraphWorld(true,assetManager,rootNode) with PhysicsSpaceSimAdapter {

  val log = LoggerFactory.getLogger(classOf[ServerWorld])

  var simCurrentTime:Long = Server.clock()

  def initEmpty() {
    super.init()
  }

  def getProjectiles() = projectNodeChildrenByData[ProjectileGO](SceneGraphWorld.SceneGraphNodeKeys.Projectiles, SceneGraphWorld.SceneGraphUserDataKeys.Projectile)

  def spawnProjectile(pr:ProjectileGO) {
    val instance = materializeProjectileServer(pr)

    //val ctrl = new ProjectileCollisionControl()


    val sphereShape =
      new SphereCollisionShape(0.1f)
    val control = new RigidBodyControl(sphereShape)
    control.setCollisionGroup(PhysicsCollisionObject.COLLISION_GROUP_03)

    control.setCollideWithGroups(PhysicsCollisionObject.COLLISION_GROUP_01)
    control.addCollideWithGroup(PhysicsCollisionObject.COLLISION_GROUP_02)
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
    //control.setUserObject(pr)
    getPhysicsSpace.add(control)

    control.setGravity(Vector3f.ZERO.clone())


    //getPhysicsSpace.addCollisionListener(control)
  }

  def unspawnPlayer(p:GameParticipant) {
    findPlayerInfo(p.playerId).foreach { case (_,s) => unspawnPlayer(s, p) }
  }

  def unspawnPlayer(s: Spatial, p:GameParticipant) = {
    log.info("Unspawning player " + p.playerId)

    val characterControl = s.getControl(classOf[BetterCharacterControl])
    getPhysicsSpace.remove(characterControl)
    //val ghostControl= s.getControl(classOf[GhostControl])
    //getPhysicsSpace.remove(ghostControl)
    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).detachChild(s)
  }

  def unspawnProjectile(s: Spatial, p:ProjectileGO) = {
    log.info("Unspawning projectile" + p.id)

    val rigidBodyControl = s.getControl(classOf[RigidBodyControl])
    getPhysicsSpace.remove(rigidBodyControl)
    getNode(SceneGraphWorld.SceneGraphNodeKeys.Projectiles).detachChild(s)
  }


  def spawnPlayer(ps:GameParticipant) {
    log.info("Spawn player")
    val tank = materializeTank(ps.gameState)
    //tankGeo.setMaterial(if(ps.playerId % 2 == 0 ) mat_default_blue else mat_default_red)
    //enemy.setModelBound(new BoundingSphere())
    //enemy.updateModelBound()
    //val tank = new Node("Bupp")
    if(ps.playerId % 2 == 0)
      tank.setLocalTranslation(new Vector3f(0, 2.5f, 1))
    else
      tank.setLocalTranslation(new Vector3f(0, 2.5f, -1))
    tank.setUserData(SceneGraphUserDataKeys.Player, ps)


    //tank.attachChild(tankModel)
    val capsuleShapeGhost = new CapsuleCollisionShape(0.4f, 0.4f, 1)

    val playerControl = new BetterCharacterControl(tankCollisionShape.getRadius,tankCollisionShape.getHeight, 5f)
    //val playerControl = new BetterCharacterControl(0.3f, 2.5f, 8f);
    tank.addControl(playerControl)
    //playerControl.setViewDirection(ps.gameState.direction.getRotationColumn(0))
    //playerControl.setViewDirection(tankForward)
    playerControl.setGravity(gravity.clone());
    getPhysicsSpace.add(playerControl)


    //playerControl.setUseViewDirection(false)

    //playerControl.setJumpSpeed(0);
    //playerControl.setFallSpeed(0.3f);
    //playerControl.setGravity(0.3f);
    //playerControl.setPhysicsLocation(new Vector3f(0, 2.5f, 0));

   /* val ghost: GhostControl = new GhostControl(capsuleShapeGhost)
    ghost.setCollisionGroup(PhysicsCollisionObject.COLLISION_GROUP_02)
    ghost.setCollideWithGroups(PhysicsCollisionObject.COLLISION_GROUP_01)
    ghost.addCollideWithGroup(PhysicsCollisionObject.COLLISION_GROUP_03)
    tank.addControl(ghost);*/
    //ghost.setUserObject(ps)
   /*{

      override def querySendUpdate(tpf:Float) {

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

        //println("ghost querySendUpdate" + vectorf + " " + quaternion)
      }
    }*/

    //ghost.setApplyPhysicsLocal(true)


    //getPhysicsSpace.add(ghost)

    getNode(SceneGraphWorld.SceneGraphNodeKeys.Enemies).attachChild(tank)
  }

  def getPhysicsSpace = physicsSpace

  override def destroy() {
    super.destroy()
    physicsSpace.destroy()
  }
}
