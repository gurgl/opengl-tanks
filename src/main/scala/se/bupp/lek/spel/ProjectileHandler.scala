package se.bupp.lek.spel

import com.jme3.scene.shape.Box
import com.jme3.math.{Quaternion, Vector3f}
import collection.mutable.ArrayBuffer
import com.jme3.scene.{Spatial, Node, Geometry}
import com.jme3.material.Material
import com.jme3.collision.CollisionResults
import com.jme3.bounding.{BoundingSphere, BoundingBox}
import collection.immutable.Stack
import se.bupp.lek.spel.GameServer.{OrientationGO, ProjectileGO, ProjectileFireGO}

/**
 * Created by IntelliJ IDEA.
 * User: karlw
 * Date: 5/26/12
 * Time: 1:45 AM
 * To change this template use File | Settings | File Templates.
 */

class ProjectileHandler(val mat:Material) {

  var projectileGeometry:Box = _

  var projectileSeqId = 0
  class Projectile(val geometry:Geometry, val velocity:Vector3f, val maxDistanceTraveled:Float) {
    var distanceTraveled = 0f
  }

  var flying = new ArrayBuffer[Projectile]()

  var fired = Stack[ProjectileFireGO]()

  def purgeFired() : List[ProjectileFireGO] = {

    val res = fired.toList
    fired = Stack[ProjectileFireGO]()
    res
  }
  def fireProjectile(pos:Vector3f, dir:Quaternion) = {

    fired = fired.push(
      new ProjectileFireGO(
        new OrientationGO(pos,dir.clone()),
        0.3f,
        System.currentTimeMillis(),
        projectileSeqId
      ))
    projectileSeqId += 1
  }

  def fire(pos:Vector3f, dir:Vector3f) : Spatial = {

    println("firing!")
    val instance = new Geometry("Box", projectileGeometry);
    instance.setModelBound(new BoundingSphere())
        instance.updateModelBound()
    instance.setMaterial(mat)
    flying += new Projectile(instance, dir.mult(2.0f), 4.5f)
    instance.setLocalTranslation(pos)
    //instance.setLocalRotation(Quaternion.IDENTITY.from)
    instance


  }
  def materializeProjectile(p:ProjectileGO) {

  }
  
  def update(rootNode:Node, tpf:Float) {

    val (keep, toRemove) = flying.partition {
      p => {
        val distanceSinceUpdate = p.velocity.mult(tpf)
        p.distanceTraveled += distanceSinceUpdate.length()
        if(p.distanceTraveled < p.maxDistanceTraveled) {
          p.geometry.setLocalTranslation(p.geometry.getLocalTranslation.add(distanceSinceUpdate))
          true
        } else {
          false
        }        
      }              
    }
    toRemove.foreach {
      p => {
        rootNode.detachChild(p.geometry)
      }
    }
    flying = keep
  }
  
  def collidesWith(enemy:Spatial,rootNode:Node) {

    val hitOpt = flying.find {
      f => {
        
        var res = new CollisionResults()

        enemy.asInstanceOf[Geometry].collideWith(f.geometry.getWorldBound,res)


        res.size() > 0
      }
    }

    hitOpt.foreach {
      x => {
        
        println("COOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOLLLISION")
        rootNode.detachChild(enemy)
        flying.remove(flying.indexWhere(_ == x))
        rootNode.detachChild(x.geometry)

      }
    }
  }

  def init() {
    projectileGeometry = new Box(Vector3f.ZERO, 0.1f, 0.1f, 0.1f)
    projectileGeometry.setBound(new BoundingSphere())
    projectileGeometry.updateBound()
    //val projectileShape = new BoxCollisionShape(0.1f,0.1f,0.1f)

  }
}
