package se.bupp.lek.common

import scalaz.NonEmptyList
import org.apache.log4j.Logger

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 6/13/12
 * Time: 3:34 AM
 * To change this template use File | Settings | File Templates.
 */

object FuncUtil {

  class SampleProbe(val name:String, val interval:Long, log:Logger) {
    var debugLastOutput = 0L

    def tick(output:() => String) {
      if (System.currentTimeMillis() - debugLastOutput > interval) {
        debugLastOutput = System.currentTimeMillis()
        log.debug(output + " " + name + " sample")
      }
    }

  }

  class RateProbe(val name:String, val interval:Long, log:Logger) {
    var debugMeasure = 0
    var debugLastOutput = 0L

    def tick() {
      debugMeasure = debugMeasure + 1
      qeury()
    }

    def qeury() {
      if (System.currentTimeMillis() - debugLastOutput > interval) {
        debugLastOutput = System.currentTimeMillis()
        log.debug(debugMeasure + " " + name + " rate")
        debugMeasure = 0
      }
    }

  }

  implicit def toDeluxeList[A](l:Seq[A]) = new DeluxeList(l)

  class DeluxeList[A](val l:Seq[A]) {

    def toListMap[T,U](implicit ev: <:<[A,(T,U)]) : Map[T,NonEmptyList[U]] = {
      val ll = l.map { e => ev.apply(e) }
      val grp = ll.groupBy(_._1)
      grp.mapValues { l =>
        val elems:Seq[U] = l.map(_._2)
        NonEmptyList.nel(elems.head, elems.tail.toList) }
    }
  }
}
