package se.bupp.lek.common

import scalaz.NonEmptyList

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 6/13/12
 * Time: 3:34 AM
 * To change this template use File | Settings | File Templates.
 */

object FuncUtil {

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
