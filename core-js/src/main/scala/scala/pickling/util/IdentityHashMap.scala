package scala.pickling.util

import scala.collection.mutable.HashMap


private[pickling] class IdentityHashMap[A <: AnyRef, B] extends HashMap[A, B] {

  override def elemEquals(key1: A, key2: A): Boolean =
    key1 eq key2

  override def elemHashCode(key: A): Int =
    System.identityHashCode(key)

}
