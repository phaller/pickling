package scala.pickling
package runtime

import scala.collection.mutable
import scala.collection.concurrent.TrieMap

object GlobalRegistry {
  val picklerMap: mutable.Map[String, StaticTypeTag[_] => Pickler[_]] =
    mutable.Map[String, StaticTypeTag[_] => Pickler[_]]()

  val unpicklerMap: mutable.Map[String, Unpickler[_]] =
    mutable.Map[String, Unpickler[_]]()
}
