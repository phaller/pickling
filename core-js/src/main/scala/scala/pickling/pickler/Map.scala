package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.collection.{ immutable, mutable }

trait MapPicklers {
  implicit def mapPickler[K: StaticTypeTag, V: StaticTypeTag](implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)], pairTag: StaticTypeTag[(K, V)], collTag: StaticTypeTag[Map[K, V]], cbf: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): Pickler[Map[K, V]] with Unpickler[Map[K, V]] =
    MapPickler[K, V, Map]
}

trait ImmutableSortedMapPicklers {
  implicit def immutableSortedMapPickler[K: StaticTypeTag, V: StaticTypeTag](implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)], pairTag: StaticTypeTag[(K, V)], collTag: StaticTypeTag[immutable.SortedMap[K, V]], cbf: CanBuildFrom[immutable.SortedMap[K, V], (K, V), immutable.SortedMap[K, V]]): Pickler[immutable.SortedMap[K, V]] with Unpickler[immutable.SortedMap[K, V]] =
    MapPickler[K, V, immutable.SortedMap]
}

trait MutableMapPicklers {
  implicit def mutableMapPickler[K: StaticTypeTag, V: StaticTypeTag](implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)], pairTag: StaticTypeTag[(K, V)], collTag: StaticTypeTag[mutable.Map[K, V]], cbf: CanBuildFrom[mutable.Map[K, V], (K, V), mutable.Map[K, V]]): Pickler[mutable.Map[K, V]] with Unpickler[mutable.Map[K, V]] =
    MapPickler[K, V, mutable.Map]
}
