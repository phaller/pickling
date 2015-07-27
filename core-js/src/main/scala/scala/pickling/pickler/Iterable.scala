package scala.pickling
package pickler

import scala.pickling.internal._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait IterablePicklers {
  implicit def iterablePickler[T: StaticTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: StaticTypeTag[Iterable[T]], cbf: CanBuildFrom[Iterable[T], T, Iterable[T]]):
    Pickler[Iterable[T]] with Unpickler[Iterable[T]] = TravPickler[T, Iterable[T]]
}

object TravPickler {
  def apply[T: StaticTypeTag, C <% Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              cbf: CanBuildFrom[C, T, C], collTag: StaticTypeTag[C]): Pickler[C] with Unpickler[C] =
    new AbstractPicklerUnpickler[C] {

    val elemTag  = implicitly[StaticTypeTag[T]]
    val isPrimitive = elemTag.isEffectivelyPrimitive

    def tag: StaticTypeTag[C] = collTag

    def pickle(coll: C, builder: PBuilder): Unit = {
      if (elemTag == StaticTypeTag.Int) builder.hintKnownSize(coll.size * 4 + 100)
      builder.beginEntry(coll)
      builder.beginCollection(coll.size)

      builder.pushHints()
      if (isPrimitive) {
        builder.hintStaticallyElidedType()
        builder.hintTag(elemTag)
        builder.pinHints()
      }

      (coll: Traversable[_]).asInstanceOf[Traversable[T]].foreach { (elem: T) =>
        builder putElement { b =>
          if (!isPrimitive) b.hintTag(elemTag)
          elemPickler.pickle(elem, b)
        }
      }

      builder.popHints()
      builder.endCollection()
      builder.endEntry()
    }

    def unpickle(tpe: String, preader: PReader): Any = {
      val reader = preader.beginCollection()

      preader.pushHints()
      if (isPrimitive) {
        reader.hintStaticallyElidedType()
        reader.hintTag(elemTag)
        reader.pinHints()
      }

      val length = reader.readLength()
      val builder = cbf.apply() // builder with element type T
      var i = 0
      while (i < length) {
        val r = reader.readElement()
        val elem = elemUnpickler.unpickleEntry(r)
        builder += elem.asInstanceOf[T]
        i = i + 1
      }

      preader.popHints()
      preader.endCollection()
      builder.result
    }
  }
}

object SeqSetPickler {
  def apply[T: StaticTypeTag, Coll[_] <: Traversable[_]]
    (implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
              cbf: CanBuildFrom[Coll[T], T, Coll[T]],
              collTag: StaticTypeTag[Coll[T]]): Pickler[Coll[T]] with Unpickler[Coll[T]] =
    TravPickler[T, Coll[T]]
}

object MapPickler {
  def apply[K: StaticTypeTag, V: StaticTypeTag, M[_, _] <: collection.Map[_, _]]
    (implicit elemPickler: Pickler[(K, V)], elemUnpickler: Unpickler[(K, V)],
              cbf: CanBuildFrom[M[K, V], (K, V), M[K, V]],
              pairTag: StaticTypeTag[(K, V)], collTag: StaticTypeTag[M[K, V]]): Pickler[M[K, V]] with Unpickler[M[K, V]] =
    TravPickler[(K, V), M[K, V]]
}
