package scala.pickling
package pickler

import scala.collection.generic.CanBuildFrom
import scala.collection.{ IndexedSeq, LinearSeq }

trait SeqPicklers {
  implicit def seqPickler[T: StaticTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: StaticTypeTag[Seq[T]], cbf: CanBuildFrom[Seq[T], T, Seq[T]]): Pickler[Seq[T]] with Unpickler[Seq[T]] =
    SeqSetPickler[T, Seq]
}

trait IndexedSeqPicklers {
  implicit def indexedSeqPickler[T: StaticTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: StaticTypeTag[IndexedSeq[T]], cbf: CanBuildFrom[IndexedSeq[T], T, IndexedSeq[T]]):
    Pickler[IndexedSeq[T]] with Unpickler[IndexedSeq[T]] =
    SeqSetPickler[T, IndexedSeq]
}

trait LinearSeqPicklers {
  implicit def linearSeqPickler[T: StaticTypeTag](implicit elemPickler: Pickler[T], elemUnpickler: Unpickler[T],
    collTag: StaticTypeTag[LinearSeq[T]], cbf: CanBuildFrom[LinearSeq[T], T, LinearSeq[T]]):
    Pickler[LinearSeq[T]] with Unpickler[LinearSeq[T]] =
    SeqSetPickler[T, LinearSeq]
}
