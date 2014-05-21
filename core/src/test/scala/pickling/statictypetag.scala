package scala.pickling.test.statictypetag

import org.scalatest.FunSuite

import scala.pickling._
import json._

case class Person(name: String, age: Int)

case class Record[T](id: String, x: T)

class StaticTypeTagTest extends FunSuite {
  test("equals") {
    val res1 = (StaticTypeTag.Int == StaticTypeTag("scala.Int"))
    val res2 = (StaticTypeTag.Int == StaticTypeTag("scala.String"))
    val res3 = (StaticTypeTag("scala.Int") == StaticTypeTag.Int)
    assert(res1 === true)
    assert(res2 === false)
    assert(res3 === true)
  }

  test("pickling base type tags") {
    val tag = StaticTypeTag.Int
    val p = tag.pickle
    val utag = p.unpickle[StaticTypeTag[_]]
    assert(utag.key === "scala.Int")

    val tag2 = StaticTypeTag.ArrayByte
    val p2 = tag2.pickle
    val utag2 = p2.unpickle[StaticTypeTag[_]]
    assert(utag2.key === "scala.Array[scala.Byte]")
  }

  test("pickling case class tags") {
    val tag3 = implicitly[StaticTypeTag[Person]]
    val p3 = tag3.pickle
    val utag3 = p3.unpickle[StaticTypeTag[_]]
    assert(utag3.key === "scala.pickling.test.statictypetag.Person")

    val tag4 = implicitly[StaticTypeTag[Record[Double]]]
    val p4 = tag4.pickle
    val utag4 = p4.unpickle[StaticTypeTag[_]]
    assert(utag4.key === "scala.pickling.test.statictypetag.Record[scala.Double]")
  }
}
