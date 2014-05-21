package scala.pickling.test.statictypetag

import org.scalatest.FunSuite

import scala.pickling._
import json._

class StaticTypeTagTest extends FunSuite {
  test("equals") {
    val res1 = (StaticTypeTag.Int == StaticTypeTag("scala.Int"))
    val res2 = (StaticTypeTag.Int == StaticTypeTag("scala.String"))
    val res3 = (StaticTypeTag("scala.Int") == StaticTypeTag.Int)
    assert(res1 === true)
    assert(res2 === false)
    assert(res3 === true)
  }
}
