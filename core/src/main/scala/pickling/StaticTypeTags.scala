package scala.pickling

import language.experimental.macros

trait StaticTypeTag[T] extends Equals {

  def key: String

  def isPrimitive: Boolean

  override def canEqual(x: Any) =
    x.isInstanceOf[StaticTypeTag[_]]

  override def equals(x: Any) =
    x.isInstanceOf[StaticTypeTag[_]]

  override def hashCode =
    key.hashCode

  override def toString =
    "StaticTypeTag[" + key + "]"
}

object StaticTypeTag {
  implicit def materializeStaticTypeTag[T]: StaticTypeTag[T] =
    macro Compat.StaticTypeTagMacros_impl[T]

  private def primitiveTag[T](name: String): StaticTypeTag[T] =
    new StaticTypeTag[T] {
      val key = name
      val isPrimitive = true
    }

  implicit val Null    = primitiveTag[Null]("scala.Null")
  implicit val Nothing = primitiveTag[Nothing]("scala.Nothing")
  implicit val Unit    = primitiveTag[Unit]("scala.Unit")

  implicit val Byte    = primitiveTag[Byte]("scala.Byte")
  implicit val Short   = primitiveTag[Short]("scala.Short")
  implicit val Char    = primitiveTag[Char]("scala.Char")
  implicit val Int     = primitiveTag[Int]("scala.Int")
  implicit val Long    = primitiveTag[Long]("scala.Long")
  implicit val Boolean = primitiveTag[Boolean]("scala.Boolean")
  implicit val Float   = primitiveTag[Float]("scala.Float")
  implicit val Double  = primitiveTag[Double]("scala.Double")

  val ScalaString = primitiveTag[String]("scala.String")
  implicit val JavaString = primitiveTag[java.lang.String]("java.lang.String")

  implicit val ArrayByte = primitiveTag[Array[Byte]]("scala.Array[Byte]")
  implicit val ArrayShort = primitiveTag[Array[Short]]("scala.Array[Short]")
  implicit val ArrayChar = primitiveTag[Array[Char]]("scala.Array[Char]")
  implicit val ArrayInt = primitiveTag[Array[Int]]("scala.Array[Int]")
  implicit val ArrayLong = primitiveTag[Array[Long]]("scala.Array[Long]")
  implicit val ArrayBoolean = primitiveTag[Array[Boolean]]("scala.Array[Boolean]")
  implicit val ArrayFloat = primitiveTag[Array[Float]]("scala.Array[Float]")
  implicit val ArrayDouble = primitiveTag[Array[Double]]("scala.Array[Double]")

  implicit val Ref = primitiveTag[refs.Ref]("scala.pickling.refs.Ref")

  def apply(name: String): StaticTypeTag[_] = {
    // TODO: complete
    def testPrimitive() = name match {
      case "scala.Int"        => true
      case "scala.String"     => true
      case "java.lang.String" => true
      case "scala.Null"       => true
      case _                  => false
    }
    new StaticTypeTag[Nothing] {
      val key = name
      val isPrimitive = testPrimitive()
    }
  }

}

trait StaticTypeTagMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"""
      new StaticTypeTag[$tpe] {
        val key = ${tpe.key}
        val isPrimitive = false
      }
    """
  }

}
