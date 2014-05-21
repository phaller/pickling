package scala.pickling

import scala.language.experimental.macros

trait StaticTypeTag[T] extends Equals {

  def key: String

  def isPrimitive: Boolean

  override def canEqual(that: Any) =
    that.isInstanceOf[StaticTypeTag[_]]

  override def equals(that: Any) =
    that.isInstanceOf[StaticTypeTag[_]] && this.key == that.asInstanceOf[StaticTypeTag[_]].key

  override def hashCode =
    key.hashCode

  override def toString =
    "StaticTypeTag[" + key + "]"
}

object StaticTypeTag {

  implicit object somePickler extends SPickler[StaticTypeTag[_]] with Unpickler[StaticTypeTag[_]] {
    val format = null // not used
    def pickle(picklee: StaticTypeTag[_], builder: PBuilder): Unit = {
      builder.beginEntry(picklee)

      builder.putField("value", b => {
        b.hintTag(implicitly[StaticTypeTag[String]])
        b.hintStaticallyElidedType()
        SPickler.stringPicklerUnpickler.pickle(picklee.key, b)
      })

      builder.endEntry()
    }
    def unpickle(tag: => StaticTypeTag[_], reader: PReader): Any = {
      val reader1 = reader.readField("value")
      reader1.hintTag(implicitly[StaticTypeTag[String]])
      reader1.hintStaticallyElidedType()

      val tag = reader1.beginEntry()
      val result = SPickler.stringPicklerUnpickler.unpickle(tag, reader1)
      reader1.endEntry()

      StaticTypeTag.apply(result.asInstanceOf[String])
    }
  }

  implicit def pickler[T] = new SPickler[StaticTypeTag[T]] with Unpickler[StaticTypeTag[T]] {
    val format = null // not used
    def pickle(picklee: StaticTypeTag[T], builder: PBuilder): Unit =
      somePickler.pickle(picklee, builder)
    def unpickle(tag: => StaticTypeTag[_], reader: PReader): Any =
      somePickler.unpickle(tag, reader)
  }

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

  val ScalaString = primitiveTag[String]("java.lang.String")
  implicit val JavaString = primitiveTag[java.lang.String]("java.lang.String")

  implicit val ArrayByte = primitiveTag[Array[Byte]]("scala.Array[scala.Byte]")
  implicit val ArrayShort = primitiveTag[Array[Short]]("scala.Array[scala.Short]")
  implicit val ArrayChar = primitiveTag[Array[Char]]("scala.Array[scala.Char]")
  implicit val ArrayInt = primitiveTag[Array[Int]]("scala.Array[scala.Int]")
  implicit val ArrayLong = primitiveTag[Array[Long]]("scala.Array[scala.Long]")
  implicit val ArrayBoolean = primitiveTag[Array[Boolean]]("scala.Array[scala.Boolean]")
  implicit val ArrayFloat = primitiveTag[Array[Float]]("scala.Array[scala.Float]")
  implicit val ArrayDouble = primitiveTag[Array[Double]]("scala.Array[scala.Double]")

  implicit val Ref = primitiveTag[refs.Ref]("scala.pickling.refs.Ref")

  def apply(name: String): StaticTypeTag[_] = {
    def testPrimitive() = name match {
      case "scala.Byte"                 => true
      case "scala.Short"                => true
      case "scala.Char"                 => true
      case "scala.Int"                  => true
      case "scala.Long"                 => true
      case "scala.Boolean"              => true
      case "scala.Float"                => true
      case "scala.Double"               => true
      case "scala.String"               => true
      case "java.lang.String"           => true
      case "scala.Null"                 => true
      case "scala.Array[scala.Byte]"    => true
      case "scala.Array[scala.Short]"   => true
      case "scala.Array[scala.Char]"    => true
      case "scala.Array[scala.Int]"     => true
      case "scala.Array[scala.Long]"    => true
      case "scala.Array[scala.Boolean]" => true
      case "scala.Array[scala.Float]"   => true
      case "scala.Array[scala.Double]"  => true
      case _                            => false
    }
    new StaticTypeTag[Nothing] {
      val key = if (name == "scala.String") "java.lang.String" else name
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
