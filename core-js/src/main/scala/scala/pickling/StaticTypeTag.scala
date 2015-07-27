package scala.pickling

import scala.language.experimental.macros

import scala.reflect.{ClassTag, classTag}

trait StaticTypeTag[T] extends Equals {

  /** A stringified key that can be used to denote this type.   This key should be unique for types within scala,
    * although the key will *not* determine uniqueness between types loaded on different classloaders.
    *
    * @return  A stringy type key.
    */
  def key: String

  /**
   * Tests whether this tag is effectively a primitive type.  Note: We duplicate logic
   * out of regular runtime reflection here to avoid the burden of requiring runtime reflection.
   */
  def isEffectivelyPrimitive: Boolean =
    StaticTypeTag.EffectivePrimitiveTags.contains(key)

  override def canEqual(x: Any) = x.isInstanceOf[StaticTypeTag[_]]

  // equals skips runtime reflection because it's potentially
  // expensive and unthreadsafe to force the lazy Type field, and
  // since we typeFromString(key) to get the Type anyhow there's
  // no downside to just using the string (the string has to
  // contain all the information).
  override def equals(x: Any) = canEqual(x) && {
    x match {
      case null => false
      case other: StaticTypeTag[_] => this.key == other.key
      case _ => false
    }
  }

  override def hashCode = key.hashCode

  override def toString = "FastTypeTag[" + key + "]"

}

object StaticTypeTag {

  implicit def materialize[T]: StaticTypeTag[T] = macro Compat.StaticTypeTagMacros_impl[T]

  private def stdTag[T: ClassTag]: StaticTypeTag[T] = new StaticTypeTag[T] {
    val key = classTag[T].runtimeClass.getName()
  }

  implicit val Null    = stdTag[Null]
  implicit val Byte    = stdTag[Byte]
  implicit val Short   = stdTag[Short]
  implicit val Char    = stdTag[Char]
  implicit val Int     = stdTag[Int]
  implicit val Long    = stdTag[Long]
  implicit val Boolean = stdTag[Boolean]
  implicit val Float   = stdTag[Float]
  implicit val Double  = stdTag[Double]
  implicit val Unit    = stdTag[Unit]

  implicit val String = stdTag[java.lang.String]

  implicit val ArrayString = stdTag[Array[String]]
  implicit val ArrayByte = stdTag[Array[Byte]]
  implicit val ArrayShort = stdTag[Array[Short]]
  implicit val ArrayChar = stdTag[Array[Char]]
  implicit val ArrayInt = stdTag[Array[Int]]
  implicit val ArrayLong = stdTag[Array[Long]]
  implicit val ArrayBoolean = stdTag[Array[Boolean]]
  implicit val ArrayFloat = stdTag[Array[Float]]
  implicit val ArrayDouble = stdTag[Array[Double]]
  implicit val ArrayUnit = stdTag[Array[Unit]]

  implicit val ArrayAnyRef: StaticTypeTag[Array[AnyRef]] = new StaticTypeTag[Array[AnyRef]] {
    val key = "scala.Array[scala.AnyRef]"
  }

  implicit val Nothing: StaticTypeTag[Nothing] = stdTag[Nothing]

  implicit val Ref = stdTag[refs.Ref]

  // NOTE; This is a bit of a hack, copied from [[Symbols.isPrimitive]]
  private val EffectivePrimitiveTags: Set[String] = {
    val primitives = Seq(
      Double, Float, Long, Int, Char, Short, Byte, Unit, Boolean
    )
    // TODO - create array primitives out of the above seq
    val arrayPrimitives = Seq(
      ArrayDouble, ArrayFloat, ArrayLong, ArrayInt, ArrayChar, ArrayShort, ArrayByte, ArrayUnit, ArrayBoolean
    )
    (primitives ++ arrayPrimitives).map(_.key).toSet
  }

  def valueTypeName(tag: StaticTypeTag[_]): String = {
    val clazz: Class[_] = tag match {
      case StaticTypeTag.String => classOf[java.lang.String]
      case StaticTypeTag.Byte => classOf[java.lang.Byte]
      case StaticTypeTag.Short => classOf[java.lang.Short]
      case StaticTypeTag.Char => classOf[java.lang.Character]
      case StaticTypeTag.Int => classOf[java.lang.Integer]
      case StaticTypeTag.Long => classOf[java.lang.Long]
      case StaticTypeTag.Boolean => classOf[java.lang.Boolean]
      case StaticTypeTag.Float => classOf[java.lang.Float]
      case StaticTypeTag.Double => classOf[java.lang.Double]
      case _ => null
    }
    if (clazz == null) tag match {
      case StaticTypeTag.Null => "null"
      case StaticTypeTag.ArrayString => "[Ljava.lang.String;"
      case StaticTypeTag.ArrayInt => "[I"
      case StaticTypeTag.ArrayDouble => "[D"
      case StaticTypeTag.ArrayBoolean => "[Z"
      case StaticTypeTag.ArrayLong => "[J"
      case StaticTypeTag.ArrayByte => "[B"
      case StaticTypeTag.ArrayFloat => "[F"
      case StaticTypeTag.ArrayChar => "[C"
      case StaticTypeTag.ArrayShort => "[S"
      case _ => tag.key
    } else clazz.getName
  }

}

trait StaticTypeTagMacros extends Macro {

  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    if (T.typeSymbol.isParameter)
      c.abort(c.enclosingPosition, s"cannot generate StaticTypeTag for type parameter $T, StaticTypeTag can only be generated for concrete types")

    q"""
      new _root_.scala.pickling.StaticTypeTag[$T] {
        val key = ${T.key}
      }
    """
  }

}
