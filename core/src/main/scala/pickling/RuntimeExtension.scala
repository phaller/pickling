package scala.pickling

import scala.reflect.runtime.{universe => ru}
import ru.{Type, Mirror, appliedType}

import internal._

object RuntimeExtensions {

  def typeFromString(mirror: Mirror, stpe: String): Type = {
    // TODO: find out why typeFromString is called repeatedly for scala.Predef.String (at least in the evactor1 bench)
    if (typeFromStringCache.contains(stpe)) typeFromStringCache(stpe)
    else {
      val result =
        AppliedType.parse(stpe) match {
          case (AppliedType(typename, appliedTypeArgs), _) =>
            val sym =
              if (typename.endsWith(".type")) mirror.staticModule(typename.stripSuffix(".type")).moduleClass
              else mirror.staticClass(typename)
            val tycon = sym.asType.toTypeConstructor
            appliedType(tycon, appliedTypeArgs.map(starg => typeFromString(mirror, starg.toString)))
          case _ =>
            sys.error(s"fatal: cannot unpickle $stpe")
        }
      typeFromStringCache(stpe) = result
      result
    }
  }

}
