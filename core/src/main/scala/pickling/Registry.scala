package scala.pickling

object Registry {
  implicit val default = new RegistryNotFound
}

/**
  Implicit registries are optionally used when picklers are generated:

  After the macro has generated the class of a pickler, it also generates
  code to register the name of the generated class, so that the unpickler
  class can be instantiated when unpickling. See `PicklerMacros.impl`.
*/
trait Registry {
  def register(canonicalTypeName: String, className: String): Unit
}

final class RegistryNotFound extends Registry {
  def register(canonicalTypeName: String, className: String): Unit = ???
}
