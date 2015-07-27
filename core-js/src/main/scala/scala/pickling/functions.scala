package scala.pickling

object functions {
  def unpickle[T](thePickle: Pickle)(implicit unpickler: Unpickler[T], format: PickleFormat): T = {
    val reader = format.createReader(thePickle.asInstanceOf[format.PickleType])
    val result = unpickler.unpickleEntry(reader).asInstanceOf[T]
    // TODO - some mechanism to disable this.
    internal.clearUnpicklees()
    result
  }

  def pickle[T](picklee: T)(implicit format: PickleFormat, pickler: Pickler[T]): format.PickleType = {
    val builder = format.createBuilder
    pickleInto(picklee, builder)
    // TODO - some mechanism to disable this.
    internal.clearPicklees()
    builder.result.asInstanceOf[format.PickleType]
  }

  def pickleInto[T](picklee: T, builder: PBuilder)(implicit pickler: Pickler[T]): Unit = {
    // TODO - BeginEntry/EndEntry needed?
    // TODO - this hinting should be in the pickler, not here.  We need to understand
    //        when we want to use this vs. something else, and avoid over-hinting everywhere.
    if (null == picklee) {
      val nullTag = new StaticTypeTag[Null] { val key = "scala.Null" }
      builder.hintTag(nullTag)
      (new _root_.scala.pickling.pickler.PrimitivePicklers {}).nullPickler.pickle(null, builder)
    } else {
      builder.hintTag(pickler.tag)
      pickler.pickle(picklee, builder)
    }
    // TODO - This usually doesn't clear Picklee's, but should we?
  }

  def pickleTo[T, F <: PickleFormat](picklee: T, output: F#OutputType)(implicit pickler: Pickler[T], format: F): Unit = {
    // Lesser HACK POWER TIME! - We should probably find a way of ensuring S <:< format.OutputType...
    val builder = format.createBuilder(output.asInstanceOf[format.OutputType])
    pickleInto(picklee, builder)
    // TODO - some mechanism to turn this off, also should we have the GRL for this?
    internal.clearPicklees()
  }
}
