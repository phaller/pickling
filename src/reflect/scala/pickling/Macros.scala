package scala.pickling

import scala.reflect.macros.AnnotationMacro
import scala.reflect.runtime.{universe => ru}
import ir._

// purpose of this macro: implementation of genPickler[T]. i.e. the macro that is selected
// via implicit search and which initiates the process of generating a pickler for a given type T
// NOTE: dispatch is done elsewhere. picklers generated by genPickler[T] only know how to process T
// but not its subclasses or the types convertible to it!
trait PicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = preferringAlternativeImplicits {
    import c.universe._
    import definitions._
    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass
    import irs._
    val pickler = {
      val builderTpe = pickleBuilderType(format)
      c.topLevelRef(syntheticPicklerQualifiedName(tpe, builderTpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        def unifiedPickle = { // NOTE: unified = the same code works for both primitives and objects
          if (tpe.typeSymbol.asClass.typeParams.nonEmpty)
            c.abort(c.enclosingPosition, s"TODO: cannot pickle polymorphic types yet ($tpe)")
          val cir = classIR(tpe)
          notImplementedYet(cir, !_.flags.isPublic, "non-public members")
          val beginEntry = q"builder.beginEntry(typeOf[$tpe], picklee)"
          val putFields = cir.fields.map(fir => q"builder.putField(${fir.name}, b => picklee.${TermName(fir.name)}.pickleInto(b))")
          val optimizedPutFields =
            if (putFields.isEmpty) q""
            else if (sym.isPrimitive || sym.isDerivedValueClass) q"{ ..$putFields; () }"
            else q"if (picklee != null) { ..$putFields; () }"
          val endEntry = q"builder.endEntry()"
          q"""
            import scala.reflect.runtime.universe._
            $beginEntry
            $optimizedPutFields
            $endEntry
          """
        }
        def pickleLogic = tpe match {
          case NothingTpe => c.abort(c.enclosingPosition, "cannot pickle Nothing") // TODO: report the serialization path that brought us here
          case _ => unifiedPickle
        }
        q"""
          class ${syntheticPicklerName(tpe, builderTpe)} extends scala.pickling.Pickler[$tpe] {
            import scala.pickling._
            import scala.pickling.`package`.PickleOps
            type PickleFormatType = ${format.tpe}
            implicit val format = new PickleFormatType()
            type PickleBuilderType = ${pickleBuilderType(format)}
            def pickle(pickleeRaw: Any, builder: PickleBuilderType): Unit = {
              val picklee = pickleeRaw.asInstanceOf[$tpe]
              $pickleLogic
            }
          }
        """
      })
    }
    q"new $pickler"
  }
}

// purpose of this macro: implementation of genUnpickler[T]. i.e., the macro that is selected via implicit
// search and which initiates the process of generating an unpickler for a given type T.
// NOTE: dispatch is done elsewhere. unpicklers generated by genUnpickler[T] only know how to process T
// but not its subclasses or the types convertible to it!
trait UnpicklerMacros extends Macro {
  def impl[T: c.WeakTypeTag](format: c.Tree): c.Tree = preferringAlternativeImplicits {
    import c.universe._
    import definitions._
    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass
    import irs._
    val unpickler = {
      val readerTpe = pickleReaderType(format)
      c.topLevelRef(syntheticUnpicklerQualifiedName(tpe, readerTpe)) orElse c.introduceTopLevel(syntheticPackageName, {
        if (tpe.typeSymbol.asClass.typeParams.nonEmpty)
          c.abort(c.enclosingPosition, s"TODO: cannot unpickle polymorphic types yet ($tpe)")
        def unpicklePrimitive = q"reader.readPrimitive(tpe)"
        def unpickleObject = {
          // TODO: validate that the tpe argument of unpickle and weakTypeOf[T] work together
          val cir = classIR(tpe)
          notImplementedYet(cir, !_.isPublic, "non-public members")
          notImplementedYet(cir, _.isNonParam, "mutable fields")
          val ctorSym = tpe.declaration(nme.CONSTRUCTOR).asMethod // TODO: multiple constructors
          def ctorArg(name: String, tpe: Type) = q"reader.readField($name).unpickle[$tpe]"
          val ctorArgs = ctorSym.paramss.flatten.map(f => ctorArg(f.name.toString, f.typeSignature)) // TODO: multiple argument lists
          q"new $tpe(..$ctorArgs)"
        }
        def unpickleLogic = tpe match {
          case NullTpe => q"null"
          case NothingTpe => c.abort(c.enclosingPosition, "cannot unpickle Nothing") // TODO: report the deserialization path that brought us here
          case _ => q"if (reader.atPrimitive) $unpicklePrimitive else $unpickleObject"
        }
        q"""
          class ${syntheticUnpicklerName(tpe, readerTpe)} extends scala.pickling.Unpickler[$tpe] {
            import scala.pickling._
            import scala.pickling.ir._
            import scala.reflect.runtime.universe._
            type PickleFormatType = ${format.tpe}
            implicit val format = new PickleFormatType()
            type PickleReaderType = ${pickleReaderType(format)}
            def unpickle(tpe: Type, reader: PickleReaderType): Any = $unpickleLogic
          }
        """
      })
    }
    q"new $unpickler"
  }
}

// purpose of this macro: implementation of PickleOps.pickle and pickleInto. i.e., this exists so as to:
// 1) perform dispatch based on the type of the argument
// 2) insert a call in the generated code to the genPickler macro (described above)
trait PickleMacros extends Macro {
  def pickle[T: c.WeakTypeTag](format: c.Tree): c.Tree = {
    import c.universe._
    val q"${_}($pickleeArg)" = c.prefix.tree
    q"""
      import scala.pickling._
      val picklee = $pickleeArg
      val builder = $format.createBuilder()
      picklee.pickleInto(builder)
      builder.result()
    """
  }
  def pickleInto[T: c.WeakTypeTag](builder: c.Tree): c.Tree = {
    import c.universe._
    import definitions._
    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass
    val q"${_}($pickleeArg)" = c.prefix.tree

    def createPickler(tpe: Type) = q"implicitly[Pickler[$tpe]]"
    def finalDispatch = createPickler(tpe)
    def nonFinalDispatch = {
      val nullDispatch = CaseDef(Literal(Constant(null)), EmptyTree, createPickler(NullTpe))
      val compileTimeDispatch = compileTimeDispatchees(tpe) map (tpe => {
        CaseDef(Bind(TermName("clazz"), Ident(nme.WILDCARD)), q"clazz == classOf[$tpe]", createPickler(tpe))
      })
      val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, q"Pickler.genPickler(getClass.getClassLoader, clazz)")
      // TODO: do we still want to use something HasPicklerDispatch?
      // NOTE: we dispatch on erasure, because that's the best we can have here anyways
      // so, if we have C[T], then we generate a pickler for C[_] and let the pickler do the rest
      // (e.g. to fetch the type tag for T as discussed yesterday and do the necessary dispatch)
      q"""
        val clazz = if (picklee != null) picklee.getClass else null
        ${Match(q"clazz", nullDispatch +: compileTimeDispatch :+ runtimeDispatch)}
      """
    }
    val dispatchLogic = if (sym.isFinal) finalDispatch else nonFinalDispatch

    q"""
      import scala.pickling._
      val picklee = $pickleeArg
      val picklerRaw = $dispatchLogic
      val pickler = picklerRaw.asInstanceOf[Pickler[_]{ type PickleBuilderType = ${builder.tpe} }]
      pickler.pickle(picklee, $builder)
    """
  }
}

// purpose of this macro: implementation of unpickle method on type Pickle, which does
// 1) dispatch to the correct unpickler based on the type of the input,
// 2) insert a call in the generated code to the genUnpickler macro (described above)
trait UnpickleMacros extends Macro {
  // TODO: implement this
  // override def onInfer(tic: c.TypeInferenceContext): Unit = {
  //   c.error(c.enclosingPosition, "must specify the type parameter for method unpickle")
  // }
  def pickleUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val pickleArg = c.prefix.tree
    q"""
      val pickle = $pickleArg
      val format = new ${pickleFormatType(pickleArg)}()
      val reader = format.createReader(pickle)
      reader.unpickle[$tpe]
    """
  }
  def readerUnpickle[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass
    val readerArg = c.prefix.tree

    def createUnpickler(tpe: Type) = q"implicitly[Unpickler[$tpe]]"
    def finalDispatch = createUnpickler(tpe)
    def nonFinalDispatch = {
      val compileTimeDispatch = compileTimeDispatchees(tpe) map (tpe => {
        // TODO: do we still want to use something HasPicklerDispatch (for unpicklers it would be routed throw tpe's companion)?
        // NOTE: we have a precise type at hand here, but we do dispatch on erasure
        // why? because picklers are created generic, i.e. for C[T] we have a single pickler of type Pickler[C[_]]
        // therefore here we dispatch on erasure and later on pass the precise type to `unpickle`
        CaseDef(Bind(TermName("tpe"), Ident(nme.WILDCARD)), q"tpe.typeSymbol == typeOf[$tpe].typeSymbol", createUnpickler(tpe))
      })
      val runtimeDispatch = CaseDef(Ident(nme.WILDCARD), EmptyTree, q"Unpickler.genUnpickler(currentMirror, tpe)")
      Match(q"tpe", compileTimeDispatch :+ runtimeDispatch)
    }
    val dispatchLogic = if (sym.isFinal) finalDispatch else nonFinalDispatch

    q"""
      import scala.reflect.runtime.universe._
      import scala.reflect.runtime.currentMirror
      val reader = $readerArg
      val tpe = reader.readType(currentMirror)
      val unpicklerRaw = $dispatchLogic
      val unpickler = unpicklerRaw.asInstanceOf[Unpickler[_]{ type PickleReaderType = ${readerArg.tpe} }]
      val result = unpickler.unpickle(tpe, reader)
      result.asInstanceOf[$tpe]
    """
  }
}

trait PickleableMacro extends AnnotationMacro {
  def impl = {
    import c.universe._
    import Flag._
    c.annottee match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
        // TODO: implement PickleableBase methods and append them to body
        ClassDef(mods, name, tparams, Template(parents :+ tq"scala.pickling.PickleableBase", self, body))
    }
  }
}
