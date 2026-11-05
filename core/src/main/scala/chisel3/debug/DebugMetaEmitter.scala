// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import logger.LazyLogging

import chisel3._
import chisel3.internal._
import chisel3.internal.binding._
import chisel3.internal.firrtl.ir._
import chisel3.debug.CtorParamExtractor.dataToTypeName
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.internal.firrtl.Serializer
import chisel3.properties.Property
import _root_.firrtl.annotations.Annotation

import scala.collection.mutable
import upickle.{default => json}

private[chisel3] object DebugIntrinsics {

  /** Adds the debug intrinsics to `circuit` and returns the annotations they need. */
  def generate(circuit: Circuit): Seq[Annotation] = {
    val emitter = new ComponentDebugEmitter
    circuit.components.foreach(emitter.generate)
    // A black box has no body to hold `circt_debug_moduleinfo`.
    circuit.components.collect { case bb: DefBlackBox =>
      circt.DebugSourceNameAnnotation(s"~${circuit.name}|${bb.name}", bb.id.desiredName)
    }
  }

  private def isIO(d:   SpecifiedDirection) = d == SpecifiedDirection.Input || d == SpecifiedDirection.Output
  private def isFlip(d: SpecifiedDirection) = d == SpecifiedDirection.Flip || d == SpecifiedDirection.Input

  /** Builds the handle tree of each variable. Every node is a `DefIntrinsicExpr`
    * returning a `UInt<0>` handle; a parent takes its children's handles:
    *   ground leaf = circt_debug_value<name?, flip?, typeName, params?>(Node(leaf))
    *   Record      = circt_debug_value<name?, flip?, typeName, params?>(circt_debug_struct(kids...))
    *   Vec         = circt_debug_value<name?, flip?, typeName, params?>(circt_debug_array(kids...))
    *   ChiselEnum  = circt_debug_enum<typeName, fqn, variant_<Name>=int..., name?, flip?>(Node(leaf))
    *   root        = circt_debug_var<name>(handle)
    * Children are emitted before their parent, into the block that declares the root.
    */
  private class ComponentDebugEmitter extends LazyLogging {
    private val emittedIds = mutable.HashSet.empty[HasId]
    private var module: BaseModule = _

    private val ctorExtractor = new CtorParamExtractor

    // Class bodies get no debug intrinsics: class ports are all properties.
    def generate(component: Component): Unit = component match {
      case ctx @ DefModule(id, _, _, _, ports, block) =>
        processModule(id, ports ++ ctx.secretPorts, block)
      case _: DefClass           => ()
      case _: DefBlackBox        => ()
      case _: DefIntrinsicModule => ()
      case ctx =>
        throw new InternalErrorException(s"generate: unknown Component type: $ctx")
    }

    private def processModule(id: BaseModule, allPorts: Seq[Port], block: Block): Unit = {
      emittedIds.clear()
      module = id
      createIntrinsic(id, id._getSourceLocator).foreach(block.addSecretCommand)
      // Same rule as `Serializer.serialize(port)`: an Input/Output port prints no flips.
      allPorts.foreach { p => createVar(p.id, isIO(Serializer.firrtlUserDirOf(p.id)), block)(p.sourceInfo) }
      processBlock(block)
    }

    // `When` is special-cased so that we read `elseRegion` only via `hasElse`;
    // the getter would otherwise materialise an empty Block on first call.
    private def processBlock(block: Block): Unit = block.getCommands().foreach { c =>
      generate(c, block)
      c match {
        case w: When =>
          processBlock(w.ifRegion)
          if (w.hasElse) processBlock(w.elseRegion)
        case lb: LayerBlock  => processBlock(lb.region)
        case dc: DefContract => processBlock(dc.region)
        case _ => ()
      }
    }

    private def generate(cmd: Command, block: Block): Unit = cmd match {
      case e: DefPrim[_] => createVar(e.id, false, block)(e.sourceInfo)
      case DefWire(si, id)                  => createVar(id, false, block)(si)
      case DefReg(si, id, _)                => createVar(id, false, block)(si)
      case DefRegInit(si, id, _, _, _)      => createVar(id, false, block)(si)
      case DefMemory(si, id, t, size)       => createIntrinsicMem(id, t, size, si).foreach(block.addSecretCommand)
      case DefSeqMemory(si, id, t, size, _) => createIntrinsicMem(id, t, size, si).foreach(block.addSecretCommand)
      case FirrtlMemory(si, id, t, size, _, _, _, _, _) =>
        createIntrinsicMem(id, t, size, si).foreach(block.addSecretCommand)
      case _ => ()
    }

    private def suppressDebugParams(target: Any): Boolean = target match {
      case _: chisel3.Bits | _: chisel3.Clock | _: chisel3.Reset => true
      case _: chisel3.experimental.Analog       => true
      case _: chisel3.Vec[_]                    => true
      case _: chisel3.debug.SuppressDebugParams => true
      case _ => false
    }

    private def extractParams(target: Any): Seq[ClassParam] =
      if (suppressDebugParams(target)) Nil else ctorExtractor.getCtorParams(target)

    private def paramsAttr(params: Seq[ClassParam]): Seq[(String, Param)] =
      if (params.isEmpty) Nil else Seq("params" -> StringParam(s"""{"x-chisel":{"params":${json.write(params)}}}"""))

    private def createIntrinsicMem(target: HasId, innerType: Data, size: BigInt, si: SourceInfo): Seq[Command] = {
      val typeName = s"${target.getClass.getSimpleName}[${dataToTypeName(innerType)}[$size]]"
      val name = target.getOptionRef
        .map(_.localName)
        .getOrElse(target match {
          case m: MemBase[_] => m.instanceName
          case _ => ""
        })
      if (name.isEmpty) {
        logger.warn(s"createIntrinsicMem: skipping memory with empty name: $target")
        return Seq.empty
      }
      Seq(
        DefIntrinsic(
          si,
          "circt_debug_var",
          Nil,
          Seq("typeName" -> StringParam(typeName), "name" -> StringParam(name)) ++ paramsAttr(extractParams(target))
        )
      )
    }

    private def createIntrinsic(target: BaseModule, si: SourceInfo): Seq[Command] = Seq(
      DefIntrinsic(
        si,
        "circt_debug_moduleinfo",
        Nil,
        Seq("typeName" -> StringParam(target.desiredName)) ++ paramsAttr(ctorExtractor.getCtorParams(target))
      )
    )

    private def createVar(target: Data, clearDir: Boolean, block: Block)(implicit si: SourceInfo): Unit = {
      if (!emittedIds.add(target)) return
      val name = signalRef(target)
      // Synthetic `_`-prefixed names don't survive to final FIRRTL.
      if (name.isEmpty || name.startsWith("_")) return
      createNode(target, Nil, clearDir, block).foreach { h =>
        block.addSecretCommand(DefIntrinsic(si, "circt_debug_var", Seq(h), Seq("name" -> StringParam(name))))
      }
    }

    /** A fresh `UInt<0>` with a module-unique name, emitted as `node <name> = intrinsic(...)`. */
    private def handle(base: String, intrinsic: String, args: Seq[Arg], params: Seq[(String, Param)], block: Block)(
      implicit si: SourceInfo
    ): Arg = {
      val h = UInt(0.W)
      h.setRef(Ref(module._namespace.name(base)))
      block.addSecretCommand(DefIntrinsicExpr(si, intrinsic, h, args, params))
      Node(h)
    }

    private def enumNames(e: EnumType): (String, String) = {
      val fqn = e.factory.enumTypeName.stripSuffix("$")
      (fqn, fqn.split("\\.").last)
    }

    /** Emits the subtree of `target` and returns its handle. `label` is the
      * `name`/`flip` of `target` in its parent struct. `flip` follows
      * `Serializer.serializeType`: an Input/Output ancestor (`clearDir`) clears
      * every flip below it. Probe, Property and domain data get no node.
      */
    private def createNode(target: Data, label: Seq[(String, Param)], clearDir: Boolean, block: Block)(
      implicit si: SourceInfo
    ): Option[Arg] = {
      if (target.probeInfo.nonEmpty || target.isInstanceOf[Property[_]] || target.isInstanceOf[domain.Type])
        return None
      val base = signalRef(target).replaceAll("[.\\[\\]]+", "_").stripSuffix("_")
      def child(d: Data, childClearDir: Boolean, name: Option[String]): Option[Arg] = {
        val flip = name.isDefined && !childClearDir && isFlip(Serializer.firrtlUserDirOf(d))
        val childLabel = name.map("name" -> StringParam(_)).toSeq ++ (if (flip) Seq("flip" -> IntParam(1)) else Nil)
        createNode(d, childLabel, childClearDir, block)
      }
      target match {
        case e: EnumType =>
          val (fqn, simple) = enumNames(e)
          val variants = e.factory.allWithNames.map { case (v, n) => s"variant_$n" -> IntParam(v.litValue) }
          Some(
            handle(
              s"${base}_h",
              "circt_debug_enum",
              Seq(Node(e)),
              Seq("typeName" -> StringParam(simple), "fqn" -> StringParam(fqn)) ++ variants ++ label,
              block
            )
          )
        case _ =>
          val inner: Arg = target match {
            case record: Record =>
              val childClearDir = clearDir || isIO(record.specifiedDirection)
              // Same order as the fields in the printed FIRRTL type.
              val kids = record._elements.toIndexedSeq.reverse.flatMap { case (n, e) =>
                child(e, childClearDir, Some(n))
              }
              handle(s"${base}_s", "circt_debug_struct", kids, Nil, block)
            case vec: Vec[_] =>
              val childClearDir = clearDir || isIO(vec.specifiedDirection)
              handle(s"${base}_a", "circt_debug_array", vec.flatMap(child(_, childClearDir, None)), Nil, block)
            case _ => Node(target)
          }
          val typeParams = Seq("typeName" -> StringParam(dataToTypeName(target))) ++ paramsAttr(extractParams(target))
          Some(handle(s"${base}_h", "circt_debug_value", Seq(inner), label ++ typeParams, block))
      }
    }
  }

  private def signalRef(d: Data): String =
    d.getOptionRef.map(_.localName).getOrElse("")
}
