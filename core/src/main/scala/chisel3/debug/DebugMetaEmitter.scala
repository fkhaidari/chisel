// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import logger.LazyLogging

import chisel3._
import chisel3.internal._
import chisel3.internal.binding._
import chisel3.internal.firrtl.ir._
import chisel3.debug.CtorParamExtractor.dataToTypeName
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.experimental.hierarchy.ModuleClone
import chisel3.reflect.DataMirror
import chisel3.util.simpleClassName

import scala.annotation.tailrec
import scala.collection.mutable
import upickle.{default => json}

private[chisel3] object DebugIntrinsics {

  def generate(circuit: Circuit): Unit = {
    val modules = circuit.components.collect { case m: DefModule => m.id: BaseModule }.toSet
    val emitter = new ComponentDebugEmitter(circuit.name, modules)
    circuit.components.foreach(emitter.generate)
  }

  // The class of a module is recorded where it is instantiated, since that is
  // what the class describes. Only the top module, which has no instantiation
  // site, records it on itself.
  private class ComponentDebugEmitter(topName: String, modules: Set[BaseModule]) extends LazyLogging {
    private val emittedIds = mutable.HashSet.empty[HasId]

    private val ctorExtractor = new CtorParamExtractor

    def generate(component: Component): Unit = component match {
      case ctx @ DefModule(id, name, _, _, ports, block) =>
        processModule(id, ports ++ ctx.secretPorts, block, name == topName)
      // Intrinsics in a class are dropped by CIRCT.
      case _: DefClass           => ()
      case _: DefBlackBox        => ()
      case _: DefIntrinsicModule => ()
      case ctx =>
        throw new InternalErrorException(s"generate: unknown Component type: $ctx")
    }

    private def processModule(id: BaseModule, allPorts: Seq[Port], block: Block, recordClass: Boolean): Unit = {
      emittedIds.clear()
      if (recordClass) createIntrinsic(id, None, id._getSourceLocator).foreach(block.addSecretCommand)
      allPorts.foreach { p => createIntrinsic(p.id, p.sourceInfo).foreach(block.addSecretCommand) }
      processBlock(block)
    }

    // `When` is special-cased so that we read `elseRegion` only via `hasElse`;
    // the getter would otherwise materialise an empty Block on first call.
    private def processBlock(block: Block): Unit = block.getCommands().foreach { c =>
      generate(c).foreach(block.addSecretCommand)
      c match {
        case w: When =>
          processBlock(w.ifRegion)
          if (w.hasElse) processBlock(w.elseRegion)
        case lb: LayerBlock  => processBlock(lb.region)
        case dc: DefContract => processBlock(dc.region)
        case _ => ()
      }
    }

    private def generate(cmd: Command): Seq[Command] = cmd match {
      case e @ DefInstance(si, id, _) =>
        val m = instantiated(id)
        if (modules(m)) createIntrinsic(m, Some(e.name), si) else Seq.empty
      case e: DefPrim[_] => createIntrinsic(e.id, e.sourceInfo)
      case DefWire(si, id)                           => createIntrinsic(id, si)
      case DefReg(si, id, _)                         => createIntrinsic(id, si)
      case DefRegInit(si, id, _, _, _)               => createIntrinsic(id, si)
      case DefMemory(si, id, t, _)                   => createIntrinsicMem(id, t, si)
      case DefSeqMemory(si, id, t, _, _)             => createIntrinsicMem(id, t, si)
      case FirrtlMemory(si, id, t, _, _, _, _, _, _) => createIntrinsicMem(id, t, si)
      case _                                         => Seq.empty
    }

    // A D/I `Instance` points at a clone of the module it instantiates.
    private def instantiated(id: BaseModule): BaseModule = id match {
      case c: ModuleClone[_] => c.getProto
      case _ => id
    }

    private def suppressDebugParams(target: Any): Boolean = target match {
      case _: chisel3.Bits | _: chisel3.Clock | _: chisel3.Reset => true
      case _: chisel3.experimental.Analog => true
      case _: chisel3.Vec[_] | _: chisel3.EnumType => true
      case _: chisel3.debug.SuppressDebugParams => true
      case _ => false
    }

    private def extractParams(target: Any): Seq[ClassParam] =
      if (suppressDebugParams(target)) Nil else ctorExtractor.getCtorParams(target)

    private def extChisel(params: Seq[ClassParam], extra: (String, ujson.Value)*): Seq[(String, Param)] = {
      val ext = Option.when(params.nonEmpty)("params" -> json.writeJs(params)).toSeq ++ extra
      if (ext.isEmpty) Nil else Seq("ext_chisel" -> StringParam(ujson.write(ujson.Obj.from(ext))))
    }

    // The memory kind goes on the memory itself, the element type on `[*]`.
    private def createIntrinsicMem(target: HasId, innerType: Data, si: SourceInfo): Seq[Command] = {
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
      val kind = "memory" -> ujson.Str(target match {
        case _: SramTarget => "SRAM"
        case _ => target.getClass.getSimpleName
      })
      typeIntrinsic(si, name, "", None, Nil, kind) +: dataIntrinsics(innerType, name, "[*]", si)
    }

    private def createIntrinsic(target: Data, si: SourceInfo): Seq[Command] = {
      if (!emittedIds.add(target)) return Seq.empty
      val name = signalRef(target)
      // Synthetic `_`-prefixed names don't survive to final FIRRTL.
      if (name.isEmpty || name.startsWith("_")) return Seq.empty
      dataIntrinsics(target, name, "", si)
    }

    // Only what FIRRTL cannot tell is sent: the class of a Record, `Bool`,
    // `Analog` and enums. A Vec element is described once, under `[*]`.
    private def dataIntrinsics(data: Data, target: String, path: String, si: SourceInfo): Seq[Command] = {
      if (onlyProbes(data)) return Nil
      data match {
        case _: chisel3.domain.Type => Nil
        case e: EnumType =>
          val variants = e.factory.allWithNames.map { case (v, n) => s"variant_$n" -> IntParam(v.litValue) }
          Seq(intrinsic(si, "circt_debug_enum", target, path, Some(enumName(e)), variants))
        case r: Record if r._isOpaqueType =>
          // An opaque type has no FIRRTL field of its own, so it shares the
          // path with its single element; the element itself is not described,
          // except an enum, which takes the path over from the wrapper.
          // Nested opaque types collapse into the outermost one.
          opaqueElement(r) match {
            case e: EnumType => dataIntrinsics(e, target, path, si)
            case d => recordIntrinsic(r, target, path, si) +: children(d, target, path, si)
          }
        case r: Record => recordIntrinsic(r, target, path, si) +: children(r, target, path, si)
        case v: Vec[_] => children(v, target, path, si)
        case _: Bool   => Seq(typeIntrinsic(si, target, path, Some("Bool"), Nil))
        case a: chisel3.experimental.Analog =>
          Seq(typeIntrinsic(si, target, path, Some(dataToTypeName(a)), Nil))
        case _ => Nil
      }
    }

    // CIRCT removes a declaration made only of probes or properties before
    // it reads the debug metadata, so it must not be named by an intrinsic.
    private def onlyProbes(data: Data): Boolean = data match {
      case _ if DataMirror.hasProbeTypeModifier(data) || DataMirror.isProperty(data) => true
      case r: Record => r._elements.nonEmpty && r._elements.values.forall(onlyProbes)
      case v: Vec[_] => v.length > 0 && onlyProbes(v.sample_element)
      case _         => false
    }

    @tailrec private def opaqueElement(r: Record): Data = r._elements.head._2 match {
      case o: Record if o._isOpaqueType => opaqueElement(o)
      case d => d
    }

    private def children(data: Data, target: String, path: String, si: SourceInfo): Seq[Command] = data match {
      case r: Record =>
        r._elements.toSeq.reverse.flatMap {
          case (field, d: Data) => dataIntrinsics(d, target, s"$path.$field", si)
          case _                => Nil
        }
      case v: Vec[_] if v.length > 0 => dataIntrinsics(v.sample_element, target, s"$path[*]", si)
      case _ => Nil
    }

    private def recordIntrinsic(r: Record, target: String, path: String, si: SourceInfo): Command =
      typeIntrinsic(si, target, path, Some(r.className), extractParams(r))

    private def typeIntrinsic(
      si:     SourceInfo,
      target: String,
      path:   String,
      name:   Option[String],
      params: Seq[ClassParam],
      extra:  (String, ujson.Value)*
    ) =
      intrinsic(si, "circt_debug_type", target, path, name, extChisel(params, extra: _*))

    private def intrinsic(
      si:     SourceInfo,
      kind:   String,
      target: String,
      path:   String,
      name:   Option[String],
      rest:   Seq[(String, Param)]
    ): Command =
      DefIntrinsic(
        si,
        kind,
        Nil,
        Seq("target" -> StringParam(target)) ++
          Option.when(path.nonEmpty)("path" -> StringParam(path)) ++
          name.map("name" -> StringParam(_)) ++ rest
      )

    private def enumName(e: EnumType): String =
      e.factory.enumTypeName.split("[.$]").filterNot(_.forall(_.isDigit)).last

    private def createIntrinsic(target: BaseModule, instance: Option[String], si: SourceInfo): Seq[Command] = Seq(
      DefIntrinsic(
        si,
        "circt_debug_moduleinfo",
        Nil,
        instance.map("instance" -> StringParam(_)).toSeq ++
          Seq("name" -> StringParam(simpleClassName(target.getClass))) ++
          extChisel(ctorExtractor.getCtorParams(target))
      )
    )
  }

  private def signalRef(d: Data): String =
    d.getOptionRef.map(_.localName).getOrElse("")
}
