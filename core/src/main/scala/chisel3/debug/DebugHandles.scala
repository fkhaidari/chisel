// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import chisel3._
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.internal._
import chisel3.internal.firrtl.ir._
import chisel3.properties.Property
import chisel3.debug.CtorParamExtractor.dataToTypeName

import upickle.{default => json}

/** Prototype of the handle-tree debug intrinsics (UHDI SSA design).
  *
  * Runs after elaboration, like [[DebugIntrinsics]]: every node is a
  * `DefIntrinsicExpr` returning `UInt<0>` (a handle) appended with
  * `addSecretCommand`; parents take their children's handles as operands.
  *   leaf   = circt_debug_value<name?, flip?, typeName>(Node(leaf))
  *   record = circt_debug_value<name?, flip?, typeName>(circt_debug_struct(kids...))
  *   vec    = circt_debug_value<name?, flip?, typeName>(circt_debug_array(kids...))
  *   root   = circt_debug_var<name>(handle)
  * Only top-level ports and wires; no enums, memories or moduleinfo.
  */
private[chisel3] object DebugHandles {

  sealed trait FlipRule

  /** `flip` iff the field's own `SpecifiedDirection` is `Flip` or `Input`. */
  case object NaiveFlip extends FlipRule

  /** Same rule as `Serializer.serializeType`: an `Input`/`Output` ancestor clears every flip below it. */
  case object EmitterFlip extends FlipRule

  def generate(circuit: Circuit, rule: FlipRule = EmitterFlip): Unit =
    circuit.components.foreach {
      case m: DefModule => new Emitter(m.id, m.block, rule).run(m.ports ++ m.secretPorts)
      case _ => ()
    }

  private def isIO(d:   SpecifiedDirection) = d == SpecifiedDirection.Input || d == SpecifiedDirection.Output
  private def isFlip(d: SpecifiedDirection) = d == SpecifiedDirection.Flip || d == SpecifiedDirection.Input

  // Copy of the private `Serializer.firrtlUserDirOf`.
  private def userDir(t: Data): SpecifiedDirection = t match {
    case t: Vec[_] => SpecifiedDirection.fromParent(t.specifiedDirection, userDir(t.sample_element))
    case t: Record if t._isOpaqueType =>
      SpecifiedDirection.fromParent(t.specifiedDirection, userDir(t.elementsIterator.next()))
    case t => t.specifiedDirection
  }

  private class Emitter(module: BaseModule, block: Block, rule: FlipRule) {
    private val ctorExtractor = new CtorParamExtractor

    def run(ports: Seq[Port]): Unit = {
      ports.foreach(p => root(p.id, isIO(userDir(p.id)), p.sourceInfo))
      block.getCommands().foreach {
        case DefWire(si, id) => root(id, false, si)
        case _ => ()
      }
    }

    /** A fresh `UInt<0>` with a module-unique name, emitted as `node <name> = intrinsic(...)`. */
    private def handle(base: String, intrinsic: String, args: Seq[Arg], params: Seq[(String, Param)])(
      implicit si: SourceInfo
    ): Arg = {
      val h = UInt(0.W)
      h.setRef(Ref(module._namespace.name(base)))
      block.addSecretCommand(DefIntrinsicExpr(si, intrinsic, h, args, params))
      Node(h)
    }

    private def root(d: Data, clearDir: Boolean, si: SourceInfo): Unit = {
      val name = d.getOptionRef.map(_.localName).getOrElse("")
      if (name.isEmpty || name.startsWith("_")) return
      node(d, Nil, clearDir)(si).foreach { h =>
        block.addSecretCommand(DefIntrinsic(si, "circt_debug_var", Seq(h), Seq("name" -> StringParam(name))))
      }
    }

    private def params(d: Data): Seq[(String, Param)] = d match {
      case _: Element | _: Vec[_] | _: SuppressDebugParams => Nil
      case _ =>
        val ps = ctorExtractor.getCtorParams(d)
        if (ps.isEmpty) Nil else Seq("params" -> StringParam(json.write(ps)))
    }

    /** Post-order: children's handles are emitted before their parent's. */
    private def node(d: Data, label: Seq[(String, Param)], clearDir: Boolean)(implicit si: SourceInfo): Option[Arg] = {
      if (d.probeInfo.nonEmpty || d.isInstanceOf[Property[_]]) return None
      val base = d.getOptionRef.map(_.localName).getOrElse("dbg").replaceAll("[.\\[\\]]+", "_").stripSuffix("_")
      val inner: Arg = d match {
        case r: Record =>
          val childClear = clearDir || isIO(r.specifiedDirection)
          val kids = r._elements.toIndexedSeq.reverse.flatMap { case (n, e) =>
            val flip = rule match {
              case NaiveFlip   => isFlip(e.specifiedDirection)
              case EmitterFlip => !childClear && isFlip(userDir(e))
            }
            node(e, Seq("name" -> StringParam(n)) ++ (if (flip) Seq("flip" -> IntParam(1)) else Nil), childClear)
          }
          handle(s"${base}_s", "circt_debug_struct", kids, Nil)
        case v: Vec[_] =>
          val childClear = clearDir || isIO(v.specifiedDirection)
          handle(s"${base}_a", "circt_debug_array", v.flatMap(e => node(e, Nil, childClear)), Nil)
        case _ => Node(d)
      }
      Some(
        handle(
          s"${base}_h",
          "circt_debug_value",
          Seq(inner),
          label ++ Seq("typeName" -> StringParam(dataToTypeName(d))) ++ params(d)
        )
      )
    }
  }
}
