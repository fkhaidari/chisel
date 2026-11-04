// SPDX-License-Identifier: Apache-2.0

package chisel3.debug

import chisel3._
import chisel3.experimental.{fromStringToStringParam, Analog}
import chisel3.util.MixedVec
import circt.stage.ChiselStage
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

object DebugTestEnum extends ChiselEnum { val EA, EB, EC = Value }
object DebugTestEnum2 extends ChiselEnum { val ED, EE, EF = Value }

object DebugTestUtils extends Matchers {

  private def q(s: String): String = java.util.regex.Pattern.quote(s)

  def countOccurrences(chirrtl: String, pattern: String): Int =
    pattern.r.findAllMatchIn(chirrtl).length

  private def attrPart(name: String, value: String): String = s"""[^)]*$name\\s*=\\s*"${q(value)}""""

  private def intrinsic(kind: String, attrs: String*): String =
    s"""intrinsic\\(circt_debug_$kind<${attrs.mkString}"""

  def moduleInfoPattern(typeName: String): String = intrinsic("moduleinfo", attrPart("typeName", typeName))

  // Collapse whitespace so regex helpers ignore line wrapping.
  def emit(gen: => RawModule): String =
    ChiselStage.emitCHIRRTL(gen, args = Array("--with-experimental-debug-intrinsics")).replaceAll("\\s+", " ")

  private val moduleInfoBodyRe = """intrinsic\(circt_debug_moduleinfo<(.*?)>\)""".r
  private val paramsBodyRe = """params\s*=\s*"((?:[^"\\]|\\.)*)"""".r

  def requireParams(chirrtl: String, typeName: String): String = {
    val tnPat = s"""typeName\\s*=\\s*"${q(typeName)}"""".r
    moduleInfoBodyRe
      .findAllMatchIn(chirrtl)
      .map(_.group(1))
      .find(body => tnPat.findFirstIn(body).isDefined)
      .flatMap(body => paramsBodyRe.findFirstMatchIn(body).map(_.group(1)))
      .getOrElse(fail(s"no circt_debug_moduleinfo for $typeName"))
  }

  def assertParam(params: String, name: String, typeName: String, value: Any): Unit = {
    val bq = "\\\""
    val valueJson = value match {
      case b: Boolean => b.toString
      case other => bq + other.toString + bq
    }
    params should include(s"${bq}name$bq:$bq$name$bq")
    params should include(s"${bq}typeName$bq:$bq$typeName$bq")
    params should include(s"${bq}value$bq:$valueJson")
  }

  /** One source-level node of a debug variable: a `circt_debug_value` (merged
    * with the struct/array it wraps) or a `circt_debug_enum`.
    */
  case class DebugNode(kind: String, params: Map[String, String], operand: Option[String]) {
    def flip: Boolean = params.get("flip").contains("1")

    /** `[flip ]<kind> <typeName>`, kind omitted for ground leaves. */
    def desc: String =
      (if (flip) "flip " else "") + (if (kind == "value") "" else s"$kind ") + params.getOrElse("typeName", "?")
  }

  private val handleRe =
    """^\s*node (\S+) = intrinsic\((circt_debug_\w+)(?:<(.*)>)? : UInt<0>(?:, ([^()]*))?\).*$""".r
  private val varRe = """^\s*intrinsic\(circt_debug_var<name = "([^"]+)">, ([^()]+)\).*$""".r
  private val paramRe = """(\w+) = ("(?:[^"\\]|\\.)*"|-?\d+)""".r

  private def unquote(v: String): String =
    if (v.startsWith("\"")) v.substring(1, v.length - 1).replace("\\\"", "\"").replace("\\\\", "\\") else v

  /** Every variable's handle tree, flattened pre-order as `path -> node`. A path
    * is a FIRRTL reference (`io.a`, `v[0]`). Fails on a malformed tree: a handle
    * used before its definition or more than once, a struct child without
    * `name`, `name`/`flip` on a root or array element, a value over a value, or a
    * leaf operand that is not the reference its path names.
    */
  def debugTree(fir: String): Seq[(String, DebugNode)] = {
    // `leaf` is the operand of a value/enum that is not a handle defined above.
    case class H(kind: String, params: Map[String, String], args: Seq[String], leaf: Option[String])
    val handles = mutable.Map.empty[String, H]
    val used = mutable.Set.empty[String]
    val roots = mutable.ArrayBuffer.empty[(String, String)]
    def use(a: String): Unit = withClue(s"handle $a used twice: ")(used.add(a) shouldBe true)
    fir.linesIterator.foreach {
      case handleRe(h, k, ps, as) =>
        val params = Option(ps).toSeq.flatMap(paramRe.findAllMatchIn(_)).map(m => m.group(1) -> unquote(m.group(2)))
        val args = Option(as).toSeq.flatMap(_.split(", ").toSeq)
        val leaf =
          args.headOption.filter(a => !handles.contains(a) && (k == "circt_debug_value" || k == "circt_debug_enum"))
        args.filterNot(leaf.contains).foreach { a =>
          withClue(s"$h: operand $a is not a handle defined above: ")(handles.contains(a) shouldBe true)
          use(a)
        }
        handles(h) = H(k, params.toMap, args, leaf)
      case varRe(n, h) =>
        withClue(s"var $n: operand $h is not a handle defined above: ")(handles.contains(h) shouldBe true)
        use(h)
        roots += (n -> h)
      case _ => ()
    }
    val out = mutable.ArrayBuffer.empty[(String, DebugNode)]
    def label(h: String): Map[String, String] = handles(h).params.filter { case (k, _) => k == "name" || k == "flip" }
    def walk(h: String, path: String): Unit = {
      val n = handles(h)
      n.leaf.foreach(op => withClue(s"leaf $path: ")(op.replace("`", "") shouldBe path))
      n.kind match {
        case "circt_debug_enum" => out += path -> DebugNode("enum", n.params, n.leaf)
        case "circt_debug_value" =>
          n.args.size shouldBe 1
          if (n.leaf.isDefined) out += path -> DebugNode("value", n.params, n.leaf)
          else
            handles(n.args.head) match {
              case H("circt_debug_struct", ps, kids, _) =>
                ps shouldBe empty
                out += path -> DebugNode("struct", n.params, None)
                kids.foreach { c =>
                  val name = withClue(s"struct child of $path: ")(label(c).get("name").getOrElse(fail("no name")))
                  walk(c, s"$path.$name")
                }
              case H("circt_debug_array", ps, kids, _) =>
                ps shouldBe empty
                out += path -> DebugNode("array", n.params, None)
                kids.zipWithIndex.foreach { case (c, i) =>
                  withClue(s"array element $path[$i]: ")(label(c) shouldBe empty)
                  walk(c, s"$path[$i]")
                }
              case other => fail(s"$path: circt_debug_value over ${other.kind}")
            }
        case other => fail(s"$path: unexpected $other")
      }
    }
    roots.foreach { case (n, h) =>
      withClue(s"root $n: ")(label(h) shouldBe empty)
      walk(h, n)
    }
    withClue("unused handles: ")(handles.keySet.diff(used) shouldBe empty)
    withClue("duplicate paths: ")(out.map(_._1).distinct.size shouldBe out.size)
    out.toSeq
  }

  /** `path -> desc` of every debug node, see [[DebugNode.desc]]. */
  def tree(gen: => RawModule): Seq[(String, String)] =
    debugTree(emitRaw(gen)).map { case (p, n) => p -> n.desc }

  def emitRaw(gen: => RawModule): String =
    ChiselStage.emitCHIRRTL(gen, args = Array("--with-experimental-debug-intrinsics"))
}

object DebugTestCircuits {

  sealed abstract class BindingChoice(label: String) {
    def apply[T <: Data](data: T): T
    override def toString = label
  }
  case object PortBinding extends BindingChoice("IO") { def apply[T <: Data](d: T): T = IO(d) }
  case object WireBinding extends BindingChoice("Wire") { def apply[T <: Data](d: T): T = Wire(d) }
  case object RegBinding extends BindingChoice("Reg") { def apply[T <: Data](d: T): T = Reg(d) }

  abstract class DebugTestModule(bindingChoice: BindingChoice) extends RawModule {
    def body: Unit
    bindingChoice match {
      case RegBinding =>
        val clock = IO(Input(Clock()))
        val reset = IO(Input(Reset()))
        withClockAndReset(clock, reset) { body }
      case _ => body
    }
  }

  object ModuleCircuits {

    class TopCircuitBlackBox extends RawModule {
      class MyBlackBox extends ExtModule(Map("PARAM1" -> "TRUE", "PARAM2" -> "DEFAULT")) {
        val io = IO(new Bundle {})
      }
      val myBlackBox1:  MyBlackBox = Module(new MyBlackBox)
      val myBlackBox2:  MyBlackBox = Module(new MyBlackBox)
      val myBlackBoxes: Seq[MyBlackBox] = Seq.fill(2)(Module(new MyBlackBox))
    }
  }

  object DataTypesCircuits {

    class TopCircuitClockReset extends RawModule {
      val clock:      Clock = IO(Input(Clock()))
      val syncReset:  Bool = IO(Input(Bool()))
      val reset:      Reset = IO(Input(Reset()))
      val asyncReset: AsyncReset = IO(Input(AsyncReset()))
    }

    class TopCircuitImplicitClockReset extends Module

    class TopCircuitGroundTypes(b: BindingChoice) extends DebugTestModule(b) {
      override def body: Unit = {
        val uint: UInt = b(UInt(8.W))
        val sint: SInt = b(SInt(8.W))
        val bool: Bool = b(Bool())
        if (b != RegBinding) {
          val analog: Analog = b(Analog(1.W))
        }
        val bits: UInt = b(Bits(8.W))
      }
    }

    class MyEmptyBundle extends Bundle

    class MyBundle extends Bundle {
      val a: UInt = UInt(8.W)
      val b: SInt = SInt(8.W)
      val c: Bool = Bool()
    }

    class TopCircuitBundles(b: BindingChoice) extends DebugTestModule(b) {
      override def body: Unit = {
        val a:   Bundle = b(new Bundle {})
        val bnd: MyEmptyBundle = b(new MyEmptyBundle)
        val c:   MyBundle = b(new MyBundle)
      }
    }

    class MyNestedBundle extends Bundle {
      val a: Bool = Bool()
      val b: MyBundle = new MyBundle
      val c: MyBundle = Flipped(new MyBundle)
    }

    class TopCircuitBundlesNested(b: BindingChoice) extends DebugTestModule(b) {
      override def body: Unit = {
        val a: MyNestedBundle = b(new MyNestedBundle)
      }
    }

    class TopCircuitVecs(b: BindingChoice) extends DebugTestModule(b) {
      override def body: Unit = {
        val a:  Vec[SInt] = b(Vec(5, SInt(23.W)))
        val bv: Vec[Vec[SInt]] = b(Vec(5, Vec(3, SInt(23.W))))
        val c = b(Vec(5, new Bundle { val x: UInt = UInt(8.W) }))
        val d = b(MixedVec(UInt(3.W), SInt(10.W)))
      }
    }

    class TopCircuitBundleWithVec(b: BindingChoice) extends DebugTestModule(b) {
      override def body: Unit = {
        val a = b(new Bundle { val vec = Vec(5, UInt(8.W)) })
      }
    }

    class TopCircuitWhenElse extends RawModule {
      val inSeq = IO(Input(Vec(8, UInt(8.W))))
      val out = IO(Output(UInt(8.W)))
      val sel = IO(Input(UInt(math.sqrt(8).ceil.toInt.W)))
      val tmp = sel + 1.U
      when(sel % 2.U === 0.U) {
        val outTmp = inSeq(sel)
        val evenSel = outTmp + 1.U
        out := evenSel
      }.elsewhen(sel === 1.U) {
        val outTmp = inSeq(sel)
        val selIsOne = outTmp + 1.U
        out := selIsOne
      }.otherwise {
        val outTmp = inSeq(sel)
        val oddSel = outTmp + 1.U
        out := oddSel
      }
    }

    class MyDeeplyNestedBundle extends Bundle {
      val a = new Bundle {
        val b = new Bundle {
          val c: UInt = UInt(8.W)
        }
      }
    }

    class TopCircuitDeeplyNested extends RawModule {
      val io = IO(Input(new MyDeeplyNestedBundle))
    }

    class TopCircuitEnumSimple extends RawModule {
      val e = IO(Input(DebugTestEnum()))
      val e2 = IO(Input(DebugTestEnum2()))
      val bnd = IO(Input(new Bundle {
        val en = DebugTestEnum()
        val en2 = DebugTestEnum2()
        val x = UInt(8.W)
      }))
      val v = IO(Input(Vec(3, DebugTestEnum())))
    }

  }

  object MemCircuits {
    class TopCircuitMem[T <: Data](gen: T) extends Module { val mem = Mem(4, gen) }
    class TopCircuitSyncMem[T <: Data](gen: T) extends Module { val mem = SyncReadMem(4, gen) }
  }
}
