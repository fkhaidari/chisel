// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DebugDataTypesSpec extends AnyFunSpec with Matchers {

  import DebugTestCircuits.DataTypesCircuits._
  import DebugTestCircuits.{BindingChoice, PortBinding, RegBinding, WireBinding}
  import DebugTestUtils._

  // `path -> desc` of every debug node must be exactly `expected`.
  private def emitAndCheck(label: String, gen: => RawModule)(expected: Seq[(String, String)]): Unit =
    it(label)(tree(gen).toMap shouldBe expected.toMap)

  def wrap(b: BindingChoice, t: String): String = b match {
    case PortBinding => s"IO[$t]"
    case WireBinding => s"Wire[$t]"
    case RegBinding  => s"Reg[$t]"
  }

  def clockResetFor(b: BindingChoice): Seq[(String, String)] =
    if (b == RegBinding) Seq("clock" -> "IO[Clock]", "reset" -> "IO[Reset]")
    else Seq.empty

  def typeTests(b: BindingChoice): Unit = {
    def w(t:  String) = wrap(b, t)
    def st(t: String) = s"struct ${w(t)}"
    def ar(t: String) = s"array ${w(t)}"

    emitAndCheck(s"[$b] should annotate ground types", new TopCircuitGroundTypes(b))(
      Seq("uint" -> w("UInt<8>"), "sint" -> w("SInt<8>"), "bool" -> w("Bool"), "bits" -> w("UInt<8>"))
        ++ clockResetFor(b)
        ++ (if (b != RegBinding) Seq("analog" -> w("Analog<1>")) else Nil)
    )

    emitAndCheck(s"[$b] should annotate bundles", new TopCircuitBundles(b))(
      Seq(
        "a" -> st("AnonymousBundle"),
        "bnd" -> st("MyEmptyBundle"),
        "c" -> st("MyBundle"),
        "c.a" -> w("UInt<8>"),
        "c.b" -> w("SInt<8>"),
        "c.c" -> w("Bool")
      ) ++ clockResetFor(b)
    )

    emitAndCheck(s"[$b] should annotate nested bundles", new TopCircuitBundlesNested(b))(
      Seq(
        "a" -> st("MyNestedBundle"),
        "a.a" -> w("Bool"),
        "a.b" -> st("MyBundle"),
        "a.b.a" -> w("UInt<8>"),
        "a.b.b" -> w("SInt<8>"),
        "a.b.c" -> w("Bool"),
        "a.c" -> s"flip ${st("MyBundle")}",
        "a.c.a" -> w("UInt<8>"),
        "a.c.b" -> w("SInt<8>"),
        "a.c.c" -> w("Bool")
      ) ++ clockResetFor(b)
    )

    emitAndCheck(s"[$b] should annotate vecs", new TopCircuitVecs(b))(
      Seq("a" -> ar("SInt<23>[5]"), "bv" -> ar("SInt<23>[3][5]"), "c" -> ar("AnonymousBundle[5]"))
        ++ (0 until 5).map(i => s"a[$i]" -> w("SInt<23>"))
        ++ (0 until 5).map(i => s"bv[$i]" -> ar("SInt<23>[3]"))
        ++ (for {
          i <- 0 until 5
          j <- 0 until 3
        } yield s"bv[$i][$j]" -> w("SInt<23>"))
        ++ (0 until 5).flatMap(i => Seq(s"c[$i]" -> st("AnonymousBundle"), s"c[$i].x" -> w("UInt<8>")))
        ++ Seq("d" -> st("MixedVec"), "d.0" -> w("UInt<3>"), "d.1" -> w("SInt<10>"))
        ++ clockResetFor(b)
    )

    emitAndCheck(s"[$b] should annotate bundle with vec", new TopCircuitBundleWithVec(b))(
      Seq("a" -> st("AnonymousBundle"), "a.vec" -> ar("UInt<8>[5]"))
        ++ (0 until 5).map(i => s"a.vec[$i]" -> w("UInt<8>"))
        ++ clockResetFor(b)
    )
  }

  describe("Clock and Reset annotations") {
    emitAndCheck("should annotate explicit clock and reset types", new TopCircuitClockReset)(
      Seq(
        "clock" -> "IO[Clock]",
        "syncReset" -> "IO[Bool]",
        "reset" -> "IO[Reset]",
        "asyncReset" -> "IO[AsyncReset]"
      )
    )
    emitAndCheck("should annotate implicit clock and reset", new TopCircuitImplicitClockReset)(
      Seq("clock" -> "IO[Clock]", "reset" -> "IO[Bool]")
    )
  }

  describe("Port (IO) annotations") { typeTests(PortBinding) }
  describe("Wire annotations") { typeTests(WireBinding) }
  describe("Reg annotations") { typeTests(RegBinding) }

  describe("ChiselEnum annotations") {
    lazy val chirrtl = emitRaw(new TopCircuitEnumSimple)

    it("should emit circt_debug_enum on every enum leaf") {
      debugTree(chirrtl).map { case (p, n) => p -> n.desc }.toMap shouldBe Map(
        "e" -> "enum DebugTestEnum",
        "e2" -> "enum DebugTestEnum2",
        "bnd" -> "struct IO[AnonymousBundle]",
        "bnd.en" -> "enum DebugTestEnum",
        "bnd.en2" -> "enum DebugTestEnum2",
        "bnd.x" -> "IO[UInt<8>]",
        "v" -> "array IO[DebugTestEnum[3]]",
        "v[0]" -> "enum DebugTestEnum",
        "v[1]" -> "enum DebugTestEnum",
        "v[2]" -> "enum DebugTestEnum"
      )
    }

    it("should carry fqn and one variant_<Name> param per value") {
      val nodes = debugTree(chirrtl).toMap
      nodes("e").params shouldBe Map(
        "typeName" -> "DebugTestEnum",
        "fqn" -> "chisel3.debug.DebugTestEnum",
        "variant_EA" -> "0",
        "variant_EB" -> "1",
        "variant_EC" -> "2"
      )
      nodes("bnd.en2").params shouldBe Map(
        "typeName" -> "DebugTestEnum2",
        "fqn" -> "chisel3.debug.DebugTestEnum2",
        "variant_ED" -> "0",
        "variant_EE" -> "1",
        "variant_EF" -> "2",
        "name" -> "en2"
      )
    }

    it("should not emit the old enum protocol") {
      (chirrtl should not).include("circt_debug_enumdef")
      (chirrtl should not).include("enumTypeName")
      (chirrtl should not).include("enumFqn")
    }
  }

  describe("Deeply nested leaves") {
    // debugTree checks that each leaf operand is the reference its path names.
    emitAndCheck("reference the declaration through the full path", new TopCircuitDeeplyNested)(
      Seq(
        "io" -> "struct IO[MyDeeplyNestedBundle]",
        "io.a" -> "struct IO[AnonymousBundle]",
        "io.a.b" -> "struct IO[AnonymousBundle]",
        "io.a.b.c" -> "IO[UInt<8>]"
      )
    )
  }

  describe("Memory annotations") {
    import DebugTestCircuits.MemCircuits._

    val memVar = """intrinsic\(circt_debug_var<[^)]*name\s*=\s*"mem"[^)]*>\)"""

    it("should annotate Mem as an operand-less var") {
      val chirrtl = emit(new TopCircuitMem(UInt(8.W)))
      countOccurrences(chirrtl, memVar) should be(1)
    }

    it("should annotate SyncReadMem as an operand-less var") {
      val chirrtl = emit(new TopCircuitSyncMem(UInt(8.W)))
      countOccurrences(chirrtl, memVar) should be(1)
    }
  }

  describe("MixedVec opaque-ctor handling") {
    // Per-element emission already covers MixedVec's `Seq[Data]` ctor arg.
    it("does not attach a `params=` attribute to the MixedVec node") {
      val nodes = debugTree(emitRaw(new TopCircuitVecs(PortBinding))).toMap
      nodes("d").params.keySet should not contain "params"
    }
  }

  describe("Tmp values in when/else") {
    it("should annotate named vals inside when blocks, in the when block") {
      val chirrtl = emitRaw(new TopCircuitWhenElse)
      (tree(new TopCircuitWhenElse) should contain).allOf(
        "evenSel" -> "OpResult[UInt<8>]",
        "selIsOne" -> "OpResult[UInt<8>]",
        "oddSel" -> "OpResult[UInt<8>]"
      )
      // Deeper than the module body: emitted into the declaring block.
      (chirrtl should include).regex("""\n {6,}intrinsic\(circt_debug_var<name = "evenSel">""")
      (chirrtl should include).regex(
        """\n {6,}node evenSel_h = intrinsic\(circt_debug_value<typeName = "OpResult\[UInt<8>\]"> : UInt<0>, evenSel\)"""
      )
    }
  }
}
