// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DebugDataTypesSpec extends AnyFunSpec with Matchers {

  import DebugTestCircuits.DataTypesCircuits._
  import DebugTestCircuits.{BindingChoice, PortBinding, RegBinding, WireBinding}
  import DebugTestUtils._

  // By-name `chirrtl` so a shared `lazy val` only forces inside `it`.
  private def emitAndCheck(label: String, gen: => RawModule)(patterns: Seq[String]): Unit =
    it(label)(checkIntrinsics(patterns.map(_ -> 1), emit(gen)))

  private def checkAt(chirrtl: => String, label: String)(patterns: Seq[String]): Unit =
    it(label)(checkIntrinsics(patterns.map(_ -> 1), chirrtl))

  def typeTests(b: BindingChoice): Unit = {
    emitAndCheck(s"[$b] should name only Bool and Analog among ground types", new TopCircuitGroundTypes(b))(
      Seq(typePattern("bool", "", "Bool")) ++
        (if (b != RegBinding) Seq(typePattern("analog", "", "Analog<1>")) else Nil)
    )

    it(s"[$b] should not describe types FIRRTL already names") {
      val chirrtl = emit(new TopCircuitGroundTypes(b))
      Seq("uint", "sint", "bits", "clock", "reset").foreach { t =>
        withClue(t)(countOccurrences(chirrtl, s"""circt_debug_\\w+<target = "$t"""") should be(0))
      }
    }

    emitAndCheck(s"[$b] should annotate bundles", new TopCircuitBundles(b))(
      Seq(
        typePattern("a", "", "AnonymousBundle"),
        typePattern("bnd", "", "MyEmptyBundle"),
        typePattern("c", "", "MyBundle"),
        typePattern("c", ".c", "Bool")
      )
    )

    emitAndCheck(s"[$b] should annotate nested bundles", new TopCircuitBundlesNested(b))(
      Seq(
        typePattern("a", "", "MyNestedBundle"),
        typePattern("a", ".a", "Bool"),
        typePattern("a", ".b", "MyBundle"),
        typePattern("a", ".b.c", "Bool"),
        typePattern("a", ".c", "MyBundle"),
        typePattern("a", ".c.c", "Bool")
      )
    )

    it(s"[$b] should describe fields in declaration order") {
      val chirrtl = emit(new TopCircuitBundlesNested(b))
      val starts = Seq(".a", ".b", ".c").flatMap(p => anyTypePattern("a", p).r.findFirstMatchIn(chirrtl).map(_.start))
      (starts should have).length(3)
      starts shouldBe sorted
    }

    emitAndCheck(s"[$b] should describe a vec element once under [*]", new TopCircuitVecs(b))(
      Seq(
        typePattern("c", "[*]", "AnonymousBundle"),
        typePattern("d", "", "MixedVec")
      )
    )

    it(s"[$b] should not describe vecs of ground types") {
      val chirrtl = emit(new TopCircuitVecs(b))
      countOccurrences(chirrtl, """circt_debug_type<target = "a"""") should be(0)
      countOccurrences(chirrtl, """circt_debug_type<target = "bv"""") should be(0)
      countOccurrences(chirrtl, """circt_debug_type<target = "d", path""") should be(0)
    }

    emitAndCheck(s"[$b] should annotate bundle with vec", new TopCircuitBundleWithVec(b))(
      Seq(typePattern("a", "", "AnonymousBundle"))
    )
  }

  describe("Clock and Reset annotations") {
    emitAndCheck("should name a Bool reset", new TopCircuitClockReset)(
      Seq(typePattern("syncReset", "", "Bool"))
    )
    emitAndCheck("should name the implicit Bool reset", new TopCircuitImplicitClockReset)(
      Seq(typePattern("reset", "", "Bool"))
    )
  }

  describe("Port (IO) annotations") { typeTests(PortBinding) }
  describe("Wire annotations") { typeTests(WireBinding) }
  describe("Reg annotations") { typeTests(RegBinding) }

  describe("ChiselEnum annotations") {
    lazy val chirrtl = emit(new TopCircuitEnumSimple)

    checkAt(chirrtl, "should annotate enum ports")(
      Seq(enumPattern("e", "", "DebugTestEnum"), enumPattern("e2", "", "DebugTestEnum2"))
    )
    checkAt(chirrtl, "should annotate enum fields")(
      Seq(
        typePattern("bnd", "", "AnonymousBundle"),
        enumPattern("bnd", ".en", "DebugTestEnum"),
        enumPattern("bnd", ".en2", "DebugTestEnum2")
      )
    )
    checkAt(chirrtl, "should annotate an enum vec element under [*]")(
      Seq(enumPattern("v", "[*]", "DebugTestEnum"))
    )
    it("should not send a width") {
      (chirrtl should not).include("width =")
    }
    emitAndCheck("should name an enum nested in an object by its simple name", new TopCircuitEnumNested)(
      Seq(enumPattern("n", "", "NestedEnum"))
    )
    emitAndCheck("should name an enum defined in a method by its simple name", new TopCircuitEnumLocal)(
      Seq(enumPattern("l", "", "LocalEnum"))
    )
  }

  describe("Deeply nested bundles") {
    emitAndCheck("emits one path per level", new TopCircuitDeeplyNested)(
      Seq(
        typePattern("io", "", "MyDeeplyNestedBundle"),
        typePattern("io", ".a", "AnonymousBundle"),
        typePattern("io", ".a.b", "AnonymousBundle")
      )
    )
  }

  describe("Memory annotations") {
    import DebugTestCircuits.MemCircuits._

    def memKind(kind: String, target: String = "mem") = java.util.regex.Pattern.quote(
      s"""intrinsic(circt_debug_type<target = "$target", ext_chisel = "{\\"memory\\":\\"$kind\\"}">)"""
    )

    it("should record the kind of a Mem") {
      val chirrtl = emit(new TopCircuitMem(UInt(8.W)))
      countOccurrences(chirrtl, memKind("Mem")) should be(1)
      countOccurrences(chirrtl, anyTypePattern("mem", "[*]")) should be(0)
    }

    it("should record the kind of a SyncReadMem") {
      val chirrtl = emit(new TopCircuitSyncMem(UInt(8.W)))
      countOccurrences(chirrtl, memKind("SyncReadMem")) should be(1)
    }

    it("should record the kind of an SRAM as SRAM") {
      val chirrtl = emit(new TopCircuitSram(UInt(8.W)))
      countOccurrences(chirrtl, memKind("SRAM", "mem_sram")) should be(1)
    }

    it("should describe the element of a vec memory under [*][*]") {
      val chirrtl = emit(new TopCircuitVecMem)
      countOccurrences(chirrtl, typePattern("mem", "[*][*]", "MyBundle")) should be(1)
      countOccurrences(chirrtl, typePattern("mem", "[*][*].c", "Bool")) should be(1)
    }

    it("should describe the element of a bundle memory under [*]") {
      val chirrtl = emit(new TopCircuitMem(new MyBundle))
      countOccurrences(chirrtl, typePattern("mem", "[*]", "MyBundle")) should be(1)
      countOccurrences(chirrtl, typePattern("mem", "[*].c", "Bool")) should be(1)
    }
  }

  describe("MixedVec opaque-ctor handling") {
    it("does not attach ext_chisel to the MixedVec") {
      val chirrtl = emit(new TopCircuitVecs(DebugTestCircuits.PortBinding))
      val mixedVec = """intrinsic\(circt_debug_type<target = "d"[^>]*>""".r
      val matched = mixedVec.findFirstIn(chirrtl).getOrElse(fail("no circt_debug_type for MixedVec port `d`"))
      (matched should not).include("ext_chisel")
    }
  }

  describe("Nested vecs and MixedVec paths") {
    lazy val chirrtl = emit(new TopCircuitVecNesting)

    checkAt(chirrtl, "should describe a vec of vec element under [*][*]")(
      Seq(typePattern("vv", "[*][*]", "MyBundle"), typePattern("vv", "[*][*].c", "Bool"))
    )
    checkAt(chirrtl, "should use the _elements index as a .N MixedVec segment")(
      Seq(typePattern("mv", "", "MixedVec"), typePattern("mv", ".0", "MyBundle"), typePattern("mv", ".0.c", "Bool"))
    )
    it("should not describe unnamed roots") {
      countOccurrences(chirrtl, """circt_debug_\w+<target = "_""") should be(0)
      countOccurrences(chirrtl, typePattern("w", "", "MyBundle")) should be(1)
    }
  }

  describe("Named values in when/else") {
    it("should not describe UInt nodes") {
      val chirrtl = emit(new TopCircuitWhenElse)
      Seq("evenSel", "selIsOne", "oddSel").foreach { n =>
        countOccurrences(chirrtl, s"""circt_debug_\\w+<target = "$n"""") should be(0)
      }
    }
    emitAndCheck("should name a Bool node inside a when", new TopCircuitBoolNode)(
      Seq(typePattern("isZero", "", "Bool"))
    )
  }

  describe("Opaque types") {
    emitAndCheck("should give the opaque type the path of its element", new TopCircuitOpaque)(
      Seq(typePattern("o", "", "DebugOpaque"), typePattern("o", ".c", "Bool"))
    )
    it("should not describe the element itself") {
      countOccurrences(emit(new TopCircuitOpaque), typePattern("o", "", "MyBundle")) should be(0)
    }
  }

  describe("Opaque types wrapping other types") {
    emitAndCheck("should describe an enum inside an opaque type", new TopCircuitOpaqueWrapped)(
      Seq(enumPattern("e", "", "DebugTestEnum"), typePattern("n", "", "DebugOpaqueNested"), typePattern("n", ".c", "Bool"))
    )
    it("should not emit an empty path segment for nested opaque types") {
      val chirrtl = emit(new TopCircuitOpaqueWrapped)
      countOccurrences(chirrtl, """path = "[^"]*\.(\.|")""") should be(0)
      countOccurrences(chirrtl, typePattern("n", "", "DebugOpaque")) should be(0)
    }
  }

  describe("Probe fields") {
    it("should skip a probe field and keep its siblings") {
      val chirrtl = emit(new TopCircuitProbe)
      countOccurrences(chirrtl, typePattern("io", ".a", "Bool")) should be(1)
      countOccurrences(chirrtl, anyTypePattern("io", ".p")) should be(0)
    }
    it("should not describe a declaration made only of probes") {
      val chirrtl = emit(new TopCircuitOnlyProbes)
      countOccurrences(chirrtl, anyTypePattern("io", "")) should be(0)
    }
  }

  describe("Old intrinsics") {
    it("are no longer emitted") {
      val chirrtl = emit(new TopCircuitEnumSimple)
      Seq("circt_debug_var", "circt_debug_subfield", "circt_debug_enumdef").foreach { n =>
        (chirrtl should not).include(n)
      }
    }
  }
}
