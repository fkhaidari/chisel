// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class HandleInner extends Bundle {
  val x = UInt(4.W)
  val y = UInt(4.W)
}

class HandleFlipTop extends RawModule {
  // Flipped bundle inside an Output-wrapped bundle.
  val outw = IO(Output(new Bundle {
    val a = UInt(8.W)
    val f = Flipped(new HandleInner)
  }))
  // Plain Input/Output mix, Flipped inside Flipped, Vec cases.
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
    val f = Flipped(new Bundle {
      val i = Input(UInt(2.W))
      val o = Output(UInt(2.W))
      val g = Flipped(new Bundle {
        val z = UInt(1.W)
        val zf = Flipped(UInt(1.W))
      })
      val w = UInt(3.W)
    })
    val v = Vec(2, Flipped(UInt(1.W)))
    val vb = Vec(
      2,
      new Bundle {
        val p = UInt(1.W)
        val q = Flipped(UInt(1.W))
      }
    )
  })
  val inp = IO(Input(new Bundle {
    val a = UInt(1.W)
    val f = Flipped(UInt(1.W))
  }))
  val fp = IO(Flipped(new Bundle {
    val a = UInt(1.W)
    val b = Flipped(UInt(1.W))
  }))
  val w = Wire(new Bundle {
    val a = UInt(1.W)
    val f = Flipped(UInt(1.W))
  })
  w := DontCare
}

object DebugHandlesSpec {
  // FIRRTL type AST, parsed from the text the emitter printed.
  sealed trait T
  case object G extends T
  case class B(fields: Seq[(String, Boolean, T)]) extends T
  case class V(t: T) extends T

  private class TypeParser(s: String) {
    var i = 0
    def ws(): Unit = while (i < s.length && s(i) == ' ') i += 1
    def word(): String = {
      ws(); val st = i; while (i < s.length && !" ,:{}[]".contains(s(i))) i += 1; s.substring(st, i)
    }
    def tpe(): T = {
      ws()
      var t: T =
        if (s(i) == '{') {
          i += 1
          val fs = mutable.ArrayBuffer.empty[(String, Boolean, T)]
          ws()
          while (s(i) != '}') {
            var n = word(); val flip = n == "flip"; if (flip) n = word()
            ws(); require(s(i) == ':'); i += 1
            fs += ((n, flip, tpe())); ws()
            if (s(i) == ',') i += 1
            ws()
          }
          i += 1; B(fs.toSeq)
        } else { word(); G }
      while (i < s.length && s(i) == '[') { while (s(i) != ']') i += 1; i += 1; t = V(t) }
      t
    }
  }

  private def flatten(t: T, path: String, out: mutable.Map[String, Boolean]): Unit = t match {
    case G     => ()
    case V(e)  => flatten(e, path + "[]", out)
    case B(fs) => fs.foreach { case (n, f, c) => out(s"$path.$n") = f; flatten(c, s"$path.$n", out) }
  }

  private val declRe = """^\s*(?:input|output|wire|reg|regreset) (\w+) : (.*?)(?: @\[.*\])?$""".r
  private val nodeRe = """^\s*node (\S+) = intrinsic\((circt_debug_\w+)(?:<(.*)>)? : UInt<0>(?:, ([^)]*))?\).*$""".r
  private val varRe = """^\s*intrinsic\(circt_debug_var<name = "(\w+)">, (\S+)\).*$""".r
  private val paramRe = """(\w+) = ("(?:[^"\\]|\\.)*"|-?\d+)""".r

  /** path -> flip, as printed in the port/wire types. */
  def typeFlips(fir: String): Map[String, Boolean] = {
    val out = mutable.LinkedHashMap.empty[String, Boolean]
    fir.linesIterator.foreach {
      case declRe(n, t) => flatten(new TypeParser(t).tpe(), n, out)
      case _            => ()
    }
    out.toMap
  }

  /** path -> flip, as carried by the debug handle tree (array indices as `[]`). */
  def debugFlips(fir: String): Map[String, Boolean] =
    DebugTestUtils
      .debugTree(fir)
      .collect { case (p, n) if p.contains('.') && !p.endsWith("]") => p.replaceAll("""\[\d+\]""", "[]") -> n.flip }
      .toMap

  def mismatches(fir: String): Seq[String] = {
    val t = typeFlips(fir)
    val d = debugFlips(fir)
    (t.keySet ++ d.keySet).toSeq.sorted.flatMap { k =>
      (t.get(k), d.get(k)) match {
        case (Some(a), Some(b)) if a == b => None
        case (a, b) => Some(s"$k: printed flip=${a.getOrElse("-")}, debug flip=${b.getOrElse("-")}")
      }
    }
  }
}

class DebugHandlesSpec extends AnyFunSpec with Matchers {
  import DebugHandlesSpec._
  import DebugTestUtils.emitRaw

  describe("handle tree") {
    it("emits handles as node = intrinsic(...) and uses them as operands") {
      val fir = emitRaw(new HandleFlipTop)
      (fir should include).regex(
        """node outw_a_h = intrinsic\(circt_debug_value<name = "a", typeName = "UInt<8>"> : UInt<0>, outw\.a\)"""
      )
      (fir should include).regex(
        """node outw_f_x_h = intrinsic\(circt_debug_value<name = "x", typeName = "UInt<4>"> : UInt<0>, outw\.f\.x\)"""
      )
      (fir should include).regex("""node outw_s = intrinsic\(circt_debug_struct : UInt<0>, outw_a_h, outw_f_h\)""")
      (fir should include).regex(
        """node outw_h = intrinsic\(circt_debug_value<typeName = "AnonymousBundle"> : UInt<0>, outw_s\)"""
      )
      (fir should include).regex("""intrinsic\(circt_debug_var<name = "outw">, outw_h\)""")
    }

    it("skips probe fields") {
      val nodes = DebugTestUtils.debugTree(emitRaw(new DebugE2eAlu(8))).toMap
      nodes.keySet should contain("io.result.ready")
      nodes.keySet.filter(_.contains("accProbe")) shouldBe empty
    }
  }

  describe("flip matches the printed FIRRTL type") {
    def check(gen: => RawModule): Unit = {
      val fir = emitRaw(gen)
      typeFlips(fir).values.count(identity) should be > 0
      withClue(fir)(mismatches(fir) shouldBe empty)
    }
    it("on Flipped, Input/Output and Vec mixes")(check(new HandleFlipTop))
    it("on the end-to-end fixtures") {
      // Probe fields are printed but have no debug node.
      val fir = emitRaw(new DebugE2eAlu(8))
      mismatches(fir) shouldBe Seq("io.accProbe: printed flip=false, debug flip=-")
      check(new DebugE2eGcd(16))
    }
  }
}
