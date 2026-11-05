// SPDX-License-Identifier: Apache-2.0
package chisel3.debug

import chisel3._
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.util.{is, switch}
import chisel3.util.experimental.InlineInstance
import circt.stage.ChiselStage

import java.nio.file.{Files, Paths}

// Designs for the CIRCT end-to-end debug-info tests. `DebugE2eFixtures.main`
// dumps each one with and without debug intrinsics.

object DebugE2eAluOp extends ChiselEnum { val ADD, SUB, AND, OR = Value }

class DebugE2eAluCfg extends Bundle {
  val enable = Bool()
  val clear = Bool()
}

class DebugE2eLane(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val valid = Bool()
}

class DebugE2eResult(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val valid = Bool()
  val ready = Flipped(Bool())
}

class DebugE2eAluIO(val w: Int) extends Bundle {
  val op = Input(DebugE2eAluOp())
  val a = Input(UInt(w.W))
  val b = Input(UInt(w.W))
  val cfg = Flipped(new DebugE2eAluCfg)
  val lanes = Output(Vec(2, new DebugE2eLane(w)))
  val result = new DebugE2eResult(w)
  val accProbe = Output(Probe(UInt(w.W)))
}

/** ChiselEnum opcode, Bundle IO with a Flipped sub-bundle, a Vec of Bundle and
  * a probe field, a RegInit accumulator, an enum reg, an enum node and a
  * `when`-scoped wire.
  */
class DebugE2eAlu(val width: Int) extends Module {
  override def desiredName = "Alu"
  val io = IO(new DebugE2eAluIO(width))
  val acc = RegInit(0.U(width.W))
  val lastOp = RegInit(DebugE2eAluOp.ADD)
  val result = Wire(UInt(width.W))
  result := 0.U
  switch(io.op) {
    is(DebugE2eAluOp.ADD) { result := io.a + io.b }
    is(DebugE2eAluOp.SUB) { result := io.a - io.b }
    is(DebugE2eAluOp.AND) { result := io.a & io.b }
    is(DebugE2eAluOp.OR) { result := io.a | io.b }
  }
  when(io.cfg.enable) {
    val next = Wire(UInt(width.W))
    next := Mux(io.cfg.clear, 0.U, acc + result)
    acc := next
    lastOp := io.op
  }
  val selOp = Mux(io.cfg.enable, io.op, lastOp)
  io.lanes(0).data := result
  io.lanes(0).valid := true.B
  io.lanes(1).data := acc
  io.lanes(1).valid := selOp === DebugE2eAluOp.ADD
  io.result.data := acc
  io.result.valid := io.result.ready && io.cfg.enable
  define(io.accProbe, ProbeValue(acc))
}

/** Instantiated three times: `tag` changes only the class params, not the hardware. */
class DebugE2eChild(val width: Int, val tag: String) extends Module {
  override def desiredName = "Child"
  val in = IO(Input(UInt(width.W)))
  val out = IO(Output(UInt(width.W)))
  val r = RegNext(in, 0.U(width.W))
  val inc = r + 1.U
  out := inc
}

class DebugE2eInlined extends Module with InlineInstance {
  override def desiredName = "Inlined"
  val a = IO(Input(UInt(8.W)))
  val y = IO(Output(UInt(8.W)))
  val mixed = a ^ "h5a".U
  y := mixed
}

/** Same child with different params (c0, c1) and with identical params (c0,
  * c2), an inlined child, and a node in a Verification layer block.
  */
class DebugE2eTop extends Module {
  override def desiredName = "Top"
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))
  val c0 = Module(new DebugE2eChild(8, "a"))
  val c1 = Module(new DebugE2eChild(8, "b"))
  val c2 = Module(new DebugE2eChild(8, "a"))
  val inl = Module(new DebugE2eInlined)
  c0.in := in
  c1.in := c0.out
  c2.in := c1.out
  inl.a := c2.out
  out := inl.y
  layer.block(layers.Verification) {
    val sum = c0.out +& c2.out
  }
}

class DebugE2eGcd(val bits: Int) extends Module {
  override def desiredName = "Gcd"
  val io = IO(new Bundle {
    val a = Input(UInt(bits.W))
    val b = Input(UInt(bits.W))
    val load = Input(Bool())
    val out = Output(UInt(bits.W))
    val valid = Output(Bool())
  })
  val x = Reg(UInt(bits.W))
  val y = Reg(UInt(bits.W))
  when(io.load) {
    x := io.a
    y := io.b
  }.elsewhen(x > y) {
    x := x - y
  }.otherwise {
    y := y - x
  }
  io.out := x
  io.valid := y === 0.U
}

object DebugE2eFixtures {
  val designs: Seq[(String, () => RawModule)] = Seq(
    "alu" -> (() => new DebugE2eAlu(8)),
    "dedup_layer" -> (() => new DebugE2eTop),
    "gcd" -> (() => new DebugE2eGcd(16))
  )

  /** Writes `<out>/<name>.fir` with debug intrinsics and `<out>/nodebug/<name>.fir` without. */
  def main(args: Array[String]): Unit = {
    val out = Paths.get(args(0))
    for {
      (dir, flags) <- Seq(
        out -> Array("--with-experimental-debug-intrinsics"),
        out.resolve("nodebug") -> Array[String]()
      )
      (name, gen) <- designs
    } {
      Files.createDirectories(dir)
      Files.write(dir.resolve(s"$name.fir"), ChiselStage.emitCHIRRTL(gen(), args = flags).getBytes)
      println(s"wrote ${dir.resolve(s"$name.fir")}")
    }
  }
}
