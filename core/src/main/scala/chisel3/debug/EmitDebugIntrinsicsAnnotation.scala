// SPDX-License-Identifier: Apache-2.0

package chisel3.debug

import firrtl.seqToAnnoSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

/** Opt-in toggle for [[chisel3.stage.phases.AddDebugIntrinsics]].
  * Activate with `--with-debug-intrinsics` or by adding this annotation.
  */
case object EmitDebugIntrinsicsAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {

  override val options: Seq[ShellOption[Unit]] = Seq(
    new ShellOption[Unit](
      longOption = "with-debug-intrinsics",
      toAnnotationSeq = _ => Seq(EmitDebugIntrinsicsAnnotation),
      helpText = "Emit circt_debug_* intrinsics carrying Chisel type metadata",
      helpValueName = None
    )
  )
}
