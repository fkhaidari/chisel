// SPDX-License-Identifier: Apache-2.0

package circt

import firrtl.annotations.NoTargetAnnotation

/** Source class name of a module whose body carries no debug intrinsics
  * (a black box). Consumed by CIRCT.
  *
  * `target` is the full `~<Circuit>|<module>` string: firrtl's `ModuleTarget`
  * in this tree cannot carry the circuit name.
  *
  * @note This API is experimental and subject to change.
  */
case class DebugSourceNameAnnotation(target: String, name: String) extends NoTargetAnnotation
