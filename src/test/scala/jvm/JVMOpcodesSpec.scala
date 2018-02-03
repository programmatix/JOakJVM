package jvm

import jvmclass.JVMOpCodes._
import jvmclass.JVMByteCode._
import org.scalatest.FunSuite

class JVMOpcodesSpec extends FunSuite {
  test("if_icmpge 0 >= 1") {
    val opcodes = Seq(
      makeInt(bipush, 0),
      makeInt(bipush, 1),
      makeInt(if_icmpge, 5),
      makeInt(bipush, 10),
      makeInt(bipush, 20)
    )
    val sf = CompilingTestUtils.executeOpcode(opcodes).sf
    assert (sf.stack.length == 2)
  }

  test("if_icmpge 1 >= 1") {
    val opcodes = Seq(
      makeInt(bipush, 1),
      makeInt(bipush, 1),
      makeInt(if_icmpge, 5),
      makeInt(bipush, 10),
      makeInt(bipush, 20)
    )
    val sf = CompilingTestUtils.executeOpcode(opcodes).sf
    assert (sf.stack.length == 1)
  }

  test("if_icmpge 2 >= 1") {
    val opcodes = Seq(
      makeInt(bipush, 2),
      makeInt(bipush, 1),
      makeInt(if_icmpge, 5),
      makeInt(bipush, 10),
      makeInt(bipush, 20)
    )
    val sf = CompilingTestUtils.executeOpcode(opcodes).sf
    assert (sf.stack.length == 1)
  }


  test("iinc 10") {
    val opcodes = Seq(
      makeInt(bipush, 10),
      makeInt(istore_3),
      makeInt(iinc, 3, 5)
    )
    val sf = CompilingTestUtils.executeOpcode(opcodes).sf
    assert (sf.stack.length == 0)
    assert (sf.locals.values.toArray sameElements Array(JVMVarInt(15)))
  }

  test("goto -2") {
    val opcodes = Seq(
      makeInt(bipush, 10), // 2
      makeInt(goto, 4),  // 3
      makeInt(return_), // 1
      makeInt(goto, -1) // 3
    )
    val sf = CompilingTestUtils.executeOpcode(opcodes).sf
    assert (sf.stack.length == 1)
  }

}
