package jvm

import jvmclass.internal.JVMClassFileReaderUtils
import org.scalatest.FunSuite

class JVMClassFileReaderUtilsSpec extends FunSuite {

  test("byte extends twos complement") {
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xff.toByte).toBinaryString == "1111 1111 1111 1111 1111 1111 1111 1111".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xff.toByte) == -1)

    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x80.toByte).toBinaryString == "1111 1111 1111 1111 1111 1111 1000 0000".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x80.toByte) == -128)

    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xfe.toByte).toBinaryString == "1111 1111 1111 1111 1111 1111 1111 1110".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xfe.toByte) == -2)

    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x0.toByte).toBinaryString == "0".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x1.toByte).toBinaryString == "1".replace(" ", ""))
  }

  test("int twos complement extended to long") {
    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(-10) == -10L)

    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(1).toBinaryString == "1")
    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(1) == 1L)
    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(0).toBinaryString == "0")
    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(0) == 0L)

    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(-1).toBinaryString == ("1111" * 16).replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendIntAsTwosComplement(-1) == -1L)

  }


}
