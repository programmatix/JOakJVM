package jvm

import java.util

import jvm.JVMTypesInternal.{JVMVarNewInstanceToken, JVMVarObjectRefUnmanaged}
import jvmclass.JVMByteCode.{JVMVarBoolean, JVMVarInt, JVMVarLong, JVMVarString}
import org.scalatest.FunSuite

class JVMSpec extends FunSuite {

  test("SetLocale") {
    CompilingTestUtils.compileAndExecuteJavaFileX("SetLocale.java")
  }

  test("Sample10Plus33") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFileX("Sample10Plus33.java", "Sample10Plus33", "test", (sf) => {
      assert (sf.stack.toArray sameElements Array(JVMVarInt(43)))
    }).jvm
  }

  test("IntMaths") {
    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "add", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(43))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "multiply", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(30))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "divide", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(3))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "minus", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(7))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "neg", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(-10))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2b", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(10))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2bNeg", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(-10))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2bLimitMax", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(127))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2bLimitMin", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(-128))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2c", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(10))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2cLimitMax", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(65535))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2cNeg", (sf) => {
      // It's going to overflow
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(65526))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2l", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarLong(10))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2lNeg", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarLong(-10))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2lLimitMax", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarLong(2147483647))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "i2lLimitMin", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarLong(-2147483648))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "ishl", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(4))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "ishr", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(1))))
    })

    CompilingTestUtils.compileAndExecuteJavaFileX("IntMaths.java", "IntMaths", "ixor", (sf) => {
      assert(CompilingTestUtils.compareStack(sf, Array(JVMVarInt(3))))
    })
  }

  test("PrintHelloWorld") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PrintHelloWorld.java").jvm
  }

  test("CallFunc") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CallFunc.java").jvm
  }

  test("ForLoop100") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("ForLoop100.java", (sf) => {
      assert(CompilingTestUtils.containsVar(sf, JVMVarInt(100)))
    }).jvm
  }

  test("StaticFields") {
    var ret = 0
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("StaticFields.java", (sf) => {
      if (ret == 1) {
        assert(CompilingTestUtils.containsVar(sf, JVMVarInt(10)))
      }
      ret += 1
    }).jvm
  }

  test("SimpleArray") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("SimpleArray.java", (sf) => {
        assert(CompilingTestUtils.containsVar(sf, JVMVarInt(10)))
        assert(CompilingTestUtils.containsVar(sf, JVMVarInt(1)))
    }).jvm
  }

  test("StringOfByteArray") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("StringOfByteArray.java", (sf) => {
      assert (sf.locals.size == 1) // there's a dup
      val ni = sf.locals.head._2.asInstanceOf[JVMVarNewInstanceToken]
      assert (ni.created.get.asInstanceOf[String] == "HE")
    }).jvm
  }

  test("SimpleIf") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("SimpleIf.java", (sf) => {
      assert (sf.locals.values.last == JVMVarInt(40))
    }).jvm
  }

  test("ArrayAsList") {
    var ret = 0
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("ArrayAsList.java", (sf) => {
      if (ret == 1) {
        val array = sf.locals.head._2.asInstanceOf[JVMVarObjectRefUnmanaged].o.asInstanceOf[util.AbstractList[String]]
//        assert (array.length == 2)
        assert (array.get(0) == "p")
        assert (array.get(1) == "pid")
      }
      ret += 1
    }).jvm
  }

  test("ReturnValFromFunc") {
    var retIdx = 0
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("ReturnValFromFunc.java", (sf) => {
      if (retIdx == 1) {
        assert(CompilingTestUtils.containsVar(sf, JVMVarInt(8)))
      }
      retIdx += 1
    }).jvm

  }

  test("CreateString") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CreateString.java", (sf) => {
      assert (sf.locals.values.last.asInstanceOf[JVMVarNewInstanceToken].created.get.asInstanceOf[String] == "hello")
    }).jvm
  }

  test("CreateEmptyString") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CreateEmptyString.java", (sf) => {
      assert (sf.locals.values.last.asInstanceOf[JVMVarNewInstanceToken].created.get.asInstanceOf[String] == "")
    }).jvm
  }


  test("CallFuncWithArgs") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CallFuncWithArgs.java", (sf) => {
    }).jvm
  }

  test("CallUnmanagedFuncWithArgs") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CallUnmanagedFuncWithArgs.java", (sf) => {
    }).jvm
  }

  test("CreateOwnClass") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CreateOwnClass.java", (sf) => {
      assert(CompilingTestUtils.getKlassInstanceLocal(sf).getField("str") == JVMVarString("hello"))
    }).jvm
  }


  test("PassArrayListToCollection") {
    var ret = 0
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PassArrayListToCollection.java", (sf) => {
      if (ret == 0) {
        assert(sf.stack.head.asInstanceOf[JVMVarInt].v == 1)
      }
      ret += 1
    }).jvm
  }

  test("PassArrayListToCollectionUnmanaged") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PassArrayListToCollectionUnmanaged.java", (sf) => {
        assert(CompilingTestUtils.containsVar(sf, JVMVarBoolean(true)))
    }).jvm
  }

  test("PassMultipleArgsToManaged") {
    var ret = 0
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PassMultipleArgsToManaged.java", (sf) => {
      if (ret == 1) {
        assert(CompilingTestUtils.containsVar(sf, JVMVarString("hello!")))
      }
      ret += 1
    }).jvm
  }


}
