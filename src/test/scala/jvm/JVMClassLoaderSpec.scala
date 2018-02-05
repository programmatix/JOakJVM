package jvm

import jvmclass.JVMByteCode.JVMVarString
import org.scalatest.FunSuite

class JVMClassLoaderSpec extends FunSuite {
  test("CreateOwnClassInOtherPackage") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("package1/CreateOwnClassInOtherPackage.java", (sf) => {
      assert(CompilingTestUtils.getKlassInstanceLocal(sf).getField("str") == JVMVarString("hello"))
    }).jvm
  }

  test("CallJar") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFileX("CallJar.java", "CallJar", "main", (sf) => {
    }, classPath = Seq("JOakJVM/src/test/resources/jopt-simple-4.5.jar")).jvm
  }

}
