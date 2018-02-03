package jvm

import jvm.JVMTypesInternal.JVMTypeObjectStr
import jvmclass.JVMTypes.{JVMTypeArray, JVMTypeDouble, JVMTypeInt, JVMTypeVoid}
import org.scalatest.FunSuite

class JVMMethodDescriptorsSpec extends FunSuite {
  test("create") {
    assert (JVMMethodDescriptors.createMethodDescriptor(
      JVMTypeVoid(), Seq(JVMTypeInt(), JVMTypeDouble())) ==  "(ID)V")
  }

  test("recover") {
    assert (JVMMethodDescriptors.methodDescriptorToTypes("()V") ==
      JVMMethodDescriptors.MethodDescriptor(JVMTypeVoid(), Seq()))
    assert (JVMMethodDescriptors.methodDescriptorToTypes("(I)V") ==
      JVMMethodDescriptors.MethodDescriptor(JVMTypeVoid(), Seq(JVMTypeInt())))
    assert (JVMMethodDescriptors.methodDescriptorToTypes("(ID)V") ==
      JVMMethodDescriptors.MethodDescriptor(JVMTypeVoid(), Seq(JVMTypeInt(),JVMTypeDouble())))
    assert (JVMMethodDescriptors.methodDescriptorToTypes("(Ljava/lang/String;)V") ==
      JVMMethodDescriptors.MethodDescriptor(JVMTypeVoid(), Seq(JVMTypeObjectStr("java/lang/String"))))
    assert (JVMMethodDescriptors.methodDescriptorToTypes("(Ljava/lang/String;I)V") ==
      JVMMethodDescriptors.MethodDescriptor(JVMTypeVoid(), Seq(JVMTypeObjectStr("java/lang/String"),JVMTypeInt())))
    assert (JVMMethodDescriptors.methodDescriptorToTypes("([Ljava/lang/Object;)Ljava/util/List;") ==
      JVMMethodDescriptors.MethodDescriptor(JVMTypeObjectStr("java/util/List"), Seq(JVMTypeArray(JVMTypeObjectStr("java/lang/Object")))))

  }

  test("objectType") {
    JVMMethodDescriptors.objectType.parse("Ljava/lang/String;").get
  }
}
