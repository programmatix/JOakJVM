package jvm

import jvmclass.JVMByteCode.JVMVar

import scala.collection.mutable.ArrayBuffer

// All methods in here can act on a single stackframe with no knowledge of the surrounded JVM
// Prefer putting code here rather than the overcrowded JVM class
object JVMStackFrame {

  private[jvm] def getMethodArgs(sf: StackFrame, methodTypes: JVMMethodDescriptors.MethodDescriptor): Seq[JVMVar] = {
    val args = ArrayBuffer.empty[JVMVar]

    methodTypes.args.foreach(arg => {
      val next = sf.pop()
      args += next
    })

    args.toVector
  }



}
