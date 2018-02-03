package jvm

import jvmclass.JVMByteCode.JVMVar
import jvmclass.JVMTypes.JVMType
import jvmclass.{JVMByteCode, JVMClassFile}

import scala.collection.mutable

// An instance of 'JVMClassFile'
class JVMClassInstance(cf: JVMClassFile) {
  protected val fields = mutable.Map.empty[String, JVMVar]

  def getField(name: String): JVMVar = fields(name)

  def putField(name: String, typ: JVMType, value: JVMByteCode.JVMVar) = {
    fields(name) = value
  }
}

// The static members of 'JVMClassFile'
class JVMClassStatic(val cf: JVMClassFile) extends JVMClassInstance(cf)  {

}