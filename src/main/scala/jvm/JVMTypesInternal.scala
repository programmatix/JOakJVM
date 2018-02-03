package jvm

import jvmclass.JVMByteCode.JVMVar
import jvmclass.JVMTypes.JVMType

object JVMTypesInternal {
  sealed trait JVMTypeInternal extends JVMType

  // Only used in the JVM itself
  //  case class JVMVarField(field: Field) extends JVMVar
  case class JVMVarObject(o: Object) extends JVMTypeInternal with JVMVar

  sealed trait JVMObjectRef extends JVMTypeInternal with JVMVar

  case class JVMVarObjectRefUnmanaged(o: Object) extends JVMObjectRef
  case class JVMVarObjectRefManaged(klass: JVMClassInstance) extends JVMObjectRef
  // (NEWINST1)
  // After <init>, JVMVarNewInstanceToken.created will be populated and it can be treated as an JVMVarObjectRefUnmanaged
  case class JVMVarNewInstanceToken(clsRef: Class[_]) extends JVMObjectRef {
    var created: Option[Object] = None
  }

    // Only used in the JVM
    // longs and doubles are meant to take up two variables on the stack
    case class JVMTypeDummy() extends JVMTypeInternal
    // java/lang/String
    case class JVMTypeObjectStr(clsRaw: String) extends JVMTypeInternal
  //  case class JVMTypeObjectCls(cls: Class[_]) extends JVMTypeInternal
    case class JVMTypeObjectRef(obj: Object) extends JVMTypeInternal

    case class JVMTypeReturnAddress() extends JVMTypeInternal
    case class JVMTypeReference() extends JVMTypeInternal


}
