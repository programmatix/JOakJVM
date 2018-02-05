package jvm

import jvmclass.JVMByteCode.JVMVar
import jvmclass.JVMTypes.JVMType

object JVMTypesInternal {

  sealed trait JVMTypeInternal extends JVMType

  // Only used in the JVM itself
  //  case class JVMVarField(field: Field) extends JVMVar
  case class JVMVarObject(o: Object) extends JVMTypeInternal with JVMVar {
    override def asObject: Object = o.asInstanceOf[Object]
  }

  sealed trait JVMObjectRef extends JVMTypeInternal with JVMVar

  case class JVMVarObjectRefUnmanaged(o: Object) extends JVMObjectRef {
    override def asObject: Object = o
  }

  case class JVMVarObjectRefManaged(klass: JVMClassInstance) extends JVMObjectRef {
    override def asObject: Object = klass.asInstanceOf[Object]
  }

  // (NEWINST1)
  // After <init>, JVMVarNewInstanceToken.created will be populated and it can be treated as an JVMVarObjectRefUnmanaged
  case class JVMVarNewInstanceToken(clsRef: Class[_]) extends JVMObjectRef {
    var created: Option[Object] = None
    override def asObject: Object = {
      assert (created.isDefined)
      created.get
    }
  }

  // Only used in the JVM
  // longs and doubles are meant to take up two variables on the stack
  case class JVMTypeDummy() extends JVMTypeInternal

  // java/lang/String
  case class JVMTypeObjectStr(clsRaw: String) extends JVMTypeInternal
  case class JVMTypeClsRef(clsRef: Class[_]) extends JVMTypeInternal

  //  case class JVMTypeObjectCls(cls: Class[_]) extends JVMTypeInternal
  case class JVMTypeObjectRef(obj: Object) extends JVMTypeInternal

  case class JVMTypeReturnAddress() extends JVMTypeInternal

  case class JVMTypeReference() extends JVMTypeInternal


}
