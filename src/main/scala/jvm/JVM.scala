package jvm

import jvm.JVMTypesInternal._
import jvmclass.JVMByteCode._
import jvmclass.JVMClassFile
import jvmclass.JVMClassFileReader.ReadParams
import jvmclass.JVMClassFileTypes._
import jvmclass.JVMTypes._
import jvmclass.internal.JVMClassFileReaderUtils
import ui.{UIInterface, UIStdOut}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class StackFrame(val cf: JVMClassFile,
                // next 3 are for debugging
                 val methodName: String,
                 val methodDescriptor: String,
                 val thisPointer: Object = null) {
  val locals = mutable.Map[Int, JVMVar]()

  // Only bytes, shorts and ints can be pushed directly onto the stack.  Other types get stored as locals.
  // Nope, fconst can push a float
  val stack = mutable.Stack[JVMVar]()

  def addLocal(idx: Int, value: JVMVar) = {
    locals(idx) = value
  }

  def getLocal(idx: Int): JVMVar = {
    if (!locals.contains(idx)) {
      JVM.err(this, s"Do not have local variable ${idx}")
    }
    locals(idx)
  }

  def push(value: JVMVar) = stack.push(value)

  def pop(): JVMVar = stack.pop()

  val constants = mutable.Stack[Constant]()
}

case class ExecuteParams(
                          // For testing: called just before a return
                          onReturn: Option[(StackFrame) => Unit] = None,

                          ui: UIInterface = new UIInterface {}
                        )

class JVMContext {
  val staticClasses = ArrayBuffer.empty[JVMClassStatic]
}

// A Java Virtual Machine implementation, just for learning purposes
// See 'JVMClassLoader' for a discussion over why two classloaders are provided (rather than standard chaining)
class JVM(classLoader: JVMClassLoader,
          systemClassLoader: ClassLoader = ClassLoader.getSystemClassLoader) {

  private[jvm] val context = new JVMContext

  private[jvm] def checkCast(sf: StackFrame, index: Int, params: ExecuteParams, context: JVMContext): Unit = {
    val cf = sf.cf
    val clsRef = cf.getConstant(index).asInstanceOf[ConstantClass]
    val className = cf.getString(clsRef.nameIndex).replace('/', '.')

    val value = sf.stack.pop()

    value match {
      case null                        =>
      case v: JVMVarString             => if (className != "java.lang.String") throw new ClassCastException
      case v: JVMVarObjectRefUnmanaged =>
        if (v.o != null) JVM.err(sf, "Cannot handle checkcast on unmanaged types")
      case v: JVMVarObjectRefManaged   =>
        if (v.klass != null) JVM.err(sf, "Cannot handle checkcast on managed types")
    }

    sf.stack.push(value)
  }

  private[jvm] def putField(sf: StackFrame, index: Int, getObjectRef: Boolean, params: ExecuteParams, context: JVMContext): Unit = {
    val cf = sf.cf
    //  java/io/PrintStream
    val fieldRef = cf.getConstant(index).asInstanceOf[ConstantFieldref]
    val clsRef = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
    val className = cf.getString(clsRef.nameIndex).replace('/', '.')
    val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]

    val name = cf.getString(nameAndType.nameIndex)
    // Ljava/lang/String;
    val descriptor = cf.getString(nameAndType.descriptorIndex)

    val fieldType = JVMMethodDescriptors.fieldDescriptorToTypes(descriptor)

    val value = sf.stack.pop()
    if (getObjectRef) {
      val objectRef = sf.stack.pop()

      objectRef match {
        case v: JVMVarObjectRefManaged =>
          params.ui.log(s"Putting field ${name} v=${value} on managed instance of ${v.klass.cf.fullName()}")
          v.klass.putField(name, fieldType, value)

        case v: JVMVarObjectRefUnmanaged =>
          JVM.err(sf, "cannot putfield on non-klas yet")
      }
    }
    else {


      //      case _ =>
      context.staticClasses.find(_.cf.fullName() == className) match {
        case Some(sc) =>
          params.ui.log(s"Putting field ${name} v=${value} on managed static of ${className}")
          sc.putField(name, fieldType, value)
        case _        =>
          JVM.err("Cannot find static class")
        // JVMClassStatic is usually created on <clinit>, but seeing class files where this isn't defined sometimes
        // So just create it whenever needed
        //            val staticClass = new JVMClassStatic(cf)
        //            context.staticClasses += staticClass
        //            staticClass.putField(name, fieldType, value)
      }
      //    }
    }
  }

  private[jvm] def getField(sf: StackFrame, index: Int, getObjectRef: Boolean, params: ExecuteParams, context: JVMContext): Unit = {
    val cf = sf.cf
    val fieldRef = cf.getConstant(index).asInstanceOf[ConstantFieldref]
    val clsRef = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
    val className = cf.getString(clsRef.nameIndex).replace('/', '.')
    val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]

    val name = cf.getString(nameAndType.nameIndex)
    val descriptor = cf.getString(nameAndType.descriptorIndex)

    val fieldType = JVMMethodDescriptors.fieldDescriptorToTypes(descriptor)

    if (getObjectRef) {
      val objectRef = sf.stack.pop()

      objectRef match {
        case v: JVMVarObjectRefManaged =>
          sf.push(v.klass.getField(name))

        case v: JVMVarObjectRefUnmanaged =>
          JVM.err(sf, "cannot getfield on non-klas yet")
      }
    }
    else {


      //      case _ =>
      context.staticClasses.find(_.cf.fullName() == className) match {
        case Some(sc) =>
          sf.push(sc.getField(name))
        case _        =>
          JVM.err("Cannot find static class")
        // JVMClassStatic is usually created on <clinit>, but seeing class files where this isn't defined sometimes
        // So just create it whenever needed
        //            val staticClass = new JVMClassStatic(cf)
        //            context.staticClasses += staticClass
        //            staticClass.putField(name, fieldType, value)
      }
      //    }
    }
  }

  // Takes arguments off the stack, and casts them into the types expected by the method we're about to call
  private[jvm] def getMethodArgsAsObjects(sf: StackFrame, methodTypes: JVMMethodDescriptors.MethodDescriptor, params: ExecuteParams): (Seq[Object], Seq[Class[_]]) = {
    val args = ArrayBuffer.empty[Object]
    val argTypes = ArrayBuffer.empty[Class[_]]

    methodTypes.args.reverse.foreach(desired => {
      val next = sf.pop()
      desired match {
        case v: JVMTypeUnmanagedClsRef =>
          args += next.asObject
          argTypes += v.clsRef
        case v: JVMTypeObjectStr       =>
          // Should have been resolved to JVMTypeClsRef
          JVM.err(s"not expecting JVMTypeObjectStr here")
        //          next match {
        //            case x: JVMVarString             =>
        //              args += x.v
        //              argTypes += classOf[java.lang.String]
        //            case x: JVMVarObject             =>
        //              args += x.o
        //              argTypes += x.o.getClass
        //            case x: JVMVarObjectRefUnmanaged =>
        //              args += x.o
        //              argTypes += x.o.getClass
        //            case x: JVMVarNewInstanceToken   =>
        //              args += x.created.get
        //              argTypes += x.created.get.getClass
        //            case _                           =>
        //              JVM.err(sf, s"cannot handle arg ${next} yet")
        //          }
        case v: JVMTypeVoid =>
          JVM.err(s"not expecting void here")

        // Handle primitive types
        case v: JVMTypeBoolean =>
          args += next.asInstanceOf[JVMVarBoolean].v.asInstanceOf[Object]
          argTypes += java.lang.Boolean.TYPE
        case v: JVMTypeInt     =>
          args += next.asInstanceOf[JVMVarInt].v.asInstanceOf[Object]
          // Type gives us the class of the primitive
          argTypes += java.lang.Integer.TYPE
        case v: JVMTypeShort   =>
          args += next.asInstanceOf[JVMVarShort].v.asInstanceOf[Object]
          argTypes += java.lang.Short.TYPE
        case v: JVMTypeByte    =>
          args += next.asInstanceOf[JVMVarByte].v.asInstanceOf[Object]
          argTypes += java.lang.Byte.TYPE
        case v: JVMTypeChar    =>
          args += next.asInstanceOf[JVMVarInteger].asChar.asInstanceOf[Object]
          argTypes += java.lang.Character.TYPE
        case v: JVMTypeFloat   =>
          args += next.asInstanceOf[JVMVarFloat].v.asInstanceOf[Object]
          argTypes += java.lang.Float.TYPE
        case v: JVMTypeDouble  =>
          args += next.asInstanceOf[JVMVarDouble].v.asInstanceOf[Object]
          argTypes += java.lang.Double.TYPE
        case v: JVMTypeLong    =>
          args += next.asInstanceOf[JVMVarLong].v.asInstanceOf[Object]
          argTypes += java.lang.Long.TYPE

        //        case v: JVMTypeObjectStr =>
        //          // java/util/List
        //          val className = v.clsRaw.replace('/', '.')
        //
        //          classLoader.loadClass(className, this, params) match {
        //            case Some(clsRef) =>
        //              JVM.err(sf, "can't handle passing around managed types")
        //
        //            case _ =>
        //              val raw = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
        //              val clsRef = ClassLoader.getSystemClassLoader.loadClass(className)
        //              args += raw
        //              argTypes += clsRef
        //          }

        case v: JVMTypeArray =>
          v.typ match {
            //            case v: JVMTypeObjectStr =>
            //              val array = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
            //              // java/util/List
            //              val className = v.clsRaw.replace('/', '.')
            //
            //              classLoader.loadClass(className, this, params) match {
            //                case Some(clsRef) =>
            //                  JVM.err(sf, "can't handle passing around managed types")
            //
            //                case _ =>
            //                  val clsRef = ClassLoader.getSystemClassLoader.loadClass(className)
            //                  args += array
            //                  argTypes += classOf[Array[Object]]
            //              }
            case _ =>
              args += next.asObject
              argTypes += next.asObject.getClass

            //              val array = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
            //              args += array
            //              argTypes += array.getClass

          }
      }
    }
    )

    (args.toVector.reverse, argTypes.toVector.reverse)
  }

  // Returns JVMClassInstance if it's a managed class, else None and the (NEWINST1) procedure kicks in
  private[jvm] def createNewInstance(sf: StackFrame, cls: ConstantClass, classLoader: JVMClassLoader, systemClassLoader: ClassLoader, params: ExecuteParams): Option[JVMVar]

  = {
    val cf = sf.cf
    //  java/io/PrintStream
    val className = cf.getString(cls.nameIndex)

    val resolvedClassName = className.replace("/", ".")

    // See JVMClassLoader for a description of what's going on here
    classLoader.loadClass(resolvedClassName, this, params) match {

      case Some(clsRef) =>
        val klass = new JVMClassInstance(clsRef)
        klass.cf.fields.foreach(field => {
          val fieldName = clsRef.getString(field.nameIndex)
          val fieldDescriptor = clsRef.getString(field.descriptorIndex)

          val fieldType = JVMMethodDescriptors.fieldDescriptorToTypes(fieldDescriptor)

          val typ = resolveFieldType(fieldType, params)

          /*
          For type byte, the default value is zero, that is, the value of (byte)0.
          For type short, the default value is zero, that is, the value of (short)0.
          For type int, the default value is zero, that is, 0.
          For type long, the default value is zero, that is, 0L.
          For type float, the default value is positive zero, that is, 0.0f.
          For type double, the default value is positive zero, that is, 0.0.
          For type char, the default value is the null character, that is, '\u0000'.
          For type boolean, the default value is false.
          For all reference types (§2.4.6), the default value is null (§2.3).
          Each method parameter (§2.5) is initialized to the corresponding argument value provided by the invoker of the method.
          Each constructor parameter (§2.5) is initialized to the corresponding argument value provided by an object creation expression or explicit constructor invocation.
          An exception-handler parameter (§2.16.2) is initialized to the thrown object representing the exception (§2.16.3).
          A local variable must be explicitly given a value, by either initialization or assignment, before it is used.
           */
          
          typ match {
            case _: JVMTypeByte => klass.putField(fieldName, typ, JVMVarByte(0))  
            case _: JVMTypeShort => klass.putField(fieldName, typ, JVMVarShort(0))  
            case _: JVMTypeInt => klass.putField(fieldName, typ, JVMVarInt(0))  
            case _: JVMTypeLong => klass.putField(fieldName, typ, JVMVarLong(0))  
            case _: JVMTypeFloat => klass.putField(fieldName, typ, JVMVarFloat(0))  
            case _: JVMTypeDouble => klass.putField(fieldName, typ, JVMVarByte(0))  
            case _: JVMTypeChar => klass.putField(fieldName, typ, JVMVarChar('\u0000'))  
            case _: JVMTypeBoolean => klass.putField(fieldName, typ, JVMVarBoolean(false))
            case v: JVMTypeManagedClsRef => klass.putField(fieldName, typ, JVMVarObjectRefManaged(null))
            case v: JVMTypeUnmanagedClsRef => klass.putField(fieldName, typ, JVMVarObjectRefUnmanaged(null))

          }
          
        })
        Some(JVMVarObjectRefManaged(klass))

      case _ =>
        val clsRef = systemClassLoader.loadClass(resolvedClassName)
        // Class.getDeclaredConstructor(String.class).newInstance("HERESMYARG");
        //        val newInstance = clsRef.newInstance().asInstanceOf[Object]
        //        JVMVarObject(newInstance)


        val token = JVMVarNewInstanceToken(clsRef)
        Some(token)
    }

  }

  private def resolveMethodTypes(methodTypesRaw: JVMMethodDescriptors.MethodDescriptor, params: ExecuteParams): JVMMethodDescriptors.MethodDescriptor = {
    // Turn the class strings into more useful class references
    methodTypesRaw.copy(args = methodTypesRaw.args.map(methodType => resolveFieldType(methodType, params)))
  }

  private def resolveFieldType(typ: JVMType, params: ExecuteParams): JVMType = {
    // Turn the class strings into more useful class references
    typ match {
      case v: JVMTypeObjectStr =>
        val resolved = v.clsRaw.replace("/", ".")
        classLoader.loadClass(resolved, this, params) match {
          case Some(clsFile) =>
            JVMTypeManagedClsRef(clsFile)
          case _             =>
            val clsRef = systemClassLoader.loadClass(resolved)
            JVMTypeUnmanagedClsRef(clsRef)
        }
      case _                   => typ
    }
  }

  private def invokeMethodRef(sf: StackFrame, index: Int, getObjectRef: Boolean, params: ExecuteParams): Unit

  = {
    val cf = sf.cf
    val fieldRef = cf.getConstant(index).asInstanceOf[ConstantRef]
    val cls = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
    val className = cf.getString(cls.nameIndex)
    val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]
    val methodName = cf.getString(nameAndType.nameIndex)
    val methodDescriptor = cf.getString(nameAndType.descriptorIndex)

    val methodTypesRaw = JVMMethodDescriptors.methodDescriptorToTypes(methodDescriptor)
    val methodTypes = resolveMethodTypes(methodTypesRaw, params)

    val resolvedClassName = className.replace("/", ".")

    // See JVMClassLoader for a description of what's going on here
    classLoader.loadClass(resolvedClassName, this, params) match {

      case Some(clsRef) =>
        clsRef.getMethod(methodName, methodDescriptor) match {
          case Some(method) =>

            // This pops from the stack, and needs to be done before getting the object ref
            val argsRaw = JVMStackFrame.getMethodArgs(sf, methodTypes)

            val objectRef = if (getObjectRef) {
              val value = sf.stack.pop()
              value match {
//                case v: JVMVarObject           => v.o
                case v: JVMVarObjectRefManaged => v.klass
                case v: JVMVarObjectRefUnmanaged => v.o
              }
            }
            else null


            var code: Option[Seq[JVMOpCodeWithArgs]] = None
            var newCf: Option[JVMClassFile] = None

            // Find the right method, following Java virtual rules
            objectRef match {

              case v: JVMClassInstance =>
                if (methodName == "<init>" || v.cf.fullName().equals(resolvedClassName)) {
                  // The object ref is of the exact same class as the method we're calling, so no overridden versions to worry about
                  // Or we're calling <init>
                  code = Some(method.getCode().codeOrig)
                  newCf = Some(clsRef)
                }
                else {
                  // invokevirtual: see if there's an overridden version of this method
                  params.ui.log(s"Finding correct method for ${resolvedClassName} ${methodName} ${methodDescriptor} for object ref ${objectRef}")

                  var cfExamining = v.cf

                  while (code.isEmpty) {
                    val possiblySimilarMethods = cfExamining.getMethods(methodName)

                    possiblySimilarMethods.foreach(m => {
                      if (code.isEmpty) {
                        val mDescriptor = cfExamining.getString(m.descriptorIndex)

                        if (mDescriptor == methodDescriptor) {
                          // Found an overridden version
                          code = Some(m.getCode().codeOrig)
                          newCf = Some(cfExamining)

                          params.ui.log(s"Found overridden method in ${cfExamining.fullName()} ${methodName} ${methodDescriptor} for object ref ${objectRef}")
                        }
                      }
                    })

                    if (code.isEmpty) {
                      val superClassName = cfExamining.getSuperClassName()

                      classLoader.loadClass(superClassName, this, params) match {

                        case Some(superClsRef) =>
                          cfExamining = superClsRef
                          params.ui.log(s"Looking for overridden method in super ${cfExamining.fullName()} ${methodName} ${methodDescriptor} for object ref ${objectRef}")

                        case _ =>
                          JVM.err(sf, "Cannot handle managed classes extending unmanaged yet")
                      }
                    }
                  }
                }

              case _ =>
                // No overridden methods to worry about
                code = Some(method.getCode().codeOrig)
                newCf = Some(clsRef)
            }

            assert(code.isDefined)
            assert(newCf.isDefined)

            //            val args: Seq[JVMVar] = if (methodName == "<init>") {
            val args: Seq[JVMVar] = if (getObjectRef) {
              // TODO can't find in spec, but this pointer is definitely passed too
              argsRaw :+ JVMVarObjectRefManaged(objectRef.asInstanceOf[JVMClassInstance])
            }
            else argsRaw

            val sfNew = new StackFrame(newCf.get, methodName, methodDescriptor, objectRef)

            sfNew.locals ++= args.reverse.zipWithIndex.map(arg => arg._2 -> arg._1).toMap

            params.ui.log(s"Executing managed method ${newCf.get.fullName()} ${methodName} with stack frame ${sfNew} ")

            executeFrame(sfNew, code.get, params) match {
              case Some(ret) => sf.stack.push(ret)
              case _         =>
            }
          case _            => JVM.err(s"Unable to find method $methodName in class ${clsRef.className}")
        }

      case _ =>
        val clsRef = systemClassLoader.loadClass(resolvedClassName)

        // This pops from the stack
        val (args, argTypes) = getMethodArgsAsObjects(sf, methodTypes, params)

        val next = if (getObjectRef) sf.stack.pop() else null

        if (next.isInstanceOf[JVMVarNewInstanceToken] && next.asInstanceOf[JVMVarNewInstanceToken].created.isEmpty) {
          val newInstance = next.asInstanceOf[JVMVarNewInstanceToken]
          val ctor = newInstance.clsRef.getDeclaredConstructor(argTypes: _*)
          val created = ctor.newInstance(args: _*).asInstanceOf[Object]
          newInstance.created = Some(created)
          // Don't want to push, the newInstance token is already on the stack
          //            sf.push(JVMVarObjectRefUnmanaged(created))
        }



        // Don't need to call <init>, it's done for us for unmanaged classes by real JVM
        if (methodName != "<init>") {
          val objectRef = if (getObjectRef) {
            next match {
//              case v: JVMVarObject             => v.o
              case v: JVMVarString             => v.v
              case v: JVMVarNewInstanceToken   => v.created.get
              case v: JVMVarObjectRefUnmanaged => v.o
            }
          }
          else null

          val methodRef = clsRef.getMethod(methodName, argTypes: _*)

          var objectRefTrying: Any = objectRef
          var result: Any = null

          if (getObjectRef) {
            params.ui.log(s"Executing unmanaged method ${resolvedClassName} ${methodName} on ref ${objectRef} with args ${args}")

            //            while (objectRefTrying != null) {
            try {
              // Spec for invokevirtual says, if we can't call the method on the given objectRef, do it recursively with the objectRef's superclass
              // We take advantage of running on a JVM here and just try to execute it, rather than implementing all the what-can-call-what rules

              result = methodRef.invoke(objectRef, args: _*) // :_* is the hint to expand the Seq to varargs

              // Done!
              objectRefTrying = null
            }
            catch {
              case e: IllegalAccessException =>
                params.ui.log(s"Warning: could not execute ${resolvedClassName} ${methodName} with args ${args}")

            }
            //                  val newObjectRef = objectRefTrying.getClass.getSuperclass.cast(objectRefTrying)
            //                  objectRefTrying = newObjectRef
            //              }
            //            }
          }
          else {
            params.ui.log(s"Executing unmanaged static method ${resolvedClassName} ${methodName} with args ${args}")

            result = methodRef.invoke(null, args: _*) // :_* is the hint to expand the Seq to varargs
          }

          if (!methodTypes.ret.isInstanceOf[JVMTypeVoid]) {
            result match {
              case null       => sf.push(JVMVarObjectRefUnmanaged(null))
              case v: Integer => sf.push(JVMVarInt(v))
              case v: Double  => sf.push(JVMVarDouble(v))
              case v: Byte    => sf.push(JVMVarByte(v))
              case v: Char    => sf.push(JVMVarChar(v))
              case v: Float   => sf.push(JVMVarFloat(v))
              case v: Short   => sf.push(JVMVarShort(v))
              case v: String  => sf.push(JVMVarString(v))
              case v: Boolean => sf.push(JVMVarBoolean(v))
              case _          =>
                sf.push(JVMVarObjectRefUnmanaged(result.asInstanceOf[Object]))
            }
          }


        }
    }

  }


  private def invokeInterface(sf: StackFrame, index: Int, count: Int, getObjectRef: Boolean, params: ExecuteParams): Unit

  = {
    invokeMethodRef(sf, index, getObjectRef, params)
    //    val cf = sf.cf
    //    val interfaceRef = cf.getConstant(index).asInstanceOf[ConstantInterfaceMethodref]
    //    val cls = cf.getConstant(interfaceRef.classIndex).asInstanceOf[ConstantClass]
    //    val className = cf.getString(cls.nameIndex)
    //    val nameAndType = cf.getConstant(interfaceRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]
    //    val methodName = cf.getString(nameAndType.nameIndex)
    //    val methodDescriptor = cf.getString(nameAndType.descriptorIndex)
    //    val methodTypes = JVMMethodDescriptors.methodDescriptorToTypes(methodDescriptor)
    //    val resolvedClassName = className.replace("/", ".")
    //
    //    // See JVMClassLoader for a description of what's going on here
    //    classLoader.loadClass(resolvedClassName, this, params) match {
    //
    //      case Some(clsRef) =>
    //        clsRef.getMethod(methodName) match {
    //          case Some(method) =>
    //            val code = method.getCode().codeOrig
    //            val sfNew = new StackFrame(clsRef, methodName)
    //
    //            // This pops from the stack
    //            val argsRaw = JVMStackFrame.getMethodArgs(sf, methodTypes)
    //
    //            val objectRef = if (getObjectRef) {
    //              sf.stack.pop() match {
    //                case v: JVMVarObject           => v.o
    //                case v: JVMVarObjectRefManaged => v.klass
    //              }
    //            }
    //            else null
    //
    //            val args: Seq[JVMVar] = if (methodName == "<init>") {
    //              // TODO can't find in spec, but this pointer is definitely passed too
    //              argsRaw :+ JVMVarObjectRefManaged(objectRef.asInstanceOf[JVMClassInstance])
    //            }
    //            else argsRaw
    //
    //            sfNew.locals ++= args.zipWithIndex.map(arg => arg._2 -> arg._1).toMap
    //
    //            executeFrame(sfNew, code, params) match {
    //              case Some(ret) => sf.stack.push(ret)
    //              case _         =>
    //            }
    //          case _            => JVM.err(s"Unable to find method $methodName in class ${clsRef.className}")
    //        }
    //
    //      case _ =>
    //        val clsRef = systemClassLoader.loadClass(resolvedClassName)
    //
    //        // This pops from the stack
    //        val (args, argTypes) = getMethodArgsAsObjects(sf, methodTypes, params)
    //
    //        val next = if (getObjectRef) sf.stack.pop() else null
    //
    //        if (next.isInstanceOf[JVMVarNewInstanceToken] && next.asInstanceOf[JVMVarNewInstanceToken].created.isEmpty) {
    //          val newInstance = next.asInstanceOf[JVMVarNewInstanceToken]
    //          val ctor = newInstance.clsRef.getDeclaredConstructor(argTypes: _*)
    //          val created = ctor.newInstance(args: _*).asInstanceOf[Object]
    //          newInstance.created = Some(created)
    //          // Don't want to push, the newInstance token is already on the stack
    //          //            sf.push(JVMVarObjectRefUnmanaged(created))
    //        }
    //
    //
    //
    //        // Don't need to call <init>, it's done for us for unmanaged classes by real JVM
    //        if (methodName != "<init>") {
    //          val objectRef = if (getObjectRef) {
    //            next match {
    //              case v: JVMVarObject           => v.o
    //              case v: JVMVarString           => v.v
    //              case v: JVMVarNewInstanceToken => v.created.get
    //              case v: JVMVarObjectRefUnmanaged => v.o
    //            }
    //          }
    //          else null
    //
    //          val methodRef = clsRef.getMethod(methodName, argTypes: _*)
    //
    //          val result = methodRef.invoke(objectRef, args: _*) // :_* is the hint to expand the Seq to varargs
    //
    //          result match {
    //            case null =>
    //            case _    =>
    //              sf.push(JVMVarObjectRefUnmanaged(result))
    //          }
    //        }
    //    }

  }


  private[jvm] def executeFrame(sf: StackFrame, code: Seq[JVMOpCodeWithArgs], params: ExecuteParams = ExecuteParams()): Option[JVMVar]

  = {
    val cf = sf.cf

    def popInt(): Int = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt

    def store(index: Int): Unit = {
      val v1 = sf.stack.pop()
      sf.addLocal(index, v1)
    }

    def load(index: Int): Unit = {
      val stored = sf.getLocal(index)
      sf.push(stored)
    }

    var opcodeAddress = 0
    var opcodeIdx = 0

    def jumpToOffset(offset: Int): Unit = {
      val targetAddress = opcodeAddress + offset
      var tempOpCodeIdx = opcodeIdx
      var tempOpCodeAddress = opcodeAddress
      if (offset >= 0) {
        while (tempOpCodeAddress < targetAddress) {
          val op = code(tempOpCodeIdx)
          tempOpCodeIdx += 1
          tempOpCodeAddress += op.oc.lengthInBytes
        }
      }
      else {
        while (tempOpCodeAddress > targetAddress) {
          tempOpCodeIdx -= 1
          val op = code(tempOpCodeIdx)
          tempOpCodeAddress -= op.oc.lengthInBytes
        }
      }
      if (tempOpCodeAddress != targetAddress) {
        JVM.err(s"failed to jump exactly to instruction ${targetAddress}")
      }
      opcodeIdx = tempOpCodeIdx
      opcodeAddress = tempOpCodeAddress
    }

    var done = false
    var out: Option[JVMVar] = None

    def doReturn(): Unit = {
      if (params.onReturn.isDefined) params.onReturn.get(sf)
      done = true
      out = Some(sf.stack.pop())
    }

    while (opcodeIdx < code.length && !done) {

      var incOpCode = true

      def handleJumpOpcode(op: JVMOpCodeWithArgs): Unit = {
        val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
        jumpToOffset(offset)
        incOpCode = false
      }

      val op = code(opcodeIdx)

      params.ui.stopBeforeExecutingOpcode(op, sf)

      op.oc.hexcode match {
        case 0x32 => // aaload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Object]]
          val value = arrayref(index)
          sf.stack.push(JVMVarObjectRefUnmanaged(value))

        case 0x53 => // aastore
          val value: Object = sf.stack.pop().asObject
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          //  arrayref must be of type reference and must refer to an array whose components are of type reference
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Object]]
          arrayref(index) = value

        case 0x01 => // aconst_null
          sf.stack.push(JVMVarObjectRefUnmanaged(null))

        case 0x19 => // aload
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val local = sf.getLocal(index)
          sf.push(local)

        case 0x2a => // aload_0
          sf.push(sf.getLocal(0))

        case 0x2b => // aload_1
          sf.push(sf.getLocal(1))

        case 0x2c => // aload_2
          sf.push(sf.getLocal(2))

        case 0x2d => // aload_3
          sf.push(sf.getLocal(3))

        case 0xbd => // anewarray
          val count = popInt()
          val array = new Array[Object](count)
          sf.push(JVMVarObjectRefUnmanaged(array))

        case 0xb0 => // areturn
          doReturn()

        case 0xbe => // arraylength
          val array = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged].o.asInstanceOf[Array[_]]
          sf.push(JVMVarInt(array.length))

        case 0x3a => // astore
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          store(index)

        case 0x4b => // astore_0
          store(0)

        case 0x4c => // astore_1
          store(1)

        case 0x4d => // astore_2
          store(2)

        case 0x4e => // astore_3
          store(3)

        case 0xbf => // athrow
          val throwing = sf.stack.pop()
          throwing match {
            case v: JVMVarNewInstanceToken => throw v.created.get.asInstanceOf[Throwable]
            case v: JVMObjectRef => throw v.asObject.asInstanceOf[Throwable]
          }

        case 0x33 => // baload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Byte]]
          val value = arrayref(index)
          sf.stack.push(JVMVarInt(value))

        case 0x54 => // bastore
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Byte]]
          arrayref(index) = value.toByte

        case 0x10 => // bipush
          sf.stack.push(op.args.head)

        case 0xca => // breakpoint
          println("Breakpoint hit!")

        case 0x34 => // caload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Char]]
          val value = arrayref(index)
          sf.stack.push(JVMVarInt(value))

        case 0x55 => // castore
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Char]]
          arrayref(index) = value.toChar

        case 0xc0 => // checkcast
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          checkCast(sf, index, params, context)

        case 0x90 => // d2flol
          val value = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarFloat(value.toFloat))

        case 0x8e => // d2i
          val value = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarInt(value.toInt))

        case 0x8f => // d2l
          val value = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarLong(value.toLong))

        case 0x63 => // dadd
          val v1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarDouble(v1 + v2))

        case 0x31 => // daload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Double]]
          val value = arrayref(index)
          sf.stack.push(JVMVarDouble(value))

        case 0x52 => // dastore
          val value = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Double]]
          arrayref(index) = value

        case 0x98 => // dcmpg
          val value2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val value1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val toPush: Int = if (value1 > value2) 1
          else if (value1 == value2) 0
          else -1
          sf.stack.push(JVMVarInt(toPush))

        case 0x97 => // dcmpl
          val value2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val value1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val toPush: Int = if (value1 > value2) 1
          else if (value1 == value2) 0
          else -1
          sf.stack.push(JVMVarInt(toPush))

        case 0x0e => // dconst_0
          sf.stack.push(JVMVarDouble(0))

        case 0x0f => // dconst_1
          sf.stack.push(JVMVarDouble(0))

        case 0x6f => // ddiv
          val v1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarDouble(v2 / v1))

        case 0x18 => // dload
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val local = sf.getLocal(index)
          sf.push(local)

        case 0x26 => // dload_0
          sf.push(sf.getLocal(0))

        case 0x27 => // dload_1
          sf.push(sf.getLocal(1))

        case 0x28 => // dload_2
          sf.push(sf.getLocal(2))

        case 0x29 => // dload_3
          sf.push(sf.getLocal(3))

        case 0x6b => // dmul
          val v1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarDouble(v1 * v2))

        case 0x77 => // dneg
          val v1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarDouble(-v1))

        case 0x73 => // drem
          val v1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarDouble(v1 + (v1 / v2) * v2))

        case 0xaf => // dreturn
          doReturn()

        case 0x39 => // dstore
          val v1 = sf.stack.pop()
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          sf.addLocal(index, v1)

        case 0x47 => // dstore_0
          store(0)

        case 0x48 => // dstore_1
          store(1)

        case 0x49 => // dstore_2
          store(2)

        case 0x4a => // dstore_3
          store(3)

        case 0x67 => // dsub
          val v1 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarDouble].v
          sf.stack.push(JVMVarDouble(v2 - v1))

        case 0x59 => // dup
          sf.stack.push(sf.stack.head)

        case 0x5a => // dup_x1
          JVM.err("Cannot handle opcode dup_x1 yet")
        case 0x5b => // dup_x2
          JVM.err("Cannot handle opcode dup_x2 yet")
        case 0x5c => // dup2
          JVM.err("Cannot handle opcode dup2 yet")
        case 0x5d => // dup2_x1
          JVM.err("Cannot handle opcode dup2_x1 yet")
        case 0x5e => // dup2_x2
          JVM.err("Cannot handle opcode dup2_x2 yet")
        case 0x8d => // f2d
          val v1 = sf.pop().asInstanceOf[JVMVarFloat]
          sf.push(JVMVarDouble(v1.v))

        case 0x8b => // f2i
          val value = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarInt(value.toInt))

        case 0x8c => // f2l
          val value = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarLong(value.toLong))

        case 0x62 => // fadd
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          val v3 = JVMVarFloat(v1.v + v2.v)
          sf.stack.push(v3)

        case 0x30 => // faload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Float]]
          val value = arrayref(index)
          sf.stack.push(JVMVarFloat(value))

        case 0x51 => // fastore
          val value = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Float]]
          arrayref(index) = value

        case 0x96 => // fcmpg
          val value2 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val value1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val toPush: Int = if (value1 > value2) 1
          else if (value1 == value2) 0
          else -1
          sf.stack.push(JVMVarInt(toPush))

        case 0x95 => // fcmpl
          val value2 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val value1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val toPush: Int = if (value1 > value2) 1
          else if (value1 == value2) 0
          else -1
          sf.stack.push(JVMVarInt(toPush))

        case 0x0b => // fconst_0
          sf.stack.push(JVMVarFloat(0))

        case 0x0c => // fconst_1
          sf.stack.push(JVMVarFloat(1))

        case 0x0d => // fconst_2
          sf.stack.push(JVMVarFloat(2))

        case 0x6e => // fdiv
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarFloat(v2 / v1))

        case 0x17 => // fload
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val stored = sf.getLocal(index)
          sf.push(stored)

        case 0x22 => // fload_0
          val stored = sf.getLocal(0)
          sf.push(stored)

        case 0x23 => // fload_1
          val stored = sf.getLocal(1)
          sf.push(stored)

        case 0x24 => // fload_2
          val stored = sf.getLocal(2)
          sf.push(stored)

        case 0x25 => // fload_3
          val stored = sf.getLocal(3)
          sf.push(stored)

        case 0x6a => // fmul
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarFloat(v2 * v1))

        case 0x76 => // fneg
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarFloat(-v1))

        case 0x72 => // frem
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarFloat(v1 + (v1 / v2) * v1))

        case 0xae => // freturn
          doReturn()

        case 0x38 => // fstore
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          sf.addLocal(index, v1)

        case 0x43 => // fstore_0
          val v1 = sf.stack.pop()
          sf.addLocal(0, v1)

        case 0x44 => // fstore_1
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          sf.addLocal(1, v1)

        case 0x45 => // fstore_2
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          sf.addLocal(2, v1)

        case 0x46 => // fstore_3
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          sf.addLocal(3, v1)

        case 0x66 => // fsub
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarFloat].v
          sf.stack.push(JVMVarFloat(v2 - v1))

        case 0xb4 => // getfield
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          getField(sf, index, true, params, context)

        case 0xb2 => // getstatic
          // getstatic pops objectref (a reference to an object) from the stack, retrieves the value of the static field
          // (also known as a class field) identified by <field-spec> from objectref, and pushes the one-word or two-word
          // value onto the operand stack.
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val fieldRef = cf.getConstant(index).asInstanceOf[ConstantFieldref]
          val cls = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
          //  java/lang/System
          val className = cf.getString(cls.nameIndex).replace('/', '.')
          val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]
          // out
          val name = cf.getString(nameAndType.nameIndex)
          // Ljava/io/PrintStream;
          val typ = cf.getString(nameAndType.descriptorIndex)
          //          val field = ClassLoader.getSystemClassLoader.loadClass("java.lang.System").getField("out")
          //          ClassLoader.getSystemClassLoader.loadClass("java.lang.System").getField("out").get(classOf[java.io.PrintStream])

          classLoader.loadClass(className, this, params) match {
            case Some(clsRef) =>
              context.staticClasses.find(_.cf.className == className) match {
                case Some(sc) =>
                  val value = sc.getField(name)
                  sf.stack.push(value)
                case _        => JVM.err(sf, s"could not find static class $className")
              }


            case _ =>
              val clsRef = ClassLoader.getSystemClassLoader.loadClass(className)
              val fieldRef2 = clsRef.getField(name)
              val fieldType = fieldRef2.getType
              val fieldInstance = fieldRef2.get(fieldType)
              sf.stack.push(JVMVarObjectRefUnmanaged(fieldInstance))
          }

        case 0xa7 => // goto
          val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
          jumpToOffset(offset)
          incOpCode = false

        case 0xc8 => // goto_w
          JVM.err("Cannot handle opcode goto_w yet")

        case 0x91 => // i2b
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val raw = value.toByte
          val extended = JVMClassFileReaderUtils.extendByteAsTwosComplement(raw)
          sf.stack.push(JVMVarInt(extended))

        case 0x92 => // i2c
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val raw = value.toChar
          //          val extended = JVMClassFileReaderUtils.extendShortAsTwosComplement(raw)
          sf.stack.push(JVMVarInt(raw))

        case 0x87 => // i2d
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          sf.stack.push(JVMVarDouble(value.toDouble))

        case 0x86 => // i2f
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          sf.stack.push(JVMVarFloat(value.toFloat))

        case 0x85 => // i2l
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val extended = JVMClassFileReaderUtils.extendIntAsTwosComplement(value)
          sf.stack.push(JVMVarLong(extended))

        case 0x93 => // i2s
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val raw = value.toShort
          val extended = JVMClassFileReaderUtils.extendShortAsTwosComplement(raw)
          sf.stack.push(JVMVarLong(extended))

        case 0x60 => // iadd
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v1.asInt + v2.asInt)
          sf.stack.push(v3)

        case 0x2e => // iaload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Int]]
          val value = arrayref(index)
          sf.stack.push(JVMVarInt(value))

        case 0x7e => // iand
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v1.asInt & v2.asInt)
          sf.stack.push(v3)

        case 0x4f => // iastore
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Int]]
          arrayref(index) = value

        case 0x02 => // iconst_m1
          sf.stack.push(JVMVarInt(-1))

        case 0x03 => // iconst_0
          sf.stack.push(JVMVarInt(0))

        case 0x04 => // iconst_1
          sf.stack.push(JVMVarInt(1))

        case 0x05 => // iconst_2
          sf.stack.push(JVMVarInt(2))

        case 0x06 => // iconst_3
          sf.stack.push(JVMVarInt(3))

        case 0x07 => // iconst_4
          sf.stack.push(JVMVarInt(4))

        case 0x08 => // iconst_5
          sf.stack.push(JVMVarInt(5))

        case 0x6c => // idiv
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v2.asInt / v1.asInt)
          sf.stack.push(v3)

        case 0xa5 => // if_acmpeq
          JVM.err("Cannot handle opcode if_acmpne yet")
        case 0xa6 => // if_acmpne
          JVM.err("Cannot handle opcode if_acmpne yet")
        case 0x9f => // if_icmpeq
          JVM.err("Cannot handle opcode if_icmpeq yet")
        case 0xa2 => // if_icmpge
          val v2 = popInt()
          val v1 = popInt()
          if (v1 >= v2) {
            handleJumpOpcode(op)
          }

        case 0xa3 => // if_icmpgt
          val v2 = popInt()
          val v1 = popInt()
          if (v1 > v2) {
            handleJumpOpcode(op)
          }

        case 0xa4 => // if_icmple
          val v2 = popInt()
          val v1 = popInt()
          if (v1 <= v2) {
            handleJumpOpcode(op)
          }

        case 0xa1 => // if_icmplt
          val v2 = popInt()
          val v1 = popInt()
          if (v1 < v2) {
            handleJumpOpcode(op)
          }

        case 0xa0 => // if_icmpne
          val v2 = popInt()
          val v1 = popInt()
          if (v1 != v2) {
            handleJumpOpcode(op)
          }

        case 0x99 => // ifeq
          val value = sf.pop().asInstanceOf[JVMVarInteger].asInt
          if (value == 0) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0x9c => // ifge
          val value = sf.pop().asInstanceOf[JVMVarInteger].asInt
          if (value >= 0) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0x9d => // ifgt
          val value = sf.pop().asInstanceOf[JVMVarInteger].asInt
          if (value > 0) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0x9e => // ifle
          val value = sf.pop().asInstanceOf[JVMVarInteger].asInt
          if (value <= 0) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0x9b => // iflt
          val value = sf.pop().asInstanceOf[JVMVarInteger].asInt
          if (value < 0) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0x9a => // ifne
          val value = sf.pop().asInstanceOf[JVMVarInteger].asInt
          if (value != 0) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0xc7 => // ifnonnull
          val value = sf.pop()

          val isNull = value match {
            case null => true
            case v: JVMObjectRef => v.asObject == null
          }

          if (!isNull) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }

        case 0xc6 => // ifnull
          val value = sf.pop()

          val isNull = value match {
            case null => true
            case v: JVMObjectRef => v.asObject == null
          }

          if (isNull) {
            val offset = op.args.head.asInstanceOf[JVMVarInteger].asInt
            jumpToOffset(offset)
            incOpCode = false
          }


        case 0x84 => // iinc
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val const = op.args.last.asInstanceOf[JVMVarInteger].asInt
          val variable = sf.locals(index).asInstanceOf[JVMVarInt]
          sf.locals(index) = JVMVarInt(variable.v + const)

        case 0x15 => // iload
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          load(index)

        case 0x1a => // iload_0
          load(0)

        case 0x1b => // iload_1
          load(1)

        case 0x1c => // iload_2
          load(2)

        case 0x1d => // iload_3
          load(3)

        case 0xfe => // impdep1
          JVM.err("Cannot handle opcode impdep1 yet")
        case 0xff => // impdep2
          JVM.err("Cannot handle opcode impdep2 yet")
        case 0x68 => // imul
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v1.asInt * v2.asInt)
          sf.stack.push(v3)

        case 0x74 => // ineg
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v1.asInt * -1)
          sf.stack.push(v3)

        case 0xc1 => // instanceof
          JVM.err("Cannot handle opcode instanceof yet")
        case 0xba => // invokedynamic
          JVM.err("Cannot handle opcode invokedynamic yet")

        case 0xb9 => // invokeinterface
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val count = op.args(1).asInstanceOf[JVMVarInteger].asInt
          val alwaysZero = op.args(2).asInstanceOf[JVMVarInteger].asInt

          assert(count != 0)
          assert(alwaysZero == 0)

          invokeInterface(sf, index, count, true, params)

        case 0xb7 => // invokespecial
          // https://cs.au.dk/~mis/dOvs/jvmspec/ref--33.html
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          // TODO there are some rules to do with choosing the method here that we don't follow
          invokeMethodRef(sf, index, true, params)

        case 0xb8 => // invokestatic
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          invokeMethodRef(sf, index, false, params)

        case 0xb6 => // invokevirtual
          // invoke virtual method on object objectref and puts the result on the stack (might be void); the method is
          // identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          invokeMethodRef(sf, index, true, params)

        case 0x80 => // ior
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v1.asInt | v2.asInt)
          sf.stack.push(v3)

        case 0x70 => // irem
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val calc = v1 - (v1 / v2) * v2
          val v3 = JVMVarInt(calc)
          sf.stack.push(v3)

        case 0xac => // ireturn
          doReturn()

        case 0x78 => // ishl
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val bottom = v1 & 0x1f // bottom 5 bits
        val v3 = JVMVarInt(v2 << bottom)
          sf.stack.push(v3)

        case 0x7a => // ishr
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val bottom = v1 & 0x1f // bottom 5 bits
        val v3 = JVMVarInt(v2 >> bottom)
          sf.stack.push(v3)

        case 0x36 => // istore
          val v1 = sf.stack.pop()
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          sf.addLocal(index, v1)

        case 0x3b => // istore_0
          store(0)

        case 0x3c => // istore_1
          store(1)

        case 0x3d => // istore_2
          store(2)

        case 0x3e => // istore_3
          store(3)

        case 0x64 => // isub
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v2.asInt - v1.asInt)
          sf.stack.push(v3)

        case 0x7c => // iushr
          JVM.err("Cannot handle opcode iushr yet")
        case 0x82 => // ixor
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val v3 = JVMVarInt(v1 ^ v2)
          sf.stack.push(v3)

        case 0xa8 => // jsr
          JVM.err("Cannot handle opcode jsr yet")
        case 0xc9 => // jsr_w
          JVM.err("Cannot handle opcode jsr_w yet")

        case 0x8a => // l2d
          val value = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarDouble(value.toDouble))

        case 0x89 => // l2f
          val value = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarFloat(value.toFloat))

        case 0x88 => // l2i
          val value = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarInt(value.toInt))

        case 0x61 => // ladd
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v2 + v1))

        case 0x2f => // laload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Long]]
          val value = arrayref(index)
          sf.stack.push(JVMVarLong(value))

        case 0x7f => // land
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v2 & v1))

        case 0x50 => // lastore
          val value = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Long]]
          arrayref(index) = value

        case 0x94 => // lcmp
          val value2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val value1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val toPush: Int = if (value1 > value2) 1
          else if (value1 == value2) 0
          else -1
          sf.stack.push(JVMVarInt(toPush))

        case 0x09 => // lconst_0
          sf.stack.push(JVMVarLong(0))

        case 0x0a => // lconst_1
          sf.stack.push(JVMVarLong(0))

        case 0x12 => // ldc
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val c = cf.getConstant(index)
          c match {
            case v: ConstantFloat   => sf.push(JVMVarFloat(v.value))
            case v: ConstantInteger => sf.push(JVMVarInt(v.value))
            case v: ConstantString  =>
              val str = cf.getString(v.stringIndex)
              sf.push(JVMVarString(str))
            case _                  => JVM.err(s"Can't handle constant ${c} in ldc yet")
          }

        case 0x13 => // ldc_w
          JVM.err("Cannot handle opcode ldc_w yet")
        case 0x14 => // ldc2_w
          JVM.err("Cannot handle opcode ldc2_w yet")

        case 0x6d => // ldiv
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v2 / v1))

        case 0x16 => // lload
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val local = sf.getLocal(index)
          sf.push(local)

        case 0x1e => // lload_0
          load(0)

        case 0x1f => // lload_1
          load(1)

        case 0x20 => // lload_2
          load(2)

        case 0x21 => // lload_3
          load(3)

        case 0x69 => // lmul
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarFloat(v2 * v1))

        case 0x75 => // lneg
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarFloat(-v1))

        case 0xab => // lookupswitch
          JVM.err("Cannot handle opcode lookupswitch yet")
        case 0x81 => // lor
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v2 | v1))

        case 0x71 => // lrem
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v1 + (v1 / v2) * v2))

        case 0xad => // lreturn
          doReturn()

        case 0x79 => // lshl
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val bottom = v1 & 0x1f // bottom 5 bits
        val v3 = JVMVarLong(v2 << bottom)
          sf.stack.push(v3)

        case 0x7b => // lshr
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val bottom = v1 & 0x1f // bottom 5 bits
        val v3 = JVMVarLong(v2 >> bottom)
          sf.stack.push(v3)

        case 0x37 => // lstore
          val v1 = sf.stack.pop()
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          sf.addLocal(index, v1)

        case 0x3f => // lstore_0
          store(0)

        case 0x40 => // lstore_1
          store(1)

        case 0x41 => // lstore_2
          store(2)

        case 0x42 => // lstore_3
          store(3)

        case 0x65 => // lsub
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v2 - v1))

        case 0x7d => // lushr
          JVM.err("Cannot handle opcode lushr yet")

        case 0x83 => // lxor
          val v1 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val v2 = sf.stack.pop().asInstanceOf[JVMVarLong].v
          sf.stack.push(JVMVarLong(v2 ^ v1))

        case 0xc2 => // monitorenter
          JVM.err("Cannot handle opcode monitorenter yet")

        case 0xc3 => // monitorexit
          JVM.err("Cannot handle opcode monitorexit yet")

        case 0xc5 => // multianewarray
          JVM.err("Cannot handle opcode multianewarray yet")

        case 0xbb => // new
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val cls = cf.getConstant(index).asInstanceOf[ConstantClass]
          createNewInstance(sf, cls, classLoader, systemClassLoader, params) match {
            case Some(newInstance) => sf.stack.push(newInstance)
            case _                 =>
          }

        case 0xbc => // newarray
          val atype = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val count = popInt()

          // Scala doesn't let us create primitive arrays, but will compile to that where possible.  Avoid Scala collection
          // methods to avoid boxing.
          val array = atype match {
            case 4  => new Array[Boolean](count)
            case 5  => new Array[Char](count)
            case 6  => new Array[Float](count)
            case 7  => new Array[Double](count)
            case 8  => new Array[Byte](count)
            case 9  => new Array[Short](count)
            case 10 => new Array[Int](count)
            case 11 => new Array[Long](count)
          }

          sf.push(JVMVarObjectRefUnmanaged(array))

        case 0x00 => // nop
        // Our work here is done

        case 0x57 => // pop
          sf.stack.pop()

        case 0x58 => // pop2
          if (sf.stack.length >= 2) {
            sf.stack.pop()
            sf.stack.pop()
          }
          else {
            sf.stack.pop()
          }

        case 0xb5 => // putfield
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          putField(sf, index, true, params, context)

        case 0xb3 => // putstatic
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          putField(sf, index, false, params, context)

        case 0xa9 => // ret
          JVM.err("Cannot handle opcode ret yet")

        case 0xb1 => // return
          if (params.onReturn.isDefined) params.onReturn.get(sf)
          done = true

        case 0x35 => // saload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Short]]
          val value = arrayref(index)
          sf.stack.push(JVMVarInt(value))

        case 0x56 => // sastore
          val value = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Short]]
          arrayref(index) = value.toShort

        case 0x11 => // sipush
          sf.stack.push(op.args.head)

        case 0x5f => // swap
          JVM.err("Cannot handle opcode swap yet")

        case 0xaa => // tableswitch
          JVM.err("Cannot handle opcode tableswitch yet")

        case _ =>
          JVM.err(s"Cannot yet handle opcode ${op}")
      }

      if (incOpCode) {
        opcodeAddress += op.oc.lengthInBytes
        opcodeIdx += 1
      }
    }

    out
  }

  private[jvm] def createEmptyStackFrame(cls: JVMClassFile, functionName: String, parms: ExecuteParams): Unit

  = {
    cls.getMethod(functionName) match {
      case Some(method) =>
        val code = method.getCode().codeOrig
        val sf = new StackFrame(cls, functionName, cls.getString(method.descriptorIndex), null)
        executeFrame(sf, code, parms)
      case _            => JVM.err(s"Unable to find method $functionName in class ${cls.className}")
    }
  }

  def execute(className: String, functionName: String, params: ExecuteParams = ExecuteParams()): Unit = {
    classLoader.loadClass(className, this, params) match {
      case Some(clsFound) =>
        createEmptyStackFrame(clsFound, functionName, params)
      case _              => JVM.err(s"Unable to find class $className")
    }
  }
}

object JVM {
  def err(message: String): Unit = {
    println("Error: " + message)
    throw new InternalError(message)
  }

  //    def unsupported(err: String) = JVMGenUnsupportedCurrently(err)

  def err(sf: StackFrame, message: String): Unit = {
    println(s"Error ${sf.cf.className}.${sf.methodName}: " + message)
    throw new InternalError(message)
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("usage: program <managed class path> <function>")
    }
    else {
      val managedClassPath = args(0).split(";")

      val name = args(1)

      println(s"Managed classpath (${managedClassPath.size} paths): ${managedClassPath.mkString(";")}")

      val classLoader = new JVMClassLoader(managedClassPath, JVMClassLoaderParams(verbose = true, ReadParams(verbose = false)))
      val systemClassLoader = ClassLoader.getSystemClassLoader

      val jvm = new JVM(classLoader, systemClassLoader)

      jvm.execute(name, "main", ExecuteParams(ui = new UIStdOut()))
    }
  }
}


