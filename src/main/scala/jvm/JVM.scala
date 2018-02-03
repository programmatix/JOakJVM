package jvm

import jvm.JVMTypesInternal._
import jvmclass.JVMByteCode._
import jvmclass.JVMClassFile
import jvmclass.JVMClassFileReader.ReadParams
import jvmclass.internal.JVMClassFileReaderUtils
import jvmclass.JVMClassFileTypes._
import jvmclass.JVMTypes._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class StackFrame(val cf: JVMClassFile, val methodName: String) {
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
                          onReturn: Option[(StackFrame) => Unit] = None
                        )

class JVMContext {
  val staticClasses = ArrayBuffer.empty[JVMClassStatic]
}

// A Java Virtual Machine implementation, just for learning purposes
// See 'JVMClassLoader' for a discussion over why two classloaders are provided (rather than standard chaining)
class JVM(classLoader: JVMClassLoader,
          systemClassLoader: ClassLoader = ClassLoader.getSystemClassLoader) {

  private[jvm] val context = new JVMContext

  private[jvm] def putField(sf: StackFrame, index: Int, objectRef: Option[JVMObjectRef], params: ExecuteParams, context: JVMContext): Unit = {
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

    objectRef match {
      case Some(v: JVMVarObjectRefManaged) =>
        v.klass.putField(name, fieldType, value)

      case Some(v: JVMVarObjectRefUnmanaged) =>
        JVM.err(sf, "cannot putfield on non-klas yet")

      case _ =>
        context.staticClasses.find(_.cf.fullName() == className) match {
          case Some(sc) =>
            sc.putField(name, fieldType, value)
          case _        =>
            JVM.err(sf, s"do not have static class for ${sf.cf.className}")
        }
    }
  }

  private[jvm] def getMethodArgsAsObjects(sf: StackFrame, methodTypes: JVMMethodDescriptors.MethodDescriptor, params: ExecuteParams): (Seq[Object], Seq[Class[_]]) = {
    val args = ArrayBuffer.empty[Object]
    val argTypes = ArrayBuffer.empty[Class[_]]

    methodTypes.args.reverse.foreach(arg => {
      val next = sf.pop()
      arg match {
        case v: JVMTypeObjectStr =>
          next match {
            case x: JVMVarString             =>
              args += x.v
              argTypes += classOf[java.lang.String]
            case x: JVMVarObject             =>
              args += x.o
              argTypes += x.o.getClass
            case x: JVMVarObjectRefUnmanaged =>
              args += x.o
              argTypes += x.o.getClass
            case _                           =>
              JVM.err(sf, s"cannot handle arg ${next} yet")
          }
        case v: JVMTypeVoid      =>
          JVM.err(s"not expecting void here")
          null
        case v: JVMTypeBoolean   =>
          args += next.asInstanceOf[JVMVarBoolean].v.asInstanceOf[Object]
          argTypes += java.lang.Boolean.TYPE
        case v: JVMTypeInt       =>
          args += next.asInstanceOf[JVMVarInt].v.asInstanceOf[Object]
          // Type gives us the class of the primitive
          argTypes += java.lang.Integer.TYPE
        case v: JVMTypeShort     =>
          args += next.asInstanceOf[JVMVarShort].v.asInstanceOf[Object]
          argTypes += java.lang.Short.TYPE
        case v: JVMTypeByte      =>
          args += next.asInstanceOf[JVMVarByte].v.asInstanceOf[Object]
          argTypes += java.lang.Byte.TYPE
        case v: JVMTypeChar      =>
          args += next.asInstanceOf[JVMVarChar].v.asInstanceOf[Object]
          argTypes += java.lang.Character.TYPE
        case v: JVMTypeFloat     =>
          args += next.asInstanceOf[JVMVarFloat].v.asInstanceOf[Object]
          argTypes += java.lang.Float.TYPE
        case v: JVMTypeDouble    =>
          args += next.asInstanceOf[JVMVarDouble].v.asInstanceOf[Object]
          argTypes += java.lang.Double.TYPE
        case v: JVMTypeLong      =>
          args += next.asInstanceOf[JVMVarLong].v.asInstanceOf[Object]
          argTypes += java.lang.Long.TYPE
        case v: JVMTypeObjectStr =>
          // java/util/List
          val className = v.clsRaw.replace('/', '.')

          classLoader.loadClass(className, this, params) match {
            case Some(clsRef) =>
              JVM.err(sf, "can't handle passing around managed types")

            case _ =>
              val raw = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
              val clsRef = ClassLoader.getSystemClassLoader.loadClass(className)
              args += raw
              argTypes += clsRef
          }

        case v: JVMTypeArray =>
          v.typ match {
            //            case _: JVMTypeVoid      =>
            //              args += array.asInstanceOf[Array[Void]]
            //              argTypes += classOf[Array[Void]]
            //            case _: JVMTypeBoolean   =>
            //              args += array.asInstanceOf[Array[java.lang.Boolean]]
            //              argTypes += classOf[Array[java.lang.Boolean]]
            //            case _: JVMTypeInt       =>
            //              args += array.asInstanceOf[Array[java.lang.Integer]]
            //              argTypes += classOf[Array[java.lang.Integer]]
            //            case _: JVMTypeShort     =>
            //              args += array.asInstanceOf[Array[java.lang.Short]]
            //              argTypes += classOf[Array[java.lang.Short]]
            //            case _: JVMTypeChar      =>
            //              args += array.asInstanceOf[Array[java.lang.Character]]
            //              argTypes += classOf[Array[java.lang.Character]]
            //            case _: JVMTypeByte      =>
            //              args += array
            //              argTypes += array.getClass
            //            case _: JVMTypeFloat     =>
            //              args += array.asInstanceOf[Array[java.lang.Float]]
            //              argTypes += classOf[Array[java.lang.Float]]
            //            case _: JVMTypeDouble    =>
            //              args += array.asInstanceOf[Array[java.lang.Double]]
            //              argTypes += classOf[Array[java.lang.Double]]
            //            case _: JVMTypeLong      =>
            //              args += array.asInstanceOf[Array[java.lang.Long]]
            //              argTypes += classOf[Array[java.lang.Long]]
            //            case _: JVMTypeString    =>
            //              args += array.asInstanceOf[Array[java.lang.String]]
            //              argTypes += classOf[Array[java.lang.String]]
            case v: JVMTypeObjectStr =>
              val array = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
              // java/util/List
              val className = v.clsRaw.replace('/', '.')

              classLoader.loadClass(className, this, params) match {
                case Some(clsRef) =>
                  JVM.err(sf, "can't handle passing around managed types")

                case _ =>
                  val clsRef = ClassLoader.getSystemClassLoader.loadClass(className)
                  args += array
                  argTypes += classOf[Array[Object]]
              }
            case _                   =>
              val array = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
              args += array
              argTypes += array.getClass

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

  private def invokeMethodRef(sf: StackFrame, index: Int, getObjectRef: Boolean, params: ExecuteParams): Unit

  = {
    val cf = sf.cf
    val fieldRef = cf.getConstant(index).asInstanceOf[ConstantMethodref]
    val cls = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
    //  java/io/PrintStream
    val className = cf.getString(cls.nameIndex)

    val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]
    // println
    val methodName = cf.getString(nameAndType.nameIndex)
    // (Ljava/lang/String;)V
    val methodDescriptor = cf.getString(nameAndType.descriptorIndex)

    val methodTypes = JVMMethodDescriptors.methodDescriptorToTypes(methodDescriptor)

    val resolvedClassName = className.replace("/", ".")

    // See JVMClassLoader for a description of what's going on here
    classLoader.loadClass(resolvedClassName, this, params) match {

      case Some(clsRef) =>
        clsRef.getMethod(methodName) match {
          case Some(method) =>
            val code = method.getCode().codeOrig
            val sfNew = new StackFrame(clsRef, methodName)

            // This pops from the stack
            val argsRaw = JVMStackFrame.getMethodArgs(sf, methodTypes)

            val objectRef = if (getObjectRef) {
              sf.stack.pop() match {
                case v: JVMVarObject           => v.o
                case v: JVMVarObjectRefManaged => v.klass
              }
            }
            else null

            val args: Seq[JVMVar] = if (methodName == "<init>") {
              // TODO can't find in spec, but this pointer is definitely passed too
              argsRaw :+ JVMVarObjectRefManaged(objectRef.asInstanceOf[JVMClassInstance])
            }
            else argsRaw

            sfNew.locals ++= args.zipWithIndex.map(arg => arg._2 -> arg._1).toMap

            executeFrame(sfNew, code, params) match {
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
              case v: JVMVarObject           => v.o
              case v: JVMVarString           => v.v
              case v: JVMVarNewInstanceToken => v.created.get
            }
          }
          else null

          val methodRef = clsRef.getMethod(methodName, argTypes: _*)

          val result = methodRef.invoke(objectRef, args: _*) // :_* is the hint to expand the Seq to varargs

          result match {
            case null =>
            case _    =>
              sf.push(JVMVarObjectRefUnmanaged(result))
          }
        }
    }

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
      op.oc.hexcode match {
        case 0x32 => // aaload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Object]]
          val value = arrayref(index)
          sf.stack.push(JVMVarObjectRefUnmanaged(value))

        case 0x53 => // aastore
          val value: Object = sf.stack.pop() match {
            case v: JVMVarObjectRefUnmanaged => v.o
            case v: JVMVarString             => v.v
          }
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Object]]
          arrayref(index) = value

        case 0x01 => // aconst_null
          JVM.err("Cannot handle opcode aconst_null yet")
        case 0x19 => // aload
          val index = sf.stack.head.asInstanceOf[JVMVarInteger].asInt
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
          val index = sf.stack.head.asInstanceOf[JVMVarInteger].asInt
          val local = sf.stack.pop().asInstanceOf[JVMObjectRef]
          sf.addLocal(index, local)

        case 0x4b => // astore_0
          val local = sf.stack.pop().asInstanceOf[JVMObjectRef]
          sf.addLocal(0, local)

        case 0x4c => // astore_1
          val local = sf.stack.pop().asInstanceOf[JVMObjectRef]
          sf.addLocal(1, local)

        case 0x4d => // astore_2
          val local = sf.stack.pop().asInstanceOf[JVMObjectRef]
          sf.addLocal(2, local)

        case 0x4e => // astore_3
          val local = sf.stack.pop().asInstanceOf[JVMObjectRef]
          sf.addLocal(3, local)

        case 0xbf => // athrow
          JVM.err("Cannot handle opcode athrow yet")
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
          JVM.err("Cannot handle opcode breakpoint yet")
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
          JVM.err("Cannot handle opcode checkcast yet")
        case 0x90 => // d2f
          JVM.err("Cannot handle opcode d2f yet")
        case 0x8e => // d2i
          JVM.err("Cannot handle opcode d2i yet")
        case 0x8f => // d2l
          JVM.err("Cannot handle opcode d2l yet")
        case 0x63 => // dadd
          JVM.err("Cannot handle opcode dadd yet")
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
          JVM.err("Cannot handle opcode dcmpg yet")
        case 0x97 => // dcmpl
          JVM.err("Cannot handle opcode dcmpl yet")
        case 0x0e => // dconst_0
          JVM.err("Cannot handle opcode dconst_0 yet")
        case 0x0f => // dconst_1
          JVM.err("Cannot handle opcode dconst_1 yet")
        case 0x6f => // ddiv
          JVM.err("Cannot handle opcode ddiv yet")
        case 0x18 => // dload
          JVM.err("Cannot handle opcode dload yet")
        case 0x26 => // dload_0
          JVM.err("Cannot handle opcode dload_0 yet")
        case 0x27 => // dload_1
          JVM.err("Cannot handle opcode dload_1 yet")
        case 0x28 => // dload_2
          JVM.err("Cannot handle opcode dload_2 yet")
        case 0x29 => // dload_3
          JVM.err("Cannot handle opcode dload_3 yet")
        case 0x6b => // dmul
          JVM.err("Cannot handle opcode dmul yet")
        case 0x77 => // dneg
          JVM.err("Cannot handle opcode dneg yet")
        case 0x73 => // drem
          JVM.err("Cannot handle opcode drem yet")
        case 0xaf => // dreturn
          doReturn()

        case 0x39 => // dstore
          JVM.err("Cannot handle opcode dstore yet")
        case 0x47 => // dstore_0
          JVM.err("Cannot handle opcode dstore_0 yet")
        case 0x48 => // dstore_1
          JVM.err("Cannot handle opcode dstore_1 yet")
        case 0x49 => // dstore_2
          JVM.err("Cannot handle opcode dstore_2 yet")
        case 0x4a => // dstore_3
          JVM.err("Cannot handle opcode dstore_3 yet")
        case 0x67 => // dsub
          JVM.err("Cannot handle opcode dsub yet")
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
          JVM.err("Cannot handle opcode f2i yet")
        case 0x8c => // f2l
          JVM.err("Cannot handle opcode f2l yet")
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
          JVM.err("Cannot handle opcode fcmpg yet")
        case 0x95 => // fcmpl
          JVM.err("Cannot handle opcode fcmpl yet")

        case 0x0b => // fconst_0
          sf.stack.push(JVMVarFloat(0))

        case 0x0c => // fconst_1
          sf.stack.push(JVMVarFloat(1))

        case 0x0d => // fconst_2
          sf.stack.push(JVMVarFloat(2))

        case 0x6e => // fdiv
          JVM.err("Cannot handle opcode fdiv yet")

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
          JVM.err("Cannot handle opcode fmul yet")
        case 0x76 => // fneg
          JVM.err("Cannot handle opcode fneg yet")
        case 0x72 => // frem
          JVM.err("Cannot handle opcode frem yet")
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
          JVM.err("Cannot handle opcode fsub yet")
        case 0xb4 => // getfield
          JVM.err("Cannot handle opcode getfield yet")
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
              sf.stack.push(JVMVarObject(fieldInstance))
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
          JVM.err("Cannot handle opcode i2d yet")
        case 0x86 => // i2f
          JVM.err("Cannot handle opcode i2f yet")
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
          JVM.err("Cannot handle opcode if_acmpeq yet")
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
          JVM.err("Cannot handle opcode ifeq yet")
        case 0x9c => // ifge
          JVM.err("Cannot handle opcode ifge yet")
        case 0x9d => // ifgt
          JVM.err("Cannot handle opcode ifgt yet")
        case 0x9e => // ifle
          JVM.err("Cannot handle opcode ifle yet")
        case 0x9b => // iflt
          JVM.err("Cannot handle opcode iflt yet")
        case 0x9a => // ifne
          JVM.err("Cannot handle opcode ifne yet")
        case 0xc7 => // ifnonnull
          JVM.err("Cannot handle opcode ifnonnull yet")
        case 0xc6 => // ifnull
          JVM.err("Cannot handle opcode ifnull yet")
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
          JVM.err("Cannot handle opcode invokeinterface yet")
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
          JVM.err("Cannot handle opcode l2d yet")
        case 0x89 => // l2f
          JVM.err("Cannot handle opcode l2f yet")
        case 0x88 => // l2i
          JVM.err("Cannot handle opcode l2i yet")
        case 0x61 => // ladd
          JVM.err("Cannot handle opcode ladd yet")
        case 0x2f => // laload
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Long]]
          val value = arrayref(index)
          sf.stack.push(JVMVarLong(value))

        case 0x7f => // land
          JVM.err("Cannot handle opcode land yet")
        case 0x50 => // lastore
          val value = sf.stack.pop().asInstanceOf[JVMVarLong].v
          val index = sf.stack.pop().asInstanceOf[JVMVarInteger].asInt
          val arrayrefRaw = sf.stack.pop().asInstanceOf[JVMVarObjectRefUnmanaged]
          val arrayref = arrayrefRaw.o.asInstanceOf[Array[Long]]
          arrayref(index) = value

        case 0x94 => // lcmp
          JVM.err("Cannot handle opcode lcmp yet")
        case 0x09 => // lconst_0
          JVM.err("Cannot handle opcode lconst_0 yet")
        case 0x0a => // lconst_1
          JVM.err("Cannot handle opcode lconst_1 yet")
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
          JVM.err("Cannot handle opcode ldiv yet")
        case 0x16 => // lload
          JVM.err("Cannot handle opcode lload yet")
        case 0x1e => // lload_0
          JVM.err("Cannot handle opcode lload_0 yet")
        case 0x1f => // lload_1
          JVM.err("Cannot handle opcode lload_1 yet")
        case 0x20 => // lload_2
          JVM.err("Cannot handle opcode lload_2 yet")
        case 0x21 => // lload_3
          JVM.err("Cannot handle opcode lload_3 yet")
        case 0x69 => // lmul
          JVM.err("Cannot handle opcode lmul yet")
        case 0x75 => // lneg
          JVM.err("Cannot handle opcode lneg yet")
        case 0xab => // lookupswitch
          JVM.err("Cannot handle opcode lookupswitch yet")
        case 0x81 => // lor
          JVM.err("Cannot handle opcode lor yet")
        case 0x71 => // lrem
          JVM.err("Cannot handle opcode lrem yet")
        case 0xad => // lreturn
          doReturn()

        case 0x79 => // lshl
          JVM.err("Cannot handle opcode lshl yet")
        case 0x7b => // lshr
          JVM.err("Cannot handle opcode lshr yet")
        case 0x37 => // lstore
          JVM.err("Cannot handle opcode lstore yet")
        case 0x3f => // lstore_0
          JVM.err("Cannot handle opcode lstore_0 yet")
        case 0x40 => // lstore_1
          JVM.err("Cannot handle opcode lstore_1 yet")
        case 0x41 => // lstore_2
          JVM.err("Cannot handle opcode lstore_2 yet")
        case 0x42 => // lstore_3
          JVM.err("Cannot handle opcode lstore_3 yet")
        case 0x65 => // lsub
          JVM.err("Cannot handle opcode lsub yet")
        case 0x7d => // lushr
          JVM.err("Cannot handle opcode lushr yet")
        case 0x83 => // lxor
          JVM.err("Cannot handle opcode lxor yet")
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
          val objectRef = sf.stack.pop().asInstanceOf[JVMObjectRef]
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          putField(sf, index, Some(objectRef), params, context)

        case 0xb3 => // putstatic
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          putField(sf, index, None, params, context)

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
        val sf = new StackFrame(cls, functionName)
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
    println(args.mkString(","))
    if (args.length != 2) {
      println("usage: program <managed class path> <function>")
    }
    else {
      val managedClassPath = args(0).split(";")

      val name = args(1)

      val classLoader = new JVMClassLoader(managedClassPath, JVMClassLoaderParams(true, ReadParams(true)))
      val systemClassLoader = ClassLoader.getSystemClassLoader

      val jvm = new JVM(classLoader, systemClassLoader)

      jvm.execute(name, "main", ExecuteParams())
    }
  }
}


