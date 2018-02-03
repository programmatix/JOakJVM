package jvm

import jvm.JVMTypesInternal.JVMTypeObjectStr
import jvmclass.JVMTypes._

// The params a method takes and the return value
object JVMMethodDescriptors {
  import fastparse.all._

  private[jvm] val baseType: P[JVMType] = P("B").map(v => JVMTypeByte()) |
    P("C").map(v => JVMTypeChar()) |
    P("D").map(v => JVMTypeDouble()) |
    P("F").map(v => JVMTypeFloat()) |
    P("I").map(v => JVMTypeInt()) |
    P("J").map(v => JVMTypeLong()) |
    P("S").map(v => JVMTypeShort()) |
    P("Z").map(v => JVMTypeBoolean())
  private[jvm] lazy val objectType: P[JVMType] = P(P("L") ~ (CharIn('0'to'9') | CharIn('a'to'z') | CharIn('A'to'Z') | P("/")).rep.! ~ P(";")).map(v => JVMTypeObjectStr(v))
  private[jvm] lazy val fieldType: P[JVMType] = baseType | objectType | arrayType
  private[jvm] lazy val componentType: P[JVMType] = fieldType
  private[jvm] lazy val arrayType: P[JVMType] = P(P("[") ~ componentType).map(v => JVMTypeArray(v))
  private[jvm] val returnDescriptor: P[JVMType] = fieldType | P("V").map(v => JVMTypeVoid())
  private[jvm] val parameterDescriptor: P[JVMType] = fieldType
  private[jvm] val methodDescriptor: P[MethodDescriptor] = (P("(") ~ parameterDescriptor.rep(0).map(_.toList) ~ P(")") ~ returnDescriptor).
    map(v => MethodDescriptor(v._2, v._1))

  case class MethodDescriptor(ret: JVMType, args: Seq[JVMType])

  // (Ljava/lang/String;)V -> (JVMTypeVoid, JVMTypeClass(classOf[java.lang.String]))
  def methodDescriptorToTypes(in: String): MethodDescriptor = {
    (methodDescriptor ~ End).parse(in) match {
      case Parsed.Success(v, x) => v
      case _ =>
        assert(false, s"Could not parse method descriptor $in")
        null
    }
  }

  // java/lang/String; -> JVMTypeClass(classOf[java.lang.String])
  def fieldDescriptorToTypes(in: String): JVMType = {
    (parameterDescriptor ~ End).parse(in) match {
      case Parsed.Success(v, x) => v
      case _ =>
        assert(false, s"Could not parse field descriptor $in")
        null
    }
  }

  def createMethodDescriptor(ret: JVMType, params: Seq[JVMType]): String = {
    s"(${params.map(typesToStr).mkString("")})${typesToStr(ret)}"
  }

  private def typesToStr(typ: JVMType): String = {
    typ match {
      case v: JVMTypeVoid    => "V"
      case v: JVMTypeChar    => "C"
      case v: JVMTypeShort   => "S"
      case v: JVMTypeInt     => "I"
      case v: JVMTypeLong    => "J"
      case v: JVMTypeFloat   => "F"
      case v: JVMTypeDouble  => "D"
      case v: JVMTypeBoolean => "Z"
      case v: JVMTypeArray   => "[" + typesToStr(v.typ)
      case v: JVMTypeString  => "Ljava/lang/String;"
      case _                 =>
        JVM.err(s"unhandled type ${typ}")
        null
    }
  }


}

