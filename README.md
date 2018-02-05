# JOak JVM
A Java Virtual Machine, written in Scala.  

Though it's a learning exercise and not suitable for production purposes, it is a nice demo of how simple a working JVM can be.  

## Implementation
It's a regular Scala program, that reads in a JVM .class file, and interprets and executes the JVM opcodes inside.  It handles the stack frames, operand stack, and local variable management.  

Since it's a JVM program itself, it can use the 'real' JVM it's running on to handle some of the more complex operations, including:

* Memory management.
* Loading and execution of core Java libraries, like String.

Yes this is a bit of a cop-out and skips some of the hardest parts of a real JVM (especially memory management), but this is a toy JVM, intended to be a learning exercise.  A real optimised and fully-functional JVM would take many man-months more effort. 

There's no compiling, AOT or JIT or otherwise: it simply interprets the opcodes as they come.  So it's probably too slow for production use.

## Classpaths
If a method is called it can involve reading in additional .class files.  JOak JVM has its own custom class-loader which attempts to find and load the .class file, and if it can't find a suitable .class file on its classpath, it tries with the JVM system classloader.  

When you run JOak JVM you can specify the JVM classpath as normal, along with JOak JVM's classpath as an argument.  This lets you choose which classes will be executed by JOak JVM, and which by the 'real' JVM.  

I use the term 'managed' to refer to code and classes handled by the JOak JVM, versus unmanaged which is handled by the standard Java JVM it's running upon.

JOak JVM can handle jar files, just add them to the managed classpath using the same syntax as with the standard java executable.

## Limitations
The following features aren't supported: 

* synchronized keyword.
* Threading in general.
* native keyword.
* Passing and return 'managed' types.
* try/catch/finally.
* .class files compiled with Java 7+ aren't officially supported - they may or may not work, depending on which features they use.  Anything compiled to Java language level 6 or below should be fine. 

## Usage
It's recommended to use the [JOakUmbrella](https://github.com/progammatix/JOakUmrella) project, which includes this project and any dependencies.

* Clone [JOakUmbrella](https://github.com/progammatix/JOakUmrella) into <some_dir>
* ```cd <some_dir>```
* ```git submodule update --init --recursive``` to grab all required dependencies.
* ```cd JOakJVM```
* ```sbt assembly``` to produce a fat JAR with all dependencies.

Then execute with 

```
java -cp <Unmanaged Classpath> -jar target/scala-2.12/JOakJVM-assembly-0.1-SNAPSHOT.jar <Managed Classpath> <Class to Execute>
```

<b>Unmanaged Classpath:</b> The standard Java classpath.  Any class files that need to be loaded and cannot be found on the managed classpath, will then be tried with the JVM system classloader using this classpath.

<b>Managed Classpath:</b> JOak JVM's classpath, e.g. any class files you want to be loaded with JOak should be on here.  Semicolon separate just as with the unmanaged classpath.  You can include jar files.

<b>Class to Execute:</b> just as with the normal ```java``` command, e.g. "com.example.MyClass".  MyClass must contain a standard main() method.

For example, to run JOak JVM on itself (JVMception!):

```
java -jar target/scala-2.12/JOakJVM-assembly-0.1-SNAPSHOT.jar target/scala-2.12/classes jvm.JVM
``` 

(No unmanaged classpath is required here, as the jar contains all JOak JVM's dependencies.)

Note this doesn't currently succeed, as JOak JVM is written in Scala and not all required features have been implemented yet.