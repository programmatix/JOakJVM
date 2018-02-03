# JOak JVM
A Java Virtual Machine, written in Scala.  

Though it's a learning exercise and not suitable for production purposes, it is a nice demo of how simple a working JVM can be.  

## Implementation
It's a regular Scala program, that reads in a JVM .class file, and executes the JVM opcodes inside.  It handles the stack frames, operand stack, and local variable management.  

Since it's a JVM program itself, it can use the 'real' JVM its running on to handle some complex operations, including:

* Memory management.
* Loading and execution of core Java libraries, like String.
* Threading.

Yes this is a bit of a cop-out and skips some of the hardest parts of a real JVM (especially memory management), but this is a toy JVM, intended to be a learning exercise.  A real optimised and fully-functional JVM would take at least man-months of effort. 

## Classpaths
If a method is called it can involve reading in additional .class files.  JOak JVM has its own custom class-loader which attempts to find and load the .class file, and if this fails it uses the JVM system classloader.  

When you run JOak JVM you can specify the JVM classpath as normal, along with JOak JVM's classpath as an argument.  This lets you choose which classes will be executed by JOak JVM, and which by the 'real' JVM.  

I use the term 'managed' to refer to code and classes handled by the JOak JVM, versus unmanaged which is handled by the standard Java JVM it's running upon.

## Limitations
In addition to those mentioned above, the following features aren't supported: 

* .class files compiled with Java 7+ aren't officially supported - they may or may not work, depending on which features they use.  Anything compiled to Java language level 6 or below should be fine. 
* Jar files.
* synchronized keyword.
* native keyword.
* Passing and return 'managed' types.
* try/catch/finally.

## Usage
It's recommended to use the [JOakUmbrella](https://github.com/progammatix/JOakUmrella) project, which includes this project and any dependencies.

* Clone [JOakUmbrella](https://github.com/progammatix/JOakUmrella) into <some_dir>
* ```cd <some_dir>```
* ```git submodule update --init --recursive``` to grab all required dependencies.
* ```cd JOakJVM```
* ```sbt assembly``` to produce a fat JAR with all dependencies.
* ```java -jar target/scala-2.12/JOakJVM-assembly-0.1-SNAPSHOT.jar target/scala-2.12/classes jvm.JVM``` to run JOak JVM on its own .class files.

