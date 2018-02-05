# Debugging Tips

Seeing the opcodes:

```
javap -verbose src/test/java/PassArrayListToCollection.class
```

Running from the CLI:

```
java -cp "C:\Users\Graham\.ivy2\cache\org.scala-lang\scala-library\jars\scala-library-2.12.4.jar;target/scala-2.12/classes;C:\Users\Graham\.ivy2\cache\com.lihaoyi\fastparse_2.12\jars\fastparse_2.12-1.0.0.jar;C:\Users\Graham\.ivy2\cache\c om.lihaoyi\fastparse-utils_2.12\jars\fastparse-utils_2.12-1.0.0.jar;C:\Users\Graham\.ivy2\cache\com.lihaoyi\sourcecode_2.12\bundles\sourcecode_2.12-0.1.4.jar;C:/Users/Graham/.m2/repository/net/sf/jopt-simple/jopt-simple/4.5/jopt-simple-4.5 .jar;target/scala-2.12/classes;../JOakClassFiles/target/scala-2.12/classes/" jvm.JVM ../../MapReduceGame/Zuul/out/production/classes com.grahampople.ZuulGateway
```