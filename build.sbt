libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"
libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.4"
libraryDependencies += "com.jfoenix" % "jfoenix" % "9.0.1"
// https://mvnrepository.com/artifact/net.sf.jopt-simple/jopt-simple
libraryDependencies += "net.sf.jopt-simple" % "jopt-simple" % "5.0.4"

exportJars := true


lazy val JOakClassFiles = ProjectRef(file("../JOakClassFiles"), "JOakClassFiles")

lazy val JOakJVM = (project in file("."))
  .settings(
    name := "JOakJVM",
    scalaVersion := "2.12.4",
    test in assembly := {}
  ).dependsOn(JOakClassFiles)

