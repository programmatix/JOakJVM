libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"

exportJars := true


lazy val JOakClassFiles = ProjectRef(file("../JOakClassFiles"), "JOakClassFiles")

lazy val JOakJVM = (project in file("."))
  .settings(
    name := "JOakJVM",
    scalaVersion := "2.12.4",
    test in assembly := {}
  ).dependsOn(JOakClassFiles)

