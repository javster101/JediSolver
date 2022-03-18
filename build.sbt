ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.lwjgl" % "lwjgl" % "3.3.1-SNAPSHOT"
libraryDependencies += "org.lwjgl" % "lwjgl-opencl" % "3.3.1-SNAPSHOT"
libraryDependencies += "org.lwjgl" % "lwjgl" % "3.3.1-SNAPSHOT" classifier "natives-linux"
lazy val root = (project in file("."))
  .settings(
    name := "JediSolver"
  )


