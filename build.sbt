import spray.revolver.RevolverPlugin.Revolver

name := "crawler"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
libraryDependencies += "com.github.scala-incubator.io" % "scala-io-file_2.11" % "0.4.3-1"
libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4-M1"

javaOptions := Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=n,suspend=y,address=5005")

seq(Revolver.settings: _*)