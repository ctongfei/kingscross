name := "kingscross"
organization := "me.tongfei"
version := "0.1.0"
scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
