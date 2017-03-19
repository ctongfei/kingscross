import sbt.Keys._

name := "kingscross"

organization := "me.tongfei"
version := "0.1.0-SNAPSHOT"
isSnapshot := true
scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.11.8", "2.12.1")

libraryDependencies += "org.scala-lang"  % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest"  %% "scalatest"     % "3.0.1"             % Test

unmanagedBase := baseDirectory.value / "lib"

publishMavenStyle := true
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
publishArtifact in Test := false
pomExtra :=
  <url>http://github.com/ctongfei/kingscross</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:ctongfei/kingscross.git</url>
      <connection>scm:git:git@github.com:ctongfei/kingscross.git</connection>
    </scm>
    <developers>
      <developer>
        <id>ctongfei</id>
        <name>Tongfei Chen</name>
        <url>http://tongfei.me/</url>
      </developer>
    </developers>
