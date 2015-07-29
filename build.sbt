import Benchmark._ // see project/Benchmark.scala
import Dependencies._ // see project/Dependencies.scala

val buildVersion = "0.10.2-SNAPSHOT"

def commonSettings = Seq(
  version in ThisBuild := buildVersion,
  scalaVersion := Util.buildScalaVersion,
  organization in ThisBuild := "org.scala-lang.modules",
  organizationName in ThisBuild := "LAMP/EPFL",
  organizationHomepage in ThisBuild := Some(url("http://lamp.epfl.ch")),
  homepage in ThisBuild := Some(url("https://github.com/scala/pickling")),
  licenses in ThisBuild := List("BSD-like" -> url("http://www.scala-lang.org/downloads/license.html")),
  scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/scala/pickling"), "git@github.com:scala/pickling.git")),
  developers in ThisBuild := List(
    Developer("xeno-by", "Eugene Burmako", "@xeno_by", url("http://github.com/xeno-by")),
    Developer("heathermiller", "Heather Miller", "@heathercmiller", url("http://github.com/heathermiller")),
    Developer("phaller", "Philipp Haller", "@philippkhaller", url("http://github.com/phaller")),
    Developer("havocp", "Havoc Pennington", "@havocp", url("https://github.com/havocp")),
    Developer("eed3si9n", "Eugene Yokota", "@eed3si9n", url("https://github.com/eed3si9n")),
    Developer("jsuereth", "Josh Suereth", "@jsuereth", url("https://github.com/jsuereth"))
  ),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  scalacOptions ++= Seq("-feature"),
  parallelExecution in Test := false, // hello, reflection sync!!
  publishMavenStyle in ThisBuild := true,
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomIncludeRepository := { x => false },
  pomExtra := <inceptionYear>2013</inceptionYear>,
  credentials ++= Util.loadCredentials()
)
def noPublish = Seq(
  publish := {},
  publishLocal := {}
)

// Use root project
lazy val root: Project = (project in file(".")).
  aggregate(core, benchmark, sandbox).
  settings(commonSettings ++ noPublish: _*).
  settings(
    name := "Scala Pickling",
    run in Compile := (run in (sandbox, Compile)).evaluated
  )

/** Scala Pickling code */
lazy val core: Project = (project in file("core")).
  dependsOn(testUtil % "test->test").
  settings(commonSettings: _*).
  settings(
    name := "scala-pickling",
    libraryDependencies ++= {
      val baseDeps = Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value, // for ToolBox 
        scalaTest % Test,
        scalaCheck % Test
      )
      val additional = CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq(parserCombinators)
        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          Seq(compilerPlugin(macroParadise), quasiquotes)
      }
      baseDeps ++ additional
    }
  )

lazy val testUtil: Project = (project in file("test-util")).
  settings(commonSettings ++ noPublish: _*)

lazy val coreJS: Project = (project in file("core-js")).
  settings(commonSettings: _*).
  settings(
    name := "scala-pickling-js",
    scalaJSStage in Global := FastOptStage, // run on Node.js
    libraryDependencies ++= {
      val baseDeps = Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value, // for ToolBox
        "org.scalatest" %%% "scalatest" % "3.0.0-M7" % Test,
        "org.scalacheck" %%% "scalacheck" % "1.12.4" % Test
      )
      val additional = CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq("org.scala-js" %%% "scala-parser-combinators" % "1.0.2")
        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          Seq(compilerPlugin(macroParadise), quasiquotes)
      }
      baseDeps ++ additional
    }
  ).enablePlugins(ScalaJSPlugin)

lazy val testCoreJS: Project = (project in file("test-core-js")).
  dependsOn(coreJS).
  settings(
    scalaVersion := Util.buildScalaVersion,
    name := "test-scala-pickling-js",
    scalacOptions ++= Seq("-Xlog-implicits"),
    libraryDependencies ++= {
      val baseDeps = Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value // for ToolBox
      )
      val additional = CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq("org.scala-js" %%% "scala-parser-combinators" % "1.0.2")
        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          Seq(compilerPlugin(macroParadise), quasiquotes)
      }
      baseDeps ++ additional
    }
  ).enablePlugins(ScalaJSPlugin)

lazy val sandbox: Project = (project in file("sandbox")).
  dependsOn(core).
  settings(commonSettings ++ noPublish: _*).
  settings(
    sourceDirectory in Test := baseDirectory.value,
    libraryDependencies += scalaTest,
    // scalacOptions ++= Seq()
    scalacOptions ++= Seq("-Xlog-implicits")
    // scalacOptions ++= Seq("-Xprint:typer")
  )

lazy val benchmark: Project = (project in file("benchmark")).
  dependsOn(core).
  settings(commonSettings ++ noPublish ++ benchmarkSettings: _*).
  settings(
    scalacOptions ++= Seq("-optimise"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      kryoSerializers, kryo)
  )
