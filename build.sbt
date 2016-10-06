import sbt.Resolver

lazy val root = project.in(file(".")).
  aggregate(codeOffJS, codeOffJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val commonSettings = Seq(
  version := "0.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:higherKinds",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-unused-import",
    "-Ywarn-dead-code",
    "-Xfuture"
  )
)

lazy val codeOff = crossProject.in(file("codeOff"))
  .settings(commonSettings: _*)
  .settings(
    name := "code-off",
    selectMainClass in Compile := Defaults.askForMainClass((discoveredMainClasses in Compile).value.sortBy(x =>
      """\d+""".r.findFirstIn(x).map(_.toInt))),
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % "0.5.1",
      "io.circe" %%% "circe-generic" % "0.5.1",
      "io.circe" %%% "circe-parser" % "0.5.1",
      "org.spire-math" %%% "spire" % "0.12.0",
      "io.monix" %%% "monix" % "2.0.2",
      "org.scalaz" %%% "scalaz-core" % "7.2.6",
      "com.lihaoyi" %%% "fastparse" % "0.4.1",
      "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test"
    ),
    testFrameworks += new TestFramework("scalacheck.ScalaCheckFramework")
  )
  .jvmSettings(
    libraryDependencies ++= Seq()
  )
  .jsSettings(
    scalaJSUseRhino in Global := false,
    scalaJSStage in Global := FullOptStage,
    persistLauncher := false,
    libraryDependencies ++= Seq()
  )

lazy val codeOffJVM = codeOff.jvm.settings(name := "codeOffJVM")
lazy val codeOffJS = codeOff.js.settings(name := "codeOffJS")

onLoad in Global := (Command.process("project codeOffJVM", _: State)) compose (onLoad in Global).value