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
//    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-unused-import",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
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
      //"oncue.quiver" %% "core" % "5.3.57",
      "org.scalaz" %%% "scalaz-core" % "7.2.6",
      "org.typelevel" %%% "cats-core" % "0.7.2",
      "org.typelevel" %%% "cats-free" % "0.7.2",
      "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test"
    ),
    testFrameworks += new TestFramework("scalacheck.ScalaCheckFramework")
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "0.9.0"/*,
      "co.fs2" %% "fs2-io" % "0.9.0" */
    )
  )
  .jsSettings(
    scalaJSUseRhino in Global := false,
    scalaJSStage in Global := FullOptStage,
    persistLauncher := false,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.0"/*,
      "be.doeraene" %%% "scalajs-jquery" % "0.9.0" */
    )/*,
    jsDependencies ++= Seq(
      //RuntimeDOM
      "org.webjars" % "jquery" % "3.1.0" / "3.1.0/jquery.js"
    )*/
  )

lazy val codeOffJVM = codeOff.jvm.settings(name := "codeOffJVM")
lazy val codeOffJS = codeOff.js.settings(name := "codeOffJS")

onLoad in Global := (Command.process("project codeOffJVM", _: State)) compose (onLoad in Global).value