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
    "-Xfatal-warnings",
    "-Yno-adapted-args",
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
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "0.7.2",
      "org.typelevel" %%% "cats-free" % "0.7.2",
      "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test"
    ),
    testFrameworks += new TestFramework("scalacheck.ScalaCheckFramework")
  )
  .jsSettings(
    scalaJSUseRhino in Global := false,
    scalaJSStage in Global := FullOptStage,
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