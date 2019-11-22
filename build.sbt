lazy val projectSettings = Seq(
  organization := "birchmd",
  scalaVersion := "2.13.1",
  version := "0.1.0-SNAPSHOT",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  scalafmtOnCompile := true,
  testOptions in Test += Tests.Argument("-oD") //output test durations
)

lazy val compilerSettings = CompilerSettings.options

lazy val dag = (project in file("."))
  .settings(projectSettings: _*)
  .settings(CompilerSettings.options)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
      "io.monix"       %% "monix"      % "3.0.0",
      "org.scalatest"  %% "scalatest"  % "3.0.8" % "test"
    )
  )
