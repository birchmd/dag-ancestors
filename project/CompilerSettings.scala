import java.lang.Runtime.getRuntime

import sbt._
import sbt.Keys._

object CompilerSettings {

  private lazy val commonOptions =
    // format: off
    Seq(
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:adapted-args",
      "-Xlint:inaccessible",
      "-Ywarn-value-discard"
    )
    // format: on

  lazy val options = Seq(
    javacOptions ++= Seq("-encoding", "UTF-8"),
    scalacOptions ++= commonOptions,
    scalacOptions in (Compile, console) ~= {
      _.filterNot(
        Set(
          "-Xfatal-warnings",
          "-Ywarn-unused-import",
          "-Ywarn-unused:imports"
        )
      )
    },
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
  )
}
