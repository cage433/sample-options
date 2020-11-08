ThisBuild / scalaVersion := "2.13.3"


// Libraries
val commonsMath3            = "org.apache.commons"    % "commons-math3"               % "3.6.1"

val scalaTest               = "org.scalatest"        %% "scalatest"                   % "3.1.1"      % Test
val enumeratum              = "com.beachape" %% "enumeratum" % "1.5.15"


lazy val example = module("example")
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3, 
      enumeratum,
      scalaTest
    ),

  )


def module(id: String): Project = {
  val base = file(id)

  Project(id = id, base = base)
    /*.settings(Defaults.coreDefaultSettings)*/
    .settings(
      Compile / scalaSource := baseDirectory.value / "src",
      Test / scalaSource := baseDirectory.value / "tests",
      /*Test / resourceDirectory := baseDirectory.value / "test-resources",*/
    )
}
