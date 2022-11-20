Global / onChangedBuildSource := ReloadOnSourceChanges

name         := "rock-the-zio"
version      := "0.1"
scalaVersion := "3.2.1"

lazy val zioVersion = "2.0.4"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"            % zioVersion,
  "dev.zio" %% "zio-test"       % zioVersion,
  "dev.zio" %% "zio-test-sbt"   % zioVersion,
  "dev.zio" %% "zio-streams"    % zioVersion,
  "dev.zio" %% "zio-test-junit" % zioVersion
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

run / fork := true

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)
