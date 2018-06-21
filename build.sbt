val http4sVersion = "0.18.4"
val logbackVersion = "1.2.3"
val circeVersion = "0.9.1"
val doobieVersion = "0.5.2"
val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.13.4"
val catsVersion = "1.1.0"

organization := "org.penny_craal"
name := "mairion"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.5"
logBuffered in Test := false
scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint:_",
)

libraryDependencies ++= Seq(
  "org.http4s"      %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"      %% "http4s-circe"        % http4sVersion,
  "org.http4s"      %% "http4s-dsl"          % http4sVersion,
  "io.circe"        %% "circe-parser"        % circeVersion,
  "io.circe"        %% "circe-generic"       % circeVersion,
  "io.circe"        %% "circe-literal"       % circeVersion,
  "io.circe"        %% "circe-java8"         % circeVersion,
  "ch.qos.logback"  %  "logback-classic"     % logbackVersion,
  "org.tpolecat"    %% "doobie-core"         % doobieVersion,
  "org.typelevel"   %% "cats-core"           % catsVersion,
  "org.typelevel"   %% "cats-free"           % catsVersion,
  "org.scalatest"   %  "scalatest_2.12"      % scalaTestVersion % "test",
  "org.scalacheck"  %% "scalacheck"          % scalaCheckVersion % "test",
  "org.tpolecat"    %% "doobie-scalatest"    % doobieVersion % "test",
)
