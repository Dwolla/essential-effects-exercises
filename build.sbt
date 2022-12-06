ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.dwolla"
ThisBuild / organizationName := "Dwolla"

lazy val `essential-effects` = (project in file("."))
  .settings(
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.2",
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7",
      "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test,
    )
  )
