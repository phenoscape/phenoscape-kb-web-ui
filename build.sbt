val zioVersion = "1.0.13"
val http4sVersion = "0.23.8"
val circeVersion = "0.14.1"
val sttpClientVersion = "3.1.9"

lazy val commonSettings = Seq(
  organization := "org.phenoscape",
  version := "0.2",
  licenses := Seq("MIT license" -> url("https://opensource.org/licenses/MIT")),
  scalaVersion := "2.13.7",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypatmat-exhaust-depth", "off")
)

lazy val parentProject = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "phenoscape-kb-ui-project",
    skip in publish := true)
  .aggregate(
    webUI,
    webServer
  )

lazy val webUI = project.in(file("ui"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings)
  .settings(
    name := "phenoscape-kb-ui",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= {
      Seq(
        "com.raquo" %%% "laminar" % "0.12.2",
        "com.raquo" %%% "waypoint" % "0.3.0",
        "com.lihaoyi" %%% "upickle" % "1.4.4",
        "io.circe" %%% "circe-core" % circeVersion,
        "io.circe" %%% "circe-generic" % circeVersion,
        "io.circe" %%% "circe-parser" % circeVersion,
        "com.softwaremill.sttp.client3" %%% "core" % sttpClientVersion,
        "com.softwaremill.sttp.client3" %%% "circe" % sttpClientVersion
      )
    }
  )

lazy val webServer = project.in(file("server"))
  .settings(commonSettings)
  .settings(
    name := "phenoscape-kb-ui-server",
    libraryDependencies ++= {
      Seq(
        "dev.zio" %% "zio" % zioVersion,
        "dev.zio" %% "zio-interop-cats" % "2.5.1.0",
        "org.http4s" %% "http4s-blaze-server" % http4sVersion,
        "org.http4s" %% "http4s-dsl" % http4sVersion,
        "com.outr" %% "scribe-slf4j" % "3.5.5"
      )
    }
  )

val jsPath = "server/src/main/resources/js"

lazy val fastOptCompileCopy = taskKey[Unit]("")

fastOptCompileCopy := {
  val source = (webUI / Compile / fastOptJS).value.data
  IO.copyFile(
    source,
    baseDirectory.value / jsPath / "app.js"
  )
}

lazy val fullOptCompileCopy = taskKey[Unit]("")

fullOptCompileCopy := {
  val source = (webUI / Compile / fullOptJS).value.data
  IO.copyFile(
    source,
    baseDirectory.value / jsPath / "app.js"
  )
}

addCommandAlias("runDev", ";fastOptCompileCopy; webServer/reStart")
addCommandAlias("runProd", ";fullOptCompileCopy; webServer/reStart")
