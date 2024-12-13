val zioVersion = "1.0.18"
val zioCatsVersion = "3.2.9.1"
val http4sVersion = "0.23.15"
val circeVersion = "0.14.5"
val sttpClientVersion = "3.4.2"

lazy val commonSettings = Seq(
  organization := "org.phenoscape",
  version := "0.2.5",
  licenses := Seq("MIT license" -> url("https://opensource.org/licenses/MIT")),
  scalaVersion := "2.13.11",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypatmat-exhaust-depth", "off")
)

lazy val parentProject = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "phenoscape-kb-ui-project",
    (publish / skip) := true)
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
        "com.raquo" %%% "laminar" % "0.14.5",
        "com.raquo" %%% "waypoint" % "9.0.0",
        "com.lihaoyi" %%% "upickle" % "1.5.0",
        "io.circe" %%% "circe-core" % circeVersion,
        "io.circe" %%% "circe-generic" % circeVersion,
        "io.circe" %%% "circe-parser" % circeVersion,
        "com.softwaremill.sttp.client3" %%% "core" % sttpClientVersion,
        "com.softwaremill.sttp.client3" %%% "circe" % sttpClientVersion,
        "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1"
      )
    }
  )

lazy val webServer = project.in(file("server"))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(commonSettings)
  .settings(dockerSettings)
  .settings(
    name := "phenoscape-kb-ui-server",
    libraryDependencies ++= {
      Seq(
        "dev.zio" %% "zio" % zioVersion,
        "dev.zio" %% "zio-interop-cats" % zioCatsVersion,
        "org.http4s" %% "http4s-blaze-server" % http4sVersion,
        "org.http4s" %% "http4s-dsl" % http4sVersion,
        "com.outr" %% "scribe-slf4j" % "3.7.1"
      )
    }
  )

lazy val dockerSettings = Seq(
  Docker / packageName := "phenoscape-kb-web-ui",
  dockerUsername := Some("phenoscape"),
  dockerExposedPorts := Seq(8080),
  dockerBuildxPlatforms := Seq("linux/arm64/v8", "linux/amd64")
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
addCommandAlias("publishDocker", ";fullOptCompileCopy; webServer/docker:publish")
