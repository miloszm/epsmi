lazy val akkaHttpVersion = "10.1.12"
lazy val akkaVersion    = "2.6.9"

assemblyJarName in assembly := "epsmi-1.0.jar"
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList("reference.conf", xs @ _*) => MergeStrategy.concat
  case x => MergeStrategy.first
}

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"                % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json"     % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion,
  "com.typesafe.akka" %% "akka-stream"              % akkaVersion,
  "ch.qos.logback"    % "logback-classic"           % "1.2.3",
  "org.bitcoin-s"     %% "bitcoin-s-core"           % "0.4.0",
  "org.bitcoin-s"     %% "bitcoin-s-bitcoind-rpc"   % "0.4.0",
  //"com.github.briandilley.jsonrpc4j"  % "jsonrpc4j" % "1.5.3",
  "org.scodec"        %% "scodec-bits"              % "1.1.12",


  "com.typesafe.akka" %% "akka-http-testkit"        % akkaHttpVersion % Test,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion     % Test,
  "org.scalatest"     %% "scalatest"                % "3.0.8"         % Test,
  "org.pegdown"       %  "pegdown"                  % "1.6.0"         % Test
  //"org.bitcoin-s"     %% "bitcoin-s-testkit"        % "0.4.0"         % Test
)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization    := "com.mhm",
      scalaVersion    := "2.13.1"
    )),
    name := "epsmi",
    parallelExecution in Test := false,
    ThisBuild / Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")
  )
