lazy val commonSettings = Seq(
  organization := "io.beehive",
  version := "0.1.0",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Beehive",
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.4.8",
    libraryDependencies += "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9-RC2",
    libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json-experimental"  % "2.4.8",
    libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.4.8",
    libraryDependencies += "joda-time" % "joda-time" % "2.9.4",
    libraryDependencies += "com.typesafe.akka" %% "akka-persistence-cassandra" % "0.19"
  )
