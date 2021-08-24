scalaVersion := "2.13.4"
scalacOptions := Seq("-unchecked", "-deprecation", "-explaintypes", "-encoding", "utf8")
name := "lab10"
version := "0.0.1"
libraryDependencies ++= {
  val akkaVersion = "2.6.10"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion
  )
}