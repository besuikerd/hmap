name := "hmap"
organization := "com.besuikerd"
version := "0.1.0"

scalaVersion := "2.11.8"

publishTo := Some(Resolver.file("file", new File(Path.userHome, ".m2/repository")))

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
    