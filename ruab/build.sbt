name := "ruab"

version := "0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
	"org.jdom" % "jdom-contrib" % "1.1.3",
	"log4j" % "log4j" % "1.2.16"
)
