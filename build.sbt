resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

initialCommands in console := """
import com.quoininc.treediffing._
import scala.collection.immutable.Seq
"""

lazy val root = (project in file(".")).
  settings(
    name := "com.quoininc.treediffing",
    version := "1.0",
    scalaVersion := "2.11.6"
  )

