name := "regular-crossword"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.12.3" % "test",
    "junit" % "junit" % "4.11"
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
                  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
                  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")
