name := "$name$"

organization := "$organization$"

version := "1.0.0"

scalaVersion := "2.11.8"

fork in Test := true

parallelExecution := false

licenses +=("Apache-2.0", url("http://opensource.org/licenses/apache2.0.php"))

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"
libraryDependencies += "org.typelevel" %% "scalaz-scalatest" % "1.1.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

// a list of cabal packages to add to the haskell build
cabalPackages := Seq("lens-4.15.1")