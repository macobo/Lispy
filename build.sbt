name := "Lispy"

version := "0.1"

scalaVersion := "2.9.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)
