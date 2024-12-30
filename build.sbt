version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.15"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2024"
  )
