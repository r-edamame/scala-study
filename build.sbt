
scalaVersion := "2.12.4"

scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ypartial-unification"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-macros" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-laws" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.0.0"

