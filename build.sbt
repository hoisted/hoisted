import AssemblyKeys._ // put this at the top of the file

//Project Information
name := "Hoisted"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

publishMavenStyle := true

autoCompilerPlugins := true

checksums := Nil

organization := "org.hoisted" 

// seq(webSettings :_*)

resolvers += "Scala Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/" 

resolvers += "Scala" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

version := "0.1-SNAPSHOT"

// crossScalaVersions in ThisBuild    := Seq("2.9.2") // "2.9.1-1", "2.9.1", "2.9.0-1", "2.9.0")

libraryDependencies ++= {
  val liftVersion = "2.5-SNAPSHOT"
  Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile"
    )
}

libraryDependencies ++= {
  Seq(
    "org.apache.tika" % "tika-parsers" % "1.1",
    "junit" % "junit" % "4.7" % "test",
    "org.apache.httpcomponents" % "httpclient" % "4.2.1" % "compile",
    "ch.qos.logback" % "logback-classic" % "1.0.6" % "compile" ,
    "org.pegdown" %  "pegdown" % "1.1.0",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "1.3.0.201202151440-r",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test",
    "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"
  )
}

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

assemblySettings

jarName in assembly := "hoisted.jar"

test in assembly := {}

mainClass in assembly := Some("org.hoisted.lib.Hoist")

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://hoisted.org</url>
  <licenses>
    <license>
      <name>Apache 2.0</name>
      <url>http://www.opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/hoisted/hoisted.git</url>
    <connection>scm:git:git://github.com/hoisted/hoisted.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dpp</id>
      <name>David Pollak</name>
      <url>http://blog.goodstuff.im</url>
    </developer>
  </developers>)

