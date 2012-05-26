//Project Information
name := "Hoisted"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

autoCompilerPlugins := true

checksums := Nil

resolvers += "Scala Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/" 

resolvers += "Scala" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

libraryDependencies += "net.liftweb" %% "lift-webkit" % "2.5-SNAPSHOT" % "compile"

libraryDependencies += "javax.servlet" % "servlet-api" % "2.5" % "provided->default"

libraryDependencies += "com.h2database" % "h2" % "1.2.138"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.26" % "compile->default"

libraryDependencies += "net.liftweb" %% "lift-mapper" % "2.5-SNAPSHOT" % "compile"
 
libraryDependencies += "junit" % "junit" % "4.7" % "test"


resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

seq(webSettings :_*)

libraryDependencies += "org.mortbay.jetty" % "jetty" % "6.1.22" % "test,container"

libraryDependencies += "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"
