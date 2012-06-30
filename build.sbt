//Project Information
name := "Hoisted"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

autoCompilerPlugins := true

checksums := Nil

seq(webSettings :_*)

resolvers += "Scala Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/" 

resolvers += "Scala" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

resolvers += "Media4u101 SNAPSHOT Repository" at "http://www.media4u101.se:8081/nexus/content/repositories/snapshots/"

libraryDependencies ++= {
  val liftVersion = "2.5-SNAPSHOT"
  Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
    "net.liftweb" %% "lift-mapper" % "2.5-SNAPSHOT" % "compile",
    "net.liftmodules" %% "fobo" % (liftVersion+"-0.5.0-SNAPSHOT")
    )
}

libraryDependencies ++= {
  Seq(
    "javax.servlet" % "servlet-api" % "2.5" % "provided->default",
    "com.h2database" % "h2" % "1.3.167",
    "ch.qos.logback" % "logback-classic" % "1.0.6" % "compile->default",
    "junit" % "junit" % "4.7" % "test",
    "org.pegdown" %  "pegdown" % "1.1.0",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "1.3.0.201202151440-r",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test,container",
    "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"
  )
}
