/*import _root_.WebStartPlugin.GenConf
import _root_.WebStartPlugin.JnlpConf
import _root_.WebStartPlugin.KeyConf*/

import sbt._
import sbt.Keys._
import sbt.Package.ManifestAttributes
import sbt.Defaults._
import java.util.jar.Attributes.Name._
import sbt.Package.ManifestAttributes
import sbt.Package.ManifestAttributes
import com.github.retronym.SbtOneJar.oneJar
import scala._
import scala.Some
import scala.Some
import WebStartPlugin._

object MyBuild extends Build {

  val JME_VERSION = "3.0.0.20120512-SNAPSHOT"

  /**
   * println(evalTask(fullClasspath in Runtime, currentState).map(_.data).mkString(";"))
   */
  lazy val moduleDefaultSettings = Defaults.defaultSettings ++ Seq(serverClassPathTask, scalaVersion := "2.10.0")

  lazy val rootProject = Project(id = "root",
    base = file("."),
    settings = moduleDefaultSettings ++ Seq(publishArtifact in Compile := false)
  ) aggregate(serverProject, clientProject)

  lazy val commonProject = Project("common",
    base = file("common"),
    settings = moduleDefaultSettings  ++ commonSettings
  )

  lazy val serverProject = Project(id = "server",
    base = file("server"),
    settings = moduleDefaultSettings  ++ serverSettings  ++ net.virtualvoid.sbt.graph.Plugin.graphSettings ++ com.github.retronym.SbtOneJar.oneJarSettings
  ) dependsOn(commonProject)

  lazy val clientProject:Project = Project("client",
    base = file("client"),
    settings = moduleDefaultSettings  ++ clientSettings ++ webStartSettings
  ) dependsOn(commonProject)

  lazy val commonSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    exportJars := true,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    libraryDependencies ++=  Seq(
      "org.scalaz" %% "scalaz-core" % "6.0.4"
    ) ++ allDependsOn ++ jmeClientAndServer ++ testDeps,
    unmanagedResourceDirectories in Compile <+=  baseDirectory { dir =>
      dir / "src/main/blender"
    }
  )

  lazy val serverSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    publishArtifact in Test := false,
    publishArtifact in Compile := false,
    libraryDependencies ++=  testDeps ++ Seq(
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.0"
    ),
    mainClass in oneJar := Some("se.bupp.lek.server.Server"),
    exportJars := true,
    Keys.`package` in Compile <<= (Keys.`package` in Compile) dependsOn(oneJar)
    )


  lazy val clientSettings = Seq(
    name:= "client",
    version:= "1.0",
    //publishTo := Some(Resolver.file("file",  new File("c:\\dev\\workspace\\cs3k\\server\\src\\main\\webapp\\game_deploy_dir_tmp\\tanks\\"))),
    libraryDependencies ++=  jmeClient ++ testDeps,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    mainClass in (Compile, packageBin)  := Some("se.bupp.lek.client.Client"),
    mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
      ms filter { case (file, toPath) =>
        true
      }
    },
    /*artifact in (Compile, packageBin) ~= { (art: Artifact) =>
      art.copy(`type` = "bundle")
    },*/
    Keys.`package` in Compile <<= (Keys.`package` in Compile) dependsOn(webstartBuild)
  )


  lazy val webStartSettings = WebStartPlugin.allSettings ++ Seq(
    webstartGenConf := GenConf(
      dname       = "CN=Snake Oil, OU=An Anonymous Hacker, O=Bad Guys Inc., L=Bielefeld, ST=33641, C=DE",
      validity    = 365
    ),

    webstartKeyConf := KeyConf(
      keyStore    = file("testKeys"),
      storePass   = "bobbafett123",
      alias       = "jdc",
      keyPass     = "bobbafett123"
    ),

    webstartJnlpConf    := Seq(JnlpConf(
      mainClass       = "se.bupp.lek.client.Client",
      fileName        = "Game.jnlp",
      codeBase        = "http://localhost:8080/game_deploy_dir_tmp/tanks",
      title           = "Tank Showdown!",
      vendor          = "PäronGlans",
      description     = "Multiplayer game",
      iconName = None,
      splashName = None,
      offlineAllowed  = true,
      allPermissions  = true,
      j2seVersion     = "1.6",
      maxHeapSize     = 192
    ))
  )

  val jmeClient = Seq(
    "com.jme3" % "jME3-effects" % JME_VERSION,
    "com.jme3" % "j-ogg-oggd" % JME_VERSION,
    "com.jme3" % "j-ogg-vorbisd" % JME_VERSION,
    "com.jme3" % "lwjgl" % JME_VERSION,
    "com.jme3" % "jME3-lwjgl" % JME_VERSION,
    "com.jme3" % "jME3-lwjgl-natives" % JME_VERSION,
    "com.jme3" % "jME3-jogg" % JME_VERSION,
    "com.jme3" % "jinput" % JME_VERSION
  )

  val jmeClientAndServer = Seq(
    "com.jme3" % "jME3-desktop" % JME_VERSION,
    "com.jme3" % "eventbus" % JME_VERSION,
    "com.jme3" % "jbullet" % JME_VERSION,
    "com.jme3" % "jME3-blender" % JME_VERSION,
    "com.jme3" % "jME3-core" % JME_VERSION,
    "com.jme3" % "jME3-jbullet" % JME_VERSION,

    //"com.jme3" % "jME3-networking" % JME_VERSION,
    //"com.jme3" % "jME3-niftygui" % JME_VERSION,
    "com.jme3" % "jME3-plugins" % JME_VERSION,
    "com.jme3" % "jME3-terrain" % JME_VERSION,
    "com.jme3" % "jME3-testdata" % JME_VERSION,
    //"com.jme3" % "nifty" % JME_VERSION,
    //"com.jme3" % "nifty-default-controls" % JME_VERSION,
    //"com.jme3" % "nifty-examples" % JME_VERSION,
    //"com.jme3" % "nifty-style-black" % JME_VERSION,
    "com.jme3" % "stack-alloc" % JME_VERSION,
    "com.jme3" % "vecmath" % JME_VERSION,
    "com.jme3" % "xmlpull-xpp3" % JME_VERSION)

  val allDependsOn = Seq(
    "org.objenesis" % "objenesis" % "1.2",
    "com.esotericsoftware.kryo" % "kryo" % "2.20",
    "se.paronglans.cs3k" %% "api" % "0.3-SNAPSHOT" changing(),
    "org.slf4j" % "slf4j-api" % "1.7.2",
    "org.slf4j" % "slf4j-log4j12" % "1.7.2",
    "org.slf4j" % "jul-to-slf4j" % "1.7.2"
  )

  val testDeps = Seq(
    "org.specs2" %% "specs2" % "1.11" % "test",
    "org.mockito" % "mockito-all" % "1.9.0" % "test"
  )

  val serverClassPath = TaskKey[Unit]("server-class-path", "Classpath.")

  var serverClassPathTask = serverClassPath := {
    println("Tja")
    (fullClasspath in Runtime) map { (cp) =>
      //println("Target path is: "+target)
      println("Full classpath is: "+cp.map(_.data).mkString(":"))
    }

    //this.runClasspath.getPaths.foreach(println)
  }

    /*println("tja" + ( fullClasspath in Compile))

    ( fullClasspath in Compile ) map { case (r) =>  r.files foreach println }
  }
  */

  //serverClassPathTask <<= serverClassPathTask.dependsOn(Compile)

  /*unmanagedJars in Compile <++= baseDirectory map { base =>
  	val baseDirectories = (base / "jme3-lib" )
  	val customJars = (baseDirectories ** "*.jar")
  	customJars.classpath
  }*/
}