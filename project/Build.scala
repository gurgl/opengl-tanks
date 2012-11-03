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
import scala.Some
import scala.Some
import WebStartPlugin._

object MyBuild extends Build {

  val JME_VERSION = "3.0.0.20120512-SNAPSHOT"

  lazy val rootProject = Project(id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(publishArtifact in Test := true)
  ) aggregate(serverProject, clientProject)

  lazy val commonProject = Project("common",
    base = file("common"),
    settings = Project.defaultSettings ++ commonSettings
  )

  lazy val serverProject = Project(id = "server",
    base = file("server"),
    settings = Project.defaultSettings ++ serverSettings  ++ net.virtualvoid.sbt.graph.Plugin.graphSettings ++ com.github.retronym.SbtOneJar.oneJarSettings  // ++ Seq(serverClassPathTask)
  ) dependsOn(commonProject)

  lazy val clientProject:Project = Project("client",
    base = file("client"),
    settings = Project.defaultSettings ++ clientSettings ++ webStartSettings
  ) dependsOn(commonProject)

  lazy val commonSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    exportJars := true,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    //publishArtifact in (Compile, packageBin) := false,
    libraryDependencies ++=  Seq(
      "org.scalaz" %% "scalaz-core" % "6.0.4"
      //"log4j" % "log4j" % "1.2.17"
      /*"com.jme3" % "jbullet" % JME_VERSION,
      "com.jme3" % "jME3-jbullet" % JME_VERSION,
      "com.jme3" % "vecmath" % JME_VERSION,
      "com.jme3" % "jME3-core" % JME_VERSION*/
      //"com.esotericsoftware.kryo" % "kryo" % "2.18"
    )
      ++ allDependsOn
      ++ jmeClientAndServer
      ++ testDeps
    ,
    unmanagedResourceDirectories in Compile <+=  baseDirectory { dir =>
      dir / "src/main/blender" // +++ dir/"src/main/resources/reports"
    }
  )


  lazy val serverSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    //publishArtifact in (Compile, packageBin) := false,
    libraryDependencies ++=  testDeps,
    mainClass in oneJar := Some("se.bupp.lek.server.Server"),
    exportJars := true,
    /*publishArtifact in Compile := true,
    publishArtifact in Test := true
    ,publishArtifact in (Test,oneJar) := true
    ,publishArtifact in (Compile,oneJar) := true,*/
    //publishArtifact in (Compile, packageBin) := false

      // create an Artifact for publishing the .war file
/*,   artifact in (Compile, oneJar) ~= { (art: Artifact) =>
      art.copy(`type` = "war", extension = "war")
    }*/
    Keys.`package` in Compile <<= (Keys.`package` in Compile) dependsOn(oneJar)
    )//packageBin in Compile <<= (packageBin in Compile) dependsOn(oneJar)


  lazy val clientSettings = Seq(
    name:= "client",
    version:= "1.0",
    libraryDependencies ++=  jmeClient ++ testDeps,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    mainClass in (Compile, packageBin)  := Some("se.bupp.lek.client.Client"),
    mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
      ms filter { case (file, toPath) =>
        //System.err.println(toPath)
        //!toPath.contains("server")
        true
      }
    },
    Keys.`package` in Compile <<= (Keys.`package` in Compile) dependsOn(webstartBuild)



    /*artifact in (Compile, packageBin) ~= { (art: Artifact) =>
      oneJar
    }*/
    //,publishArtifact in (Test, oneJar) := true
    //,  packageBin in Compile <<= (packageBin in Compile) dependsOn(packageBin in Compile, oneJar in Compile)
    //addArtifact(artifact in (Compile, packageBin), oneJar)


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
    /*"com.esotericsoftware" % "kryo" % "2.09",
"com.esotericsoftware" % "kryonet" % "2.09",
"com.esotericsoftware" % "minlog" % "1.2",
"com.esotericsoftware" % "jsonbeans" % "0.2",
"com.esotericsoftware" % "reflectasm" % "1.03",
"org.ow2.asm" % "asm" % "4.0",*/
    //"org.springframework" % "spring-context" % "3.1.2.RELEASE",
    "org.objenesis" % "objenesis" % "1.2",
    "com.esotericsoftware.kryo" % "kryo" % "2.20",
    "se.paronglans.cs3k" %% "api" % "0.3-SNAPSHOT" changing(),
    "log4j" % "log4j" % "1.2.17"
    /*"com.jmonkey" % "engine" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-core.jar",
"com.jmonkey" % "engine-terr" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-terrain.jar"*/
  )

  val testDeps = Seq(
    "org.specs2" %% "specs2" % "1.11" % "test",
    "org.mockito" % "mockito-all" % "1.9.0" % "test"
  )

  val serverClassPath = TaskKey[Unit]("server-class-path", "Deletes files produced by the build, such as generated sources, compiled classes, and task caches.")

  var serverClassPathTask = serverClassPath := {
    println("tja" + ( fullClasspath in Compile))

    ( fullClasspath in Compile ) map { r =>  r.files foreach println }
  }

  //serverClassPathTask <<= serverClassPathTask.dependsOn(Compile)

  /*unmanagedJars in Compile <++= baseDirectory map { base =>
  	val baseDirectories = (base / "jme3-lib" )
  	val customJars = (baseDirectories ** "*.jar")
  	customJars.classpath
  }*/
}