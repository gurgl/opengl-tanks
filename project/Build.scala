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
import scala.Some
import scala.Some
import WebStartPlugin._

object MyBuild extends Build {

  val JME_VERSION = "3.0.0.20120512-SNAPSHOT"

  lazy val commonProject = Project("common",
    base = file("common"),
    settings = Project.defaultSettings ++ commonSettings
  )

  lazy val serverProject = Project(id = "server",
    base = file("server"),
    settings = Project.defaultSettings ++ serverSettings ++ Seq(serverClassPathTask)
  ) dependsOn(commonProject)

  lazy val clientProject = Project("client",
    base = file("client"),
    settings = Project.defaultSettings ++ clientSettings ++ webStartSettings
  ) dependsOn(commonProject)

  lazy val rootProject = Project(id = "root",
    base = file(".")
  ) aggregate(serverProject, clientProject)

  lazy val commonSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    //publishArtifact in (Compile, packageBin) := false,
    libraryDependencies ++=  Seq(
      "org.scalaz" %% "scalaz-core" % "6.0.4",
      "log4j" % "log4j" % "1.2.17",
      "com.jme3" % "jbullet" % JME_VERSION,
      "com.jme3" % "jME3-jbullet" % JME_VERSION,
      "com.jme3" % "vecmath" % JME_VERSION,
      "com.jme3" % "jME3-core" % JME_VERSION
      //"com.esotericsoftware.kryo" % "kryo" % "2.18"
    )
  )


  lazy val serverSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    //publishArtifact in (Compile, packageBin) := false,
    libraryDependencies ++=  allDependsOn ++ jmeMaven
  )

  lazy val clientSettings = Seq[Project.Setting[_]](
    name:= "client",
    version:= "1.0",
    libraryDependencies ++=  allDependsOn ++ jmeMaven,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public",
    unmanagedResourceDirectories in Compile <+=  baseDirectory { dir =>
      dir / "src/main/blender" // +++ dir/"src/main/resources/reports"
    },
    mainClass in (Compile, packageBin)  := Some("se.bupp.lek.client.Client"),
    mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
      ms filter { case (file, toPath) =>
        //System.err.println(toPath)
        //!toPath.contains("server")
        true
      }
    }
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
      j2seVersion     = "1.6+",
      maxHeapSize     = 192
    ))
  )

  val jmeMaven = Seq(
    "com.jme3" % "eventbus" % JME_VERSION,
    "com.jme3" % "jbullet" % JME_VERSION,
    "com.jme3" % "jinput" % JME_VERSION,
    "com.jme3" % "jME3-blender" % JME_VERSION,
    "com.jme3" % "jME3-core" % JME_VERSION,
    "com.jme3" % "jME3-desktop" % JME_VERSION,
    "com.jme3" % "jME3-effects" % JME_VERSION,
    "com.jme3" % "jME3-jbullet" % JME_VERSION,
    "com.jme3" % "jME3-jogg" % JME_VERSION,
    "com.jme3" % "jME3-lwjgl" % JME_VERSION,
    "com.jme3" % "jME3-lwjgl-natives" % JME_VERSION,
    "com.jme3" % "jME3-networking" % JME_VERSION,
    "com.jme3" % "jME3-niftygui" % JME_VERSION,
    "com.jme3" % "jME3-plugins" % JME_VERSION,
    "com.jme3" % "jME3-terrain" % JME_VERSION,
    "com.jme3" % "jME3-testdata" % JME_VERSION,
    "com.jme3" % "j-ogg-oggd" % JME_VERSION,
    "com.jme3" % "j-ogg-vorbisd" % JME_VERSION,
    "com.jme3" % "lwjgl" % JME_VERSION,
    "com.jme3" % "nifty" % JME_VERSION,
    "com.jme3" % "nifty-default-controls" % JME_VERSION,
    "com.jme3" % "nifty-examples" % JME_VERSION,
    "com.jme3" % "nifty-style-black" % JME_VERSION,
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
    "org.specs2" %% "specs2" % "1.11" % "test",
    "org.mockito" % "mockito-all" % "1.9.0" % "test",
    "org.objenesis" % "objenesis" % "1.2",
    "com.esotericsoftware.kryo" % "kryo" % "2.20",
    "se.paronglans" %% "cs3k-api" % "0.3-SNAPSHOT" changing(),
    "log4j" % "log4j" % "1.2.17"
    /*"com.jmonkey" % "engine" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-core.jar",
"com.jmonkey" % "engine-terr" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-terrain.jar"*/
  )

  val serverClassPath = TaskKey[Unit]("server-class-path", "Deletes files produced by the build, such as generated sources, compiled classes, and task caches.")

  var serverClassPathTask = serverClassPath := {
    println("tja" + ( fullClasspath in Compile))

    ( fullClasspath in Compile ) map { r => println("Bupp") ;  r.files foreach println }
  }

  //serverClassPathTask <<= serverClassPathTask.dependsOn(Compile)

  /*unmanagedJars in Compile <++= baseDirectory map { base =>
  	val baseDirectories = (base / "jme3-lib" )
  	val customJars = (baseDirectories ** "*.jar")
  	customJars.classpath
  }*/
/*
  val oneJar = TaskKey[File]("one-jar", "Create a single executable JAR using One-JAR™")
  val oneJarRedist = TaskKey[Set[File]]("one-jar-redist", "The redistributable One-JAR™ launcher, unzipped.")

  val oneJarSettings: Seq[Project.Setting[_]] = inTask(oneJar)(Seq(
    artifactPath <<= artifactPathSetting(artifact),
    cacheDirectory <<= cacheDirectory / oneJar.key.label
  )) ++ Seq(

    publishArtifact in oneJar <<= publishMavenStyle,
    artifact in oneJar <<= moduleName(Artifact(_, "one-jar")),
    packageOptions in oneJar := Seq(ManifestAttributes((MAIN_CLASS, "com.simontuffs.onejar.Boot"))),
    mainClass in oneJar <<= mainClass in run in Compile,
    packageOptions in oneJar <++= (mainClass in oneJar).map {
      case Some(mainClass) => Seq(ManifestAttributes(("One-Jar-Main-Class", mainClass)))
      case _ => Seq()
    },
    baseDirectory in oneJarRedist <<= (target)(_ / "one-jar-redist"),
    oneJarRedist <<= (baseDirectory in oneJarRedist).map { (base) =>
      val oneJarResourceName = "one-jar-boot-0.97.jar"
      System.err.println("one jar 1")
      val s = getClass.getClassLoader.getResourceAsStream(oneJarResourceName)
      if (s == null) sys.error("could not load: " + oneJarResourceName)
      def include(path: String) = path match {
        case "META-INF/MANIFEST.MF" => false
        case x => !x.startsWith("src/")
      }
      IO.unzipStream(s, base, include _)
    },
    mappings in oneJar <<= (packageBin in Compile, dependencyClasspath in Runtime,
      oneJarRedist, baseDirectory in oneJarRedist).map {
      (artifact, classpath, oneJarRedist, oneJarRedistBase) =>
        val thisArtifactMapping = (artifact, (file("main") / artifact.name).getPath)
        val deps: Seq[(File, String)] = {
          val allDeps = Build.data(classpath).map(f => (f, (file("lib") / f.name).getPath))
          allDeps.filterNot(_._1 == artifact)
        }

        val redist = oneJarRedist.toSeq x relativeTo(oneJarRedistBase)
        Seq(thisArtifactMapping) ++ deps ++ redist
    },
    oneJar <<= (mappings in oneJar, artifactPath in oneJar, packageOptions in oneJar, cacheDirectory in oneJar, streams) map {
      (mappings, output, packOpts, cacheDir, s) =>
        val packageConf = new Package.Configuration(mappings, output, packOpts)
        Package(packageConf, cacheDir, s.log)
        output
    }
  )
*/
}