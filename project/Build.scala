import sbt._
import sbt.Keys._
import sbt.Package.ManifestAttributes
import sbt.Defaults._
import java.util.jar.Attributes.Name._
import sbt.Package.ManifestAttributes
import scala.Some
import WebStartPlugin._

object MyBuild extends Build {



  lazy val root = Project(id = "server",
                          base = file("."),
                          settings = Project.defaultSettings ++ projSettings  ++ serverSettings
  )

  lazy val serverSettings = Seq(
    mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
      ms filter { case (file, toPath) =>
        !toPath.contains("client")
      }
    }
  )

  unmanagedJars in Compile <++= baseDirectory map { base =>
  	val baseDirectories = (base / "jme3-lib" )
  	val customJars = (baseDirectories ** "*.jar")
  	customJars.classpath
  }

  val projSettings = Seq(
    version := "0.1",
    organization := "se.bupp",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public" //Resolver.sonatypeRepo("releases")
    ,
    //publishArtifact in (Compile, packageBin) := false,
    libraryDependencies ++= Seq(
      /*"com.esotericsoftware" % "kryo" % "2.09",
      "com.esotericsoftware" % "kryonet" % "2.09",
      "com.esotericsoftware" % "minlog" % "1.2",
      "com.esotericsoftware" % "jsonbeans" % "0.2",
      "com.esotericsoftware" % "reflectasm" % "1.03",
      "org.ow2.asm" % "asm" % "4.0",*/
      "org.specs2" %% "specs2" % "1.11" % "test",
      "org.mockito" % "mockito-all" % "1.9.0" % "test",
      "org.scalaz" %% "scalaz-core" % "6.0.4",
      "org.objenesis" % "objenesis" % "1.2",
      "com.jme3" % "eventbus" % "3.0.0.20120512-SNAPSHOT",
      //"com.jme3" % "jbullet" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jinput" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-blender" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-core" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-desktop" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-effects" % "3.0.0.20120512-SNAPSHOT",
      //"com.jme3" % "jME3-jbullet" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-jogg" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-lwjgl" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-lwjgl-natives" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-networking" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-niftygui" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-plugins" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-terrain" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "jME3-testdata" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "j-ogg-oggd" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "j-ogg-vorbisd" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "lwjgl" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "nifty" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "nifty-default-controls" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "nifty-examples" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "nifty-style-black" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "stack-alloc" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "vecmath" % "3.0.0.20120512-SNAPSHOT",
      "com.jme3" % "xmlpull-xpp3" % "3.0.0.20120512-SNAPSHOT"
      /*"com.jmonkey" % "engine" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-core.jar",
      "com.jmonkey" % "engine-terr" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-terrain.jar"*/
    )
  )

  // defines a new configuration "samples" that will delegate to "compile"
  lazy val ClientBuild = config("samples") extend(Compile)

  // defines the project to have the "samples" configuration
  lazy val serverProject = Project("client", file("."), settings = clientSettings)
    .configs(ClientBuild)

  def clientSettings = Project.defaultSettings ++
    projSettings ++
    //oneJarSettings ++
    webStartSettings ++
    Seq(
      unmanagedResourceDirectories in Compile <+=  baseDirectory { dir =>
        dir/"src/main/blender" // +++ dir/"src/main/resources/reports"
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



  /*def publishArtifact(task: TaskKey[File]): Seq[Setting[_]] =
    addArtifact(artifact in (ServerBuild, task), task in ServerBuild).settings
  */

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
      codeBase        = "http://localhost:8080/game",
      title           = "My Title",
      vendor          = "My Company",
      description     = "My Webstart Project",
      iconName = None,
      splashName = None,
      offlineAllowed  = true,
      allPermissions  = true,
      j2seVersion     = "1.6+",
      maxHeapSize     = 192
    ))
  )
}