import sbt._
import Keys._


object MyBuild extends Build {


  resolvers += "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

  //resolvers += Resolver.file("my-test-repo", file(Path.userHome.absolutePath+"/.m2/repository"))
  //override lazy val settings = super.settings ++ Seq(resolvers := Seq("Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"))
  
  lazy val root = Project(id = "hello",
                          base = file("."),
                          settings = Project.defaultSettings ++ projSettings
  )

  /*unmanagedJars in Compile <++= baseDirectory map { base =>
    	val baseDirectories = (base / "libA") +++ (base / "b" / "lib") +++ (base / "libC")
    	val customJars = (baseDirectories ** "*.jar") +++ (base / "d" / "my.jar")
    	customJars.classpath
    }*/
  unmanagedJars in Compile <++= baseDirectory map { base =>
  	val baseDirectories = (base / "jme3-lib" )
  	val customJars = (baseDirectories ** "*.jar")
  	customJars.classpath
  }

  /*
  unmanagedJars in Compile <++= baseDirectory map { base =>

    val baseDirectories = base +++ new File("/home/karlw/src/3rdparty/jme3/engine/dist/lib")
  	val customJars = (baseDirectories ** "*.jar") // +++ (base / "d" / "my.jar")
    println("Tjao")
    customJars.classpath
  }*/
  /*
  unmanagedJars in Compile <++= baseDirectory map { base:File =>
    val lol = new File("/home/karlw/src/3rdparty/jme3/engine/dist/") 
    val lal = lol / "lib" ** "*.jar"
    println(lal.classpath)
    lal.classpath
  }*/
  /*{
      val lol = new File("/home/karlw/src/3rdparty/jme3/dist/lib/") ** "*.jar"
      lol.classpath
    }                               */


  val projSettings = Seq(
    name := "Lek",
    version := "0.1",
    organization := "se.bupp",

    libraryDependencies ++= Seq(
      "org.lwjgl.lwjgl" % "lwjgl" % "2.8.3",
      "org.lwjgl.lwjgl" % "lwjgl_util" % "2.8.3",
      "org.lwjgl.lwjgl" % "lwjgl-platform" % "2.8.3",
      "com.esotericsoftware" % "kryo" % "2.09",
      "com.esotericsoftware" % "kryonet" % "2.09",
      "com.esotericsoftware" % "minlog" % "1.2",
      "com.esotericsoftware" % "jsonbeans" % "0.2",
      "com.esotericsoftware" % "reflectasm" % "1.03",
      "org.ow2.asm" % "asm" % "4.0",
      "org.specs2" %% "specs2" % "1.11" % "test",

      "org.objenesis" % "objenesis" % "1.2"
//      "org.objectweb" % "asm" % "4.0"



      /*"com.jmonkey" % "engine" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-core.jar",
      "com.jmonkey" % "engine-terr" % "3.0beta" from "file:///home/karlw/src/3rdparty/jme3/engine/dist/lib/jME3-terrain.jar"*/


    )
  )
}