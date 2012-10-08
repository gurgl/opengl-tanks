// NOTE: Need to run with option as : gen-idea no-sbt-classifiers
resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/public/"

//addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0-RC1")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

//addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.5.2")

addSbtPlugin("de.djini" % "xsbt-webstart" % "0.0.5")