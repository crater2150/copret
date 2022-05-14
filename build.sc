import mill._, scalalib._, publish._
import $ivy.`com.lihaoyi::mill-contrib-bloop:0.9.5`

object copret extends ScalaModule with PublishModule {
  def scalaVersion = "3.1.0"
  override def ammoniteVersion = "2.5.3"


  def publishVersion = "0.0.1"
  def pomSettings = PomSettings(
    description = "Use ammonite scripts for command line presentations",
    organization = "de.qwertyuiop",
    versionControl = VersionControl.github("crater2150", "copret"),
    url = "https://qwertyuiop.de/copret/",
    licenses = Seq(License.MIT),
    developers = Seq(
      Developer("crater2150", "Alexander Gehrke", "https://github.com/crater2150")
	)
      )

  def ivyDeps = Agg(
    ivy"org.jline:jline:3.19.0",
    ivy"com.lihaoyi::ammonite-ops:2.3.8".withDottyCompat(scalaVersion()),
    ivy"com.lihaoyi::fansi:0.2.14",
    )
}

