// -*- mode: scala -*-

import mill._, os._, scalalib._, publish._
import scala.util.Properties

object meta {

  val crossVersions = Seq("3.1.1")

  implicit val wd: Path = pwd

  def nonEmpty(s: String): Option[String] = s.trim match {
    case v if v.isEmpty => None
    case v              => Some(v)
  }

  val MILL_VERSION = Properties.propOrNull("MILL_VERSION")
  val versionFromEnv = Properties.propOrNone("PUBLISH_VERSION")
  val gitSha = nonEmpty(
    proc("git", "rev-parse", "--short", "HEAD").call().out.trim
  )
  val gitTag = nonEmpty(
    proc("git", "tag", "-l", "-n0", "--points-at", "HEAD").call().out.trim
  )
  val publishVersion =
    (versionFromEnv orElse gitTag orElse gitSha).getOrElse("latest")

  def pomSettings = PomSettings(
    description = "ZStream based microKanren",
    organization = "com.github.vic",
    url = "https://github.com/vic/zkanren",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("vic", "zkanren"),
    developers = Seq(
      Developer("vic", "Victor Borja", "https://github.com/vic")
    )
  )

}

object zkanren extends Cross[ZKanren](meta.crossVersions: _*)
class ZKanren(val crossScalaVersion: String)
    extends CrossScalaModule
    with PublishModule { self =>
  def publishVersion = meta.publishVersion
  override def artifactName = "zkanren"
  override def pomSettings: T[PomSettings] = meta.pomSettings
}
