package de.qwertyuiop.copret
import de.qwertyuiop.copret.syntax._
import ammonite.ops.{%%, pwd}

case class Theme(styles: Map[String, fansi.Attrs], figletFonts: Map[String, String]) {
  def style(key: String, default: fansi.Attrs = fansi.Attrs()) =
    styles.getOrElse(key, default)

  def font(key: String, default: String) =
    figletFonts.getOrElse(key, default)
  
  def extend(newStyles: Map[String, fansi.Attrs]) = copy(styles = styles ++ newStyles)
  def ++(newStyles: Map[String, fansi.Attrs]) = copy(styles = styles ++ newStyles)

  def extend(newFonts: Map[String, String])(implicit d: DummyImplicit) = copy(figletFonts = figletFonts ++ newFonts)
  def ++(newFonts: Map[String, String])(implicit d: DummyImplicit) = copy(figletFonts = figletFonts ++ newFonts)
}
object Theme {
  implicit val default = Theme(Map(
    "titleLine" -> (fansi.Bold.On ++ fansi.Color.DarkGray),
    "code" -> fansi.Color.Yellow
  ),
  Map("titleLine" -> "pagga")
  )
}

object Format {
  def alignRight(str: String, padding: Int = 2) =" " * (columns - str.length - padding) + str + " " * padding

  def center(str: String) = " " * ((columns - str.length) / 2) + str

  def figlet(str: String, font: String) = %%("figlet", "-t", "-f", font, str)(pwd).out.string

  def centerLines(str: String) = str.split("\n").map(center).mkString("\n")
  def centerBlock(str: String) = {
    val lines = str.split("\n")
    val maxLen = lines.map(_.length).max
    val pad = " " * ((columns - maxLen) / 2)
    lines.map(pad + _).mkString("\n")
  }

  def distribute(texts: String*) = {
    val totalPad = columns - texts.map(_.length).sum
    val numPads = texts.size - 1
    val pad = " " * (totalPad / numPads)
    texts.init.mkString(pad) + pad + " " * (totalPad % numPads) + texts.last
  }

  private[copret] val ticks = raw"`([^`]*)`".r
}

/* vim:set tw=120: */
