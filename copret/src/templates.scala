package de.qwertyuiop.copret
import syntax.*
import ammonite.ops.{Path, %, %%, pwd, ImplicitWd}

trait Templates:
  def titleLine(title: String)(using theme: Theme) = Paragraph(
    "\n" + Format.figlet(title, theme.font("titleLine", "pagga")).block.blue + "\n"
      )

  def header(using theme: Theme) = Meta((p, pos) => {
    val left = p.meta.getOrElse("author", "")
    val center = p.meta.getOrElse("title", "")
    val right = s"${pos} / ${p.slides.size - 1}"
    theme.style("titleLine")(Format.distribute(left, center, right)).text
  })

  def slide(title: String)(slides: Slide*)(using Theme) = Group(Clear :: header :: titleLine(title) :: slides.toList)
  def slide(slides: Slide*)(using Theme) = Group(Clear :: header :: slides.toList)

  def markdown(title: String, content: Path)(using Theme) = slide(title)(
    Paragraph(
      %%%("/usr/bin/mdcat", "--columns", (columns * 0.8).toInt.toString, content.toString)(using ImplicitWd.implicitCwd).block
    )
  )

  lazy val --- = Paragraph(("═" * columns).yellow.toString)

/* vim:set tw=120: */
