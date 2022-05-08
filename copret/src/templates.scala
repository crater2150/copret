package de.qwertyuiop.copret
import syntax._
import ammonite.ops.{Path, %, %%, pwd}

trait Templates:
  def titleLine(title: String)(implicit theme: Theme) = Paragraph(
    "\n" + Format.figlet(title, theme.font("titleLine", "pagga")).block.blue + "\n"
      )

  def header(implicit theme: Theme) = Meta((p, pos) => {
    val left = p.meta.getOrElse("author", "")
    val center = p.meta.getOrElse("title", "")
    val right = s"${pos} / ${p.slides.size - 1}"
    theme.style("titleLine")(Format.distribute(left, center, right)).text
  })

  def slide(title: String)(slides: Slide*) = Group(Clear :: header :: titleLine(title) :: slides.toList)
  def slide(slides: Slide*) = Group(Clear :: header :: slides.toList)

  def markdown(title: String, content: Path) = slide(title)(IncludeMarkdown(content))

  lazy val --- = Paragraph(("═" * columns).yellow)

/* vim:set tw=120: */
