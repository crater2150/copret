package de.qwertyuiop.copret

object syntax extends Templates with TerminalSyntax with SlideSyntax {
  implicit class PresenterStringExtensions(val str: String) {
    import Format._
    def code(implicit theme: Theme) = Format.ticks.replaceAllIn(str, m => theme.style("code")("$1").render)
    def text(implicit theme: Theme) = Paragraph(str)
    def par(implicit theme: Theme) = Paragraph(str.stripMargin.code.padLeft(2))
    def style(key: String, default: fansi.Attrs = fansi.Attrs())(implicit theme: Theme) = theme.style(key, default)(str)

    def centered = center(str)
    def block = centerBlock(str)
    def right = alignRight(str)
    def right(padding: Int) = alignRight(str, padding)
    def padLeft(padding: Int) = {
      val pad = " " * padding
      str.linesIterator.map(pad + _).mkString("\n")
    }

    def blue = fansi.Color.Blue(str)
    def green = fansi.Color.Green(str)
    def yellow = fansi.Color.Yellow(str)
    def red = fansi.Color.Red(str)
  }

  implicit class PresenterFansiStringExtensions(val str: fansi.Str) {
    import Format._
    def text(implicit theme: Theme) = Paragraph(str)
    def style(key: String, default: fansi.Attrs = fansi.Attrs())(implicit theme: Theme) = theme.style(key, default)(str)

    def blue = fansi.Color.Blue(str)
    def green = fansi.Color.Green(str)
    def yellow = fansi.Color.Yellow(str)
    def red = fansi.Color.Red(str)
  }

}
/* vim:set tw=120: */
