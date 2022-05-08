package de.qwertyuiop.copret

object syntax extends Templates with TerminalSyntax with SlideSyntax:
  import Format._
  extension (str: String)
    def code(using theme: Theme) = Format.ticks.replaceAllIn(str, m => theme.style("code")("$1").render)
    def text(using Theme) = Paragraph(str)
    def par(using Theme) = Paragraph(str.stripMargin.code.padLeft(2))
    def style(key: String, default: fansi.Attrs)(using theme: Theme) = theme.style(key, default)(str)

    def centered = center(str)
    def block = centerBlock(str)
    def right = alignRight(str)
    def right(padding: Int) = alignRight(str, padding)
    def padLeft(padding: Int) =
      val pad = " " * padding
      str.linesIterator.map(pad + _).mkString("\n")

    def blue = fansi.Color.Blue(str)
    def green = fansi.Color.Green(str)
    def yellow = fansi.Color.Yellow(str)
    def red = fansi.Color.Red(str)

  extension (str: fansi.Str)
    def text(using Theme) = Paragraph(str)
    def style(key: String, default: fansi.Attrs)(using theme: Theme) = theme.style(key, default)(str)

    def blue = fansi.Color.Blue(str)
    def green = fansi.Color.Green(str)
    def yellow = fansi.Color.Yellow(str)
    def red = fansi.Color.Red(str)

/* vim:set tw=120: */
