package de.qwertyuiop.copret
import ammonite.ops.{Path, ShelloutException, pwd, read, %, %%}
import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder

object Terminal {
  def safe_%%(cmd: Vector[String])(implicit wd: Path): String =
    try {
      %%(cmd).out.string
    } catch {
      case e: ShelloutException => e.result.err.string
    }

    
  def tryCmd[T](cmd: => T, default: => T) =
    try { cmd } catch { case e: ShelloutException => default }

  def enterRawMode(): Unit = {
    %%("sh", "-c", "stty -icanon min 1 < /dev/tty")(pwd)
    %%("sh", "-c", "stty -echo < /dev/tty")(pwd)
  }

  private[copret] lazy val jterm = org.jline.terminal.TerminalBuilder.terminal()
  private[copret] lazy val lineReader = LineReaderBuilder.builder().terminal(jterm).build()

  def waitkey(implicit keymap: Keymap): SlideAction = {
    // ignore keypresses done during slide animations
    while(Console.in.ready()) Console.in.read

    var key = scala.collection.mutable.ArrayBuffer[Int]()
    key += Console.in.read
    while(Console.in.ready)
      key += Console.in.read
    keymap(key.toList)
  }

  def prompt[T](prefix: String, parse: String => T)(
      retry: (T, String) => Boolean = (t: T, s: String) => false,
      error: String => String = in => s"Invalid input: $in"
    ): T = {
      val input = lineReader.readLine(prefix + " ")
      val result = parse(input)
      if(retry(result, input)) {
        println(error(input))
        prompt(prefix, parse)(retry, error)
      }
      else result
  }

  def isTmux = sys.env.contains("TMUX") || sys.env("TERM").startsWith("screen")

  def osc = if (isTmux) "\u001bPtmux\u001b\u001b]" else "\u001b]"
  def st = if (isTmux) "\u0007\u001b\\" else "\u0007"

  def showImage(img: Path, width: String = "100%", height: String = "100%", keepAspect: Boolean = true) = {
    import java.util.Base64
    val image = Base64.getEncoder.encodeToString(read.bytes(img))
    val aspect = if(keepAspect) 1 else 0
    s"${osc}1337;File=inline=1;width=$width;height=$height;preserveAspectRatio=$aspect:$image$st"
  }
}

private[copret] trait TerminalSyntax {
  import Terminal._

  def %%%(cmd: String*)(implicit wd: Path) = safe_%%(cmd.toVector)
  def columns = jterm.getSize.getColumns
  def rows = jterm.getSize.getRows

}

/* vim:set tw=120: */
