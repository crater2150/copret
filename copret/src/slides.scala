package de.qwertyuiop.copret
import ammonite.ops._
import Terminal._
import syntax._

case class Presentation(slides: Vector[Slide], meta: Map[String, String] = Map.empty) {
  def start(keymap: Keymap = Keymap.default) = {
    Terminal.enterRawMode()
    run(keymap)
  }

  import Presentation._
  def run(implicit k: Keymap) = {
    @annotation.tailrec def rec(p: Presentation, pos: Int, action: SlideAction): Unit = {
      action match {
        case Start =>
          executeSlide(p, pos)()
          rec(p, 1, waitkey)
        case Next | Other(_) =>
          if(pos + 1 < p.slides.size) {
            executeSlide(p, pos + 1)()
            rec(p, pos + 1, waitkey)
          } else rec(p, pos, waitkey)
        case QuickNext =>
          if(pos + 1 < p.slides.size) {
            executeQuick(p, pos + 1)()
            rec(p, pos + 1, waitkey)
          } else rec(p, pos, waitkey)
        case Prev =>
          if(pos > 0) {
            executeQuick(p, pos - 1)()
            rec(p, pos - 1, waitkey)
          } else rec(p, pos, waitkey)
        case Interactive(cmd, path) =>
          %(cmd)(path)
          rec(p, pos - 1, QuickNext)
        case Goto(target) =>
          for (i <- 0 until target) executeSilent(p, i)()
          rec(p, target - 1, QuickNext)
        case GotoSelect =>
          val maxSlide = p.slides.size - 1
          val target = prompt(s"Go to slide (0 - $maxSlide):", _.toIntOption)(
            (res, input) => res.filter((0 to maxSlide).contains).isEmpty && input.nonEmpty,
            in => s"No such slide: $in (empty input to abort)"
          )
          target match {
            case Some(i) => rec(p, pos, Goto(i))
            case None => rec(p, pos - 1, QuickNext)
          }
        case Quit => ()
      }
    }
    rec(this, 0, Start)
  }
}
object Presentation {
  def executeSlide(p: Presentation, pos: Int)(slide: Slide = p.slides(pos)): Unit = slide match {
    case Paragraph(contents) => println(contents)
    case Clear => print("\u001b[2J\u001b[;H")
    case PauseKey => waitkey(Keymap.empty)
    case Pause(msec) => Thread.sleep(msec)
    case incMd @ IncludeMarkdown(_) => println(incMd.markdownBlock())
    case Image(file, width, height, keepAspect) => print(Terminal.showImage(file, width, height, keepAspect))
    case cmd: TypedCommand[_] => cmd.show()
    case Silent(actions) => actions()
    case Group(slides) => slides.foreach(executeSlide(p, pos))
    case lios @ LazyIOSlide(_, display) => executeSlide(p, pos)(lios.genSlide())
    case Meta(genSlide) => executeSlide(p, pos)(genSlide(p, pos))
    case other => println("Error: Unknown slide type:"); println(other)
  }

  def executeQuick(p: Presentation, pos: Int)(slide: Slide = p.slides(pos)): Unit = slide match {
    case Pause(msec) => ()
    case PauseKey => ()
    case cmd: TypedCommand[_] => cmd.quickShow()
    case Group(slides) => slides.foreach(executeQuick(p, pos))
    case lios @ LazyIOSlide(_, display) => executeQuick(p, pos)(lios.genSlide())
    case _ => executeSlide(p, pos)(slide)
  }

  def executeSilent(p: Presentation, pos: Int)(slide: Slide = p.slides(pos)): Unit = slide match {
    case cmd: TypedCommand[_] => cmd.force()
    case Group(slides) => slides.foreach(executeSilent(p, pos))
    case lios @ LazyIOSlide(_, display) => executeSilent(p, pos)(lios.genSlide())
    case Paragraph(_) | Image(_,_,_,_) | Clear | IncludeMarkdown(_) | Meta(_) => ()
    case _ => executeQuick(p, pos)(slide)
  }
}


sealed trait Slide
case class Paragraph(contents: fansi.Str) extends Slide
case class IncludeMarkdown(path: Path) extends Slide {
  def markdownBlock() = %%%("/usr/bin/mdcat", "--columns", (columns * 0.8).toInt.toString, path.toString)(pwd).block
}
case class Image(path: Path, width: String = "100%", height: String = "100%", keepAspect: Boolean = true) extends Slide
case object Clear extends Slide
case class Pause(millisec: Long) extends Slide
case object PauseKey extends Slide
case class Meta(contents: (Presentation, Int) => Slide) extends Slide


case class TypedCommand[T](exec: T => String, display: String, cmd: T) extends Slide {
  private lazy val _output = exec(cmd)
  def output = _output
  def display(s: String): TypedCommand[T] = TypedCommand(exec, s, cmd)

  def show() = {
    prompt()
    typeCmd()
    print(output)
  }

  def quickShow() = {
    prompt()
    println(display)
    print(output)
  }

  def prompt() = print(fansi.Color.LightGreen("user@host % "))
  def force() = _output

  private def typeCmd() = {
    for (char <- display) {
      print(char)
      Thread.sleep(50 + scala.util.Random.nextInt(80))
    }
    println()
  }

  /* Conditionally disable execution. Useful for e.g. a debug mode, or a non-interactive mode */
  def disable(altDisplay: String = display, condition: Boolean = true) =
    if(condition) copy(display = altDisplay, exec = (_:T) => "")
    else this

  /* Conditionally replace the executed command (but still displaying the same). Useful for e.g. a non-interactive mode,
   * where a call to an editor is replaced with a file operation */
  def replaceIf(condition: Boolean)(tc: TypedCommand[_]): TypedCommand[_] =
    if(condition) tc.display(display)
    else this

}

object TypedCommand {
  val shell = sys.env.getOrElse("SHELL", "sh")

  def run(implicit wd: Path): Vector[String] => String =
    c => safe_%%(c)

  def runShell(implicit wd: Path): Vector[String] => String = 
    c => safe_%%(Vector(shell, "-c", c.mkString(" ")))

  def runInteractive(implicit wd: Path): Vector[String] => String = 
    c => { %(c); ""}

  def apply(cmd: String*)(implicit wd: Path): TypedCommand[Vector[String]] = 
    TypedCommand(run, cmd.mkString(" "), cmd.toVector)

  def shell(cmd: String*)(implicit wd: Path): TypedCommand[Vector[String]] = 
    TypedCommand(runShell, cmd.mkString(" "), cmd.toVector)

  def fake(cmd: String): TypedCommand[String] = 
    TypedCommand(_ => "", cmd, cmd)

  def interactive(cmd: String*)(implicit wd: Path): TypedCommand[Vector[String]] = 
    TypedCommand(runInteractive, cmd.mkString(" "), cmd.toVector)
}

sealed abstract case class Silent[T] private (doStuff: () => T) extends Slide
object Silent { def apply[T](doStuff: => T) = new Silent(() => doStuff){} }


case class Group(slides: List[Slide]) extends Slide
object Group { def apply(slides: Slide*): Group = Group(slides.toList) }

case class LazyIOSlide[T](runOnce: () => T, display: T => Slide) extends Slide {
  private lazy val data = runOnce()
  def genSlide(): Slide = display(data)
}

trait SlideSyntax {
  private[copret] class LazyIOSlideBuilder[T](runOnce: => T) {
    def useIn(display: T => Slide) = LazyIOSlide(() => runOnce, display)
  }

  def prepare[T](runOnce: => T): LazyIOSlideBuilder[T] = new LazyIOSlideBuilder(runOnce)
}


/* vim:set tw=120: */
