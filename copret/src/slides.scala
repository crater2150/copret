package de.qwertyuiop.copret
import ammonite.ops._
import Terminal._
import syntax._

case class Presentation(slides: Vector[Slide], meta: Map[String, String] = Map.empty):
  def start(using keymap: Keymap = Keymap.default) =
    Terminal.enterRawMode()
    Terminal.hideCursor()
    run()

  import Presentation._
  def run()(using Keymap) =
    import SlideAction.*
    @annotation.tailrec def rec(p: Presentation, pos: Int, action: SlideAction): Unit =
      action match
        case Start =>
          executeSlide(p, pos)()
          rec(p, 0, waitkey)
        case Next | Other(_) =>
          if pos + 1 < p.slides.size then
            executeSlide(p, pos + 1)()
            rec(p, pos + 1, waitkey)
          else
            rec(p, pos, waitkey)
        case QuickNext =>
          if pos + 1 < p.slides.size then
            executeQuick(p, pos + 1)()
            rec(p, pos + 1, waitkey)
          else rec(p, pos, waitkey)
        case Prev =>
          if pos > 0 then
            executeQuick(p, pos - 1)()
            rec(p, pos - 1, waitkey)
          else
            rec(p, pos, waitkey)
        case Interactive(cmd, path) =>
          Terminal.showCursor()
          try
            %(cmd)(path)
          catch case _ => ()
          Terminal.hideCursor()
          Terminal.clear()
          rec(p, pos - 1, QuickNext)
        case Goto(target) =>
          for i <- 0 until target
          do executeSilent(p, i)()
          rec(p, target - 1, QuickNext)
        case GotoSelect =>
          val maxSlide = p.slides.size - 1
          val target = prompt(s"Go to slide (0 - $maxSlide):", _.toIntOption)(
            (res, input) => res.filter((0 to maxSlide).contains).isEmpty && input.nonEmpty,
            in => s"No such slide: $in (empty input to abort)"
          )
          target match
            case Some(i) => rec(p, pos, Goto(i))
            case None => rec(p, pos - 1, QuickNext)
        case Quit =>
          ()
    rec(this, 0, Start)


object Presentation:
  def executeSlide(p: Presentation, pos: Int)(slide: Slide = p.slides(pos)): Unit = slide match
    case Paragraph(contents) => println(contents)
    case Clear => Terminal.clear()
    case PauseKey => waitkey(using Keymap.empty)
    case Pause(msec) => Thread.sleep(msec)
    case incMd @ IncludeMarkdown(_) => println(incMd.markdownBlock())
    case Image(file, None) => Terminal.showImage(file)
    case Image(file, Some(ImageSize(w, h, aspect))) => Terminal.showImageScaled(file, w, h, aspect)
    case cmd: TypedCommand[_] => cmd.show()
    case Silent(actions) => actions()
    case Group(slides) => slides.foreach(executeSlide(p, pos))
    case lios @ LazyIOSlide(_, display) => executeSlide(p, pos)(lios.genSlide())
    case Meta(genSlide) => executeSlide(p, pos)(genSlide(p, pos))

  def executeQuick(p: Presentation, pos: Int)(slide: Slide = p.slides(pos)): Unit = slide match
    case Pause(msec) => ()
    case PauseKey => ()
    case cmd: TypedCommand[_] => cmd.quickShow()
    case Group(slides) => slides.foreach(executeQuick(p, pos))
    case lios @ LazyIOSlide(_, display) => executeQuick(p, pos)(lios.genSlide())
    case _ => executeSlide(p, pos)(slide)

  def executeSilent(p: Presentation, pos: Int)(slide: Slide = p.slides(pos)): Unit = slide match
    case cmd: TypedCommand[_] => cmd.force()
    case Group(slides) => slides.foreach(executeSilent(p, pos))
    case lios @ LazyIOSlide(_, display) => executeSilent(p, pos)(lios.genSlide())
    case Paragraph(_) | Image(_,_) | Clear | IncludeMarkdown(_) | Meta(_) => ()
    case _ => executeQuick(p, pos)(slide)


case class ImageSize(width: Double, height: Double, keepAspect: Boolean)

sealed trait Slide
case class Paragraph(contents: String) extends Slide:
  def centerVertical(height: Int): Paragraph =
    val lines = contents.toString.count(_ == '\n') + 1
    val pad = "\n" * ((height - lines) / 2)
    Paragraph(pad + contents + pad)

object Paragraph:
  def apply(str: fansi.Str): Paragraph = Paragraph(str.toString)

case class IncludeMarkdown(path: Path) extends Slide:
  def markdownBlock() =
    %%%("/usr/bin/mdcat", "--columns", (columns * 0.8).toInt.toString, path.toString)(using ImplicitWd.implicitCwd).block

case class Image(path: Path, sizing: Option[ImageSize]) extends Slide
object Image:
  def apply(path: Path) = new Image(path, None)
  def scaled(path: Path, width: Double, height: Double, keepAspect: Boolean) =
    Image(path, Some(ImageSize(width, height, keepAspect)))

case object Clear extends Slide
case class Pause(millisec: Long) extends Slide
case object PauseKey extends Slide
case class Meta(contents: (Presentation, Int) => Slide) extends Slide

case class TypedCommand[T](exec: T => String, display: String, cmd: T) extends Slide:
  private lazy val _output = exec(cmd)
  def output = _output

  infix def showing (s: String): TypedCommand[T] = TypedCommand(exec, s, cmd)

  def show() =
    prompt()
    Terminal.showCursor()
    typeCmd()
    print(output)
    Terminal.hideCursor()

  def quickShow() =
    prompt()
    println(display)
    print(output)

  def prompt() = print(fansi.Color.LightGreen("user@host % "))
  def force() = _output

  private def typeCmd() =
    for char <- display do
      print(char)
      Thread.sleep(50 + scala.util.Random.nextInt(80))
    println()

  /* Conditionally disable execution. Useful for e.g. a debug mode, or a non-interactive mode */
  def disable(altDisplay: String = display, condition: Boolean = true) =
    if condition then copy(display = altDisplay, exec = (_:T) => "")
    else this

  /* Conditionally replace the executed command (but still displaying the same). Useful for e.g. a non-interactive mode,
   * where a call to an editor is replaced with a file operation */
  def replaceIf(condition: Boolean)(tc: TypedCommand[_]): TypedCommand[_] =
    if condition then tc.showing(display)
    else this


object TypedCommand:
  val shell = sys.env.getOrElse("SHELL", "sh")

  def run(using Path): Vector[String] => String =
    c => safe_%%(c)

  def runShell(using Path): Vector[String] => String =
    c => safe_%%(Vector(shell, "-c", c.mkString(" ")))

  def runInteractive(using Path): Vector[String] => String =
    c => { %(c); ""}

  def apply(cmd: String*)(using Path): TypedCommand[Vector[String]] =
    TypedCommand(run, cmd.mkString(" "), cmd.toVector)

  def shell(cmd: String*)(using Path): TypedCommand[Vector[String]] =
    TypedCommand(runShell, cmd.mkString(" "), cmd.toVector)

  def fake(cmd: String): TypedCommand[String] =
    TypedCommand(_ => "", cmd, cmd)

  def interactive(cmd: String*)(using Path): TypedCommand[Vector[String]] =
    TypedCommand(runInteractive, cmd.mkString(" "), cmd.toVector)


sealed abstract case class Silent[T] private (doStuff: () => T) extends Slide
object Silent:
  def apply[T](doStuff: => T) = new Silent(() => doStuff){}


case class Group(slides: List[Slide]) extends Slide
object Group:
  def apply(slides: Slide*): Group = Group(slides.toList)

case class LazyIOSlide[T](runOnce: () => T, display: T => Slide) extends Slide:
  private lazy val data = runOnce()
  def genSlide(): Slide = display(data)

trait SlideSyntax:
  private[copret] class LazyIOSlideBuilder[T](runOnce: => T):
    def useIn(display: T => Slide) = LazyIOSlide(() => runOnce, display)

  def prepare[T](runOnce: => T): LazyIOSlideBuilder[T] = LazyIOSlideBuilder(runOnce)


/* vim:set tw=120: */
