package de.qwertyuiop.copret

import ammonite.ops.Path

enum SlideAction:
  case Start
  case Goto(slideIndex: Int)
  case GotoSelect
  case Prev
  case Next
  case QuickNext
  case Quit
  case Help
  case Interactive(cmd: Vector[String], wd: Path)
  case Other(code: List[Int])

  def show: String = this match
    case Start => "go to first slide"
    case Goto(slideIndex: Int) => s"jump directly to slide $slideIndex"
    case GotoSelect => "jump to slide"
    case Prev => "previous slide (skip animations)"
    case Next => "next slide"
    case QuickNext => "next slide (skip animations)"
    case Quit => "quit"
    case Help => "show help"
    case Interactive(cmd: Vector[String], wd: Path) => s"execute command \"${cmd.mkString(" ")}\""
    case Other(code: List[Int]) => s"Unknown key sequence: $code"


object SlideAction:
  def runForeground(cmd: String*)(implicit wd: Path) = Interactive(cmd.toVector, wd)

import SlideAction.*

case class Keymap(bindings: Map[Key, SlideAction]):
  private val lookup = bindings.map((k,v) => k.codes -> v)
  def apply(keycode: List[Int]): SlideAction = lookup.getOrElse(keycode, Other(keycode))

  def extend(newBindings: Map[Key, SlideAction]) = Keymap(bindings ++ newBindings)
  def ++(newBindings: Map[Key, SlideAction]) = extend(newBindings)
  def help: String =
    bindings.toSeq.sortBy(_._2.toString).map((k,v) => k.show.padTo(8, ' ') + " " + v.show).mkString("\n")

object Keymap:
 val empty = Keymap(Map())
 val default = Keymap(Map(
  Key.Up        -> Prev,
  Key.Left      -> Prev,
  Key.PageUp    -> Prev,
  Key('k')      -> Prev,
  Key.Space     -> Next,
  Key('j')      -> Next,
  Key.Down      -> QuickNext,
  Key.Right     -> QuickNext,
  Key.PageDown  -> QuickNext,
  Key('q')      -> Quit,
  Key('g')      -> Start,
  Key.Enter     -> Start,
  Key('s')      -> GotoSelect,
  Key('?')      -> Help,
  ))

enum Key:
  case Code(name: String, codepoints: List[Int])
  case Printable(char: Char)

  def codes: List[Int] =
    this match
      case Code(_, cp) => cp
      case Printable(char) => List(char.toInt)

  def show: String =
    this match
      case Code(name, _) => name
      case Printable(c) => c.toString

object Key:
  def apply(char: Char): Key = Printable(char)
  def apply(name: String, codepoints: Int*): Key = Code(name, codepoints.toList)
  object codes:
    val esc = 27
    val backspace = 127

  val Esc = Key("Escape", codes.esc)
  val Backspace = Key("Backspace", codes.backspace)
  val Delete = Key("Delete", codes.esc, '[', '3', '~')

  val PageUp = Key("PageUp", codes.esc, '[', '5', '~')
  val PageDown = Key("PageDown", codes.esc, '[', '6', '~')

  val Home = Key("Home", codes.esc, '[', 'H')
  val End = Key("End", codes.esc, '[', 'F')

  val F1 = Key("F1", codes.esc, 'P')
  val F2 = Key("F2", codes.esc, 'Q')
  val F3 = Key("F3", codes.esc, 'R')
  val F4 = Key("F4", codes.esc, 'S')

  val F5 = Key("F5", codes.esc, '1', '5', '~')
  val F6 = Key("F6", codes.esc, '1', '7', '~')
  val F7 = Key("F7", codes.esc, '1', '8', '~')
  val F8 = Key("F8", codes.esc, '1', '9', '~')

  val F9 = Key("F9", codes.esc, '2', '0', '~')
  val F10 = Key("F10", codes.esc, '2', '1', '~')
  val F11 = Key("F11", codes.esc, '2', '3', '~')
  val F12 = Key("F12", codes.esc, '2', '4', '~')

  val Space = Key("<space>", ' ')
  val Tab = Key("<tab>", '\t')
  val Enter = Key("<enter>", '\n')

  val Up = Key("Up", codes.esc, '[', 'A')
  val Down = Key("Down", codes.esc, '[', 'B')
  val Right = Key("Right", codes.esc, '[', 'C')
  val Left = Key("Left", codes.esc, '[', 'D')

/* vim:set tw=120: */
