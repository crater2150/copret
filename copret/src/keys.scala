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
  case Interactive(cmd: Vector[String], wd: Path)
  case Other(code: List[Int])

object SlideAction:
  def runForeground(cmd: String*)(implicit wd: Path) = Interactive(cmd.toVector, wd)

import SlideAction.*

case class Keymap(bindings: Map[List[Int], SlideAction]):
  def apply(keycode: List[Int]): SlideAction = bindings.getOrElse(keycode, Other(keycode))

  def extend(newBindings: Map[List[Int], SlideAction]) = Keymap(bindings ++ newBindings)
  def ++(newBindings: Map[List[Int], SlideAction]) = Keymap(bindings ++ newBindings)
object Keymap:
 val empty = Keymap(Map())
 val default = Keymap(Map(
  Key.Up        -> Prev,
  Key.Left      -> Prev,
  Key.PageUp    -> Prev,
  Key('k')      -> Prev,
  Key(' ')      -> Next,
  Key('j')      -> Next,
  Key.Down      -> QuickNext,
  Key.Right     -> QuickNext,
  Key.PageDown  -> QuickNext,
  Key('q')      -> Quit,
  Key('g')      -> Start,
  Key('s')      -> GotoSelect,
  ))


object Key:
  object codes:
    val Esc = 27
    val Backspace = 127
  val Esc = List[Int](codes.Esc)
  val Backspace = List[Int](codes.Backspace)
  val Delete = List[Int](codes.Esc, '[', '3', '~')

  val PageUp = List[Int](codes.Esc, '[', '5', '~')
  val PageDown = List[Int](codes.Esc, '[', '6', '~')

  val Home = List[Int](codes.Esc, '[', 'H')
  val End = List[Int](codes.Esc, '[', 'F')

  val F1 = List[Int](codes.Esc, 'P')
  val F2 = List[Int](codes.Esc, 'Q')
  val F3 = List[Int](codes.Esc, 'R')
  val F4 = List[Int](codes.Esc, 'S')

  val F5 = List[Int](codes.Esc, '1', '5', '~')
  val F6 = List[Int](codes.Esc, '1', '7', '~')
  val F7 = List[Int](codes.Esc, '1', '8', '~')
  val F8 = List[Int](codes.Esc, '1', '9', '~')

  val F9  = List[Int](codes.Esc, '2', '0', '~')
  val F10 = List[Int](codes.Esc, '2', '1', '~')
  val F11 = List[Int](codes.Esc, '2', '3', '~')
  val F12 = List[Int](codes.Esc, '2', '4', '~')

  val Tab = List[Int]('\t')

  val Up =    List[Int](codes.Esc, '[', 'A')
  val Down =  List[Int](codes.Esc, '[', 'B')
  val Right = List[Int](codes.Esc, '[', 'C')
  val Left =  List[Int](codes.Esc, '[', 'D')

  def apply(char: Char): List[Int] = List(char.toInt)

/* vim:set tw=120: */
