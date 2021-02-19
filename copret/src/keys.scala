package de.qwertyuiop.copret

import ammonite.ops.Path

sealed trait SlideAction
case object Start extends SlideAction
case class Goto(slideIndex: Int) extends SlideAction
case object GotoSelect extends SlideAction
case object Prev extends SlideAction
case object Next extends SlideAction
case object QuickNext extends SlideAction
case object Quit extends SlideAction
case class Interactive(cmd: Vector[String], wd: Path) extends SlideAction
case class Other(code: List[Int]) extends SlideAction

object SlideAction {
  def runForeground(cmd: String*)(implicit wd: Path) = Interactive(cmd.toVector, wd)
}


case class Keymap(bindings: Map[List[Int], SlideAction]) {
  def apply(keycode: List[Int]): SlideAction = bindings.getOrElse(keycode, Other(keycode))

  def extend(newBindings: Map[List[Int], SlideAction]) = Keymap(bindings ++ newBindings)
  def ++(newBindings: Map[List[Int], SlideAction]) = Keymap(bindings ++ newBindings)
}
object Keymap {
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

}

object Key {
  object codes {
    val Esc = 27
    val Backspace = 127
  }
  val Esc = List(codes.Esc)
  val Backspace = List(codes.Backspace)
  val Delete = List(codes.Esc, '[', '3', '~')

  val PageUp = List(codes.Esc, '[', '5', '~')
  val PageDown = List(codes.Esc, '[', '6', '~')

  val Home = List(codes.Esc, '[', 'H')
  val End = List(codes.Esc, '[', 'F')

  val F1 = List(codes.Esc, 'P')
  val F2 = List(codes.Esc, 'Q')
  val F3 = List(codes.Esc, 'R')
  val F4 = List(codes.Esc, 'S')

  val F5 = List(codes.Esc, '1', '5', '~')
  val F6 = List(codes.Esc, '1', '7', '~')
  val F7 = List(codes.Esc, '1', '8', '~')
  val F8 = List(codes.Esc, '1', '9', '~')

  val F9  = List(codes.Esc, '2', '0', '~')
  val F10 = List(codes.Esc, '2', '1', '~')
  val F11 = List(codes.Esc, '2', '3', '~')
  val F12 = List(codes.Esc, '2', '4', '~')

  val Tab = List('\t')

  val Up =    List(codes.Esc, '[', 'A')
  val Down =  List(codes.Esc, '[', 'B')
  val Right = List(codes.Esc, '[', 'C')
  val Left =  List(codes.Esc, '[', 'D')

  def apply(char: Char): List[Int] = List(char.toInt)
}

/* vim:set tw=120: */
