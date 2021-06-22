package me.afox.tetriss.ui

import scala.swing._

import scala.language.postfixOps

abstract class BasePane extends BoxPanel(Orientation.Vertical) {
  def HEIGHT = 320 * 3
  def WIDTH : Int
  def initContent

  border = Swing.CompoundBorder(Swing.BeveledBorder(Swing.Lowered),
    Swing.EmptyBorder(10, 10, 10, 10))

  minimumSize = new Dimension(WIDTH, HEIGHT)
  maximumSize = minimumSize

  border = Swing.CompoundBorder(Swing.BeveledBorder(Swing.Lowered),
    Swing.EmptyBorder(10, 10, 10, 10))

  initContent
}

class LeftPane extends BasePane {

  override def WIDTH = 320

  override def initContent: Unit = {
    contents += new Label("Left Pane!")
    contents += Button("Press me, please") {
      println("Thank you")
    }
    contents += Button("Close") {
      sys.exit(0)
    }
  }
}


class RightPane extends BasePane {
  override def  WIDTH = 320 * 2

  override def initContent: Unit = {
    contents += new Label("Right Pane!")
    contents += Button("Press me, please") {
      println("Thank you")
    }
    contents += Button("Close") {
      sys.exit(0)
    }
  }
}
