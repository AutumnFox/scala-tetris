package me.afox.tetriss
import scala.swing._

import me.afox.tetriss.ui._
import scala.language.postfixOps

object Bootstrap {
  def main(args: Array[String]): Unit = {
    println("Start of main function")
    val ui = new UI
    ui.visible = true
    println("End of main function")
  }

}

class UI extends MainFrame {
  val leftPane = new LeftPane
  val rightPane = new RightPane

  title = "GUI Program #1"
  preferredSize = new Dimension(leftPane.HEIGHT, leftPane.WIDTH + rightPane.WIDTH)
  minimumSize = preferredSize
  maximumSize = maximumSize

  contents = new BoxPanel(Orientation.Horizontal) {
    contents += new LeftPane()
    contents += new RightPane()
  }

}


