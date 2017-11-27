package me.afox.tetriss.console

import me.afox.tetriss.console.ConsoleTools.Renderers
import me.afox.tetriss.data.{Figures, Pod}

object ConsoleTools {
  trait ConsoleRenderer {
    def render: Seq[String]
  }

  // wrapped class
  class PodRenderer(data: Pod) extends ConsoleRenderer {
    def render = data.lines.map(line =>
        line.map(cell =>
          cell.map(brick => "#").getOrElse("_")
        ).mkString("|")
      )
  }

  def out(renderer: ConsoleRenderer) = {
    renderer.render foreach (s => println("|"+s+"|"))
  }

  object Renderers {
    implicit def wrap(pod: Pod) = new PodRenderer(pod)
  }
}

object ConsoleBootstrap {
  import Renderers._

  def main(args: Array[String]): Unit = {
    val pod = new Pod() inject Figures.F_LL
    val move1 = pod.stepDown.stepDown.stepDown.stepDown.stepDown.stepDown.rotate.commit

    ConsoleTools.out(move1)
    println("_____________________________________________________________")

    val move2 = (move1 inject Figures.F_LR).rotate.commit

    ConsoleTools.out(move2)

  }
}




