package me.afox.tetriss.console

import me.afox.tetriss.data.{Figures, Pod}
import org.fusesource.jansi.{ AnsiConsole, Ansi }
//import scala.language.postfixOps

trait Renderer[T] {
  def render: Seq[T]
}
trait ConsoleRenderer extends Renderer[String]

object ConsoleTools {
  def out(renderer: ConsoleRenderer) = {
    renderer.render
      .foreach (s => println("["+s+"]"))
  }
}

object ConsoleBootstrap {
  implicit class ConsolePodRenderer(data: Pod) extends ConsoleRenderer {
    def render = data.lines.map(line =>
      line.map(cell =>
        cell.map(brick => "#").getOrElse("_")
      ).mkString("|")
    )
  }

  def letsFall(nextStep: Pod): Pod = {
    AnsiConsole.out().println(Ansi.ansi().eraseScreen())
    AnsiConsole.out().println(Ansi.ansi().cursor(0,0).fg(Ansi.Color.RED).a("VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"))
    //AnsiConsole.out().println("===================================================================================")

    ConsoleTools.out(ConsolePodRenderer(nextStep.commit))
    println("_____________________________________________________________")

    Thread.sleep(100)

    if(nextStep.stepDown.testCommit) letsFall(nextStep.stepDown)
    else nextStep.commit
  }

  def main(args: Array[String]): Unit = {
    AnsiConsole.systemInstall

    val pod = new Pod() inject Figures.F_LL

    val move1 = letsFall(pod.rotate)
    val move2 = letsFall(move1.inject(Figures.F_LR))
    letsFall(move2
      .inject(Figures.F_I)
      .rotate)

  }
}




