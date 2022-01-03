package me.afox.tetris.domain

import java.awt.Color


object Figures extends Enumeration {
  protected case class Val(figure: Figure) extends super.Val {}

  val F_I: Val = Val(Figure(Seq(XY(0,0), XY(0,1), XY(0,2), XY(0,3)), Color.RED,  Seq(XY(0, 1), XY(0, 2))))
  val F_LR: Val = Val(Figure(Seq(XY(0,0), XY(0,1), XY(0,2), XY(1,2)), Color.BLUE, Seq(XY(1, 1))))
  val F_LL: Val = Val(Figure(Seq(XY(1,0), XY(1,1), XY(1,2), XY(0,2)), Color.GREEN, Seq(XY(1,1))))

  def random: Val = Figures.apply(Math.round(Math.random()*(Figures.maxId-1)).toInt).asInstanceOf[Val]
}

object xxx {

}


