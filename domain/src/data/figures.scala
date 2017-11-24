package me.afox.tetriss.data

import java.awt.Color

// Available brick colours
object Bricks {
  sealed abstract class Brick(color: Color)

  case object RED extends Brick(Color.RED)
  case object GREEN extends Brick(Color.GREEN)
  case object BLUE extends Brick(Color.BLUE)
  case object ORANGE extends Brick(Color.ORANGE)
  case object GRAY extends Brick(Color.GRAY)
}

// Represent a position/coordinate in the tetris pad
case class Block(col: Int, row: Int) {
  def -(right: Block) = Block(col - right.col, row - right.row)
  def +(right: Block) = Block(col + right.col, row + right.row)
  def unary_! = Block(-row,col)
}


object Figures {
  import Bricks._
  sealed class Figure(blocks: Seq[Block], brick: Brick, centers: Seq[Block] = Seq(Block(0,0))) {
    def allRotates: Seq[Figure] = {
      centers map rotatedFigure
    }

    def normalize = {
      val minx = blocks.map(b=>b.col).min
      val miny = blocks.map(b=>b.row).min
      val move = Block(-minx, -miny)

      new Figure(blocks.map(b=>b+move), brick, centers)
    }

    // Move a figure using supplayed delta position
    def + (delta: Block) = new Figure(blocks map(b => b + delta), brick,centers)

    protected def rotatedFigure(center: Block) = new Figure(blocks.map(b => !(b - center) + center), brick, Figure.this.centers)

    def brikAt(x: Int,y: Int) = {
      blocks.find(b=> b.col==x && b.row==y)
    }
  }

  case object F_I extends Figure(Seq(Block(0,0), Block(0,1), Block(0,2), Block(0,3)), RED, Seq(Block(0, 1), Block(0, 2)))
  case object F_LR extends Figure(Seq(Block(0,0), Block(0,1), Block(0,2), Block(1,2)), BLUE, Seq(Block(0, 2)))
  case object F_LL extends Figure(Seq(Block(1,0), Block(1,1), Block(1,2), Block(0,2)), BLUE, Seq(Block(0,1), Block(1,2)))

}

object objects {
  import Figures._

  def p(f: Figure): Unit = {
    for (y <- 0 to 8) {
      print("|")
      for (x <- 0 to 8) {
        print(f.brikAt(x,y).flatMap(b=>Option[String]("*|")).orElse(Option("_|")).get)
      }
      print("\n")
    }
    print("________________________________________\n")
  }
  def main(args: Array[String]): Unit = {
    val delta = Block(4,2)
    p(F_LR+delta);

    val r1 = F_LR.allRotates.head
    val r2 = r1.allRotates.head
    val r3 = r2.allRotates.head
    val r4 = r3.allRotates.head

    Seq(r1,r2,r3,r4) map(f=>f+delta) foreach p
  }
}
