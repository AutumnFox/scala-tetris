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
case class Block(column: Int, row: Int) {
  def -(right: Block) = Block(column - right.column, row - right.row)
  def +(right: Block) = Block(column + right.column, row + right.row)
  def unary_! = Block(-row,column)
}


object Figures {
  import Bricks._
  val down = Block(0,1)
  val left = Block(-1,0)
  val right = Block(1,0)

  sealed class Figure(blocks: Seq[Block], brick: Brick, centers: Seq[Block] = Seq(Block(0,0))) {
    def minCol = blocks.map(b=>b.column).min
    def minRow = blocks.map(b=>b.row).min
    def maxCol = blocks.map(b=>b.column).min
    def maxRow = blocks.map(b=>b.row).min

    /**
      * Rotate around all rotation centers defined
      * @return all possible variants of rotated figure
      */
    def rotates = centers map rotatedFigure

    /**
      * Moves a figure using supplayed delta position
      */
    def move(step: Block) = new Figure(blocks.map(b => b + step), brick, centers.map(c => c + step))
    def +(step: Block) = move(step)

    /**
      * Returns a Brick (if any) at the (column,row) position
      * @param column
      * @param row
      * @return
      */
    def brickAt(column: Int, row: Int): Option[Brick] = blocks.find(b => b.column==column && b.row==row) map (_ => brick)

    //def normalize = move(Block(-minCol, -minRow))

    protected def rotatedFigure(center: Block) = new Figure(blocks.map(b => !(b - center) + center), brick, Figure.this.centers)

  }

  case object F_I extends Figure(Seq(Block(0,0), Block(0,1), Block(0,2), Block(0,3)), RED, Seq(Block(0, 1), Block(0, 2)))
  case object F_LR extends Figure(Seq(Block(0,0), Block(0,1), Block(0,2), Block(1,2)), BLUE, Seq(Block(0, 2)))
  case object F_LL extends Figure(Seq(Block(1,0), Block(1,1), Block(1,2), Block(0,2)), BLUE, Seq(Block(0,1), Block(1,2)))

}


