package me.afox.tetriss.data

import java.awt.Color
import scala.language.postfixOps

// Available brick colours
object Bricks {
  sealed abstract class Brick(color: Color)

  case object RED extends Brick(Color.RED)
  case object GREEN extends Brick(Color.GREEN)
  case object BLUE extends Brick(Color.BLUE)
  case object ORANGE extends Brick(Color.ORANGE)
  case object GRAY extends Brick(Color.GRAY)
}

/**
 * Represents a position (either absolute or relative) of a block in the tetris pad or
 * of an element/rotation center/desired new position on a move within a block definition
 *
 * @param column
 * @param row
 */
case class BlockPosition(column: Int, row: Int) {
  def -(right: BlockPosition): BlockPosition = BlockPosition(column - right.column, row - right.row)
  def +(right: BlockPosition): BlockPosition = BlockPosition(column + right.column, row + right.row)
  def unary_! : BlockPosition = BlockPosition(row,-column)
}


object Figures {
  import Bricks._

  case object F_I extends Figure(Seq(BlockPosition(0,0), BlockPosition(0,1), BlockPosition(0,2), BlockPosition(0,3)),
    RED,
    Seq(BlockPosition(0, 1), BlockPosition(0, 2)))
  case object F_LR extends Figure(Seq(BlockPosition(0,0), BlockPosition(0,1), BlockPosition(0,2), BlockPosition(1,2)),
    BLUE, Seq(BlockPosition(1, 1)))
  case object F_LL extends Figure(Seq(BlockPosition(1,0), BlockPosition(1,1), BlockPosition(1,2), BlockPosition(0,2)),
    BLUE, Seq(BlockPosition(1,1)))

  val down = BlockPosition(0,1)
  val left = BlockPosition(-1,0)
  val right = BlockPosition(1,0)

  sealed class Figure(blockPositions: Seq[BlockPosition], brick: Brick, centers: Seq[BlockPosition] = Seq(BlockPosition(0,0))) {
    def minCol = blockPositions.map(b=>b.column).min
    def minRow = blockPositions.map(b=>b.row).min
    def maxCol = blockPositions.map(b=>b.column).max
    def maxRow = blockPositions.map(b=>b.row).max

    /**
      * Rotates around all rotation centers defined
      * @return all possible variants of rotated figure
      */
    def rotates = centers.map(c => rotatedFigure(c))

    /**
      * Moves a figure using supplied delta position
      */
    def move(step: BlockPosition) =
      new Figure(blockPositions.map(b => b + step), brick, centers.map(c => c + step))

    //def +(step: BlockPosition) = move(step)

    /**
      * Returns a Brick (if any) at the (column,row) position
      * @param column
      * @param row
      * @return
      */
    def brickAt(column: Int, row: Int): Option[Brick] =
      blockPositions.find(bp => bp.column == column && bp.row == row)
            .map (_ => brick)

    //def normalize = move(Block(-minCol, -minRow))

    protected def rotatedFigure(center: BlockPosition) =
      new Figure(blockPositions.map(b => !(b - center) + center), brick, centers)

  }
}


