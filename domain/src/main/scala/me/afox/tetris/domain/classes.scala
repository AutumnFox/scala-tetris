package me.afox.tetris.domain

import java.awt.Color

/**
 * Represents a position (either absolute or relative) of a block in the tetris pad or
 * of an element/rotation center/desired new position on a move within a block definition
 *
 * @param column number of column
 * @param row number of row
 */
case class XY(column: Int, row: Int) {
  def sub(right: XY): XY = XY(column - right.column, row - right.row)
  def sum(right: XY): XY = XY(column + right.column, row + right.row)
  def rotate : XY = XY(row,-column)
}

case class Figure(blockPositions: Seq[XY], brick: Color, centers: Seq[XY] = Seq(XY(0,0))) {
  def minCol: Int = blockPositions.map(b=>b.column).min
  def minRow: Int = blockPositions.map(b=>b.row).min
  def maxCol: Int = blockPositions.map(b=>b.column).max
  def maxRow: Int = blockPositions.map(b=>b.row).max

  /**
    * Rotates around all rotation centers defined
    * @return all possible variants of rotated figure
    */
  def rotates: Seq[Figure] = centers.map(c => rotatedFigure(c))

  /**
    * Moves a figure using supplied delta position
    */
  def move(step: XY): Figure = Figure(blockPositions.map(b => b sum step), brick, centers.map(c => c sum step))

  //def +(step: XY) = move(step)

  /**
    * Returns a Brick (if any) at the (column,row) position
    * @param column column number
    * @param row row number
    * @return
    */
  def brickAt(column: Int, row: Int): Option[Color] =
    blockPositions.find(bp => bp.column == column && bp.row == row)
      .map (_ => brick)

  protected def rotatedFigure(center: XY): Figure = Figure(blockPositions.map(b => b.sub(center).rotate.sum(center)), brick, centers)
}
