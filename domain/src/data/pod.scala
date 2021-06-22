package  me.afox.tetriss.data

import Bricks._
import Figures._

import scala.collection.immutable.Stream.Empty
import scala.language.postfixOps

case class Pod(lines: Pod.Lines = Pod.empty.lines, position: Block = Block(4,1), figure: Option[Figure] = None) {

  def inject(f: Figure) = new Pod(lines = lines, figure = Some(f + position))
  def stepDown = new Pod(lines = lines, figure = Some(figure.get.move(down)))
  def rotate = new Pod(lines = lines, figure = Some(figure.get.rotates.head))

  def aggregatePod[T_AGGREGATED_ROW,T_AGGREGATED_CELL,T_CELL] (
                            cellExtractor: (Int, Int, Option[Brick]) => T_CELL,

                            cellBase: T_AGGREGATED_CELL,
                            cellAggregator: (T_CELL,T_AGGREGATED_CELL) => T_AGGREGATED_CELL,

                            rowBase: T_AGGREGATED_ROW,
                            rowAggregator: (T_AGGREGATED_CELL, T_AGGREGATED_ROW) => T_AGGREGATED_ROW): T_AGGREGATED_ROW = {

    def aggregateRow(rowNumber: Int = 0, restOfRows: Pod.Lines): T_AGGREGATED_ROW = {
      def aggregateCell(colNumber: Int = 0, rowNumber: Int, restOfCells: Pod.Row): T_AGGREGATED_CELL =  restOfCells match {
          case (head :: Seq()) => cellAggregator(cellExtractor(colNumber, rowNumber, head), cellBase)
          case (head :: tail)  => cellAggregator(cellExtractor(colNumber, rowNumber, head),
                                                 aggregateCell(colNumber + 1, rowNumber, tail))
      }

      restOfRows match {
        case (head :: Seq()) => rowAggregator(aggregateCell(0, rowNumber, head), rowBase)
        case (head :: tail)  => rowAggregator(aggregateCell(0, rowNumber, head), aggregateRow(rowNumber + 1, tail))
      }
    }

    aggregateRow(0,lines)
  }

  def testCommit: Boolean = {
    figure.forall(
      f => f.maxRow < Pod.ROWS && f.maxCol < Pod.COLUMNS &&
        aggregatePod(
          cellExtractor = (colNumber: Int, rowNumber: Int, rowHead: Option[Brick]) => figure.get.brickAt(colNumber, rowNumber).isEmpty || rowHead.isEmpty,

          cellBase = true,
          cellAggregator = (rowHead: Boolean, aggregatedRow: Boolean) => rowHead && aggregatedRow,

          rowBase = true,
          rowAggregator  = (podHead: Boolean, aggregatedPod: Boolean) => podHead && aggregatedPod
        ))
  }


  def commit : Pod = {
    if(figure.isEmpty) return this

    new Pod(aggregatePod(
      cellExtractor = (colNumber: Int, rowNumber: Int, rowHead: Option[Brick]) => figure.get.brickAt(colNumber, rowNumber) orElse rowHead,

      cellBase = Seq(),
      cellAggregator = (cell: Option[Brick], aggregatedCells: Pod.Row) => cell +: aggregatedCells,

      rowBase  = Seq(),
      rowAggregator = (row: Pod.Row, aggregatedRows: Seq[Pod.Row]) => row +: aggregatedRows
    ))
  }
}

object Pod {
  type Row = Seq[Option[Brick]]
  type Lines = Seq[Row]

  val ROWS = 19
  val COLUMNS = 9

  /**
    * New empty pod
    * @return
    */
  def empty: Pod = {
    def addEmptyLines(rowNumber: Int = 0): Lines = {
      if(rowNumber == ROWS) {
        Seq(Seq.fill(COLUMNS+1)(None))
      } else {
        Seq.fill(COLUMNS+1)(None) +: addEmptyLines(rowNumber+1)
      }
    }
    new Pod(addEmptyLines())
  }

}