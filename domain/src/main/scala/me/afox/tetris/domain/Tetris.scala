package  me.afox.tetris.domain

import java.awt.Color
import scala.annotation.tailrec

//import scala.language.postfixOps

object GameEvents extends Enumeration {
  type GameEvent = Value
  val ROTATE, LEFT, RIGHT, DOWN, DROP, EXIT = Value
}

case class Tetris(lines: Tetris.Lines = Tetris.empty.lines,
                  //position: XY = XY(4,1),
                  figure: Figure = Figures.random.figure.move(XY(4,1))) {

  val width: Int = lines.head.size
  val height: Int = lines.size

  val nextFigure: Figure = Figures.random.figure.move(XY(4, 1))

  //def inject(f: Figure) = new Tetris(lines = lines, figure =f.move(position))

  def stepDown = new Tetris(lines = lines, figure = figure.move(XY(0, 1)))

  def stepRight = new Tetris(lines = lines, figure = figure.move(XY(1, 0)))

  def stepLeft = new Tetris(lines = lines, figure = figure.move(XY(-1, 0)))

  def rotate = new Tetris(lines = lines, figure = figure.rotates.head) // TODO replace rotates.head with first appropriate rotation

  /**
    * Recursively applies cellAggregator and rowAggregator functions on the current Tetris state,
    * the current figure and the current position. Result is depended on the aggregator functions.
    *
    * 2 use cases of the generic function here:
    * 1. A Boolean - true if the current figure position is free and the figure may be merged into the pod
    * 2. A new Tetris state - makes a pod with the figure merged
    *
    * @param cellExtractor  is used to calculate/map a cell value from the Tetris cell state and the Figure block at the cell
    *                       position
    * @param rowBase        is a seed value to calculate aggregated pod value
    * @param cellAggregator is a mapper providing a result from merging a cell/figure block into the current cell
    * @param cellBase       is a seed value to create an aggregated row value
    * @param rowAggregator  is a mapper providing a result from merging cellAggregator domain into row sequence
    * @tparam MAPPED_CELL      - type of cellExtractor work (used to map a cell content to a value usable for further opperations
    * @tparam AGGREGATED_CELLS - type of the cellAggregator result
    * @tparam AGGREGATED_ROWS - type of #rowAggregator result
    * @return new value of applying the current figure in the current position on the current pod state
    */
  def aggregate[AGGREGATED_ROWS, AGGREGATED_CELLS, MAPPED_CELL](
          cellExtractor: ( /* collNumber*/ Int, /* rowNumber*/ Int, /* rowHead (AKA cell) */ Option[Color]) => MAPPED_CELL,
          cellAggregator: (MAPPED_CELL, AGGREGATED_CELLS) => AGGREGATED_CELLS,
          rowAggregator: (AGGREGATED_CELLS, AGGREGATED_ROWS) => AGGREGATED_ROWS
      )(cellBase: AGGREGATED_CELLS, rowBase: AGGREGATED_ROWS): AGGREGATED_ROWS = {

    def aggregateRow(restOfRows: Tetris.Lines, rowNumber: Int = 0): AGGREGATED_ROWS = {
      def aggregateCell(restOfCells: Tetris.Row, colNumber: Int = 0): AGGREGATED_CELLS = restOfCells match {
        case head :: Seq() => cellAggregator(cellExtractor(colNumber, rowNumber, head), cellBase)
        case head :: tail => cellAggregator(cellExtractor(colNumber, rowNumber, head),
          aggregateCell(tail, colNumber + 1))
      }

      restOfRows match {
        case head :: Seq() => rowAggregator(aggregateCell(head), rowBase)
        case head :: tail => rowAggregator(aggregateCell(head), aggregateRow(tail, rowNumber + 1))
      }
    }

    aggregateRow(lines)
  }

  def foreachCell(action: (Int, Int, Option[Color]) => Unit): Unit = {
    aggregate(
      cellExtractor = action,
      cellAggregator = (_: Unit, _: Unit) => {},
      rowAggregator = (_: Unit, _: Unit) => {}
    )((), ())
  }

  /**
    * Check if the current figure may be placed at the current possition in the pod (no cells under the figure is taken)
    *
    * @return true - the figure may be placed
    */
  def testCommit: Boolean = {
    val pod2booleanAggregator = aggregate(
      cellExtractor = (colNumber: Int, rowNumber: Int, rowHead: Option[Color]) =>
        figure.brickAt(colNumber, rowNumber).isEmpty || rowHead.isEmpty,
      cellAggregator = (rowHead: Boolean, aggregatedRow: Boolean) => rowHead && aggregatedRow,
      rowAggregator = (podHead: Boolean, aggregatedPod: Boolean) => podHead && aggregatedPod
    )(_, _)

    figure.maxRow <= Tetris.ROWS && figure.maxCol <= Tetris.COLUMNS &&
      figure.minCol >= 0 && pod2booleanAggregator(true, true)

  }


  /**
    * Place the current figure into the pod, making corresponding cells being taken
    *
    * @return
    */
  def commit: Tetris = Tetris(aggregate(
    cellExtractor = (colNumber, rowNumber, rowHead) => rowHead.orElse(figure.brickAt(colNumber, rowNumber)),

    cellAggregator = (cell: Option[Color], aggregatedCells: Tetris.Row) =>
      cell +: aggregatedCells,

    rowAggregator = (row: Tetris.Row, aggregatedRows: Seq[Tetris.Row]) =>
      row +: aggregatedRows)(Seq(), Seq()), nextFigure)
  }

object Tetris {
  type Row = Seq[Option[Color]]
  type Lines = Seq[Row]

  val ROWS = 19
  val COLUMNS = 9

  /**
    * New empty pod
    * @return
    */
  def empty: Tetris = {
    def emptyRow: Row = Seq.fill(COLUMNS+1)(None)
    def addEmptyLines(rowNumber: Int = 0): Lines = rowNumber match {
        case ROWS => Seq(emptyRow)
        case _    => emptyRow +: addEmptyLines(rowNumber+1)
    }

    new Tetris(addEmptyLines())
  }

}