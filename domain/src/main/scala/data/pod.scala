package  me.afox.tetriss.data

import Bricks._
import Figures._

import scala.language.postfixOps

case class Pod(lines: Pod.Lines = Pod.empty.lines, position: BlockPosition = BlockPosition(4,1), figure: Option[Figure] = None) {

  lazy val width = lines.head.size
  lazy val height = lines.size

  def inject(f: Figure) =
    new Pod(lines = lines, figure = Some(f.move(position)))

  def stepDown =
    new Pod(lines = lines, figure = Some(figure.get.move(down)))

  def stepRight =
    new Pod(lines = lines, figure = Some(figure.get.move(right)))

  def stepLeft =
    new Pod(lines = lines, figure = Some(figure.get.move(left)))


  def rotate =
    new Pod(lines = lines, figure = Some(figure.get.rotates.head)) // TODO replace rotates.head with first appropriate rotation

  /**
   * Recursively applies cellAggregator and rowAggregator functions on the current Pod state,
   * the current figure and the current position. Result is depended on the aggregator functions.
   *
   * 2 use cases of the generic function here:
   * 1. A Boolean - true if the current fugure position is free and the figure may be merged into the pod
   * 2. A new Pod state - makes a pod with the figure merged
   *
   * @param cellExtractor is used to calculate/map a cell value from the Pod cell state and the Figure block at the cell
   * position
   *
   * @param rowBase is a seed value to calculate aggregated pod value
   *
   * @param cellAggregator is a mapper providing a result from merging a cell/figure block into the current cell
   *
   * @param cellBase is a seed value to create an aggregated row value
   *
   * @param rowAggregator is a mapper providing a result from merging cellAggregator data into row sequence
   *
   *
   * @tparam AGGREGATED_ROWS
   * @tparam AGGREGATED_CELLS
   * @tparam MAPPED_CELL
   *
   * @return new value of applying the current figure in the current position on the current pod state
   */
   def aggregatePod[AGGREGATED_ROWS,AGGREGATED_CELLS,MAPPED_CELL](
              cellExtractor: (Int, Int, Option[Brick]) => MAPPED_CELL,
              cellAggregator: (MAPPED_CELL,AGGREGATED_CELLS) => AGGREGATED_CELLS,
              rowAggregator: (AGGREGATED_CELLS, AGGREGATED_ROWS) => AGGREGATED_ROWS
       )(cellBase: AGGREGATED_CELLS, rowBase: AGGREGATED_ROWS): AGGREGATED_ROWS = {

    def aggregateRow(restOfRows: Pod.Lines, rowNumber: Int = 0): AGGREGATED_ROWS = {
      def aggregateCell(restOfCells: Pod.Row, colNumber: Int = 0): AGGREGATED_CELLS =  restOfCells match {
        case (head :: Seq()) => cellAggregator(cellExtractor(colNumber, rowNumber, head), cellBase)
        case (head :: tail)  => cellAggregator(cellExtractor(colNumber, rowNumber, head),
          aggregateCell(tail, colNumber + 1))
      }

      restOfRows match {
        case (head :: Seq()) => rowAggregator(aggregateCell(head), rowBase)
        case (head :: tail)  => rowAggregator(aggregateCell(head), aggregateRow(tail, rowNumber + 1))
      }
    }

    aggregateRow(lines)
  }

  /**
   * Check if the current figure may be placed at the current possition in the pod (no cells under the figure is taken)
   * @return true - the figure may be placed
   */
  def testCommit: Boolean = {
    val pod2booleanAggregator = aggregatePod(
      cellExtractor = (colNumber: Int, rowNumber: Int, rowHead: Option[Brick]) =>
        (figure.get.brickAt(colNumber, rowNumber).isEmpty || rowHead.isEmpty),
      cellAggregator = (rowHead: Boolean, aggregatedRow: Boolean) => rowHead && aggregatedRow,
      rowAggregator  = (podHead: Boolean, aggregatedPod: Boolean) => podHead && aggregatedPod
    )(_,_)

    figure.forall(
      f => f.maxRow <= Pod.ROWS &&
        f.maxCol <= Pod.COLUMNS &&
        f.minCol >= 0 &&
        pod2booleanAggregator(true, true)
    )
  }


  /**
   * Place the current figure into the pod, making corresponding cells being taken
   * @return
   */
  def commit : Pod = {
    if(figure.isEmpty) return this

    new Pod(aggregatePod(
        cellExtractor = (colNumber, rowNumber, rowHead) =>
          figure.get
            .brickAt(colNumber, rowNumber)
            .orElse(rowHead),

        cellAggregator = (cell: Option[Brick], aggregatedCells: Pod.Row) =>
          cell +: aggregatedCells,

        rowAggregator = (row: Pod.Row, aggregatedRows: Seq[Pod.Row]) =>
          row +: aggregatedRows
      )(Seq(), Seq())
    )
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
    def emptyRow: Row = Seq.fill(COLUMNS+1)(None)
    def addEmptyLines(rowNumber: Int = 0): Lines = rowNumber match {
        case ROWS => Seq(emptyRow)
        case _    => emptyRow +: addEmptyLines(rowNumber+1)
    }

    new Pod(addEmptyLines())
  }

}