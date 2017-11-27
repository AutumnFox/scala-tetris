package  me.afox.tetriss.data

import Bricks._
import Figures._

import scala.collection.immutable.Stream.Empty


case class Pod(lines: Pod.Lines = Pod.empty.lines, position: Block = Block(4,1), figure: Option[Figure] = None) {

  def inject(f: Figure) = new Pod(lines = lines, figure = Some(f + position))
  def stepDown = new Pod(lines = lines, figure = Some(figure.get.move(down)))
  def rotate = new Pod(lines = lines, figure = Some(figure.get.rotates.head))

  /**
    * Place the current figure in the pod, current figure became None
    * @return
    */
  def commit = {
    def addCommittedRow(rowNumber: Int = 0, restOfRows: Pod.Lines): Pod.Lines = {
      def addCommittedCell(colNumber: Int = 0, rowNumber: Int, restOdCells: Pod.Row): Pod.Row = {
        restOdCells match {
          case (head::tail) => {
            colNumber match {
              case Pod.COLUMNS => Seq(figure.get.brickAt(colNumber, rowNumber))
              case col =>   (figure.get.brickAt(col, rowNumber) match {
                  case Some(brick) => Some(brick)
                  case None => head
              })  +: addCommittedCell(colNumber + 1, rowNumber, tail)
            }
          }
          case _ => Seq()
        }
      }

      restOfRows match {
        case (head::tail) => {
          rowNumber match {
            case Pod.ROWS => Seq(addCommittedCell(0, rowNumber, head))
            case n => addCommittedCell(0, rowNumber, head) +: addCommittedRow(n + 1, tail)
          }
        }
        case _ => Seq(addCommittedCell(0, rowNumber, Seq()))
      }
    }

    new Pod(addCommittedRow(0, lines))
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