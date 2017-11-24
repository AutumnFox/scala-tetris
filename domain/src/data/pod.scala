package  me.afox.tetriss.data

import Bricks._
import Figures._


case class Pod(lines: Pod.Lines = Pod.empty.lines, position: Block = Block(5,0), currentFigure: Option[Figure] = None) {

}

object Pod {
  type Lines = Seq[Seq[Option[Brick]]]

  def empty: Pod = {
    def addEmptyLines(lineNumber: Int): Seq[Seq[Option[Brick]]] = {
      lineNumber match {
        case 1 => Seq[Seq[Option[Brick]]](Seq[Option[Brick]](None,None,None,None,None,None,None,None,None,None))
        case n => Seq[Option[Brick]](None,None,None,None,None,None,None,None,None,None) +: addEmptyLines(n-1)
      }
    }
    new Pod(addEmptyLines(20))
  }


  def main(args: Array[String]): Unit = {
  }

}