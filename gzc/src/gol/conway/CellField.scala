package gol.conway

import scala.util.Random

object CellField {
  
  def computeNextGeneration(currGenField: CellField): CellField = {
    val nextGenContent = Array.ofDim[Cell](currGenField.rowCount, currGenField.columnCount)
    for (r <- 0 to (currGenField.rowCount - 1))
      for (c <- 0 to (currGenField.columnCount - 1))
        nextGenContent(r)(c) = currGenField.getNextGenCell(r, c)
    new CellField(nextGenContent, currGenField.rowCount, currGenField.columnCount)
  }
  
  def generateRandomField(rowCount: Int, columnCount: Int): CellField = {
    val content = Array.ofDim[Cell](rowCount, columnCount)
    for (r <- 0 to (rowCount - 1))
      for (c <- 0 to (columnCount - 1))
        Random.nextInt(2) match {
          case 0 => content(r)(c) = Cell.Dead
          case 1 => content(r)(c) = Cell.Alive
        }
    new CellField(content, rowCount, columnCount)
  }
}

class CellField(
    val content: Array[Array[Cell]],
    val rowCount: Int,
    val columnCount: Int) {

  /**
   * Conway's game of life:
   *   C   N                 -> new C
   *   1   0,1               -> 0 # Lonely
   *   1   4,5,6,7,8         -> 0 # Overcrowded
   *   1   2,3               -> 1 # Lives
   *   0   3                 -> 1 # Birth
   *   0   0,1,2,4,5,6,7,8   -> 0 # Barren
   */
  private def getNextGenCell(r: Int, c: Int): Cell = {
    val liveCells = neighbourCells(r, c).map(_.alive).sum
    content(r)(c) match {
      case Cell.Alive|Cell.Born => liveCells match {
        case 0|1 => Cell.Dead
        case 4|5|6|7|8 => Cell.Dead
        case 2|3 => Cell.Alive
      }
      case Cell.Dead => liveCells match {
        case 3 => Cell.Born
        case 0|1|2|4|5|6|7|8 => Cell.Dead
      }
    }
  }

  // Moore neighborhood
  private def neighbourCells(r: Int, c: Int): List[Cell] = {
    val cellNW = { if (r == 0) None else if (c == 0) None else Some(content(r-1)(c-1)) }
    val cellN = { if (c == 0) None else Some(content(r)(c-1)) }
    val cellNE = { if (r == rowCount-1) None else if (c == 0) None else Some(content(r+1)(c-1)) }
    val cellW = { if (r == 0) None else Some(content(r-1)(c)) }
    val cellE = { if (r == rowCount-1) None else Some(content(r+1)(c)) }
    val cellSW = { if (r == 0) None else if (c == columnCount-1) None else Some(content(r-1)(c+1)) }
    val cellS = { if (c == columnCount-1) None else Some(content(r)(c+1)) }
    val cellSE = { if (r == rowCount-1) None else if (c == columnCount-1) None else Some(content(r+1)(c+1)) }
    List(cellNW, cellN, cellNE, cellW, cellE, cellSW, cellS, cellSE).flatten
  }
  
  def aliveCellCount: Int = content.map(r => { r.map(_.alive).sum }).sum
  
  def print = {
    println(" # Alive Cells: " + aliveCellCount + "/" + (rowCount*columnCount))
    content
      .foreach { r =>
        println(" | " + r.toList.map(c => if (c.alive == 1) "X" else "-").mkString(" | ") + " | ")
    }
  }
}
