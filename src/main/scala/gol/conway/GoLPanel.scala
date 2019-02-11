package gol.conway

import scala.swing.Panel
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D

object GoLPanel {
  val CellSize = 10
}

class GoLPanel(var cellField: CellField) extends Panel {

  import GoLPanel._

  background = Color.darkGray
  preferredSize = new Dimension(cellField.rowCount * CellSize, cellField.columnCount * CellSize)

  override protected def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    for (r <- 0 until cellField.rowCount) {
      val row = cellField.content(r)
      for (c <- 0 until cellField.columnCount) {
        val x = r * CellSize
        val y = c * CellSize
        row(c) match {
          case Cell.Alive => {
            g setColor Color.orange
            g.fillRect(x, y, CellSize, CellSize)
          }
          case Cell.Born => {
            g setColor Color.yellow
            g.fillRect(x, y, CellSize, CellSize)
          }
          case Cell.Dead => // none
        }
      }
    }

    g setColor Color.gray
    for (x <- 0 until size.width by CellSize)
      g.drawLine(x, 0, x, size.height)
    for (y <- 0 until size.height by CellSize)
      g.drawLine(0, y, size.width, y)
  }
}
