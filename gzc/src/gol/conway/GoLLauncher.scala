package gol.conway

import scala.swing._

object GoLLauncher extends SimpleSwingApplication {

  val GenerationCount = 500
  
  case class AllDead(val genNum: Int) extends Exception { }
  
  def top = new MainFrame {
    title = "GoL App"
    resizable = false
    contents = ui
  }
  
  lazy val ui = new GoLPanel(CellField.generateRandomField(100, 80))
  private def regen(nextGenField: CellField) = {
    ui.cellField = nextGenField
    ui.repaint
  }
  
  override def main(args: Array[String]) {
    startup(args)
    try {
      var lastGenField = runGenerations(ui.cellField, GenerationCount, true)
      println("Cells alive after " + GenerationCount + " generations -> " + lastGenField.aliveCellCount)
      regen(lastGenField)
    }
    catch {
      case e: AllDead => println("All Dead after " + e.genNum + " generations")
    }
  }
  
  private def runGenerations(cellField: CellField, maxGenCount: Int, printDebug: Boolean): CellField = {
    var currGenField = cellField
    var run = true
    for (i <- 0 until maxGenCount) {
      val nextGenField = CellField.computeNextGeneration(currGenField)
      if (printDebug) {
        regen(nextGenField)
      }
      if (nextGenField.aliveCellCount == 0)
        throw AllDead(i+1)
      currGenField = nextGenField
      Thread.sleep(100)
    }
    currGenField
  }
}
