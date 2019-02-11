package gol.conway

sealed trait Cell {
  def alive: Int
}

object Cell {
  case object Alive extends Cell {
    def alive = 1
  }
  case object Dead extends Cell {
    def alive = 0
  }
  case object Born extends Cell {
    def alive = 1
  }
}