package adders

import drawio._

trait Edge

case class P[T](p: T) extends Edge
case class G[T](g: T) extends Edge // == carry (generate)
case class PG[T](p: T, g: T) extends Edge

case object TempNill extends Edge

object PG {
  def apply[T](pg: (Option[T], Option[T])): Edge = {
    pg match {
      case (Some(p), Some(g)) => PG(p, g)
      case (None, Some(g)) => G(g)
      case (Some(p), None) => P(p)
      case _ => TempNill
    }
  }
}
case class AB[T](a: T, b: T) extends Edge

trait Cell {
  def color: String
  def level: Int
  def offset: Int

  val incomings = Array.ofDim[(Cell, Edge)](2)
  val outgoings = collection.mutable.ArrayBuffer[Cell]()

  def connectFromRight(from: Cell, edge: Edge) = {
    incomings(0) = (from, edge)
    from.outgoings += this
  }
  def connectFromTop(that: Cell, edge: Edge) = {
    incomings(1) = (that, edge)
    that.outgoings += this
  }
  def name: String = s"${this.getClass.getSimpleName} [$level,$offset]"
}

case class BlackCell(level: Int, offset: Int) extends Cell {
  val color = "#000000"
}
case class GrayCell(level: Int, offset: Int) extends Cell {
  val color = "#cccccc"
}

case class FA(level: Int, offset: Int) extends Cell {
  val color = "#ffffff"
}
case class RegCell(level: Int, offset: Int) extends Cell {
  val color = "#FFF2CC" // yellow
}

case class InCell(level: Int, offset: Int) extends Cell {
  val color = "#ffffff"
}
case class OutCell(level: Int, offset: Int) extends Cell {
  val color = "#ffffff"
}

class Graph {
  val cells = collection.mutable.ArrayBuffer[Map[Int, Cell]]() // array of rows, each row is a map of offset -> cells

  def addCell(cell: Cell) = {
    while (cell.level >= cells.size) {
      cells += Map()
    }
    cells(cell.level) += cell.offset -> cell
    cell
  }

  def cellAt(level: Int, offset: Int): Option[Cell] = {
    if (level < 0 || level >= cells.size) return None
    cells(level).get(offset)
  }

  def cellAbove(level: Int, offset: Int): Option[Cell] = {
    if (level <= 0 || offset < 0 || level >= cells.size) return None
    (level - 1).to(0, -1).map(cells(_).get(offset)).collectFirst { case Some(value) =>
      value
    }
  }

}

case class Grapher(startX: Int, startY: Int, horizontalSpacing: Int, verticalSpacing: Int) {

// def n: Int
// def depth: Int

  def getNodeOption(i: Int, j: Int): Option[Node] = nodes.get((i, j))

  def addNode(
    i: Int,
    j: Int,
    width: Int,
    height: Int,
    horizontalSpacing: Int = horizontalSpacing,
    verticalSpacing: Int = verticalSpacing,
    n: Option[Int] = None): Node = {
    addNodeAt(
      i,
      j,
      startX + n.orElse(getN).map(_ - j).getOrElse(j) * (width + horizontalSpacing),
      startY + i * verticalSpacing,
      width,
      height
    )
  }

  def addNodeAt(
    i: Int,
    j: Int,
    x: Int,
    y: Int,
    width: Int,
    height: Int): Node = {
    assert(!nodes.contains((i, j)), s"Node at $i, $j already exists")
    val node = graph.createNode(
      x,
      y,
      width,
      height
    )
    nodes((i, j)) = node
    node
  }

  def strokeColor: String = "#000000"

  val graph = new Document()

  val nodes = collection.mutable.HashMap[(Int, Int), Node]()

  def numNodes: Int = nodes.size

  def maxJ: Int = nodes.keys.maxBy(_._2)._2

  private var _n: Option[Int] = None

  def getN = _n

  def nOrMaxJ = _n.getOrElse(maxJ)

  def setN(n: Int) = {
    _n = Some(n)
  }

  // def flipFinalGraph() = {
  //   val n = nOrMaxJ
  //   for (((_, j), node) <- nodes) {
  //     node.setX(startX + (n - j) * (boxWidth + horizontalSpacing))
  //   }
  // }

  def getNodeAbove(i: Int, j: Int): Option[Node] =
    (i to 0 by -1).map(getNodeOption(_, j)).collectFirst { case Some(node) =>
      node
    }

  def addBox(
    i: Int,
    j: Int,
    inRight: Int,
    fillColor: String,
    width: Int,
    height: Int,
    edgeStyle: Map[String, Any]) = {
    // println(s"Adding black box at ${i + 1}, $j")
    val node = addNode(i + 1, j, width = width, height = height).setStyle(
      "fillColor" -> fillColor,
      "strokeColor" -> strokeColor,
    )
    // node.setLabel(s"{$i:$j}")
    node.setTooltip(s"{$i:$j}")
    getNodeAbove(i, j).foreach(
      _.connect(
        node,
        edgeStyle ++ Map(
          "exitX" -> 0.25,
          "entryX" -> 0.25,
        )
      )
    )
    if (inRight >= 0) {
      getNodeAbove(i, inRight).foreach(
        _.connect(
          node,
          edgeStyle ++ Map(
            "exitX" -> { if (inRight == j) 0.75 else 0.25 },
            "entryX" -> { if (inRight <= j) 0.75 else 0.25 },
          )
        )
      )
    }
    node
  }

  // def addCell(cellType: CellType, x: Int, y: Int, parentsLoc: (Int, Int)*) = {
  //   val node = graph.createNode(x, y, 30, 30)
  //   node.setStyle(
  //     "fillColor" -> cellType.color,
  //     "strokeColor" -> "#000000",
  //   )
  //   nodes((x, y)) = node
  //   node
  // }
}

trait AdderGraph[T] extends Adder[T] {

  def startX = 10
  def startY = 10
  def boxWidth = 30
  def boxHeight = 30
  def horizontalSpacing = 20
  def verticalSpacing = boxHeight + 30

  lazy val graphh = new Graph()

  lazy val grapher = Grapher(startX, startY, horizontalSpacing, verticalSpacing)

  def edgeStyle = Map(
    "edgeStyle" -> "orthogonalEdgeStyle",
    // "curved" -> 0,
    "jumpStyle" -> "gap",
    "arcSize" -> boxHeight * 2,
    "jettySize" -> 5,
    "rounded" -> 1,
    "orthogonalLoop" -> 1,
    "html" -> 1,
    "endSize" -> 3,
    "exitX" -> 0.5,
    "exitY" -> 1,
    "exitDx" -> 0,
    "exitDy" -> 0,
    "entryX" -> 0.5,
    "entryY" -> 0,
    "entryDx" -> 0,
    "entryDy" -> 0
  )

  // def graph = grapher.graph
  // def nodes = grapher.nodes
  def save(filename: String) = grapher.graph.save(filename)
  def numBlackCells = graphh.cells
    .map(_.values.count { case x =>
      x match {
        case _: BlackCell => true
        case _ => false
      }
    })
    .sum

  def numGrayCells = graphh.cells
    .map(_.values.count { case x =>
      x match {
        case _: GrayCell => true
        case _ => false
      }
    })
    .sum

  def setN(n: Int) = grapher.setN(n)

  /// FIXME temporary
  def addBox(
    cell: Cell,
    right: Option[(Cell, Edge)] = None): Node = {
    addBox(cell, graphh.cellAbove(cell.level, cell.offset).map(_ -> PG((None, None))), right)
  }

  def addBox(
    cell: Cell,
    top: Option[(Cell, Edge)],
    right: Option[(Cell, Edge)]) = {
    val height = cell match {
      case _: InCell => boxHeight / 2
      case _: OutCell => boxHeight / 2
      case _: RegCell => boxHeight / 2
      case _ => boxHeight
    }
    top.foreach { case (topCell, edge) => cell.connectFromTop(topCell, edge) }
    right.foreach { case (rightCell, edge) => cell.connectFromRight(rightCell, edge) }
    graphh.addCell(cell)

    val node = grapher.addBox(
      cell.level,
      cell.offset,
      right.map(_._1.offset).getOrElse(-1),
      cell.color,
      boxWidth,
      height,
      edgeStyle
    )
    cell match {
      case _: InCell => node.setY(node.y + boxHeight / 2)
      case _ =>
    }
    node
  }

  def mkCell(p: T, g: T, c: T, d: Option[T], i: Int, j: Int, jr: Int): (T, T) = {
    val (op, og) = mkCell((Some(p), Some(g)), (d, Some(c)), i, j, jr)
    (op.getOrElse(zero), og.getOrElse(zero))
  }

  def mkCell(pg: (Option[T], Option[T]), pgr: (Option[T], Option[T]), i: Int, j: Int, jr: Int)
    : (Option[T], Option[T]) = {

    (pgr) match {
      case (None, pg._2) =>
        (None, None)
      case (None, _) =>
        addBox(GrayCell(i, j), graphh.cellAbove(i, j).map(_ -> PG(pg)), graphh.cellAt(i - 1, jr).map(_ -> PG(pgr)))
        grayCell(pg._1, pg._2, pgr._2)
      case _ =>
        addBox(BlackCell(i, j), graphh.cellAbove(i, j).map(_ -> PG(pg)), graphh.cellAt(i - 1, jr).map(_ -> PG(pgr)))
        blackCell(pg, pgr)
    }
  }

  def currentDepth: Int = graphh.cells.size
}
