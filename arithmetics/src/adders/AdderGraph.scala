package adders

import drawio._

trait Edge

case class P[T](p: Option[T]) extends Edge
case class G[T](g: Option[T]) extends Edge // == carry (generate)
case class PG[T](p: T, g: T) extends Edge

case object TempNill extends Edge

object PG {
  def apply[T](pg: (Option[T], Option[T])): Edge = {
    pg match {
      case (Some(p), Some(g)) => PG(p, g)
      case (None, g) => G(g)
      case (p, None) => P(p)
      case _ => TempNill
    }
  }
}

case class AB[T](ab: (Option[T], Option[T])) extends Edge

trait CellType

case object HalfAdder extends CellType
case object FullAdder extends CellType
case object PGSum extends CellType
case object GrayCellT extends CellType
case object BlackCellT extends CellType
case object NullIn extends CellType
case object NullOut extends CellType

trait CellNode {
  def level: Int
  def offset: Int

  def style: Map[String, String] = Map(
    "fillColor" -> "#ffffff",
    "strokeColor" -> "#000000",
    "rounded" -> "1"
  )

  val label: Option[String] = None

  val incomings = Array.ofDim[(CellNode, Edge)](2)
  val outgoings = collection.mutable.ArrayBuffer[CellNode]()

  def connectFromRight(from: CellNode, edge: Edge) = {
    incomings(0) = (from, edge)
    from.outgoings += this
  }

  def connectFromTop(that: CellNode, edge: Edge) = {
    incomings(1) = (that, edge)
    that.outgoings += this
  }

  def name: String = s"${this.getClass.getSimpleName} [$level,$offset]"
}

case class BlackCell(level: Int, offset: Int) extends CellNode {
  override val style = super.style.updated("fillColor", "#000000")
}
case class GrayCell(level: Int, offset: Int) extends CellNode {
  override val style = super.style.updated("fillColor", "#cccccc")
}

case class HA(level: Int, offset: Int) extends CellNode {
  override val style = super.style.updated("fillColor", "#f4f4f4")
  override val label: Option[String] = Some("HA")
}
case class FA(level: Int, offset: Int) extends CellNode {

  override val style = super.style.updated("fillColor", "#dce9f1")
  override val label: Option[String] = Some("FA")
}
case class RegCell(level: Int, offset: Int) extends CellNode {
  override val style = super.style.updated("fillColor", "#FFF2CC") // yellow
}

case class InCell(level: Int, offset: Int) extends CellNode {
  override val style = super.style.updated("rounded", "0")
}
case class OutCell(level: Int, offset: Int) extends CellNode {
  override val style = super.style.updated("rounded", "0")
}

class Graph {
  val cells =
    collection.mutable.ArrayBuffer[Map[Int, CellNode]]() // array of rows, each row is a map of offset -> cells

  def addCell(cell: CellNode) = {
    while (cell.level >= cells.size) {
      cells += Map()
    }
    cells(cell.level) += cell.offset -> cell
    cell
  }

  def cellAt(level: Int, offset: Int): Option[CellNode] = {
    if (level < 0 || level >= cells.size) return None
    cells(level).get(offset)
  }

  def cellAbove(level: Int, offset: Int): Option[CellNode] = {
    // println(s"   cellAbove $level $offset cells.size=${cells.size}")
    if (level < 0 || offset < 0) {
      println("  --- none")
      return None
    }
    (level - 1).to(0, -1).map(cells(_).get(offset)).collectFirst { case Some(value) =>
      value
    }
  }

  def currentDepth: Int = cells.zipWithIndex.reverse.collectFirst {
    case (c, i) if c.nonEmpty =>
      if (
        c.values.exists(p =>
          p match {
            case _: OutCell => false
            case _ => true
          }
        )
      ) i + 1
      else i
  }.getOrElse(0)

}

trait AdderGraph[T] extends Adder[T] {

  def startX = 10
  def startY = 10
  def boxWidth = 30
  def boxHeight = 20
  def horizontalSpacing = 20
  def verticalSpacing = boxHeight + 25

  class Grapher(startX: Int, startY: Int, horizontalSpacing: Int, verticalSpacing: Int) {

    def getNodeOption(i: Int, j: Int): Option[Node] = nodes.get((i, j))

    def addNodeAt(
      i: Int,
      j: Int,
      x: Int,
      y: Int,
      width: Int,
      height: Int): Node = {
      assert(!nodes.contains((i, j)), s"Node at $i, $j already exists")
      val node = doc.createNode(
        x,
        y,
        width,
        height
      )
      nodes((i, j)) = node
      node
    }

    private val doc = new Document()

    def saveDoc(filename: String) = doc.save(filename)

    private val nodes = collection.mutable.HashMap[(Int, Int), Node]()

    def numNodes: Int = nodes.size

    def maxJ: Int = nodes.keys.maxBy(_._2)._2

    private var _n: Option[Int] = None

    def getN = _n

    def nOrMaxJ = _n.getOrElse(maxJ)

    def setN(n: Int) = {
      _n = Some(n)
    }

    def getNodeAbove(i: Int, j: Int): Option[Node] =
      (i to 0 by -1).map(getNodeOption(_, j)).collectFirst { case Some(node) =>
        node
      }

    def addBox(
      i: Int,
      j: Int,
      inRight: Int,
      width: Int,
      height: Int,
      verticalSpacing: Int,
      nodeStyle: Map[String, Any],
      label: Option[String],
      edgeStyle: Map[String, Any]) = {

      val n = None

      val node = addNodeAt(
        i + 1,
        j,
        startX + n.orElse(getN).map(_ - j).getOrElse(j) * (width + horizontalSpacing),
        startY + i * verticalSpacing,
        width,
        height
      ).setStyle(nodeStyle)

      label.foreach(node.setLabel)

      node.setTooltip(s"$i:$j")
      getNodeAbove(i, j).foreach { x =>
        x.createConnection(
          node,
          edgeStyle ++ Map(
            "exitX" -> 0.25,
            "entryX" -> 0.25,
          )
        )
      }

      if (inRight >= 0) {
        getNodeAbove(i, inRight).foreach { rightNode =>
          val conn = rightNode.createConnection(
            node,
            edgeStyle ++ Map(
              "exitX" -> { if (inRight == j) 0.75 else 0.25 },
              "entryX" -> { if (inRight <= j) 0.75 else 0.25 },
            )
          )

          if (inRight < j) {
            // val outs =
            //   rightNode.outgoingConnections.filter(_.target.y == node.y)
            conn.addPoint(
              rightNode.x + 0.25 * rightNode.width,
              node.y - (verticalSpacing - height) / 2.0
            )
          }

        }
      }
      node
    }
  }

  private lazy val graph = new Graph()

  private lazy val grapher = new Grapher(startX, startY, horizontalSpacing, verticalSpacing)

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

  def save(filename: String) = grapher.saveDoc(filename)

  def numBlackCells = graph.cells
    .map(_.values.count { case x =>
      x match {
        case _: BlackCell => true
        case _ => false
      }
    })
    .sum

  def numGrayCells = graph.cells
    .map(_.values.count { case x =>
      x match {
        case _: GrayCell => true
        case _ => false
      }
    })
    .sum

  def setN(n: Int) = grapher.setN(n)

  /// FIXME temporary
  private def addBox(
    cell: CellNode,
    right: Option[(CellNode, Edge)] = None): Node = {
    addBox(cell, None, right)
  }

  private def addBox(
    cell: CellNode,
    top: Option[(CellNode, Edge)],
    right: Option[(CellNode, Edge)]) = {
    val height = cell match {
      case _: InCell => boxHeight / 2
      // case _ if cell.level == 0 => boxHeight / 2
      case _: OutCell => boxHeight / 2
      case _: RegCell => boxHeight / 2
      case _ => boxHeight
    }
    graph.addCell(cell)
    top.orElse {
      graph.cellAbove(cell.level, cell.offset).map(_ -> PG((None, None)))
    }.foreach { case (topCell, edge) => cell.connectFromTop(topCell, edge) }
    right.foreach { case (rightCell, edge) => cell.connectFromRight(rightCell, edge) }

    val node = grapher.addBox(
      cell.level,
      cell.offset,
      right.map(_._1.offset).getOrElse(-1),
      boxWidth,
      height,
      verticalSpacing = verticalSpacing,
      nodeStyle = cell.style,
      label = cell.label,
      edgeStyle = edgeStyle,
    )
    cell match {
      case _: InCell => node.setY(node.y + boxHeight / 2)
      // case _ if cell.level == 0 => node.setY(node.y + boxHeight / 2)
      case _: OutCell => node.setY(node.y - boxHeight / 2)
      case _ =>
    }
    node
  }

  def mkBlackCell(pg: (Option[T], Option[T]), pgr: (Option[T], Option[T]), i: Int, j: Int, jr: Int)
    : (Option[T], Option[T]) = {
    (pg, pgr) match {
      case ((_, g), (None, None)) =>
        (None, g)
      case ((None, g), (_, _)) =>
        (None, g)
      case (_, (None, gr)) =>
        addBox(GrayCell(i, j), graph.cellAbove(i, j).map(_ -> PG(pg)), graph.cellAbove(i, jr).map(_ -> G(gr)))
        grayCell(pg._1, pg._2, gr)
      case _ =>
        addBox(BlackCell(i, j), graph.cellAbove(i, j).map(_ -> PG(pg)), graph.cellAbove(i, jr).map(_ -> PG(pgr)))
        blackCell(pg, pgr)
    }
  }

  def mkCell[C <: CellType](ct: C, pg: (Option[T], Option[T]), gr: Option[T], i: Int, j: Int, jr: Int = -1)
    : (Option[T], Option[T]) =
    mkCell(ct, pg, (None, gr), i, j, jr)

  def mkCell[C <: CellType](ct: C, pg: (Option[T], Option[T]), pgr: (Option[T], Option[T]), i: Int, j: Int, jr: Int)
    : (Option[T], Option[T]) = {
    ct match {
      case NullIn =>
        addBox(InCell(i, j))
        pg

      case NullOut =>
        addBox(OutCell(i, j))
        pg

      case PGSum =>
        addBox(OutCell(i, j))
        pgSum(pg._1, pgr._2)

      case HalfAdder =>
        addBox(HA(i, j))
        halfAdder(pg._1, pg._2)

      case FullAdder if pgr._2.isEmpty =>
        mkCell(HalfAdder, pg, None, i, j, jr)

      case FullAdder =>
        addBox(FA(i, j), graph.cellAbove(i, j).map(_ -> AB(pg)), graph.cellAt(i - 1, jr).map(_ -> G(pg._2)))
        fullAdder(pg._1, pg._2, pgr._2)

      case GrayCellT =>
        mkBlackCell(pg, (None, pgr._2), i, j, jr)

      case BlackCellT =>
        mkBlackCell(pg, pgr, i, j, jr)

      case _ =>
        throw new IllegalArgumentException(s"Unsupported cell type: $ct")
    }
  }

  def mkCell(pg: (Option[T], Option[T]), pgr: (Option[T], Option[T]), i: Int, j: Int, jr: Int)
    : (Option[T], Option[T]) = {

    mkBlackCell(pg, pgr, i, j, jr)
  }

  def currentDepth: Int = graph.currentDepth
}
