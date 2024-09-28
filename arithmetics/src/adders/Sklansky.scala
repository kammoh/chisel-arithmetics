package adders

import chisel3._
import chisel3.util._

class SklanskyAdder(val width: Int, val withCin: Boolean) extends BitsPrefixAdderModule with Sklansky[Bool] {
  def this(width: Int) = this(width, false)

  import drawio._

  val graph = new Document()

  override def add(a: Seq[Bool], b: Seq[Bool], cin: Option[Bool]): Seq[Bool] = {
    val n = a.length
    require(n == b.length, "Inputs must have the same width")

    val startX = 10
    val startY = 10
    val boxWidth = 30
    val boxHeight = 30
    val horizontalSpacing = 20
    val verticalSpacing = boxHeight + 30

    val edgeStyle = Map(
      "edgeStyle" -> "orthogonalEdgeStyle",
      // "curved" -> "1",
      "arcSize" -> boxHeight * 2,
      "rounded" -> 1,
      "orthogonalLoop" -> 1,
      "html" -> 1,
      "endSize" -> 2,
    )

    val depth = log2Ceil(n)

    val nodes = collection.mutable.ArrayBuffer.tabulate(depth + 2, n) {
      case (0, j) =>
        val node = graph.createNode()
        node.setBounds(startX + (n - j) * (boxWidth + horizontalSpacing), startY, boxWidth, boxHeight)
        Some(node)
      case _ => None
    }

    def getNode(i: Int, j: Int) = {
      (i to 0 by -1) collectFirst {
        case i if nodes(i)(j).nonEmpty =>
          nodes(i)(j).get
      }
    }

    def addNode(i: Int, j: Int) = {
      val node = graph.createNode()
      node.setBounds(
        startX + (n - j) * (boxWidth + horizontalSpacing),
        startY + i * verticalSpacing,
        boxWidth,
        boxHeight
      )
      assert(nodes(i)(j).isEmpty, s"Node at $i, $j already exists")
      nodes(i)(j) = Some(node)
      node
    }

    def addBox(i: Int, j: Int, inRight: Int, fillColor: String) = {
      // println(s"Adding black box at ${i + 1}, $j")
      val node = addNode(i + 1, j)
      node.setStyle("fillColor", fillColor)
      node.setStyle("strokeColor", "#000000")
      getNode(i, j).foreach(
        _.connect(
          node,
          edgeStyle ++ Map(
            "exitX" -> 0.25,
            "exitY" -> 1,
            "exitDx" -> 0,
            "exitDy" -> 0,
            "entryX" -> 0.25,
            "entryY" -> 0,
            "entryDx" -> 0,
            "entryDy" -> 0
          )
        )
      )
      getNode(i, inRight).foreach(
        _.connect(
          node,
          edgeStyle ++ Map(
            "exitX" -> { if (inRight == j) 0.75 else 0.25 },
            "exitY" -> 1,
            "exitDx" -> 0,
            "exitDy" -> 0,
            "entryX" -> { if (inRight <= j) 0.75 else 0.25 },
            "entryY" -> 0,
            "entryDx" -> 0,
            "entryDy" -> 0
          )
        )
      )
      node
    }

    def mkCell(p: Bool, g: Bool, c: Bool, d: Option[Bool], i: Int, j: Int, jj: Int): (Bool, Bool) = {
      println(s"mkCell( $i, $j, $jj)")
      d match {
        case Some(d) =>
          addBox(i, j, jj, "#000000")
          blackCell(p, g, c, d)
        case None =>
          addBox(i, j, jj, "#cccccc")
          grayCell(p, g, c)
      }
    }

    val (p0, gk) = sklansky(a, b, cin, mkCell)

    graph.save(s"${desiredName}.drawio")
    p0.head +: xor(p0.tail, gk.init) :+ gk.last
  }

}

trait Sklansky[T <: Data] extends Adder[T] {

  /** @param a
    * @param b
    * @param c
    * @return
    *   (P, G)
    */
  def initCarry(a: Seq[T], b: Seq[T], c: Option[T]): (Seq[T], Seq[T]) = {
    (xor(a, c.map({ xor(b.head, _) +: b.tail }).getOrElse(b)), majority(a(0), b(0), c) +: and(a.tail, b.tail))
  }

  def sklansky(a: Seq[T], b: Seq[T], cin: Option[T], mkCell: Function7[T, T, T, Option[T], Int, Int, Int, (T, T)])
    : (Seq[T], Seq[T]) = {

    def nextLayer(p: Seq[T], g: Seq[T], i: Int): (Seq[T], Seq[T]) = {
      val l = 1 << i
      p.zip(g)
        .zipWithIndex
        .map { case ((pj, gj), j) =>
          if ((j / l) % 2 == 1) {
            val jj = j - (j % l) - 1
            val (pjj, gjj) = (p(jj), g(jj))
            mkCell(pj, gj, gjj, Option.when(j >= 2 * l)(pjj), i, j, jj)
          } else {
            (pj, gj)
          }
        }
        .unzip
    }

    val n = a.length
    require(n == b.length, "Inputs must have the same width")
    val (p0, g0) = initCarry(a, b, cin)
    val (_, gk) = (0 until log2Ceil(n))
      .foldLeft((p0, g0)) { case ((pi, gi), i) =>
        nextLayer(pi, gi, i)
      }
    (p0, gk)
  }

  def sum(p0: UInt, gk: UInt): UInt = (gk << 1) ^ p0

}
