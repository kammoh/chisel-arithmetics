package adders

object drawio {

  import org.nasdanika.{drawio => nas}

  import java.nio.file.Files
  import scala.jdk.CollectionConverters._

  abstract class Element[E <: nas.Element](protected val element: E) {

    def children = element.getChildren().asScala.map { case e: nas.Element =>
      new Element(e) {}
    }
  }

  abstract class ModelElement[E <: nas.ModelElement](element: E) extends Element(element) {
    def setLabel(label: String) = element.setLabel(label)
    def getLabel() = element.getLabel()
    def getLink() = element.getLink()
    def setLink(link: String) = element.setLink(link)
    def getTooltip() = element.getTooltip()
    def setTooltip(tooltip: String) = element.setTooltip(tooltip)

    def getParent() = element.getParent()

    def getPropertyNames() = element.getPropertyNames().asScala
    def getProperty(key: String) = element.getProperty(key)
    def setProperty(key: String, value: String) = element.setProperty(key, value)

    def setStyle(key: String, value: String) = element.getStyle().asScala(key) = value

    def setStyle(style: Map[String, Any]) = element.getStyle().asScala ++= style.map { case (k, v) => k -> v.toString }
    def setStyle(style: (String, Any)*) = element.getStyle().asScala ++= style.map { case (k, v) => k -> v.toString }

    def isVisible = element.isVisible()
    def setVisible(visible: Boolean) = element.setVisible(visible)

    def addTag(tag: String) = element.getTags.add(tag)

  }

  abstract class LayerElement[E <: nas.LayerElement](element: E) extends ModelElement(element) {
    def getLayer() = new Layer(element.getLayer())
  }

  case class Connection(con: nas.Connection) extends LayerElement(con) {
    def source = Node(con.getSource())
    def target = Node(con.getTarget())
  }

  case class Node(node: nas.Node) extends LayerElement(node) {

    def setBounds(x: Int, y: Int, width: Int, height: Int) = geometry.setBounds(x, y, width, height)

    def x = geometry.getX()
    def y = geometry.getY()
    def setX(x: Int) = geometry.setX(x)
    def setX(x: Double) = geometry.setX(x)
    def setY(y: Int) = geometry.setY(y)
    def setY(y: Double) = geometry.setY(y)

    def width = geometry.getWidth()
    def height = geometry.getHeight()
    def setWidth(width: Int) = geometry.setWidth(width)
    def setHeight(height: Int) = geometry.setHeight(height)

    def geometry = node.getGeometry()

    def connect(target: Node): Connection = new Connection(node.getLayer().createConnection(node, target.element))
    def connect(target: Node, style: Map[String, Any]): Connection = {
      val connection = connect(target)
      connection.setStyle(style)
      connection
    }
  }

  case class Layer(layer: nas.Layer) extends ModelElement(layer) {
    def createNode(): Node = new Node(layer.createNode())
  }

  class Document {

    def layout(int: Int) = org.nasdanika.drawio.Util.layout(root, int)

    val document = nas.Document.create(false, null)
    val page = document.createPage()
    page.setName("My first new page")

    val model = page.getModel()
    val root = model.getRoot()
    val layers = root.getLayers()

    def createLayer(): Layer = new Layer(root.createLayer())

    val defaultLayer = new Layer(layers.asScala.headOption.getOrElse(root.createLayer()))
    // defaultLayer.setLabel("My new layer")

    def createNode(): Node = defaultLayer.createNode()

    def save(path: String, compressed: Boolean = false) = {
      val p = new java.io.File(path).toPath()
      println(s"Writing to $p")
      Files.writeString(p, document.save(compressed))
    }
  }
}

object DrawTest extends App {

  import drawio._

  val doc = new Document()

  // Add nodes
  val source = doc.createNode()
  source.setLabel("src")
  source.setX(200)
  source.setX(100)
  source.setWidth(70)
  source.setHeight(30)

  source.setStyle("fillColor", "#f8cecc")
  source.setStyle("strokeColor", "#b85450")
  // source.getTags.add("aws")

  val target = doc.createNode()
  target.setLabel("My target node")
  target.setBounds(300, 150, 100, 30)
  // val targetTags = target.getTags
  // targetTags.add("aws")
  // targetTags.add("azure")

  // Add connection
  val connection = source.connect(target)
  // connection.setLabel("My connection")
  connection.setStyle("edgeStyle", "orthogonalEdgeStyle")
  connection.setStyle("rounded", "1")
  connection.setStyle("orthogonalLoop", "1")
  connection.setStyle("jettySize", "auto")
  connection.setStyle("html", "1")

  doc.save("new-uncompressed.drawio")
}
