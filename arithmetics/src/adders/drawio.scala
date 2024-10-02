package adders

object drawio {

  import org.nasdanika.{drawio => nas}

  import java.nio.file.Files
  import scala.jdk.CollectionConverters._

  class Element[E <: nas.Element](val element: E) {

    def children = element.getChildren().asScala.map { case e: nas.Element =>
      new Element(e) {}
    }
  }

  class LinkTarget[E <: nas.LinkTarget](element: E) extends Element(element) {}

  class ModelElement[E <: nas.ModelElement](element: E) extends Element(element) {
    def setLabel(label: String): this.type = { element.setLabel(label); this }
    def label: String = element.getLabel()
    def getLink() = element.getLink()
    def setLink(link: String): this.type = { element.setLink(link); this }
    def getTooltip() = element.getTooltip()
    def setTooltip(tooltip: String): this.type = { element.setTooltip(tooltip); this }

    def getParent() = element.getParent()

    def getPropertyNames() = element.getPropertyNames().asScala
    def getProperty(key: String) = element.getProperty(key)
    def setProperty(key: String, value: String): this.type = { element.setProperty(key, value); this }

    def setStyle(key: String, value: String): this.type = { element.getStyle().asScala(key) = value; this }

    def setStyle(style: Map[String, Any]): this.type = {
      element.getStyle().asScala ++= style.map { case (k, v) => k -> v.toString }; this
    }
    def setStyle(style: (String, Any)*): this.type = {
      element.getStyle().asScala ++= style.map { case (k, v) => k -> v.toString }; this
    }

    def setStyle(otherEl: nas.ModelElement): this.type = {
      element.getStyle().asScala.addAll(otherEl.getStyle().asScala)
      this
    }

    def isVisible: Boolean = element.isVisible()
    def setVisible(visible: Boolean = true): this.type = { element.setVisible(visible); this }

    def addTag(tag: String): this.type = { element.getTags.add(tag); this }

  }

  abstract class LayerElement[E <: nas.LayerElement](element: E) extends ModelElement(element) {
    lazy val layer = new Layer(element.getLayer())
  }

  case class Connection(con: nas.Connection) extends LayerElement(con) {
    def source = Node(con.getSource())
    def target = Node(con.getTarget())
  }

  case class Page(page: nas.Page) extends LinkTarget(page) {

    def model = page.getModel()
    def name = page.getName()
    def setName(name: String): this.type = { page.setName(name); this }
    def id = page.getId()
    def document = page.getDocument()

    def root = model.getRoot()

    def createLayer(): Layer = new Layer(root.createLayer())

    def layers = root.getLayers().asScala.map(new Layer(_))

    private var _activeLayer: Option[Layer] = None

    def setActiveLayer(layer: Layer): this.type = { _activeLayer = Some(layer); this }

    def activeLayer = _activeLayer.getOrElse {
      val layer = layers.headOption.getOrElse(createLayer())
      _activeLayer = Some(layer)
      layer
    }

  }

  case class Node(node: nas.Node) extends LayerElement(node) {

    def setBounds(x: Double, y: Double, width: Double, height: Double): this.type = {
      geometry.setBounds(x, y, width, height); this
    }

    def x = geometry.getX()
    def y = geometry.getY()

    def setX(x: Double): this.type = { geometry.setX(x); this }
    def setY(y: Double): this.type = { geometry.setY(y); this }
    def setX(x: Int): this.type = setX(x.toDouble)
    def setY(y: Int): this.type = setY(y.toDouble)

    def width = geometry.getWidth()
    def height = geometry.getHeight()
    def setWidth(width: Int): this.type = { geometry.setWidth(width); this }
    def setHeight(height: Int): this.type = { geometry.setHeight(height); this }

    def geometry = node.getGeometry()

    def setGeometry(other: Node): this.type = {
      geometry.setBounds(other.x, other.y, other.width, other.height)
      this
    }

    def setGeometry(other: nas.Node): this.type = {
      val g = other.getGeometry()
      geometry.setBounds(g.getX(), g.getY(), g.getWidth(), g.getHeight())
      this
    }

    def setStyle(other: nas.Node): this.type = {
      setStyle(other.getStyle().asScala.toMap)
      this
    }

    def setGeometry(g: nas.Rectangle): this.type = {
      geometry.setBounds(g.getX(), g.getY(), g.getWidth(), g.getHeight())
      this
    }

    def createConnection(target: Node): Connection = new Connection(
      node.getLayer().createConnection(node, target.element)
    )

    def createConnection(target: Node, style: Map[String, Any]): Connection = {
      val connection = createConnection(target)
      connection.setStyle(style)
      connection
    }

    def connect(target: Node, style: Map[String, Any]): this.type = {
      createConnection(target).setStyle(style)
      this
    }
  }

  case class Layer(layer: nas.Layer) extends ModelElement(layer) {
    def createNode(): Node = new Node(layer.createNode())

    def createConnection(source: Node, target: Node): Connection =
      createConnection(source.element, target.element)

    def createConnection(source: nas.Node, target: nas.Node): Connection = new Connection(
      layer.createConnection(source, target)
    )

    def elements = layer.getElements().asScala.map {
      case n: nas.Node => Node(n)
      case c: nas.Connection => Connection(c)
      case _ => ???
    }
  }

  object Document {
    def apply(compressed: Boolean = false): Document = new Document(compressed)
  }

  case class Document(document: nas.Document) extends Element(document) {
    def this(compressed: Boolean = false) = this(nas.Document.create(compressed, null))

    def autoLayout(int: Int) = org.nasdanika.drawio.Util.layout(activePage.root, int)

    def pages = document.getPages().asScala.map(Page).toSeq

    def createPage(): Page = new Page(document.createPage())

    private var _activePage: Option[Page] = None

    def activePage: Page = _activePage.getOrElse {
      val page = pages.headOption.getOrElse(createPage())
      _activePage = Some(page)
      page
    }

    def setActivePage(page: Page): this.type = { _activePage = Some(page); this }

    // activePage.setName("Page 1")

    def model = activePage.model

    def createLayer(): Layer = activePage.createLayer()

    def setActiveLayer(layer: Layer): this.type = { activePage.setActiveLayer(layer); this }

    def createNode(): Node = activePage.activeLayer.createNode()

    def createNode(x: Double, y: Double, width: Double, height: Double, style: (String, Any)*): Node =
      activePage.activeLayer.createNode().setBounds(x, y, width, height).setStyle(style: _*)

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
  val connection = source.createConnection(target)
  // connection.setLabel("My connection")
  connection.setStyle("edgeStyle", "orthogonalEdgeStyle")
  connection.setStyle("rounded", "1")
  connection.setStyle("orthogonalLoop", "1")
  connection.setStyle("jettySize", "auto")
  connection.setStyle("html", "1")

  doc.save("new-uncompressed.drawio")
}
