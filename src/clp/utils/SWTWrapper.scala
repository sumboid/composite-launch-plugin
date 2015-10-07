package clp.utils

import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Button => SWTButton}
import org.eclipse.swt.widgets.{Composite => SWTComposite}
import org.eclipse.swt.widgets.{Table => SWTTable}
import org.eclipse.swt.widgets.{TableColumn => SWTTableColumn}
import org.eclipse.swt.widgets.{TableItem => SWTTableItem}
import org.eclipse.swt.widgets.{Tree => SWTTree}
import org.eclipse.swt.widgets.{TreeColumn => SWTTreeColumn}
import org.eclipse.swt.widgets.{TreeItem => SWTTreeItem}
import org.eclipse.swt.custom.{TreeEditor => SWTTreeEditor}
import org.eclipse.swt.widgets.Listener
import org.eclipse.swt.widgets.Event
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{Button => SWTButton}
import org.eclipse.swt.widgets.{Composite => SWTComposite}
import org.eclipse.swt.widgets.{Table => SWTTable}
import org.eclipse.swt.widgets.{TableColumn => SWTTableColumn}
import org.eclipse.swt.widgets.{TableItem => SWTTableItem}
import org.eclipse.swt.widgets.{Menu => SWTMenu}

/**
 * Wrapper of [[swt.widgets.Button]]
 */
case class PushButton(parent: SWTComposite, text: String, enabled: Boolean, handler: () => Unit) {
  val btn = new SWTButton(parent, SWT.PUSH)
  btn.setText(text)
  btn.setEnabled(enabled)
  btn.addListener(SWT.Selection, new Listener () {
    override def handleEvent(e: Event) = {
      handler()
    }
  })
  
  def enable() = btn.setEnabled(true)
  def disable() = btn.setEnabled(false)
}

/**
 * Wrapper of [[swt.widgets.Table]]
 */
case class Table(parent: SWTComposite, style: Int = SWT.VIRTUAL | SWT.SINGLE | SWT.BORDER, layoutData: Object = new GridData(SWT.FILL, SWT.FILL, true, true)) {
  val table = new SWTTable(parent, style)
  table.setHeaderVisible(true)
  table.setLinesVisible(true)
  table.setLayoutData(layoutData)
  
  /**
   * Sets header of table
   * 
   * @param xs List of header titles
   */
  def header(xs: List[String]) = { 
    xs foreach {x => new SWTTableColumn(table, SWT.NONE).setText(x)}
    dirtyHack(xs)
  }
  
  protected def dirtyHack(xs: List[String]): Unit = {
    add(xs, 0)
    table.getColumns foreach (_.pack)
    remove(0)
  } 
  
  /**
   * Returns table item as list of string
   * 
   * @param index Index of item 
   */
  def get(index: Int): List[String] = {
    val item = table.getItem(index) 
    val range = 0 until table.getColumnCount
    range.map{i => item.getText(i)}.toList
  }
 
  /**
   * Returns all items
   */
  def getAll = (0 until length) map { x => get(x) }
  
  /**
   * Low-level function that adds list of string as row of table
   * 
   * @param xs Row item as list of string
   * @param item Low-level container [[swt.widgets.TableItem]]
   */
  def add(xs: List[String], item: SWTTableItem): Unit = {
    xs.zipWithIndex foreach { case (x, i) => item.setText(i, x) }
    table.notifyListeners(SWT.Modify, null)    
  }
  
  /**
   * Adds list of string as row of table
   * 
   * @param xs Row item
   */
  def add(xs: List[String]): Unit = add(xs, new SWTTableItem(table, SWT.NONE))
  
  /**
   * Adds list of string as row of table
   * 
   * @param xs Row item
   * @param index Index of row needs to be placed 
   */
  def add(xs: List[String], index: Int): Unit = add(xs, new SWTTableItem(table, SWT.NONE, index))
  
  /**
   * Removes row with definite index
   * 
   * @param index Index of row
   */
  def remove(index: Int) {
    table.getItem(index).dispose
    table.notifyListeners(SWT.Modify, null)
  }
  
  /**
   * Moves row in table
   * 
   * @param from Initial index of row
   * @param to New index of row
   */
  def move(from: Int, to: Int) = {
    if(from != to) {
      val item = get(from)
      remove(from)
      add(item, to)
    }
  }

  /**
   * Select raw with definite index
   * 
   * @param index Index of row
   */
  def select(index: Int) = {
    table.select(index)
    table.notifyListeners(SWT.Selection, null)
  }
  
  /**
   * Sets selection listener of table
   * 
   * @param f Handler of events
   */
  def selectListener(f: (Event) => Unit) = {
    table.addListener(SWT.Selection, new Listener() {
      def handleEvent(e: Event) = f(e)
    })
  }
  
  /**
   * Sets modify listener of table
   * 
   * @param f Handler of events
   */
  def modifyListener(f: (Event) => Unit) = {
    table.addListener(SWT.Modify, new Listener() {
      def handleEvent(e: Event) = f(e)
    })    
  }
  
  /**
   * Returns selection of table
   */
  def selection: List[Int] = {
    table.getSelection.map{ table.indexOf(_) }.toList
  }
  
  def length: Int = table.getItemCount
  def clear = (0 until length) foreach { x => remove(0) }
  
  def focus = table.setFocus()
}

/**
 * Node of tree abstraction 
 */
trait Node {
  /**
   * Returns all child nodes
   */
  def childs: List[Node with NonRoot]
  
  /**
   * Adds child to node
   * 
   * @param node Child node
   */
  def add(node: Node with NonRoot): Unit
  
  /**
   * Removes child node 
   */
  def remove(node: Node): Unit
  
  /**
   * Removes node with all child nodes
   */
  def remove: Unit
  
  /**
   * Returns child node with id
   * 
   * @param cid Child node id
   */
  def get(cid: Int): Node
  
  /**
   * Find node that contains definite [[swt.widget.TreeItem]] down of tree 
   */
  def find(item: SWTTreeItem): Option[Node] = {
    if(childs.isEmpty) { None }
    childs find { x => x.contains(item) } match {
      case None => childs find { x => x.find(item) != None } match {
        case None => None
        case Some(x) => x.find(item)
      }
      case Some(x) => Some(x)
    }
  }
  
  /**
   * Returns child node with id
   * 
   * @param cid Child node id
   */
  def \ (cid: Int): Node = get(cid)
  
  /**
   * Returns id of node
   */
  def id: Int
}

/**
 * Non root tree node abstraction
 */
trait NonRoot { self: Node =>
  /**
   * Initializes node with low-level parent root-node
   */
  def init(root: SWTTree): Unit
  
  /**
   * Initializes node with low-level parent node
   */
  def init(node: SWTTreeItem): Unit
  
  /**
   * Sets parent node
   */
  def setParent(parent: Node): Unit
  
  /**
   * Set string of field position
   */
  def set(field: Int, s: String)

  def contains(item: SWTTreeItem): Boolean
  
  /**
   * Returns parent node
   */
  def parent: Node
}

/**
 * Realization of root node of tree
 */
case class Root(parent: SWTComposite, style: Int = SWT.BORDER, layoutData: Object = new GridData(SWT.FILL, SWT.FILL, true, true))  extends Node {
  val tree = new SWTTree(parent, style)
  var childList: List[Node with NonRoot] = Nil
  tree.setLayoutData(layoutData)
  tree.setHeaderVisible(true)
  
  def header(xs: List[String]) = { 
    xs foreach { x =>
      val column = new SWTTreeColumn(tree, SWT.LEFT)
      column.setText(x)
      column.setWidth(200)
    }
    dirtyHack(xs)
  }
  
  protected def dirtyHack(xs: List[String]): Unit = {
    val fakeitem = new SWTTreeItem(tree, SWT.NONE)
    xs.zipWithIndex.foreach{ case (x, i) => fakeitem.setText(i, x) }
    tree.getColumns foreach (_.pack)
    fakeitem.dispose
  } 
  
  def add(node: Node with NonRoot) = {
    node.init(tree)
    node.setParent(this)
    childList = node :: childList
  }
  
  def remove(node: Node) = {
    childList.find { x => x == node } match {
      case None => {}
      case Some(x) => {
        x.remove
        childList = childList.filterNot(_ == node)
      }
    }
  }
  
  def childs = childList
  
  def remove = tree.dispose
  def get(cid: Int) = childList.find(x => x.id == cid).get
  
  def id = 0
  def focus = tree.setFocus
  def addMenu = {
    val menu = new SWTMenu(tree)
    tree.setMenu(menu)
    menu
  }
  
  def getSelection = {
    if(tree.getSelectionCount == 0) {
      None
    } else {
      find(tree.getSelection()(0))
    }
  }
  
  def addListener(eventType: Int)(f: (Event) => Unit) = tree.addListener(eventType, new Listener() {
    override def handleEvent(e: Event) = f(e)
  })
}

/**
 * Realization of non-root node of tree
 */
class SubTree(nodeID: Int) extends Node with NonRoot {
  private var item: SWTTreeItem = null
  private var childList: List[Node with NonRoot] = Nil
  private var p: Node = null
  
  def add(node: Node with NonRoot) = {
    node.init(item)
    node.setParent(this)
    childList = node :: childList
  }
  
  def remove(node: Node) = {
    childList.find { x => x == node } match {
      case None => {}
      case Some(x) => {
        x.remove
        childList = childList.filterNot(_ == node)
      }
    }
  }
  
  def contains(i: SWTTreeItem) = { item.equals(i) }
  def remove = item.dispose
  def get(cid: Int) = childList.find(x => x.id == cid).get //TODO: Drop some exception
  def parent = p
  def setParent(node: Node) = { p = node }
  
  
  def id = nodeID
  
  def init(node: SWTTree) = {
    item = new SWTTreeItem(node, SWT.NONE)
  }
  
  def init(node: SWTTreeItem) = {
    item = new SWTTreeItem(node, SWT.NONE)
  }
  
  def set(field: Int, s: String) = item.setText(field, s)
  def childs = childList
}