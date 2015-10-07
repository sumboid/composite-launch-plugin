package clp.launch.views

import scala.language.postfixOps

import org.eclipse.swt.SWT
import org.eclipse.swt.events.MenuAdapter
import org.eclipse.swt.events.MenuEvent
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Event
import org.eclipse.swt.widgets.Listener
import org.eclipse.swt.widgets.Menu
import org.eclipse.swt.widgets.MenuItem
import org.eclipse.ui.part.ViewPart

import akka.actor.ActorSelection.toScala
import clp.launch.composite.CompositeLaunchManager.RemoveCompositeLaunch
import clp.launch.composite.CompositeLaunchManager.StartCompositeLaunch
import clp.launch.composite.CompositeLaunchManager.StartLaunch
import clp.launch.composite.CompositeLaunchManager.TerminateCompositeLaunch
import clp.launch.composite.CompositeLaunchManager.TerminateLaunch
import clp.launch.composite.System
import clp.utils.Node
import clp.utils.NonRoot
import clp.utils.Root
import clp.utils.SubTree

/**
 * State trait used to be mixed in nodes of launch tree
 */

trait State {
  class CurrentState(val value: String)
  case object Running extends CurrentState("Running")
  case object Terminated extends CurrentState("Terminated")
  case object Failed extends CurrentState("Failed")
  case object Unknown extends CurrentState("")
  
  var state: CurrentState = Unknown
  
  def running = { state = Running }
  def stopped = { state = Terminated }
  def failed =  { state = Failed}
  
  def getStatusString = state.value
  def getState = state
}

/**
 * Menu items trait used to be mixed in nodes of launch tree
 */

trait MenuItems {
  /**
   * Array of item names, listeners and predicates that represent state of item (enabled or not)
   */
  val itemstr: Array[(String, Listener, () => Boolean)]
  
  /**
   * Creates items for menu
   * 
   * @param parent Menu target
   */
  def createMenuItems(parent: Menu) = itemstr foreach { case (str, listener, active) => 
    val item = new MenuItem(parent, SWT.NONE)
    item.setText(str)
    item.addListener(SWT.Selection, listener)
    item.setEnabled(active()) 
  }  
}

trait LaunchMenuItems extends MenuItems { self: Node with NonRoot with State =>
  val launchListener = new Listener() {
    override def handleEvent(e: Event) = {
      System.getLaunchManager ! StartLaunch(parent.id, id)
    }
  }
  
  val terminateListener = new Listener() {
    override def handleEvent(e: Event) = {
      System.getLaunchManager ! TerminateLaunch(parent.id, id)
    }
  }
  
  val itemstr = Array(("Launch",    launchListener,    () => { state == Terminated || state == Failed }),
                      ("Terminate", terminateListener, () => { state == Running }))
}

trait CompositeLaunchMenuItems extends MenuItems { self: Node with NonRoot with State =>
  val launchListener = new Listener() {
    override def handleEvent(e: Event) = {
      System.getLaunchManager ! StartCompositeLaunch(id)
    }
  }
  
  val terminateListener = new Listener() {
    override def handleEvent(e: Event) = {
      System.getLaunchManager ! TerminateCompositeLaunch(id)
    }
  }

  val removeListener = new Listener() {
    override def handleEvent(e: Event) = {
      System.getLaunchManager ! RemoveCompositeLaunch(id)
    }
  }  
  
  val itemstr = Array(("Launch",               launchListener,    () => { state == Terminated }),
                      ("Terminate",            terminateListener, () => { state == Running }),
                      ("Terminate and remove", removeListener,    () => { state == Running || state == Terminated }))
}

/**
 * View part that visualizes execution of composite launches and takes control of execution to user 
 */
class CMView extends ViewPart {
  private var root: Root = null
  private var menu: Menu = null
  
  override def createPartControl(parent: Composite) = {
    root = Root(parent)
    root.header("Launch" :: "Status" :: Nil)
    
    menu = root.addMenu
    menu.addMenuListener(new MenuAdapter() {
      override def menuShown(e: MenuEvent) = {
        root.getSelection match {
          case None => {}
          case Some(x) => {
            menu.getItems foreach { _.dispose }
            val node = x.asInstanceOf[Node with NonRoot with State with MenuItems]
            node.createMenuItems(menu)
          }
        } 
      }
    })
    
    System.getViewManager ! CompositeManagerViewActor.RegisterView(this)
  }
  
  override def dispose = {
    super.dispose
    System.getViewManager! CompositeManagerViewActor.UnregisterView(this)
  }
  
  override def setFocus = root.focus
  
  /**
   * Synchronized call of function
   * 
   * @param f Function
   */
  private def sync(f: => Unit) = this.getSite.getShell.getDisplay.syncExec(new Runnable() {
    override def run = try {
      f
    } catch {
      case x: Throwable => { System.getLaunchManager ! x }
    }
  })
  
  /**
   * Adds composite launch to launch tree
   * 
   * @param id Composite launch unique identifier
   * @param name Name of composite launch
   */
  
  def addCompositeLaunch(id: Int, name: String) = sync {
    val node = new SubTree(id) with State with CompositeLaunchMenuItems
    root.add(node)
    node.set(0, name)
  }
  
  /**
   * Adds launch to launch tree
   * 
   * @param pid Composite launch identifier
   * @param id Launch unique identifier
   * @param name Name of composite launch
   */
  def addLaunch(pid: Int, id: Int, name: String) = sync {
    val node = new SubTree(id) with State with LaunchMenuItems
    root \ pid add node
    node.set(0, name)
  }
  
  /**
   * Set launch state to running
   * 
   * @param pid Composite launch identifier
   * @param id Launch identifier
   */
  def launchRunning(pid: Int, id: Int) = sync {
    val node = (root \ pid \ id).asInstanceOf[SubTree with State]
    node.running
    node.set(1, node.getStatusString)
  }
  
  /**
   * Set launch state to stopped
   * 
   * @param pid Composite launch identifier
   * @param id Launch identifier
   */
  def launchStopped(pid: Int, id: Int) = sync {
    val node = (root \ pid \ id).asInstanceOf[SubTree with State]
    node.stopped
    node.set(1, node.getStatusString)
  }
  
  /**
   * Set launch state to failed
   * 
   * @param pid Composite launch identifier
   * @param id Launch identifier
   */
  def launchFailed(pid: Int, id: Int) = sync {
    val node = (root \ pid \ id).asInstanceOf[SubTree with State]
    node.failed
    node.set(1, node.getStatusString)  
  }
  
  /**
   * Set composite launch state to running
   * 
   * @param id Composite launch identifier
   */
  def compositeLaunchRunning(id: Int) = sync {
    val node = (root \ id).asInstanceOf[SubTree with State]
    node.running
    node.set(1, node.getStatusString)
  }
  
  /**
   * Set composite launch state to stopped
   * 
   * @param id Composite launch identifier
   */
  def compositeLaunchStopped(id: Int) = sync {
    val node = (root \ id).asInstanceOf[SubTree with State]
    node.stopped
    node.set(1, node.getStatusString)
  }
  
  /**
   * Removes launch from launch tree
   * 
   * @param pid Composite launch identifier
   * @param id Launch identifier
   */
  def removeLaunch(pid: Int, id: Int) = sync { (root \ pid \ id).remove }
  
  /**
   * Removes composite launch from launch tree
   * 
   * @param id Composite launch identifier
   */
  def removeCompositeLaunch(pid: Int) = sync { (root \ pid).remove }
}