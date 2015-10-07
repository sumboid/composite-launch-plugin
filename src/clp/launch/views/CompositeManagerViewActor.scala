package clp.launch.views

import akka.actor.Actor
import org.eclipse.ui.IPartListener
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.IPartService


object CompositeManagerViewActor {
  trait CurrentState
  case object Unknown extends CurrentState
  case object Running extends CurrentState
  case object Terminated extends CurrentState
  case object Failed extends CurrentState
  
  trait WithState {
    private var s: CurrentState = Unknown
    
    def state = s
    def running = { s = Running }
    def terminated = { s = Terminated }
    def failed = { s = Failed }
  }
  
  /**
   * Composite launch abstraction
   * 
   * @param id Unique identifier of composite launch
   * @param name Name of composite launch
   */
  case class CompositeLaunch(id: Int, name: String) extends WithState {
    var launches: List[Launch] = Nil
    
    /**
     * Adds launch to composite launch
     * 
     * @param launch Child launch
     */
    def add(launch: Launch) = { launches = launch :: launches }
  }
  
  /**
   * Launch abstraction
   * 
   * @param id Unique identifier of launch
   * @param name Name of launch
   */
  case class Launch(id: Int, name: String) extends WithState
  
  /**
   * Message that register view into [[CompositeManagerViewActor]]
   * 
   * @param view Target view
   */
  case class RegisterView(view: CMView)
  
  /**
   * Message that unregister view into [[CompositeManagerViewActor]]
   * 
   * @param view Target view
   */
  case class UnregisterView(view: CMView)
  
  /**
   * Message that indicates composite launch was added
   * 
   * @param id Internal unique identifier of composite launch
   * @param name Name of composite launch
   */
  case class CompositeLaunchWasAdded(id: Int, name: String)
  
  /**
   * Message that indicates composite launch was removed
   * 
   * @param id Internal unique identifier of composite launch
   */
  case class CompositeLaunchWasRemoved(id: Int)
  
  /**
   * Message that indicates composite launch was started
   * 
   * @param id Internal unique identifier of composite launch
   */
  case class CompositeLaunchWasStarted(id: Int)
  
  /**
   * Message that indicates composite launch was terminated
   * 
   * @param id Internal unique identifier of composite launch
   */
  case class CompositeLaunchWasTerminated(id: Int)
  
  /**
   * Message that indicates launch was added
   * 
   * @param pid Internal identifier of parent composite launch
   * @param id Internal unique identifier of launch
   * @param name Name of launch
   */
  case class LaunchWasAdded(pid: Int, id: Int, name: String)
  
  /**
   * Message that indicates launch was started
   * 
   * @param pid Internal identifier of parent composite launch
   * @param id Internal unique identifier of launch
   */
  case class LaunchWasStarted(pid: Int, id: Int)
  
  /**
   * Message that indicates launch was terminated
   * 
   * @param pid Internal identifier of parent composite launch
   * @param id Internal unique identifier of launch
   */
  case class LaunchWasTerminated(pid: Int, id: Int)
  
  /**
   * Message that indicates launch was failed to start
   * 
   * @param pid Internal identifier of parent composite launch
   * @param id Internal unique identifier of launch
   */
  case class LaunchWasFailed(pid: Int, id: Int)
}

/**
 * Composite manager view singleton actor used to control all opened [[CompositeManagerView]]
 */
class CompositeManagerViewActor extends Actor {
  import CompositeManagerViewActor._
  
  private var views: List[CMView] = Nil
  private var compositeLaunches: List[CompositeLaunch] = Nil
  
  /**
   * Updates state of composite manager view
   * 
   * @param f Function of updating view
   */
  def updateState(f: (CMView) => Unit) = {
    views foreach { view =>
      try {
        f(view)
      } catch {
        case _: Throwable => {}
      }
    }
  }
  
  /**
   * Initializes opened composite manager view
   * 
   * @param view Composite manager view target
   */
  def initView(view: CMView) = {
    compositeLaunches foreach { compositeLaunch =>
      val pid = compositeLaunch.id
      
      view.addCompositeLaunch(pid, compositeLaunch.name)
      compositeLaunch.state match {
        case Unknown => {}
        case Running => view.compositeLaunchRunning(pid)
        case Terminated => view.compositeLaunchStopped(pid)
        case _ => {}
      }
      
      compositeLaunch.launches foreach { launch =>
        val id = launch.id
        view.addLaunch(pid, id, launch.name)
        
        launch.state match {
          case Unknown => {}
          case Running => view.launchRunning(pid, id)
          case Terminated => view.launchStopped(pid, id)
          case _ => {}
        }
      }
    }
  }
  
  /**
   * Returns composite launch by id
   * 
   * @param id internal identifier of composite launch
   */
  def getCompositeLaunchById(id: Int) = compositeLaunches find { x => x.id == id }
  
  /**
   * Returns launch by id
   * 
   * @param id internal identifier of launch
   */
  def getLaunchById(pid: Int, id: Int) = getCompositeLaunchById(pid) match {
    case None => None
    case Some(compositeLaunch) => compositeLaunch.launches find { x => x.id == id }
  }
  
  def receive = {
    case RegisterView(view) => { 
      views = view :: views
      initView(view)
    }
    case UnregisterView(view) => { views = views filterNot { _ == view } }
    
    case CompositeLaunchWasAdded(id, name) => {
      compositeLaunches = CompositeLaunch(id, name) :: compositeLaunches
      updateState { x => x.addCompositeLaunch(id, name) }
    }
    case CompositeLaunchWasStarted(id) => getCompositeLaunchById(id) match {
      case Some(compositeLaunch) => {
        compositeLaunch.running
        updateState { x => x.compositeLaunchRunning(id) }
      }
      case None => {}
    }
     
    case CompositeLaunchWasTerminated(id) => getCompositeLaunchById(id) match {
      case Some(compositeLaunch) => {
        compositeLaunch.terminated
        updateState { x => x.compositeLaunchStopped(id) }
      }
      case None => {}
    }
    
    case CompositeLaunchWasRemoved(id) => getCompositeLaunchById(id) match {
      case Some(compositeLaunch) => {
        compositeLaunches = compositeLaunches filterNot { _ == compositeLaunch }
        updateState { x => x.removeCompositeLaunch(id) }
      } 
      case None => {}
    }
    
    case LaunchWasAdded(pid, id, name) => getCompositeLaunchById(pid) match {
      case Some(compositeLaunch) => {
        compositeLaunch.add(Launch(id, name))
        updateState { x => x.addLaunch(pid, id, name) }
      }
      case None => {}
    }
    
    case LaunchWasStarted(pid, id) => getLaunchById(pid, id) match {
      case Some(launch) => {
        launch.running
        updateState { x => x.launchRunning(pid, id) }
      }
      case None => {}
    }
    
    case LaunchWasTerminated(pid, id) => getLaunchById(pid, id) match {
      case Some(launch) => {
        launch.terminated
        updateState { x => x.launchStopped(pid, id) }
      }
      case None => {}
    }
    
    case LaunchWasFailed(pid, id) => getLaunchById(pid, id) match {
      case Some(launch) => {
        launch.failed
        updateState { x => x.launchFailed(pid, id) }
      }
      case None => {}
    }
    case _ => {}
  }
}