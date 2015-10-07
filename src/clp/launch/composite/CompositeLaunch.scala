package clp.launch.composite

import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.debug.core.DebugPlugin
import org.eclipse.debug.core.ILaunch
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.debug.core.ILaunchesListener2

import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala


/**
 * Launch object contains possible states of Launch
 */
object Launch {
  /** 
   * Launch status base interface
   */
  trait State
  
  /**
   * State indicates launch was started (added to Launch Manager)
   */
  case object Started    extends State
  
  /**
   * State indicates launch was removed from Launch Manager
   */
  case object Removed    extends State
  
  /**
   * State indicates launch was terminated
   */
  case object Terminated extends State
  
  /**
   * State indicates error while starting launch
   */
  case object Error      extends State
  
  /**
   * Initial state for every launch that was not tried to start
   */
  case object Unknown    extends State
}

/**
 * Launch wrapper for CompositeLaunch
 * 
 * @constructor Create new Launch
 * 
 * @param id External id
 * 
 * @param configuration Launch configuration needs to be interacted
 */
case class Launch(id: Int, configuration: ILaunchConfiguration) {
  import Launch._
  
  /**
   * State of launch
   */
  var state: State = Unknown
  
  /**
   * Actual launch linked with launch configuration
   */
  var launch: Option[ILaunch] = None
  
  def terminate = launch match {
    case None => {}
    case Some(x) => {
      val processes = x.getProcesses
      processes foreach { _.terminate }
      x.terminate
    }
  }
  
  def remove = launch match {
    case None => {}
    case Some(x) => {
      DebugPlugin.getDefault.getLaunchManager.removeLaunch(x)
    }
  }
  
  def start(mode: String): Boolean = try {
    launch = Some(configuration.launch(mode, new NullProgressMonitor, true, true))
    true
  } catch {
    case _: Throwable => {
      launch = None
      false 
    }
  }
  
  def restart(mode: String): Boolean = {
    state match {
      case Started => {
        terminate
        remove
      }
      case Terminated => {
        remove
      }
      case Error => {
        remove
      }
      case _ => {}
    }
    
    start(mode)
  }
}

object CompositeLaunch {
  /**
   * Creates props of instance of [[CompositeLaunch]]
   */
  def apply(confs: List[ILaunchConfiguration], mode: String, monitor: SubMonitor) = Props(new CompositeLaunch(confs, mode, monitor))

  /**
   * Message indicates launch was started
   */
  case class WasStarted(launch: ILaunch)
  /**
   * Message indicates launch was terminated
   */
  case class WasTerminated(launch: ILaunch)
  /**
   * Message indicates launch was removed
   */
  case class WasRemoved(launch: ILaunch)    
  
  /**
   * Message that requires start all launches
   */
  case object StartAll
  /**
   * Message that requires terminate all launches
   */
  case object TerminateAll
  
  /**
   * Message that requires start launch
   * 
   * @param id Identifier of launch
   */
  case class Start(id: Int)
  /**
   * Message that requires terminate launch
   * 
   * @param id Identifier of launch
   */
  case class Terminate(id: Int)
}

/**
 * Composite Launch Actor represented Composite Launch that can control multiple launch configurations
 * 
 * @param confs List of launch configurations that needed to be interact
 * @param mode Mode of launching
 */

class CompositeLaunch(confs: List[ILaunchConfiguration], mode: String, monitor: SubMonitor) extends Actor {
  import CompositeLaunch._
  
  /**
   * List of launches
   */
  val launches = ((0 until confs.length) zip confs) map { case (id, configuration) => Launch(id, configuration)}
  
  class LaunchesListener extends ILaunchesListener2 {
    override def launchesTerminated(ls: Array[ILaunch]) = ls foreach { l => self ! WasTerminated(l) }
    override def launchesAdded     (ls: Array[ILaunch]) = ls foreach { l => self ! WasStarted(l) }
    override def launchesChanged   (ls: Array[ILaunch]) = {}
    override def launchesRemoved   (ls: Array[ILaunch]) = {}
  }
  
  val listener = new LaunchesListener
  
  /**
   * Predicate that checks launch is child of composite launch
   * 
   * @param launch launch that need to be checked
   */
  def isChild(launch: ILaunch) = getLaunch(launch) match {
    case None => false
    case Some(x) => true
  }
  
  /**
   * Returns launch by identifier
   */
  def getLaunch(id: Int) = launches find { _.id == id }
  
  /**
   * Returns launch by low-level [[ILaunch]]
   */
  def getLaunch(l: ILaunch) = launches find { _.launch == Some(l) }

  override def preStart = {
    DebugPlugin.getDefault.getLaunchManager.addLaunchListener(listener)
    launches foreach { x => context.parent ! CompositeLaunchManager.LaunchWasAdded(x.id, x.configuration.getName) }
    startAll
  }
  
  override def postStop = {
    DebugPlugin.getDefault.getLaunchManager.removeLaunchListener(listener)
    launches foreach { x => x.state match {
      case Launch.Started => {
        x.terminate
        x.remove
      }
      case Launch.Terminated => x.remove
      case _ => {}
    }}
    monitor.done
  }
  
  /**
   * Starts launch
   * 
   * @param launch launch needs to be started
   */
  def launchConfig(launch: Launch): Unit = if(!launch.restart(mode)) {
    launch.state = Launch.Error
    context.parent ! CompositeLaunchManager.FailStarting(launch.id)
  }
  
  /**
   * Starts all child launches
   */
  def startAll = {
    launches foreach { launchConfig(_) }
  }
  
  def checkEnd = launches.forall { case x => x.state == Launch.Terminated ||
                                             x.state == Launch.Removed    ||
                                             x.state == Launch.Unknown    ||
                                             x.state == Launch.Error }

  def receive = {
    case WasStarted(x) => if(isChild(x)) {
      val l = getLaunch(x).get
      l.state = Launch.Started
      context.parent ! CompositeLaunchManager.Running
      context.parent ! CompositeLaunchManager.LaunchWasStarted(l.id)
    }
    case WasTerminated(x) => if(isChild(x)) {
      val l = getLaunch(x).get
      l.state = Launch.Terminated
      context.parent ! CompositeLaunchManager.LaunchWasTerminated(l.id)
      if(checkEnd) {
        context.parent ! CompositeLaunchManager.Terminated
      }
    }
    case WasRemoved(x) => if(isChild(x)) {
      val l = getLaunch(x).get
      l.state = Launch.Removed
      l.launch = None
      context.parent ! CompositeLaunchManager.LaunchWasRemoved(l.id)
    }
    
    case StartAll => startAll
    
    case TerminateAll => launches foreach { x => x.state match {
      case Launch.Started => x.terminate
      case _ => {}
    }}
    
    case Start(id) => getLaunch(id) match {
      case None => {}
      case Some(x) => launchConfig(x)
    }
    
    case Terminate(id) => getLaunch(id) match {
      case None => {}
      case Some(x) => x.terminate
    }
    
    case _ => {}
  }
}