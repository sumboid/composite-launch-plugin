package clp.launch.composite

import scala.collection.mutable

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSelection.toScala
import akka.actor.Props
import akka.actor.actorRef2Scala

import clp.launch.views.CompositeManagerViewActor
import clp.utils.PreferencesWrapper
import clp.utils.UniqueIDGenerator

object CompositeLaunchManager {
  def props = Props[CompositeLaunchManager]
  
  /**
   * Message that register Composite Launch into Composite Launch Manager
   * 
   * @param compositeLaunchProps Props of Composite Launch Actor
   * @param name Name of Composite Launch
   */
  case class RegisterLaunch(compositeLaunchProps: Props, name: String)
  
  /**
   * Message that inform about Launch internal id and name
   * 
   * @param id Internal identifier of Launch
   * @param name Name of Launch
   */
  case class LaunchWasAdded(id: Int, name: String)
  
  /**
   * Message that inform about Launch was started
   * 
   * @param id Internal identifier of Launch
   */
  case class LaunchWasStarted(id: Int)
  
  /**
   * Message that inform about Launch was terminated
   * 
   * @param id Internal identifier of Launch
   */
  case class LaunchWasTerminated(id: Int)
  
  /**
   * Message that inform about Launch was removed
   * 
   * @param id Internal identifier of Launch
   */
  case class LaunchWasRemoved(id: Int)
  
  /**
   * Message that inform about Launch was failed to start
   * 
   * @param id Internal identifier of Launch
   */
  case class FailStarting(id: Int)
  
  /**
   * Message that inform about Composite Launch was started
   */
  case object Running
  
  /**
   * Message that inform about Composite Launch was terminated
   */
  case object Terminated
  
  /**
   * Message that requires start Launch
   * 
   * @param pid Composite Launch ID
   * @param id Launch ID
   */
  case class StartLaunch(pid: Int, id: Int)
  
  /**
   * Message that requires terminate Launch
   * 
   * @param pid Composite Launch ID
   * @param id Launch ID
   */
  case class TerminateLaunch(pid: Int, id: Int)
  
  /**
   * Message that requires start Composite Launch
   * 
   * @param id Composite Launch ID
   */
  case class StartCompositeLaunch(id: Int)
  
  /**
   * Message that requires terminate Composite Launch
   * 
   * @param id Composite Launch ID
   */
  case class TerminateCompositeLaunch(id: Int)
  
  /**
   * Message that requires remove Composite Launch
   * 
   * @param id Composite Launch ID
   */
  case class RemoveCompositeLaunch(id: Int)
}

/**
 * Global singleton actor that manages all Composite Launches
 */
class CompositeLaunchManager extends Actor {
  import CompositeLaunchManager._
  
  private val view = System.getViewManager
  private val generator: UniqueIDGenerator = new UniqueIDGenerator
  private val compositeLaunchID: mutable.Map[ActorRef, Int] = mutable.Map()
  private val compositeLaunchName: mutable.Map[ActorRef, String] = mutable.Map()
  private var returnPreferences: () => Unit = null
  
  override def postStop = {
    compositeLaunchID map { _._2 } foreach { removeCompositeLaunch(_) }
  }
  
  /**
   * Removes composite launch
   * 
   * @param id identifier of composite launch
   */
  def removeCompositeLaunch(id: Int) = {
    val compositeLaunch = getLaunchById(id)
    context.stop(compositeLaunch)
    
    compositeLaunchID -= compositeLaunch
    compositeLaunchName -= compositeLaunch
    
    generator.returnID(id)
      
    view ! CompositeManagerViewActor.CompositeLaunchWasRemoved(id)
      
    if(compositeLaunchID.isEmpty) {
      returnPreferences()
      returnPreferences = null
    }
  }
  
  /**
   * Returns launch by identifier
   * 
   * @param id identifier of composite launch
   */
  def getLaunchById(id: Int) = (compositeLaunchID find { case (x, i) => i == id }).get._1
  
  /**
   * Registers composite launch in Composite Launch Manager
   * 
   * @param compositeLaunch Composite launch actor
   * @param name Name of composite launch
   * @param id Identifier of composite launch
   */
  def registerLaunch(compositeLaunch: ActorRef, name: String, id: Int) = {
    if(returnPreferences == null) {
      returnPreferences = PreferencesWrapper.disableAutoremoveLaunches
    }
    compositeLaunchID += (compositeLaunch -> id)
    compositeLaunchName += (compositeLaunch -> name)
      
    view ! CompositeManagerViewActor.CompositeLaunchWasAdded(id, name)
    view ! CompositeManagerViewActor.CompositeLaunchWasTerminated(id)
  }
  
  def receive = {
    case RegisterLaunch(prop, name) => {
      val compositeLaunch = context.actorOf(prop)
      val id = generator.get
      registerLaunch(compositeLaunch, name, id)
    }
    
    case LaunchWasAdded(id, name) => {
      val pid = compositeLaunchID(sender())
      view ! CompositeManagerViewActor.LaunchWasAdded(pid, id, name)
    }
    
    case LaunchWasStarted(id) => {
      val pid = compositeLaunchID(sender())
      view ! CompositeManagerViewActor.LaunchWasStarted(pid, id)
    }
    
    case LaunchWasTerminated(id) => {
       val pid = compositeLaunchID(sender())
       view ! CompositeManagerViewActor.LaunchWasTerminated(pid, id)
    }
    
    case LaunchWasRemoved(x) => {}
    
    case FailStarting(id) => {
      val pid = compositeLaunchID(sender())
      view ! CompositeManagerViewActor.LaunchWasFailed(pid, id)
    }
    
    case Running => {
      val id = compositeLaunchID(sender())
      view ! CompositeManagerViewActor.CompositeLaunchWasStarted(id)
    }

    case Terminated => {
      val id = compositeLaunchID(sender())
      view ! CompositeManagerViewActor.CompositeLaunchWasTerminated(id)
    }
    
    case StartLaunch(pid, id) => {
      val compositeLaunch = getLaunchById(pid)
      compositeLaunch ! CompositeLaunch.Start(id)
    }
    
    case TerminateLaunch(pid, id) => {
      val compositeLaunch = getLaunchById(pid)
      compositeLaunch ! CompositeLaunch.Terminate(id)      
    }
    
    case StartCompositeLaunch(pid) => {
      val compositeLaunch = getLaunchById(pid)
      compositeLaunch ! CompositeLaunch.StartAll
    }
    
    case TerminateCompositeLaunch(pid) => {
      val compositeLaunch = getLaunchById(pid)
      compositeLaunch ! CompositeLaunch.TerminateAll
    }
    
    case RemoveCompositeLaunch(pid) => removeCompositeLaunch(pid)
    
    case x: Throwable => {}
    case _      => {}
  }
}