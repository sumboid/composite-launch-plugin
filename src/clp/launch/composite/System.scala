package clp.launch.composite

import akka.actor.ActorSystem
import akka.actor.Props
import org.eclipse.debug.core.ILaunchConfiguration
import java.util.concurrent.atomic.AtomicReference
import clp.launch.views.CompositeManagerViewActor

/**
 * Global object contains composite launch manager and view manager singletons
 */
object System {
  val actorSystem = ActorSystem("clp")
  val launchManager = actorSystem.actorOf(Props[CompositeLaunchManager], "LaunchManager").path
  val viewManager = actorSystem.actorOf(Props[CompositeManagerViewActor], "ViewManager").path
  
  /**
   * Returns composite launch manager singleton
   */

  def getLaunchManager = this.synchronized(actorSystem.actorSelection(launchManager))
  
  /**
   * Returns view manager singleton
   */
  def getViewManager = this.synchronized(actorSystem.actorSelection(viewManager))
  
  /**
   * Stops launch manager
   */
  def stopLaunchManager = actorSystem.stop(actorSystem.actorOf(Props[CompositeLaunchManager], "LaunchManager"))
}