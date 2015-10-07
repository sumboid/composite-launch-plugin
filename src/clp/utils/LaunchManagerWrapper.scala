package clp.utils

import org.eclipse.debug.core.DebugPlugin
import org.eclipse.core.runtime.CoreException 
import org.eclipse.debug.core.ILaunch

/**
 * Wrapper of [[ILaunchManager]] interface
 */
object LaunchManager {
  val manager = DebugPlugin.getDefault.getLaunchManager
  
  /**
   * Returns all not composite launch configurations from current workspace
   */
  def getNotCompositeConfigs = {
    manager.getLaunchConfigurations.filterNot { x => 
      try {
        val undef = "undefined"
        val conftype = x.getAttribute("config.type", undef)
        conftype match {
          case "undefined" => false
          case "cl" => true
          case _ => false
        }
      } catch {
        case _: Throwable => false
      }
    }
  }
  
  /**
   * Returns all launch configurations from current workspace
   */
  def getConfigs = manager.getLaunchConfigurations
  
  /**
   * Returns launch configuration by name
   * 
   * @param name Launch configuration name
   */
  def getConfiguration(name: String) = getConfigs.find { x => x.getName == name }
  
  /**
   * Checks launch configuration with specified name exists
   * 
   * @param name Name of launch configuration 
   */
  def exists(name: String) = getConfigs.exists { x => x.getName == name }
  
  /**
   * Removes launch from launch manager
   * 
   * @param launch Launch need to be removed
   */
  def remove(launch: ILaunch) = manager.removeLaunch(launch)
}