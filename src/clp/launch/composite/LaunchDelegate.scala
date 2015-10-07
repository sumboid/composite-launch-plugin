package clp.launch.composite

import scala.collection.JavaConverters.asScalaBufferConverter

import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.debug.core.ILaunch
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate

import CompositeLaunchManager.RegisterLaunch
import akka.actor.ActorSelection.toScala
import clp.utils.LaunchManager

class LaunchDelegate extends AbstractJavaLaunchConfigurationDelegate {
  override def launch(config: ILaunchConfiguration, mode: String, launch: ILaunch, monitor: IProgressMonitor): Unit = { 
    val jlist: java.util.List[String] = new java.util.ArrayList
    val configNames = config.getAttribute("configs", jlist).asScala 
    val configs = configNames.map { LaunchManager.getConfiguration(_).get}.toList
        
    val currentMonitor = SubMonitor.convert(if(monitor == null) new NullProgressMonitor else monitor, configs.length) 
    System.getLaunchManager ! RegisterLaunch(CompositeLaunch(configs, mode, currentMonitor), config.getName)
    LaunchManager.remove(launch)
  }
}