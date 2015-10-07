package clp

import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.ui.plugin.AbstractUIPlugin
import org.osgi.framework.BundleContext
import clp.launch.composite.System

object Activator {
  val PLUGIN_ID = "clp"
  var plugin: Option[Activator] = None
  
  def getDefault = plugin.getOrElse(() => null)
  def getImageDescriptor(path: String) = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN_ID, path)
}

class Activator extends AbstractUIPlugin {
  override def start(context: BundleContext): Unit = {
    super.start(context)
    Activator.plugin = Some(this)
  }

  override def stop(context: BundleContext): Unit = {
    Activator.plugin = None
    super.stop(context)
  }
}