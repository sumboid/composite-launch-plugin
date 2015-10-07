package clp.launch.manager

import org.eclipse.debug.ui._
import org.eclipse.debug.ui.sourcelookup._
import org.eclipse.jdt.debug.ui.launchConfigurations._
import org.eclipse.jdt.ui._

class LaunchTabGroup extends AbstractLaunchConfigurationTabGroup {
  override def createTabs(dialog : ILaunchConfigurationDialog, mode : String) = {
    setTabs(Array[ILaunchConfigurationTab](
      new LaunchTab,
      new CommonTab
    ))
  }
}