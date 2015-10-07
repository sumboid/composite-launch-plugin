package clp.utils

import org.eclipse.debug.internal.ui.DebugUIPlugin
import org.eclipse.debug.ui.IDebugUIConstants

/**
 * Preference store wrapper
 */
object PreferencesWrapper {
  val preferencesStore = DebugUIPlugin.getDefault.getPreferenceStore
  
  /**
   * Disable preference [[IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES]]
   * 
   * @return Returns function that restore this preference back
   */
  def disableAutoremoveLaunches: () => Unit = {
    val stored = preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES)
    preferencesStore.setValue(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES, false)
    
    () => { preferencesStore.setValue(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES, stored) }
  }
}