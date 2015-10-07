package clp

import org.junit.Assert._
import org.junit.Test

import clp.utils.PreferencesWrapper
import org.eclipse.debug.ui.IDebugUIConstants

class PreferenceWrapperTest {
  @Test
  def disableAutoremoveTest = {
    val initialState = PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES)
    
    disableAutoremove(true)
    disableAutoremove(false)
    
    PreferencesWrapper.preferencesStore.setValue(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES, initialState)
  }
  
  def disableAutoremove(init: Boolean): Unit = {
    PreferencesWrapper.preferencesStore.setValue(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES, init)
    val restore = PreferencesWrapper.disableAutoremoveLaunches
    
    val newState = PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES)
    if(newState != false) {
      fail("State wasn't changed")
    }
    
    restore()
    
    val restoredState = PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES)
    if(restoredState != init) {
      fail("State wasn't restored")
    }
  }
}