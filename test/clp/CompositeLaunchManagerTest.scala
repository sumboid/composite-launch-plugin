package clp

import org.junit.Assert._
import org.junit._

import akka.testkit.TestActorRef
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.testkit.TestKit
import akka.testkit.TestProbe
import clp.launch.composite.CompositeLaunchManager
import clp.launch.composite.CompositeLaunchManager._
import clp.launch.composite.CompositeLaunch.{StartAll, TerminateAll, Terminate, Start}
import clp.launch.views.CMView
import org.eclipse.ui.PlatformUI
import clp.utils.PreferencesWrapper
import org.eclipse.debug.ui.IDebugUIConstants

class CompositeLaunchManagerTest extends TestKit(ActorSystem("test")) {
	val managerActor = TestActorRef[CompositeLaunchManager]
  val manager = managerActor.underlyingActor
  val testProbe = 0 -> TestProbe() :: 
                  1 -> TestProbe() :: Nil
  
  @Test
	def behaviourTest: Unit = {
    testProbe foreach { case (id, probe) =>
      registerTestProbe(id, probe)
      addFakeLaunches(probe)
      terminateLaunches(id, probe)
      startLaunches(id, probe)
    }
    
    testProbe foreach { case (id, probe) =>
      terminateCompositeLaunch(id, probe)
      startCompositeLaunch(id, probe)
      removeCompositeLaunch(id)
    }
	}
                  
  @Test
  def preferencesChangesTest: Unit = {
    val probe1 = testProbe(0)
    val probe2 = testProbe(1)
    
    
    
    val initialState = PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES)
    registerTestProbe(probe1._1, probe1._2)
    if(PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES) != false) {
      fail("PREF_AUTO_REMOVE_OLD_LAUNCHES must be false")
    }
    
    registerTestProbe(probe2._1, probe2._2)
    if(PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES) != false) {
      fail("PREF_AUTO_REMOVE_OLD_LAUNCHES must be false")
    }
    
    removeCompositeLaunch(probe1._1)
    if(PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES) != false) {
      fail("PREF_AUTO_REMOVE_OLD_LAUNCHES must be false")
    }
    
    removeCompositeLaunch(probe2._1)
    if(PreferencesWrapper.preferencesStore.getBoolean(IDebugUIConstants.PREF_AUTO_REMOVE_OLD_LAUNCHES) != initialState) {
      fail("PREF_AUTO_REMOVE_OLD_LAUNCHES must be " + initialState)
    }
    
  }
  
  def registerTestProbe(id: Int, probe: TestProbe): Unit = {
    manager.registerLaunch(probe.ref, "test" + id, id)
  }
  
  def addFakeLaunches(probe: TestProbe): Unit = {
    probe.send(managerActor, LaunchWasAdded(0, "test-0"))
    probe.send(managerActor, LaunchWasAdded(1, "test-1"))
  }
  
  def terminateLaunches(id: Int, probe: TestProbe): Unit = {
    0 to 1 foreach { lid =>
      managerActor ! TerminateLaunch(id, lid)
      probe.expectMsg(Terminate(lid))
    }
  }
  
  def startLaunches(id: Int, probe: TestProbe): Unit = {
    0 to 1 foreach { lid =>
      managerActor ! StartLaunch(id, lid)
      probe.expectMsg(Start(lid))
    }
  }
  
  def terminateCompositeLaunch(id: Int, probe: TestProbe): Unit = {
    managerActor ! TerminateCompositeLaunch(id)
    probe.expectMsg(TerminateAll)
  }
  
  def startCompositeLaunch(id: Int, probe: TestProbe): Unit = {
    managerActor ! StartCompositeLaunch(id)
    probe.expectMsg(StartAll)
  }
  
  def removeCompositeLaunch(id: Int): Unit = {
    managerActor ! RemoveCompositeLaunch(id)
  }

  
}