package clp

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.debug.core.ILaunchConfiguration
import org.junit.Test
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.testkit.TestKit
import akka.testkit.TestProbe
import clp.launch.composite.CompositeLaunchManager
import clp.utils.LaunchManager
import org.junit.After


class FakeCompositeLaunchManager(testActor: ActorRef, configs: List[ILaunchConfiguration], mode: String) extends Actor {
  val compositeLaunch = context.actorOf(clp.launch.composite.CompositeLaunch(configs, mode, SubMonitor.convert(new NullProgressMonitor)))
  
  override def postStop = {
    context.stop(compositeLaunch)
  }
  
  override def receive = {
    case x: Any => {
      val who = sender()
      if(who == testActor) {
        compositeLaunch ! x
      } else if(who == compositeLaunch) {
        testActor ! x
      }
    }
  }
}


class CompositeLaunchTest extends TestKit(ActorSystem("test")) {
  val allCorrectConfigs = ("dummy-0-0" :: "dummy-1-0" :: Nil) map { LaunchManager.getConfiguration(_).get }
  val mixedConfigs = ("dummy-0-0" :: "dummy-1-1" :: Nil) map { LaunchManager.getConfiguration(_).get }
  
  val testProbe = TestProbe()
  val probeRef = testProbe.ref
  
  @After
  def stopEverything = {
    TestKit.shutdownActorSystem(system)
  }
  
  @Test
  def correctConfigsTest: Unit = {
      val fakeManager = system.actorOf(Props(new FakeCompositeLaunchManager(probeRef, allCorrectConfigs, "run")))
      
      testProbe.expectMsgAllOf(CompositeLaunchManager.LaunchWasAdded(0, "dummy-0-0"),
                               CompositeLaunchManager.LaunchWasAdded(1, "dummy-1-0"))
  
                                                     
      testProbe.expectMsgAllOf(10 seconds, CompositeLaunchManager.LaunchWasStarted(0),
                               CompositeLaunchManager.LaunchWasStarted(1),
                               CompositeLaunchManager.Running,
                               CompositeLaunchManager.Running)
                               
      testProbe.expectMsgAllOf(20 seconds, CompositeLaunchManager.LaunchWasTerminated(0),
                               CompositeLaunchManager.LaunchWasTerminated(1),
                               CompositeLaunchManager.Terminated)
                               
      testProbe.send(fakeManager, clp.launch.composite.CompositeLaunch.StartAll)
      testProbe.expectMsgAllOf(10 seconds, CompositeLaunchManager.LaunchWasStarted(0),
                               CompositeLaunchManager.LaunchWasStarted(1),
                               CompositeLaunchManager.Running,
                               CompositeLaunchManager.Running)
                               
      testProbe.expectMsgAllOf(20 seconds, CompositeLaunchManager.LaunchWasTerminated(0),
                               CompositeLaunchManager.LaunchWasTerminated(1),
                               CompositeLaunchManager.Terminated)   
      testProbe.send(fakeManager, clp.launch.composite.CompositeLaunch.Start(0))
      testProbe.expectMsgAllOf(10 seconds, CompositeLaunchManager.LaunchWasStarted(0),
                               CompositeLaunchManager.Running)
                               
      testProbe.expectMsgAllOf(20 seconds, CompositeLaunchManager.LaunchWasTerminated(0),
                               CompositeLaunchManager.Terminated)   
  }
  
  @Test
  def mixedConfigsTest: Unit = {
      val fakeManager = system.actorOf(Props(new FakeCompositeLaunchManager(probeRef, mixedConfigs, "run")))
      
      testProbe.expectMsgAllOf(CompositeLaunchManager.LaunchWasAdded(0, "dummy-0-0"),
                               CompositeLaunchManager.LaunchWasAdded(1, "dummy-1-1"))
  
                                                     
      testProbe.expectMsgAllOf(10 seconds, CompositeLaunchManager.LaunchWasStarted(0),
                               CompositeLaunchManager.FailStarting(1),
                               CompositeLaunchManager.Running)
                               
      testProbe.expectMsgAllOf(20 seconds, CompositeLaunchManager.LaunchWasTerminated(0),
                               CompositeLaunchManager.Terminated)
                               
      testProbe.send(fakeManager, clp.launch.composite.CompositeLaunch.StartAll)
      testProbe.expectMsgAllOf(10 seconds, CompositeLaunchManager.LaunchWasStarted(0),
                               CompositeLaunchManager.FailStarting(1),
                               CompositeLaunchManager.Running)
                               
      testProbe.expectMsgAllOf(20 seconds, CompositeLaunchManager.LaunchWasTerminated(0),
                               CompositeLaunchManager.Terminated)                       
  }
}