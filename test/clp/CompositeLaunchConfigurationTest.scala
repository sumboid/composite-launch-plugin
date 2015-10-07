package clp

import org.junit.Assert._
import org.junit.Test
import clp.utils.LaunchManager
import clp.launch.manager.LaunchTab


class CompositeLaunchConfigurationTest {
  private val INCORRECT_CONFIG = "incorrect-0"
  private val CORRECT_CONFIG = "correct-0"
  private val launchTab = new LaunchTab 
  
  @Test
  def incorrectConfigTest = {
    LaunchManager.getConfiguration(INCORRECT_CONFIG) match {
      case None    => fail("Incorrect config does not exist")
      case Some(x) => {
        val subconfigs = launchTab.getSubConfigs(x)
        val invalidSubconfigs = launchTab.invalidConfigs(subconfigs)
        invalidSubconfigs match {
          case Nil => fail("There are no invalid subconfigs")
          case x: List[String] => {}
        }
      }
    }
  }
  
  @Test
  def correctConfigTest = {
    LaunchManager.getConfiguration(CORRECT_CONFIG) match {
      case None => fail("Correct config does not exist")
      case Some(x) => {
        val subconfigs = launchTab.getSubConfigs(x)
        val invalidSubconfigs = launchTab.invalidConfigs(subconfigs)
        invalidSubconfigs match {
          case Nil => {}
          case x: List[String] => fail("There are invalid subconfigs")
        }        
      }
    }
  }
}