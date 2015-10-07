package clp

import org.junit.Assert._
import org.junit.Test
import clp.utils.LaunchManager

class LaunchManagerWrapperTest {
  private val configs = "dummy-0-0"   ::
                        "dummy-0-1"   ::
                        "dummy-1-0"   ::
                        "dummy-1-1"   ::
                        "incorrect-0" ::
                        "correct-0"   ::
                        Nil
  private val nonCompositeConfigs = configs.dropRight(2)
  private val notExistConfig = "not-exist"
  
  @Test
  def getConfigsTest = {
    val testConfigs = LaunchManager.getConfigs map { _.getName }
    if(configs.length != testConfigs.length) {
      fail("Wrong number of configs")
    }
    
    var rightConfigs = configs
    testConfigs foreach { x => 
      if(rightConfigs contains { x }) {
        rightConfigs = rightConfigs.filterNot { y =>
          x == y  
        }         
      } else {
        fail("Config \"" + x + "\" does not exist")
      }
    }
    if(rightConfigs.nonEmpty) {
      fail("Wrapper of LaunchManager has returned wrong config list")
    }
  }
  
  @Test
  def getNonCompositeConfigList = {
    val testConfigs = (LaunchManager.getNotCompositeConfigs map { x => x.getName }) .toList
     if(nonCompositeConfigs.length != testConfigs.length) {
      fail("Wrapper of LaunchManager has returned wrong number of configs")
    }
    
    var rightConfigs = nonCompositeConfigs
    testConfigs foreach { x => 
      if(rightConfigs contains { x }) {
        rightConfigs = rightConfigs.filterNot { y =>
          x == y  
        }        
      } else {
        fail("Config \"" + x + "\" does not exist")
      }
    }
    if(rightConfigs.nonEmpty) {
      fail("Wrapper of LaunchManager has returned wrong config list")
    }   
  }
  
  @Test
  def existConfigTest = configs foreach { x => 
    if(!LaunchManager.exists(x)) {
      fail("Config \"" + x + "\" does not exist")
    }
  }
  
  @Test
  def getConfigurationTest = configs foreach { x =>
    if(LaunchManager.getConfiguration(x) == None) {
      fail("Can't get configuration \"" + x + "\"")
    }
  }
  
  @Test
  def notExistConfigTest = if(LaunchManager.exists(notExistConfig)) {
    fail("Config \"" + notExistConfig + "\" exist, but it's not")
  }
  
  @Test
  def getNotExistConfigurationTest = if(LaunchManager.getConfiguration(notExistConfig) != None) {
    fail("Config \"" + notExistConfig + "\" was getted, but it's not exist")
  }
}