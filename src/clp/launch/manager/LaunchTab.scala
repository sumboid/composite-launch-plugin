package clp.launch.manager

import scala.collection.JavaConverters.asScalaBufferConverter

import org.eclipse.debug.core.DebugPlugin
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Event

import clp.utils.LaunchManager
import clp.utils.PushButton
import clp.utils.Table

/**
 * Launch Tab that used to configure composite launch configuration
 */
class LaunchTab extends AbstractLaunchConfigurationTab {
  var upBtn: PushButton = null
  var downBtn: PushButton = null
  var addBtn: PushButton = null
  var removeBtn: PushButton = null
  var launchConfTbl: Table = null
  var combo: Combo = null
  
  def invalidConfigs(configNames: List[String]) = configNames.filterNot{ LaunchManager.exists(_) }
  
  def getSubConfigs(config: ILaunchConfiguration) = {
    val jlist: java.util.List[String] = new java.util.ArrayList
    config.getAttribute("configs", jlist).asScala.toList
  }
  
  override def isValid(config: ILaunchConfiguration) = {
    setMessage(null)
    setErrorMessage(null)
    
    val invalid = invalidConfigs(getSubConfigs(config))
    invalid match {
      case Nil => true
      case xs => {
        setErrorMessage(xs.distinct.mkString(", ") + " not exist")
        false
      }
    }
  }
  
  override def createControl(parent: Composite): Unit = {
    val comp = new Composite(parent, SWT.NONE)
    
    comp.setLayout(new GridLayout(2, false))
    comp.setFont(parent.getFont)
    
    combo = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY)
    combo.setItems(LaunchManager.getNotCompositeConfigs.map{ _.getName })
    combo.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false))
    combo.setText("Select launch configuration")
    
    val woo = DebugPlugin.getDefault.getLaunchManager.getLaunchConfigurations
    
    val addBtnComp = new Composite(comp, SWT.NONE)
    addBtnComp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, false, false))
    addBtnComp.setLayout(new FillLayout(SWT.FILL))
    
    addBtn = PushButton(addBtnComp, "Add", true, () => {
      val lc = combo.getText
      if(lc != "") {
        launchConfTbl.add(combo.getText :: Nil)
        setDirty(true)
        updateLaunchConfigurationDialog
      }
    })
        
    launchConfTbl = Table(comp)
    launchConfTbl.header("Configuration name" :: Nil)
    val handler = { e: Event => {
      val selection = launchConfTbl.selection
      if(!selection.isEmpty) {
        val end = launchConfTbl.length - 1
        val index = launchConfTbl.selection.head
        
        upBtn.enable
        downBtn.enable
        removeBtn.enable
          
        if(index == 0) {
          upBtn.disable
        }
        if(index == end) {
          downBtn.disable
        }     
      } else {
        upBtn.disable
        downBtn.disable
        removeBtn.disable
      }
    }}
    
    launchConfTbl.selectListener(handler)
    launchConfTbl.modifyListener(handler)
    
    val btnComp = new Composite(comp, SWT.NONE)
    btnComp.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, false, false))
    
    val flayout = new FillLayout(SWT.VERTICAL)
    flayout.spacing = 3
    btnComp.setLayout(flayout)

    upBtn = PushButton(btnComp, "Up", false, () => {
      val selection = launchConfTbl.selection
      selection.isEmpty match {
        case true => {}
        case false => {
          val index = selection.head
          if(index != 0) {
            launchConfTbl.move(index, index - 1)
            launchConfTbl.select(index - 1)
            setDirty(true)
            updateLaunchConfigurationDialog
          }
        }
      }
    })
    downBtn = PushButton(btnComp, "Down", false, () => {
      val selection = launchConfTbl.selection
      selection.isEmpty match {
        case true => {}
        case false => {
          val index = selection.head
          if(index != launchConfTbl.length - 1) {
            launchConfTbl.move(index, index + 1)
            launchConfTbl.select(index + 1)
            setDirty(true)
            updateLaunchConfigurationDialog
          }
        }
      }
    })
    removeBtn = PushButton(btnComp, "Remove", false, () => {
      val selection = launchConfTbl.selection
      selection.isEmpty match {
        case true => {}
        case false => {
          val index = selection.head
          val end = launchConfTbl.length - 1
          launchConfTbl.remove(index)
          if (end != 0) {
            launchConfTbl.select(if(end == index) { index - 1 } else { index })
          }
          setDirty(true)
          updateLaunchConfigurationDialog
        }
      }
    })

    setControl(comp)
  }

  override def getName() = "Main"

  override def initializeFrom(config: ILaunchConfiguration): Unit = {
    val configs = getSubConfigs(config)
    
    launchConfTbl.clear
    configs.foreach(x => launchConfTbl.add(x :: Nil))
  }

  override def performApply(config: ILaunchConfigurationWorkingCopy): Unit = {
    val range = 0 until launchConfTbl.length
    val jlist: java.util.List[String] = new java.util.ArrayList
    range foreach { x => jlist.add(launchConfTbl.get(x).head) }
 
    config.setAttribute("configs", jlist)
  }

  override def setDefaults(config: ILaunchConfigurationWorkingCopy): Unit = {
    config.setAttribute("config.type", "cl")
  }
}