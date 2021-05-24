package models

import main.scala.Control
import main.scala.Ant2D

/**
 * This model extends the Ant2D model. Convolution is added. That means, that this models turn function uses
 * a history. The last x steps can be stored in history and then these are used for the turn function.
 */
class Ant2DConv extends Ant2D{
  
  protected var history: Array[Int] = null
  
  protected var initHistory: Array[Int] = null
  
  /**
   * Places v in the history and drops for that the oldest history value.
   * 
   * @param v new element for history.
   */
  protected def updateHistory(v : Int) = {
    for(i <- history.length-1 to 1 by -1){
      history(i) = history(i-1)
    }
    history(0) = v
  }
  
  /**
   * Sums up the history and returns that value modulo two.
   * 
   * @return 0 or 1 depending on the history.
   */
  protected def historyToValue: Int = {
    var ret = 0
    history.foreach(ret += _)
    ret = ret%2
    return ret
  }
  
  /**
   * The value in memory at current position is added to history. If historyToValue is 0 turn left, else if 1 turn right.
   */
  override
  protected def turn = {    
    val i = positionToIndex
    updateHistory(memory(i))
    
    if(historyToValue == 0) direction -= 1
    else direction += 1
    
    if(direction > 3) direction -= 4
    else if(direction < 0) direction += 4
  }
  
  /**
   * Since for this model is no inversion implemented, a simpler step function is used.
   */
  override
  def step = {
    turn
    recolor
    forward
  }
  
  /**
   * Print function of the model. This displays all relevant variables of the model.
   */
  override
  def print = {
    Control.fOut("direction = "+direction)
    
    var s = "position = ("+position(0)
    for(i <- 1 to Control.dimension-1){
      s += " "+position(i) 
    }
    s += ")"
    Control.fOut(s)
    
    s = "history = ("+history(0)
    for(i <- 1 to history.length-1){
      s += " "+history(i) 
    }
    s += ")"
    Control.fOut(s)
    
    Control.fOut("memory:")
    s = ""
    var c = 0
    for(i <- 0 to scala.math.pow(Control.size, Control.dimension).toInt-1){
      s += memory(i)+" "
      c += 1
      if(c >= 50){
        Control.fOut(s)
        s = ""
        c = 0
      }
    }
    if(s != ""){
      Control.fOut(s)
    }
  }
  
  /**
   * Is used to add the initial histroy to the model. Must be used before the step function is called.
   * Thereby the history size is defined, too.
   */
  override
  def setParameter(param: Array[Int]) = {
    history = new Array[Int](param.length)
    initHistory = new Array[Int](param.length)
    for(i <- 0 to param.length-1){
      history(i) = param(i)
      initHistory(i) = param(i)
    }
  }
  
  /**
   * Resets position, direction and history.
   */
  override
  def reset{
    position(0) = 0
    position(1) = 0
    direction = 0
    if(history != null){
      for(i <- 0 to initHistory.length-1){
        history(i) = initHistory(i)
      }
    }
  }
  
  /**
   * This function is not implemented for this model.
   */
  override
  def invertAnt = {
    Control.fOut("not invertable")
  }
  
  /**
   * This model is not invertible.
   * 
   * @return false
   */
  override
  def isInvertible() : Boolean = {
    return false
  }
}