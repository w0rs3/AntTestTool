/**
* MIT License
* 
* Copyright (c) 2021 Jonas Lummerzheim
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*/
package main.scala

/**
 * This model extends the Ant model. Convolution is added. That means, that this models turn function uses
 * a history. The last x steps can be stored in history and then these are used for the turn function.
 */
class AntConv extends Ant {    
  protected var history : Array[Int] = null
  
  protected var initHistory : Array[Int] = null
  
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
   * Sums up the history and returns that value.
   * 
   * @return sum of history.
   */
  protected def historyToValue: Int = {
    var ret = 0
    history.foreach(ret += _)
    return ret
  }
  
  /**
   * This function rotates the ant and uses upadeteDimRing, invertDimRing and shiftDimRing for that. 
   * The value in memory at current position is added to history. Then the value for further calculation is calculated by
   * summing up the history modulo the color set size. The further steps are like in Ant.
   */
  override
  protected def turn = {
    updateHistory(memory(positionToIndex))
    val value = historyToValue%colorSetSize
    var v = 0
    var invert = false
    if(value >= Control.dimension-1) {
      invert = true
      direction *= -1
      v = value-Control.dimension+1
    }else{
      v = value
    }
    
    updateDimRing(dimRing(v+1))
    
    if(invert){
      invertDimRing
      shiftDimRing(Control.dimension-1)
    }
    
  }
  
  /**
   * simplified step function. One whole step is done. Therefore turn, recolor and forward are used.
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
    
    s = "dimRing = ("+dimRing(0)
    for(i <- 1 to Control.dimension-1){
      s += " "+dimRing(i) 
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
   * Resets position, dimRing and history.
   */
  override
  def reset = {
    for(i <- 0 to position.length-1) position(i) = 0
    for(i <- 0 to dimRing.length-1) dimRing(i) = i
    direction = 1
    for(i <- 0 to initHistory.length-1){
      history(i) = initHistory(i)
    }
  }
  
  /**
   * Not implemented for this model.
   */
  override
  def invertAnt = {
    Control.fOut("not invertable")
  }
  
  /**
   * This model is not invertible.
   * @return false
   */
  override
  def isInvertible() : Boolean = {
    return false
  }
}
