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
 * This model is an extension of Ant2D. The number of colors must be defind and for every color a decision must be set. A decision is 0 for left 
 * or 1 for right.
 */
class Ant2DMC extends Ant2D{
  
  protected var colors : Array[Int] = null
  protected var initColors : Array[Int] = null
  protected var inverted = false
  
  /**
   * This function changes the direction based on current direction and the color found at current postion.
   */
  override
  protected def turn = {
    if(colors(memory(positionToIndex)) == 0) direction -= 1
    else direction += 1
    
    if(direction > 3) direction -= 4
    else if(direction < 0) direction += 4
  }
  
  /**
   * If the model is inverted the value found in memory at current location is reduced by one, else it is increased by 1.
   * Both operations use modulo color set size.
   */
  override
  protected def recolor = {
    val i = positionToIndex
    if(inverted){
      memory(i) -= 1
      if(memory(i) < 0) memory(i) += colors.length
    }else{
      memory(i) = (memory(i)+1)%colors.length
    }
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
    
    s = "colors = ("+colors(0)
    for(i <- 1 to colors.length-1){
      s += " "+colors(i) 
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
   * This function is used to set the colors. It must be used after the model is created and before the step function is used.
   * 
   * @param param a array containing only zeros and ones.
   */
  override
  def setParameter(param: Array[Int]) = {
    colors = new Array[Int](param.length)
    initColors = new Array[Int](param.length)
    for(i <- 0 to param.length-1){
      colors(i) = param(i)
      initColors(i) = param(i)
    }
  }
  
  /**
   * Resets position, direction, inverted and colors. 
   */
  override
  def reset{
    position(0) = 0
    position(1) = 0
    direction = 0
    inverted = false
    if(initColors!=null){
      for(i <- 0 to initColors.length-1){
        colors(i) = initColors(i)
      }
    }
  }
  
  /**
   * Inverts the model. For that direction is inverted, one step forward is done and inverted is set to true. Then the color set
   * is updated by doing a right shift by 1, replacing all zeros by ones and vice versa.
   */
  override
  def invertAnt = {
    direction = (direction+2)%4
    forward
    inverted = true
    var tmp = new Array[Int](colors.length)
    for(i <- 0 to colors.length-1){
      tmp((i+1)%colors.length) = colors(i)
    }
    for(i <- 0 to colors.length-1){
      colors(i) = tmp(i)
    }
    for(i <- 0 to colors.length-1){
      colors(i) = (colors(i)+1)%2
    }
  }
}
