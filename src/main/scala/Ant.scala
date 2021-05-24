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
 * This model can be initialized with any dimension and size. Its state in the space is represented by the
 * position, direction and the dimRing. Here direction is zero or one and describes the direction on the actual
 * axis. The current axis is dimRing(0) and the rotation is represented by the order of dimensions in the dimRing.
 * The turn function is based on replacement, inversion and shift operation on the dimRing.
 * The memory has a total_size = size^dimension and The number of colors is 2*(dimension-1).
 */
class Ant extends Model {  
  
  protected var memory : Array[Int] = new Array[Int](scala.math.pow(Control.size, Control.dimension).toInt)
  
  protected var position: Array[Int] = new Array[Int](Control.dimension)
  
  protected var colorSetSize = (Control.dimension-1)*2
  
  protected var dimRing: Array[Int] = new Array[Int](Control.dimension)
  protected var direction: Int = 1
  
  for(i <- 0 to Control.dimension-1){
    dimRing(i) = i
  }
  
  protected var inverted = false
  
  /**
   * Shifts dimRing value positions to the left.
   * 
   * @value positions to shift
   */
  protected def shiftDimRing(value: Int) = {
    var tmpArray = new Array[Int](Control.dimension)
    var v = value
    for(i <- 0 to Control.dimension-1){
      tmpArray(i) = dimRing(v)
      v = modd(v+1)
    }
    dimRing = tmpArray
  }
  
  /**
   * Places value at index zero of dimRing. Then filling all remaining dimensions in the old order to dimRing.
   * 
   * @param value dimension that will placed in dimRing(0).
   */
  protected def updateDimRing(value: Int) = {
    var tmpArray = new Array[Int](Control.dimension)
    tmpArray(0) = value
    var d = 1
    for(i <- 0 to Control.dimension-1){
      if(dimRing(i) != value){
        tmpArray(d) = dimRing(i)
        d += 1
      }
    }
    dimRing = tmpArray
  }
  
  /**
   * Inverts the order of the dimRing.
   */
  protected def invertDimRing = {
    var tmpArray = new Array[Int](Control.dimension)
    var c = Control.dimension-1
    for(i <- 0 to Control.dimension-1){
      tmpArray(i) = dimRing(c)
      c -= 1
    }
    dimRing = tmpArray
  }
  
  
  /**
   * Calculates the index of memory that is related to the actual position.
   * 
   * @return index
   */
  protected def positionToIndex : Int = {
    var index = 0
    var c = 1
    for(p <- position){
      index += p * scala.math.pow(Control.size, Control.dimension-c).toInt
      c += 1
    }
    return index
  }
  
  /**
   * Recolors the value found at the actual memory index based on the position. Increases the found value by 1
   * and then it uses mod 2*(dimension-1) on it.
   */
  protected def recolor = {
    val index = positionToIndex
    memory(index) = (memory(index)+1)%colorSetSize
  }
  
  /**
   * This function rotates the ant and uses upadeteDimRing, invertDimRing and shiftDimRing for that. If the value of current position is >= dimension-1
   * then this value is reduced by dimension-1, stored as v and invert is true, else invert is false and v equals value. Then updateDimring is called 
   * using dimRing(v+1). if invert is true, the direction is multiplied by -1, invertDimRing and shiftDimRing are used. Therefore dimension-1 is passed to
   * shiftDimRing.
   */
  protected def turn = {
    val value = memory(positionToIndex)
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
   * Calculates a modulo dimension and is usable for negative values.
   * 
   * @param a input value
   * @return a mod dimension
   */
  protected def modd(a: Int) : Int = {
    if(a >= 0){
      return a%Control.dimension
    }else{
      return modd(a+Control.dimension)
    }
  }
  
  /**
   * Calculates a modulo size and is usable for negative values.
   * 
   * @param a input value
   * @return a mod size
   */
  protected def mods(a: Int) : Int = {
    if(a >= 0){
      return a%Control.size
    }else{
      return mods(a+Control.size)
    }
  }
  
  /**
   * Updates position based on dimRing(0) and direction.
   */
  protected def forward = {
    position(dimRing(0)) = mods(position(dimRing(0))+direction)
  }
  
  /**
   * Copys the content of arr into the memory.
   * 
   * @param arr new memory
   */
  override
  def write(arr : Array[Int]) = {
    for(i <- 0 to arr.length-1){
      memory(i) = arr(i)
    }
  }
  
  /**
   * One whole step is done. Therefore turn, recolor and forward are used. If inverted is true,
   * this function inverts itself and uses inverted functions.
   */
  override
  def step = {
    if(inverted){
      backward
      invertedRecolor
      invertedTurn
    }else{
      turn
      recolor
      forward
    }
  }
  
  /**
   * Getter function for the memory
   * 
   * @return the memory of the model
   */
  override
  def read : Array[Int] = {
    return memory
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
   * Not needed for this model.
   * 
   * @param param
   */
  override
  def setParameter(param : Array[Int]) = {
    
  }
  
  /**
   * Resets position, direction and dimRing.
   */
  override
  def reset = {
    for(i <- 0 to position.length-1) position(i) = 0
    for(i <- 0 to dimRing.length-1) dimRing(i) = i
    direction = 1
    inverted = false
  }
  
  /**
   * Inverts the ant. Therefore only inverted is set to true.
   */
  override
  def invertAnt = {
    inverted = true
  }
  
  /**
   * Inverted version of recolor.
   */
  protected def invertedRecolor = {
    val index = positionToIndex
    memory(index) = (memory(index)-1)
    if(memory(index) < 0) memory(index) += colorSetSize
  }
  
  /**
   * Inverted version of turn.
   */
  protected def invertedTurn = {
    val value = memory(positionToIndex)
    var v = 0
    var invert = false
    if(value >= Control.dimension-1) {
      invert = true
      direction *= -1
      v = value-Control.dimension+1
    }else{
      v = value
    }
    
    if(invert){
      shiftDimRing(1)
      invertDimRing
    }
    invertedUpdateDimRing(v+1)
  }
  
  /**
   * Inverted version of updateDimRing.
   */
  protected def invertedUpdateDimRing(value: Int) = {
    var tmpArray = new Array[Int](Control.dimension)
    tmpArray(value) = dimRing(0)
    var d = 0
    for(i <- 1 to Control.dimension-1){
      if(d == value){
        d += 1
      }
      tmpArray(d) = dimRing(i)
      d += 1
    }
    dimRing = tmpArray
  }
  
  /**
   * Inverted version of forward.
   */
  protected def backward = {
    position(dimRing(0)) = mods(position(dimRing(0))-direction)
  }
}
