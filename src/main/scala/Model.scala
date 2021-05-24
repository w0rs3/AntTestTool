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
 * Every ant implementation must extend this trait.
 */
trait Model {
  
  /**
   * Copies the content of arr into the memory.
   * 
   * @param arr new memory
   */
  def write(arr : Array[Int])
  
  /**
   * The step function of the model
   */
  def step
  
  /**
   * Getter function for the memory
   * 
   * @return the memory of the model
   */
  def read : Array[Int]
  
  /**
   * Print function of the model. This should display all relevant variables of the model.
   */
  def print
  
  /**
   * A function used to add more parameters if the model needs that.
   * 
   * @param param an array containing values.
   */
  def setParameter(param: Array[Int])
  
  /**
   * Resets the model except its memory.
   */
  def reset
  
  /**
   * Inverts the model.
   */
  def invertAnt
  
  /**
   * Override this function if the model is not invertible or invertAnt is not implemented.
   * 
   * @return true if the model is invertible, else false
   */
  def isInvertible() : Boolean = {return true}
}
