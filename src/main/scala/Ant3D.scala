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
 * This model is based on four colors and working on a 3D memory curved to a torus. Its state in the 3D space is represented by the position,
 * front and up. Here front and up are values in range zero to five and represent an axis and a direction on this axis. Therefore front
 * points in front direction and up points to the up direction of the ant. The turn function allows to change front in all four directions,
 * that are not on the actual axis. Zero turns up, one turns right, two turns down and tree turns left.
 * The memory has a total_size = size^3 and contains only zeros, ones, twos and threes.
 */
class Ant3D extends Model{
  
  protected var memory : Array[Int] = new Array[Int](scala.math.pow(Control.size, 3).toInt)
  
  protected var position: Array[Int] = new Array[Int](3)
  
  protected var front: Int = 0 //0-5 (front) x,y,z,-x,-y,-z
  
  protected var up: Int = 1 //0-5 (up) x,y,z,-x,-y,-z
  
  protected var inverted: Boolean = false
  
  reset
  
  /**
   * Copies the content of arr into the memory.
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
   * The turn function allows to change the direction in all four directions, that are not on the actual axis.
   * It uses position, front, up and the value in memory at the index related to the actual position
   * to calculate the next direction and rotation. Zero turns up, one turns right, two turns down and tree turns left.
   */
  protected def turn{
    front match{
      case 0 => up match{  //x
        
        case 1 => memory(positionToIndex) match{  //y
          case 0 => {
            front = 1 //y
            up = 3 //-x
          }
          case 1 => front = 5 //-z
          case 2 =>  {
            front = 4 //-y
            up = 0 //x
          }
          case 3 => front = 2 //z
        }
        
        case 2 => memory(positionToIndex) match{  //z
          case 0 => {
            front = 2 //z
            up = 3 //-x
          }
          case 1 => front = 1 //y
          case 2 => {
            front = 5 //-z
            up = 0 //x
          }
          case 3 => front = 4 //-y
        }
        
        case 4 => memory(positionToIndex) match{  //-y
          case 0 => {
            front = 4 //-y
            up = 3 //-x
          }
          case 1 => front = 2 //z
          case 2 =>{
            front = 1 //y
            up = 0 //x
          }
          case 3 => front = 5 //-z
        }
        
        case 5 => memory(positionToIndex) match{  //-z
          case 0 => {
            front = 5 //-z
            up = 3 //-x
          }
          case 1 => front = 4 //-y
          case 2 =>{
            front = 2 //z
            up = 0 //x
          }
          case 3 => front = 1 //y
        }
        
      }
      
      case 1 => up match{  //y
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 0 //x
            up = 4 //-y
          }
          case 1 => front = 2 //z
          case 2 =>{
            front = 3 //-x
            up = 1 //y
          }
          case 3 => front = 5 //-z
        }
        
        case 2 => memory(positionToIndex) match{ //z
          case 0 => {
            front = 2 //z
            up = 4 //-y
          }
          case 1 => front = 3 //-x
          case 2 =>{
            front = 5 //-z
            up = 1 //y
          }
          case 3 => front = 0 //x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 3 //-x
            up = 4 //-y
          }
          case 1 => front = 5 //-z
          case 2 =>{
            front = 0 //x
            up = 1 //y
          }
          case 3 => front = 2 //z
        }
        
        case 5 => memory(positionToIndex) match{ //-z
          case 0 => {
            front = 5 //-z
            up = 4 //-y
          }
          case 1 => front = 0 //x
          case 2 =>{
            front = 2 //z
            up = 1 //y
          }
          case 3 => front = 3 //-x
        }
        
      }
      
      case 2 => up match{ //z
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 0 //x
            up = 5 //-z
          }
          case 1 => front = 4 //-y
          case 2 =>{
            front = 3 //-x
            up = 2 //z
          }
          case 3 => front = 1 //y
        }
        
        case 1 => memory(positionToIndex) match{ //y
          case 0 => {
            front = 1 //y
            up = 5 //-z
          }
          case 1 => front = 0 //x
          case 2 =>{
            front = 4 //-y
            up = 2 //z
          }
          case 3 => front = 3 //-x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 3 //-x
            up = 5 //-z
          }
          case 1 => front = 1 //y
          case 2 =>{
            front = 0 //x
            up =  2 //z
          }
          case 3 => front = 4 //-y
        }
        
        case 4 => memory(positionToIndex) match{ //-y
          case 0 => {
            front = 4 //-y
            up = 5 //-z
          }
          case 1 => front = 3 //-x
          case 2 =>{
            front = 1 //y
            up = 2 //z
          }
          case 3 => front = 0 //x
        }
        
      }
      
      case 3 => up match{ //-x
        
        case 1 => memory(positionToIndex) match{ //y
          case 0 => {
            front = 1 //y
            up = 0 //x
          }
          case 1 => front = 2 //z
          case 2 =>{
            front = 4 //-y
            up = 3 //-x
          }
          case 3 => front = 5 //-z
        }
        
        case 2 => memory(positionToIndex) match{ //z
          case 0 => {
            front = 2 //z
            up = 0 //x
          }
          case 1 => front = 4 //-y
          case 2 =>{
            front = 5 //-z
            up = 3 //-x
          }
          case 3 => front = 1 //y
        }
        
        case 4 => memory(positionToIndex) match{ //-y
          case 0 => {
            front = 4 //-y
            up = 0 //x
          }
          case 1 => front = 5 //-z
          case 2 =>{
            front = 1 //y
            up = 3 //-x
          }
          case 3 => front = 2 //z
        }
        
        case 5 => memory(positionToIndex) match{ //-z
          case 0 => {
            front = 5 //-z
            up = 0 //x
          }
          case 1 => front = 1 //y 
          case 2 =>{
            front = 2 //z
            up = 3 //-x
          }
          case 3 => front = 4 //-y
        }
        
      }
      
      case 4 => up match{ //-y
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 0 //x
            up = 1 //y
          }
          case 1 => front = 5 //-z
          case 2 =>{
            front = 3 //-x
            up = 4 //-y
          }
          case 3 => front = 2 //z
        }
        
        case 2 => memory(positionToIndex) match{ //z
          case 0 => {
            front = 2 //z
            up = 1 //y
          }
          case 1 => front = 0 //x
          case 2 =>{
            front = 5 //-z
            up = 4 //-y
          }
          case 3 => front = 3 //-x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 3 //-x
            up = 1 //y
          }
          case 1 => front = 2 //z
          case 2 =>{
            front = 0 //x
            up = 4 //-y
          }
          case 3 => front = 5 //-z
        }
        
        case 5 => memory(positionToIndex) match{ //-z
          case 0 => {
            front = 5 //-z
            up = 1 //y
          }
          case 1 => front = 3 //-x
          case 2 =>{
            front = 2 //z
            up = 4 //-y
          }
          case 3 => front = 0 //x
        }
        
      }
      
      case 5 => up match{ //-z
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 0 //x
            up = 2 //z
          }
          case 1 => front = 1 //y
          case 2 =>{
            front = 3 //-x
            up = 5 //-z
          }
          case 3 => front = 4 //-y
        }
        
        case 1 => memory(positionToIndex) match{ //y
          case 0 => {
            front = 1 //y
            up = 2 //z
          }
          case 1 => front = 3 //-x
          case 2 =>{
            front = 4 //-y
            up = 5 //-z
          }
          case 3 => front = 0 //x 
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 3 //-x
            up = 2 //z
          }
          case 1 => front = 4 //-y
          case 2 =>{
            front = 0 //x
            up = 5 //-z
          }
          case 3 => front = 1 //y
        }
        
        case 4 => memory(positionToIndex) match{ //-y
          case 0 => {
            front = 4 //-y
            up = 2 //z
          }
          case 1 => front = 0 //x
          case 2 =>{
            front = 1 //y
            up = 5 //-z
          }
          case 3 => front = 3 //-x
        }
        
      }
      
    }
    
  }
  
  /**
   * This function allows to change the direction in all four directions, that are not on the actual axis. This function is the inverted turn function.
   */
  protected def invertTurn{
    front match{
      case 0 => up match{  //x
        
        case 1 => memory(positionToIndex) match{  //y
          case 0 => {
            front = 4 //-y
            up = 0 //x
          }
          case 1 => front = 2 //z
          case 2 =>  {
            front = 1 //y
            up = 3 //-x
          }
          case 3 => front = 5 //-z
        }
        
        case 2 => memory(positionToIndex) match{  //z
          case 0 => {
            front = 5 //-z
            up = 0 //x
          }
          case 1 => front = 4 //-y
          case 2 =>  {
            front = 2 //z
            up = 3 //-x
          }
          case 3 => front = 1 //y
        }
        
        case 4 => memory(positionToIndex) match{  //-y
          case 0 => {
            front = 1 //y
            up = 0 //x
          }
          case 1 => front = 5 //-z
          case 2 =>  {
            front = 4 //-y
            up = 3 //-x
          }
          case 3 => front = 2 //z
        }
        
        case 5 => memory(positionToIndex) match{  //-z
          case 0 => {
            front = 2 //z
            up = 0 //x
          }
          case 1 => front = 1 //y
          case 2 =>  {
            front = 5 //-z
            up = 3 //-x
          }
          case 3 => front = 4 //-y
        }
        
      }
      
      case 1 => up match{  //y
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 3 //-x
            up = 1 //y
          }
          case 1 => front = 5 //-z
          case 2 =>  {
            front = 0 //x
            up = 4 //-y
          }
          case 3 => front = 2 //z
        }
        
        case 2 => memory(positionToIndex) match{ //z
          case 0 => {
            front = 5 //-z
            up = 1 //y
          }
          case 1 => front = 0 //x
          case 2 =>  {
            front = 2 //z
            up = 4 //-y
          }
          case 3 => front = 3 //-x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 0 //x
            up = 1 //y
          }
          case 1 => front = 2 //z
          case 2 =>  {
            front = 3 //-x
            up = 4 //-y
          }
          case 3 => front = 5 //-z
        }
        
        case 5 => memory(positionToIndex) match{ //-z
          case 0 => {
            front = 2 //z
            up = 1 //y
          }
          case 1 => front = 3 //-x
          case 2 =>  {
            front = 5 //-z
            up = 4 //-y
          }
          case 3 => front = 0 //x
        }
        
      }
      
      case 2 => up match{ //z
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 3 //-x
            up = 2 //z
          }
          case 1 => front = 1 //y
          case 2 =>  {
            front = 0 //x
            up = 5 //-z
          }
          case 3 => front = 4 //-y
        }
        
        case 1 => memory(positionToIndex) match{ //y
          case 0 => {
            front = 4 //-y
            up = 2 //z
          }
          case 1 => front = 3 //-x
          case 2 =>  {
            front = 1 //y
            up = 5 //-z
          }
          case 3 => front = 0 //x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 0 //x
            up = 2 //z
          }
          case 1 => front = 4 //-y
          case 2 =>  {
            front = 3 //-x
            up = 5 //-z
          }
          case 3 => front = 1 //y
        }
        
        case 4 => memory(positionToIndex) match{ //-y
          case 0 => {
            front = 1 //y
            up = 2 //z
          }
          case 1 => front = 0 //x
          case 2 =>  {
            front = 4 //-y
            up = 5 //-z
          }
          case 3 => front = 3 //-x
        }
        
      }
      
      case 3 => up match{ //-x
        
        case 1 => memory(positionToIndex) match{ //y
          case 0 => {
            front = 4 //-y
            up = 3 //-x
          }
          case 1 => front = 5 //-z
          case 2 =>  {
            front = 1 //y
            up = 0 //x
          }
          case 3 => front = 2 //z
        }
        
        case 2 => memory(positionToIndex) match{ //z
          case 0 => {
            front = 5 //-z
            up = 3 //-x
          }
          case 1 => front = 1 //y
          case 2 =>  {
            front = 2 //z
            up = 0 //x
          }
          case 3 => front = 4 //-y
        }
        
        case 4 => memory(positionToIndex) match{ //-y
          case 0 => {
            front = 1 //y
            up = 3 //-x
          }
          case 1 => front = 2 //z
          case 2 =>  {
            front = 4 //-y
            up = 0 //0
          }
          case 3 => front = 5 //-z
        }
        
        case 5 => memory(positionToIndex) match{ //-z
          case 0 => {
            front = 2 //z
            up = 3 //-x
          }
          case 1 => front = 4 //-y
          case 2 =>  {
            front = 5 //-z
            up = 0 //x
          }
          case 3 => front = 1 //y
        }
        
      }
      
      case 4 => up match{ //-y
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 3 //-x
            up = 4 //-y
          }
          case 1 => front = 2 //z
          case 2 =>  {
            front = 0 //x
            up = 1 //y
          }
          case 3 => front = 5 //-z
        }
        
        case 2 => memory(positionToIndex) match{ //z
          case 0 => {
            front = 5 //-z
            up = 4 //-y
          }
          case 1 => front = 3 //-x
          case 2 =>  {
            front = 2 //z
            up = 1 //y
          }
          case 3 => front = 0 //x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 0 //x
            up = 4 //-y
          }
          case 1 => front = 5 //-z
          case 2 =>  {
            front = 3 //-x
            up = 1 //y
          }
          case 3 => front = 2 //z
        }
        
        case 5 => memory(positionToIndex) match{ //-z
          case 0 => {
            front = 2 //z
            up = 4 //-y
          }
          case 1 => front = 0 //x
          case 2 =>  {
            front = 5 //-z
            up = 1 //y
          }
          case 3 => front = 3 //-x
        }
        
      }
      
      case 5 => up match{ //-z
        
        case 0 => memory(positionToIndex) match{ //x
          case 0 => {
            front = 3 //-x
            up = 5 //-z
          }
          case 1 => front = 4 //-y
          case 2 =>  {
            front = 0 //x
            up = 2 //z
          }
          case 3 => front = 1 //y
        }
        
        case 1 => memory(positionToIndex) match{ //y
          case 0 => {
            front = 4 //-y
            up = 5 //-z
          }
          case 1 => front = 0 //x
          case 2 =>  {
            front = 1 //y
            up = 2 //z
          }
          case 3 => front = 3 //-x
        }
        
        case 3 => memory(positionToIndex) match{ //-x
          case 0 => {
            front = 0 //x
            up = 5 //-z
          }
          case 1 => front = 1 //y
          case 2 =>  {
            front = 3 //-x
            up = 2 //z
          }
          case 3 => front = 4 //-y
        }
        
        case 4 => memory(positionToIndex) match{ //-y
          case 0 => {
            front = 1 //y
            up = 5 //-z
          }
          case 1 => front = 3 //-x
          case 2 =>  {
            front = 4 //-y
            up = 2 //z
          }
          case 3 => front = 0 //x
        }
        
      }
      
    }
    
  }
  
  /**
   * Recolors the value found at the actual memory index based on the position. This function flips a 0 to 1 and a 1 to 0.
   */
  protected def recolor = {
    if(inverted){
      var i = positionToIndex
      memory(i) -= 1
      if(memory(i)<0) memory(i) = 3
    }else{
      memory(positionToIndex) = (memory(positionToIndex)+1)%4
    }
  }
  
  /**
   * The model moves one step forward. Changes the position based on the current direction.
   */
  protected def forward = {
    front match {
      case 0 => position(0) += 1
      case 1 => position(1) += 1
      case 2 => position(2) += 1
      case 3 => position(0) -= 1
      case 4 => position(1) -= 1
      case 5 => position(2) -= 1
    }
    if(position(0) < 0) position(0) += Control.size 
    if(position(0) > Control.size-1) position(0) -= Control.size
    if(position(1) < 0) position(1) += Control.size 
    if(position(1) > Control.size-1) position(1) -= Control.size
    if(position(2) < 0) position(2) += Control.size 
    if(position(2) > Control.size-1) position(2) -= Control.size
  }
  
  /**
   * The model moves one step backward. Changes the position based on the inverted current direction.
   */
  protected def backward = {
    front = (front+3)%6
    forward
    front = (front+3)%6
  }
  
  /**
   * One whole step is done. Therefore turn, recolor and forward are used. If inverted is true,
   * this function inverts itself and uses inverted functions.
   */
  override
  def step = {
    if(inverted){
      backward
      recolor
      invertTurn
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
    Control.fOut("direction = "+front)
    Control.fOut("rotation = "+up)
    
    var s = "position = ("+position(0)
    for(i <- 1 to Control.dimension-1){
      s += " "+position(i) 
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
  def setParameter(param: Array[Int]) = {
    
  }
  
  /**
   * Resets position, direction, rotation and inverted.
   */
  override
  def reset{
    position(0) = 0
    position(1) = 0
    position(2) = 0
    front = 0
    up = 1
    inverted = false
  }
  
  /**
   * Inverts the ant. Therefore only inverted is set to true.
   */
  override
  def invertAnt = {
    inverted = true
  }
}
