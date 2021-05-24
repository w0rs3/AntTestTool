package main.scala

/**
 * This model implements the classical langton's ant on a 2D memory curved to a torus.
 * The memory has a total_size = size^2 and contains only zeros and ones.
 */
class Ant2D extends Model{
  
  protected var memory : Array[Int] = new Array[Int](scala.math.pow(Control.size, 2).toInt)
  
  protected var position: Array[Int] = new Array[Int](2)
  
  protected var direction = 0
  
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
   * Changes the direction based on the value found at the actual memory index based on the position.
   * Thereby 0 is left and 1 is right turn.
   */
  protected def turn = {
    if(memory(positionToIndex) == 0) direction -= 1
    else direction += 1
    
    if(direction > 3) direction -= 4
    else if(direction < 0) direction += 4
  }
  
  /**
   * Recolors the value found at the actual memory index based on the position. This function flips a 0 to 1 and a 1 to 0.
   */
  protected def recolor = {
    memory(positionToIndex) = (memory(positionToIndex)+1)%2
  }
  
  /**
   * The model moves one step forward. Changes the position based on the current direction.
   */
  protected def forward = {
    direction match {
      case 0 => position(0) += 1
      case 1 => position(1) += 1
      case 2 => position(0) -= 1
      case 3 => position(1) -= 1
    }
    if(position(0) < 0) position(0) += Control.size 
    if(position(0) > Control.size-1) position(0) -= Control.size
    if(position(1) < 0) position(1) += Control.size 
    if(position(1) > Control.size-1) position(1) -= Control.size
  }
  
  /**
   * One whole step is done. Therefore turn, recolor and forward are used.
   */
  override
  def step = {
    turn
    recolor
    forward
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
   * Resets position and direction.
   */
  override
  def reset{
    position(0) = 0
    position(1) = 0
    direction = 0
  }
  
  /**
   * Inverts this model. Here this is done by inverting the direction and moving one step forward. Only this model inverts this way.
   */
  override
  def invertAnt = {
    direction = (direction+2)%4
    forward
  }
}