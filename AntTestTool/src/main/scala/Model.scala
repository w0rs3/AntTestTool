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