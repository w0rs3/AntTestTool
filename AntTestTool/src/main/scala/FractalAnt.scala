package main.scala

/**
 * This model extends Ant. A parental hierarchy is added. Depth describes the number of hierarchical levels. The lowest level acts like a normal Ant.
 * All other levels have exactly that much children like their memory is big. If one value in the memory of a FractalAnt is recolored, the corresponding
 * child recolors its whole memory and all of its children, then they do the same and so on. The first value in the memory of a child represents the corresponding 
 * memory value of the parent.
 * The parameters must confirm dimension = (depth+1)*subdimension.
 * All FractalAnts have a memory of total_size = size^subdimension
 */
class FractalAnt extends Ant {
  
  protected var children : Array[FractalAnt] = null
  protected var depth = 0
  protected var subdimension = 0
  
  /**
   * Shifts dimRing value positions to the left.
   * 
   * @value positions to shift
   */
  override
  protected def shiftDimRing(value: Int) = {
    var tmpArray = new Array[Int](subdimension)
    var v = value
    for(i <- 0 to subdimension-1){
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
  override
  protected def updateDimRing(value: Int) = {
    var tmpArray = new Array[Int](subdimension)
    tmpArray(0) = value
    var d = 1
    for(i <- 0 to subdimension-1){
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
  override
  protected def invertDimRing = {
    var tmpArray = new Array[Int](subdimension)
    var c = subdimension-1
    for(i <- 0 to subdimension-1){
      tmpArray(i) = dimRing(c)
      c -= 1
    }
    dimRing = tmpArray
  }
  
  /**
   * Recolors the value found at the actual memory index based on the position. Increases the found value by 1
   * and then it uses mod 2*(subdimension-1) on it. Recolors children too.
   */
  override
  protected def recolor = {
    val index = positionToIndex
    memory(index) = (memory(index)+1)%colorSetSize
    if(depth>1){
      children(index).recolorAll
    }
  }
  
  /**
   * Recolors the whole memory and all children.
   */
  protected def recolorAll : Unit = {
    for(i <- 0 to memory.length-1){
      memory(i) = (memory(i)+1)%colorSetSize
      if(depth>=1){
        children(i).recolorAll
      }
    }
  }
  
  /**
   * This function rotates the ant and uses upadeteDimRing, invertDimRing and shiftDimRing for that. If the value of current position is >= subdimension-1
   * then this value is reduced by subdimension-1, stored as v and invert is true, else invert is false and v equals value. Then updateDimring is called 
   * using dimRing(v+1). if invert is true, the direction is multiplied by -1, invertDimRing and shiftDimRing are used. Therefore subdimension-1 is passed to
   * shiftDimRing.
   */
  override
  protected def turn = {
    val value = memory(positionToIndex)
    var v = 0
    var invert = false
    if(value >= subdimension-1) {
      invert = true
      direction *= -1
      v = value-subdimension+1
    }else{
      v = value
    }
    
    updateDimRing(dimRing(v+1))
    
    if(invert){
      invertDimRing
      shiftDimRing(subdimension-1)
    }
    
  }
  
  /**
   * Calculates the index of memory that is related to the actual position.
   * 
   * @return index
   */
  override
  protected def positionToIndex : Int = {
    var index = 0
    var c = 1
    for(p <- position){
      index += p * scala.math.pow(Control.size, subdimension-c).toInt
      c += 1
    }
    return index
  }
  
  /**
   * Calculates a modulo subdimension and is usable for negative values.
   * 
   * @param a input value
   * @return a mod subdimension
   */
  override
  protected def modd(a: Int) : Int = {
    if(a >= 0){
      return a%subdimension
    }else{
      return modd(a+subdimension)
    }
  }
  
  /**
   * Creates the fractal structure and handels the initialization of memory. Therefore the memory is split in parts given to all children of the lowest level.
   * All other memories are created by interpreting the first value in memory of a child as value for the memory at that position.
   * @param arr new memory
   */
  override
  def write(arr : Array[Int]) = {
    
    memory = new Array[Int](scala.math.pow(Control.size, subdimension).toInt)
    position = new Array[Int](subdimension)
    colorSetSize = (subdimension-1)*2
    dimRing = new Array[Int](subdimension)
    
    for(i <- 0 to subdimension-1){
      dimRing(i) = i
    }
    
    val ts = scala.math.pow(Control.size, subdimension).toInt
    if(depth>=1){
      children = new Array[FractalAnt](scala.math.pow(Control.size, subdimension).toInt)
   
      for(i <- 0 to ts-1){
        children(i) = new FractalAnt
        children(i).setParameter(Array(depth-1, subdimension))
        val tds = scala.math.pow(Control.size, depth*subdimension).toInt
        var tmp = new Array[Int](tds)
        for(j <- 0 to tds-1){
          tmp(j) = arr(i*tds+j)
        }
        memory(i) = tmp(0)
        children(i).write(tmp)
      }
    }else{
      for(i <- 0 to ts-1){
        memory(i) = arr(i)
      }
    }
  }
  
  /**
   * Assembels all memories of childeren of the lowest level and exports that as new array.
   * 
   * @return the whole memory of the fractal.
   */
  override
  def read() : Array[Int] = {
    if(depth>=1){
      val ts = scala.math.pow(Control.size, subdimension).toInt
      val tds = scala.math.pow(Control.size, depth*subdimension).toInt
      var tmp = new Array[Int](tds*ts)
      for(i <- 0 to ts-1){
        var mem = children(i).read()
        for(j <- 0 to tds-1){
          tmp(i*tds+j) = mem(j)
        }
      }
      return tmp
    }else{
      return memory
    }
  }
  
  /**
   * Print function of the model. This displays all relevant variables of the model.
   */
  override
  def print() = {
    var s = ""
    for(j <- 0 to depth-1){
      s += "+ "
    }
    Control.fOut(s)
    Control.fOut("direction = "+direction)
  
    s = "position = ("+position(0)
    for(i <- 1 to subdimension-1){
      s += " "+position(i) 
    }
    s += ")"
    Control.fOut(s)
    
    s = "dimRing = ("+dimRing(0)
    for(i <- 1 to subdimension-1){
      s += " "+dimRing(i) 
    }
    s += ")"
    Control.fOut(s)   
    
    Control.fOut("memory:")
    s = ""
    var c = 0
    for(i <- 0 to scala.math.pow(Control.size, subdimension).toInt-1){
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
    if(depth>=1){
      for(i <- 0 to children.length-1){
        children(i).print()
      }
    }
  }
  
  /**
   * Step is first performed, then step on all children is called and then the memory is updated. If the
   * model is inverted this function inverts itself.
   */
  override
  def step = {
    if(inverted){
      if(depth>=1){
        for(i <- children.length-1 to 0 by -1){
          children(i).step
          memory(i) = children(i).memory(0)
        }
      }
      super.step
    }else{
      super.step
      if(depth>=1){
        for(i <- 0 to children.length-1){
          children(i).step
          memory(i) = children(i).memory(0)
        }
      }
    }
  }
  
  /**
   * Is used to set depth and subdimension. Index zero is depth and index one is subdimension.
   * 
   * @param param array consisting of depth and subdimension
   */
  override
  def setParameter(param : Array[Int]) = {
    depth = param(0)
    subdimension = param(1)
  }
  
  /**
   * Resets itself and all children.
   */
  override
  def reset = {
    super.reset
    if(children != null){
      for(i <- 0 to children.length-1){
        children(i).reset
      }
    }
  }
  
  /**
   * Inverts itself and all children.
   */
  override
  def invertAnt = {
    super.invertAnt
    if(depth>=1){
      for(i <- 0 to children.length-1){
        children(i).invertAnt
      }
    }
  }
  
  /**
   * Inverted recolor function.
   */
  override
  protected def invertedRecolor = {
    val index = positionToIndex
    memory(index) -= 1
    if(memory(index)<0) memory(index) += colorSetSize
    if(depth>1){
      children(index).invertedRecolorAll
    }
  }
  
  /**
   * Inverted recolorAll function.
   */
  protected def invertedRecolorAll : Unit = {
    for(i <- memory.length-1 to 0 by -1){
      memory(i) -= 1
      if(memory(i)<0) memory(i) += colorSetSize
      if(depth>=1){
        children(i).invertedRecolorAll
      }
    }
  }
  
  /**
   * Inverted turn function.
   */
  override
  protected def invertedTurn = {
    val value = memory(positionToIndex)
    var v = 0
    var invert = false
    if(value >= subdimension-1) {
      invert = true
      direction *= -1
      v = value-subdimension+1
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
   * Inverted updateDimRing function.
   */
  override
  protected def invertedUpdateDimRing(value: Int) = {
    var tmpArray = new Array[Int](subdimension)
    tmpArray(value) = dimRing(0)
    var d = 0
    for(i <- 1 to subdimension-1){
      if(d == value){
        d += 1
      }
      tmpArray(d) = dimRing(i)
      d += 1
    }
    dimRing = tmpArray
  }
  
}