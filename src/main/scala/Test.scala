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
package control

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import scala.collection.mutable.ArrayBuffer
import main.scala.Control

/**
 * This object is used for the test menu of Control. Here all tests are implemented.
 */
object Test {  
  
  /** 
   * Resets and initializes the model with an all zero memory, do cycles steps and print the result. This test is for debugging purposes.
   * 
   * @param cycles number of steps
   */
  def testZero(cycles: Int) = {
    Control.model.reset
    Control.fOut("test zero with "+cycles+" cycles:")
    val size = scala.math.pow(Control.size, Control.dimension).toInt
    var mem = new Array[Int](size)
    for(i <- 0 to size-1){
      mem(i) = 0
    }
    Control.model.write(mem)
    
    if(Control.debug){
      Control.fOut("before:")
      Control.model.print
    }
    
    for(i <- 0 to cycles-1){
      Control.model.step
      
      if(Control.debug){
        Control.fOut("step "+i+":")
        Control.model.print
      }
      
    }
    
    if(Control.debug) println
    
    mem = Control.model.read
    for(i <- 0 to mem.length-1){
      print(mem(i)+" ")
      if(i%Control.size==Control.size-1) println
    }
    println
  }
  
  /**	
   * Resets and initializes the model with a random memory. The random values are created in a way to match all possible memory dimensions, sizes and
   * colors. The the step limit varies. The step limit for FraktalAnt is 100.000, all other models have the step limit multiplied by 10. In
   * debug mode the step limit is divided by 1000, because the outputs slow down the process. There is one attempt to detect a cycle of
   * maximum size equal or less the step limit. Then two more attempts are done to detect a lasso with a maximum ring size equal or less
   * the step limit. 
   * 
   * @return an integer value. If no cycle or lasso is found zero is returned. if a cycle is found, the ring size is returned as positive
   * value, else the ring size of the lasso is returned as negative value.
   */
  def testRandomRing : Int = {
    Control.model.reset
    Control.fOut("test random ring cycle count:")
    var steps = 0
    val r = scala.util.Random
    var size = scala.math.pow(Control.size, Control.dimension).toInt
    var mem = new Array[Int](size)
    if(Control.multiColor > 0){
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(Control.multiColor)
      }
    }else if(Control.subdimension <= 0){
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(2*(Control.dimension-1))
      }
    }else{
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(2*(Control.subdimension-1))
      }
    }
    
    Control.model.write(mem)
    
    if(Control.debug){
      println("init:")
      Control.model.print
    }
    
    var mem2 = new Array[Int](size)
    for(i <- 0 to size-1){
      mem2(i) = mem(i)
    }
    
    var stepLimit = 100000
    if(Control.subdimension<=0) stepLimit *= 10
    if(Control.debug) stepLimit /= 1000
    
    var lasso = 0
    var modelIsEqual = false
    while(!modelIsEqual && lasso < 4){
      if(steps >= stepLimit){
        lasso += 1
        steps = 0
        
        for(i <- 0 to size-1){
          mem2(i) = mem(i)
        }
      }
      
      Control.model.step
      
      mem = Control.model.read
      
      if(Control.debug){
        Control.model.print
        println("step "+steps+":")
        for(i <- 0 to mem.length-1){
          print(mem(i))
        }
        println()
      }
      
      steps += 1
      modelIsEqual = true
      
      breakable {
        for(i <- 0 to size-1){
          if(mem(i)!=mem2(i)){
            modelIsEqual = false
            break
          }
        }
      }
    }
    
    if(lasso>=3){
      Control.fOut("no cycle or lasso of size "+stepLimit+" or smaller detected")
      return 0
    }
    else if(lasso<3 && lasso>0){
      Control.fOut("lasso detected with ring of size "+steps)
      return -steps
    }else{
      Control.fOut("it took "+steps+" steps to complete a cycle")
      return steps
    }
  }
  
  /** 
   * Creates and exports a test file for the Nist statistical test suite and prints the sequence length as well as the number of sequences. Resets 
   * and initializes the model with a random memory. The random values are created in a way to match all possible memory dimensions, sizes and colors.
   * The number of steps per sequence is calculated by 2*dimension*size. In this equation dimension is replaced by subdimension or multicolor 
   * if the model is Ant2DMC or FractalAnt. All calculated sequences are then transformed into a 0-1-Pattern and are exported to testdata.txt.
   * Only working correctly for value ranges in the memory that are a power of 2.
   * 
   * @lines is the number of sequences.
   */
  def createTestFile(lines : Int){
    Control.model.reset
    val r = scala.util.Random
    val size = scala.math.pow(Control.size, Control.dimension).toInt
    var mem = new Array[Int](size)
    var c = 0
    if(Control.multiColor > 0){
      c = Control.multiColor
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(c)
      }
    }else if(Control.subdimension <= 0){
      c =  2*(Control.dimension-1)
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(c)
      }
    }else{
      c = 2*(Control.subdimension-1)
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(c)
      }
    }
    
    var cycles = 16 * size * c
    if(Control.subdimension > 0) cycles = size * c / 16
    
    Control.model.write(mem)
    
    val b = (scala.math.log(c)/scala.math.log(2)).toInt
    val res = new Array[Int](lines*size*b)
    
    for(i <- 0 to lines-1){
      
      for(j <- 0 to cycles){
        Control.model.step
      }
      
      if(Control.debug){
        println("line "+i+":")
        Control.model.print
      }
      
      mem = Control.model.read
      var tmpArray = new Array[Int](mem.length)
      for(k <- 0 to mem.length-1){
        tmpArray(k) = mem(k)
      }
      
      for(j <- 0 to size-1){
        var h = scala.math.pow(2, b-1).toInt
        for(k <- 0 to b-1){
          if(tmpArray(j) >= h){
            res(i*size*b+j*b+k) = 1
            tmpArray(j) -= h
          }else{
            res(i*size*b+j*b+k) = 0
          }
          h /= 2
        }
      }
    }
    
    Control.exportTestFile(res)
    
    Control.fOut("created testfile with "+lines+" sequences and sequence_length = "+(size*b))
  }
  
  /**
   * Resets and initializes the model with a random memory. The random values are created in a way to match all possible memory dimensions, sizes and colors.
   * The model does cycles numbers of steps, is inverted and does again cycles numbers of steps. Then the initialized memory is compared to the
   * actual memory.
   * 
   * @param cycles number of steps
   * @return true if the model is invertible, else false
   */
  def testRandomInvertAnt(cycles: Int) : Boolean = {
    Control.model.reset
    Control.fOut("test random inversion:")
    var steps = 0
    val r = scala.util.Random
    var size = scala.math.pow(Control.size, Control.dimension).toInt
    var mem = new Array[Int](size)
    if(Control.multiColor > 0){
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(Control.multiColor)
      }
    }else if(Control.subdimension <= 0){
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(2*(Control.dimension-1))
      }
    }else{
      for(i <- 0 to size-1){
        mem(i) = r.nextInt(2*(Control.subdimension-1))
      }
    }
    
    Control.model.write(mem)
    
    if(Control.debug){
      for(i <- 0 to mem.length-1){
        print(mem(i)+" ")
        if(i%Control.size==Control.size-1) println
      }
      println
    }
    
    var mem2 = new Array[Int](size)
    for(i <- 0 to size-1){
      mem2(i) = mem(i)
    }
    
    for(i <- 0 to cycles-1){
      Control.model.step
      
      if(Control.debug){
        println("step "+i+":")
        mem = Control.model.read
        for(i <- 0 to mem.length-1){
          print(mem(i)+" ")
          if(i%Control.size==Control.size-1) println
        }
        println
      }
    }
    
    Control.model.invertAnt
    
    if(Control.debug){
      println("inverted!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
    
    for(i <- 0 to cycles-1){
      Control.model.step
      
      if(Control.debug){
        println("step "+i+":")
        mem = Control.model.read
        for(i <- 0 to mem.length-1){
          print(mem(i)+" ")
          if(i%Control.size==Control.size-1) println
        }
        println
      }
    }
    
    mem = Control.model.read
    
    if(Control.debug){
      for(i <- 0 to mem.length-1){
        print(mem(i)+" ")
        if(i%Control.size==Control.size-1) println
      }
      println
    }
    
    var isEqual = true
    for(i <- 0 to mem.length-1){
      if(mem(i) != mem2(i)) isEqual = false
    }
    if(isEqual) {
      Control.fOut("Success! After "+cycles+" steps, invert and again "+cycles+" steps the original memory is recreated")
      return true
    }
    else {
      Control.fOut("Inversion failed")
      return false
    }
  }
  
  /**
   * Resets the model and initialize the model the an all zero memory. The model does cycles numbers of steps,
   * is inverted and does again cycles numbers of steps. Then the initialized memory is compared to the actual memory.
   * This test is for debugging purposes.
   * 
   * @param cycles number of steps
   * @return true if the model is invertible, else false
   */
  def testZeroInvertAnt(cycles: Int) = {
    Control.model.reset
    Control.fOut("test zero inversion:")
    var steps = 0
    val r = scala.util.Random
    var size = scala.math.pow(Control.size, Control.dimension).toInt
    var mem = new Array[Int](size)
    
    for(i <- 0 to size-1){
      mem(i) = 0
    }
    
    Control.model.write(mem)
    
    if(Control.debug){
      println("init:")
      for(i <- 0 to mem.length-1){
        print(mem(i)+" ")
        if(i%Control.size==Control.size-1) println
      }
      println
      Control.model.print
      println
    }
    
    var mem2 = new Array[Int](size)
    for(i <- 0 to size-1){
      mem2(i) = mem(i)
    }
    
    for(i <- 0 to cycles-1){
      Control.model.step
      
      if(Control.debug){
        println("step "+i+":")
        mem = Control.model.read
        for(i <- 0 to mem.length-1){
          print(mem(i)+" ")
          if(i%Control.size==Control.size-1) println
        }
        println
        Control.model.print
        println
      }  
    }
    
    Control.model.invertAnt
    if(Control.debug) println("inverted!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    
    for(i <- 0 to cycles-1){
      Control.model.step
      
      if(Control.debug){
        println("step "+i+":")
        mem = Control.model.read
        for(i <- 0 to mem.length-1){
          print(mem(i)+" ")
          if(i%Control.size==Control.size-1) println
        }
        println
        Control.model.print
        println
      }
      
    }
    
    mem = Control.model.read
    var isEqual = true
    for(i <- 0 to mem.length-1){
      if(mem(i) != mem2(i)) isEqual = false
    }
    if(isEqual) Control.fOut("Success! After "+cycles+" steps, invert and again "+cycles+" steps the original memory is recreated")
    else Control.fOut("Inversion failed")
  }
  
  /**
   * Does the following tests loops times:
   * 	testRandomRing
   * 	If model is invertible, testRandomInvertAnt is done for 10, 100, 1000, 10000 and 100000 steps.
   * The test results are gathered. Averages and number of result types are counted.
   * 
   *  @param loops number of times all tests are done
   */
  def testFull(loops : Int) = {
    Control.fOut("full test:")
    
    val inversions = new Array[Int](5)
    val ringCounts = new Array[Int](3)
    val rings = new ArrayBuffer[Int]()
    val averages = new Array[Double](2)
    
    averages(0) = 0.0
    averages(1) = 0.0
    
    for(i <- 0 to inversions.length-1){
      inversions(i) = 0
    }
    for(i <- 0 to ringCounts.length-1){
      ringCounts(i) = 0
    }
    
    for(i <- 0 to loops-1){
      var ring = testRandomRing
      rings.append(ring)
      if(ring > 0){
        ringCounts(0) += 1
      }else if(ring < 0){
        ringCounts(1) += 1
      }else{
        ringCounts(2) += 1
      }
      
      if(Control.model.isInvertible()){
        var steps = 10
        for(j <- 0 to inversions.length-1){
          var inverse = testRandomInvertAnt(steps)
          steps *= 10
          if(inverse) inversions(j) += 1
        } 
      }
    }
    
    for(j <- 0 to rings.length-1){
      if(rings(j) > 0){
        averages(0) += (1.0*rings(j))/ringCounts(0)
      }else if(rings(j) < 0){
        averages(1) -= (1.0*rings(j))/ringCounts(1)
      }
    }
    
    Control.fOut("test loops: "+loops)
    Control.fOut("detected ring size counts:")
    while(rings.length>0){
      var c = 0
      var i = 0
      var r = rings(0)
      while(i < rings.length){
        if(rings(i) == r){
          rings.remove(i)
          c += 1
        }else{
          i += 1
        }
      }
      Control.fOut(r+"("+c+")")
    }    
    Control.fOut("average ring size: "+averages(0))
    Control.fOut("average lasso size: "+averages(1))
    Control.fOut("number of rings: "+ringCounts(0))
    Control.fOut("number of lassos: "+ringCounts(1))
    Control.fOut("number of out of range: "+ringCounts(2))
    
    if(Control.model.isInvertible()){
      var steps = 10
      for(i <- 0 to inversions.length-1){
        Control.fOut("number of working inversions after "+steps+" steps: "+inversions(i))
        steps *= 10
      }
    }else{
      Control.fOut("model is not invertible")
    }
  }
  
}
