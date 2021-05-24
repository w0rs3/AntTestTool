
package main.scala

import scala.collection.mutable.ListBuffer
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import models.Ant2DConv
import control.Test

/**
 * This object has multiple purposes. Here are variables stored that are used by all classes and objects.
 * In addition export functions are implemented and the main function can be found here. That means that the
 * menu is also included. Last but not least here the logging system is defined. 
 * 
 * Use fOut for logging and extra print messages if debug is true if you extend the project.
 * 
 * Menu:
 * 	Logging types:
 * 		deactivated logging: Prints only
 * 		activated logging: Prints and adds to log
 * 		debug: prints, adds to log, and prints extra information
 * 	dimension: Dimension of the memory of the model.
 * 	size: Size of the memory of the model
 * 	models:
 * 		Ant2D: A casual ant on a 2D torus
 * 		Ant2DMC: An Ant2D with multiple colors (values must be entered as 0(left) or 1(right) per color)
 * 		Ant2DConv: An Ant2D with convolution (values of history must be 0 or 1)
 * 		Ant3D: An ant on a 3D torus with 4 colors
 * 		Ant: A Dynamic scaling ant on any multidimensional torus
 * 		AntConv: An Ant with convolution (values of history must be in range 0 to 2*(dimension-1)-1. Example if dimension = 3 then 0,1,2 and 3 is possible.
 * 		FractalAnt: A fractal hierarchical Ant ((depth+1)*subdimension == dimension must be true)
 * 	Test menu:
 * 		export log
 *    test zero (enter cycles)
 *    test random ring cycle count
 *    test random inversion (enter cycles)
 *    test zero inversion (enter cycles)
 *    test full (enter loops)
 *    create test file (enter sequences)
 *    print model
 *    exit
 */
object Control {
  
  var fOut = (s : String) => println(s)
  var log  = ListBuffer[String]()
  
  var dimension : Int = 0
  var size : Int = 0
  var model: Model = null
  
  var subdimension = -1
  var depth = -1
  var multiColor = -1
  var debug = false
  
  /*
   * Exports the log to log.csv
   */
  def export = {
    val file = new File("log.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    for(i <- 0 to log.size-1){
      bw.write(log(i)+";\n")
    }
    bw.close
  }
  
  /**
   * Exports a array to testfile.txt. The syntax of the test file is created in a way that the Nist statistical test suite can use it.
   * The array must consist of zeros and ones only.
   * 
   * @param arr must consist of zeros and ones only.
   */
  def exportTestFile(arr: Array[Int]) = {
    val m = 25
    val file = new File("testdata.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("   ")
    for(i <- 0 to arr.length-1){
      bw.write(arr(i)+"")
      if(i%m==m-1 && i<arr.length-1) bw.write("\n   ")
    }
    bw.write("\n")
    bw.close
  }
  
  /**
   * Main function and entry point. At the moment args is not in use.
   */
  def main(args: Array[String]): Unit = {
    
    // Welcome
    println("Welcome to the langtons ant test tool.")
    
    // Choose logging
    var choice = 0
    while(choice<1||choice>3){
      println("choose:")
      println("1 - deactivate logging")
      println("2 - activate logging")
      println("3 - debug")
      choice = Console.readInt
    }
    choice match {
      // print
      case 1 => {
        fOut = (s : String) => println(s)
      }
      // print and log
      case 2 => {
        fOut = (s : String) => {
          println(s)
          log += s
        }
      }
      // print, log and debug
      case 3 => {
        debug = true
        fOut = (s : String) => {
          println(s)
          log += s
        }
      }
      case _ =>
    }
    
    // Choose dimension
    while(dimension<2||dimension>32){
      println("enter the dimension in range 2 to 32:")
      dimension = Console.readInt
    }
    fOut("dimension = "+dimension)
    
    // Choose size
    while(size<2||size>256){
      println("enter the size in range 2 to 256:")
      size = Console.readInt
    }
    fOut("size = "+size)
    
    // Choose Model
    choice = 0;
    while(choice<1||choice>7){
      println("choose:")
      println("1 - 2D Ant")
      println("2 - 2D Multi Color Ant")
      println("3 - 2D Convolutional Ant")
      println("4 - 3D Ant")
      println("5 - Multi Dimensional Ant")
      println("6 - Multi Dimensional Convolutional Ant")
      println("7 - Fractal Multi Dimensional Ant")
      choice = Console.readInt
    }
    choice match {
      case 1 => {
        model = new Ant2D
        fOut("model = Ant2D")
      }
      case 2 => {
        model = new Ant2DMC
        fOut("model = Ant2DMC")
        println("Enter pattern size")
        multiColor = Console.readInt
        var pattern = new Array[Int](multiColor)
        var str = ""
        println("Enter Pattern")
        choice = -1
        for(i <- 0 to multiColor-1){
          pattern(i) = Console.readInt
          str += pattern(i)
        }
        fOut("pattern = "+str)
        model.setParameter(pattern)
      }
      case 3 => {
        model = new Ant2DConv
        fOut("model = Ant2DConv")
        println("Enter history size")
        val s = Console.readInt
        var pattern = new Array[Int](s)
        var str = ""
        println("Enter Pattern")
        choice = -1
        for(i <- 0 to s-1){
          pattern(i) = Console.readInt
          str += pattern(i)+" "
        }
        fOut("pattern = "+str)
        model.setParameter(pattern)
      }
      case 4 => {
        model = new Ant3D
        fOut("model = Ant3D")
      }
      case 5 => {
        model = new Ant
        fOut("model = Ant")
      }
      case 6 => {
        model = new AntConv
        fOut("model = AntConv")
        println("Enter history size")
        val s = Console.readInt
        var pattern = new Array[Int](s)
        var str = ""
        println("Enter Pattern")
        choice = -1
        for(i <- 0 to s-1){
          pattern(i) = Console.readInt
          str += pattern(i)+" "
        }
        fOut("pattern = "+str)
        model.setParameter(pattern)
      }
      case 7 => {
        model = new FractalAnt
        fOut("model = FractalAnt")
        while(depth<0||depth>8){
          println("enter the depth in range 0 to 8 (must fit memory):")
          depth = Console.readInt
        }
        fOut("depth = "+depth)
        while(subdimension<2||subdimension>8){
          println("enter the subdimension size in range 2 to 8 (must fit memory):")
          subdimension = Console.readInt
          
        }
        fOut("subdimension = "+subdimension)
        model.setParameter(Array(depth, subdimension))
      }
      case _ =>
    }
    
    // Test menu
    var running = true;
    while(running){
      choice = 0
      while(choice<1||choice>9){
        println("choose:")
        println("1 - export log")
        println("2 - test zero (enter cycles)")
        println("3 - test random ring cycle count")
        println("4 - test random inversion (enter cycles)")
        println("5 - test zero inversion (enter cycles)")
        println("6 - test full (enter loops)")
        println("7 - create test file (enter sequences)")
        println("8 - print model")
        println("9 - exit")
        choice = Console.readInt
      }
      choice match{
        case 1 => export
        case 2 => {
          choice = 0
          choice = Console.readInt
          Test.testZero(choice)
        }
        case 3 => Test.testRandomRing
        case 4 => {
          choice = 0
          choice = Console.readInt
          Test.testRandomInvertAnt(choice)
        }
        case 5 => {
          choice = 0
          choice = Console.readInt
          Test.testZeroInvertAnt(choice)
        }
        case 6 => {
          choice = 0
          choice = Console.readInt
          Test.testFull(choice)
        }
        case 7 => {
          choice = 0
          choice = Console.readInt
          Test.createTestFile(choice)
        }
        case 8 => model.print
        case 9 => running = false
        case _ =>
      }
    }
  }
}