# AntTestTool
This is a test tool for Langton's ant extensions on tori. The extensions included so far build on multicolor sets, discrete convolution, variable multidimensional spaces and the combination with a fractal. The 3 last mentioned extensions are presented here for the first time. The test tool can be used in combination with the NIST-Statistical-Test-Suite. The test tool is used to check the suitability of models as PRG or block ciphers.

## Prerequisites
### Mandatory
Scala library container version 2.12.3  
Java 1.8

### Optional
Scala IDE: http://scala-ide.org/  
SBT: https://www.scala-sbt.org/download.html  
NIST-Statistical-Test-Suite: https://github.com/terrillmoore/NIST-Statistical-Test-Suite

## Overview
![Class-Diagram](https://user-images.githubusercontent.com/61475724/119365517-48686800-bcb0-11eb-9df7-432a0bc04726.png)  
__Figure 1__: Simplified UML class diagram

## Run the project
A simple way to launch the project is to use __sbt run__. The displayed warning can be ignored.
![sbt](https://user-images.githubusercontent.com/61475724/119367180-fb859100-bcb1-11eb-89f6-5aa7e7a11ca4.PNG)  
__Figure 2__: Launch the project using SBT

## Application notes
### Note on Ant2DConv, AntConv and Ant2DMC
If one of these models is selected, first the number of the now following parameters must be specified and then each parameter individually. Both the number and the individual parameters must be confirmed with Enter. Ant2DConv and Ant2DMC expect only 0 or 1 as parameter. For AntConv the parameters must be integer values, for which the following applies:  
0 <= param < 2*(dimension-1)
### Note on FractalAnt
The configuration must meet the following condition:  
dimension = subdimension*(depth+1)  
FractalAnt is not invertible in all valid configurations, so currently a bug is still assumed. However, it can be assumed that the functionality for invertible configurations, work flawlessly.

### Note on vocabulary
Here ring stands for an arrangement of states, objects, properties, or variables according to a circle in graph theory. Here, each element of a ring has an in-ring predecessor and successor. It is important that from each element, its predecessor and successor can be reached. Elements of a ring can have additionally elements outside of the ring as predecessors. A ring has a defined forward and backward direction.  
A lasso consists of a ring and a finite number of additional elements which belong to no ring. The additional elements are related to the ring in such a way that they lead to the ring in forward direction. If a lasso is detected that means indirectly that there is a bridge between 2 rings detected.
![ring_lasso_bridge](https://user-images.githubusercontent.com/61475724/119404442-1b33ae00-bce0-11eb-99bd-e0377c761c61.png)  
__Figure 3__: Ring, lasso and 2 rings connected by a bridge
