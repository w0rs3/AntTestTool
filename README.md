# AntTestTool
This is a test tool for Langton's ant extensions on tori. The extensions included so far build on multicolor sets, discrete convolution, variable multidimensional spaces and the combination with a fractal. The test tool can be used in combination with the NIST-Statistical-Test-Suite. The test tool is used to check the suitability of models as PRG or block ciphers.

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

## Related papers
Report.pdf can be used as a manual.
