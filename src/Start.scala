import scout.smelltool.SmellFinder
import java.io.File

object Start {
	def main(args:Array[String]): Unit = {
    if(hasSmells(args))
      System.exit(-1)
	}

  def hasSmells(args: Array[String]) = {
   var returnVal = false
   if(isValid(args)) {
      def violations = SmellFinder.getSmells(args(1).stripMargin, args.head(1))
      if(violations.length > 0) {
        println("the violations are: ")
        violations.foreach { println }
        returnVal = true
      }
    }
    returnVal
  }

  def isValid(args: Array[String]) = {
    if(args.length < 2) {
      println("**** Usage *****")
      println("-f <filename.scala>")
      println("-r <dir to recursively traverse for .scala file>")
      println("-s <scala source>")
      false
    }
    else {
      val inputFile = new File(args(1).stripMargin)
      val inputType = args.head(1)
      inputType match {
        case 'f' if(! inputFile.isFile) => println("2nd argument is not a file")
          false
        case 'r' if(! inputFile.isDirectory) => println("2nd argument is not a directory")
          false
        case 's' if(inputFile.isDirectory || inputFile.isFile) => println("2nd argument is not scala source")
          false
        case arg1 if(List('f', 'r', 's') contains arg1) => true
        case _ => false
      }
    }
  }
}