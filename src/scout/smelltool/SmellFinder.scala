package scout.smelltool
import scala.tools.nsc
import nsc.util.BatchSourceFile
import nsc.io.PlainFile
import java.io.File
import scala.collection.JavaConversions._

object SmellFinder {  
  import SmellFinderGlobal._
  
  def getSmells(src: String, inputType: Char = 's') = {
    try {
      val srcFiles = getBatchSourceFiles(src, inputType)
      (List[Violation]() /: srcFiles ) {(result: List[Violation], srcFile: BatchSourceFile) =>
        result ::: new SmellsCollection(ClassLocator.smellFindersAsInstanceOfAbstractSmellFinder).getSmells(getParseTree(srcFile))
      }
    } catch {
      case ex:ConsoleReporterException => List(new Violation(ex.getMessage, "Compiler Error:"))
    }
  }

  def getParseTree(srcFile: BatchSourceFile) = {
    def parseTree = new global.syntaxAnalyzer.UnitParser(new global.CompilationUnit(srcFile)).parse
    parseTree.asInstanceOf[global.Tree]
  }

  def getBatchSourceFiles(src: String, inputType: Char) = {
    val srcFiles = inputType match {
      case 's' => List(new BatchSourceFile("", src.stripMargin))
      case 'f' => List(new BatchSourceFile(new PlainFile(new File(src))))
      case 'r' => getBatchSourceFilesFromDir(new File(src))
      case _ => println("invalid case")
        List[BatchSourceFile]()
    }
    global.reporter.reset
    (new global.Run).compileSources(srcFiles)
    srcFiles
  }

  def getBatchSourceFilesFromDir(dir: File): List[BatchSourceFile] = {
      if (dir.isHidden) {
        List[BatchSourceFile]()
      }
      
      var batchList = List[BatchSourceFile]()
      dir.listFiles.foreach { file => batchList = batchList ::: getBatchSource(file) }
      batchList
	}
	
	private def getBatchSource(file : File) = {
	  if (file.isDirectory)
	    getBatchSourceFilesFromDir(file)
	  else {
	    if (file.getName.endsWith(".scala"))
	      List(new BatchSourceFile(new PlainFile(file)))
	    else
	      List[BatchSourceFile]()
	  }
	}
}