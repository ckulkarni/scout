package scout.smelltool

import scala.tools.nsc.util.ScalaClassLoader
import java.net.URL
import java.io.File
import java.io.PrintWriter

object ClassLocator {  
  val fs = File.separatorChar.toString
  val smellFinderPackage = List("scout", "smelltool")
  val smellFinderFile = "SmellFinders.txt"
  val baseDir = System.getProperty("user.dir")

  val defaultSmellFinders = List("ExplicitReturnSmellFinder", "NullUsageSmellFinder", "RedundantReturnTypeSmellFinder")

  def smellFindersAsInstanceOfAbstractSmellFinder = {
    getSmellFinders.map{ _.asInstanceOf[scout.smelltool.AbstractSmellFinder] }.toList
  }
  
  def getSmellFinders = {
    val url = new URL("file:/" + baseDir + fs)
    val classLoader = new ScalaClassLoader.URLClassLoader(List(url), classOf[scout.smelltool.AbstractSmellFinder].getClassLoader())
    val smellFinderFileName = (List(baseDir, smellFinderFile)).mkString(fs)
    getClassNames(smellFinderFileName).map { className => 
      classLoader.loadClass(smellFinderPackage.mkString(".") + "." + className).newInstance().asInstanceOf[AnyRef] 
    }
  }

  def getClassNames(file: String) = {
    val smellFindersFile = new File(file)
    if(!smellFindersFile.exists() && !createFileWithDefaultSmells(smellFindersFile))
      List()
    else
      scala.io.Source.fromFile(file, "utf-8").getLines.toList
  }

  private def createFileWithDefaultSmells(f: File) = {
    try {
      f.createNewFile()
      val printWriter = new PrintWriter(f)
      try {
         defaultSmellFinders.foreach(printWriter.println)
         true
      } finally { 
          printWriter.close() 
      }
    } catch {
      case _ => 
        println("failed to create SmellFinders.txt, please create one manually")
        false
   } 
    
  }
}