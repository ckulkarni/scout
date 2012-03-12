package scout.smelltool
import scala.tools.nsc
import nsc.{Settings, Global}
import nsc.reporters.ConsoleReporter
import nsc.util.Position
import java.io.File

object SmellFinderGlobal {
   val classesDir = new File(List(System.getProperty("user.dir"), "tmpClasses").mkString(File.separatorChar.toString))

   val global = {
    val settings = new Settings()

    val baseDir = List(System.getProperty("user.dir"), "lib").mkString(File.separatorChar.toString)
    val scalaLib = List(baseDir, "scala-library.jar").mkString(File.separatorChar.toString)
    val scalaCompiler = List(baseDir, "scala-compiler.jar").mkString(File.separatorChar.toString)

	  settings.classpath.value = List(scalaCompiler, scalaLib, settings.classpath.value).mkString(File.pathSeparator.toString)
    classesDir.mkdir()
    settings.outdir.value = classesDir.getPath 
    settings.deprecation.value = true
    settings.unchecked.value = true 

    val reporter = new ConsoleReporter(settings) {
     override def printMessage(pos: Position, msg: String) = {
      throw new ConsoleReporterException(pos.source.file.name, pos.line, msg)
     }
    }
	  new Global(settings, reporter)
  }
}