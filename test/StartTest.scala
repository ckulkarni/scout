import org.specs.runner._
import org.specs.Specification
import java.io.File

class StartTest extends Specification 
                      with ScalaTest {

   val fs = File.separatorChar.toString
   val sampleFile = Array(System.getProperty("user.dir"), "src", "Start.scala").mkString(fs)
   val sampleDir = Array(System.getProperty("user.dir"), "src").mkString(fs)
    
    "test isValid behavior when input type is f" in {
      Start.isValid(Array("-f", sampleFile)) must_== true
      Start.isValid(Array("-f", sampleDir)) must_== false
    }

    "test isValid behavior when input type is r" in {
      Start.isValid(Array("-r", sampleFile)) must_== false
      Start.isValid(Array("-r", sampleDir)) must_== true
    }

    "test isValid behavior when input type is s" in {
      Start.isValid(Array("-s", sampleFile)) must_== false
      Start.isValid(Array("-s", sampleDir)) must_== false
      Start.isValid(Array("-s", "object Test { val x = 2}")) must_== true
    }

    "test isValid wrong number of args" in {
      Start.isValid(Array("-s")) must_== false
      Start.isValid(Array("-f", "this is good")) must_== false
      Start.isValid(Array("-s", "object Test { val x = 2}", "dummy arg")) must_== true
    }

    "test isValid invalid first arg" in {
      Start.isValid(Array("-k", sampleFile)) must_== false
      Start.isValid(Array("-p", "object Test { val x = 2}")) must_== false
    }

    "test processForSmells- No Smells" in {
      val noSmellFile = Array(System.getProperty("user.dir"), "NoSmell.scala").mkString(fs)
      Start.hasSmells(Array("-f", noSmellFile)) must_== false
    }

    "test processForSmells- Smells" in {
      val exampleFile = Array(System.getProperty("user.dir"), "Example.scala").mkString(fs)

      Start.hasSmells(Array("-f", exampleFile)) must_== true
    }

    "test processForSmells- invalid Args" in {
      Start.hasSmells(Array("-f")) must_== false
    }
}