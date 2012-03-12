package scout.smelltool

import org.specs.runner._
import org.specs.Specification

class SmellFinderTest extends Specification 
                      with ScalaTest {
   
  val inputType = 's'
  "detect smell due to compiler error - missing definition" in {
    val src = "dummy = 4"
    SmellFinder.getBatchSourceFiles(src, inputType) must throwA[ConsoleReporterException]
  }

  "detect smell due to compiler error - missing return type when return keyword used" in {
    val src = "object Test { def dummyMethod = return 4 }"
    SmellFinder.getBatchSourceFiles(src, inputType) must throwA[ConsoleReporterException]
  }

  "verify compiler error message - missing definition" in {
    val src = "dummy = 4"
    try {
      SmellFinder.getBatchSourceFiles(src, inputType)
    }
    catch {
      case ex:ConsoleReporterException => ex.getMessage must_== "[source: , line-1] error: expected class or object definition"
      case _ => fail("unexpected Exception")
    }
    //ck: SmellFinder.getParseTree(src) throwA(ConsoleReporterException("code compilation fails: [0]: error: expected class or object definition"))
    //does not work.. is there someError in the way custom Exception is defined??
  }

  "verify compiler is also a violation - missing definition" in {
    val src = "dummy = 4"
    val violations = SmellFinder.getSmells(src)
    violations must haveSize(1)
    violations.head.toString must_=="Compiler Error: [source: , line-1] error: expected class or object definition"
  }

  "verify compiler error is also a violation - explicit return with no returntype specified" in {
    val src = "object Test { def dummyMethod =  return 4 }"
    val violations = SmellFinder.getSmells(src)
    violations must haveSize(1)
    violations.head.toString must_=="Compiler Error: [source: , line-1] error: method dummyMethod has return statement; needs result type"
  }

  "detect no smell - check for multiple smells" in {
    val src = """object Test { 
          |def someMethod(x:Int, y:Int) = 3
        |}"""
    SmellFinder.getSmells(src) must beEmpty
  }

  "detect smell - check for multiple smells - one smell" in {
    val src = """object Test { 
          |def someMethod(x:Int, y:Int):Int = 3
        |}"""
    val violations = SmellFinder.getSmells(src)
    violations must haveSize(1)
    violations.head.getMessage must_== "someMethod(Params: 2) has redundant return type specified"
  }

  "detect smell - check for multiple smells - one smell - explicit returntype smell" in {
    val src = """object Test { 
          |def someMethod:Int = return 3
        |}"""
    val violations = SmellFinder.getSmells(src)
    violations must haveSize(1)
    violations.head.getMessage must_== "avoid use of keyword return"
  }

  "detect smell - check for multiple smells - two different smell" in {
    val src = """object Test { 
          |def someMethod(x:Int, y:Int):Int = 3
          |def someMethod(x:Any) = if( x == null) 4 else 5
        |}"""
    val violations = SmellFinder.getSmells(src)
    violations must haveSize(2)
    violations map {_.getMessage}  must containAll(List("someMethod(Params: 2) has redundant return type specified", 
      "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"))
  }

  "detect same smell in 2 different methods" in {
    val src = """object Test { 
        |def someMethod(x:Int, y:Int):Int = 3
        |def someMethod(n:Int):Int = 4
      |}"""
    val smells = SmellFinder.getSmells(src) 
    smells must haveSize(2)
    smells map { _.getMessage } must containAll(List("someMethod(Params: 2) has redundant return type specified", 
      "someMethod(Params: 1) has redundant return type specified"))
 }

  "detect smells- 2 different smells in the same method" in {
    val src = """object Test { 
        |def someMethod(x:Any, y:Int):Int = if(x != null) 3 else 4
      |}"""
    val smells = SmellFinder.getSmells(src) 
    smells must haveSize(2)
    smells map { _.getMessage } must containAll(List("someMethod(Params: 2) has redundant return type specified", 
      "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"))
  }

  "test parsing the directory" in {
    val dir = List(System.getProperty("user.dir"), "src").mkString(java.io.File.separatorChar.toString)
    val files = SmellFinder.getBatchSourceFilesFromDir(new java.io.File(dir))

    files.length must beGreaterThanOrEqualTo(12)
   }

  "test getBatchSourceFile for -r input" in {
    val dir = List(System.getProperty("user.dir"), "src").mkString(java.io.File.separatorChar.toString)
    val files = SmellFinder.getBatchSourceFiles(dir, 'r')

    files.length must beGreaterThanOrEqualTo(12)
  }

  "test getBatchSourceFile for -s input" in {
    val src = """object Test { 
        |def someMethod(x:Any, y:Int):Int = if(x != null) 3 else 4
      |}"""
    val files = SmellFinder.getBatchSourceFiles(src, 's')
    files must haveSize(1)
  }

  "test getBatchSourceFile for -f input" in {
    val file = List(System.getProperty("user.dir"), "src", "scout", "smelltool", "SmellFinder.scala").mkString(java.io.File.separatorChar.toString)
    val files = SmellFinder.getBatchSourceFiles(file, 'f')

    files must haveSize(1)
  }

  "test getBatchSourceFile for invalid input type" in {
    val file = List(System.getProperty("user.dir"), "src", "SmellFinder.scala").mkString(java.io.File.separatorChar.toString)
    val files = SmellFinder.getBatchSourceFiles(file, 'k')

    files must haveSize(0)
  }

  "test generation of parseTree" in {
    val src = """object Test { 
        |def someMethod(x:Any, y:Int):Int = if(x != null) 3 else 4
      |}"""
    val files = SmellFinder.getBatchSourceFiles(src, 's')
    SmellFinder.getParseTree(files.head) must notBeNull
  }
  
}
