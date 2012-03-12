package scout.smelltool

import org.specs.runner._
import org.specs.Specification

class ExplicitReturnSmellFinderTest extends Specification 
                      with ScalaTest {
     
   var explicitReturnSmellFinder: ExplicitReturnSmellFinder =_
   def explicitReturnSmellFinderMethod = { explicitReturnSmellFinder.treeTraverse _ }
   def getTree(src: String) = SmellFinder.getParseTree(SmellFinder.getBatchSourceFiles(src, 's').head)

   "ExplicitReturnSmellFinder" should {
     doBefore {
       explicitReturnSmellFinder = new ExplicitReturnSmellFinder
     }

    "detect no smell" in {
      val src = """object Test { 
          |def someMethod(n:Int) = 4
        |}"""

     explicitReturnSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell" in {
      val src = """object Test { 
          |def someMethod:Int = return 4
        |}"""

     explicitReturnSmellFinderMethod(getTree(src)) must_== true
     explicitReturnSmellFinder.violations must haveSize(1)
     explicitReturnSmellFinder.violations.head.getMessage must_== "avoid use of keyword return"
    }
  }
}