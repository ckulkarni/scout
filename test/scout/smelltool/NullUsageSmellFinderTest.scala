package scout.smelltool

import org.specs.runner._
import org.specs.Specification

class NullUsageSmellFinderTest extends Specification 
                      with ScalaTest {

   var nullUsageSmellFinder: NullUsageSmellFinder = _
   def nullUsageSmellFinderMethod = { nullUsageSmellFinder.treeTraverse _ }
   def getTree(src: String) = SmellFinder.getParseTree(SmellFinder.getBatchSourceFiles(src, 's').head)

   "NullUsageSmellFinder" should {
     doBefore {
       nullUsageSmellFinder = new NullUsageSmellFinder
     }

     "detect no smell" in {
        val src = """object Test { 
            |def someMethod(n: Int) = 4
          |}"""
     
       nullUsageSmellFinderMethod(getTree(src)) must_== false
      }

      "detect smell" in {
        val src = """object Test { 
            |def someMethod(n: Int) = null
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(1)
       nullUsageSmellFinder.violations.head.getMessage must_== "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"
      }

      "detect smell - null used for comparison in if statement" in {
        val src = """object Test { 
            |def someMethod(n: AnyRef) = if(n == null) 3
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(1)
       nullUsageSmellFinder.violations.head.getMessage must_== "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"
      }

      "detect smell if null is not used with ne" in {
        val src = """object Test { 
            |def someMethod(n: AnyRef) = if(n == null) n else n
            |def otherMethod(n: AnyRef) = assert(someMethod(n) ne null)
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
       nullUsageSmellFinder.violations.head.getMessage must_== "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"
      }

      "detect smell if null is not used with ne - ne first" in {
        val src = """object Test { 
            |def otherMethod(n: AnyRef) = assert(someMethod(n) ne null)
            |def someMethod(n: AnyRef) = if(n == null) n else n
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
       nullUsageSmellFinder.violations.head.getMessage must_== "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"
      }

      "detect no smell if null used with eq" in {
        val src = """object Test { 
            |def someMethod(n: AnyRef) = if(n eq null) 3
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
      }

      "detect smell if null used in assert with not equal" in {
        val src = """object Test { 
            |def someMethod(n: AnyRef) = if(n eq null) n else n
            |def otherMethod(n: AnyRef) = assert(someMethod(n) != null)
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
       nullUsageSmellFinder.violations.head.getMessage must_== "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead"
      }

      "detect smell if null used in method calls with ne" in {
        val src = """object Test { 
            |def someMethod(n: Boolean) = "string"
            |def otherMethod(n: AnyRef) = if(someMethod(n eq null) != null) 3 else 0
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
      }

      "detect smell if null in method call with ==" in {
        val src = """object Test { 
            |def someMethod(n: Boolean) = "this"
            |def otherMethod(n: AnyRef) = if(someMethod(n == null) eq null) 3 else 0
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
      }

      "detect no smell if null is used multiple times with only eq" in {
        val src = """object Test { 
            |def someMethod(n: Boolean) = "this"
            |def otherMethod(n: AnyRef) = if(someMethod(n eq null) eq null) 3 else 0
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
      }

     "detect smell if null used for initialization" in {
        val src = """object Test { 
            |val x:String = null
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(1)
      }

      "detect 2 smells" in {
        val src = """object Test { 
            |def someMethod(n: Int) = { var x = null
            | var y = null
            |}
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
      }

      "detect 2 smells in different methods" in {
        val src = """object Test { 
            |def someMethod(n: Int) = { var x = null }
            |def otherMethod = { var y = null }
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
      }

      "detect 2 smells if null used twice in same statement" in {
        val src = """object Test { 
            |def someMethod(n: Boolean) = "someString"
            |def otherMethod(n: AnyRef) = if(someMethod(n == null) != null) 3 else 0
          |}"""

       nullUsageSmellFinderMethod(getTree(src)) must_== true
       nullUsageSmellFinder.violations must haveSize(2)
       nullUsageSmellFinder.violations(0).position must_!= nullUsageSmellFinder.violations(1).position
      }
   }
}