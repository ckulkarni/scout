package scout.smelltool

import org.specs.runner._
import org.specs.Specification
/**
* Assumes there are no compilation errors in the code.
**/
class RedundantReturnTypeSmellFinderTest extends Specification 
                      with ScalaTest {

   var redundantReturnTypeSmellFinder : RedundantReturnTypeSmellFinder = _
   def redundantReturnTypeSmellFinderMethod = { redundantReturnTypeSmellFinder.treeTraverse _ }
   def getTree(src: String) = SmellFinder.getParseTree(SmellFinder.getBatchSourceFiles(src, 's').head)

   "RedundantReturnTypeSmellFinder" should {
     doBefore {
       redundantReturnTypeSmellFinder = new RedundantReturnTypeSmellFinder
     }

    "detect no smell" in {
      val src = """object Test { 
          |def someMethod(n:Int) = 4
        |}"""

     redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell- return is unit" in {
      val src = """object Test { 
          |def someMethod(n:Int) = println("this is nice")
        |}"""

     redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell- return is inferred to be unit" in {
      val src = """object Test { 
          |def someMethod(n:Int) { println("this is nice") }
        |}"""

     redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell in class with var, compiler inserts setter methods in initialization" in {
      val src = """ object Test {
        |var x:String = "hello"
      |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell in class with var for assignment" in {
      val src = """ class Test {
        |var x:String = _
      |}
      |object Test1 {
      |val test = new Test()
      |test.x = "hello"
      |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell- return is scala.unit -explicitly specified" in {
      val src = """object Test { 
          |def someMethod(n:Int) : scala.Unit = { println("this is nice") }
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell return is unit - explicitly specified" in {
      val src = """object Test { 
          |def someMethod(n:Int):Unit = println("this is nice")
        |}"""

     redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell in simple class definition" in {
      val src = "class Test(x:Int)"
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }
     
    "detect smell - redundant return type declaration" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
    }

    "detect smell - capture violation- redundant return type declaration" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = n * 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect no smell when return type is List" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int) = List(1,2)
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell when return type is List" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int):List[Int] = List(1,2)
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect no smell when return type is Map" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int) = {
        |println("This")
        |Map(1 -> "hello" ,2 -> "World")
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell when return type is Map" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int):Map[Int, String] = {
        |println("This")
        |Map(1 -> "hello" ,2 -> "World")
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect no smell when return type is Tuple" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int) = {
        |println("This")
        |(3,4)
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell when return type is tuple" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int):Tuple2[Int, String] = {
        |println("This")
        |(1, "hello")
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect smell when return type is tuple in parenthesis" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int): (Int, String) = {
        |println("This")
        |(1, "hello")
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.toString must_== "[source: , line-2] someMethod(Params: 1) has redundant return type specified"
    }

     "detect no smell when return type is Function" in {
      val src = """class Test(x:Int) {
        |def someMethod(x:Int) = {
        | { c:Int => 
        |   println("this is nice")
        |   3
        |  }
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell when return type is Function" in {
      val src = """class Test(x : Int) {
        |def someMethod: (Int, Int) => String = {
        | (c : Int, d : Int) => {
        |   println("this is nice")
        |   "hello"
        |  }
        |}
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 0) has redundant return type specified"
    }

    "detect smell in the right function -- smelly function first" in {
      val src = """object Test {
        |def otherMethod(x : Int) : List[Int] = List(1,2)
        |def someMethod(s : Int) = 3
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "otherMethod(Params: 1) has redundant return type specified"
    } 

    "detect smell in the right function -- smelly function last" in {
      val src = """object Test {
        |def otherMethod(s : Int) = 3
        |def someMethod(x : Int) : List[Int] = List(1,2)
        |}"""
      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect no smell when function is recursive" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = if(n>0) someMethod(n-1) else 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell when any function is non recursive - recursive first" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = if(n>0) someMethod(n-1) else 4
          |def someMethod(x:Int, y:Int):Int = 3
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 2) has redundant return type specified"
    }

    "detect smell when any function is non recursive - non recursive first" in {
      val src = """object Test { 
          |def someMethod(x:Int, y:Int):Int = 3
          |def someMethod(n:Int):Int = if(n>0) someMethod(n-1) else 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 2) has redundant return type specified"
    }

    "detect smell when function is non recursive" in {
        val src = """object Test { 
          |def someMethod(n:Int):Int = someMethod(1,3); 
          |def someMethod(i:Int, j:Int) = 3; 
          |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect smell when function is non recursive defined in different class" in {
        val src = """object Test { 
          |def someMethod(n:Int):Int = Test2.someMethod(1);
          |}
          |object Test2 {
          |def someMethod(n:Int) = 4
          |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect no smell when there is explicit return" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = return 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect smell when any functions has redundant return -- return first" in {
      val src = """object Test { 
          |def returnMethod(i:Int):Int = return 5
          |def someMethod(n:Int):Int = 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect smell when any functions has redundant return -- return last" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = 4
          |def returnMethod(i:Int):Int = return 5
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(1)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
    }

    "detect no smell when any functions has explicit return -- return first" in {
      val src = """object Test { 
          |def returnMethod(i:Int):Int = return 5
          |def someMethod(n:Int) = 4
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell when any functions has explicit return -- return last" in {
      val src = """object Test { 
          |def someMethod(n:Int) = 4
          |def returnMethod(i:Int):Int = return 5
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }

    "detect no smell when functions are indirectly recursive" in {
      val src = """object Test { 
        |def aMethod(c:Int):Int = { if(c ==0) 1 else whatMethod(c) }
        |def whatMethod(x:Int) = { println("whatever"); aMethod(x-1) }
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    } 

    "detect 2 redundant return smells" in {
      val src = """object Test { 
          |def someMethod(n:Int):Int = 4
          |def returnMethod(i:Int):Int = 5
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== true
      redundantReturnTypeSmellFinder.violations must haveSize(2)
      redundantReturnTypeSmellFinder.violations.head.getMessage must_== "someMethod(Params: 1) has redundant return type specified"
      redundantReturnTypeSmellFinder.violations(1).getMessage must_== "returnMethod(Params: 1) has redundant return type specified"
    }

     "detect no smell for abstract method" in {
      val src = """trait Test { 
          |def someMethod : Int
        |}"""

      redundantReturnTypeSmellFinderMethod(getTree(src)) must_== false
    }
  }
}