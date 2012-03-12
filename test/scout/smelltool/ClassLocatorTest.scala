package scout.smelltool

import org.specs.runner._
import org.specs.Specification
import java.io.File

class ClassLocatorTest extends Specification 
                      with ScalaTest {
	
   "find class" in {
      val smellFinders = ClassLocator.getSmellFinders
//      smellFinders must haveSize(3)
      smellFinders.map {_.getClass.getName()} must containAll(List("scout.smelltool.RedundantReturnTypeSmellFinder",
        "scout.smelltool.ExplicitReturnSmellFinder", "scout.smelltool.NullUsageSmellFinder"))
    }

    "find class names" in {
      val fileName = (List(System.getProperty("user.dir"), "tmpClasses", "smellFinderFileTest")).mkString(File.separatorChar.toString)

      val smellFinders = ClassLocator.getClassNames(fileName)
//      smellFinders must haveSize(3)
      smellFinders must containAll(List("RedundantReturnTypeSmellFinder", "ExplicitReturnSmellFinder", "NullUsageSmellFinder"))
    }

    "find class names invalid file" in {
      val fileName = (List(System.getProperty("user.dir"), "tmpClasses", ":that;+$#")).mkString(File.separatorChar.toString)

      val smellFinders = ClassLocator.getClassNames(fileName)
//      smellFinders must haveSize(3)
      smellFinders must_== List()
    }
}