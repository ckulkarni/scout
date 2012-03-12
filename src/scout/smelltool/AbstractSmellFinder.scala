package scout.smelltool

import scala.collection.mutable.LinkedHashMap

trait AbstractSmellFinder {
   var message : String = _
   var smellMap =  LinkedHashMap.empty[AbstractSmellInfo, Boolean]
   
   import SmellFinderGlobal.global._
    
   def analyzeNode: PartialFunction[Tree, Unit]

   val defaultAnalyzer : PartialFunction[Tree, Unit] = {
      case _ =>
   }
    
   def analyzeNodeForSmell(tree : Tree) = (analyzeNode orElse defaultAnalyzer)(tree)

   def getViolations(smellMap: LinkedHashMap[AbstractSmellInfo, Boolean]): List[Violation] = {
     smellMap.filter { 
        _._2 
     }.map { funcInfo => 
        new Violation(funcInfo._1.position, message)
     }.toList
   }

  def violations = getViolations(smellMap)

  def treeTraverse(tree: Tree) = {
     AbstractSmellFinder.traverseTreeAndApply(tree) { 
      analyzeNodeForSmell 
     }
    !violations.isEmpty
  }
}

object AbstractSmellFinder {
  import SmellFinderGlobal.global._
  
   def traverseTreeAndApply(tree: Tree)(nodeAnalyzer : Tree => Unit) = {
    new Traverser() {
      override def traverse(tree : Tree) = {
        nodeAnalyzer(tree)
        super.traverse(tree)
     }
    }.traverse(tree)
  }
}