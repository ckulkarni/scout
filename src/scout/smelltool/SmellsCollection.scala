package scout.smelltool

class SmellsCollection(val smellFinders : List[AbstractSmellFinder]) {
  
  import SmellFinderGlobal.global._
    
  def getSmells(tree: Tree) = {
    AbstractSmellFinder.traverseTreeAndApply(tree) { node => 
      smellFinders.foreach { _.analyzeNodeForSmell(node) }
    }

    smellFinders.map { _.violations }.flatten
  } 
}