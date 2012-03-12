package scout.smelltool

class NullUsageSmellFinder extends AbstractSmellFinder {
  
  import SmellFinderGlobal.global._
 
  message = "avoid usage of null, use Option or collection methods like getOrElse, getOrElseUpdate instead" 

  override def analyzeNode = {
    case tree @ Literal(Constant(value)) if Option(value) == None => 
      smellMap += (AbstractSmellInfo(tree.pos)) -> true 
  }
}