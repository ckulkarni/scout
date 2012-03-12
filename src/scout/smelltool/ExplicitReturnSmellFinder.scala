package scout.smelltool

class ExplicitReturnSmellFinder extends AbstractSmellFinder{
  
  import SmellFinderGlobal.global._
 
  message = "avoid use of keyword return" 

  override def analyzeNode = {
    case Return(expr) =>
      smellMap += (AbstractSmellInfo(expr.pos)) -> true
  }
}
