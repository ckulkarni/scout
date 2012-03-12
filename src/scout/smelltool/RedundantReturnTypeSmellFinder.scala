package scout.smelltool

import scala.collection.mutable.LinkedHashMap

class RedundantReturnTypeSmellFinder extends AbstractSmellFinder {
  import SmellFinderGlobal.global._
  
  message = "has redundant return type specified" 

  override def analyzeNode = {
    case tree @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if !(tpt.isEmpty || isUnit(tpt) || rhs.isEmpty) => 
      val currentFunc = AbstractSmellInfo(tree.pos, name.toString, if (vparamss.length > 0) vparamss(0).length else 0)
      smellMap += (currentFunc) -> true
    case Apply(fun, args) => 
      smellMap = checkForRedundancy(smellMap, fun.toString, args.length)
    case Return(_) => smellMap(smellMap.last._1) = false
  }

  private def isUnit(typeTree: Tree) ={
    typeTree match {
      case Ident(name) if name.toString == "Unit" => true
      case Select(tree, sym) if sym.toString == "Unit" => true
      case _ => false
    }
  }

  private def checkForRedundancy(smellMap: LinkedHashMap[AbstractSmellInfo, Boolean], fun: String, args: Int) = {
    smellMap.lastOption match {
        case Some(smellInfo) => 
          if(fun == smellInfo._1.functionName && args == smellInfo._1.numberOfParams) 
            smellMap(smellInfo._1) = false
        case None =>
      }
      smellMap
  }

  override def getViolations(smellMap: LinkedHashMap[AbstractSmellInfo, Boolean])  = {
    smellMap.filter { 
      _._2 
    }.map { funcInfo => 
        new Violation(funcInfo._1.position, message, funcInfo._1.functionName + "(Params: " + funcInfo._1.numberOfParams + ")")
     }.toList
  }
}