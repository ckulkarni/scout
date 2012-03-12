package scout.smelltool
import scala.tools.nsc.util.Position

case class AbstractSmellInfo(val position:Position, val functionName: String = "", val numberOfParams: Int = 0)