package scout.smelltool

import scala.tools.nsc.util.Position

class Violation(message:String, details:String, pos:Option[Position]) {
  def this(position:Position, message:String, details:String) = this(message, details, Some(position))
  def this(position:Position, message:String) = this(message, "", Some(position))
  def this(message:String, details:String) = this(message, details, None)

  def position =  pos.get 

  override def toString = {
    def result = pos match {
      case Some(position) => "[source: " + position.source +", line-" + position.line  + "] "
      case None => ""
    } 
    result + getMessage
  }

  def getMessage = (details + " " + message).trim
}