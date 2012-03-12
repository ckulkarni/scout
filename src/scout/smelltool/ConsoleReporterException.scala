package scout.smelltool
class ConsoleReporterException(fileName: String, line: Int, message: String) extends Exception {

  override def getMessage = "[source: " + fileName + ", line-" + line + "] " + message
}