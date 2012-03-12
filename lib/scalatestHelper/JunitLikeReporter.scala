import org.scalatest._
import org.scalatest.events._

class JunitLikeReporter extends Reporter {
  private var testsSucceededCount = 0
  private var testsFailedCount = 0
  def apply(event: Event) {
    event match {
      case rs:RunStarting => println("Run Starting Expected Tests: " + rs.testCount)
      case _: RunStopped => println("Run stopped")
		  case ra: RunAborted => println("Run aborted")
        stackTrace(ra.throwable)
			case rc: RunCompleted => println("Run completed.")
                               rc.summary match {
                                case Some(sum) => println("Success: " + testsSucceededCount + ", Failed: " + testsFailedCount)
                                case None => println("Success: 0, Failed: 0")
                               }
      case sc:SuiteStarting => if(sc.suiteName.trim != "specifies") println("Starting: " + sc.suiteName)
      case sc: SuiteCompleted => if(sc.suiteName.trim != "specifies") println("\n Suite Completed: " + sc.suiteName)
      case ts: TestStarting => //println("starting: " + ts.testName)
			case _: TestPending =>
      case ti: TestIgnored => println("Test Ignored: " + ti.testName)
      case sa: SuiteAborted => println("Suite Aborted: " + sa.suiteName)
        stackTrace(sa.throwable)
      case ip: InfoProvided => println(ip.message)
      case _: TestSucceeded => print(".")
        testsSucceededCount += 1 
      case tf: TestFailed => println("Test Failed: " + tf.testName)
        stackTrace(tf.throwable)
        testsFailedCount += 1 
    }
  }

  def stackTrace(t:Option[Throwable]) = t match {
    case Some(exception) => exception.printStackTrace
    case None =>
  }
}

