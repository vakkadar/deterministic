import System.{getProperty => Prop}
import System.{getProperties => Props}
import collection.JavaConverters._
import java.io._

object Parent extends App{

  val workingDir = Prop("user.dir")
  val pb = new ProcessBuilder("java", "-cp", "c:\\cygwin64\\home\\Rajkumar\\projects\\hello-scala-2_11\\target\\scala-2.11\\classes;c:\\Users\\Rajkumar\\.ivy2\\cache\\org.scala-lang\\scala-library\\jars\\scala-library-2.11.7.jar", "Child")
  val cwd = new File("c:\\cygwin64\\home\\Rajkumar\\projects\\hello-scala-2_11\\target\\scala-2.11\\classes")
  pb.directory(new File(workingDir))

  val process = pb.start()

  def getProcessInputHandlers(filename:String) :List[ProcessInputHandler] =
    List(ProcessInputHandler(process, OutputType, filename),ProcessInputHandler(process, ErrorType, filename) )

  val inputHandlerRunnables = getProcessInputHandlers("output")

  val inputHandlerThreads = inputHandlerRunnables map (new Thread(_))
  inputHandlerThreads foreach (_.start())
  process.waitFor()

  inputHandlerRunnables foreach (_.terminate())
  inputHandlerThreads foreach (_.join())

  println (process.exitValue())
}

case class ProcessInputThread(process:Process, streamType:StreamType, filename:String) extends Thread {
  val handler = ProcessInputHandler(process, streamType, filename)
  override def run() = handler.run()
}



sealed trait StreamType{
  val FileConcatenator = "."
  def name:String
  def suffix:String
  def getCanonicalFileName(filename:String) = filename + FileConcatenator + suffix
  import scala.annotation._
  def getInputStream(process:Process)=
    (this: @unchecked) match {
      case OutputType => process.getInputStream
      case ErrorType => process.getErrorStream
    }
}

case object InputType extends StreamType{
  val name = "InputStream"
  val suffix = "in"
}
case object OutputType extends StreamType{
  val name = "OutputStream"
  val suffix = "out"
}
case object ErrorType extends StreamType{
  val name = "ErrorStream"
  val suffix = "err"
}

object ProcessConstants{
  val SuccessfullyStarted = "SUCCESSFULLY STARTED"
  val StartedWithErrors = "STARTED WITH ERRORS"
}

case class ProcessInputHandler(process:Process,  streamType:StreamType, filename:String) extends  Runnable{

  val canonicalFilename = streamType.getCanonicalFileName(filename)
  val fos = new FileOutputStream(canonicalFilename)
  val writer :PrintWriter = new PrintWriter(fos)
  val inputStream = streamType.getInputStream(process)

  @volatile
  private[this] var startedFlag = false

  @volatile
  private[this] var terminateFlag = false

  def terminate():Unit = terminateFlag =true

  def run():Unit ={
    try{
      val br = new BufferedReader(new InputStreamReader(inputStream))
      while(!startedFlag){
        write(getLine(br))
      }
      while(!terminateFlag){
        write(getLine(br))
      }
      writer.flush()
    }catch {
      case ioe: IOException => ioe.printStackTrace()
    }
  }

  def getLine(reader:BufferedReader): Option[String] = {
    val line = reader.readLine()
    if(line != null) Some(line) else None
  }

  def write(line:Option[String]):Unit =
   line.foreach(str => {
     writer.write(str)
     println(streamType + ">" + str)
   })




}