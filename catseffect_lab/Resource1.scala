import cats.effect.Resource
/**
*  Resource[IO, A] describes the ability to initialize and release a resource.
* similar to trycatch but composable and referentially transparent.
 * Allows guarenteed finalizations (Whether the f given completes, fails or cancelled)
 the resource is closed, will not leak
*/
import java.io.{FileInputStream, FileOutputStream}

object ResourceExample extends App {

  // Dummy class representing the content of a file
  case class FileContent(content: String)

  // Function to open a file
  def openFile(name: String): Resource[IO, FileInputStream] =
    Resource.make(IO(new FileInputStream(name)))(stream => IO(stream.close()).handleErrorWith(_ => IO.unit))

  // Function to close a file
  def close(file: FileInputStream): IO[Unit] =
    IO(file.close()).handleErrorWith(_ => IO.unit)

  // Function to read from a file
  def read(file: FileInputStream): IO[FileContent] =
    IO {
      val byteArray = new Array[Byte](file.available())
      file.read(byteArray)
      FileContent(new String(byteArray))
    }.handleErrorWith(_ => IO(FileContent("")))

  // Function to write to a file
  def write(file: FileOutputStream, content: String): IO[Unit] =
    IO(file.write(content.getBytes)).handleErrorWith(_ => IO.unit)

  // Example usage
  val concat: IO[Unit] =
    (
      for {
        in1 <- openFile("file1")
        in2 <- openFile("file2")
        out <- openFile("file3")
      } yield (in1, in2, out)
      ).use { case (file1, file2, file3) =>
      for {
        content1 <- read(file1)
        content2 <- read(file2)
        _ <- write(new FileOutputStream("result.txt"), content1.content + content2.content)
      } yield ()
    }

  implicit val runtime = cats.effect.unsafe.IORuntime.global
  concat.unsafeRunSync()
}

object Example1 extends IOApp.Simple {

  def getFileContents(source: scala.io.Source, content: StringBuilder): IO[String] = {
    if (source.hasNext) {
      val line = source.getLines().toList
      getFileContents(source, content.append(line).append(System.lineSeparator()))
    } else {
      IO(content.toString())
    }
  }

  def readFile(path: String): Resource[IO, BufferedSource] = {
    Resource.make {
      new File(path)
      IO(scala.io.Source.fromFile(path))
      //new Scanner(new FileReader(file))
      //getFileContents(sc, new StringBuilder())
    } { s =>
      IO(s.close()) *>
        IO.println(s"Stopped ${new File(path).getAbsoluteFile.toString}!")
    }
  }

  def printDots: IO[Unit] = (IO(println(".")) *> IO.sleep(1.second)) >> printDots

  override def run: IO[Unit] = {
    (for {
      c1 <- readFile("/Users/awani/test1.txt")
      c2 <- readFile("/Users/awani/test2.txt")
    } yield (c1, c2)
      ).use { case (f1, f2) =>
      for {
        s1 <- getFileContents(f1, new StringBuilder())
        s2 <- getFileContents(f2, new StringBuilder())
        _ <- IO.println(s1 + s2)
        _ <- printDots
      } yield ()
    }
  }
}
