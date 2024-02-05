package http4s

import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.http4s.server.Server

object Server extends IOApp {

  import com.comcast.ip4s._
  import org.http4s._
  import org.http4s.dsl.io._
  import org.http4s.ember.server.EmberServerBuilder
  import org.http4s.implicits._
  import org.http4s.server.middleware.Logger


  val app = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / index =>
       Ok(s"Index: $index.")
  }.orNotFound

  val finalHttpApp = Logger.httpApp(true, true)(app)

  val server: Resource[IO, Server] = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(finalHttpApp)
    .build
  override def run(args: List[String]): IO[ExitCode] = server.use(_ => IO.never).as(ExitCode.Success)

}
