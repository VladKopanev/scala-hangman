import cats.effect.Console.io._
import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.io.Source

object Hangman extends IOApp with HangmanAlgebra[IO] {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      gameResult <- runGame
      exitCode <- gameResult.fold(interpretError, interpretSuccess)
    } yield exitCode

  override def askAndGetInput(assumption: Option[Char]): IO[String] = for {
    _ <- assumption.fold(putStrLn(s"Enter initial characters to start the game, for example like so a????c")) { guess =>
      putStrLn(s"Does word contain '$guess'? Put new line")
    }
    input <- readLn
  } yield input

  override def onInvalidInput(input: String): IO[FailAck] = {
    putStrLn(s"The input $input was invalid!!").map(_ => Continue)
  }

  override def loadDictionary(collector: DictionaryCollector): IO[List[String]] = {
    Resource.fromAutoCloseable(acquireDictionaryResource).use(iter => IO(iter.getLines().collect(collector).toList))
  }

  private val acquireDictionaryResource = IO(Source.fromInputStream(getClass.getResourceAsStream("dict.txt")))

  private def interpretError(gameError: GameError): IO[ExitCode] = (gameError match {
    case InvalidInput => putStrLn("The input was wrong!!")
    case UnknownWord => putStrLn("I don't know the word you asking for :(")
  }).map(_ => ExitCode.Error)

  private def interpretSuccess(gameResult: HangmanResult): IO[ExitCode] = (gameResult match {
    case Rescued => putStrLn("I won!!!")
    case Choked => putStrLn("I loose!!!")
  }).map(_ => ExitCode.Success)
}
