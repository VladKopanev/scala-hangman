import cats.Monad

import scala.language.higherKinds

trait HangmanAlgebra[F[_]] {

  import cats.Monad.ops._

  type GameResult = F[Either[GameError, HangmanResult]]
  type DictionaryCollector = PartialFunction[String, String]

  protected val maxFailCount: Int = 10

  protected def askAndGetInput(assumption: Option[Char]): F[String]

  protected def loadDictionary(collector: DictionaryCollector): F[List[String]]

  protected def onInvalidInput(input: String): F[FailAck]

  def runGame(implicit m: Monad[F]): GameResult = {
    def runComputer(initialInput: String) = for {
      dict <- loadDictionary(initialInputCollector(initialInput))
      result <- computerTurn(initialInput, dict, guessed = "", failed = "")
    } yield result

    for {
      initialInput <- askAndGetInput(None)
      result <- initialInput match {
        case invalidInput(_) => onInvalidInput(initialInput, runGame)
        case _ => runComputer(initialInput)
      }
    } yield result
  }

  private def userTurn(dictionary: List[String],
                       prevInput: String,
                       guess: Char,
                       guessed: String,
                       failed: String)
                      (implicit m: Monad[F]): GameResult = {

    lazy val repeat = userTurn(dictionary, prevInput, guess, guessed, failed)

    def processInput(input: String) = input match {
      case invalidInput(_) => onInvalidInput(input, repeat)
      case `prevInput` if failed.length == maxFailCount => endGame(Choked)
      case `prevInput` => computerTurn(input, filterFailed(dictionary, guess), guessed, failed + guess)
      case newInput if input.contains("?") => computerTurn(newInput, filterGuessed(dictionary, input), guessed + guess, failed)
      case _ => endGame(Rescued)
    }

    for {
      input <- askAndGetInput(Some(guess))
      result <- processInput(input)
    } yield result
  }

  def filterFailed(dict: List[String], failed: Char): List[String] =
    dict.filterNot(_.contains(failed))

  def filterGuessed(dict: List[String], input: String): List[String] = {
    val rawRegex = input.substring(1, input.length - 1).replace("?", "(.)")
    dict.filter(_.matches(rawRegex))
  }

  private def computerTurn(input: String, dictionary: List[String], guessed: String, failed: String)
                          (implicit m: Monad[F]): GameResult = {
    if (dictionary.isEmpty) failGame(UnknownWord)
    else {
      recalculateAndFind(input.length, dictionary, guessed, failed).fold[GameResult](failGame(UnknownWord)) {
        guessedChar => userTurn(dictionary, input, guessedChar, guessed, failed)
      }
    }
  }

  /**
    * Heuristic calculated accordingly to [[http://datagenetics.com/blog/april12012/index.html Data Genetics]]
    **/
  type WordLength = Int
  type MostFrequentLetters = String

  private def firstHitFrequency: Map[WordLength, MostFrequentLetters] = Map(
    1 -> "ai",
    2 -> "aoeiumbh",
    3 -> "aeoiuyhbck",
    4 -> "aeoiuysbf",
    5 -> "seaoiuyh",
    6 -> "eaiousy",
    7 -> "eiaous",
    8 -> "eiaou",
    9 -> "eiaou",
    10 -> "eioau",
    11 -> "eioad",
    12 -> "eioaf",
    13 -> "ieoa",
    14 -> "ieo",
    15 -> "iea",
    16 -> "ieh",
    17 -> "ier",
    18 -> "iea",
    19 -> "iea",
    20 -> "ie"
  )

  /**
    * Recalculate frequencies of letters in new dictionary and select most frequent letter
    **/
  private def recalculateAndFind(size: Int, dict: List[String], guessed: String, failed: String): Option[Char] = {
    if (guessed.isEmpty) Some(firstHitFrequency(size).charAt(failed.length))
    else {
      val candidates = dict
        .flatten
        .filter(c => !guessed.contains(c) && !failed.contains(c))

      if (candidates.isEmpty) None
      else Some(candidates.groupBy(identity).map { case (k, v) => (k, v.length) }.maxBy(_._2)._1)
    }
  }

  private def onInvalidInput(input: String,
                             continue: => GameResult)
                            (implicit m: Monad[F]): GameResult =
    onInvalidInput(input).flatMap[Either[GameError, HangmanResult]] {
      case Continue => continue
      case Stop => failGame(InvalidInput)
    }

  private def endGame(hr: HangmanResult)(implicit m: Monad[F]): GameResult = m.pure(Right(hr))

  private def failGame(ge: GameError)(implicit m: Monad[F]): GameResult = m.pure(Left(ge))

  private object invalidInput {
    def unapply(input: String): Option[String] = Some(input).filterNot(_.matches("""\w(\?|\w|')+\w"""))
  }

  private def initialInputCollector(initialInput: String): DictionaryCollector = {
    val first = initialInput.head
    val last = initialInput.last
    val length = initialInput.length
    val collector: DictionaryCollector = {
      case s if s.head == first && s.last == last && s.length == length => s.substring(1, s.length - 1)
    }
    collector
  }
}
