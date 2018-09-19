import cats.Id
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

import scala.io.Source

object HangmanAlgebraSpec extends Properties("HangmanAlgebra") {

  property("startGame") = forAll(wordGen) { word =>
    new TestHangmanAlgebra(word).runGame == Right(Rescued)
  }

  private lazy val wordGen = Gen.oneOf(dict)

  private lazy val dict = {
    val source = Source.fromInputStream(getClass.getResourceAsStream("test-dict.txt"))
    try {
      source.getLines().toList
    } finally {
      source.close()
    }
  }

  private class TestHangmanAlgebra(testInput: String) extends HangmanAlgebra[Id] {

    private val userBehaviour = new Function[Option[Char], String] {
      var hangmanState = testInput.head + ("?" * (testInput.length - 2)) + testInput.last
      val indexedIn = testInput.zipWithIndex

      override def apply(guessOpt: Option[Char]): String = guessOpt match {
        case Some(guess) if testInput.contains(guess) && !hangmanState.substring(1, hangmanState.length - 1).contains(guess) =>
          indexedIn.collect { case (`guess`, i) => i }.foreach { charPos =>
            hangmanState = hangmanState.take(charPos) + guess + hangmanState.takeRight(testInput.length - charPos - 1)
          }
          hangmanState
        case _ => hangmanState
      }
    }

    override def askAndGetInput(assumption: Option[Char]): Id[String] = userBehaviour(assumption)

    override def loadDictionary(collector: DictionaryCollector): Id[List[String]] = dict.collect(collector)

    override def onInvalidInput(input: String): Id[FailAck] = Stop
  }

}
