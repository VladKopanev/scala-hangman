sealed trait HangmanResult
case object Rescued extends HangmanResult
case object Choked extends HangmanResult

sealed trait GameError
case object InvalidInput extends GameError
case object UnknownWord extends GameError

sealed trait FailAck
case object Continue extends FailAck
case object Stop extends FailAck
