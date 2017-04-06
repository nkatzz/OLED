package logic

object Exceptions {

   class MyParsingException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

   class AbductionException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

   class ASPInputException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

   class LogicException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

   class TrainingExamplesException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

}