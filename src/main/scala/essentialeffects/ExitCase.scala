package essentialeffects

sealed trait ExitCase[+E]

object ExitCase {
  case object Completed extends ExitCase[Nothing]
  case class Error[+E](error: E) extends ExitCase[E]
  case object Canceled extends ExitCase[Nothing]
}
