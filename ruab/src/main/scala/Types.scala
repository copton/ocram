package ch.ethz.inf.vs.ruab {
  object Types {
    type SourceLine = Int

    abstract class FQI( // fully-qualified identifier
      val name: String
    )
    case class GlobalIdentifier(override val name: String) extends FQI(name)
    case class ScopedIdentifier(
      override val name: String,
      val function: String,
      val scope: List[Int]
    ) extends FQI(name)


    class Frame(val function: String, val line: Int)
    class Backtrace(val frames: List[Frame])
  }
}
