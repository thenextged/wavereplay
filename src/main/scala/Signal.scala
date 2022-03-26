package aha
package wavereplay

/**
 * Information about a `Signal` at a certain time
 *
 * @param Value     the value of the signal
 * @param Width     the width of the signal.
 *                  Width == 0 => SigInfo is invalid
 * @param Time      the time for which the signal value is provided
 */
case class SigInfo(Value: Int = 0, Width: Int = 0, Time: Long = 0)

/**
 * History of a `Signal`
 *
 * @param Prev      information about a signal's previous state
 * @param Cur       information about a signal's current state
 */
case class SigHistory(Prev: Option[SigInfo] = None, Cur: Option[SigInfo] = None)


/**
 * A signal in a waveform
 *
 * @param FullPath  the full hierarchical name of a signal
 * @param Wvfm      the waveform containing the signal
 */
class Signal(val FullPath: String)(implicit val Wvfm: WaveForm) {

    /**
     * Convert to an Int`
     */
    def toInt: Int = Wvfm.GetCurValue(this) match {
        case Some(v)    => v
        case _          => 0
    }

    /**
     * Convert to a boolean
     */
    def toBool: Int = Wvfm.GetCurValue(this) match {
        case Some(v)    => v != 0
        case _          => false
    }

    /**
     * Signal equality
     */
    override def equals(x: Any): Boolean = x match {
        case s: Signal  => FullPath == s.FullPath
        case _          => false
    }

    /**
     * Signal hash code
     */
    override def hashCode: Int = 31 * FullPath.hashCode

}

object Signal {

    /**
     * Implicit conversion of a `Signal` to an `Int`
     */
    implicit def sigToInt(s: Signal): Int = s.toInt

    /**
     * Implicit conversion of a `Signal` to a `Boolean`
     */
    implicit def sigToBoolean(s: Signal): Boolean = s.toBool
}
