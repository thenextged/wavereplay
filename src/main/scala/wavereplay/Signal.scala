package aha
package wavereplay

/* Scala Imports */
import scala.language.implicitConversions

/**
 * Information about a `Signal` at a certain time
 *
 * @param Value     the value of the signal
 * @param Width     the width of the signal.
 *                  Width == 0 => SigInfo is invalid
 * @param Time      the time for which the signal value is provided
 */
case class SigInfo(Value: Long = 0, Width: Int = 0, Time: Long = 0)

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
     * Get the waveform backing this signal
     */
    def GetWaveForm(): WaveForm = Wvfm

    /**
     * Convert to a Long`
     */
    def toLong: Long = Wvfm.GetCurValue(this) match {
        case Some(v)    => v
        case _          => 0
    }

    /**
     * Convert to a boolean
     */
    def toBool: Boolean = Wvfm.GetCurValue(this) match {
        case Some(v)    => v != 0
        case _          => false
    }

    /**
     *
     * Get Signal's Value
     * (works only for integer values)
     */
    def Value(): Long = toLong

    /**
     * Get Signal's Width
     */
    def GetWidth(): Int = Wvfm.GetSigWidth(this)

    /**
     * Signal equality
     */
    override def equals(x: Any): Boolean = x match {
        case s: Signal  => FullPath == s.FullPath
        case l: Long    => l == toLong
        case i: Int     => i == toLong.toInt
        case b: Boolean => b == toBool
        case _          => false
    }

    /**
     * Signal hash code
     */
    override def hashCode: Int = 31 * FullPath.hashCode
}

class SignalOps(val s: Signal) {
    def unary_~ = ~(s.toLong)

    def +(that: Signal) = s.toLong + that.toLong
    def -(that: Signal) = s.toLong - that.toLong
    def *(that: Signal) = s.toLong * that.toLong
    def /(that: Signal) = s.toLong / that.toLong
}

object Signal {

    /**
     * Implicit conversion of a `Signal` to a `Long`
     */
    implicit def sigToLong(s: Signal): Long = s.toLong

    /**
     * Implicit conversion of a `Signal` to a `Boolean`
     */
    implicit def sigToBoolean(s: Signal): Boolean = s.toBool

    /**
     * Implicit conversion of a `Signal` to a `SignalOps`
     */
    implicit def sigToSigOps(s: Signal): SignalOps = new SignalOps(s)

    /**
     * Factory method
     */
    def apply(fullPath: String)(implicit w: WaveForm): Signal = new Signal(fullPath)(w)
}
