package aha
package wavereplay

/**
 * Base class for waveform events
 *
 */
abstract class WaveEvent {

    /**
     * Check for event equality
     *
     * @param that the other WaveEvent to compare for equality
     * @return true if the events are equal, false otherwise
     */
    def EventEquals(that: WaveEvent): Boolean

    /**
     * Hash of a WaveEvent
     *
     * @return hash of the event
     */
    def EventHash: Int

    /**
     * Check whether this event occured based on values in waveform
     *
     * @param wvfm the waveform to base decision on
     * @return true if event occured, false otherwise
     */
    def Occured(wvfm: WaveForm): Boolean

    override def equals(that: Any): Boolean = that match {
        case e: WaveEvent   => EventEquals(e)
        case _              => false
    }

    override def hashCode: Int = EventHash
}

/**
 * Positive Edge Event
 */
class PosEdgeEvent(val s: Signal) extends WaveEvent {

    def EventEquals(that: WaveEvent): Boolean = that match {
        case e: PosEdgeEvent => s == e.s
        case _              => false
    }

    def EventHash: Int = 37 * s.hashCode

    def Occured(wvfm: WaveForm): Boolean = s.Wvfm.GetSigWidth(s) match {
        case 1 => {
            val hist = wvfm.GetSigHistory(s)
            for (p <- hist.Prev; c <- hist.Cur) {
                return (p.Value == 0) && (c.Value == 1)
            }
            return false
        }
        case _ => false
    }
}

object posedge {
    def apply(s: Signal): PosEdgeEvent = new PosEdgeEvent(s)
}

/**
 * Negative Edge Event
 */
class NegEdgeEvent(val s: Signal) extends WaveEvent {

    def EventEquals(that: WaveEvent): Boolean = that match {
        case e: NegEdgeEvent => s == e.s
        case _              => false
    }

    def EventHash: Int = 41 * s.hashCode

    def Occured(wvfm: WaveForm): Boolean = s.Wvfm.GetSigWidth(s) match {
        case 1 => {
            val hist = wvfm.GetSigHistory(s)
            for (p <- hist.Prev; c <- hist.Cur) {
                return (p.Value == 1) && (c.Value == 0)
            }
            return false
        }
        case _ => false
    }
}

object negedge {
    def apply(s: Signal): NegEdgeEvent = new NegEdgeEvent(s)
}
