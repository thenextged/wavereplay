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
