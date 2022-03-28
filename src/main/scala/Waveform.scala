package aha
package wavereplay

abstract class WaveForm {

    /**
     * Waveform stepping hook
     * called to advance to the next time mark
     *
     * @return True if new time mark is reached and new values are read,
     *          False otherwise
     */
    def Next(): Boolean

    /**
     * Get current time mark in the waveform
     */
    def GetCurTime(): Int

    /**
     * Check for equality
     */
    def SameAs(that: WaveForm): Boolean

    /**
     * Get a SigInfo for the given signal
     *
     * @param s the signal of interest
     * @return SigInfo for the given signal
     */
    def GetSigInfo(s: Signal): SigInfo

    /**
     * Get a SigHistory for the given signal
     *
     * @param s the signal of interest
     * @return SigHistory for the given signal
     */
    def GetSigHistory(s: Signal): SigHistory

    /**
     * Initialization hook
     * called before signal values are read
     */
    def Init(): Unit = {}

    /**
     * De-initialization hook
     * called after all signals are read
     */
    def DeInit(): Unit = {}

    /**
     * Get previous value of a signal
     *
     * @param s the signal of interest
     * @return optionally the previous value of a signal
     */
    def GetPrevValue(s: Signal): Option[Int] = GetSigHistory(s).Prev match {
        case Some(sInfo)    => Some(sInfo.Value)
        case _              => None
    }

    /**
     * Get current value of a signal
     *
     * @param s the signal of interest
     * @return optionally the current value of a signal
     */
    def GetCurValue(s: Signal): Option[Int] = GetSigHistory(s).Cur match {
        case Some(sInfo)    => Some(sInfo.Value)
        case _              => None
    }

    /**
     * Get width of a signal
     *
     * @param s the signal of interest
     * @return the width of the given signal
     */
    def GetSigWidth(s: Signal): Int = GetSigInfo(s).Width

    override def equals(that: Any): Boolean = that match {
        case w: WaveForm    => SameAs(w)
        case _              => false
    }
}
