package aha
package wavereplay

/* Scala Imports */
import scala.collection.mutable.ArrayBuffer

case class Action(Event: WaveEvent, Work: () => Unit)

/**
 * Base class for wave actions
 *
 */
abstract class WaveAction(val Wvfm: WaveForm) {

    // Implicit WaveForm for signals declared within a WaveAction
    implicit val impWaveForm: WaveForm = Wvfm

    // Internal container for all actions
    private val actions = ArrayBuffer[Action]()

    /**
     * Define an action
     *
     * @param events the events that trigger an action
     * @param callback the work to do for an action
     */
    def At(events: WaveEvent*)(callback: => Unit): Unit = events foreach {
        case e => actions += Action(e, (() => callback))
    }

    /**
     * Get all defined actions
     */
    def GetActions(): Seq[Action] = actions.toSeq
}
