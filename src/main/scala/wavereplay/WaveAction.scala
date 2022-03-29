package aha
package wavereplay

/* Scala Imports */
import scala.collection.mutable.ArrayBuffer

case class Action(Events: Seq[WaveEvent], Work: () => Unit)

/**
 * Base class for wave actions
 *
 */
abstract class WaveAction(implicit val Wvfm: WaveForm) {

    // Internal container for all actions
    private val actions = ArrayBuffer[Action]()

    /**
     * Define an action
     *
     * @param events the events that trigger an action
     * @param callback the work to do for an action
     */
    def At(events: WaveEvent*)(callback: => Unit): Unit = {
        actions += Action(events.toSeq, (() => callback))
    }

    /**
     * Get all defined actions
     */
    def GetActions(): Seq[Action] = actions.toSeq
}
