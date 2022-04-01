package aha
package wavereplay

/* Scala Imports */
import scala.collection.mutable.{ArrayBuffer, HashMap}

object WaveReplay {


    def Replay(wActions: Seq[WaveAction]): Unit = {

        if (wActions.length > 0) {

            // Get waveform
            val wvfm = wActions.head.Wvfm

            // Get all actions
            val actions = wActions.flatMap(x => x.GetActions())

            // Get all events
            val events = actions.flatMap(x => x.Events)
            val eventSet = Set(events: _*)

            // Initialize waveform
            wvfm.Init()

            // Run loop
            while(wvfm.Next()) {

                // Get events that occured
                val eventsFired = eventSet.filter(x => x.Occurred(wvfm))

                // Run event callbacks
                actions foreach { case x =>
                    FireCallback(eventsFired, x)
                }
            }

            // De-Initialize waveform
            wvfm.DeInit()
        }
    }

    def FireCallback(eventsFired: Set[WaveEvent], waveAct: Action): Unit = {
        val validEvents = eventsFired.intersect(waveAct.Events.toSet)
        if (validEvents.size > 0) {
            waveAct.Work()
        }
    }
}
