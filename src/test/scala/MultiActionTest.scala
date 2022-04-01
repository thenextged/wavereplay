package aha
package wavereplay

/* Scala Test */
import org.scalatest.flatspec.AnyFlatSpec

class ActionA(val wvfm: WaveForm) extends WaveAction()(wvfm) {
    var nEvents = 0
    val clk     = Signal("top.CLK")
    val a       = Signal("top.A")

    def MonitorA(): Unit = At(posedge(clk)) {
        if (a) nEvents += 1
    }

    def NumEvents(): Int = nEvents
}

class ActionB(val wvfm: WaveForm) extends WaveAction()(wvfm) {
    var nEvents = 0
    val clk     = Signal("top.CLK")
    val b       = Signal("top.B")

    def MonitorB(): Unit = At(posedge(clk)) {
        if (b) nEvents += 1
    }

    def NumEvents(): Int = nEvents
}

class ActionAB(val wvfm: WaveForm) extends WaveAction()(wvfm) {
    var nEventsA    = 0
    var nEventsB    = 0
    val clk         = Signal("top.CLK")
    val a           = Signal("top.A")
    val b           = Signal("top.B")

    def MonitorA(): Unit = At(posedge(clk)) {
        if (a) nEventsA += 1
    }

    def MonitorB(): Unit = At(posedge(clk)) {
        if (b) nEventsB += 1
    }

    def NumEventsA(): Int = nEventsA
    def NumEventsB(): Int = nEventsB
}

class MultiActionTest extends AnyFlatSpec {

    "WaveReplay" should "replay multiple wave actions on the same waveform" in {

        val vcdContent  = """$scope module top $end
                             $var wire 1 !  CLK $end
                             $var wire 1 # A $end
                             $var wire 1 $ B $end
                             $upscope $end
                             $enddefinitions $end

                             #0
                             $dumpvars
                             0!
                             0#
                             0$
                             $end

                             #10
                             1!
                             #20
                             0!
                             #30
                             1!
                             1$
                             #40
                             0!
                             #50
                             1!
                             1#
                             #60
                             0!
                             #70
                             1!
                             0#
                             #80
                             0!
                             #90
                             1!
                             """
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)

        val wvactA          = new ActionA(wvfm)
        val wvactB          = new ActionB(wvfm)

        wvactA.MonitorA()
        wvactB.MonitorB()

        WaveReplay.Replay(Seq(wvactA, wvactB))

        assert(wvactA.NumEvents() == 1)
        assert(wvactB.NumEvents() == 4)
    }

    "WaveReplay" should "replay multiple tasks in the same wave action" in {

        val vcdContent  = """$scope module top $end
                             $var wire 1 !  CLK $end
                             $var wire 1 # A $end
                             $var wire 1 $ B $end
                             $upscope $end
                             $enddefinitions $end

                             #0
                             $dumpvars
                             0!
                             0#
                             0$
                             $end

                             #10
                             1!
                             #20
                             0!
                             #30
                             1!
                             1$
                             #40
                             0!
                             #50
                             1!
                             1#
                             #60
                             0!
                             #70
                             1!
                             0#
                             #80
                             0!
                             #90
                             1!
                             """
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)

        val wvactAB         = new ActionAB(wvfm)

        wvactAB.MonitorA()
        wvactAB.MonitorB()

        WaveReplay.Replay(Seq(wvactAB))

        assert(wvactAB.NumEventsA() == 1)
        assert(wvactAB.NumEventsB() == 4)
    }

}
