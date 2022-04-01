package aha
package wavereplay

/* Scala Test */
import org.scalatest.flatspec.AnyFlatSpec

class PosedgeCountAction(val wvfm: WaveForm) extends WaveAction()(wvfm) {

    val recEvents = scala.collection.mutable.ArrayBuffer[Long]()

    val clk = new Signal("top.CLK")

    def EnableEventMonitor(): Unit = At(posedge(clk)) {
        recEvents += clk.GetWaveForm().GetCurTime()
    }

    def Events(): List[Long] = recEvents.toList
}

class NegedgeCountAction(val wvfm: WaveForm) extends WaveAction()(wvfm) {

    val recEvents = scala.collection.mutable.ArrayBuffer[Long]()

    val clk = new Signal("top.CLK")

    def EnableEventMonitor(): Unit = At(negedge(clk)) {
        recEvents += clk.GetWaveForm().GetCurTime()
    }

    def Events(): List[Long] = recEvents.toList
}

class TimeMarkCountAction(val wvfm: WaveForm) extends WaveAction()(wvfm) {

    val recEvents = scala.collection.mutable.ArrayBuffer[Long]()

    def EnableEventMonitor(): Unit = At(timemark()) {
        recEvents += implicitly[WaveForm].GetCurTime()
    }

    def Events(): List[Long] = recEvents.toList
}

// Custom event that represents two successive values of 0 in a 1-bit signal
class CustomEvent(val s: Signal) extends WaveEvent {
    def EventEquals(that: WaveEvent): Boolean =  that match {
        case e: CustomEvent => s == e.s
        case _              => false
    }

    def EventHash: Int = 47 * s.hashCode

    def Occurred(w: WaveForm): Boolean = s.GetWidth() match {
        case 1 => {
            val SigHistory(prev, cur) = w.GetSigHistory(s)
            for (p <- prev; c <- cur) {
                return (p.Value == 0) && (c.Value == 0)
            }
            return false
        }
        case _ => false
    }
}

class CustomCountAction(val wvfm: WaveForm) extends WaveAction()(wvfm) {

    val recEvents = scala.collection.mutable.ArrayBuffer[Long]()

    val clk = new Signal("top.CLK")

    def EnableEventMonitor(): Unit = At(new CustomEvent(clk)) {
        recEvents += implicitly[WaveForm].GetCurTime()
    }

    def Events(): List[Long] = recEvents.toList
}

case class ReadyValidRecord(Time: Long, Data: Long)

class ReadyValidAction(val wvfm: WaveForm) extends WaveAction()(wvfm) {

    val recEvents = scala.collection.mutable.ArrayBuffer[ReadyValidRecord]()

    val clk     = Signal("top.CLK")
    val ready   = Signal("top.READY")
    val valid   = Signal("top.VALID")
    val data    = Signal("top.DATA")

    def EnableEventMonitor(): Unit = At(posedge(clk)) {
        if (ready && valid) {
            recEvents += ReadyValidRecord(implicitly[WaveForm].GetCurTime(), data)
        }
    }

    def Events(): List[ReadyValidRecord] = recEvents.toList
}

class BigValueAction(w: WaveForm) extends WaveAction()(w) {
    var v = 0L

    val data = Signal("top.DATA")

    At(timemark()) {
        v = data
    }

    def GetValue(): Long = v
}

class WaveActionTest extends AnyFlatSpec {

    "WaveReplay" should "detect posedge event" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #0
                                 0!
                                 #10
                                 1!
                                 #20
                                 0!
                                 #30
                                 1!"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val wvact           = new PosedgeCountAction(wvfm)

        wvact.EnableEventMonitor()
        WaveReplay.Replay(Seq(wvact))

        val events = wvact.Events()

        assert(events.size == 2)
        assert(events(0) == 10)
        assert(events(1) == 30)
    }

    "WaveReplay" should "detect negedge event" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #0
                                 0!
                                 #10
                                 1!
                                 #20
                                 0!
                                 #30
                                 1!
                                 #40
                                 0!"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val wvact           = new NegedgeCountAction(wvfm)

        wvact.EnableEventMonitor()
        WaveReplay.Replay(Seq(wvact))

        val events = wvact.Events()

        assert(events.size == 2)
        assert(events(0) == 20)
        assert(events(1) == 40)
    }

    "WaveReplay" should "detect timemark event" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #0
                                 0!
                                 #10
                                 1!
                                 #20
                                 0!"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val wvact           = new TimeMarkCountAction(wvfm)

        wvact.EnableEventMonitor()
        WaveReplay.Replay(Seq(wvact))

        val events = wvact.Events()

        assert(events.size == 3)
        assert(events(0) == 0)
        assert(events(1) == 10)
        assert(events(2) == 20)
    }

    "WaveReplay" should "detect custom event" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #0
                                 0!
                                 #10
                                 1!
                                 #20
                                 0!
                                 #30
                                 0!
                                 #40
                                 1!"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val wvact           = new CustomCountAction(wvfm)

        wvact.EnableEventMonitor()
        WaveReplay.Replay(Seq(wvact))

        val events = wvact.Events()

        assert(events.size == 1)
        assert(events(0) == 30)
    }

    "WaveReplay" should "detect a multi-signal transaction" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $var wire 1 # VALID $end
                                 $var wire 1 $ READY $end
                                 $var wire 8 % DATA [7:0] $end
                                 $upscope $end
                                 $enddefinitions $end

                                 #500
                                 $dumpvars
                                 0!
                                 0#
                                 1$
                                 b00000000 %
                                 $end

                                 #510
                                 1!
                                 #520
                                 0!
                                 #530
                                 1!
                                 1#
                                 b10101110 %
                                 #540
                                 0!
                                 0#
                                 """
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val wvact           = new ReadyValidAction(wvfm)

        wvact.EnableEventMonitor()
        WaveReplay.Replay(Seq(wvact))

        val events = wvact.Events()

        assert(events.size == 1)
        assert(events(0).Time == 530)
        assert(events(0).Data == 0xAE)
    }

    "WaveReplay" should "record signals with big integer values" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 33 ! DATA $end
                                 $upscope $end
                                 $enddefinitions $end

                                 #500
                                 b111001010110010101100101011001010 !
                                 """
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val wvact           = new BigValueAction(wvfm)

        WaveReplay.Replay(Seq(wvact))

        assert(wvact.GetValue() == 0x1CACACACAL)
    }

}
