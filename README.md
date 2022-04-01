# WaveReplay

A library for reconstructing transactions from a waveform file

## Example

```scala

import aha.wavereplay._
import aha.wavereplay.vcd

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

object Main extends App {
    val vcdContent = """$timescale 1 ns $end
                        $scope module top $end
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

    println(wvact.Events())
}

```
