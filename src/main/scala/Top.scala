package aha
package wavereplay

/* Scala Imports */
import scala.collection.mutable.ArrayBuffer

/* WaveReplay Imports */
import wavereplay._

class MyAction(val w: WaveForm) extends WaveAction()(w) {


    val clk     = new Signal("clk")
    val ready   = new Signal("ready")
    val valid   = new Signal("valid")
    val data    = new Signal("data")

    var numTrans = 0

    def RecordTrans(): Unit = At(posedge(clk)) {
        if (ready && valid) {
            numTrans = numTrans + 1
        }
    }

    def PrintData(): Unit = At(posedge(clk)) {
        if (ready && valid) {
            val d: Int = data
            println(s"Data: $d, Time = ${data.GetWaveForm().GetCurTime()}")
        }
    }
}

class MyWaveForm extends WaveForm {
    var i       = -1;
    val clk     = Seq(0, 1, 0, 1, 0, 1, 0)
    val valid   = Seq(0, 1, 0, 1, 0, 1, 0)
    val ready   = Seq(1, 1, 1, 1, 1, 1, 1)
    val data    = Seq(0, 25, 0, 30, 0, 40, 0)

    def Next(): Boolean = {
        i = i + 1
        i < valid.size
    }

    def GetCurTime(): Int = i

    def SameAs(that: WaveForm): Boolean = that match {
        case w: MyWaveForm  => true
        case _              => false
    }

    def GetSigInfo(s: Signal): SigInfo = s.FullPath match {
        case "clk"      => SigInfo(if(i >= 0) clk(i) else 0, 1, i)
        case "ready"    => SigInfo(if(i >= 0)  ready(i) else 0, 1, i)
        case "valid"    => SigInfo(if(i >= 0)  valid(i) else 0, 1, i)
        case "data"     => SigInfo(if(i >= 0)  data(i) else 0, 8, i)
        case _          => SigInfo(0, 0, 0)
    }

    def GetSigHistory(s: Signal): SigHistory = s.FullPath match {
        case "clk"      => SigHistory(
                            if(i > 0) Some(SigInfo(clk(i-1), 1, i-1)) else None,
                            if(i >= 0) Some(SigInfo(clk(i), 1, i)) else None
                        )
        case "ready"      => SigHistory(
                            if(i > 0) Some(SigInfo(ready(i-1), 1, i-1)) else None,
                            if(i >= 0) Some(SigInfo(ready(i), 1, i)) else None
                        )
        case "valid"      => SigHistory(
                            if(i > 0) Some(SigInfo(valid(i-1), 1, i-1)) else None,
                            if(i >= 0) Some(SigInfo(valid(i), 1, i)) else None
                        )
        case "data"      => SigHistory(
                            if(i > 0) Some(SigInfo(data(i-1), 8, i-1)) else None,
                            if(i >= 0) Some(SigInfo(data(i), 8, i)) else None
                        )
        case _          => SigHistory(None, None)
    }
}


object Main extends App {

    /**
    val myWaveForm = new MyWaveForm()

    val myAction = new MyAction(myWaveForm)
    myAction.RecordTrans()
    myAction.PrintData()

    val replay = WaveReplay.Replay(Seq(myAction))

    println(s"num trans = ${myAction.numTrans}")
    */

    val content = """$date June 26, 1989 10:05:41
                     $end
                     $version VERILOG-SIMULATOR 1.0a
                     $end
                     $timescale 1 ns
                     $end
                     $scope module top $end
                     $var trireg 1 *@ net1 $end
                     $var trireg 1 *# net2 $end
                     $var trireg 1 *$ net3 $end
                     $upscope $end
                     $upscope $end
                     $enddefinitions $end
                     $comment
                        dumpvars was executed at time '#500'.
                        All initial values are dumped at this time.
                     $end

                     #500
                     $dumpvars
                     x*@
                     x*#
                     x*$
                     bx (k
                     bx {2}
                     $end
                     #505
                     0*@
                     1*#
                     1*$
                     b10zx1110x11100 (k
                     b1111000101Z01x {2
                     #510
                     0*$
                     #520
                     1*$"""

    vcd.VcdLexer(content) match {
        case Right(result)  => result foreach println
        case Left(x)        => println(x)
    }


}
