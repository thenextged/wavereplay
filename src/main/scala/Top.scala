package aha
package wavereplay

/* Scala Imports */
import scala.collection.mutable.ArrayBuffer

/* WaveReplay Imports */
import wavereplay._

class MyAction(val w: WaveForm) extends WaveAction()(w) {


    val clk     = new Signal("top.design.clk")
    val ready   = new Signal("top.design.ready")
    val valid   = new Signal("top.design.valid")
    val data    = new Signal("top.design.data")

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

object Main extends App {

    val vcdFilePath = "src/main/resources/vcd/dump.vcd"
    val myWaveForm = new vcd.VcdInMemWaveForm(vcdFilePath)

    val myAction = new MyAction(myWaveForm)
    myAction.RecordTrans()
    myAction.PrintData()

    val replay = WaveReplay.Replay(Seq(myAction))

    println(s"num trans = ${myAction.numTrans}")


}
