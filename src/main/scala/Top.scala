package aha
package wavereplay

/* Scala Imports */
import scala.collection.mutable.ArrayBuffer

/* WaveReplay Imports */
import wavereplay._

//
// Note:
//  @-calls will execute in the order they are called.
//  One might want to make @-calls within defs in order to control
//  their calling order later
//
class AxiAction extends WaveAction {

    // clock
    val aclk        = Signal("...")

    // write address
    val awaddr      = Signal("...")
    val awlen       = Signal("...")
    val awsize      = Signal("...")
    val awburst     = Signal("...")
    val awvalid     = Signal("...")
    val awready     = Signal("...")

    // write data
    val wdata       = Signal("...")
    val wstrb       = Signal("...")
    val wlast       = Signal("...")
    val wvalid      = Signal("...")
    val wready      = Signal("...")

    // write response
    val bvalid      = Signal("...")
    val bready      = Signal("...")

    // read address
    val araddr      = Signal("...")
    val arlen       = Signal("...")
    val arsize      = Signal("...")
    val arburst     = Signal("...")
    val arvalid     = Signal("...")
    val arready     = Signal("...")

    // read data
    val rdata       = Signal("...")
    val rlast       = Signal("...")
    val rready      = Signal("...")
    val rvalid      = Signal("...")

    //
    // Record Addresses
    //
    val axi_addr  = ArrayBuffer[Int]()

    def RecordAddresses: Unit = at(posedge aclk) {
        if (awvalid & awready) {
            axi_addr += awaddr
        }
    }


    //
    // Record Data
    //
    val axi_data = ArrayBuffer[Int]()

    def RecordData: Unit = at(posedge aclk) {
        if (wvalid & wready) {
            axi_data += wdata
        }
    }

    //
    // Print (Addr, Data) pairs
    //

    def PrintAddrDataPairs: Unit = (axi_addr zip axi_data) foreach {
        case (addr, data) => println(s"${addr} -> ${data}")
        case _ =>
    }


}

object Main extends App {

    val axi_action = new AxiAction

    val waveform = WaveForm.FromVCD("....");
    waveform.replay(Seq(axi_replay))

}
