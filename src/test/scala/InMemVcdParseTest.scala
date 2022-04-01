package aha
package wavereplay

/* Scala Test */
import org.scalatest.flatspec.AnyFlatSpec

class InMemVcdParseTest extends AnyFlatSpec {

    "VCD parser" should "read in timescale entry" in {
        val vcdContent      = "$timescale 10 ns $end"
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val timescale       = wvfm.Timescale()

        assert(timescale.isDefined && timescale.get == (10, "ns"))
    }

    "VCD parser" should "read in version entry" in {
        val vcdContent      = "$version AHA SIMULATOR $end"
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val version         = wvfm.Version()

        assert(version.isDefined && version.get == "AHA SIMULATOR")
    }


}
