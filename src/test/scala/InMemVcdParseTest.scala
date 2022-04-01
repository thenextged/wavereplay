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

    "VCD parser" should "read in comment entry" in {
        val vcdContent      = "$comment this is a comment $end"
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val comment         = wvfm.Comment()

        assert(comment.isDefined && comment.get == "this is a comment")
    }

    "VCD parser" should "read in scoped var entries" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $scope module sub $end
                                 $var reg 8 # DATA [7:0] $end
                                 $upscope $end
                                 $upscope $end"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val vars            = wvfm.Vars()

        assert(vars.size == 2)
        assert(vars("top.CLK") == vcd.VarInfo("wire", 1, "!"))
        assert(vars("top.sub.DATA") == vcd.VarInfo("reg", 8, "#"))
    }

    "VCD parser" should "read in time marker entries" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #0
                                 0!
                                 #10
                                 1!
                                 #3000000000
                                 0!"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val timeSeries      = wvfm.Data()

        assert(timeSeries.size == 3)
        assert(timeSeries(0).Time == 0)
        assert(timeSeries(1).Time == 10)
        assert(timeSeries(2).Time == 3000000000L)
    }

    "VCD parser" should "read in binary value change entry" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 1 ! CLK $end
                                 $var wire 1 # VALID $end
                                 $var wire 8 $ DATA [7:0] $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #0
                                 $dumpvars
                                 0!
                                 0#
                                 b00000000 $
                                 $end
                                 #100
                                 1!
                                 1#
                                 b10101100 $"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val timeSeries      = wvfm.Data()

        assert(timeSeries.size == 2)

        assert(timeSeries(0).Time == 0)
        assert(timeSeries(0).ChangeList(0).Id == "!")
        assert(timeSeries(0).ChangeList(0).Value == "b0")
        assert(timeSeries(0).ChangeList(1).Id == "#")
        assert(timeSeries(0).ChangeList(1).Value == "b0")
        assert(timeSeries(0).ChangeList(2).Id == "$")
        assert(timeSeries(0).ChangeList(2).Value == "b00000000")

        assert(timeSeries(1).Time == 100)
        assert(timeSeries(1).ChangeList(0).Id == "!")
        assert(timeSeries(1).ChangeList(0).Value == "b1")
        assert(timeSeries(1).ChangeList(1).Id == "#")
        assert(timeSeries(1).ChangeList(1).Value == "b1")
        assert(timeSeries(1).ChangeList(2).Id == "$")
        assert(timeSeries(1).ChangeList(2).Value == "b10101100")
    }

    "VCD parser" should "read in long binary value change entry" in {
        val vcdContent      = """$scope module top $end
                                 $var wire 33 $ DATA [32:0] $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #10
                                 b111001010110010101100101011001010 $"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val timeSeries      = wvfm.Data()

        assert(timeSeries.size == 1)

        assert(timeSeries(0).Time == 10)
        assert(timeSeries(0).ChangeList(0).Id == "$")
        assert(timeSeries(0).ChangeList(0).Value == "b111001010110010101100101011001010")
    }

    "VCD parser" should "read in real value change entry" in {
        val vcdContent      = """$scope module top $end
                                 $var real 32 $ DATA [31:0] $end
                                 $upscope $end
                                 $enddefinitions $end
                                 #10
                                 r1.2 $"""
        val wvfm            = new vcd.VcdInMemWaveForm(vcdContent)
        val timeSeries      = wvfm.Data()

        assert(timeSeries.size == 1)

        assert(timeSeries(0).Time == 10)
        assert(timeSeries(0).ChangeList(0).Id == "$")
        assert(timeSeries(0).ChangeList(0).Value == "r1.2")
    }


}
