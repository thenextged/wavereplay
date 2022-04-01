package aha
package wavereplay
package vcd

/* Scala Imports */
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

/* Project Imports */
import aha.wavereplay.{Signal, SigInfo, SigHistory, WaveForm}
import aha.wavereplay.vcd.{VcdLexer, VcdToken}


case class VcdScope(name: String, parent: Option[VcdScope] = None) {
    def GetFullPath(): String = {
        val parentPath = parent match {
            case Some(scope)    => scope.GetFullPath() + "."
            case _              => ""
        }

        parentPath + name
    }

    def NameVar(varName: String): String = GetFullPath() match {
        case "" => varName
        case x  => x + "." + varName
    }
}

/**
 * Contain information retrieved from VCD $var ... $end
 *
 * @param kind the kind of the variable
 * @param width the width of the variable
 * @param id the VCD identifier for the variable
 */
case class VarInfo(Kind: String, Width: Int, Id: String)

/**
 * Reprensent a value change in a VCD dump at a given time
 *
 * @param id the VCD identifier for the signal which changed value
 * @param value the new value
 */
case class ValueEntry(Id: String, Value: String)

/**
 * Represent the time and value changes at a particular simulation time mark
 *
 * @param time the simulation time mark
 * @param changeList the list of value changes at the associated time
 */
case class TimeSeriesEntry(Time: Int, ChangeList: List[ValueEntry])

/**
 * Represent a value in the waveform for a signal at a particular time
 *
 * @param time the time at which the value is given
 * @param value the value
 */
case class TimedValue(Time: Int, Value: String)

/**
 * Represent the history of a signal's value
 *
 * @param prev the previous value of the signal
 * @param cur the current value of the signal
 */
case class TimedValueHistory(Prev: Option[TimedValue] = None, Cur: Option[TimedValue] = None)


/**
 * VCD WaveForm with all data loaded in memory in batch
 *
 */
class VcdInMemWaveForm(val vcdInMemContent: String) extends WaveForm {

    // Map of Identifiers to RTL signal references
    private val idToRefMap = mutable.HashMap[String, String]()

    // Map of RTL signal reference name to information about the signal
    private val refToInfoMap = mutable.HashMap[String, VarInfo]()

    // Sequence of value changes at each simulation time mark
    private val timeSeries = mutable.ArrayBuffer[TimeSeriesEntry]()

    // Map of signal's RTL reference name to the value history of the signal
    private val valueMap   = mutable.HashMap[String, TimedValueHistory]()

    // Current index into the timeSeries sequence
    private var curIdx = -1;

    // VCD timescale entry
    private var vcdTimescale: Option[Tuple2[Int, String]] = None

    // VCD version entry
    private var vcdVersion: Option[String] = None

    // Initialize VCD
    VcdInit()

    /**
     * Retrieve VCD Timescale Entry
     */
    def Timescale(): Option[Tuple2[Int, String]] = vcdTimescale

    /**
     * Retrive VCD Version Entry
     *
     */

    def Version(): Option[String] = vcdVersion

    def Next(): Boolean  = {
        curIdx += 1
        if (curIdx >= timeSeries.size) false
        else {
            // Get current time mark
            val curTime        = timeSeries(curIdx).Time

            // Propage each value change to the valueMap
            timeSeries(curIdx).ChangeList foreach { case ValueEntry(id, newVal) =>
                // Convert from signal ID to RTL reference name
                val sigRef = idToRefMap(id)

                // Construct the new timed value of the signal
                val newCurVal = Some(TimedValue(curTime, newVal))

                // Update valueMap
                if (valueMap.contains(sigRef)) {
                    val TimedValueHistory(prevVal, curVal) = valueMap(sigRef)
                    valueMap.update(sigRef, TimedValueHistory(curVal, newCurVal))
                } else {
                    valueMap.update(sigRef, TimedValueHistory(None, newCurVal))
                }
            }
            true
        }
    }

    def GetCurTime(): Int = {
        if (curIdx < 0) curIdx
        else if (curIdx >= timeSeries.size) -1
        else timeSeries(curIdx).Time
    }

    def SameAs(that: WaveForm): Boolean = that match {
        case w: VcdInMemWaveForm => w.vcdInMemContent == vcdInMemContent
        case _                   => false
    }

    def GetSigInfo(s: Signal): SigInfo = {
        if (curIdx < 0 || curIdx >= timeSeries.size) SigInfo(0, 0, -1)
        else if (!refToInfoMap.contains(s.FullPath)) SigInfo(0, 0, -1)
        else if (!valueMap.contains(s.FullPath)) SigInfo(0, 0, -1)
        else {
            val sigTime = timeSeries(curIdx).Time
            val VarInfo(_, sigWidth, _) = refToInfoMap(s.FullPath)
            val TimedValueHistory(_, curVal) = valueMap(s.FullPath)

            curVal match {
                case Some(t)    => SigInfo(MakeIntValue(t.Value), sigWidth, sigTime)
                case _          => SigInfo(0, 0, -1)
            }
        }
    }

    def GetSigHistory(s: Signal): SigHistory = {
        val curSigInfo = GetSigInfo(s)
        if (curSigInfo.Time < 0) SigHistory(None, None)
        else {
            val sigWidth = curSigInfo.Width
            val TimedValueHistory(prevVal, _) = valueMap(s.FullPath)

            prevVal match {
                case Some(t)    => {
                    val pV = SigInfo(MakeIntValue(t.Value), sigWidth, t.Time)
                    SigHistory(Some(pV), Some(curSigInfo))
                }
                case _          => SigHistory(None, Some(curSigInfo))
            }
        }
    }

    def GetSigWidth(s: Signal): Int = {
        if (!refToInfoMap.contains(s.FullPath)) 0
        else {
            val VarInfo(_, sigWidth, _) = refToInfoMap(s.FullPath)
            sigWidth
        }
    }

    def VcdInit() : Unit = {
        // Open VCD file
        val fileContent = vcdInMemContent

        // Parse file content and fill in internal data structures
        VcdLexer(fileContent) match {
            case Left(x)            => throw new Exception(x.toString())
            case Right(tokenList)   => {

                var scope: Option[VcdScope] = None
                var curTime: Int = -1
                var enValueChange: Boolean = true
                val valueChanges = mutable.ArrayBuffer[ValueEntry]()

                tokenList foreach { case t =>
                    t match {
                        case ScopeTok(_, id) => scope = Some(VcdScope(id, scope))
                        case VarTok(kind, width, id, ref) => {
                            val varFullPath = scope match {
                                case Some(s) => s.NameVar(ref)
                                case _       => ref
                            }
                            idToRefMap.update(id, varFullPath)
                            refToInfoMap.update(varFullPath, VarInfo(kind, width, id))
                        }
                        case UpscopeTok() => scope.get.parent
                        case DumpOffTok() => enValueChange = false
                        case DumpOnTok()  => enValueChange = true
                        case DumpVarsTok() => enValueChange = true
                        case EndTok() => enValueChange = true
                        case TimeMarkerTok(t) => {
                            if (curTime >= 0) {
                                timeSeries += TimeSeriesEntry(curTime, valueChanges.toList)
                                valueChanges.clear()
                            }
                            curTime = t
                        }
                        case ValueTok(v, id) => {
                            if (enValueChange) {
                                valueChanges += ValueEntry(id, v)
                            }
                        }
                        case TimescaleTok(t, u) => vcdTimescale = Some((t, u))
                        case VersionTok(v) => vcdVersion = Some(v)

                        case _ => ()

                    }
                }

                if (!valueChanges.isEmpty) {
                    timeSeries += TimeSeriesEntry(curTime, valueChanges.toList)
                }
            }
        }
    }

    private def MakeIntValue(strVal: String): Int =  {
        val xzRemoved = strVal.replaceAll("(x|X|z|Z)", "0")

        val binNumPat   = "b([01]+)".r

        binNumPat.findFirstMatchIn(xzRemoved) match {
            case Some(v)    => Integer.parseInt(v.group(1), 2)
            case _          => 0
        }
    }
}
