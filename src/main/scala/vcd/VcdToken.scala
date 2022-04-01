package aha
package wavereplay
package vcd

import scala.util.parsing.input.Positional

sealed trait VcdToken extends Positional

case class EndTok() extends VcdToken
case class DateTok(date: String) extends VcdToken
case class VersionTok(version: String) extends VcdToken
case class TimescaleTok(number: Int, unit: String) extends VcdToken
case class ScopeTok(kind: String, id: String) extends VcdToken
case class VarTok(kind: String, width: Int, id: String, ref: String) extends VcdToken
case class UpscopeTok() extends VcdToken
case class EndDefTok() extends VcdToken
case class CommentTok(comment: String) extends VcdToken
case class TimeMarkerTok(time: Long) extends VcdToken
case class DumpVarsTok() extends VcdToken
case class DumpOffTok() extends VcdToken
case class DumpOnTok() extends VcdToken
case class ValueTok(value: String, id: String) extends VcdToken
