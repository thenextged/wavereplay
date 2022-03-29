package aha
package wavereplay
package vcd

import scala.util.parsing.combinator.RegexParsers


case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
}

case class VcdLexerError(location: Location, msg: String)

object VcdLexer extends RegexParsers {

    val KW_END              = "$end"
    val KW_DATE             = "$date"
    val KW_VERSION          = "$version"
    val KW_TIMESCALE        = "$timescale"
    val KW_SCOPE            = "$scope"
    val KW_VAR              = "$var"
    val KW_UPSCOPE          = "$upscope"
    val KW_ENDDEFS          = "$enddefinitions"
    val KW_COMMENT          = "$comment"
    val KW_DUMPVARS         = "$dumpvars"
    val KW_DUMPALL          = "$dumpall"
    val KW_DUMPOFF          = "$dumpoff"
    val KW_DUMPON           = "$dumpon"

    val TEXT_STRING         = """[^("$")]*""".r
    val TIME_NUMBER         = """(1|10|100)""".r
    val TIME_UNIT           = """(s|ms|us|ns|ps|fs)""".r

    val SCOPE_KIND          = """(begin|fork|function|module|task)""".r
    val SCOPE_IDENTIFIER    = """[a-zA-Z_][a-zA-Z_0-9\(\)]*""".r

    val DECIMAL_NUM         = """[0-9]+""".r
    val VAR_KIND            = """(event|integer|parameter|realtime|real|reg|supply0|supply1|time|triand|trior|trireg|tri0|tr1|tri|wand|wire|wor)""".r
    val VAR_ID_CODE         = """[a-zA-Z_0-9!/\,\.@':~#\*\(\)\+\{\}\$\%\[\]`\"&;<>=\?\-\^\(\)\|\\]+""".r

    val SCALAR_VAL          = """(0|1|x|X|z|Z)""".r
    val BINARY_VAL          = """(b|B)(0|1|x|X|z|Z)+""".r
    val REAL_VALUE          = """(r|R)[0-9]+(\.[0-9]+)?""".r

    override def skipWhitespace = true
    override val whiteSpace = "[ \t\r\f\n]+".r

    def apply(vcdContent: String): Either[VcdLexerError, List[VcdToken]] = {
        parse(tokens, vcdContent) match {
            case Success(result, _)   => Right(result)
            case NoSuccess(msg, next) => Left(VcdLexerError(Location(next.pos.line, next.pos.column), msg))
            case Error(msg, next)     => Left(VcdLexerError(Location(next.pos.line, next.pos.column), msg))
            case Failure(msg, next)   => Left(VcdLexerError(Location(next.pos.line, next.pos.column), msg))
        }
    }

    def tokens: Parser[List[VcdToken]] = {
        phrase(rep1(Date | Version | Timescale | Scope | Var | Upscope | EndDef |
                    Comment | TimeMarker | DumpVars | DumpOff | DumpOn | Value | End
                   )
              )
    }

    def End: Parser[EndTok] = positioned {
        KW_END ^^ { x => EndTok() }
    }

    def Date: Parser[DateTok] = positioned {
        KW_DATE ~> TEXT_STRING <~ End ^^ { case d => DateTok(d.trim) }
    }

    def Version: Parser[VersionTok] = positioned {
        KW_VERSION ~> TEXT_STRING <~ End ^^ { case v => VersionTok(v.trim) }
    }

    def Timescale: Parser[TimescaleTok] = positioned {
        KW_TIMESCALE ~> TIME_NUMBER ~ TIME_UNIT <~ End ^^ {
            case n ~ u => TimescaleTok(n.trim.toInt, u.trim)
        }
    }

    def Scope: Parser[ScopeTok] = positioned {
        KW_SCOPE ~> SCOPE_KIND ~ SCOPE_IDENTIFIER <~ End ^^ { case k ~ id =>
            ScopeTok(k.trim, id.trim)
        }
    }

    def Var: Parser[VarTok] = positioned {
        KW_VAR ~> VAR_KIND ~ DECIMAL_NUM ~ VAR_ID_CODE ~ SCOPE_IDENTIFIER <~ End ^^ {
            case k ~ w ~ id ~ r => VarTok(k.trim, w.trim.toInt, id.trim, r.trim)
        }
    }

    def Upscope: Parser[UpscopeTok] = positioned {
        KW_UPSCOPE ~ End ^^ { case x => UpscopeTok() }
    }

    def EndDef: Parser[EndDefTok] = positioned {
        KW_ENDDEFS ~ End ^^ { case x => EndDefTok() }
    }

    def Comment: Parser[CommentTok] = positioned {
        KW_COMMENT ~> TEXT_STRING <~ End ^^ { case c => CommentTok(c.trim) }
    }

    def TimeMarker: Parser[TimeMarkerTok] = positioned {
        "#" ~> DECIMAL_NUM ^^ { case t => TimeMarkerTok(t.trim.toInt) }
    }

    def DumpVars: Parser[DumpVarsTok] = positioned {
        KW_DUMPVARS ^^ { case d => DumpVarsTok() }
    }

    def DumpOff: Parser[DumpOffTok] = positioned {
        KW_DUMPOFF ^^ { case d => DumpOffTok() }
    }

    def DumpOn: Parser[DumpOnTok] = positioned {
        KW_DUMPON ^^ { case d => DumpOnTok() }
    }

    def Value: Parser[ValueTok] = ScalarValue | BinaryValue | RealValue

    def ScalarValue: Parser[ValueTok] = positioned {
        SCALAR_VAL ~ VAR_ID_CODE ^^ { case v ~ id =>
            ValueTok("b" + v.trim.toLowerCase, id.trim)
        }
    }

    def BinaryValue: Parser[ValueTok] = positioned {
        BINARY_VAL ~ VAR_ID_CODE ^^ { case v ~ id =>
            ValueTok(v.trim.toLowerCase, id.trim)
        }
    }

    def RealValue: Parser[ValueTok] = positioned {
        REAL_VALUE ~ VAR_ID_CODE ^^ { case v ~ id =>
            ValueTok(v.trim.toLowerCase, id.trim)
        }
    }

}
