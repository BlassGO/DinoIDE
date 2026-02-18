#Include %A_LineFile%\..\Util.ahk

GetDinoRegex(Byref Constants) {
    return "
        ( LTrim Join Comments
            ODims)
            ((?:^|[ \t]*)\K(?:#\*.*?\*#|#[^\n]+))       ; 1. Comments
            |(^[ \t]*:{1,2}[\p{L}_$][\p{L}0-9_$]*)      ; 2. TAGs
            |(^[ \t]*\>[^\n]+\n)                        ; 3. Literal Redirection
            |([+*!~&\/\\<>^|=?:```%\-]+)                ; 4. Punctuation
            |\b(0x[0-9a-fA-F][0-9a-fA-F_]+
               |(?:[0-9][0-9_]*(?:\.[0-9][0-9_]+)?[eE][+-]?[0-9_]+)
               |(?:0b[01][01_]*)
               |(?:[0-9][0-9_]*n)
               |(?:[0-9][0-9_]*(?:\.[0-9][0-9_]+)?))    ; 5. Numbers
            |(""[^""]*"")                               ; 6. Double-quoted String
            |('[^']*')                                  ; 7. Single-quoted String
            |\b(" Constants.Builtins ")\b               ; 8. Builtins
            |\b(" Constants.Flow ")\b                   ; 9. Flow
            |^\s*(" Constants.Signals ")\b              ; 10. Signals
            |\b(" Constants.Opers ")\b                  ; 11. Opers
            |(^[ \t]*[\p{L}_$][\p{L}0-9_$]*(?=\s*(?:[+*\\^?\-\.]{0,2}=|<-))) ; 12. Variable Assignment
            |((?:[a-zA-Z_][a-zA-Z0-9_$]*(?=\())|(?:^[ \t]*\b[\p{L}_][\p{L}0-9_$]*(?![+-]{2}|\s*=|\.)\b|\b(?:" Constants.Connectors ")\b|(?<=\$\()\s*[\p{L}_][\p{L}0-9_$]*)) ; 13. Functions
            |([\p{L}_$][\p{L}0-9_$]*)                   ; 14. Variables
        )"
}

InitHighlightDino() {
    static IsInitialized := false, Constants
    if (!IsInitialized) {
        Constants :=
        (join
            {
                "Connectors": "in",
                "ConnectorsRegex": "\b(?:in)\b",
                "Builtins": "false|true|none|self",
                "Signals": "break|return|continue",
                "Flow": "if|elif|else|for|until|while|use|global|exit",
                "Opers": "not|and|or"
            }
        )
        Constants.Needle := GetDinoRegex(Constants)
        IsInitialized := true
    }
    return Constants
}

FormatDinoStr(str := "", ByRef RTF := "", ByRef Settings := "", ByRef Map := "", is_block := false, update := "") {
    static StrConstants := {Delimiter: Chr(34), Escape: Chr(96)}, Languages := {"DinoCode": Func("HighlightDino")}
    local _pos := 1, _strLen := StrLen(str), _prevPos := 1, _braceLevel, _parenLevel, _scanPos, _ch, _quoteChar

    if update {
        for var, val in update
            StrConstants[var] := val
        return
    }

    while (_pos <= _strLen) {
        _escPos := InStr(str, StrConstants.Escape,, _pos)
        _dollarPos := InStr(str, "$",, _pos)

        if (!_escPos && !_dollarPos) {
            if (_pos <= _strLen)
                RTF .= "\cf" Map.Strings " " EscapeRTF(SubStr(str, _prevPos))
            break
        }

        _nextPos := (!_escPos || (_dollarPos && _dollarPos < _escPos)) ? _dollarPos : _escPos

        if (_nextPos > _prevPos)
            RTF .= "\cf" Map.Strings " " EscapeRTF(SubStr(str, _prevPos, _nextPos - _prevPos))

        if (_nextPos = _escPos) {
            _char := SubStr(str, _escPos + 1, 1)
            RTF .= "\cf" Map.Punctuation " " EscapeRTF(StrConstants.Escape . _char)
            _pos := _escPos + 2
        } else {
            _pos := _dollarPos
            _nextChar := SubStr(str, _pos + 1, 1)

            if (_nextChar = "{") {
                ; Buscar cierre de ${...}
                _braceLevel := 1
                _scanPos := _pos + 2
                while (_scanPos <= _strLen && _braceLevel > 0) {
                    _ch := SubStr(str, _scanPos, 1)
                    if (_ch = "{") {
                        _braceLevel++
                    } else if (_ch = "}") {
                        _braceLevel--
                    } else if (_ch = StrConstants.Delimiter || _ch = "'") {
                        _quoteChar := _ch
                        _nextQuote := InStr(str, _quoteChar,, _scanPos + 1)
                        if (!_nextQuote) {
                            _braceLevel := 0  ; Unclosed quote, treat as error
                            break
                        }
                        _scanPos := _nextQuote
                    }
                    _scanPos++
                }

                if (_braceLevel = 0 && _scanPos <= _strLen + 1 && SubStr(str, _scanPos - 1, 1) = "}") {
                    _end := _scanPos - 1
                    _expr := SubStr(str, _pos + 2, _end - (_pos + 2))
                    RTF .= "\cf" Map.Variables " " EscapeRTF("$")
                    RTF .= "\cf" Map.Punctuation " " EscapeRTF("{")
                    RTF .= Languages.DinoCode.Call(Settings, _expr, True, False, True)
                    RTF .= "\cf" Map.Punctuation " " EscapeRTF("}")
                    _pos := _end + 1
                } else {
                    ; Not closed correctly: paint ${ in red as warning
                    RTF .= "\cf" Map.Errors " " EscapeRTF("${")
                    _pos += 2
                }
            } else if (_nextChar = "(") {
                ; Find closing )
                _parenLevel := 1
                _scanPos := _pos + 2
                while (_scanPos <= _strLen && _parenLevel > 0) {
                    _ch := SubStr(str, _scanPos, 1)
                    if (_ch = "(") {
                        _parenLevel++
                    } else if (_ch = ")") {
                        _parenLevel--
                    } else if (_ch = StrConstants.Delimiter || _ch = "'") {
                        _quoteChar := _ch
                        _nextQuote := InStr(str, _quoteChar,, _scanPos + 1)
                        if (!_nextQuote) {
                            _parenLevel := 0  ; Unclosed quote, treat as error
                            break
                        }
                        _scanPos := _nextQuote
                    }
                    _scanPos++
                }

                if (_parenLevel = 0 && _scanPos <= _strLen + 1 && SubStr(str, _scanPos - 1, 1) = ")") {
                    _end := _scanPos - 1
                    _expr := SubStr(str, _pos + 2, _end - (_pos + 2))
                    RTF .= "\cf" Map.Variables " " EscapeRTF("$")
                    RTF .= "\cf" Map.Punctuation " " EscapeRTF("(")
                    RTF .= Languages.DinoCode.Call(Settings, _expr, True)
                    RTF .= "\cf" Map.Punctuation " " EscapeRTF(")")
                    _pos := _end + 1
                } else {
                    ; Not closed correctly: paint $( in red as warning
                    RTF .= "\cf" Map.Errors " " EscapeRTF("$(")
                    _pos += 2
                }
            } else if RegExMatch(SubStr(str, _pos + 1), "^[\p{L}_][\p{L}0-9_$]*", _match) {
                RTF .= "\cf" Map.Variables " " EscapeRTF("$" . _match)
                _pos += 1 + StrLen(_match)
            } else {
                RTF .= "\cf" Map.Strings " " EscapeRTF("$")
                _pos++
            }
        }

        _prevPos := _pos
    }
}


FormatDinoTAG(str:="", Byref RTF:="", Byref Settings:="", Byref Map:="", Byref Constants:="") {
    local Update := false
    if (_def:=InStr(str,"->")) {
        ; Split into tag+args (before ->) and attributes (after ->)
        tagPart:=SubStr(str,1,_def-1), _cdef:=SubStr(str,_def+2), _cdeflen:=StrLen(_cdef)
        
        ; Process tagPart (tag and arguments)
        if (tagPart!="") {
            ; Match the tag
            if (RegExMatch(tagPart, "^(:[\p{L}_][\p{L}0-9_$]*)", tagMatch)) {
                ; Color the tag
                RTF .= "\cf" ((SubStr(tagMatch1,1,6)=":main") ? Map.TAGMAIN : Map.TAGs) . " " . EscapeRTF(tagMatch1)
                ; Color the rest of tagPart as arguments with Map.Variables
                args := SubStr(tagPart, StrLen(tagMatch1) + 1)
                if (args != "") {
                    RTF .= "\cf" Map.Variables . " " . EscapeRTF(args)
                }
            } else {
                ; Fallback: treat entire tagPart as tag
                RTF .= "\cf" ((SubStr(tagPart,1,6)=":main") ? Map.TAGMAIN : Map.TAGs) . " " . EscapeRTF(tagPart)
            }
        }
        ; Color the -> symbol
        RTF .= "\cf" Map.Signals . " " . EscapeRTF("->")
        ; Existing attribute parsing logic
        _last:=_pos:=1
        while,(_pos:=RegexMatch(_cdef,"(\s*[a-zA-Z_$][a-zA-Z0-9_$]*)(?:([\s=]+)(?:($$ \s*)((?:[^\s\, $$]+[\s\,]*)+)(\])))?(\s*(?:\,|$))",_with,_pos)) {
            if (A_Index=1&&_pos>1)||(_last<_pos) {
                _last:=(_last<_pos)?_last:1
                break
            }
            RTF .= "\cf" Map.Functions . " " . EscapeRTF(_with1)
            Constants.Connectors .= "|\Q" . Trim(_with1) . "\E", Update := true
            if _with2 {
                RTF .= "\cf" Map.Signals . " " . EscapeRTF(_with2)
                if _with3 {
                    RTF .= "\cf" Map.Signals . " " . EscapeRTF(_with3)
                    if _with4 {
                        Loop, parse, _with4, `,
                        {
                            (A_Index>1) ? RTF .= "\cf" Map.Plain . " " . EscapeRTF(",")
                            if A_LoopField {
                                if (_at:=InStr(A_LoopField,"="))&&(_prop:=SubStr(A_LoopField,1,_at-1)) {
                                    RTF .= "\cf" Map.Variables . " " . EscapeRTF(_prop)
                                    RTF .= "\cf" Map.Signals . " " . EscapeRTF("=")
                                    RTF .= "\cf" Map.Numbers . " " . EscapeRTF(SubStr(A_LoopField,_at+1))
                                } else {
                                    Constants.Connectors .= "|\Q" . Trim(A_LoopField) . "\E"
                                    RTF .= "\cf" Map.Strings . " " . EscapeRTF(A_LoopField)
                                    Update := true
                                }
                            }
                        }
                    }
                    if _with5 {
                        RTF .= "\cf" Map.Signals . " " . EscapeRTF(_with5)
                    }
                }
            }
            if _with6 {
                RTF .= "\cf" Map.Plain . " " . EscapeRTF(_with6)
            }
            _last:=_pos+=StrLen(_with)
        }
        if (_last<_cdeflen) {
            RTF .= "\cf" Map.Plain . " " . EscapeRTF(SubStr(_cdef, _last))
        }
    } else {
        ; No ->, process tag and arguments
        ; Match the tag
        if (RegExMatch(str, "^(:[\p{L}_][\p{L}0-9_$]*)", tagMatch)) {
            ; Color the tag
            RTF .= "\cf" ((SubStr(tagMatch1,1,6)=":main") ? Map.TAGMAIN : Map.TAGs) . " " . EscapeRTF(tagMatch1)
            ; Color the rest as arguments with Map.Variables
            args := SubStr(str, StrLen(tagMatch1) + 1)
            if (args != "") {
                RTF .= "\cf" Map.Variables . " " . EscapeRTF(args)
            }
        } else {
            ; Fallback: treat entire string as tag
            RTF .= "\cf" ((SubStr(str,1,6)=":main") ? Map.TAGMAIN : Map.TAGs) . " " . EscapeRTF(str)
        }
    }
    if Update
        Constants.Needle := GetDinoRegex(Constants), Constants.ConnectorsRegex := "\b(?:" . Constants.Connectors . ")\b"
}

HighlightDino(Settings:="", ByRef Code:="", Bare:=False, reset:=false, isBraceExpr:=false) {
    static Constants := InitHighlightDino(), Languages := {"DinoCode": Func("HighlightDino")}
    local RTF := "", Map, Pos := 1, FoundPos, GroupingLevel := 0, Continuation := 0
    , ParenLevel := 0, BraceLevel := 0, BracketLevel := 0, isExprLevel := []
    , Match, PreText, indent := "", multiline := false, is_string := false, already := false
    , _expr := 0, _con := 0
    
    if reset {
        GenHighlighterCache(Settings)
        Constants := InitHighlightDino()
        return
    }
    
    GenHighlighterCache(Settings)
    Map := Settings.Cache.ColorMap
    
    while (FoundPos := RegExMatch(Code, Constants.Needle, Match, Pos)) {
        if (FoundPos - Pos > 0) {
            PreText := SubStr(Code, Pos, FoundPos - Pos)
            StrReplace(PreText, "(", "", ParenOpen)
            StrReplace(PreText, ")", "", ParenClose)
            StrReplace(PreText, "{", "", BraceOpen)
            StrReplace(PreText, "}", "", BraceClose)
            StrReplace(PreText, "[", "", BracketOpen)
            StrReplace(PreText, "]", "", BracketClose)
            
            ParenLevel += ParenOpen
            if (ParenClose > 0 && ParenLevel > 0)
                ParenLevel := Max(0, ParenLevel - ParenClose), isExprLevel[ParenLevel] ? (isExprLevel[ParenLevel] := 0) : 0
            BraceLevel += BraceOpen
            if (BraceClose > 0 && BraceLevel > 0)
                BraceLevel := Max(0, BraceLevel - BraceClose)
            BracketLevel += BracketOpen
            if (BracketClose > 0 && BracketLevel > 0)
                BracketLevel := Max(0, BracketLevel - BracketClose)
            
            GroupingLevel := ParenLevel + BraceLevel + BracketLevel
            Continuation := Continuation || (RegExMatch(PreText, "ODims)[\n\r\t ]*[.,]") > 0)
            RTF .= "\cf" Map.Plain " " EscapeRTF(PreText)
        }

        (InStr(PreText, ";") || (Match.Value(13) = "")) ? (Continuation := 0) : 0
        
        if (Match.Value(1) != "")
            RTF .= "\cf" Map.Comments
        else if (Match.Value(2) != "") {
            FormatDinoTAG(Match.Value(2), RTF, Settings, Map, Constants), already := true
        } else if (Match.Value(3) != "") {
            if RegExMatch(Match.Value(3), "O)^(\s*\>)([^\n]+)", indent) {
                RTF .= "\cf" Map.Punctuation " " EscapeRTF(indent.Value(1))
                if (indent.Value(2) != "")
                {
                    RTF .= Languages.DinoCode.Call(Settings, indent.Value(2), True, False, True)
                    RTF .= "\cf" Map.Plain " " EscapeRTF("`n")
                }
                indent := StrLen(StrReplace(indent.Value(1), A_Tab, "    ")) - 1
                already := true
            }
        } else if (Match.Value(4) != "") {
            switch (Match.Value(4)) {
                case "->",  "<-", "=>":
                    RTF .= "\cf" Map.Punctuation   ; Same for the moment
                default:
                    RTF .= "\cf" Map.Punctuation
            }
            if (Match.Value(4) != "++" && Match.Value(4) != "--")
                Continuation := (RegExMatch(Code, "ODims)\s*[\r\n]",, FoundPos + Match.Len()) > 0)
        } else if (Match.Value(5) != "") {
            RTF .= "\cf" Map.Numbers
        } else if (Match.Value(6) != "") {
            is_string := true
            FormatDinoStr(Match.Value(), RTF, Settings, Map)
        } else if (Match.Value(7) != "") {
            RTF .= "\cf" Map.Strings
        } else if (Match.Value(8) != "") {
            RTF .= "\cf" Map.Builtins
        } else if (Match.Value(9) != "") {
            RTF .= "\cf" Map.Flow
        } else if (Match.Value(10) != "") {
            RTF .= "\cf" Map.Signals
            switch (Match.Value(10)) {
                case "escape":
                    if (_escape := Trim(SubStr(Code, FoundPos + Match.Len(), 2), "`r`n`t ")) && (StrLen(_escape) = 1)
                        FormatDinoStr(,,,,, {Escape: _escape})
                    else
                        FormatDinoStr(,,,,, {Escape: Chr(96)})
            }
        } else if (Match.Value(11) != "") {
            RTF .= "\cf" Map.Opers
            Continuation := (RegExMatch(Code, "ODims)\s*[\r\n]",, FoundPos + Match.Len()) > 0)
        } else if (Match.Value(12) != "") {
            RTF .= "\cf" Map.Variables
        } else if (Match.Value(13) != "") {
            if (_expr || (Substr(Code, FoundPos + Match.Len(), 1) = "("))
                RTF .= "\cf" Map.Functions, (_expr) ? (isExprLevel[ParenLevel] := 1) : (_expr := 1)
            else {
                _con := (GroupingLevel = 0 || isExprLevel[ParenLevel]) && RegExMatch(Match.Value(13), Constants.ConnectorsRegex)
                if (GroupingLevel = 0 && !Continuation)
                {
                    _explicit_operator := (RegExMatch(SubStr(Code, FoundPos + Match.Len(), 2), "ODi)^[.,+*!~&\/\\<^|=?```%\-]+") > 0)
                    _multi_line_operator := !_explicit_operator && (RegExMatch(SubStr(Code, FoundPos + Match.Len(), 200), "ODi)^\s*[\r\n]+\s*[.,+*!~&\/\\<^|=?```%\-]+") > 0)
                    RTF .= "\cf" ((isBraceExpr || _multi_line_operator || _explicit_operator) ? Map.Variables : Map.Functions)
                }
                else if _con
                    RTF .= "\cf" Map.Functions
                else
                    RTF .= "\cf" Map.Variables
            }
            Continuation := !_expr && _con
            _expr := _con := _multi_line_operator := 0
        } else if (Match.Value(14) != "") {
            _expr := (Substr(Code, FoundPos + Match.Len(), 1) = "(")
            RTF .= "\cf" Map.Variables
        } else {
            RTF .= "\cf" Map.Plain
        }
        
        if already
            already := false
        else if is_string
            is_string := false
        else
            RTF .= " " . EscapeRTF(Match.Value())
        
        Pos := FoundPos + Match.Len()
        
        if (indent != "") {
            while BlockPos := RegExMatch(Code, "ODims)^([ \t]*)[^\n]*(\n)", BlockLine, Pos) {
                if (StrLen(StrReplace(BlockLine.Value(1), A_Tab, "    ")) > indent) {
                    FormatDinoStr(BlockLine.Value(), RTF, Settings, Map, true)
                    Pos += BlockLine.Len()
                } else {
                    break
                }
            }
            indent := ""
        }
    }
    
    RTF .= "\cf" Map.Plain " " EscapeRTF(SubStr(Code, Pos))
    
    if Bare
        return RTF
    
    return Settings.Cache.RTFHeader . RTF . "\`n}"
}