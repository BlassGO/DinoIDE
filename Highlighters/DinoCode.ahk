#Include %A_LineFile%\..\Util.ahk

FormatDinoStr(str, Byref RTF, Byref Settings, Byref Map, is_block:=false) {
	static Delimiter:=Chr(34),Escape:="\",Deref:=Chr(4), Languages := {"DinoCode": Func("HighlightDino")}
	, regex_expr:="\$\(((?:[^\" . Delimiter . "\(\)]+|([\" . Delimiter . "]).*?\2|\(([^\(\)]+|(?1))*\)|(?R))+)\)"
	orig:=str, _result:=[], _escape:=[], _evaluated:=[]
	_pos:=1, _extra:=0
	while,(_pos:=InStr(str,Escape,,_pos+_extra)) {
		if (_end:=Substr(str,_pos+1,1)) {
				_escape.Push(Escape . _end), _max:=_escape.MaxIndex()
			_max:=Deref . "&_" . _max . "_&" . Deref,str:=RegExReplace(str,".{2}",_max,,1,_pos), _extra:=StrLen(_max)
		} else {
			break
		}
	}
	_pos:=1, _extra:=0
	while,(_pos:=RegExMatch(str,"O)" . regex_expr,_char,_pos+_extra))
		_result.Push(solve_escape(_char.Value(),_escape)), _max:=_result.MaxIndex(), _max:=Deref . "``_" . _max . "_``" . Deref, str:=RegExReplace(str,".{" . _char.Len() . "}",_max,,1,_pos), _extra:=StrLen(_max)
	_pos:=1, _extra:=0
	while,(_pos:=InStr(str,"%",,_pos+_extra))
	{
		if (_end:=InStr(str,"%",,_pos+1)) {
			_eval:=Substr(str,_pos,(_end-_pos)+1)
			solve_escape(_eval,_escape), solve_escape(_eval,_result,"``")
			_evaluated.Push(_eval), _eval_rpl:=Deref . "~_" . _evaluated.MaxIndex() . "_~" . Deref
			str:=RegExReplace(str,".{" . (_end-_pos)+1 . "}",_eval_rpl,,1,_pos), _extra:=StrLen(_eval_rpl)
		} else {
			break
		}
	}
	_rex:="\x04([``&~])_(\d+)_[``&~]\x04", _origlen:=StrLen(str)
	if is_block {
		_lastexpr:=1
		while,(_pos2:=RegExMatch(str,_rex,_2char,_lastexpr))
		{
			(_before:=SubStr(str,_lastexpr,_pos2-_lastexpr)) ? (RTF .= "\cf" Map.Strings . " " . EscapeRTF(_before))
			switch (_2char1) {
				case "&":
					RTF .= "\cf" Map.Punctuation " " EscapeRTF(_escape[_2char2])
				case "~":
					RTF .= Languages.DinoCode.Call(Settings, _evaluated[_2char2], True)
				case "``":
					RTF .= "\cf" Map.Strings " " EscapeRTF(_result[_2char2])
			}
			_lastexpr:=_pos2+StrLen(_2char)
		}
		RTF .= "\cf" Map.Strings
		if (_lastexpr=1) {
			RTF .= " " EscapeRTF(str)
		} else if (_lastexpr<_origlen) {
			RTF .= " " EscapeRTF(SubStr(str,_lastexpr))
		}
	} else {
		_pos:=1, _len:=1
		while,(_pos:=RegExMatch(str,"O)([^""]*)(""[^""]*"")([^""]*)",_char,_pos)) {
			if (_char.Value(1)!="") {
				RTF .= Languages.DinoCode.Call(Settings, solve_escape(solve_escape(solve_escape(_char.Value(1),_evaluated,"~"), _result, "``"), _escape), True)
			}
			if (_char.Value(2)!="") {
				_lastexpr:=1
				while,(_pos2:=RegExMatch(_char.Value(2),_rex,_2char,_lastexpr))
				{
					(_before:=SubStr(_char.Value(2),_lastexpr,_pos2-_lastexpr)) ? (RTF .= "\cf" Map.Strings . " " . EscapeRTF(_before))
					switch (_2char1) {
						case "&":
							RTF .= "\cf" Map.Punctuation " " EscapeRTF(_escape[_2char2])
						case "``":
							RTF .= Languages.DinoCode.Call(Settings, _result[_2char2], True)
						case "~":
							RTF .= Languages.DinoCode.Call(Settings, _evaluated[_2char2], True)
					}
					_lastexpr:=_pos2+StrLen(_2char)
				}
				RTF .= "\cf" Map.Strings
				if (_lastexpr=1) {
					RTF .= " " EscapeRTF(_char.Value(2))
				} else if (_lastexpr<=_char.Len(2)) {
					RTF .= " " EscapeRTF(SubStr(_char.Value(2),_lastexpr))
				}
			}
			if (_char.Value(3)!="") {
				RTF .= Languages.DinoCode.Call(Settings, solve_escape(solve_escape(solve_escape(_char.Value(3),_evaluated,"~"), _result, "``"), _escape), True)
			}
			_len:=_pos+=_char.Len()
		}
		if (_len=1) {
			RTF .= " " . EscapeRTF(orig)
		} else if (_len<_origlen) {
			RTF .= " " . EscapeRTF(solve_escape(solve_escape(solve_escape(SubStr(str, _len),_evaluated,"~"), _result, "``"), _escape))
		}
	}
}
HighlightDino(Settings, ByRef Code, Bare:=False)
{
	static Flow := "if|else|for|until|while|use|global|section|sleep|set|exit|si|sino|mientras|hasta|usar|para|seccion|dormir|salir"
	, Connectors := "in|with|to|unto|en|hacia|con"
	, Builtins := "clipboard|comspec|errorlevel|false|true|falso|verdadero|programfiles"
	, Signals := "break|return|continue|romper|retornar|continuar"
	, Opers := "not|and|or"
	, Needle := "
	( LTrim Join Comments
		ODims)
		((?:^|[ \t]*)\K#[^\n]+)          ; Comments
		|(^:\w+\b)                       ; TAGs
		|(^[ \t]*\>\s*\w+\s*(?:\n|$))    ; Literal Redirection
		|([+*!~&\/\\<>^|=?:
			```%{}\[\]\-]+)              ; Punctuation
		|\b(0x[0-9a-fA-F]+|[0-9]+)       ; Numbers
		|(""[^\n]*)                      ; Start String
		|\b(A_\w*|" Builtins ")\b        ; Builtins
		|\b(" Flow ")\b                  ; Flow
		|^\s*(" Signals ")\b             ; Signals
		|\b(" Opers ")\b                 ; Opers
		|((?:[a-zA-Z_]+(?=\())|(?:^[ \t]*\b[a-zA-Z_]+(?![+-]{2}|\.)\b|\b(?:" Connectors ")\b|(?<=\$\()\s*\w+))      ; Functions
		|(\$|\w+)                        ; Variables
		|(([a-zA-Z_$]+)(?=\())           ; Functions
	)"
	, Languages := {"DinoCode": Func("HighlightDino")}
	GenHighlighterCache(Settings)
	Map := Settings.Cache.ColorMap
	Pos := 1
	while (FoundPos := RegExMatch(Code, Needle, Match, Pos))
	{
		RTF .= "\cf" Map.Plain " "
		RTF .= EscapeRTF(SubStr(Code, Pos, FoundPos-Pos))
		
		; Flat block of if statements for performance
		if (Match.Value(1) != "") {
			(SubStr(Match.Value(),2,1)="*") ? multiline:=true
			RTF .= "\cf" Map.Comments
		} else if (Match.Value(2) != "")
			RTF .= "\cf" ((Match.Value(2)=":main") ? Map.TAGMAIN : Map.TAGs)
		else if (Match.Value(3) != "") {
			RegExMatch(Match[3], "ODims)^([ \t]*)\>\s*(?=(.*))", Cont)
			indent:=StrLen(StrReplace(Cont.Value(1), A_Tab, "    "))
			RTF .= "\cf" . Map.Punctuation . " " . EscapeRTF(Cont.Value())
			RTF .= "\cf" . Map.Variables . " " . EscapeRTF(Cont.Value(2))
			already:=true
		} else if (Match.Value(4) != "")
			RTF .= "\cf" Map.Punctuation
		else if (Match.Value(5) != "")
			RTF .= "\cf" Map.Numbers
		else if (Match.Value(6) != "")
			is_string:=true
		else if (Match.Value(7) != "")
			RTF .= "\cf" Map.Builtins
		else if (Match.Value(8) != "")
			RTF .= "\cf" Map.Flow
		else if (Match.Value(9) != "")
			RTF .= "\cf" Map.Signals
		else if (Match.Value(10) != "")
			RTF .= "\cf" Map.Opers
		else if (Match.Value(11) != "")
			RTF .= "\cf" Map.Functions
		else if (Match.Value(12) != "")
			RTF .= "\cf" Map.Variables
		else if (Match.Value(13) != "")
			RTF .= "\cf" Map.Functions
		else
			RTF .= "\cf" Map.Plain
		if already {
			already:=false
		} else if is_string {
			FormatDinoStr(Match.Value(), RTF, Settings, Map)
			is_string:=false
		} else {
			RTF .= " " . EscapeRTF(Match.Value())
		}
		Pos := FoundPos + Match.Len()
		if multiline {
			RegExMatch(Code, "ODims).*\*#", Match, Pos)
			RTF .= "\cf" Map.Comments
			RTF .= " " EscapeRTF(Match.Value())
			Pos+=Match.Len()
			multiline:=false
		} else if (indent!="") {
			while BlockPos := RegExMatch(Code, "ODims)^([ \t]*)[^\n]*(\n)", BlockLine, Pos)
			{
				if (StrLen(StrReplace(BlockLine.Value(1), A_Tab, "    "))>indent) {
					FormatDinoStr(BlockLine.Value(), RTF, Settings, Map, true)
					Pos+=BlockLine.Len()
				} else {
					break
				}
			}
			indent:=""
		}
	}
	
	if Bare
		return RTF . "\cf" Map.Plain " " EscapeRTF(SubStr(Code, Pos))
	
	return Settings.Cache.RTFHeader . RTF
	. "\cf" Map.Plain " " EscapeRTF(SubStr(Code, Pos)) "\`n}"
}
