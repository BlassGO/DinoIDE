#Include %A_LineFile%\..\Util.ahk

FormatDinoStr(str:="", Byref RTF:="", Byref Settings:="", Byref Map:="", is_block:=false, update:="") {
	static Delimiter:=Chr(34),Escape:="\",Deref:=Chr(4), Languages := {"DinoCode": Func("HighlightDino")}
	, regex_expr:="\$\(((?:[^\" . Delimiter . "\(\)]+|([\" . Delimiter . "]).*?\2|\(([^\(\)]+|(?1))*\)|(?R))+)\)"
	if isObject(update) {
		for var, val in update
			%var%:=val
		return
	}
	orig:=str, _result:=[], _escape:=[], _evaluated:=[]
	_pos:=1, _extra:=0
	while,(_pos:=InStr(str,Escape,,_pos+_extra)) {
		if (_end:=Substr(str,_pos+1,1)) {
				_escape.Push(Escape . _end), _max:=_escape.MaxIndex()
			_max:=Deref . "&" . _max . "&" . Deref,str:=RegExReplace(str,".{2}",_max,,1,_pos), _extra:=StrLen(_max)
		} else {
			break
		}
	}
	_pos:=1, _extra:=0
	while,(_pos:=RegExMatch(str,"O)" . regex_expr,_char,_pos+_extra))
		_result.Push(solve_escape(_char.Value(),_escape)), _max:=_result.MaxIndex(), _max:=Deref . "``" . _max . "``" . Deref, str:=RegExReplace(str,".{" . _char.Len() . "}",_max,,1,_pos), _extra:=StrLen(_max)
	_pos:=1, _extra:=0
	while,(_pos:=InStr(str,"%",,_pos+_extra))
	{
		if (_end:=InStr(str,"%",,_pos+1)) {
			_eval:=Substr(str,_pos,(_end-_pos)+1)
			solve_escape(_eval,_escape), solve_escape(_eval,_result,"``")
			_evaluated.Push(_eval), _eval_rpl:=Deref . "~" . _evaluated.MaxIndex() . "~" . Deref
			str:=RegExReplace(str,".{" . (_end-_pos)+1 . "}",_eval_rpl,,1,_pos), _extra:=StrLen(_eval_rpl)
		} else {
			break
		}
	}
	_rex:="\x04([``&~])(\d+)[``&~]\x04", _origlen:=StrLen(str)
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
FormatDinoTAG(str:="", Byref RTF:="", Byref Settings:="", Byref Map:="") {
	if (_def:=InStr(str,"->")) {
		_cdef:=SubStr(str,_def+2), tag:=SubStr(str,1,_def-1), ConnectorsNew:="", _cdeflen:=StrLen(_cdef)
		if (tag!="")
		   RTF .= "\cf" ((Trim(tag,"`r`n`t ")=":main") ? Map.TAGMAIN : Map.TAGs) . " " . EscapeRTF(tag)
		RTF .= "\cf" Map.Signals . " " . EscapeRTF("->")
		_last:=_pos:=1
		while,(_pos:=RegexMatch(_cdef,"(\s*[a-zA-Z_$][a-zA-Z0-9_$]*)(?:([\s=]+)(?:(\[\s*)((?:[^\s\,\]]+[\s\,]*)+)(\])))?(\s*(?:\,|$))",_with,_pos)) {
			if (A_Index=1&&_pos>1)||(_last<_pos) {
				_last:=(_last<_pos)?_last:1
				break
			}
			RTF .= "\cf" Map.Functions . " " . EscapeRTF(_with1)
			ConnectorsNew.="|\Q" . Trim(_with1) . "\E"
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
									ConnectorsNew.="|\Q" . Trim(A_LoopField) . "\E"
									RTF .= "\cf" Map.Strings . " " . EscapeRTF(A_LoopField)
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
		(ConnectorsNew!="") ? HighlightDino(,,,true, {Connectors: ConnectorsNew})
		if (_last<_cdeflen) {
			RTF .= "\cf" Map.Plain . " " . EscapeRTF(SubStr(_cdef, _last))
		}
	} else {
		RTF .= "\cf" ((Trim(str,"`r`n`t ")=":main") ? Map.TAGMAIN : Map.TAGs) . " " . EscapeRTF(str)
	}
}
HighlightDino(Settings:="", ByRef Code:="", Bare:=False, reset:=false, add:="")
{
	static Flow := "if|else|for|until|while|use|global|section|sleep|set|exit|si|sino|mientras|hasta|usar|para|seccion|dormir|salir"
	, Connectors := "in|with|to|unto|en|hacia|con"
	, Builtins := "clipboard|comspec|errorlevel|false|true|falso|verdadero|programfiles"
	, Signals := "break|return|continue|romper|retornar|continuar|escape"
	, Opers := "not|and|or"
	, Needle := "
	( LTrim Join Comments
		ODims)
		((?:^|[ \t]*)\K#[^\n]+)          ; Comments
		|(^:[^\n]+)                      ; TAGs
		|(^[ \t]*\>\s*\w+\s*(?:\n|$))    ; Literal Redirection
		|([+*!~&\/\\<>^|=?:
			```%{}\[\]\-]+)              ; Punctuation
		|\b(0x[0-9a-fA-F]+|[0-9]+)       ; Numbers
		|(""[^\n]*)                      ; Start String
		|\b(A_\w*|" Builtins ")\b        ; Builtins
		|\b(" Flow ")\b                  ; Flow
		|^\s*(" Signals ")\b             ; Signals
		|\b(" Opers ")\b                 ; Opers
		|((?:[a-zA-Z_][a-zA-Z0-9_$]*(?=\())|(?:^[ \t]*\b[a-zA-ZáéíóúÁÉÍÓÚüÜ_][\wáéíóúÁÉÍÓÚüÜ$]*(?![+-]{2}|\.)\b|\b(?:" Connectors ")\b|(?<=\$\()\s*[a-zA-ZáéíóúÁÉÍÓÚüÜ_][\wáéíóúÁÉÍÓÚüÜ$]*))      ; Functions
		|([a-zA-Z_$][a-zA-Z0-9_$]*)                        ; Variables
	)"
	, Languages := {"DinoCode": Func("HighlightDino")}
	if reset {
		if isObject(add) {
			for var, val in add
				%var%.=val
		} else {
			Flow := "if|else|for|until|while|use|global|section|sleep|set|exit|si|sino|mientras|hasta|usar|para|seccion|dormir|salir"
			, Connectors := "in|with|to|unto|en|hacia|con"
			, Builtins := "clipboard|comspec|errorlevel|false|true|falso|verdadero|programfiles"
			, Signals := "break|return|continue|romper|retornar|continuar|escape"
			, Opers := "not|and|or"
		}
		Needle := "
		( LTrim Join Comments
			ODims)
			((?:^|[ \t]*)\K#[^\n]+)          ; Comments
			|(^:[^\n]+)                      ; TAGs
			|(^[ \t]*\>[^\n]+(?:\n|$))    	 ; Literal Redirection
			|([+*!~&\/\\<>^|=?:
				```%{}\[\]\-]+)              ; Punctuation
			|\b(0x[0-9a-fA-F]+|[0-9]+)       ; Numbers
			|(""[^\n]*)                      ; Start String
			|\b(A_\w*|" Builtins ")\b        ; Builtins
			|\b(" Flow ")\b                  ; Flow
			|^\s*(" Signals ")\b             ; Signals
			|\b(" Opers ")\b                 ; Opers
			|((?:[a-zA-Z_][a-zA-Z0-9_$]*(?=\())|(?:^[ \t]*\b[a-zA-ZáéíóúÁÉÍÓÚüÜ_][\wáéíóúÁÉÍÓÚüÜ$]*(?![+-]{2}|\.)\b|\b(?:" Connectors ")\b|(?<=\$\()\s*[a-zA-ZáéíóúÁÉÍÓÚüÜ_][\wáéíóúÁÉÍÓÚüÜ$]*))      ; Functions
			|([a-zA-Z_$][a-zA-Z0-9_$]*)                        ; Variables
		)"
		return
	}
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
		    FormatDinoTAG(Match.Value(2), RTF, Settings, Map), already:=true
		else if (Match.Value(3) != "") {
			if RegExMatch(Match[3], "ODims)^([ \t]*)\>\s*(?=(\S+)(.*))", Cont) {
				indent:=StrLen(StrReplace(Cont.Value(1), A_Tab, "    "))
				RTF .= "\cf" . Map.Punctuation . " " . EscapeRTF(Cont.Value())
				RTF .= "\cf" . Map.Variables . " " . EscapeRTF(Cont.Value(2))
				(Cont.Value(3)!="")?FormatDinoStr(Cont.Value(3), RTF, Settings, Map, true)
				already:=true
			} else {
				RTF .= "\cf" Map.Plain
			}
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
		else if (Match.Value(9) != "") {
			RTF .= "\cf" Map.Signals
			switch (Match.Value(9))
			{
				case "escape":
					if (_escape:=Trim(SubStr(Code,FoundPos+Match.Len(),2), "`r`n`t ")) && (StrLen(_escape)=1)
						FormatDinoStr(,,,,,{Escape: _escape})
					else
						FormatDinoStr(,,,,,{Escape: "\"})
			}
		} else if (Match.Value(10) != "")
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
