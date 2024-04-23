;=======================================================================================
;
; Function:			Eval
; Description:		Evaluate Expressions in Strings.
; Return value:		Depending on the _action parameter it can return an array with multiple resolutions or a single string.
;
; Author:           BlassGO
; Credits:          Inspired by Pulover's Eval
; Credits:			Original ExprEval() by Uberi
;
;=======================================================================================
;
; Parameters:
;
;	str				The input string to be evaluated. You can enter multiple expressions
;					separated by commas (inside the string).
;	FD	            The FD Array inherited from DinoCode, serves to resolve references to Variables at the current level
;	_Objects        Provisional array to store special escapes during the compilation of Expressions (segments of the original string), especially focused on References to Object structures
;   _escape         DinoCode array that stores resolved strings and therefore allows Eval to resolve references to corresponding to these
;   _result         DinoCode array that stores resolved or unresolved special expressions such as $() and therefore allows Eval to resolve references to corresponding to these
;   _evaluated      DinoCode array that stores initial resolution expressions such as %%, allows Eval to resolve references to corresponding to these
;   _action         Integer that defines the behavior mode of Eval
;   _hasexpr        Integer with the minimum level of resolved Expressions in _result, allows identifying expressions stored to be resolved
;
;
;=======================================================================================
SetBatchLines -1

; Initialize variables for ExprEval() functions
ExprInit()

Eval(str, Byref FD := "", Byref _Objects:="", Byref _escape:="", Byref _result:="", ByRef _evaluated:="", _action:=0, Byref _hasexpr:=0)
{
	Static $Quote:=Chr(2), c1:=Chr(1), Deref:=Chr(4), $RE:=Chr(96), $Hash:=Chr(35)
	; If it is a number, avoid unnecessary expression compilation
	if str is number
	return,(_action>=2)?str:[str]
	; Evaluate parsed expression with ExprEval()
	$Result:=ExprEval((_action=4||_action=-1)?str:ExprCompile(str,_Objects), FD, _Objects, _escape, _result, _evaluated, _action, _hasexpr)
	if $Result=
		return
	; When only the resolution of a single segment is expected
	if (_action>=2) {
		if (_action=2||_action=4) {
			; Restore object references
			if (SubStr($Result,1,1)=Deref)&&(_end:=InStr($Result,Deref,2)) {
				switch SubStr($Result,2,1)
				{
					case $Hash:
						return FD[FD_CURRENT][SubStr($Result,3,-2)]
					case $RE:
						return _result[SubStr($Result,3,-2)]
				}
			}
		}
		return,$Result
	} else {
		$Result:=StrSplit($Result,c1)
		; Restore object references
		For _i, _v in $Result
		{
			if (SubStr(_v,1,1)=Deref)&&(_end:=InStr(_v,Deref,2)) {
				switch SubStr(_v,2,1)
				{
					case $Hash:
						$Result[_i]:=FD[FD_CURRENT][SubStr(_v,3,-2)]
					case $RE:
						$Result[_i]:=_result[SubStr(_v,3,-2)]
				}
			}
		}
		return,$Result
	}
}

; Always work with a Local copy of the Objects, allowing recursion on these
EvalLocal(str, Byref FD := "", _Objects:="", Byref _escape:="", Byref _result:="", ByRef _evaluated:="", _action:=0, Byref _hasexpr:=0) {
	return Eval(str, FD, _Objects, _escape, _result, _evaluated, _action, _hasexpr)
}

/*
			Try
			{
				If o_Oper=:=
					_ArrayObject := _ArrayObject[_Key*] := o_Value
				Else If o_Oper=+=
					_ArrayObject := _ArrayObject[_Key*] += o_Value
				Else If o_Oper=-=
					_ArrayObject := _ArrayObject[_Key*] -= o_Value
				Else If o_Oper=*=
					_ArrayObject := _ArrayObject[_Key*] *= o_Value
				Else If o_Oper=/=
					_ArrayObject := _ArrayObject[_Key*] /= o_Value
				Else If o_Oper=//=
					_ArrayObject := _ArrayObject[_Key*] //= o_Value
				Else If o_Oper=.=
					_ArrayObject := _ArrayObject[_Key*] .= o_Value
				Else If o_Oper=|=
					_ArrayObject := _ArrayObject[_Key*] |= o_Value
				Else If o_Oper=&=
					_ArrayObject := _ArrayObject[_Key*] &= o_Value
				Else If o_Oper=^=
					_ArrayObject := _ArrayObject[_Key*] ^= o_Value
				Else If o_Oper=>>=
					_ArrayObject := _ArrayObject[_Key*] >>= o_Value
				Else If o_Oper=<<=
					_ArrayObject := _ArrayObject[_Key*] <<= o_Value
			}
*/

; By Pulover
; I made some optimizations for the convenience of DinoCode (BlassGO)
ParseObjects(Byref v_String, Byref FD := "", o_Oper :=  "", Byref o_Value := "",Byref _escape:="", Byref _result:="", ByRef _evaluated:="",Byref _hasexpr:=0)
{
	Static Deref:=Chr(4),$BraL:=Chr(91),$BraR:=Chr(93),$ParentL:=Chr(40),$ParentR:=Chr(41)
	l_Matches:=[], _pos:=1, len:=StrLen(v_String)
	While,(_method:=GetNextMethod(v_String,_pos,len,isdot))!=""
	(!isdot)?l_Matches.Push(_method)
	v_Obj:=l_Matches[1], l_MatchesLen:=l_Matches.Length(),(SubStr(v_Obj,1,1)=Deref)?(inkey:=SubStr(v_Obj, 3, -2), (inkey<=_hasexpr)?_ArrayObject:=_result[inkey]:(_ArrayObject:=load_config(_result[inkey],,,FD_CURRENT,"resolve",,false),_hasexpr+=1)):_ArrayObject:=FD[FD_CURRENT][v_Obj]
	For $i, $v in l_Matches
	{
		_start:=SubStr($v, 1, 1), _end:=SubStr($v, 0)
		If (_start=$ParentL&&_end=$ParentR)
			continue
		_Key:=(_start=$BraL&&_end=$BraR)?Eval(SubStr($v, 2, -1), FD, ,_escape, _result, _evaluated):[$v],$n:=l_Matches[$i+1], _start:=SubStr($n, 1, 1), _end:=SubStr($n, 0)
		If (_start=$ParentL&&_end=$ParentR)
		{
			_Key:=_Key[1], _Params:=Eval(SubStr($n, 2, -1), FD, ,_escape, _result, _evaluated)
			Try _ArrayObject:=($i=1)?%_Key%(_Params*):_ArrayObject[_Key](_Params*)
			Catch e
			{
				If (InStr(e.Message, "0x800A03EC"))
				{
					; Workaround for strange bug in some Excel methods
					For _i, _v in _Params
						_Params[_i]:=" " . _v
					_ArrayObject:=($i=1)?%_Key%(_Params*):_ArrayObject[_Key](_Params*)
				}
				Else
				Throw e
			}
		} Else If ($i=l_MatchesLen&&o_Oper=":=")
		try _ArrayObject:=_ArrayObject[_Key*]:=o_Value
		Else If ($i>1)
		_ArrayObject:=_ArrayObject[_Key*]
	}
	return,_ArrayObject
}

AssignParse(Byref String, ByRef VarName, ByRef Oper, ByRef VarValue)
{
	RegExMatch(String, "(.*?)(:=|\+=|-=|\*=|/=|//=|\.=|\|=|&=|\^=|>>=|<<=)(?=([^""]*""[^""]*"")*[^""]*$)(.*)", Out)
,	VarName := Trim(Out1), Oper := Out2, VarValue := Trim(Out4)
}

StrJoin(ByRef InputArray, JChr := "", Quote := false, Init := true, Unquote := false)
{
    Static $Quote := Chr(2)
    JoinedStr := ""
    For i, v in InputArray
    {
        If (IsObject(v))
            return v
        If v is not Number
        {
            If (!Init)
				v := RegExReplace(v, """{1,2}", """""")
            If (Quote)
                v := """" . v . """"
            If (Unquote)
				v := StrReplace(v, """""", """")
			v := StrReplace(v, $Quote, """")
        }
        JoinedStr .= v . JChr
    }
    return (JChr != "")?SubStr(JoinedStr, 1, -(StrLen(JChr))):JoinedStr
}

; Extra functions by BlassGO
; Inspired in Exprmlb()
Enclosing(ByRef s, p, b="(", e=")", len:=0) {
    bc:=1, len:=len?len:StrLen(s), p+=1
    while,bc&&p<=len
        ((_chr:=SubStr(s,p,1))=b)?(bc+=1):((_chr=e)?bc-=1:false), p+=1
    return,(bc=0)?p:0
}
EnclosingWithQuotes(ByRef s, p, b="(", e=")", len:=0) {
    static $Quote=Chr(34)
    bc:=1, len:=len?len:StrLen(s), p+=1
    while,bc&&p<=len
    {
        (SubStr(s,p,1)=b)?(bc+=1,p+=1):(((_chr:=SubStr(s,p,1))=e)?(bc-=1,p+=1):((bc>=1)?((_chr=$Quote)?(p:=InStr(s,$Quote,false,p+1),p:=p?p+1:0):p+=1):p+=1))
        if p=0
        return 0
    }
    return,(bc=0)?p:0
}
NextChar(ByRef s:="", chr:="", p:=0) {
    Loop
	p:=InStr(s,chr,false,p+1)
	Until,p=0||!(SubStr(s,p+1,1)=chr&&(p+=1))
	return,p
}
isObjRef(ByRef s,p:=1, len:=0) {
    static $Quote=Chr(34), $Dot:=Chr(46), $BraL:=Chr(91)
	len:=len?len:StrLen(s)
    while,p<=len
    {
        _chr:=SubStr(s,p,1), (_chr=$Quote)?(p:=InStr(s,$Quote,false,p+1),p:=p?p+1:0):p+=1
        if p=0
        return 0
		if (_chr=$Dot||_chr=$BraL)
        return,p-2
    }
    return,0
}
EnclosingExpr(ByRef s, p, b="$(", e=")", len:=0) {
    static $Quote=Chr(34), $ParentL:=Chr(40)
    bc:=1, len:=len?len:StrLen(s), p+=2
    while,bc&&p<=len
    {
        (SubStr(s,p,2)=b)?(bc+=1,p+=2):(((_chr:=SubStr(s,p,1))=e)?(bc-=1,p+=1):((bc>=1)?((_chr=$Quote)?(p:=NextChar(s,$Quote,p),p:=p?p+1:0):((_chr=$ParentL)?(bc+=1,p+=1):p+=1)):p+=1))
        if p=0
        return 0
    }
    return,(bc=0)?p:0
}
GetWord(Byref s,w,p:=1,len:=0,casesense:=false) {
	len:=len?len:StrLen(s)
	return,(_at:=StartOfWord(s,w,p,casesense))?SubStr(s,_at,WhileWord(s,_at,len)-_at+1):""
}
StartOfWord(Byref s,w,p:=1,casesense:=false) {
	len:=StrLen(w)
	Loop
	p:=InStr(s,w,casesense,p)
	Until,p=0||p=1||((_char:=SubStr(s,p-1,1))=A_Space||_char=A_Tab)||p+=len
	return,p
}
WhileWord(ByRef s, p:=1, len:=0) {
    p:=p?p:1, len:=len?len:StrLen(s)
	while,p<=len&&((_char:=SubStr(s,p,1))!=A_Space&&_char!=A_Tab)
	p+=1,word:=true
	return,word?p-1:0
}
GetIndent(ByRef s, Byref p:=1) {
    p:=p?p:1, indent:=0
	while,((_char:=SubStr(s,p,1))!="")&&(_char=A_Space||_char=A_Tab)
	p+=1,indent+=(_char=A_Tab)?4:1
	return,indent
}
WhileWordBack(ByRef s, p:=0, len:=0) {
	static Chars:="_,$"
    p:=p?p:0, len:=len?len:StrLen(s)
    while,p>0&&((_char:=SubStr(s,p,1))!=A_Space&&_char!=A_Tab) {
		if _char in %Chars%
        word:=true
		else if _char is alnum
		word:=true
		else
		break
		p--
    }
    return,word?p+1:0
}
RestoreElements(Byref s, Byref _Elements, b:="_&", e:="&_", p:=0) {
    at:=p:=(p)?p:InStr(s,b,false,1)
    if !p
       return s
    bc:=1, len:=StrLen(s),blen:=StrLen(b),elen:=StrLen(e), resolved:=SubStr(s,1,p-1), p+=blen
    while,(bc=1&&p <= len) {
        _chr := SubStr(s, p, blen)
        if (_chr = b)
            bc+=1, at:=p, p+=blen
        else if (_chr = e)
            bc-=1, p+=elen
        else if (bc>=1)&&((_chr:=SubStr(_chr,1,1))=A_Space)||(_chr=A_Tab)
            bc-=1, p+=blen, ignore:=true
        else
            p++
        if (bc = 0) {
			resolved.=SubStr(s, lastat, at-lastat)
            if ignore
				resolved.=SubStr(s, at, p-at)
			else
                lastat:=p,resolved.=_Elements[SubStr(s, at, p-at)]
            if (next:=InStr(s, b, false, p))
                ignore:=false, at:=p:=next, bc:=1, p+=blen
            else
                break
        }
    }
    return (lastat)?(resolved . SubStr(s, p)):s
}
NextToChar(Byref str, Byref char, n:=1, get:=1) {
	return,(_pos:=InStr(str,char,false,1,n))?SubStr(str,_pos+1,get):""
}

;##################################################
; Author: Uberi
; Modified by: Pulover and BlassGO
; http://autohotkey.com/board/topic/64167-expreval-evaluate-expressions/
;##################################################
;	Exprot:="`n:= 0 R 2`n+= 0 R 2`n-= 0 R 2`n*= 0 R 2`n/= 0 R 2`n//= 0 R 2`n.= 0 R 2`n|= 0 R 2`n&= 0 R 2`n^= 0 R 2`n>>= 0 R 2`n<<= 0 R 2`n|| 3 L 2`n&& 4 L 2`n\! 5 R 1`n= 6 L 2`n== 6 L 2`n<> 6 L 2`n~= 6 L 2`n!= 6 L 2`n> 7 L 2`n< 7 L 2`n>= 7 L 2`n<= 7 L 2`n\. 8 L 2`n& 9 L 2`n^ 9 L 2`n| 9 L 2`n<< 10 L 2`n>> 10 L 2`n+ 11 L 2`n- 11 L 2`n* 12 L 2`n/ 12 L 2`n// 12 L 2`n\- 13 R 1`n! 13 R 1`n~ 13 R 1`n\& 13 R 1`n\* 13 R 1`n** 14 R 2`n\++ 15 R 1`n\-- 15 R 1`n++ 15 L 1`n-- 15 L 1`n. 16 L 2`n`% 17 R 1`n",Exprol:=SubStr(RegExReplace(Exprot,"iS) \d+ [LR] \d+\n","`n"),2,-1)
ExprInit()
{
	global
	Exprot:="`n|| 3 L 2`n&& 4 L 2`n\! 5 R 1`n= 6 L 2`n== 6 L 2`n<> 6 L 2`n~= 6 L 2`n!= 6 L 2`n> 7 L 2`n< 7 L 2`n>= 7 L 2`n<= 7 L 2`n\. 8 L 2`n& 9 L 2`n^ 9 L 2`n| 9 L 2`n<< 10 L 2`n>> 10 L 2`n+ 11 L 2`n- 11 L 2`n* 12 L 2`n/ 12 L 2`n// 12 L 2`n\- 13 R 1`n! 13 R 1`n~ 13 R 1`n\& 13 R 1`n\* 13 R 1`n** 14 R 2`n\++ 15 R 1`n\-- 15 R 1`n++ 15 L 1`n-- 15 L 1`n. 16 L 2`n`% 17 R 1`n",Exprol:=SubStr(RegExReplace(Exprot,"iS) \d+ [LR] \d+\n","`n"),2,-1)
	Sort,Exprol,FExprols
}

ExprCompile(Byref e, Byref objs:="", format:=true)
{
	static c1:=Chr(1),$L:="L",$R:="R"
	format?(objs:=[],e:=Exprt(e,objs))
	Loop,Parse,e,%c1%
	{
		lf:=A_LoopField,tt:=SubStr(lf,1,1),to:=SubStr(lf,2)
		If lf=,
		{
			While,s<>""
			Exprp1(ou,Exprp2(s))
		}
		Else If tt=o
		{
			While,SubStr(so:=Exprp3(s),1,1)="o"
			{
				ta:=Expras(to),tp:=Exprpr(to),sop:=Exprpr(SubStr(so,2))
				If ((ta=$L&&tp>sop)||(ta=$R&&tp>=sop))
				Break
				Exprp1(ou,Exprp2(s))
			}
			Exprp1(s,lf)
		}
		Else Exprp1(ou,lf)
	}
	While,s<>""
	{
		t1:=Exprp2(s)
		If t1 In (,)
		Return
		Exprp1(ou,t1)
	}
	Return,ou
}
ExprEval(Byref e,Byref lp, Byref objs, Byref esc, Byref re, Byref eva, _action:=0, Byref _hasexpr:=0)
{
	static c1:=Chr(1),Deref:=Chr(4),$v:="v",$HashL:=Chr(4) . Chr(35), $HashR:=Chr(35) . Chr(4)
	Loop,Parse,e,%c1%
	{
		lf:=A_LoopField,tt:=SubStr(lf,1,1),t:=SubStr(lf,2),InStr(lf,Deref)?(lf:=solve_any_escape(lf,esc,re,eva,_hasexpr))
		If tt In l,v
		lf:=Exprp1(s,lf)
		Else{
			a:=Exprac(t),Exprp1(s,Exprap(t,s,a,lp,objs,esc,re,eva,stop,_action,_hasexpr))
			If unexpected
			return 0
			if stop
			break
		}
	}
	Loop,Parse,s,%c1%
	{
		lf:=A_LoopField
		If (SubStr(lf,1,1)=$v)
		t1:=SubStr(lf,2),r.=(isObject(lp[FD_CURRENT][t1])?$HashL . t1 . $HashR:lp[FD_CURRENT][t1]) . c1
		Else r.=SubStr(lf,2) . c1
	}
	Return,SubStr(r,1,-1)
}
/*
	If o=:=
	{
		lp[FD_CURRENT][a1]:=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=+=
	{
		lp[FD_CURRENT][a1]+=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=-=
	{
		lp[FD_CURRENT][a1]-=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=*=
	{
		lp[FD_CURRENT][a1]*=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=/=
	{
		lp[FD_CURRENT][a1]/=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=//=
	{
		lp[FD_CURRENT][a1]//=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=.=
	{
		lp[FD_CURRENT][a1].=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=|=
	{
		lp[FD_CURRENT][a1]|=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=&=
	{
		lp[FD_CURRENT][a1]&=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=^=
	{
		lp[FD_CURRENT][a1]^=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=>>=
	{
		lp[FD_CURRENT][a1]>>=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
	If o=<<=
	{
		lp[FD_CURRENT][a1]<<=(a2v ? lp[FD_CURRENT][a2]:a2)
		Return,"v" . a1
	}
*/
Exprap(o,ByRef s,ac,Byref lp, Byref objs, Byref esc, Byref re, Byref eva, Byref stop:=false, _action:=0, Byref _hasexpr:=0)
{
	local i,t1,a1,a2,a1v,a2v,r1,r2,r3
	static DerefEL:=Chr(4) . Chr(96),DerefER:=Chr(96) . Chr(4) , c1:=Chr(1), $SC:=Chr(59), $v:="v", $l:="l",$resolve:="resolve"
	Loop,%ac%
	i:=ac-(A_Index-1),t1:=Exprp2(s),a%i%:=SubStr(t1,2),(SubStr(t1,1,1)=$v)?(a%i%v:=1)
	r2:=SubStr(o,1,1)
	If r2=r
	{
		r2:=SubStr(o,2)
		If (r2<=_hasexpr)
		Return,$l . (IsObject(re[r2])?$l . DerefEL . r2 . DerefER:re[r2])
		Else
		r1:=load_config(re[r2],,,FD_CURRENT,$resolve,,false), _hasexpr+=1
	} Else If r2=o
	r1:=ParseObjects(objs[SubStr(o,2)], lp,,,esc,re,eva,_hasexpr)
	Else if r2=p
	r1:=ExprEval(objs[SubStr(o,2)],lp,objs,esc,re,eva,_action,_hasexpr)
	Else if r2=f
	r3:=InStr(o,$SC), r2:=SubStr(o,2,r3-2),r1:=IsFunc(r2)?((skip_functions&&r2~=skip_functions)?"":%r2%(EvalLocal(objs[SubStr(o,r3+1)],lp,objs,esc,re,eva,-1,_hasexpr)*)):((unexpected:="Unrecognized function name: " . r2)?"":"")
	If o=++
	Return,$l . lp[FD_CURRENT][a1]++
	If o=--
	Return,$l . lp[FD_CURRENT][a1]--
	If o=\++
	Return,$l . ++lp[FD_CURRENT][a1]
	If o=\--
	Return,$l . --lp[FD_CURRENT][a1]
	If o=!
	Return,$l . !(a1v ? lp[FD_CURRENT][a1]:a1)
	If o=\!
	Return,$l . (a1v ? lp[FD_CURRENT][a1]:a1)
	If o=~
	Return,$l . ~(a1v ? lp[FD_CURRENT][a1]:a1)
	If o=**
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)**(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=*
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)*(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=\*
	Return,$l . *(a1v ? lp[FD_CURRENT][a1]:a1)
	If o=/
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)/(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=//
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)//(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=+
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)+(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=-
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)-(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=\-
	Return,$l . -(a1v ? lp[FD_CURRENT][a1]:a1)
	If o=<<
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)<<(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=>>
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)>>(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=&
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)&(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=\&
	Return,$l . &(a1v ? lp[FD_CURRENT][a1]:a1)
	If o=^
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)^(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=|
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)|(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=\.
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1) . (a2v ? lp[FD_CURRENT][a2]:a2))
	If o=.
	Return,$v . a1
	If o=<
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)<(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=>
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)>(a2v ? lp[FD_CURRENT][a2]:a2))
	If o==
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)=(a2v ? lp[FD_CURRENT][a2]:a2))
	If o===
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)==(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=<>
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)<>(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=~=
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)~=(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=!=
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)!=(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=>=
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)>=(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=<=
	Return,$l . ((a1v ? lp[FD_CURRENT][a1]:a1)<=(a2v ? lp[FD_CURRENT][a2]:a2))
	If o=&&
	Return,$l . (stop:=((a1v ? lp[FD_CURRENT][a1]:a1)&&(a2v ? lp[FD_CURRENT][a2]:a2))), stop:=!stop
	If o=||
	Return,$l . (stop:=((a1v ? lp[FD_CURRENT][a1]:a1)||(a2v ? lp[FD_CURRENT][a2]:a2)))
	If IsObject(r1)
	{
		re.Push(r1)
		Return,$l . DerefEL . re.MaxIndex() . DerefER
	} Else
	Return,$l . r1
}
/*
SetFormat,IntegerFast,Hex
While,RegExMatch(t1,"iS)[^\w']",c)
t1:=StrReplace(t1,c,"'" . SubStr("0" . SubStr(Asc(c),3),-1))
SetFormat,IntegerFast,D

f1:=1,f:=1
While,(f:=RegExMatch(e,"S)(^|[^\w#@\$'])([\w#@\$]{1,253})(?=\()",m,f))
{
	t1:=f+StrLen(m)
	If (SubStr(e,t1+1,1)=")")
	ac=0
	Else
	{
		If !Exprmlb(e,t1,fa)
		Return
		fa:=StrReplace(fa,"`,","`,",c)
		ac:=c+1
	}
	e1.=SubStr(e,f1,f-f1) . m1 . c1 . "f" . ac . "'20" . m2 . c1,f+=StrLen(m),f1:=f
}
e:=e1 . SubStr(e,f1)

,	e:=StrReplace(e,"(",c1 "(" c1)
,	e:=StrReplace(e,")",c1 ")" c1)
*/

Exprpa(e, Byref objs) {
	static c1:=Chr(1), $ParentL:=Chr(40), $Apos:=Chr(39), $Apos27:="'27"
	len:=StrLen(e), f2:=f1:=1
	while,(f:=InStr(e,$ParentL,false,f1))&&(f1:=Enclosing(e,f,,,len))
	{
		m:=StrReplace(Exprpa(SubStr(e,f+1,f1-f-2),objs),c1 . c1,c1), m:=SubStr(m,2,-1),f3:=0
		While,f3:=InStr(m,$Apos,False,f3+1)
		{
			If (t1:=SubStr(m,f3+1,2))<>27
			m:=StrReplace(m,$Apos . t1,Chr("0x" . t1))
		}
		objs.Push(ExprCompile(StrReplace(m,$Apos27,$Apos),,false)), (word:=WhileWordBack(e,f-1)) ? (e1.=SubStr(e,f2,word-f2) . c1 . "df" . SubStr(e, word, f-word) . ";" . objs.MaxIndex() . c1) : (e1.=SubStr(e,f2,f-f2) . c1 . "dp" . objs.MaxIndex() . c1), f2:=f1
	}
	return,e1 . SubStr(e,f2)
}

Exprt(e, Byref objs:="")
{
	global Exprol
	static c1:=Chr(1), c1_2:=Chr(1) . Chr(1), $Quote=Chr(34), $Apos:=Chr(39), $Apos27:="'27", $l:="l", $n:="n", $do:="do", $dr:="dr"
	, $Space4:="    ", $Dot:=Chr(46), $Concat:=" . ", $ConcatRpl:="\.", $Comma:=Chr(44), $CommaRpl:=Chr(1) . Chr(44) . Chr(1), $C1N:=Chr(1) . "n", $C1L:=Chr(1) . "l", $2E:="'2E", $Not:="!", $And:="&&", $Or:="||"
	, $Regex1:="iS)[^\w']"
	, $Regex2:="S)([\w#@\$\x04] +|\) *)(?=" . Chr(1) . "*[\w#@\$\(]|\x04)"
	, $Regex2Rpl:="$1 . "
	, $Regex3:="S)(^|[^\w#@\$])\x04``(\d+)``\x04"
	, $Regex4:="iS)(^|[^\w#@\$'])(0x[0-9a-fA-F]+|\d+(?:\.\d+)?)(?=[^\d\.]|$)"
	, $RegexVar:="S)(?:^|[^\w#@\$'" . Chr(1) . "])\K[\w#@\$]{1,253}(?=[^\(\w#@\$]|$)"
	, $RegexVarRpl:=Chr(1) . "v$0" . Chr(1)
	, $DotNot:="\." . Chr(1) . "vNot" . Chr(1) . "\."
	, $DotAnd:="\." . Chr(1) . "vAnd" . Chr(1) . "\."
	, $DotOr:="\." . Chr(1) . "vOr" . Chr(1) . "\."
	, $NotDot:=Chr(1) . "vNot" . Chr(1) . "\."
	, $AndDot:=Chr(1) . "vAnd" . Chr(1) . "\."
	, $OrDot:=Chr(1) . "vOr" . Chr(1) . "\."
	, $DotNotC1:="\." . Chr(1) . "vNot" . Chr(1) . Chr(1)
	, $DotAndC1:="\." . Chr(1) . "vAnd" . Chr(1) . Chr(1)
	, $DotOrC1:="\." . Chr(1) . "vOr" . Chr(1) . Chr(1)
	, $NotC1:=Chr(1) . "vNot" . Chr(1) . Chr(1)
	, $AndC1:=Chr(1) . "vAnd" . Chr(1) . Chr(1)
	, $OrC1:=Chr(1) . "vOr" . Chr(1) . Chr(1)
	, $BackMinus:="S)(^|[^" . Chr(1) . "\)-])-" . Chr(1) . "(?=[lvd])"
	, $BackMinusRpl:="$1\-" . Chr(1)
	, $BackAmper:="S)(^|[^" . Chr(1) . "\)&])&" . Chr(1) . "(?=[lvd])"
	, $BackAmperRpl:="$1\&" . Chr(1)
	, $BackAsteri:="S)(^|[^" . Chr(1) . "\)\*])\*" . Chr(1) . "(?=[lvd])"
	, $BackAsteriRpl:="$1\*" . Chr(1)
	, $BackCounter:="S)(^|[^" . Chr(1) . "\)])(\+\+|--)" . Chr(1) . "(?=[lvd])"
	, $BackCounterRpl:="$1\$2" . Chr(1)
	, $ExprolResult, $ExprolResultRpl:=Chr(1) . "o$0" . Chr(1)
	, $RegexFail:="S)" . Chr(1) . "[^lvod,\n]"
	($ExprolResult="")?$ExprolResult:="S)" . StrReplace(RegExReplace(Exprol,"S)[\\\.\*\?\+\[\{\|\(\)\^\$]","\$0"),"`n","|")
	f1:=1,_pos:=1, _extra:=0
    while,_pos:=InStr(e,$Quote,false,_pos+_extra)
    {
	  if _end:=InStr(e,$Quote,false,_pos+1)
	  {
		t1:=Substr(e, _pos+1, (_end-_pos)-1), _extra:=StrLen(t1)+2
		SetFormat,IntegerFast,Hex
		While,RegExMatch(t1,$Regex1,c)
		t1:=StrReplace(t1,c,$Apos . SubStr("0" . SubStr(Asc(c),3),-1))
		SetFormat,IntegerFast,D
		e1.=SubStr(e,f1,_pos-f1) . c1 . $l . t1 . c1,f1:=_pos+_extra
	  } else {
		 unexpected := "A closure was expected--->"""
		 return 0
	  }
    }
	e1.=SubStr(e,f1),e:=e1
,	e:=StrReplace(e,A_Tab,$Space4)
,	e:=RegExReplace(e,$Regex2,$Regex2Rpl)
,	e:=StrReplace(e,$Concat,$ConcatRpl)
,	e:=StrReplace(e,A_Space)
	e1:="",f:=1,f1:=1,len:=StrLen(e)
	While,m:=GetNextObjRef(e,start,f,len)
	objs.Push(m), e1.=SubStr(e,f1,start-f1) . c1 . $do . objs.MaxIndex() . c1,f1:=f
	e:=e1 . SubStr(e,f1),e1:="",f:=1,f1:=1
	While,f:=RegExMatch(e,$Regex3,m,f)
	e1.=SubStr(e,f1,f-f1) . m1 . c1 . $dr . m2 . c1,f+=StrLen(m),f1:=f
	e:=e1 . SubStr(e,f1),e1:="",f:=1,f1:=1
	While,f:=RegExMatch(e,$Regex4,m,f)
		m2+=0,m2:=StrReplace(m2,$Dot,$2E,,1),e1.=SubStr(e,f1,f-f1) . m1 . c1 . $n . m2 . c1,f+=StrLen(m),f1:=f
	e:=e1 . SubStr(e,f1),e1:="" ; ,e:=RegExReplace(e,"S)(^|\(|[^" . c1 . "-])-" . c1 . "n","$1" . c1 . "n'2D")
,	e:=StrReplace(e,$C1N,$C1L) ;,	e:=RegExReplace(e,"\\\.(\d+)\.(\d+)",c1 . "l$1'2E$2" . c1)
,	e:=RegExReplace(e,$RegexVar,$RegexVarRpl)
,	e:=StrReplace(e,$DotNot,$Not)
,	e:=StrReplace(e,$DotAnd,$And)
,	e:=StrReplace(e,$DotOr,$Or)
,	e:=StrReplace(e,$NotDot,$Not)
,	e:=StrReplace(e,$AndDot,$And)
,	e:=StrReplace(e,$OrDot,$Or)
,	e:=StrReplace(e,$DotNotC1,$Not)
,	e:=StrReplace(e,$DotAndC1,$And)
,	e:=StrReplace(e,$DotOrC1,$Or)
,	e:=StrReplace(e,$NotC1,$Not)
,	e:=StrReplace(e,$AndC1,$And)
,	e:=StrReplace(e,$OrC1,$Or)
,	e:=RegExReplace(e,$BackMinus,$BackMinusRpl)
,	e:=RegExReplace(e,$BackAmper,$BackAmperRpl)
,	e:=RegExReplace(e,$BackAsteri,$BackAsteriRpl)
,	e:=RegExReplace(e,$BackCounter,$BackCounterRpl)
,	e:=RegExReplace(e,$ExprolResult,$ExprolResultRpl)
,	e:=StrReplace(e,$Comma,$CommaRpl)
,	e:=Exprpa(e,objs)
	e:=StrReplace(e,c1_2,c1)
	If RegExMatch(e,$RegexFail)
	Return
	e:=SubStr(e,2,-1),f:=0
	While,f:=InStr(e,$Apos,False,f+1)
	{
		If (t1:=SubStr(e,f+1,2))<>27
		e:=StrReplace(e,$Apos . t1,Chr("0x" . t1))
	}
	e:=StrReplace(e,$Apos27,$Apos)
	Return,e
}

Exprols(o1,o2)
{
	Return,StrLen(o2)-StrLen(o1)
}

Exprpr(o)
{
	global Exprot
	static $LBreak:="`n"
	t:=InStr(Exprot,$LBreak . o . A_Space)+StrLen(o)+2
	Return,SubStr(Exprot,t,InStr(Exprot,A_Space,0,t)-t)
}

Expras(o)
{
	global Exprot
	static $LBreak:="`n"
	Return,SubStr(Exprot,InStr(Exprot,A_Space,0,InStr(Exprot,$LBreak . o . A_Space)+StrLen(o)+2)+1,1)
}

Exprac(o)
{
	global Exprot
	static $LBreak:="`n"
	Return,SubStr(Exprot,InStr(Exprot,$LBreak,0,InStr(Exprot,$LBreak . o . A_Space)+1)-1,1)
}

Exprmlb(ByRef s,p,ByRef o="",b="(",e=")")
{
	t:=SubStr(s,p),bc:=0,VarSetCapacity(o,StrLen(t))
	If (SubStr(t,1,1)<>b)
	Return,0
	Loop,Parse,t
	{
		lf:=A_LoopField
		If lf=%b%
		bc++
		Else If lf=%e%
		{
			bc--
			If bc=0
			Return,p
		}
		Else If bc=1
		o.=lf
		p++
	}
	Return,0
}

Exprp1(ByRef dl,d)
{
	dl.=((dl="")? "":Chr(1)) . d
}

Exprp2(ByRef dl)
{
	t:=InStr(dl,Chr(1),0,0),t ?(t1:=SubStr(dl,t+1),dl:=SubStr(dl,1,t-1)):(t1:=dl,dl:="")
	Return,t1
}

Exprp3(ByRef dl)
{
	Return,SubStr(dl,InStr(dl,Chr(1),0,0)+1)
}