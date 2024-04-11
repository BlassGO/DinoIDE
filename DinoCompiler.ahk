; General DinoCode compiler .config --> .exe
; By @BlassGO
;

; Dependencies path
current := A_ScriptDir "\bin"
global ahk := current "\AutoHotkeyU64.exe"
      , ahk2exe := current "\Ahk2Exe.exe"
	   , bin := ahk
	   , dinocode := current "\dinocode"
      , tmp := dinocode "\build.tmp"
	   , compress
      , out
	   , config
      , libs, extralibs, error, with_gui, admin
	   , icon := current "\icon.ico"
	   , SetName          := "DinoCode"
	   , SetDescription   := "Compiled DinoScript"
	   , SetCopyright     := "Copyright (c) since 2023"
	   , SetCompanyName   := "DinoCode"
	   , SetOrigFilename

; Check
StringCaseSense, Off
Files := ahk2exe
Loop, parse, Files, `,
   (A_LoopField && !InStr(FileExist(A_LoopField), "A")) ? abort("Cant find """ basename(A_LoopField) """ file")

; Extra props
GuiFont := GuiDefaultFont()
textz := GuiFont.Size
style := GuiFont.Name
OnError("RuntimeError")

; ARGS Functions
Arg_in(str) {
   InStr(FileExist(str), "A") ? config:=str : abort("Cant find config-->" str)
}
Arg_bin(str) {
   InStr(FileExist(str), "A") ? bin:=str : abort("Cant find bin-->" str)
}
Arg_ahk2exe(str) {
   InStr(FileExist(str), "A") ? ahk2exe:=str : abort("Cant find Ahk2Exe-->" str)
}
Arg_icon(str) {
   InStr(FileExist(str), "A") ? icon:=str : abort("Cant find icon-->" str)
}
Arg_dinocode(str) {
   InStr(FileExist(str), "D") ? (dinocode:=str, tmp:=dinocode "\build.tmp") : abort("Cant find DinoCode source-->" str)
}
Arg_compress(str) {
   if str is integer
     (str>0) ? compress:=str : compress:=false
   else
     abort("Invalid compression id-->" str)
}
Arg_show(str) {
   (str=1) ? with_gui()
}
Arg_admin(str) {
   (str=1) ? admin:=true : admin:=false
}
Arg_out(str) {
   out:=str
}
Arg_Name(str) {
   SetName:=str
}
Arg_Description(str) {
   SetDescription:=str
}
Arg_Copyright(str) {
   SetCopyright:=str
}
Arg_CompanyName(str) {
   SetCompanyName:=str
}
Arg_OrigFilename(str) {
   SetOrigFilename:=str
}

; Functions
abort(str) {
   MsgBox, 262160, Build Exception, % str
   error:=true
}
RuntimeError(error) {
   MsgBox, 262160, Runtime Exception, % "Internal error in -> [" . A_ScriptName . "]`n`nFile: " . RegExReplace(error.file, ".*\\([^\\]+)$", "$1") . "`nLine: " . error.line . "`nReason: " .  error.message . "`n`n---> " . error.what
   ExitApp
}
GuiDefaultFont() {
   VarSetCapacity(LF, szLF := 28 + (A_IsUnicode ? 64 : 32), 0) ; LOGFONT structure
   If DllCall("GetObject", "Ptr", DllCall("GetStockObject", "Int", 17, "Ptr"), "Int", szLF, "Ptr", &LF)
      Return {Name: StrGet(&LF + 28, 32), Size: Round(Abs(NumGet(LF, 0, "Int")) * (72 / A_ScreenDPI), 1)
            , Weight: NumGet(LF, 16, "Int"), Quality: NumGet(LF, 26, "UChar")}
   return False
}
GetFullPathName(path) {
   cc := DllCall("GetFullPathName", "str", path, "uint", 0, "ptr", 0, "ptr", 0, "uint")
   VarSetCapacity(buf, cc*(A_IsUnicode?2:1))
   DllCall("GetFullPathName", "str", path, "uint", cc, "str", buf, "ptr", 0, "uint")
   return buf
}
LV_SubitemHitTest(HLV) {
   ; To run this with AHK_Basic change all DllCall types "Ptr" to "UInt", please.
   ; HLV - ListView's HWND
   Static LVM_SUBITEMHITTEST := 0x1039
   VarSetCapacity(POINT, 8, 0)
   ; Get the current cursor position in screen coordinates
   DllCall("User32.dll\GetCursorPos", "Ptr", &POINT)
   ; Convert them to client coordinates related to the ListView
   DllCall("User32.dll\ScreenToClient", "Ptr", HLV, "Ptr", &POINT)
   ; Create a LVHITTESTINFO structure (see below)
   VarSetCapacity(LVHITTESTINFO, 24, 0)
   ; Store the relative mouse coordinates
   NumPut(NumGet(POINT, 0, "Int"), LVHITTESTINFO, 0, "Int")
   NumPut(NumGet(POINT, 4, "Int"), LVHITTESTINFO, 4, "Int")
   ; Send a LVM_SUBITEMHITTEST to the ListView
   SendMessage, LVM_SUBITEMHITTEST, 0, &LVHITTESTINFO, , ahk_id %HLV%
   ; If no item was found on this position, the return value is -1
   If (ErrorLevel = -1)
      Return 0
   ; Get the corresponding subitem (column)
   Subitem := NumGet(LVHITTESTINFO, 16, "Int") + 1
   Return Subitem
}
/*
typedef struct _LVHITTESTINFO {
  POINT pt;
  UINT  flags;
  int   iItem;
  int   iSubItem;
  int   iGroup;
} LVHITTESTINFO, *LPLVHITTESTINFO;
*/
SetEditCueBanner(HWND, Cue) {  ; requires AHL_L, thanks to just.me (ahk forum)
   Static EM_SETCUEBANNER := (0x1500 + 1)
   Return DllCall("User32.dll\SendMessageW", "Ptr", HWND, "Uint", EM_SETCUEBANNER, "Ptr", True, "WStr", Cue)
}
basename(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str, Name
   } else {
      RegExMatch(str, "(.*[\\/])*\K(.+)$", Name)
   }
   return Name
}
dirname(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str,, Dir
   } else {
      RegExMatch(str, "(.*?)(?=[\\/][^\\/]*$)", Dir)
   }
   return Dir
}
extname(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str,,,Ext
   } else {
      RegExMatch(str, ".*\.\K(.+)$", Ext)
   }
   return Ext
}
simplename(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str,,,,Name,
   } else {
      RegExMatch(str, "(.*[\\/])*\K(.*?)(?=(\.[^.]*)$|$)", Name)
   }
   return Name
}
isNumber(n) {
	if n is Number
	   return true
}
EnclosingWithQuotes(ByRef s, p, b="(", e=")", len:=0) {
    static $Quote=Chr(34)
    bc:=1, len:=(len)?len:StrLen(s), p+=1
    while,(bc&&p<=len)
    {
        (SubStr(s,p,1)=b)?(bc+=1,p+=1):(((_chr:=SubStr(s,p,1))=e)?(bc-=1,p+=1):((bc>=1)?((_chr=$Quote)?(p:=InStr(s,$Quote,false,p+1),p:=p?p+1:0):p+=1):p+=1))
        if p=0
          return 0
    }
    return (bc=0)?p:0
}
GetNextToken(ByRef s, Byref p, len:=0, Byref _chr:="", Byref _lastchr:="", Byref stop:=false, delimiter:=";")
{
    static $Quote=Chr(34), Deref:=Chr(4)
	p:=(p)?p:1, len:=(len)?len:StrLen(s), _chr:="", _lastchr:="", stop:=""
	while,(p>0&&p<=len)
	{
		_char:=SubStr(s, p, 1)
		if (_char=A_Space||_char=A_Tab) {
           if word
              break
           else
              p++
		} else if _char=(
            _chr:="%", _lastchr:=_char, at:=word?at:p, p:=EnclosingWithQuotes(s,p,,,len)
		else if _char=[
            _chr:="%", _lastchr:=_char, at:=word?at:p, p:=EnclosingWithQuotes(s,p,"[","]",len)
		else if word&&(_char=".")
			_chr:="%", _lastchr:=_char, p+=1
		else if (_char=delimiter||_char="#") {
            stop:=_char
			break
        } else if !word {
            if (_char=$Quote)
                _chr:=_char, at:=p, p:=InStr(s,$Quote,false,p+1),p:=p?p+1:0
            else if (_char=Deref)
                _chr:=SubStr(s,p+1,1), word:=p, p+=1
            else
                word:=p, p+=1
            if (p=0)
               at:=0, p:=-1
        } else 
          p++
		if at
			break
	}
    return,(word)?SubStr(s,word,p-word):(at?SubStr(s,at,p-at):"")
}
solve_escape(ByRef str, ByRef from:="", key:="&") {
	static Deref:=Chr(4)
   resolved:="",resolved_end:=0
   Loop, Parse, str, % Deref
	resolved.=((Mod(A_Index,2)=0)&&IsNumber(_index:=SubStr(A_LoopField,2,-1)))?(((_chr:=SubStr(A_LoopField,1,1))&&_chr=key)?from[_index]:Deref . A_LoopField . Deref):A_LoopField, resolved_end:=A_Index
   return,(resolved_end>1)?str:=resolved:str
}
trim_all(str){
   return RegexReplace(str,"s)^\s*(.*?)\s*$","$1")
}
GetIndent(ByRef s, Byref p:=1) {
    p:=p?p:1, indent:=0
	while,((_char:=SubStr(s,p,1))!="")&&(_char=A_Space||_char=A_Tab)
	p+=1, indent+=(_char=A_Tab)?4:1
	return indent
}
with_indent(str){
   static IndentChar:=Chr(1), AnyS:=A_Space . A_Tab . "`r`n"
   return IndentChar . GetIndent(str,_pos) . IndentChar . RTrim(SubStr(str,_pos),AnyS)
}
NextChar(ByRef s:="", chr:="", p:=0) {
   Loop
	p:=InStr(s,chr,false,p+1)
	Until,(p=0)||!(SubStr(s,p+1,1)=chr&&(p+=1))
	return p
}
EnclosingExpr(ByRef s, p, b="$(", e=")", len:=0) {
    static $Quote=Chr(34)
    bc:=1, len:=(len)?len:StrLen(s), p+=2
    while,(bc&&p<=len)
    {
        (SubStr(s,p,2)=b)?(bc+=1,p+=2):(((_chr:=SubStr(s,p,1))=e)?(bc-=1,p+=1):((bc>=1)?((_chr=$Quote)?(p:=NextChar(s,$Quote,p),p:=p?p+1:0):((_chr="(")?(bc+=1,p+=1):p+=1)):p+=1))
        if p=0
          return 0
    }
    return (bc=0)?p:0
}
EscapeExpr(Byref read_line, Byref _result, len:=0, Byref _hasexpr:=0, Byref _maxexpr:=0) {
	static Deref:=Chr(4), $Expr:="$", $ExprP:="$(", $ExprChr:="``"
    _pos:=1, _extra:=0, _last:=1, resolvedstr:="", read_line_len:=len?len:StrLen(read_line)
    while,(_pos:=InStr(read_line,$ExprP,false,_pos+_extra))
    {
		if (SubStr(read_line,_pos-1,1)="$")
		resolvedstr.=SubStr(read_line, _last, _pos-_last) . $Expr, _end:=_pos+1
		else if (_end:=EnclosingExpr(read_line,_pos,,,read_line_len))
		_result.Push(SubStr(read_line,_pos,_end-_pos)), _maxexpr:=_result.MaxIndex(), resolvedstr.=SubStr(read_line, _last, _pos-_last) . Deref . $ExprChr . _maxexpr . $ExprChr . Deref, (A_Index=1)?_hasexpr:=_maxexpr:false
		else {
         abort("Expression -> '$(' without closing -> ')'")
			break
		}
		_extra:=_end-_pos, _last:=_end
    }
    return,unexpected?"":((_last>1)?resolvedstr . SubStr(read_line, _last) : read_line)
}
EscapeStr(Byref read_line, Byref Escape, Byref _escape) {
    static Deref:=Chr(4), $Quote=Chr(34), $Quotes=Chr(34) . Chr(34)
    _pos:=1, _extra:=0, _last:=0, resolvedstr:=""
    while,(_pos:=InStr(read_line,$Quote,false,_pos+_extra))
    {
        _end:=_pos, is_quoted:=0
        Loop
        _end:=InStr(read_line,$Quote,false,_end+1)
		  Until,(_end=0)||!(SubStr(read_line,_end+1,1)=$Quote&&(is_quoted:=_end+=1))
        if (_end=0) {
            abort("A closure was expected--->""")
            break
        } else {
			((_end-_pos)-1=0) ? (resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . $Quotes):(_escape.Push(SubStr(read_line,_pos+1,(_end-_pos)-1)), resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . $Quote . Deref . "&" . _escape.MaxIndex() . "&" . Deref . $Quote)
			_extra:=(_end-_pos)+1, _last:=_end
        }
    }
    return,unexpected?"":((_last)?resolvedstr . SubStr(read_line, _last+1) : read_line)
}
read_config(file, Escape:="``") {
   static IndentChar:=Chr(1), Delimiter:=Chr(34), AnyS:=A_Space . A_Tab . "`r`n"
   InStr(FileExist(file), "A") ? dir:=dirname(file) : abort("Cant find config-->" file)
   if error
   return 0
   FileEncoding, UTF-8
	if (file:=FileOpen(file, "r `n")) {
		while (!file.AtEOF())
		{
         tag:=false, _pos:=0, read_line:=file.ReadLine(),line_indent:=GetIndent(read_line,_pos), read_line:=RTrim(SubStr(read_line,_pos),"`r`n")
         (txt!=""&&line_indent<=txt) ? (txt:="", include_blank:=true)
         if (txt="") {
            read_line:=RTrim(read_line,AnyS)
            if (SubStr(read_line,1,1)=":") {
               tag:=true
            } else if (SubStr(read_line,1,1)=">") {
               txt:=line_indent ;, read_line:=StrReplace(read_line, A_Space)
            } else if (read_line="") {
               if include_blank
               content.=IndentChar . line_indent . IndentChar . "`r`n", include_blank:=false
               continue
            } else if multi_note {
               (SubStr(read_line,-1)="*#") ? multi_note:=false
               continue
            } else if (SubStr(read_line,1,1)="#") {
               if (SubStr(read_line,2,1)="*") {
                  multi_note:=true
               } else if RegExMatch(read_line,"^#include\s+\K\S.*$",include){
                  include:=InStr(FileExist(dir "\" include), "A") ? GetFullPathName(dir "\" include) : (InStr(FileExist(include), "A") ? GetFullPathName(include) : "")
                  include ? (extralibs.=((extralibs="")?"":"`n") . "#include " . include) : false
               }
               continue
            }
         }
         action:=""
		   if tag {
            read_line:=StrReplace(StrReplace(read_line, A_Space),A_Tab) . "`r`n"
         } else if (txt!="") {
            read_line:=IndentChar . line_indent . IndentChar . read_line . "`r`n"
         } else {
            ; This analysis is superficial focused on the simplification of the indentation and obtaining simple parameters.
            _escape:=[], _result:=[]
            read_line:=EscapeExpr(read_line,_result), read_line:=EscapeStr(read_line, Escape, _escape)
            if error
            break
            (_pos:=InStr(read_line,"#"))?read_line:=Substr(read_line,1,_pos-1):false, read_line_opt:=""
            Loop, Parse, read_line, `;
            {
                  if A_LoopField=
                  continue
                  _pos:=1,read_line_len:=StrLen(A_LoopField),main_action:=[],action:=""
                  while,(_token:=GetNextToken(A_LoopField,_pos,read_line_len,_chrfound,_lastchr,_stop))!=""
                  _isstr:=(_chrfound=Delimiter),(A_Index=1)?(action:=_token):main_action.Push(solve_escape(solve_escape(_isstr?_escape[SubStr(_token, 4, -3)]:_token, _escape), _result, "``"))
                  switch (action)
                  {
                     case "import", "importar":
                        for count, value in main_action
                           content:=read_config(InStr(FileExist(dir "\" value), "A") ? dir "\" value : value, Escape) . content
                        continue
                     case "escape":
                        if (StrLen(main_action.1)=1) {
                           Escape:=main_action.1
                        } else {
                           abort("Reason: Invalid escape character--->" main_action.1 "`n`n---> " read_line)
                           break 2
                        }
                  }
                  main_action:="", read_line_opt.=((A_Index>1)?";":"") . solve_escape(solve_escape((A_Index=1)?IndentChar . line_indent . IndentChar . A_LoopField:Trim(A_LoopField,AnyS), _escape), _result, "``")
            }
            read_line:=(read_line_opt="")?"":read_line_opt . "`r`n", read_line_opt:=""
         }
         content.=read_line
		}
		file.Close()
	}
	return content
}
build() {
   global to_exe, config_tracking
   Gui 1: Submit, NoHide
   libs:="", extralibs:="", error:=false
   out:=dirname(config) "\" simplename(config) ".exe"
   tmp:=out . "~"
   (compress="") ? compress:=2
   (SetOrigFilename="") ? SetOrigFilename := basename(config)
   admin ? extraprops:=";@Ahk2Exe-UpdateManifest 1"
   for name, props in Libraries()
   {
      if props.include
      libs.=(libs=""?"":"`n") . "#include " . props.path
   }
   FileDelete, % tmp
   FileDelete, % out
   InStr(FileExist(out), "A") ? abort("The file " basename(out) " is busy")
   if error
      return 0
   FileAppend,
(
#NoEnv
#SingleInstance Ignore
SendMode Input
SetWorkingDir `%A_ScriptDir`%
SetBatchLines, -1
%libs%
;@Ahk2Exe-SetName         %SetName%
;@Ahk2Exe-SetDescription  %SetDescription%
;@Ahk2Exe-SetCopyright    %SetCopyright%
;@Ahk2Exe-SetCompanyName  %SetCompanyName%
;@Ahk2Exe-SetOrigFilename %SetOrigFilename%
;@Ahk2Exe-SetMainIcon %icon%
;@Ahk2Exe-AddResource %icon%, 160
;@Ahk2Exe-AddResource %icon%, 206
;@Ahk2Exe-AddResource %icon%, 207
;@Ahk2Exe-AddResource %icon%, 208
%extraprops%
config_tracking:=%config_tracking%

), % tmp
   _total:=StrLen(_encoded:=Crypt.Encrypt.StrEncrypt(read_config(config),"petecito",CryptAlg:=1,HashAlg:=1))+1
   _pos:=1
   Loop {
      _pos+=StrLen(_part:=SubStr(_encoded,_pos,12000))
      FileAppend, _part%A_Index%=%_part%`n, % tmp 
      _append ? (_append.=" . ")
      _append.="_part" . A_Index
   } until (_pos=_total)
   FileAppend, % "load_config(Crypt.Encrypt.StrDecrypt(" . _append . ",""petecito"",CryptAlg:=1,HashAlg:=1))`n" extralibs "`nExitApp", % tmp  
   if !error {
      if to_exe {
         RunWait, % ahk2exe . " /in """ . tmp . """ /out """ out """ /bin """ bin """" (compress ? " /compress " compress : "")
         FileDelete, % tmp
         (!InStr(FileExist(out), "A")) ? abort("Cant compile-->" config)
      } else {
         RunWait, % ahk . " """ . tmp . """"
         FileDelete, % tmp
      }
   }
}
Libraries(with_gui:=false) {
   global current, style, lib_findhwnd, to_find_lib, libcontrol, check_all, uncheck_all
   Libraries:={}, list:=current . "\skiplibs.did"
   if FileExist(list) {
      if (SubStr(ObjHeader(list),0)="L") {
         skip:=ObjLoad(list)
      } else {
         MsgBox, % 262144 + 16, Library Manager, Invalid file format`n`n-> %list%
         return
      }
   } else {
      skip:={}
   }
   Loop, %dinocode%\*.ahk
      key:=simplename(A_LoopFileName), Libraries[key]:={path: A_LoopFileFullPath, include: (!skip[key])}
   if with_gui {
      Gui lib: New 
      Gui lib: Default 
      Gui lib: +AlwaysOnTop +Resize
      Gui lib: Font, s10, %style%
      Gui lib: Add, Edit, w520 hwndlib_findhwnd vto_find_lib glib_find Section,
      Gui lib: Add, Button, X+10 YS w100 h30 vcheck_all gcheck_lib, CHECK
      Gui lib: Add, Button, XP Y+5 w100 h30 vuncheck_all guncheck_lib, UNCHECK
      Gui lib: Add, ListView, AltSubmit -ReadOnly NoSortHdr -LV0x10 LV0x20 Checked glibcontrol vlibcontrol XS YS+25 w520 h200, |Name|Path
      gosub lib_find
      SetEditCueBanner(lib_findhwnd, "Search by name...")
      Gui lib: show, AutoSize Center, Library Manager
      Gui lib: +LastFound
      WinWaitClose
      libGuiClose:
         ObjDump(list,skip,,"L")
         Gui lib: Destroy
      return Libraries
   }
   return Libraries
   uncheck_lib:
   check_lib:
      Gui lib: Default 
      Loop % LV_GetCount()
         LV_Modify(A_Index, (InStr(A_ThisLabel, "Un")) ? "-Check" : "Check")
   return
   lib_find:
      GuiControl, lib:-g -Redraw, libcontrol
      Gui lib: Default 
      LV_Delete()
      Sleep 250
      GuiControl, lib: -g, to_find_lib
      Gui lib: Submit, NoHide
      at_pos:=0
      for name, props in Libraries
      {
         switch (name)
         {
            case "DinoCode", "eval", "active_script", "console", "Crypt":
            default:
               if InStr(name, to_find_lib) {
                  at_pos++
                  LV_Add("", "", name, props.path)
                  (props.include&&!skip[name]) ? LV_Modify(at_pos, "Check")
               }
         }
      }
      Loop % LV_GetCount("Column")
         LV_ModifyCol(A_Index, "AutoHdr")
      GuiControl, lib:+glibcontrol +Redraw, libcontrol
      GuiControl, lib:+glib_find, to_find_lib
   return
   libcontrol:
      on_action:=ErrorLevel, on_col:=LV_SubItemHitTest(libmanager)
      GuiControl, lib:-g, libcontrol
      if (on_col=1) {
         If (A_GuiEvent == "I") {
            If (on_action = "c") && LV_GetText(name, A_EventInfo, 2) {
               (on_action == "C") ? (skip.Delete(name)) : skip[name]:=true
            }
         }
      }
      GuiControl, lib:+glibcontrol, libcontrol
   return
   libGuiSize:
      if (A_EventInfo = 1)
         return
      AutoXYWH("w", "to_find_lib"), AutoXYWH("wh", "libcontrol"), AutoXYWH("x", "uncheck_all", "check_all")
   return
}
with_gui(){
   global
   with_gui:=true
   Gui compiler:  +LastFound
   Gui compiler:  Color, 0xDCDCDC, 0xDDF8D2
   Gui compiler:  margin, 10, 10
   Gui compiler:  Font, s10, %style%
   Gui compiler:  Add, Text, Y+10 XP cgreen Section, Icon:
   Gui compiler:  Add, Text, Y+10 XP cgreen, Description:
   Gui compiler:  Add, Text, Y+10 XP cgreen, Copyright:
   Gui compiler:  Add, Text, Y+10 XP cgreen, Company:
   Gui compiler:  Add, Text, Y+10 XP cgreen, OrigName:
   Gui compiler:  Font, s10, Arial Black
   Gui compiler:  Add, Text, X+20 YS c49CC14, -►
   Gui compiler:  Add, Text, XP Y+10 c49CC14, -►
   Gui compiler:  Add, Text, XP Y+10 c49CC14, -►
   Gui compiler:  Add, Text, XP Y+10 c49CC14, -►
   Gui compiler:  Add, Text, XP Y+10 c49CC14, -►
   Gui compiler:  Add, ListView, X+10 YS w300 h20 c747070 0x200 gselect_icon -Hdr -LV0x20 ReadOnly vlist1, |
   Gui compiler:  Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetDescription center, %SetDescription%
   Gui compiler:  Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetCopyright center, %SetCopyright%
   Gui compiler:  Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetCompanyName center, %SetCompanyName%
   Gui compiler:  Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetOrigFilename center, %SetOrigFilename%
   Gui compiler:  Font, s10, %style%
   Gui compiler:  Add, CheckBox, XP Y+10 c747070 vadmin AltSubmit, Request admin permissions
   Gui compiler:  Add, Button, center X+30 YP h30 w90 gupdateinfo, SAVE
   ;Gui compiler:  Add, CheckBox, XP Y+10 c747070 vconsole AltSubmit, Console mode
   Gui compiler:  show, AutoSize, DinoCC
   GuiControl, compiler:, admin, %admin%
   Gui compiler:  ListView, list1
   LV_Add("",basename(icon))
   LV_ModifyCol(1,"AutoHdr Center Select")
   WinWaitClose, DinoCC
   updateinfo:
   compilerGuiClose:
   Gui compiler:  Submit, NoHide
   IniWrite, %icon%, %ini%, COMPILER, icon
   IniWrite, %SetDescription%, %ini%, COMPILER, SetDescription
   IniWrite, %SetCopyright%, %ini%, COMPILER, SetCopyright
   IniWrite, %SetCompanyName%, %ini%, COMPILER, SetCompanyName
   IniWrite, %admin%, %ini%, COMPILER, admin
   Gui compiler:  Destroy
   return
}

select_icon() {
   global icon
   Gui compiler:  ListView, list1
   FileSelectFile, icontmp, 1,, Please select some .ico, (*.ico)
   if icontmp {
      icon:=icontmp
      LV_GetCount() ? LV_Modify(1,"Col1",basename(icon)) : LV_Add("",basename(icon))
      LV_ModifyCol(1,"AutoHdr Center Select")
   }
}