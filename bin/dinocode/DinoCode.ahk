;=======================================================================================
;
; Function:			load_config (DinoCode Interpreter)
; Description:		Run DinoCode from a string.
; Return value:		Depending on the resolution of the code it can return a string, a number, or an object.
;
; Author:           BlassGO
;
;=======================================================================================
;   Useful initial concepts:
;                 FD:                 A static Array, where each element corresponds to an "DObj" Object that will store the local variables of DinoCode
;                                       (The first FD is used for "main" and always exists, additional FDs are generally used during the execution of sections (TAGs), and are always destroyed).
;
;   Definitions:
;                 GLOBAL:             Global object that stores global DinoCode definitions
;                                       (Its elements are shared with all existing FDs).
;				  unexpected:         Global String that, when defined from any function, stops the execution of DinoCode and an Error message is built with this exception
;				                        (and additional information).
;                 FD_CURRENT:         Global integer with the identifier of the FD in use
;                                       (It is also a marker when DinoCode is running).
;
;	OnError("RuntimeError"):
;                 A safe to display an Error message at runtime, since it is possible to overload AHK during the evaluation of long expressions.
;
;   ConfigRC:
;                 A function to define native DinoCode variables
;                   (Customizable according to needs).
;
;   Extra functions:
;                 print:              This function must be modified to coexist with the main program
;                                        (Otherwise it won't work).
;                 NOTE:               The "option/help/input" functions use GUI identifiers: new, new2 and new3 respectively (also okay, okay2 and okay3 labels), therefore they are reserved and reuse in the main program is not recommended.
;
;                 
;   lang function:
;                 It consists of several groups/objects with respective identifiers and allows Functions, Connectors or Conditionals to have several possible nicknames,
;                   nicknames must be standardized without diacritics, since these disappear during processing.
;
;   maps function:
;                 It is a group/object that serves to define the semantics or syntax of the sentences, being able to play with Connectors in order to build a standardized list of parameters that will be sent to the respective function.
;                    
;                     EXAMPLE--->  option: {option: {max:2, at:"1,2", atpos:true}, with: {support:true, expand:true, at:3}}
;                     
;                     EXPLAIN----> The main "option" object is the container for all the specifications that the "option" sentence must meet, this includes specifications for both the function itself and its connectors.
;
;                                     max:         Limit of parameters that can receive.
;                                     support:     Special for Connectors, it should always be used to specify that the main action supports that connector.
;                                     literal:     Enables the use of literal arguments, a literal argument does not require Quotes/Delimiter (Similar to a number), BUT, it cannot contain spaces or have the same name of any Connector.
;                                     atpos:       Enables the use of positioning "at" for all Connectors (must be defined in the main Action).
;                                     at:          It establishes in which position the received parameters should be placed before being sent to the function (Positions that have not been covered use empty references, avoiding errors).
;                                     expand:      Enables adding in queue any number of parameters that are received, if combined with "at" it is possible to specify that from N position all the parameters received by the Connector must be added.
;                                     concat:      Indicates that all parameters received by the Connector where "concat" was defined must be added directly to the specified action or connector (without overwriting)
;                                                      EXAMPLE---> option: {option: {max:5}, with: {support:true, concat:"option"}}
;                                                      EXPLAIN---> All the parameters of the "with" connector will be concatenated in queue to those existing in "option", that is, if "option" has 3 parameters, and "with" receives 2 additional ones, they will be added in positions 4 and 5, total: 5 parameters for the "option" function.
;
;                                     ignore_func:  Avoid taking the main action as a function, even if it is (must be defined in the main Action).
;
;   load_config function:
;                configstr:        Code to execute (String).
;                local:            Enable Local work (The code will be executed in a new temporary FD, simulating a local execution).
;                local_obj:        When "local" is enabled, here you can send an additional Object that will be merged with the new local FD (Predefined variables for the new local space).
;                from_fd:          The identifier of any existing FD from which the code should be executed (By default, it works on the first FD).
;                from_type:        Especially used to identify special sections of code.
;                newmain:          If enabled, the entire work environment will be reset, leaving everything ready for the execution of a new Script
;                                     (It must always be used to end the execution of the entire Script, otherwise all the variables defined in the Script will be kept in memory, waiting for the execution of more code).
;                   
;=======================================================================================
; SECURITY RESTRICTIONS
; 
; -> By default, any variables defined in AHK, including native variables (A_) are accessible but NOT modifiable
;      It is possible to "change" its value but it will never affect the AHK variable, it will simply be self-defined within DinoCode
;
; -> If they are global objects, they may be modified, since they are resolved from the original object.
;
; NOTE: Specifying a custom restriction can reduce overall performance because it must be checked every time a variable is defined or its value is obtained.
;       It is recommended to use it only if the security of access to variables / functions is critical
; 
; skip_functions:   It is a Regex that defines which functions CANNOT be called during the execution of DinoCode
; skip_vars: 		It is a Regex that defines which variables should strictly NOT be modified (Read Only Access)
; shared_vars:      It is a Regex that defines in detail which AHK variables are accessible (Once established, the value of any variable that does NOT match is not accessible)
;
; If the same global variable is specified in both shared_vars and skip_vars, it implies that the global variable can be read but NOT modified.
;
/*

	global skip_functions := "i)(?:^(?:load_config|ParseObjects|AssignParse|Expr))"
	global skip_vars := "i)(?:^(?:skip_functions|skip_vars|shared_vars)$)"
	global shared_vars := "i)(?:^(?:A_|(?:FD_CURRENT|clipboard|comspec|errorlevel|programfiles)$))"
*/
;
;=======================================================================================
SetBatchLines -1

; Definitions
global GLOBAL:={}, unexpected, FD_CURRENT, verdadero:=true, falso:=false
OnError("RuntimeError")
OnMessage(KeyDown:=0x100, "__DinoEvent__")

; ConfigRC
config_rc() {
   ; Native
   ; Currently a global definition as AHK variables for constants is preferred
   ; Removed:   GLOBAL.false:=0, GLOBAL.true:=1, GLOBAL.verdadero:=1, GLOBAL.falso:=0
}

; Event control
__DinoEvent__(wParam, lParam, msg, hwnd) {
	static Enter:=0x0D
	if (wParam=Enter)&&(gui:=(A_Gui="new"||A_Gui="new2"||A_Gui="new3") ?  A_Gui : false) {
	   Gui, % gui ":Submit", NoHide
       Gui, % gui ":Destroy"
	}
	return 
}
__DinoGui__(Hwnd,GuiEvent,EventInfo,ErrLevel:="")
{
    GuiControlGet, _var, % A_Gui ":Name", % Hwnd
	_var ? gui(,_var)
}

; Moved to console.ahk

; Extra functions
WaitWindow(id, HiddenWindows:=false) {
   DetectHiddenWindows, % HiddenWindows
   WinWaitClose % id
   return ErrorLevel
}
WinMaximize(id) {
	WinMaximize, % id
}
new(opt*) {
	class:=opt[1], opt.RemoveAt(1)
	if (class&&IsObject(%class%)) {
	   try {	
			return new %class%(opt*)
	   } catch {
			unexpected:="Could not generate object with class -> " . class
	   }
	} else {
		unexpected:="Invalid class name -> " . class
	}
	return 0
}
random(n:=1,n2:=0) {
	if n2
		Random, random, n, n2
	else if n
		Random,, n
	return random
}
repeat(Byref str,n) {
    return StrReplace(Format("{:0" . n . "}",""),0,str)
}
sleep(n) {
   if n is integer
      Sleep, n
   return 1
}
msg(title,d:=262144,msg:="") {
   try
      MsgBox, % d, % title, % msg
   catch
      return 0
   return 1
}
inmsg(as){
   IfMsgBox % as
      return 1
   return 0
}
isNumber(Byref n) {
	if n is Number
	return true
}
isTypeOf(Byref var,type) {
	object:=isObject(var) 
	switch (type)
	{
		case "array","object":
			if object {
				for key in var
					if (key!=A_Index)
						return (type="object")?true:false
				return (type="array")?true:false
			}
			return false
		case "string":
		    if object
			   return false
		    if var is not Number
	   			return true
		default:
			if object
			   return false
		    try {
				if var is %type%
	   				return true
			}
	}
	return false
}
TypeOf(Byref var) {
	if isObject(var) {
		for key in var
		    if (key!=A_Index)
			   return "object"
		return "array"
	} Else If (var="")
		return "empty"
	Else If var is Number
		If var is Integer
			return "integer"
		Else
			return "float"
	Else If var is Space
		return "space"
	Else If var is Alpha
		If var is Upper
			return "uppercase"
		Else If var is Lower
			return "lowercase"
		Else
		    return "alpha"
	Else if var is Alnum
		return "alnum"
	Else if var is Time
	    return "time"
	Else
	   return "string"
}
question(title:="",q:="") {
   (!title) ? title:="Question"
   MsgBox, 262148, % title, % q
   IfMsgBox Yes
      return 1
   return 0 
}
press(key) {
   if isLabel(key) {
      try
         gosub % key
	  catch
	     return 0
   } else {
      unexpected:="Unrecognized label: " . key
	  return 0
   }
   return 1
}
GuiControl(do) {
	Gui(,,,do)
}
GuiControlGet(do) {
	Gui(,,,,do)
}
Gui(do:="",onevent:="",reset:=false,control:="",controlget:="") {
   static
   local args:=0, _label, _var, _random
   (reset||!isObject(labels)) ? (labels:={},vars:=[])
   if do {
	   do:=StrSplit(do,",",,4), args:=do.MaxIndex()
	   for cont, value in do
		  do[cont]:=Trim(value)
	   try {
		   StringCaseSense, Off
		   if (args=1) {
			  if (_at:=InStr(do[1],"return"))&&(_return:=Trim(SubStr(do[1],_at+6)))
				 return _return:=%_return%
		   }
		   if (args>=3) && InStr(do[1],"Add") {
		      RegexMatch(do[3],"v\K\S+",_var) ? (GLOBAL[_var]:="",vars.Push(_var))
			  if RegexMatch(do[3],"g\S+",_label) {
			     if !_var {
				    Random, _random, 1, 1000
				    _var:="_v" . _random . "v_", do[3].=" v" . _var
                 }
				 do[3]:=StrReplace(do[3],_label,"g__DinoGui__",, 1), labels[_var]:=SubStr(_label,2)
			  }
			  is_control:=true
		   }
		   Gui, % do[1], % do[2], % do[3], % do[4]
		   (is_control&&RegexMatch(do[3],"hwnd\K\S+",_var)) ? (GLOBAL[_var]:=%_var%)
		   if InStr(do[1],"submit")
		      for cont, value in vars
                  GLOBAL[value]:=%value%, %value%:=""
	   } catch e {
		   unexpected:=e.message
	   }
   } else if onevent && labels.HasKey(onevent){
       load_config(labels[onevent],,, FD_CURRENT, "resolveblock")
   } else if control {
		do:=StrSplit(control,",",,3)
		for cont, value in do
			do[cont]:=Trim(value)
		try {
			GuiControl, % do[1], % do[2], % do[3]
		} catch e {
			unexpected:=e.message
		}
   } else if controlget {
	    do:=StrSplit(controlget,",",,4)
		for cont, value in do
			do[cont]:=Trim(value)
		try {
			GuiControlGet, _OutputVar, % do[2], % do[3], % do[4]
			if InStr(do[2],"Pos") {
               GLOBAL[do.1 . "X"]:=_OutputVarX, GLOBAL[do.1 . "Y"]:=_OutputVarY, GLOBAL[do.1 . "W"]:=_OutputVarW, GLOBAL[do.1 . "H"]:=_OutputVarH
			} else {
               GLOBAL[do.1]:=_OutputVar
			}
		} catch e {
			unexpected:=e.message
		}
   }
   return
}
option(title,txt,options*) {
   static multi, okay
   (!title) ? title:="Option"
   Gui new: +AlwaysOnTop
   Gui new: Font, s10
   if txt
      Gui new: Add, Text, Y+0, % txt
   for count, value in options {
       if (count=1)
	      Gui new: Add, Radio, AltSubmit vmulti XP Y+10, % value
	   else
          Gui new: Add, Radio, , % value
   }
   Gui new: Add, Button, XS Y+10 h20 w100 vokay gokay, OK
   Gui new: Show, AutoSize Center, % title
   GuiControl, new: Move, okay, x%okay_x%
   Gui new: +LastFound
   WinWaitClose
   return multi
   newGuiClose:
   okay:
   Gui new: Submit, NoHide
   Gui new: Destroy
   return
   newGuiSize:
   okay_x:=(A_GuiWidth - 100) // 2
   okay_y:=(A_GuiHeight - 20) // 2
   return
}
input(txt:="",w:="",h:=""){
   Static input, okay2, create:=true
   (!(h~="[0-9]+")) ? h:=20
   if (w~="[0-9]+") {
      w:="w" . w
   } else if !txt {
      w:="w300"
   } else {
      w:="WP"
	  (StrLen(txt)<30) ? w:=w . "+60"
   }
   Gui new2: -MinimizeBox +AlwaysOnTop
   Gui new2: Font, s10
   if txt
      Gui new2: Add, Text, Y+0, % txt
   Gui new2: Add, Edit, % "XS Y+10 " . w . " h" . h . " vinput",
   Gui new2: Add, Button, XS Y+10 h20 w100 vokay2 gokay2, OK
   Gui new2: Show, AutoSize Center, Input
   GuiControl, new2: Move, okay2, x%okay_x%
   Gui new2: +LastFound
   WinWaitClose
   Gui new2: Destroy
   return input
   new2GuiClose:
   okay2:
   Gui new2: Submit, NoHide
   Gui new2: Destroy
   return
   new2GuiSize:
   okay_x:=(A_GuiWidth - 100) // 2
   okay_y:=(A_GuiHeight - 20) // 2
   return
}
help(image:="",txt:="") {
   static okay3, pic
   if image&&!InStr(FileExist(image), "A")
      return 0
   Gui new3: -MinimizeBox +AlwaysOnTop
   Gui new3: Font, s10
   if txt
      Gui new3: Add, Text, Y+0, % txt
   if image
      Gui new3: Add, Picture, XS Y+10 vpic, % image
   Gui new3: Add, Button, center XS Y+10 h20 w100 vokay3 gokay3, OK
   Gui new3: Show, AutoSize Center, HELP
   GuiControl, new3: Move, okay3, % "x" . (guiw - 100) // 2
   if image {
      GuiControlGet, size, new3: Pos, pic
      GuiControl, new3: Move, pic, % "x" . (guiw - sizeW) // 2
   }
   Gui new3: +LastFound
   WinWaitClose
   okay3:
   Gui new3: Destroy
   return
   new3GuiSize:
   guiw:=A_GuiWidth
   guih:=A_GuiHeight
   return
}
GetFullPathName(path) {
   cc := DllCall("GetFullPathName", "str", path, "uint", 0, "ptr", 0, "ptr", 0, "uint")
   VarSetCapacity(buf, cc*(A_IsUnicode?2:1))
   DllCall("GetFullPathName", "str", path, "uint", cc, "str", buf, "ptr", 0, "uint")
   return buf
}
ReadFile(file,enc:="UTF-8") {
   try {
      FileEncoding, % enc
      FileRead, content, % file
   } catch e {
      unexpected:="Cant read-->""" file """, Ended with: " e.message
   }
   return content
}
WriteFile(file, content:="", enc:="UTF-8-RAW", EOLTranslation:=false) {
   try {
    if !Exist("folder",dest:=dirname(GetFullPathName(file)))
	FileCreateDir, % dest
    FileAppend, % content, % (EOLTranslation?"":"*") . file, % enc
	return 1
   } catch e {
    unexpected:="Cant write in-->""" file """, Ended with: " e.message
	return 0
   }
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
Exist(options*) {
    type:=(options[1]="file"||options[1]="folder"||options[1]="hidden")?options.RemoveAt(1):""
    for count, path in options
    {
        filetype:=FileExist(path)
        switch type
        {
            case "file":
                if !(filetype~="^[^DH]+$")
                    return false
            case "folder":
                if !(InStr(filetype, "D")&&!InStr(filetype, "H"))
                    return false
            default:
                if (filetype="")
                    return false
        }
    }
    return filetype?true:false
}
CreateDir(options*) {
   for key,dir in options {
      if !Exist("folder",dir) {
         FileCreateDir, % dir
         if ErrorLevel
         return false
      }
   }
   return true
}
Delete(options*) {
   for key,dir in options {
        try FileDelete, % dir
        catch
           try FileRemoveDir, % dir, 1
        if ErrorLevel
        return false
   }
   return true
}
Move(orig,dir,replace:=false) {
    if Exist("folder",orig) {
        SplitPath, orig, name
        dir:=(!replace&&Exist("folder",dir)) ? dir . (SubStr(dir,0)="\"?"":"\") . name : dir
        FileMoveDir, % orig, % dir, 2
    } else {
        FileMove, % orig, % dir, 1
    }
    return (ErrorLevel=0)
}
Copy(orig,dir,replace:=false) {
    if Exist("folder",orig) {
        SplitPath, orig, name
        dir:=(!replace&&Exist("folder",dir)) ? dir . (SubStr(dir,0)="\"?"":"\") . name : dir
        FileCopyDir, % orig, % dir, 1
    } else {
        FileCopy, % orig, % dir, 1
    }
    return (ErrorLevel=0)
}
SelectFile(title:="",filter:="",rootdir:="",options:="") {
   try FileSelectFile, result, % options, % rootdir, % title, % filter
   return result
}
SelectFolder(prompt:="",rootdir:="",options:="") {
   try FileSelectFolder, result, % rootdir, % options, % prompt
   return result
}
list(options*) {
   restore:=[]
   while options.MaxIndex() {
      if (SubStr(options[1],1,1)="-") {
         switch (opt:=options.RemoveAt(1)) {
            case "-files","-f":
               mode.="F"
            case "-dirs","-d":
               mode.="D"
            case "-recursive","-r":
               mode.="R"
            case "-pattern","-p":
               pattern:=true
			default:
			   restore.Push(opt)
         }
      } else {
         restore.Push(options.RemoveAt(1))
      }
   }
   mode:=mode?mode:"FD",options:=(restore.MaxIndex())?restore:[A_ScriptDir], restore:=""
   for cont, path in options
   {
      (!pattern&&InStr(FileExist(path), "D")) ? (path .= SubStr(path,0)="\" ? "*" : "\*"):false
      Loop, Files, %path%, %mode%
        files.=((files="")?"":"`n") . A_LoopFileLongPath
   }
   return files
}

; Classes
class DObj
{
    __New(Byref parse:="", Byref may:="") {
	    if IsObject(parse) {
          for key, value in parse
             this[key]:=value
		} else if parse {
			this[parse]:=may?may:{}
		}
    }
    __Get(key) {
		return GLOBAL.HasKey(key)?GLOBAL[key]:((!shared_vars||key~=shared_vars)?%key%:"")
    }
	__Set(key, Byref value) {
        if GLOBAL.HasKey(key) {
            GLOBAL[key]:=value
			return
		} else if (key="Clipboard")||((shared_vars&&key~=shared_vars)&&!(skip_vars&&key~=skip_vars)) {
			%key%:=value
			return
		}
    }
	HasKey(key) {
        return ObjHasKey(GLOBAL,key)||ObjHasKey(this,key)||(key="Clipboard")||((!shared_vars||key~=shared_vars)&&isSet(%key%))
    }
	global(key) {
		GLOBAL[key]:=this[key]
		this.Delete(key)
	}
	parse(parse) {
		for key, value in parse
			this[key]:=value
	}
}
class Thread {
    __New(fn,t,args*) {
        this.fn:=Func(fn)
        this.args:=args
        SetTimer, % this, % t
    }
    Call() {
        this.fn.Call(this.args*)
    }
	Parse(arg){
		SetTimer, % this, % arg
	}
}

; Main functions
LineStr(ByRef S, P, C:="", D:="") {   ;  LineStr v0.9d,   by SKAN on D341/D449 @ tiny.cc/linestr
Local L := StrLen(S),   DL := StrLen(D:=(D ? D : Instr(S,"`r`n") ? "`r`n" : "`n") ),   F, P1, P2 
Return SubStr(S,(P1:=L?(P!=1&&InStr(S,D,,0))?(F:=InStr(S,D,,P>0,Abs(P-1)))?F+DL:P-1<1?1:0:(F:=1)
:0),(P2:=(P1&&C!=0)?C!=""?(F:=InStr(S,D,,(C>0?F+DL:0),Abs(C)))?F-1:C>0?L:1:L:0)>=P1?P2-P1+1:0)
}
StrUnmark(string) {    ; By Lexicos
    len := DllCall("Normaliz.dll\NormalizeString", "int", 2
        , "wstr", string, "int", StrLen(string)
        , "ptr", 0, "int", 0)  ; Get *estimated* required buffer size.
    Loop {
        VarSetCapacity(buf, len * 2)
        len := DllCall("Normaliz.dll\NormalizeString", "int", 2
            , "wstr", string, "int", StrLen(string)
            , "ptr", &buf, "int", len)
        if len >= 0
            break
        if (A_LastError != 122) ; ERROR_INSUFFICIENT_BUFFER
            return
        len *= -1  ; This is the new estimate.
    }
    ; Remove combining marks and return result.
    return RegExReplace(StrGet(&buf, len, "UTF-16"), "\pM")
}
lang(key:="",str:="",regex:=false,group:="langs",add:="",reset:=false) {
   static custom, custom_main, return, connectors, condition, scripts, signal, langs, case
   str ? str:=StrUnmark(str)
   if reset||!isObject(return) {
       custom:={}, custom_main={}
       return:=
       (join
          {
             return: ["retornar"]
		  }
	   )
       connectors:=
       (join
          {
             in: ["en"],
			 with: ["=", "con"],
			 to: ["unto", "hacia"]
		  }
	   )
       condition:=
       (join
          {
             if: ["si"],
			 else: ["sino"],
			 switch: ["cambiar"],
			 while: ["mientras"],
			 until: ["hasta"],
			 do: ["hacer"],
			 use: ["usar"],
			 for: ["para"],
			 forchar: ["paracaracter"],
			 forobj: ["paraobjeto"]
		  }
	   )
	   case:=
	   (join
          {
             case: ["caso"],
			 default: ["pordefecto"]
		  }
	   )
       scripts:=
       (join
          {
             js: ["javascript"],
			 vbs: ["vbscript"]
		  }
	   )
       signal:=
       (join
          {
             all: ["todo"],
			 main: ["principal"]
		  }
	   )
	   langs:=
	   (join
		  {
		     escape: [],
			 global: [],
			 setworkingdir: [],
			 new: ["nuevo"],
			 set: ["definir"],
			 section: ["seccion"],
			 thread: ["hilo"],
			 press: ["presionar"],
			 import: ["importar"],
			 sleep: ["dormir"],
			 print: ["imprimir","escribir"],
			 msg: ["message","mensaje"],
			 input: ["entrada","leer"],
			 question: ["pregunta"],
			 option: ["opcion"],
			 break: ["romper"],
			 continue: ["continuar"],
			 abort: ["abortar"],
			 exit: ["salir"],
			 help: ["ayuda"],
			 readfile: ["leerarchivo"],
			 writefile: ["escribirarchivo"],
			 waitwindow: ["esperarventana"],
			 waitconsole: ["esperarconsola"],
			 startconsole: ["iniciarconsola"],
			 readconsole: ["leerconsola"]
		  }
	   )
   }
   if (add&&group) {
      custom[group]:={}, to_add:={}, to_return:=0, _pos:=1
      while,(_pos:=RegexMatch(add,"([a-zA-Z_$][a-zA-Z0-9_$]*)(?:[\s=]+(?:\[\s*((?:[^\s\,\]]+[\s\,]*)+)\]))?\s*(?:\,|$)",_with,_pos+StrLen(_with))) {
		if (_with1=group) {
		   if _with2 {
			   custom_main[group]:=[], to_add[_with1]:={}, to_return:=2
			   Loop, parse, _with2, % A_Space . ","""
			   {
			       if A_LoopField {
					  if (_at:=InStr(A_LoopField,"="))&&(_prop:=SubStr(A_LoopField,1,_at-1)) {
						  if (_prop~="^[a-zA-Z_$][a-zA-Z0-9_$]*$") {
						     to_add[_with1][_prop]:=SubStr(A_LoopField,_at+1), maps:=true
						  } else {
							 unexpected:="Invalid Prop name--->" . _prop
							 break 2
						  }
					  } else {
					     custom_main[group].Push(StrUnmark(A_LoopField))
					  }
				   }
			   }
		   }
		} else {
			custom[group][_with1]:=[StrUnmark(_with1)], to_add[_with1]:={}, (!to_return) ? to_return:=1
			if _with2 {
			   Loop, parse, _with2, % A_Space . ","""
			   {
			      if A_LoopField {
					  if (_at:=InStr(A_LoopField,"="))&&(_prop:=SubStr(A_LoopField,1,_at-1)) {
						  if (_prop~="^[a-zA-Z_$][a-zA-Z0-9_$]*$") {
						     to_add[_with1][_prop]:=SubStr(A_LoopField,_at+1), maps:=true
						  } else {
							 unexpected:="Invalid Prop name--->" . _prop
							 break 2
						  }
					  } else {
					     custom[group][_with1].Push(StrUnmark(A_LoopField))
					  }
				   }
			   }
		    }
		}
	  }
	  (!to_return) ? (unexpected:="Invalid Conector definition--->" . add)
	  (!unexpected) ? (maps ? maps(group,to_add))
	  return to_return
   }
   if (group="custom") {
       if custom.HasKey(key) {
		   for keylang, value in custom[key]
			  for cont, each_value in value
				 if (each_value=str)
				    return keylang
	   }
   } else if key {
       if regex {
	       for cont, each_value in %group%[key]
			   (regex_list ? regex_list.="|" : false), regex_list.=each_value
		   return regex_list
	   }
	   for cont, each_value in %group%[key]
		  if (each_value=str)
			 return 1
   } else {
	   for keylang, value in %group% {
		   if (keylang=str)
		      return keylang
		   for cont, each_value in value
		      if (each_value=str)
			     return keylang
	   }
   }
}
maps(key:="",add:="",reset:=false) {
   static maps
   if reset||!isObject(maps) {
       ; AHK does not support very long expressions, it is necessary to split the object
       maps:=
       (join
          {
		     global: {global: {literal:3}},
			 import: {import: {literal:true}},
			 section: {section: {max:1, literal:true}},
			 thread: {thread: {max:3, literal:true}},
		     print: {print: {max:3}},
			 cmd: {cmd: {literal:true}},
			 new: {new: {literal:-1}, with: {support: true, concat:"new"}},
		     set: {set: {max:1, literal:-1}, with: {support: true}},
			 setworkingdir: {setworkingdir: {max:1}},
		     msg: {msg: {max:2, at:"1,2", atpos:true}, with: {support:true, max:1, at:3}},
			 question: {question: {max:1, at:1, atpos:true}, with: {support:true, max:1, at:2}},
             option: {option: {max:2, at:"1,2", atpos:true}, with: {support:true, expand:true, at:3}},
			 gui: {gui: {literal:true, max:1}},
			 escape: {escape: {literal:true, max:1}},
			 eval: {eval: {literal:true, max:1, ignore_func:true}},
			 for: {for: {literal:-1, max:2}, in: {support: true, literal:3}},
			 forchar: {forchar: {max:1}, in: {support: true, literal:3}},
			 forobj: {forobj: {literal:-1, max:2}, in: {support: true, literal:3}},
			 switch: {switch: {max:2}}
		   }
		)
	   ; Custom definitions
	   maps2:=
       (join
          {
			 list: {list: {literal:true}},
			 exist: {exist: {literal:true}, with: {support: true, concat:"exist"}},
			 SelectFile: {SelectFile: {max:2, at:"1,4", atpos:true}, with: {support:true, max:1, at:2}, in: {support:true, max:1, at:3}},
			 SelectFolder: {SelectFolder: {max:2, at:"1,3", atpos:true}, in: {support:true, max:1, at:2}}
		  }
	   )
	   for k, value in maps2
	       maps[k]:=value
   }
   if (key&&add) {
      (!isObject(maps[key])) ? maps[key]:={}
	  for k, value in add {
		if isObject(maps[key][k]) {
           for k2, value2 in value
		      maps[key][k][k2]:=value2
		} else {
		   maps[key][k]:=value
		}
	  }
   }
   return maps[key]
}
GetNextToken(ByRef s, Byref p, len:=0, Byref _chr:="", Byref _lastchr:="", Byref stop:=false, delimiter:=";")
{
    static $Quote=Chr(34), Deref:=Chr(4)
	p:=p?p:1,len:=len?len:StrLen(s),_chr:=_lastchr:=stop:=""
	while,p>0&&p<=len
	{
		_char:=SubStr(s,p,1)
		if (_char=A_Space||_char=A_Tab)
		{
		if word
		break
		else
		p++
		} else if _char=(
        _chr:="%", _lastchr:=_char, at:=word?at:p, p:=EnclosingWithQuotes(s,p,,,len)
		else if _char=[
        _chr:="%", _lastchr:=_char, at:=word?at:p, p:=EnclosingWithQuotes(s,p,"[","]",len)
		else if word&&_char="."
		_chr:="%", _lastchr:=_char, p+=1
		else if (_char=delimiter||_char="#")
        stop:=_char
        else if !word
        (_char=$Quote)?(_chr:=_char, at:=p, (p:=InStr(s,$Quote,false,p+1))?(p+=1):(at:=0, p:=-1)):((_char=Deref)?(_chr:=SubStr(s,p+1,1), word:=p, p+=1):(word:=p, p+=1))
        else 
        p++
		if at||stop
		break
	}
    return,word?SubStr(s,word,p-word):(at?SubStr(s,at,p-at):"")
}
GetNextObjRef(ByRef s, Byref word, Byref p, len:=0)
{
	static Chars:="_,$,``," . Chr(4)
	p:=p?p:1,len:=len?len:StrLen(s)
	while,p>0&&p<=len
	{
		_char:=SubStr(s,p,1)
		if _char in `t, ,)
        {
		if objref
		break
		else
		word:=0, p+=1
		} else if word {
		if _char=(
		(p:=EnclosingWithQuotes(s,p,,,len))?true:(word:=0, p:=-1)
		else if _char=[
		objref:=true, (p:=EnclosingWithQuotes(s,p,"[","]",len))?true:(word:=0, p:=-1)
		else if _char=.
		objref:=true, p+=1
		else if _char in %Chars%
		p++
		else if _char is alnum
		p++
		else if objref
		break
		else
		word:=0, p+=1
        } else if _char in %Chars%
        word:=p, p+=1
		else if _char is alpha
		word:=p, p+=1
        else
        p++
	}
    return,(objref&&word)?SubStr(s,word,p-word):""
}
GetNextMethod(ByRef s, Byref p, len:=0, Byref isdot:=false)
{
	p:=p?p:1,len:=len?len:StrLen(s),isdot:=false
	while,p>0&&p<=len
	{
		_char:=SubStr(s,p,1)
		if (_char=A_Space||_char=A_Tab)
        break
		else if _char=.
        method:=p,isdot:=!_key,p:=_key?p:p+1
        else if _char=(
        method:=p, p:=_key?p:EnclosingWithQuotes(s,p,,,len)
        else if _char=[
        method:=p, p:=_key?p:EnclosingWithQuotes(s,p,"[","]",len)
        else if _key=
        _key:=p
        if method
        break
        p++
	}
    return,_key?(SubStr(s,_key,p-_key)):(method?SubStr(s,method,p-method):"")
}
EscapeExpr(Byref read_line, Byref _result, len:=0, Byref _hasexpr:=0, Byref _maxexpr:=0) {
	static Deref:=Chr(4),$Expr:="$",$ExprP:="$(",$ExprChr:="``"
    _pos:=1,_extra:=0,_last:=1,resolvedstr:="",read_line_len:=len?len:StrLen(read_line)
    while,_pos:=InStr(read_line,$ExprP,false,_pos+_extra)
    {
		if (SubStr(read_line,_pos-1,1)=$Expr)
		resolvedstr.=SubStr(read_line, _last, _pos-_last), _end:=_pos+1
		else if _end:=EnclosingExpr(read_line,_pos,,,read_line_len)
		_result.Push(SubStr(read_line,_pos+2,_end-_pos-3)), _maxexpr:=_result.MaxIndex(), resolvedstr.=SubStr(read_line, _last, _pos-_last) . Deref . $ExprChr . _maxexpr . $ExprChr . Deref, (A_Index=1)?_hasexpr:=_maxexpr:false
		else {
			unexpected:="Expression -> '$(' without closing -> ')'"
			break
		}
		_extra:=_end-_pos,_last:=_end
    }
    return,unexpected?"":((_last>1)?resolvedstr . SubStr(read_line, _last):read_line)
}
EscapePercent(Byref read_line, Byref _evaluated, Byref FD, just_append:=false) {
    static Deref:=Chr(4),$Percent:="%"
    _pos:=1,_extra:=0,_last:=0,resolvedstr:=""
    while,_pos:=InStr(read_line,$Percent,false,_pos+_extra)
    {
		_end:=InStr(read_line,$Percent,false,_pos+1)
		if _end=0
		break
		if ((_end-_pos)-1=0)
		resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . $Percent
		else {
			_eval:=Eval(SubStr(read_line,_pos+1,(_end-_pos)-1),FD,,_escape,,,3)
			if unexpected
	  		break
			if just_append||isNumber(_eval)
			resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . _eval
			else
			_evaluated.Push(_eval), resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . Deref . "~" . _evaluated.MaxIndex() . "~" . Deref
		}
		_eval:="",_extra:=(_end-_pos)+1,_last:=_end
    }
    return,unexpected?"":((_last)?resolvedstr . SubStr(read_line, _last+1):read_line)
}
EscapeStr(Byref read_line, Byref Escape, Byref _escape) {
    static Deref:=Chr(4),$Quote=Chr(34),$Quotes=Chr(34) . Chr(34)
    _pos:=1,_extra:=0,_last:=0,resolvedstr:=""
    while,_pos:=InStr(read_line,$Quote,false,_pos+_extra)
    {
        _end:=_pos,is_quoted:=0
        Loop
        _end:=InStr(read_line,$Quote,false,_end+1)
		Until,(_end=0)||!(SubStr(read_line,_end+1,1)=$Quote&&(is_quoted:=_end+=1))
        if _end=0
		{
            unexpected:="A closure was expected--->"""
            break
        } else {
			((_end-_pos)-1=0) ? (resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . $Quotes):(_escape.Push(EscapeChars(is_quoted?StrReplace(SubStr(read_line,_pos+1,(_end-_pos)-1),$Quotes,$Quote):SubStr(read_line,_pos+1,(_end-_pos)-1), Escape)), resolvedstr.=SubStr(read_line, _last+1, (_pos-_last)-1) . $Quote . Deref . "&" . _escape.MaxIndex() . "&" . Deref . $Quote)
			_extra:=(_end-_pos)+1,_last:=_end
        }
    }
    return,unexpected?"":((_last)?resolvedstr . SubStr(read_line, _last+1):read_line)
}
EscapeChars(Byref _string, Byref Escape) {
    static Deref:=Chr(4),_escapechars:={78:"`r`n",110:"`n",97:"`a",98:"`b",102:"`f",114:"`r",116:"`t",118:"`v"}
    resolved:="",start:=1,end:=0,last_skip:=0,carefully:=(Escape="&"||Escape="~"||Escape="``")
    while,start:=InStr(_string,Escape,false,start)
    is_ref:=(carefully&&start-1>end&&SubStr(_string,start-1,1)=Deref), resolved.=SubStr(_string, last_skip+1, start-1-last_skip), (is_ref&&(end:=InStr(_string, Deref, false, start+1))) ? (resolved.=SubStr(_string, start, end-start+1), start:=end):(start+=1, char:=Substr(_string, start, 1), resolved.=((_escapechars[key:=Asc(char)])="") ? char : _escapechars[key]), last_skip:=start, start+=1
    return,last_skip?resolved . Substr(_string, last_skip+1):_string
}
solve_escape(ByRef str, ByRef from:="", key:="&") {
	static Deref:=Chr(4)
    resolved:="",resolved_end:=0
    Loop,Parse,str,% Deref
	resolved.=((Mod(A_Index,2)=0)&&IsNumber(_index:=SubStr(A_LoopField,2,-1)))?(((_chr:=SubStr(A_LoopField,1,1))&&_chr=key)?from[_index]:Deref . A_LoopField . Deref):A_LoopField, resolved_end:=A_Index
    return,(resolved_end>1)?str:=resolved:str
}
solve_any_escape(ByRef str, Byref _escape:="", Byref _result:="", ByRef _evaluated:="", Byref _hasexpr:=0) {
	static Deref:=Chr(4)
    resolved:="",resolved_end:=0
    Loop,Parse,str,% Deref
	resolved.=((Mod(A_Index,2)=0)&&IsNumber(_index:=SubStr(A_LoopField,2,-1))&&(_chr:=SubStr(A_LoopField,1,1)))?((_chr="&")?(solve_escape_string(_escape[_index],_result,_evaluated,_hasexpr)):((_chr="~")?_evaluated[_index]:((_chr="``")?_result[_index]:Deref . A_LoopField . Deref))):A_LoopField, resolved_end:=A_Index
    return,(resolved_end>1)?resolved:str
}
solve_escape_string(ByRef str, Byref _result:="", ByRef _evaluated:="", Byref _hasexpr:=0) {
	static Deref:=Chr(4)
    resolved:="",resolved_end:=0
    Loop,Parse,str,% Deref
	resolved.=((Mod(A_Index,2)=0)&&IsNumber(_index:=SubStr(A_LoopField,2,-1))&&(_chr:=SubStr(A_LoopField,1,1)))?((_chr="~")?(_evaluated[_index]):((_chr="``")?((_index<=_hasexpr)?_result[_index]:((_hasexpr+=1)?load_config(_result[_index],,,FD_CURRENT,"resolve",,false):"")):Deref . A_LoopField . Deref)):A_LoopField, resolved_end:=A_Index
    return,(resolved_end>1)?resolved:str
}
load_config(configstr:="",local:=false,local_obj:="",from_fd:=0,from_type:="",newmain:=false,ResetEval:=true) {
   global unexpected,secure_user_info,config_tracking
   static IndentChar:=Chr(1),SpaceTAB:=A_Space . A_Tab,LineBreak:="`n",Delimiter:=Chr(34),Escape:=Chr(96),Deref:=Chr(4),last_label,main_nickname,script_section,FD,SIGNALME,_thread,_escape,_result,_evaluated,read_line_,ARGS,to_return
   (newmain||!isObject(FD)) ? (last_label:="",FD_CURRENT:="",unexpected:="",main_nickname:="",script_section:={},GLOBAL:={},FD:={1:new DObj({main: A_Args})},SIGNALME:={exit:1, def:{}},_escape:=[],_result:=[],_evaluated:=[],_thread:={},lang(,,,,,true),maps(,,true),gui(,,true),config_rc())
   if configstr {
	   local ? (outfd:=FD.MaxIndex()+1, FD[outfd]:=new DObj(local_obj), last_label ? SIGNALME.main:={[outfd]: last_label}) : (FD.HasKey(from_fd) ? (outfd:=from_fd, FD[outfd].Parse(local_obj)) : outfd:=1)
	   if (outfd>=100) {
	      MsgBox, 262160, Config File, % "Memory limit exceeded for local recursive calls (" . outfd . ")"
	      SIGNALME.code:=1.1, SIGNALME.exit:=0, FD:=""
	      return 0
	   }
	   StringCaseSense, Off
	   to_return:=0, line:=0, total:=1, _pos:=0, unexpected:="", is_partline:=(from_type="resolvepart"), is_callmulti:=(from_type="resolveblock"), is_callout:=(from_type="resolve"), (!(newmain||is_partline||is_callout||is_callmulti))?(_escape:=[],_result:=[],_evaluated:=[]):newmain:=false
	   if !(is_partline||is_callout)
	   while,_pos:=InStr(configstr,LineBreak,false,_pos+1)
	   total++
   } else {
      return 0
   }
   Loop,parse,configstr,`n,`r 
   {
      line+=1
	  if multi_note {
		(SubStr(RTrim(A_LoopField,SpaceTAB),-1)="*#")?multi_note:=false
		continue
	  }
	  _startchr:="",_pos:=0,is_callout?(line_indent:=0, read_line:=LTrim(A_LoopField,SpaceTAB)):((SubStr(A_LoopField,1,1)=IndentChar)?(_pos:=InStr(A_LoopField,IndentChar,false,2), line_indent:=SubStr(A_LoopField,2,_pos-2), read_line:=_pos?SubStr(A_LoopField,_pos+1):"", is_compiled:=true):(line_indent:=GetIndent(A_LoopField,_pos), read_line:=SubStr(A_LoopField,_pos))), is_to_var:=(to_var.var="")?"":(line_indent>to_var.indent)
	  if !is_to_var {
		if (read_line=""&&is_to_var=""&&line!=total)
		continue
		_startchr:=SubStr(read_line,1,2), read_line2:=read_line
		if !is_compiled&&(SubStr(_startchr,1,1)="#") {
			(SubStr(_startchr,2,1)="*")?multi_note:=true
			continue
		}
	  }
	  if (SubStr(_startchr,1,1)=":") {
	     tag:=StrReplace(SubStr(read_line,2),A_Space)
		 (script&&section) ? (script_section[section]:=script,script:="",section:="")
		 (_def:=InStr(tag,"->")) ? (_cdef:=SubStr(tag,_def+2),tag:=SubStr(tag,1,_def-1))
		 section:=(tag="main")?"":((tag~="^[a-zA-Z0-9_$]+$") ? tag : (unexpected:="Invalid tag--->" . tag))
		 (_def&&tag&&!unexpected) ? (((_def:=lang(,,,tag,_cdef))&&(SIGNALME.def[tag]:=true)),(_def=2) ? main_nickname:=true) : false
		 if unexpected
		    break
	     else
		    continue
	  } else if section {
	     script.=(is_compiled?A_LoopField:IndentChar . line_indent . IndentChar . read_line) . LineBreak
		 (line=total) ? (script_section[section]:=script,script:="",section:="")
		 continue
	  } else if (Floor(SIGNALME.code)=1) {
	     if (SIGNALME.code=1.1||(SIGNALME.code=1&&last_label&&last_label=SIGNALME[outfd].from)||(SIGNALME.code=1.2&&last_label!=SIGNALME.main[outfd]))
		    return SIGNALME.exit
		 else
		    SIGNALME.code:=""
	  }
	  FD_CURRENT:=outfd,main_type:=main_action:=main_orig:=_hasexpr:=_maxexpr:=""
	  if block_type {
	     if (line_indent>block_indent) {
		    if !block_result {
			   last_label:=back_label
			   continue
			}
	        block_capture.=LineBreak . ((block_key="use") ? StrReplace(Format("{:0" . (line_indent-block_indent)-1 . "}",""),0,A_Space) . read_line : (is_compiled?A_LoopField:IndentChar . ((line_indent-block_indent)-1) . IndentChar . read_line))
			if (line!=total)
		       continue
			else
			   last_line_on_block:=true
		 }
		 if !(block_capture=""||block_capture=LineBreak) {
		   if (block_key="if"||block_key="else") {
			  to_return:=load_config(block_capture,,,outfd,block_type)
		   } else if (block_key="while") {
			  while,block_result {
			     to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
				 if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
					break
				 else if (SIGNALME.code=3.1) {
					SIGNALME.code:=""
					continue
				 }
				 block_result:=Eval(block_with, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,4)?1:0
			  }
		   } else if (block_key="until") {
			  Loop {
				 to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
				 if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
					break
				 else if (SIGNALME.code=3.1) {
					SIGNALME.code:=""
					continue
				 }
				 block_result:=Eval(block_with, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,4)?1:0
			  } until,block_result
		   } else if (block_key="use") {
		      if (config_tracking=1) {
				  MsgBox, 262145, % "Code Block--->" . last_label . "--->" . block_with1, % block_capture
			      IfMsgBox Cancel
				  {
				     SIGNALME.code:=1.1, SIGNALME.exit:=0
			         return 0
				  }
		      }
			  (block_with="js") ? id:="{16d51579-a30b-4c8b-a276-0ff4dc41e755}" : (block_with="vbs") ? id:="VBScript"
			  try {
				  script:=new ActiveScript(id)
				  script.AddObject("Dino",FD[outfd],true)
				  script.Exec(block_capture)
			   } catch e {
				  from_type:=block_type
				  RegExMatch(e.Message, "s)\s*Line:\s*\K(.*?)(?=\s|\R|$)", line), read_line2:=RegexReplace(LineStr(block_capture,line,1), "\s*(.*)", "$1")
				  if !RegExMatch(e.Message, "s)\s*Description:\s*\K(.*?)(?=\R|$)", unexpected)
					 unexpected:="Invalid syntax\logic"
			   }
		   } else if (block_key="for") {
			  _for:=read_line_, read_line_:=""
			  if isNumber(SubStr(_for.for.1,1,2)) {
				(isNumber(SubStr(_for.for.2,1,2))) ? FD[outfd].Index:=Index:=_for.for.1 : (FD[outfd].Index:=Index:=1, _for.for.2:=_for.for.1)
				while,Index<=_for.for.2
				{
				    to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
					if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
					break
					FD[outfd].Index:=Index+=1
					if (SIGNALME.code=3.1){
						SIGNALME.code:=""
						continue
					}
				}
			  } else if _for.for.2 {
				Index:=1, _for_vars:=[_for.for.1, _for.for.2]
                for key,val in _for.in {
					for key2, val2 in Eval(val, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,2) {
						FD[outfd][_for_vars.1]:=key2, FD[outfd][_for_vars.2]:=val2, FD[outfd].Index:=Index,to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
						if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
						break 2
						else if (SIGNALME.code=3.1){
							SIGNALME.code:=""
							continue
						}
						Index++
					}
				}
			  } else {
				_for_vars:=[_for.for.1]
				for key,val in _for.in {
					FD[outfd][_for_vars.1]:=Eval(val, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,2), FD[outfd].Index:=A_Index
					to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
					if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
					break
					else if (SIGNALME.code=3.1){
						SIGNALME.code:=""
						continue
					}
				}
			  }
			  _for:=""
		   } else if (block_key="forchar") {
			  _for_vars:=["Section"]
			  Loop,Parse,% Eval(read_line_.in.1, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,2), % read_line_.forchar.1
			  {
					FD[outfd].Section:=A_LoopField, FD[outfd].Index:=A_Index,to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
					if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
					break
					else if (SIGNALME.code=3.1){
						SIGNALME.code:=""
						continue
					}
			  }
		   } else if (block_key="forobj") {
			  	Index:=1, _for:=read_line_, read_line_:="", _for_vars:=[_for.forobj.1, _for.forobj.2]
                if _for.forobj.2 {
					for key,val in _for.in {
						for key2,val2 in Eval(val, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,2) {
							FD[outfd][_for_vars.1]:=key2, FD[outfd][_for_vars.2]:=val2, FD[outfd].Index:=Index,to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
							if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
							break 2
							else if (SIGNALME.code=3.1){
								SIGNALME.code:=""
								continue
							}
							Index++
						}
					}
				} else {
					for key,val in _for.in {
						for key2 in Eval(val, FD,_Objects,_escapetmp,_resulttmp,_evaluatedtmp,2) {
							FD[outfd][_for_vars.1]:=key2, FD[outfd].Index:=Index,to_return:=load_config(block_capture,,,outfd,block_type),FD_CURRENT:=outfd
							if isObject(SIGNALME.unexpected)||(SIGNALME.code&&SIGNALME.code<=3)
							break 2
							else if (SIGNALME.code=3.1){
								SIGNALME.code:=""
								continue
							}
							Index++
						}
					}
				}
				_for:=""
		   } else if (block_key="switch") {
			  _switch:=read_line_.switch.1, _switch_sensitive:=read_line_.switch.2, _case_indent:=_switch_block:=_switch_pass:=""
			  Loop,parse,block_capture,`n
   			  {
				if (_pos:=InStr(A_LoopField,IndentChar,false,1,2))&&(_case_indent=""||SubStr(A_LoopField,2,_pos-2)<=_case_indent) && (_endword:=WhileWord(A_LoopField,_pos+1)) {
					if _key:=lang(,SubStr(A_LoopField,_pos+1,_endword-_pos),,"case")
					{
						if _switch_pass
						break
						_case_indent:=SubStr(A_LoopField,2,_pos-2)
						if (_key="default") {
							_switch_pass:=true
						} else {
							for key,val in Eval(EscapeStr(SubStr(A_LoopField, _endword+1),Escape,_escape),FD,,_escape)
								if _switch_pass:=_switch_sensitive?_switch==val:_switch=val
								break
						}
					} else {
						unexpected:="Only ""CASE"" blocks can exist within a SWITCH block", orig_block.="`n               " . SubStr(A_LoopField,_pos+1)
						break
					}
				} else if _switch_pass
				_switch_block.=LineBreak . A_LoopField
			  }
			  if !(_switch_block=""||_switch_block=LineBreak)
			  to_return:=load_config(_switch_block,,,outfd,block_type)
		   }
		   block_capture:="", last_label:=back_label
		   (unexpected&&!from_type) ? from_type:=block_type
		   Switch Floor(SIGNALME.code)
		   {
				case 1:
					return,SIGNALME.exit
				case 2:
					return,(is_nested:=lang(,from_type,,"condition"))?to_return:Eval(to_return, FD,,_escape,_result,_evaluated,2), SIGNALME.code:=is_nested?SIGNALME.code:""
				case 3:
					if is_loop
					SIGNALME.code:=""
					else
					return,SIGNALME.exit
		   }
		 } else if block_result {
		   unexpected:="At least one action was expected for the """ . block_type . """ block"
		 }
         if unexpected||isObject(SIGNALME.unexpected) {
		    last_label:=back_label,(!(block_key="use"&&read_line2)) ? (read_line2:=orig_block,line:=orig_line)
			break
	     } else {
			if (block_key="for")||(block_key="forchar")||(block_key="forobj") {
               for key, val in _for_vars
			       FD[outfd][val]:=""
			   FD[outfd].Index:=Index:=_for_vars:=""
			}
		    block_type:=block_with:=with_partial:=is_loop:=""
		 }
		 if last_line_on_block {
		    last_line_on_block:=false
		    continue
		 }
		 FD_CURRENT:=outfd, last_label:=back_label, _Objects:=_escapetmp:=_resulttmp:=_evaluatedtmp:=""
	  }
	  if (config_tracking=1)&&!(is_callout||is_callmulti)&&(read_line!="") {
		 if to_var.var {
			MsgBox, 262145, % "To Var--->" . to_var.var  . "   ( " . line . "/" . total . " )", % read_line
		 } else if last_label {
			MsgBox, 262145, % "Block Line--->" . last_label  . "   ( " . line . "/" . total . " )", % read_line
		 } else {
			MsgBox, 262145, % "Main Line   ( " . line . "/" . total . " )", % read_line
		 }
		 IfMsgBox Cancel
		 {
		   SIGNALME.code:=1.1, SIGNALME.exit:=0
		   return 0
		 }
	  }
	  if (is_to_var!="") {
	     if is_to_var {
			read_line:=EscapeChars(read_line, Escape), read_line:=EscapePercent(read_line,_evaluated,FD,true)
			if unexpected
	  		break
            to_var.content.=((to_var.content="")?"":"`n") . StrReplace(Format("{:0" . (line_indent-to_var.indent)-1 . "}",""),0,A_Space) . read_line
			if (line!=total)
		       continue
			else
			   last_line_on_var:=true
		 }
		 if (to_var.content!="") {
            FD[outfd][to_var.var]:=to_var.content
		 } else {
			unexpected:="At least one line was expected for the variable--->" .  to_var.var
			last_label:=back_label,read_line2:=orig_var,line:=orig_line
			break
		 }
		 to_var:=""
		 if last_line_on_var {
		    last_line_on_var:=false
		    continue
		 }
      }
	  if (SubStr(_startchr,1,1)=">") {
		 read_line:=EscapeChars(LTrim(SubStr(read_line,2),SpaceTAB), Escape), read_line:=EscapePercent(read_line,_evaluated,FD,true), to_var:={var:(_endword:=WhileWord(read_line,1))?SubStr(read_line,1,_endword):"",indent:line_indent,content:LTrim(SubStr(read_line,_endword+1),SpaceTAB)}, orig_var:=read_line2, orig_line:=line
		 if !(to_var.var~="^[a-zA-Z_$][a-zA-Z0-9_$]*$") {
			unexpected:="Invalid variable name--->" . to_var.var
			break
		 }
		 continue
	  }
	  is_partline?false:(read_line:=EscapeExpr(read_line,_result,,_hasexpr,_maxexpr), read_line:=EscapePercent(read_line,_evaluated,FD), read_line:=EscapeStr(read_line,Escape,_escape)), read_line_len:=StrLen(read_line), condition1:=(_endword:=WhileWord(read_line,1,read_line_len))?SubStr(read_line,1,_endword):""
	  if (condition1&&block_key:=lang(,condition1,,"condition")) {
		 block_capture:="", block_indent:=line_indent, block_type:=condition1, block_with:=SubStr(read_line,_endword+1), orig_block:=read_line2, orig_line:=line, back_label:=last_label, back_key:=block_key
		 if (block_key="else") {
			if (block_result="") {
			   unexpected:="Expected a conditional block prior to """ . block_type . """"
			   break
			} else if !else_used {
				block_with:=Trim(block_with,SpaceTAB)
				if (block_with!="") {
					if (condition1:=(_endword:=WhileWord(block_with))?SubStr(block_with,1,_endword):"") && (block_key:=lang(,condition1,,"condition"))
						block_type:=condition1, block_with:=SubStr(block_with,_endword+1)
					else {
						unexpected:="Unrecognized conditional block type: " . condition1
						break
					}
				} else {
					block_with:=1
				}
			} else {
			   block_result:=block_with:=0
			   continue
			}
		 }
		 is_loop:=(block_key="while"||block_key="until"||block_key="for"||block_key="forchar"||block_key="forobj")
		 if (block_key="for"||block_key="forchar"||block_key="forobj"||block_key="switch") {
			if is_loop
			_escapetmp:=_escape,_resulttmp:=_result,_evaluatedtmp:=_evaluated
			with_partial:=block_key, block_with:=""
		 } else if is_loop&&InStr(block_with, Deref) {
			_escapetmp:=_escape,_resulttmp:=_result,_evaluatedtmp:=_evaluated
		 }
		 last_label:=block_type
		 if (block_key="use") {
			 solve_escape(block_with, _evaluated, "~")
			 if RegExMatch(block_with,"(\S+)",block_with) {
				if (block_with:=lang(,block_with,,"scripts")) {
				   block_result:=1
				   if secure_user_info&&!question("ActiveScript", """" . block_with1 . """ code is going to be executed, do you want to allow it?`n`nNOTE: This code will not have a security check, be careful")
					  block_result:=0
				} else {
				   unexpected:="Unrecognized code block type--->" . block_with1
				}
			 } else {
				unexpected:="Expected code block type"
			 }
		 } else {
			 block_result:=(with_partial)?true:(Eval(block_with:=ExprCompile(block_with,_Objects), FD, _Objects,_escape,_result,_evaluated,4)?1:0)
			 ,(is_loop&&Floor(SIGNALME.code)=3)?SIGNALME.code:="":false, (block_key="until")?block_result:=!block_result:false, else_used:=(back_key="else")?(else_used?else_used:block_result):block_result
		 }
		 if unexpected {
			last_label:=back_label
			break
		 } else if !with_partial {
			continue
		 }
	  } else if (condition1&&lang(,condition1,,"return"))
	  	return,(from_type&&lang(,from_type,,"condition")&&SIGNALME.code:=2)?SubStr(read_line,_endword+1):Eval(SubStr(read_line,_endword+1), FD,,_escape,_result,_evaluated,2)
	  if _hasexpr&&!(with_partial="for"||with_partial="forobj") {
		_resulttmp:=_result,_evaluatedtmp:=_evaluated,_escapetmp:=_escape,_result:=[],_evaluated:=[]
		while,(_hasexpr<=_maxexpr)
		{
			_resulttmp[_hasexpr]:=load_config(_resulttmp[_hasexpr],,,outfd,"resolve")
			if unexpected
			break 2
			_hasexpr++
		}
		_result:=_resulttmp, _evaluated:=_evaluatedtmp, _escape:=_escapetmp, _resulttmp:="", _evaluatedtmp:="", _escapetmp:=""
	  }
	  _pos:=1, _lastoption:="", read_line_:={}
	  while,(_token:=GetNextToken(read_line,_pos,read_line_len,_chrfound,_lastchr,_stop))!=""
	  {
		 option:=(_chrfound="~")?solve_escape(_token,_evaluated,"~"):_token
		 if option=
		 {
			unexpected:="Percentage Expression ended: """""
			break 2
		 } else {
			overwrite:=true, _expand:=0, _isstr:="", _isexpr:="", _isvar:="", _literal:="", _number:="", _var:=""
			if (A_Index=1) {
				always_literal:=false, force_literal:=false
				if with_partial {
                   main_orig:=option, main_action:=with_partial, option:=main_action, maps:=maps(main_action)
			    } else if (_isexpr:=(_chrfound="%"))||(_chrfound="``")||((_chr:=SubStr(option,-1))&&(_chr="++"||_chr="--")) {
					if _isexpr
						Eval((SubStr(option,1,1)="(")?SubStr(option, 2, -1):option, FD,,_escape,_result,_evaluated,3)
					else if _chr
					   (FD[outfd].HasKey(_var:=SubStr(option,1,-2))) ? (((_chr="++") ? FD[outfd][_var]++ : FD[outfd][_var]--),to_return:=FD[outfd][_var]) : (to_return:=Eval(option, FD,,_escape,_result,_evaluated,3))
					if unexpected
					break 2
					continue 2
				} else {
					(main_nickname&&_try:=lang(,option,,"custom_main")) ? option:=_try
					main_action:=(main_type:=script_section.HasKey(option) ? "section" : (isFunc(option)?"function":""))?option:lang(,option)
					main_type:=(main_type="")?((main_action="")?"":(isFunc(main_action)?"function":"lang")):main_type
					if main_type {
					   main_orig:=option, option:=main_action, maps:=maps(main_action), custom_option:=SIGNALME.def.HasKey(main_action), maps[main_action].ignore_func ? main_type:="lang":false
					} else {
					   unexpected:=(option~="\x04[&``]\d+[&``]\x04") ? "Only %expressions% are allowed for the main action and connectors" : "Invalid start of action--->" . option
					   break 2
					}
				}
			}
			if (A_Index=1)||(force_literal=3)||(_isstr:=(_chrfound=Delimiter))||(_literal:=(_chrfound="``")?"$":"")||(_number:=(isNumber(SubStr(option,1,2))))||(force_literal=2)||(_isexpr:=(_chrfound="%"))||(custom_option&&option:=lang(main_action,option,,"custom"))||(option:=lang(,_token,,"connectors"))||always_literal||(unexpected:=(_token=")")?"Closing -> ')' without opening -> '('":"")||(_isvar:=FD[outfd].HasKey(_token))||(_expand:=(SubStr(_token,0)="*"))
			{
				if unexpected
				break 2
				if (_isstr||_expand||_literal||_number||_isvar||_isexpr) {
				   if (option:=_lastoption) {
					  (_expand) ? (_token:=SubStr(_token,1,-1)) : ((_expand:=(SubStr(_token,0)="*")) ? (_token:=SubStr(_token,1,-1)) : false)
				      (!_isstr) ? (_var:=(_isexpr&&!always_literal) ? Eval((SubStr(_token,1,1)="(")?SubStr(_token, 2, -1):_token,FD,,_escape,_result,_evaluated,2,_hasexpr) : ((always_literal&&maps[always_literal].literal>0) ? solve_escape(_token, _escape) : _token))
					  _literal ? (inkey:=SubStr(_var,3,-2))
					  if unexpected
					  break 2
					  if _isstr
					  	   read_line_[option].Push(solve_escape_string(_escape[SubStr(_token, 4, -3)],_result,_evaluated,_hasexpr))
				      else if _expand {
						  if (_literal="$") {
							if isObject(_result[inkey]) {
								if (_typeobj:=ComObjType(_result[inkey]))&&(_typeobj=9||_typeobj=0xD) {
									read_line_[option]:=_result[inkey]
								} else {
									for cont, value in _result[inkey] 
										read_line_[option].Push(value)
								}
							}
						  } else if isObject(_var) {
							if (_typeobj:=ComObjType(_var))&&(_typeobj=9||_typeobj=0xD) {
								read_line_[option]:=_var
							} else {
								for cont, value in _var
									read_line_[option].Push(value)
							}
						  } else if isObject(FD[outfd][_var]) {
							if (_typeobj:=ComObjType(FD[outfd][_var]))&&(_typeobj=9||_typeobj=0xD) {
								read_line_[option]:=FD[outfd][_var]
							} else {
								for cont, value in FD[outfd][_var]
									read_line_[option].Push(value)
							}
						  }
					  } else {
					     _literal ? read_line_[option].Push((_literal="$") ? _result[inkey] : _escape[inkey]) : read_line_[option].Push(_number ? _var : _isvar ? FD[outfd][_var] : _var)
					  }
					  overwrite:=false
				   } else {
					  unexpected:="A connector/action was expected before the "
					  unexpected.=_literal ? "expression" : _number ? "number" : "variable"
					  unexpected.="--->" . _var
					  break 2
				   }
				} else if always_literal {
					(force_literal||option="") ? (option:=always_literal, overwrite:=false, read_line_[option].Push(_token))
				}
				force_literal:=(always_literal:=maps[option].literal ? option : false) ? ((maps[option].literal>1) ? maps[option].literal : false) : false
				maps[option].concat ? (option:=maps[option].concat, overwrite:=false)
				(overwrite||!isObject(read_line_[option])) ? (read_line_[option]:=[],_lastoption:=option)
				if !(option=main_action||main_type="section") && !maps[option].support {
				   unexpected:="""" . main_orig . """ doesn't support the connector--->" . _token
				   break 2
			    }
				if maps[option].max&&(read_line_[option].MaxIndex()>maps[option].max) {
					unexpected:=option . "---> Only supports " . maps[option].max . " parameters"
					break 2
				}
			} else {
			   unexpected:="Unrecognized connector--->" . _token
			   break 2
			}
		 }
		if _stop
		break
	  }
	 if with_partial {
		switch (with_partial) {
			case "for":
				if IsObject(read_line_.in) {
					for count, var in read_line_[with_partial]
					{
						if !(var ~= "^[a-zA-Z_$][a-zA-Z0-9_$]*$") {
							unexpected:="Invalid variable name--->" . var
							break
						}
					}
				} else {
					for count, number in read_line_[with_partial]
						(!isNumber(SubStr(number,1,2)))?read_line_[with_partial][count]:=Eval(number,FD,,_escape,_result,_evaluated,3)
				}
		}
		if unexpected
		break
		continue
	 }
	 back_label:=last_label
	 if main_type=section
	 {
		last_label:=main_action, to_return:=load_config(script_section[main_action],true,read_line_), (FD_CURRENT>1)?FD.Pop():false, FD_CURRENT:=outfd, last_label:=back_label
		if isObject(SIGNALME.unexpected)
		   break
	 }
	 else if main_type=function
	 {
		if maps[main_action].atpos {
		  ARGS:=[]
		  for key in read_line_ {
			 if maps[key].at {
				Loop,parse,% maps[key].at,`,
				{
				   if maps[key].expand {
					  for count, value in read_line_[key]
					  ARGS[A_LoopField+count-1]:=value
				   } else
					ARGS[A_LoopField]:=read_line_[key][A_Index]
				}
			 }
		  }
		  read_line_:={}, read_line_[main_action]:=ARGS, ARGS:=""
		}
		to_return:=(skip_functions&&main_action~=skip_functions)?"":%main_action%(read_line_[main_action]*)
		if unexpected
		break	
	 } else {
		switch (main_action) {
		   case "global":
			   for count, value in read_line_.global {
				  try FD[outfd].global(value)
				  catch {
					 unexpected:="Invalid variable name--->" . value
					 break 2
				  }
			   }
		   case "import":
			   for count, value in read_line_.import {
				  if InStr(FileExist(value), "A") {
					 load_config(readfile(value),,,outfd,"import:" . value)
				     if unexpected||isObject(SIGNALME.unexpected)
		                break 2
				  } else {
					 unexpected:="Cant find--->" . value
					 break 2
				  }
			   }
		   case "escape":
		        if (StrLen(read_line_.escape.1)=1)
				   Escape:=read_line_.escape.1
				else {
				   unexpected:="Invalid escape character-->" . read_line_.escape.1
				   break
				}
		   case "break":
				SIGNALME.code:=3
				return 1
		   case "continue":
		   		SIGNALME.code:=3.1
				return 1
		   case "abort":
				read_line_.abort.MaxIndex() ? print(read_line_.abort*)
				load_config(,,,,,true)
				SIGNALME.code:=1.1, SIGNALME.exit:=0
				return 0
		   case "exit":
				last_label ? SIGNALME[outfd].from:=last_label
				if lang("all",read_line_.exit.1,,"signal") {
				   SIGNALME.code:=1.1
				} else if lang("main",read_line_.exit.1,,"signal") {
				   SIGNALME.code:=1.2
				} else {
				   SIGNALME.code:=1
				}
				return 1
		   case "set":
			   if _var:=isObjRef(read_line_.set.1)
			   {
				    _var:=SubStr(read_line_.set.1,1,_var)
					try {
						(SubStr(_var,1,1)=Deref)?false:(isObject(FD[outfd][_var])?(IsObject(read_line_.with)?false:FD[outfd][_var]:={}):FD[outfd][_var]:={})
						ParseObjects(read_line_.set.1,FD,":=",(read_line_.with.MaxIndex()>1)?read_line_.with:read_line_.with.1,_escape, _result, _evaluated,_hasexpr)
						if unexpected
			   			break
					} catch {
						unexpected:="Could not define property--->" . read_line_.set.1
						break
					}
			   } else {
			       try FD[outfd][read_line_.set.1]:=(read_line_.with.MaxIndex()>1)?read_line_.with:read_line_.with.1
				   catch {
					  unexpected:="Could not define var--->" . read_line_.set.1
					  break
				   }
			   }
		   case "setworkingdir":
		       if InStr(FileExist(read_line_.setworkingdir.1),"D")
		   			SetWorkingDir, % read_line_.setworkingdir.1
		   case "section":   
			   if script_section.HasKey(read_line_.section.1) {
			      last_label:=read_line_.section.1
				  to_return:=load_config(script_section[last_label],,,outfd)
				  last_label:=back_label
				  if isObject(SIGNALME.unexpected)
		             break
			   }
		   case "thread":
		        if script_section.HasKey(read_line_.thread.1) {
				   if (read_line_.thread.2~="i)^(-?\d+)|on|off|delete$"||read_line_.thread.2:=-1)
					   _thread.HasKey(read_line_.thread.1) ? _thread[read_line_.thread.1].Parse(read_line_.thread.2) : _thread[read_line_.thread.1]:=new Thread("load_config",read_line_.thread.2,script_section[read_line_.thread.1],,,outfd)
                   if isObject(SIGNALME.unexpected)
		              break
				}
		   case "eval":   
				to_return:=Eval(read_line_.eval.1,FD)
				if isObject(SIGNALME.unexpected)
		           break
		}	   
	 }
     read_line_:="",(_stop=";")?to_return:=load_config(SubStr(read_line,_pos+1),,,outfd,"resolvepart")
   }
   if unexpected||isObject(SIGNALME.unexpected) {
     read_line_:=""
	 if block_type {
		 if block_with
			main_orig:=block_type . "--->" . (block_with1 ? block_with1 : solve_escape(solve_escape(solve_escape(block_with, _escape), _result, "``"), _evaluated, "~"))
		 else
			main_orig:=block_type
	 }
	 (!last_label||is_callout||is_callmulti||with_partial) ? show_error:=true
     if !isObject(SIGNALME.unexpected) {
	    if is_callout||is_callmulti
		   SIGNALME.unexpected:={unexpected: unexpected, last_label: "", to_show: last_label ? "Error in expression from--->" . last_label : "Line: " . line . "--in---> Expression", main_orig: main_orig, read_line2: solve_escape(solve_escape(solve_escape(read_line2, _escape), _result, "``"), _evaluated, "~")}
		else if (from_type&&InStr(from_type,"import:"))
		   SIGNALME.unexpected:={unexpected: unexpected, to_show: "Error in line: " . line . "`nFile: " RegExReplace(SubStr(from_type,InStr(from_type,":")), ".*\\([^\\]+)$", "$1"), main_orig: main_orig, read_line2: read_line2, show_error: true}
		else if lang(,from_type,,"condition")
		   SIGNALME.unexpected:={unexpected: unexpected, line: line . "--from---> " . from_type, main_orig: main_orig, read_line2: read_line2, show_error: true}
		else
	       SIGNALME.unexpected:={unexpected: unexpected, last_label: last_label, line: line, main_orig: main_orig, read_line2: read_line2, show_error: true}
	 } else if last_label && !SIGNALME.unexpected.last_label {
	    SIGNALME.unexpected.last_label:=last_label
     }
	 if show_error {
		 for key, value in SIGNALME.unexpected
		    %key%:=value
		 read_line2 ? unexpected.="`n`n---> " . read_line2
		 to_show:=to_show ? to_show : last_label ? "Error in tag: " . last_label . "`nLine: " . line : "Error in line: " . line
		 main_orig ? to_show.="`nAction: " . main_orig
		 to_show.="`nReason: " . unexpected
		 load_config(,,,,,true)
		 SIGNALME.code:=1.1, SIGNALME.exit:=0
		 MsgBox, 262160, Config Exception, % to_show
	 }
	 return,0
   } else
   return,to_return
}
RuntimeError(error) {
   MsgBox, 262160, Runtime Exception, % "Internal error in -> [" . A_ScriptName . "]`n`nFile: " . RegExReplace(error.file, ".*\\([^\\]+)$", "$1") . "`nLine: " . error.line . "`nReason: " .  error.message . "`n`n---> " . error.what
   ExitApp
}