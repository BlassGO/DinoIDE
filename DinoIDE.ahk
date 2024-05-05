#NoEnv
#SingleInstance, ignore
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%

#include icon.ahk
#include DinoCompiler.ahk
#include DinoIDE_Class.ahk
#include RichCode.ahk
#include gui_resize.ahk
#include obj_dump.ahk
#include Highlighters\DinoCode.ahk
#include bin\dinocode\Crypt.ahk

; AHK2 configs
;@Ahk2Exe-SetName         DinoIDE
;@Ahk2Exe-SetDescription  Official DinoCode editor
;@Ahk2Exe-SetCopyright    Copyright (c) since 2024
;@Ahk2Exe-SetCompanyName  by @BlassGO
;@Ahk2Exe-SetMainIcon icon.ico
;@Ahk2Exe-AddResource icon.ico, 160
;@Ahk2Exe-AddResource icon.ico, 206
;@Ahk2Exe-AddResource icon.ico, 207
;@Ahk2Exe-AddResource icon.ico, 208

; Working
ide_version=v1.3.0
ini := current "\config.ini" 
IniRead, icon, %ini%, COMPILER, icon, 0
(!FileExist(icon)) ? icon:=current "\icon.ico"
IniRead, SetDescription, %ini%, COMPILER, SetDescription, Compiled DinoScript
IniRead, SetCopyright, %ini%, COMPILER, SetCopyright, Copyright (c) since 2023
IniRead, SetCompanyName, %ini%, COMPILER, SetCompanyName, DinoCode
IniRead, admin, %ini%, COMPILER, admin, 0

; Table of supported languages and sample codes
Codes :=
( LTrim Join Comments
{
	"DinoCode": {
		"Highlighter": "HighlightDino",
		"FileDefault": "Examples\ADVANCED\__SIMPLE_EXAMPLE.config"
	}
}
)

; Settings array for the RichCode control
Settings :=
( LTrim Join Comments
{
	"TabSize": 4,
	"Indent": "`t",
	"FGColor": 0xEDEDCD,
	"BGColor": 0x2A2C2A,
	"Font": {"Typeface": "Consolas", "Size": 11},
	"WordWrap": False,
	"Multiline": True,
	
	"UseHighlighter": True,
	"HighlightDelay": 400,
	"Gutter": {
		; Width in pixels. Make this larger when using
		; larger fonts. Set to 0 to disable the gutter.
		"Width": 35,

		"FGColor": 0x9FAFAF,
		"BGColor": 0x262626
	},
	"Colors": {
		"Comments":     0x7F9F7F,
		"Functions":    0x7CC8CF,
		"Multiline":    0x7F9F7F,
		"Numbers":      0xCB8DD9,
		"Punctuation":  0xEF5A37,
		"Strings":      0xDAC491,
		
		; DinoCode
		"Builtins":     0xF79B57,
		"Variables":    0xC8E723,
		"Signals":      0xC87227,
		"Opers":        0xC87227,
		"Flow":         0x52E0E4,
		"TAGs":         0xF79B57,
		"TAGMAIN":      0xF2300A
	},
	; AutoComplete
	"UseAutoComplete": True,
	"ACListRebuildDelay": 500 ; Delay until the user is finished typing
}
)

; Menus
Zoom := "100 %"
Menu, Opened, Add
Menu, Zoom, Add, 200 `%, Zoom
Menu, Zoom, Add, 150 `%, Zoom
Menu, Zoom, Add, 125 `%, Zoom
Menu, Zoom, Add, 100 `%, Zoom100
Menu, Zoom, Check, 100 `%
Menu, Zoom, Add, 75 `%, Zoom
Menu, Zoom, Add, 50 `%, Zoom
Menu, File, Add, &New, FileNew
Menu, File, Add, &Open, FileOpen
Menu, File, Add, &Open Recent, :Opened
Menu, File, Add, &Open Script Dir, FileOpenDir
Menu, File, Add, &Append, FileAppend
Menu, File, Add, &Insert, FileInsert
Menu, File, Add, &Close, FileClose
Menu, File, Add, &Save, FileSave
Menu, File, Add, Save &as, FileSaveAs
Menu, File, Add, Version, Version 
Menu, File, Add, &Exit, Close
Menu, Search, Add, &Find-Replace, Find
Menu, Build, Add, Just RUN, RUN
Menu, Build, Add, Just RUN (Step by Step), RUN
Menu, Build, Add, Just Optimize Code, Optimize
Menu, Build, Add, Convert to .EXE, Compile
Menu, Build, Add, Convert to .EXE and RUN, Compile
Menu, Build, Add, Change properties, with_gui
Menu, Examples, Add
try {
	Loop, Files, Examples\*, D
	{
		item:=A_LoopFileName
		Loop, % A_LoopFileFullPath . "\*.config"
			Menu, % item, Add, %A_LoopFileName%, Examples
		Menu, Examples, Add, %A_LoopFileName%, % ":" item
	}
}
Menu, Comments, Add, Block Comment (Selected text), Comments
Menu, Comments, Add, Block Uncomment (Selected text), Comments
Menu, Comments, Add, Remove ALL Comments (Selected text), Comments
Menu, View, Add, Prediction, AutoComplete
Menu, View, Add, Highlighter, Highlighter
Menu, View, Add, Always On Top, ToggleOnTop
Menu, View, Add, &Zoom, :Zoom
Menu, View, Check, Prediction
Menu, View, Check, Highlighter
Menu, GuiMenu, Add, &File, :File

; Extras
GuiFont := GuiDefaultFont()
textz := GuiFont.Size
style := GuiFont.Name

; Main GUI
Gui, +Resize +LastFound -AlwaysOnTop
Gui, Menu, GuiMenu
IDE:=new DinoIDE(Settings), RC:=IDE.RichCode

; Add extra Menus
RC.Add_Recent()
Menu, GuiMenu, Add, &Edit, % ":" RC.MenuName
Menu, GuiMenu, Add, &Build, :Build
Menu, GuiMenu, Add, &Search, :Search
Menu, GuiMenu, Add, &View, :View
Menu, GuiMenu, Add, &Comments, :Comments
Menu, GuiMenu, Add, &Examples, :Examples
Menu, GuiMenu, Add, &Libraries, Libraries
GuiControl, Focus, % RC.hWnd
GoSub, ChangeLang
WinMaximize, A
return

Close:
	IDE.GuiClose()
return

Version:
	Gui info: New 
	Gui info: Default 
	Gui info: +AlwaysOnTop -MinimizeBox
	Gui info: Color, 2A2C2A
	Gui info: Font, s11, %style%
	Gui info: Add, Picture, w100 h100, % "HBITMAP:*" Create_icon_ico()
	Gui info: Add, Edit, w100 ReadOnly Center 0x200 cwhite -HScroll -VScroll NoTab, DinoIDE
	Gui info: Add, Edit, w100 ReadOnly Center 0x200 cwhite -HScroll -VScroll NoTab, %ide_version%
	Gui info: Add, Button, w100 Center gDeveloper, @BlassGO
	Gui info: show, AutoSize Center, INFO
	Gui info: +LastFound
	WinWaitClose
	Gui info: Destroy
return

Developer:
	Run, https://github.com/BlassGO
	Gui info: Destroy
return

ToggleOnTop:
	if (IDE.AlwaysOnTop := !IDE.AlwaysOnTop)
	{
		Menu, View, Check, Always On Top
		Gui, +AlwaysOnTop
	}
	else
	{
		Menu, View, UnCheck, Always On Top
		Gui, -AlwaysOnTop
	}
return

Highlighter:
	if (IDE.Settings.UseHighlighter := !IDE.Settings.UseHighlighter)
		Menu, View, Check, Highlighter
	else
		Menu, View, Uncheck, Highlighter
	; Force refresh the code, adding/removing any highlighting
	IDE.RichCode.Value := IDE.RichCode.Value
return

AutoComplete:
	if (IDE.Settings.UseAutoComplete := !IDE.Settings.UseAutoComplete)
		Menu, View, Check, Prediction
	else
		Menu, View, Uncheck, Prediction
	IDE.AC.Enabled := IDE.Settings.UseAutoComplete
return

Open_Recent:
	If FileExist(A_ThismenuItem) {
		if !RC.FileClose()
			return
		RC.LoadFile(A_ThismenuItem), RC.PosFromFile(A_ThismenuItem)
	}
Return

Zoom:
Zoom100:
	Menu, Zoom, UnCheck, %Zoom%
	If (A_ThisLabel = "Zoom100")
		Zoom := "100 %"
	Else
		Zoom := A_ThismenuItem
	Menu, Zoom, Check, %Zoom%
	RegExMatch(Zoom, "\d+", Ratio)
	RC.SetZoom(Ratio)
	GuiControl, Focus, % RC.HWND
Return

Examples:
	If FileExist(File:="Examples\" . A_ThisMenu . "\" . A_ThismenuItem) {
		if !RC.FileClose()
			return
		RC.LoadFile(File), RC.PosFromFile(File)
	}
return

RUN:
	to_exe:=false
	if !RC.FileClose(true)
		return
	if (InStr(A_ThisMenuItem, "Step")) {
		config_tracking:=true
	}
	build()
	config_tracking:=false
return

Optimize:
	if !RC.FileClose(true)
		return
	error:=false, out:=dirname(config) "\" simplename(config) "-Optimized.config"
	FileDelete, % out
	FileAppend, % read_config(config), % out
	if error {
		FileDelete, % out
	} else {
		Run, explorer.exe /select`,"%out%"
	}
return

Compile:
	to_exe:=true
	if !RC.FileClose(true)
		return
	build()
	if !error {
		if InStr(A_ThismenuItem, "RUN") {
			Run, "%out%"
		} else {
			Run, explorer.exe /select`,"%out%"
		}
	}
return

Comments:
	if InStr(A_ThismenuItem, "Remove")
		RC.SelectedText := RegExReplace(RegExReplace(RC.SelectedText, "`ams)^\s*(?:(?:#\*.+?\*#\s*$)|(?:#[^\r\n]*))"),"`am)^\s+$")
	else if InStr(A_ThismenuItem, "Un")
		RC.SelectedText := RegExReplace(RegExReplace(RC.SelectedText, "`ams)^\s*\K#\*(.+?)\*#\s*$", "$1"), "`ams)^\s*\K#([^\r\n]*)", "$1")
	else
		RC.SelectedText := (InStr(Trim(RC.SelectedText, "`r`n"), "`n")) ? "#*" RC.SelectedText "*#`n" : "#" RC.SelectedText
return

Find:
	;RC.FindText()
	if WinExist("ahk_id" IDE.FindInstance.hWnd)
		WinActivate, % "ahk_id" IDE.FindInstance.hWnd
	else
		IDE.FindInstance := new IDE.Find(IDE)
return

ChangeLang:
	IniRead, last_opened, %ini%, GENERAL, last_opened, 0
	fileinfo:=RC.GetFilePathPos(last_opened)
	Language:="DinoCode"
	RC.Settings.Highlighter := Codes[Language].Highlighter
	if FileExist(fileinfo.1)
	RC.LoadFile(fileinfo.1), RC.PosFromNumber(fileinfo.2)
	else
	RC.LoadFile(Codes[Language].FileDefault)
return


; ======================================================================================================================
; Menu File
; ======================================================================================================================
FileAppend:
FileOpen:
FileInsert:
	RC.FileOpen(SubStr(A_ThisLabel, 5))
Return
; ----------------------------------------------------------------------------------------------------------------------
FileOpenDir:
	if config
		Run, explorer.exe /select`,"%config%"
return
; ----------------------------------------------------------------------------------------------------------------------
FileClose:
	RC.FileClose()
Return
; ----------------------------------------------------------------------------------------------------------------------
FileSave:
	RC.FileSave()
Return
; ----------------------------------------------------------------------------------------------------------------------
FileNew:
	RC.FileSaveAs(true)
return
FileSaveAs:
	RC.FileSaveAs()
Return

