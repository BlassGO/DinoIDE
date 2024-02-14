#NoEnv
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%

#include DinoCompiler.ahk
#include RichCode.ahk
#include gui_resize.ahk
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
ini := current "\config.ini" 
IniRead, icon, %ini%, COMPILER, icon, 0
(!FileExist(icon)) ? icon:=current "\icon.ico"
IniRead, SetDescription, %ini%, COMPILER, SetDescription, Compiled DinoScript
IniRead, SetCopyright, %ini%, COMPILER, SetCopyright, Copyright (c) since 2023
IniRead, SetCompanyName, %ini%, COMPILER, SetCompanyName, DinoCode
IniRead, admin, %ini%, COMPILER, admin, 0

; Functions
Add_Recent(File:="") {
	global ini, Open_File
	Menu, Opened, DeleteAll
	if File {
		IniWrite, %File%, %ini%, GENERAL, last_opened
		Menu, Opened, Add, %File%, Open_Recent
		Gui, +LastFound
		WinGetTitle, Title
		StringSplit, Title, Title, -, %A_Space%
		WinSetTitle, %Title1% - %File%
		Open_File:=new_list:=File
		Arg_in(Open_File)
		Arg_OrigFilename(basename(Open_File))
	}
	IniRead, last_opened_list, %ini%, GENERAL, last_opened_list, 0
	(last_opened_list=0) ? last_opened_list:=""
	Loop, Parse, last_opened_list, `;
	{
		if (A_Index<=6) {
			if (A_LoopField!=File&&FileExist(A_LoopField)) {
				new_list.=";" . A_LoopField
				Menu, Opened, Add, %A_LoopField%, Open_Recent
			}
		} else {
			break
		}
	}
	IniWrite, %new_list%, %ini%, GENERAL, last_opened_list
}

; Table of supported languages and sample codes
Codes :=
( LTrim Join Comments
{
	"DinoCode": {
		"Highlighter": "HighlightDino",
		"Code": FileOpen("Examples\__SIMPLE_EXAMPLE.config", "r").Read()
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
	}
}
)

; Menus
Zoom := "100 %"
Menu, Opened, Add
Add_Recent()
Menu, Zoom, Add, 200 `%, Zoom
Menu, Zoom, Add, 150 `%, Zoom
Menu, Zoom, Add, 125 `%, Zoom
Menu, Zoom, Add, 100 `%, Zoom100
Menu, Zoom, Check, 100 `%
Menu, Zoom, Add, 75 `%, Zoom
Menu, Zoom, Add, 50 `%, Zoom
Menu, File, Add, &Open, FileOpen
Menu, File, Add, &Open Recent, :Opened
Menu, File, Add, &Append, FileAppend
Menu, File, Add, &Insert, FileInsert
Menu, File, Add, &Close, FileClose
Menu, File, Add, &Save, FileSave
Menu, File, Add, Save &as, FileSaveAs
Menu, File, Add, &Exit, GuiClose
Menu, Search, Add, &Find, Find
Menu, Search, Add, &Replace, Replace
Menu, Build, Add, Just RUN, RUN
Menu, Build, Add, Convert to .EXE, Compile
Menu, Build, Add, Convert to .EXE and RUN, Compile
Menu, Build, Add, Change properties, with_gui
Menu, Examples, Add
Loop, Examples/*.config
{
	Menu, Examples, Add, %A_LoopFileName%, Examples
}
Menu, Comments, Add, Block Comment, Comments
Menu, Comments, Add, Block Uncomment, Comments
Menu, View, Add, &Zoom, :Zoom
Menu, GuiMenu, Add, &File, :File

; Extras
GuiFont := GuiDefaultFont()
textz := GuiFont.Size
style := GuiFont.Name

; Add some controls
Gui, +Resize +LastFound
Gui, Menu, GuiMenu

; Add the RichCode
RC := new RichCode(Settings, "xm w640 h470")

; Add extra Menus
Menu, GuiMenu, Add, &Edit, % ":" RC.MenuName
Menu, GuiMenu, Add, &Build, :Build
Menu, GuiMenu, Add, &Search, :Search
Menu, GuiMenu, Add, &View, :View
Menu, GuiMenu, Add, &Comments, :Comments
Menu, GuiMenu, Add, &Examples, :Examples
GuiControl, Focus, % RC.hWnd
GoSub, ChangeLang
Gui, Show
WinMaximize, A
return

Open_Recent:
	If FileExist(A_ThismenuItem) {
		GoSub FileClose
		IfMsgBox, Cancel
			return
		RC.LoadFile(A_ThismenuItem)
		Add_Recent(A_ThismenuItem)
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
	If FileExist(File:="Examples\" . A_ThismenuItem) {
		GoSub FileClose
		IfMsgBox, Cancel
			return
		RC.LoadFile(File)
		Add_Recent(File)
	}
return

RUN:
	to_exe:=false
	RC.SaveFile(configtmp:=(Open_File) ? Open_File : current "\tmp\on_load.config")
	Arg_in(configtmp)
	build()
return

Compile:
	to_exe:=true
	If !(Open_File) {
		GoSub, FileSaveAs
		if !(Open_File)
		   return
	} else {
		RC.SaveFile(Open_File)
	}
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
	if InStr(A_ThismenuItem, "Un") {
		RC.SelectedText := RegExReplace(RC.SelectedText, "Dims)#\*?(.+?)(?:\*#|$)", "$1")
	} else {
		RC.SelectedText := (InStr(Trim(RC.SelectedText, "`r`n"), "`n")) ? "#*" RC.SelectedText "*#`n" : "#" RC.SelectedText
	}
return

Find:
	RC.FindText()
return

Replace:
	RC.ReplaceText()
return

GuiClose:
    GoSub FileClose
	IfMsgBox, Cancel
		return
	RC := ""
	Gui, Destroy
	ExitApp
return

GuiSize:
	if (A_EventInfo = 1)
		return
	AutoXYWH("wh", RC.hWnd)
return

ChangeLang:
	IniRead, last_opened, %ini%, GENERAL, last_opened, 0
	Language:="DinoCode"
	RC.Settings.Highlighter := Codes[Language].Highlighter
	if FileExist(last_opened) {
		RC.LoadFile(last_opened)
		Add_Recent(last_opened)
	} else {
		RC.Value := Codes[Language].Code
	}
return


; ======================================================================================================================
; Menu File
; ======================================================================================================================
FileAppend:
FileOpen:
FileInsert:
	If (File := RC.FileDlg("O")) {
		GoSub FileClose
		IfMsgBox, Cancel
			return
		RC.LoadFile(File, SubStr(A_ThisLabel, 5))
		If (A_ThisLabel = "FileOpen") {
			Add_Recent(File)
		}
	}
	GuiControl, Focus, % RC.HWND
Return
; ----------------------------------------------------------------------------------------------------------------------
FileClose:
	If (Open_File) {
		If RC.Modified {
			Gui, +OwnDialogs
			MsgBox, 35, Close File, Content has been modified!`nDo you want to save changes?
			IfMsgBox, Cancel
			{
				GuiControl, Focus, % RC.HWND
				Return
			}
			IfMsgBox, Yes
				GoSub, FileSave
		}
		Gui, +LastFound
		WinGetTitle, Title
		StringSplit, Title, Title, -, %A_Space%
		WinSetTitle, %Title1%
		Open_File := ""
	}
	RC.Modified:=false
	GuiControl, Focus, % RC.HWND
Return
; ----------------------------------------------------------------------------------------------------------------------
FileSave:
	If !(Open_File) {
		GoSub, FileSaveAs
		Return
	}
	RC.SaveFile(Open_File)
	RC.Modified:=false
	GuiControl, Focus, % RC.HWND
Return
; ----------------------------------------------------------------------------------------------------------------------
FileSaveAs:
	If (File := RC.FileDlg("S")) {
		RC.SaveFile(File)
		Add_Recent(File)
	}
	GuiControl, Focus, % RC.HWND
Return

