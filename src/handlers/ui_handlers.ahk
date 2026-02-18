Close:
	IDE.GuiClose()
return

Version:
	Gui info: New 
	Gui info: Default 
	Gui info: +AlwaysOnTop -MinimizeBox
	Gui info: Color, %COLOR_BG%
	Gui info: Font, s11, %style%
	Gui info: Add, Picture, w100 h100, % "HBITMAP:*" Create_icon_ico()
	Gui info: Add, Edit, w100 ReadOnly Center 0x200 c%COLOR_WHITE% -HScroll -VScroll NoTab, DinoIDE
	Gui info: Add, Edit, w100 ReadOnly Center 0x200 c%COLOR_WHITE% -HScroll -VScroll NoTab, %ide_version%
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
		Menu, %MENU_VIEW%, Check, Always On Top
		Gui, +AlwaysOnTop
	}
	else
	{
		Menu, %MENU_VIEW%, UnCheck, Always On Top
		Gui, -AlwaysOnTop
	}
return

Highlighter:
	if (IDE.Settings.UseHighlighter := !IDE.Settings.UseHighlighter)
		Menu, %MENU_VIEW%, Check, Highlighter
	else
		Menu, %MENU_VIEW%, Uncheck, Highlighter
	; Force refresh the code
	IDE.RichCode.Value := IDE.RichCode.Value
return

AutoComplete:
	if (IDE.Settings.UseAutoComplete := !IDE.Settings.UseAutoComplete)
		Menu, %MENU_VIEW%, Check, Prediction
	else
		Menu, %MENU_VIEW%, Uncheck, Prediction
	IDE.AC.Enabled := IDE.Settings.UseAutoComplete
return

Zoom:
Zoom100:
	Menu, %MENU_ZOOM%, UnCheck, %Zoom%
	If (A_ThisLabel = "Zoom100")
		Zoom := "100 %"
	Else
		Zoom := A_ThismenuItem
	Menu, %MENU_ZOOM%, Check, %Zoom%
	RegExMatch(Zoom, "\d+", Ratio)
	RC.SetZoom(Ratio)
	GuiControl, Focus, % RC.HWND
Return

Comments:
	if InStr(A_ThismenuItem, "Remove")
		RC.SelectedText := RegExReplace(RegExReplace(RC.SelectedText, "`ams)^\s*(?:(?:#\*.+?\*#\s*$)|(?:#[^\r\n]*))"),"`am)^\s+$")
	else if InStr(A_ThismenuItem, "Un")
		RC.SelectedText := RegExReplace(RegExReplace(RC.SelectedText, "`ams)^\s*\K#\*(.+?)\*#\s*$", "$1"), "`ams)^\s*\K#([^\r\n]*)", "$1")
	else
		RC.SelectedText := (InStr(Trim(RC.SelectedText, "`r`n"), "`n")) ? "#*" RC.SelectedText "*#`n" : "#" RC.SelectedText
return

Find:
	if WinExist("ahk_id" IDE.FindInstance.hWnd)
		WinActivate, % "ahk_id" IDE.FindInstance.hWnd
	else
		IDE.FindInstance := new IDE.Find(IDE)
return

ChangeLang:
	IniRead, last_opened, %CONFIG_PATH%, GENERAL, last_opened, 0
	fileinfo := RC.GetFilePathPos(last_opened)
	Language := "DinoCode"
	RC.Settings.Highlighter := Codes[Language].Highlighter
	if FileExist(fileinfo.1)
		RC.LoadFile(fileinfo.1), RC.PosFromNumber(fileinfo.2)
	else
		RC.LoadFile(Codes[Language].FileDefault)
return
