; Fully Based on Code Quick Tester by GeekDude
; -> https://github.com/tdalon/ahk/blob/main/CodeQuickTester.ahk
class DinoIDE
{
	static Msftedit := DllCall("LoadLibrary", "Str", "Msftedit.dll")
	,Title := "DinoIDE"
	,Keywords:=
	(join
          {
             If: ["Si"],
			 Else: ["SiNo"],
			 Switch: ["Cambiar"],
			 While: ["Mientras"],
			 Until: ["Hasta"],
			 Return: ["Retornar"],
			 Do: ["Hacer"],
			 Use: ["Usar"],
			 For: ["Para"],
			 ForChar: ["ParaCaracter"],
			 ForFile: ["ParaArchivo"],
			 ForObj: ["ParaObjeto"],
			 in: ["en"],
			 with: ["=", "con"],
			 to: ["unto", "hacia"],
			 Case: ["Caso"],
			 Default: ["PorDefecto"],
			 JS: ["JavaScript"],
			 VBS: ["VBScript"],
			 Escape: [],
			 Global: [],
			 SetWorkingDir: [],
			 New: ["Nuevo"],
			 Set: ["Definir"],
			 Section: ["Seccion"],
			 Thread: ["Hilo"],
			 Press: ["Presionar"],
			 Import: ["Importar"],
			 Sleep: ["Dormir"],
			 Print: ["Imprimir","Escribir"],
			 Msg: ["Message","Mensaje"],
			 Input: ["Entrada","Leer"],
			 Question: ["Pregunta"],
			 Option: ["Opcion"],
			 Break: ["Romper"],
			 Continue: ["Continuar"],
			 Abort: ["Abortar"],
			 Exit: ["Salir"],
			 Help: ["Ayuda"],
			 ReadFile: ["LeerArchivo"],
			 WriteFile: ["EscribirArchivo"],
			 WaitWindow: ["EsperarVentana"],
			 WaitConsole: ["EsperarConsola"],
			 StartConsole: ["IniciarConsola"],
			 ReadConsole: ["LeerConsola"]
		  }
	   )
	
	__New(Settings)
	{
		this.Settings := Settings		
		this.Bound := []
		this.Bound.GuiSize := this.GuiSize.Bind(this)
		this.Bound.OnMessage := this.OnMessage.Bind(this)
		this.Bound.UpdateStatusBar := this.UpdateStatusBar.Bind(this)
		this.Bound.UpdateAutoComplete := this.UpdateAutoComplete.Bind(this)
		this.Bound.Highlight := this.Highlight.Bind(this)
		this.Bound.SyncGutter := this.SyncGutter.Bind(this)
		
		Gui, +LastFoundExist
		this.AlwaysOnTop := False
		this.hMainWindow := WinExist()
		
		; Register for events
		WinEvents.Register(this.hMainWindow, this)
		for each, Msg in [0x111, 0x100, 0x101, 0x201, 0x202, 0x204] ; WM_COMMAND, WM_KEYDOWN, WM_KEYUP, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONDOWN
			OnMessage(Msg, this.Bound.OnMessage)
		
		; Add code editor and gutter for line numbers
		this.RichCode := new RichCode(this.Settings, "-E0x20000", this)
		RichEdit_AddMargins(this.RichCode.hWnd, 3, 3)
		if Settings.Gutter.Width
			this.AddGutter()
		
		; Add status bar
		Gui, Add, StatusBar, hWndhStatusBar
		;this.UpdateStatusBar()
		;ControlGetPos,,,, StatusBarHeight,, ahk_id %hStatusBar%
		this.StatusBarHeight := 20
		
		; Initialize the AutoComplete
		this.AC := new this.AutoComplete(this, this.settings.UseAutoComplete)
		
		Gui, Show, w640 h480
	}
	
	AddGutter()
	{
		s := this.Settings, f := s.Font, g := s.Gutter
		
		; Add the RichEdit control for the gutter
		Gui, Add, Custom, ClassRichEdit50W hWndhGutter +0x5031b1c6 -HScroll -VScroll
		this.hGutter := hGutter
		
		; Set the background and font settings
		FGColor := RichCode.BGRFromRGB(g.FGColor)
		BGColor := RichCode.BGRFromRGB(g.BGColor)
		VarSetCapacity(CF2, 116, 0)
		NumPut(116,        &CF2+ 0, "UInt") ; cbSize      = sizeof(CF2)
		NumPut(0xE<<28,    &CF2+ 4, "UInt") ; dwMask      = CFM_COLOR|CFM_FACE|CFM_SIZE
		NumPut(f.Size*20,  &CF2+12, "UInt") ; yHeight     = twips
		NumPut(FGColor,    &CF2+20, "UInt") ; crTextColor = 0xBBGGRR
		StrPut(f.Typeface, &CF2+26, 32, "UTF-16") ; szFaceName = TCHAR
		SendMessage(0x444, 0, &CF2,    hGutter) ; EM_SETCHARFORMAT
		SendMessage(0x443, 0, BGColor, hGutter) ; EM_SETBKGNDCOLOR
		
		RichEdit_AddMargins(hGutter, 3, 3, -3, 0)
	}
	
	OnMessage(wParam, lParam, Msg, hWnd)
	{
		if (hWnd == this.hMainWindow && Msg == 0x111 ; WM_COMMAND
			&& lParam == this.RichCode.hWnd)         ; for RichEdit
		{
			Command := wParam >> 16
			
			if (Command == 0x400) ; An event that fires on scroll
			{
				this.SyncGutter()
				
				; If the user is scrolling too fast it can cause some messages
				; to be dropped. Set a timer to make sure that when the user stops
				; scrolling that the line numbers will be in sync.
				SetTimer(this.Bound.SyncGutter, -50)
			}
			else if (Command == 0x200) ; EN_KILLFOCUS
				if this.Settings.UseAutoComplete
					this.AC.Fragment := ""
		}
		else if (hWnd == this.RichCode.hWnd)
		{
			; Call UpdateStatusBar after the edit handles the keystroke
			SetTimer(this.Bound.UpdateStatusBar, -0)
			
			if this.Settings.UseAutoComplete
			{
				SetTimer(this.Bound.UpdateAutoComplete
				, -Abs(this.Settings.ACListRebuildDelay))
				
				if (Msg == 0x100) ; WM_KEYDOWN
					return this.AC.WM_KEYDOWN(wParam, lParam)
				else if (Msg == 0x201) ; WM_LBUTTONDOWN
					this.AC.Fragment := ""
			}
		}
		else if (hWnd == this.hGutter
			&& {0x100:1,0x101:1,0x201:1,0x202:1,0x204:1}[Msg]) ; WM_KEYDOWN, WM_KEYUP, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONDOWN
		{
			; Disallow interaction with the gutter
			return True
		}
	}
	
	SyncGutter()
	{
		static BUFF, _ := VarSetCapacity(BUFF, 16, 0)
		
		if !this.Settings.Gutter.Width
			return
		
		SendMessage(0x4E0, &BUFF, &BUFF+4, this.RichCode.hwnd) ; EM_GETZOOM
		SendMessage(0x4DD, 0, &BUFF+8, this.RichCode.hwnd)     ; EM_GETSCROLLPOS
		
		; Don't update the gutter unnecessarily
		/*
		State := NumGet(BUFF, 0, "UInt") . NumGet(BUFF, 4, "UInt")
		. NumGet(BUFF, 8, "UInt") . NumGet(BUFF, 12, "UInt")
		if (State == this.GutterState)
			return
		*/
		NumPut(-1, BUFF, 8, "UInt") ; Don't sync horizontal position
		Zoom := [NumGet(BUFF, "UInt"), NumGet(BUFF, 4, "UInt")]
		PostMessage(0x4E1, Zoom[1], Zoom[2], this.hGutter)     ; EM_SETZOOM
		PostMessage(0x4DE, 0, &BUFF+8, this.hGutter)           ; EM_SETSCROLLPOS
		this.ZoomLevel := Zoom[1] / Zoom[2]
		this.LastZoomLevel := this.ZoomLevel
		this.GutterState := State
	}
	
	GetKeywordFromCaret()
	{
		; https://autohotkey.com/boards/viewtopic.php?p=180369#p180369
		static Buffer
		IsUnicode := !!A_IsUnicode
		
		rc := this.RichCode
		sel := rc.Selection
		
		; Get the currently selected line
		LineNum := rc.SendMsg(0x436, 0, sel[1]) ; EM_EXLINEFROMCHAR
		
		; Size a buffer according to the line's length
		Length := rc.SendMsg(0xC1, sel[1], 0) ; EM_LINELENGTH
		VarSetCapacity(Buffer, Length << !!A_IsUnicode, 0)
		NumPut(Length, Buffer, "UShort")
		
		; Get the text from the line
		rc.SendMsg(0xC4, LineNum, &Buffer) ; EM_GETLINE
		lineText := StrGet(&Buffer, Length)
		
		; Parse the line to find the word
		LineIndex := rc.SendMsg(0xBB, LineNum, 0) ; EM_LINEINDEX
		RegExMatch(SubStr(lineText, 1, sel[1]-LineIndex), "[#\w]+$", Start)
		RegExMatch(SubStr(lineText, sel[1]-LineIndex+1), "^[#\w]+", End)
		
		return Start . End
	}
	
	UpdateStatusBar()
	{
		; Delete the timer if it was called by one
		SetTimer(this.Bound.UpdateStatusBar, "Delete")
		
		; Get the document length and cursor position
		VarSetCapacity(GTL, 8, 0), NumPut(1200, GTL, 4, "UInt")
		Len := this.RichCode.SendMsg(0x45F, &GTL, 0) ; EM_GETTEXTLENGTHEX (Handles newlines better than GuiControlGet on RE)
		ControlGet, Row, CurrentLine,,, % "ahk_id" this.RichCode.hWnd
		ControlGet, Col, CurrentCol,,, % "ahk_id" this.RichCode.hWnd
		
		; Get Selected Text Length
		; If the user has selected 1 char further than the end of the document,
		; which is allowed in a RichEdit control, subtract 1 from the length
		Sel := this.RichCode.Selection
		Sel := Sel[2] - Sel[1] - (Sel[2] > Len)
		
		; Get the syntax tip, if any
		if (SyntaxTip := HelpFile.GetSyntax(this.GetKeywordFromCaret()))
			this.SyntaxTip := SyntaxTip
		
		; Update the Status Bar text
		Gui, % this.hMainWindow ":Default"
		SB_SetText("Len " Len ", Line " Row ", Col " Col
		. (Sel > 0 ? ", Sel " Sel : "") "     " this.SyntaxTip)
		
		; Update the gutter to match the document
		if this.Settings.Gutter.Width
		{
			ControlGet, Lines, LineCount,,, % "ahk_id" this.RichCode.hWnd
			if (Lines != this.LineCount)
			{
				Loop, %Lines%
					Text .= A_Index "`n"
				/*MayW:=StrLen(Lines)*((this.Settings.Font.Size?this.Settings.Font.Size:0)+10)
				if (MayW>this.Settings.Gutter.Width)
					GuiControl, Move, % this.hGutter  , % "w" . this.Settings.Gutter.Width:=MayW
				*/
				GuiControl,, % this.hGutter, %Text%
				this.SyncGutter()
				this.LineCount := Lines
			}
		}
	}
	
	UpdateAutoComplete()
	{
		; Delete the timer if it was called by one
		SetTimer(this.Bound.UpdateAutoComplete, "Delete")
		
		this.AC.BuildWordList()
	}
	
	RegisterCloseCallback(CloseCallback)
	{
		this.CloseCallback := CloseCallback
	}
	
	GuiSize()
	{
		static RECT, _ := VarSetCapacity(RECT, 16, 0)
		if A_Gui
			gw := A_GuiWidth, gh := A_GuiHeight
		else
		{
			DllCall("GetClientRect", "UPtr", this.hMainWindow, "Ptr", &RECT, "UInt")
			gw := NumGet(RECT, 8, "Int"), gh := NumGet(RECT, 12, "Int")
		}
		gtw := 3 + Round(this.Settings.Gutter.Width) * (this.ZoomLevel ? this.ZoomLevel : 1), sbh := this.StatusBarHeight
		GuiControl, Move, % this.RichCode.hWnd, % "x" 0+gtw "y" 0         "w" gw-gtw "h" gh-sbh
		if this.Settings.Gutter.Width
			GuiControl, Move, % this.hGutter  , % "x" 0     "y" 0         "w" gtw    "h" gh-sbh
		;GuiControl, Move, % this.hRunButton   , % "x" 0     "y" gh-28-sbh "w" gw     "h" 28
	}
	
	GuiDropFiles(hWnd, Files)
	{
		; TODO: support multiple file drop
		this.RichCode.LoadFile(Files[1])
	}
	
	GuiClose()
	{
		if !this.RichCode.FileClose()
			return true
		
		; Free up the AC class
		this.AC := ""
		
		; Release wm_message hooks
		for each, Msg in [0x100, 0x201, 0x202, 0x204] ; WM_KEYDOWN, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONDOWN
			OnMessage(Msg, this.Bound.OnMessage, 0)
		
		; Delete timers
		SetTimer(this.Bound.SyncGutter, "Delete")
		SetTimer(this.Bound.GuiSize, "Delete")
		
		; Break all the BoundFunc circular references
		this.Delete("Bound")
		
		; Release WinEvents handler
		WinEvents.Unregister(this.hMainWindow)
		
		; Release GUI window and control glabels
		Gui, Destroy
		
		ExitApp
	}
	class Find
	{
		__New(Parent)
		{
			this.Parent := Parent
			
			ParentWnd := this.Parent.hMainWindow
			Gui, New, +Owner%ParentWnd% +ToolWindow +hWndhWnd
			this.hWnd := hWnd
			Gui, Margin, 5, 5
			
			
			; Search
			Gui, Add, Edit, hWndhWnd w200
			SendMessage, 0x1501, True, &cue := "Search Text",, ahk_id %hWnd% ; EM_SETCUEBANNER
			this.hNeedle := hWnd
			
			Gui, Add, Button, yp-1 x+m w75 Default hWndhWnd, Find Next
			Bound := this.BtnFind.Bind(this)
			GuiControl, +g, %hWnd%, %Bound%
			
			Gui, Add, Button, yp x+m w75 hWndhWnd, Coun&t All
			Bound := this.BtnCount.Bind(this)
			GuiControl, +g, %hWnd%, %Bound%
			
			
			; Replace
			Gui, Add, Edit, hWndhWnd w200 xm Section
			SendMessage, 0x1501, True, &cue := "Replacement",, ahk_id %hWnd% ; EM_SETCUEBANNER
			this.hReplace := hWnd
			
			Gui, Add, Button, yp-1 x+m w75 hWndhWnd, &Replace
			Bound := this.Replace.Bind(this)
			GuiControl, +g, %hWnd%, %Bound%
			
			Gui, Add, Button, yp x+m w75 hWndhWnd, Replace &All
			Bound := this.ReplaceAll.Bind(this)
			GuiControl, +g, %hWnd%, %Bound%
			
			
			; Options
			Gui, Add, Checkbox, hWndhWnd xm, &Case Sensitive
			this.hOptCase := hWnd
			Gui, Add, Checkbox, hWndhWnd, Re&gular Expressions
			this.hOptRegEx := hWnd
			Gui, Add, Checkbox, hWndhWnd, Transform`, &Deref
			this.hOptDeref := hWnd
			
			
			Gui, Show,, % this.Parent.Title " - Find"
			
			WinEvents.Register(this.hWnd, this)
		}
		
		GuiClose()
		{
			GuiControl, -g, % this.hButton
			WinEvents.Unregister(this.hWnd)
			Gui, Destroy
		}
		
		GuiEscape()
		{
			this.GuiClose()
		}
		
		GetNeedle()
		{
			Opts := this.Case ? "`n" : "i`n"
			Opts .= this.Needle ~= "^[^\(]\)" ? "" : ")"
			if this.RegEx
				return Opts . this.Needle
			else
				return Opts "\Q" StrReplace(this.Needle, "\E", "\E\\E\Q") "\E"
		}
		
		Find(StartingPos:=1, WrapAround:=True)
		{
			Needle := this.GetNeedle()
			
			; Search from StartingPos
			NextPos := RegExMatch(this.Haystack, Needle, Match, StartingPos)
			
			; Search from the top
			if (!NextPos && WrapAround)
				NextPos := RegExMatch(this.Haystack, Needle, Match)
			
			return NextPos ? [NextPos, NextPos+StrLen(Match)] : False
		}

		Submit()
		{
			; Options
			GuiControlGet, Deref,, % this.hOptDeref
			GuiControlGet, Case,, % this.hOptCase
			this.Case := Case
			GuiControlGet, RegEx,, % this.hOptRegEx
			this.RegEx := RegEx
			
			; Search Text/Needle
			GuiControlGet, Needle,, % this.hNeedle
			if Deref
				Transform, Needle, Deref, %Needle%
			this.Needle := Needle
			
			; Replacement
			GuiControlGet, Replace,, % this.hReplace
			if Deref
				Transform, Replace, Deref, %Replace%
			this.Replace := Replace
			
			; Haystack
			this.Haystack := StrReplace(this.Parent.RichCode.Value, "`r")
		}
		
		BtnFind()
		{
			Gui, +OwnDialogs
			this.Submit()
			
			; Find and select the item or error out
			if (Pos := this.Find(this.Parent.RichCode.Selection[1]+2))
				this.Parent.RichCode.Selection := [Pos[1] - 1, Pos[2] - 1]
			else
				MsgBox, 0x30, % this.Parent.Title " - Find", Search text not found
		}
		
		BtnCount()
		{
			Gui, +OwnDialogs
			this.Submit()
			
			; Find and count all instances
			Count := 0, Start := 1
			while (Pos := this.Find(Start, False))
				Start := Pos[1]+1, Count += 1
			
			MsgBox, 0x40, % this.Parent.Title " - Find", %Count% instances found
		}
		
		Replace()
		{
			this.Submit()
			
			; Get the current selection
			Sel := this.Parent.RichCode.Selection
			
			; Find the next occurrence including the current selection
			Pos := this.Find(Sel[1]+1)
			
			; If the found item is already selected
			if (Sel[1]+1 == Pos[1] && Sel[2]+1 == Pos[2])
			{
				; Replace it
				this.Parent.RichCode.SelectedText := this.Replace
				
				; Update the haystack to include the replacement
				this.Haystack := StrReplace(this.Parent.RichCode.Value, "`r")
				
				; Find the next item *not* including the current selection
				Pos := this.Find(Sel[1]+StrLen(this.Replace)+1)
			}
			
			; Select the next found item or error out
			if Pos
				this.Parent.RichCode.Selection := [Pos[1] - 1, Pos[2] - 1]
			else
				MsgBox, 0x30, % this.Parent.Title " - Find", No more instances found
		}
		
		ReplaceAll()
		{
			rc := this.Parent.RichCode
			this.Submit()
			
			Needle := this.GetNeedle()
			
			; Replace the text in a way that pushes to the undo buffer
			rc.Frozen := True
			Sel := rc.Selection
			rc.Selection := [0, -1]
			rc.SelectedText := RegExReplace(this.Haystack, Needle, this.Replace, Count)
			rc.Selection := Sel
			rc.Frozen := False
			
			MsgBox, 0x40, % this.Parent.Title " - Find", %Count% instances replaced
		}
	}
	/*
		Implements functionality necessary for AutoCompletion of keywords in the
		RichCode control. Currently works off of values stored in the provided
		Parent object, but could be modified to work off a provided RichCode
		instance directly.
		
		The class is mostly self contained and could be easily extended to other
		projects, and even other types of controls. The main method of interacting
		with the class is by passing it WM_KEYDOWN messages. Another way to interact
		is by modifying the Fragment property, especially to clear it when you want
		to cancel autocompletion.
		
		Depends on CQT.ahk, and optionally on HelpFile.ahk
	*/
	
	class AutoComplete
	{
		; Maximum number of suggestions to be displayed in the dialog
		static MaxSuggestions := 9
		
		; Minimum length for a word to be entered into the word list
		static MinWordLen := 4
		
		; Minimum length of fragment before suggestions should be displayed
		static MinSuggestLen := 3
		
		; Stores the initial caret position for newly typed fragments
		static CaretX := 0, CaretY := 0
		
		
		; --- Properties ---
		
		Fragment[]
		{
			get
			{
				return this._Fragment
			}
			
			set
			{
				this._Fragment := Value
				
				; Give suggestions when a fragment of sufficient
				; length has been provided
				if (StrLen(this._Fragment) >= 3)
					this._Suggest()
				else
					this._Hide()
				
				return this._Fragment
			}
		}
		
		Enabled[]
		{
			get
			{
				return this._Enabled
			}
			
			set
			{
				this._Enabled := Value
				if (Value)
					this.BuildWordList()
				else
					this.Fragment := ""
				return Value
			}
		}
		
		
		; --- Constructor, Destructor ---
		
		__New(Parent, Enabled:=True)
		{
			this.Parent := Parent
			this.Enabled := Enabled
			this.WineVer := DllCall("ntdll.dll\wine_get_version", "AStr")
			
			; Create the tool GUI for the floating list
			hParentWnd := this.Parent.hMainWindow
			Gui, +hWndhDefaultWnd
			Relation := this.WineVer ? "Parent" Parent.RichCode.hWnd : "Owner" Parent.hMainWindow
			Gui, New, +%Relation% -Caption +ToolWindow +hWndhWnd
			this.hWnd := hWnd
			Gui, Margin, 0, 0
			
			; Create the ListBox control withe appropriate font and styling
			Font := this.Parent.Settings.Font
			Gui, Font, % "s" Font.Size, % Font.Typeface
			Gui, Add, ListBox, x0 y0 r1 0x100 AltSubmit hWndhListBox, Item
			this.hListBox := hListBox
			
			; Finish GUI creation and restore the default GUI
			Gui, Show, Hide, % this.Parent.Title " - AutoComplete"
			Gui, %hDefaultWnd%:Default
			
			; Get relevant dimensions of the ListBox for later resizing
			SendMessage, 0x1A1, 0, 0,, % "ahk_id" this.hListBox ; LB_GETITEMHEIGHT
			this.ListBoxItemHeight := ErrorLevel
			VarSetCapacity(ListBoxRect, 16, 0)
			DllCall("User32.dll\GetClientRect", "Ptr", this.hListBox, "Ptr", &ListBoxRect)
			this.ListBoxMargins := NumGet(ListBoxRect, 12, "Int") - this.ListBoxItemHeight
			
			; Set up the GDI Device Context for later text measurement in _GetWidth
			this.hDC := DllCall("GetDC", "UPtr", this.hListBox, "UPtr")
			SendMessage, 0x31, 0, 0,, % "ahk_id" this.hListBox ; WM_GETFONT
			this.hFont := DllCall("SelectObject", "UPtr", this.hDC, "UPtr", ErrorLevel, "UPtr")
			
			; Record the total screen width for later user. If the monitors get
			; rearranged while the script is still running this value will be
			; inaccurate. However, this will not likely be a significant issue,
			; and the issues caused by it would be minimal.
			SysGet, ScreenWidth, 78
			this.ScreenWidth := ScreenWidth
			
			; Pull a list of default words from the help file.
			; TODO: Include some kind of hard-coded list for when the help file is
			;       not present, or to supplement the help file.
			for Key,Val in this.Parent.Keywords
			{
				this.DefaultWordList .= "|" Key
				for key2, nickname in Val
					(nickname)?this.DefaultWordList .= "|" . nickname
			}

			Loop, Read, bin\dinocode\DinoCode_Native.txt
				this.DefaultWordList .= "|" . A_LoopReadLine
			
			; Build the initial word list based on the default words and the
			; RichCode's contents at the time of AutoComplete's initialization
			this.BuildWordList()
		}
		
		__Delete()
		{
			Gui, % this.hWnd ":Destroy"
			this.Visible := False
			DllCall("SelectObject", "UPtr", this.hDC, "UPtr", this.hFont, "UPtr")
			DllCall("ReleaseDC", "UPtr", this.hListBox, "UPtr", this.hDC)
		}
		
		
		; --- Private Methods ---
		
		; Gets the pixel-based width of a provided text snippet using the GDI font
		; selected into the ListBox control
		_GetWidth(Text)
		{
			MaxWidth := 0
			Loop, Parse, Text, |
			{
				DllCall("GetTextExtentPoint32", "UPtr", this.hDC, "Str", A_LoopField
				, "Int", StrLen(A_LoopField), "Int64*", Size), Size &= 0xFFFFFFFF
				
				if (Size > MaxWidth)
					MaxWidth := Size
			}
			
			return MaxWidth
		}
		
		; Shows the suggestion dialog with contents of the provided DisplayList
		_Show(DisplayList)
		{
			; Insert the new list
			GuiControl,, % this.hListBox, %DisplayList%
			GuiControl, Choose, % this.hListBox, 1

			Font := this.Parent.Settings.Font, ZoomLevel:=(this.Parent.ZoomLevel ? this.Parent.ZoomLevel : 1)
			
			; Resize to fit contents
			StrReplace(DisplayList, "|",, Rows)
			Height := Rows * this.ListBoxItemHeight * ZoomLevel + this.ListBoxMargins
			Width := this._GetWidth(DisplayList) * ZoomLevel + 10
			GuiControl, Move, % this.hListBox, w%Width% h%Height%
			
			; Keep the dialog from running off the screen
			X := this.CaretX, Y := this.CaretY + 20 * ZoomLevel
			if ((X + Width) > this.ScreenWidth)
				X := this.ScreenWidth - Width
			
			; Update Font size based on ZoomLevel
			Gui % this.hWnd ":Font", % "s" Font.Size * ZoomLevel, % Font.Typeface
			GuiControl, Font, % this.hListBox

			; Make the dialog visible
			Gui, % this.hWnd ":Show", x%X% y%Y% AutoSize NoActivate
			this.Visible := True
		}
		
		; Hides the dialog if it is visible
		_Hide()
		{
			if !this.Visible
				return
			
			Gui, % this.hWnd ":Hide"
			this.Visible := False
		}
		
		; Filters the word list for entries starting with the fragment, then
		; shows the dialog with the filtered list as suggestions
		_Suggest()
		{
			; Filter the list for words beginning with the fragment
			Suggestions := LTrim(RegExReplace(this.WordList
			, "i)\|(?!" this.Fragment ")[^\|]+"), "|")
			
			; Fail out if there were no matches
			if !Suggestions
				return true, this._Hide()
			
			; Pull the first MaxSuggestions suggestions
			if (Pos := InStr(Suggestions, "|",,, this.MaxSuggestions))
				Suggestions := SubStr(Suggestions, 1, Pos-1)
			this.Suggestions := Suggestions
			
			this._Show("|" Suggestions)
		}
		
		; Finishes the fragment with the selected suggestion
		_Complete()
		{
			; Get the text of the selected item
			GuiControlGet, Selected,, % this.hListBox
			Suggestion := StrSplit(this.Suggestions, "|")[Selected]
			
			; Replace fragment preceding cursor with selected suggestion
			RC := this.Parent.RichCode
			RC.Selection[1] -= StrLen(this.Fragment)
			RC.SelectedText := Suggestion
			RC.Selection[1] := RC.Selection[2]
			
			; Clear out the fragment in preparation for further typing
			this.Fragment := ""
		}
		
		
		; --- Public Methods ---
		
		; Interpret WM_KEYDOWN messages, the primary means of interfacing with the
		; class. These messages can be provided by registering an appropriate
		; handler with OnMessage, or by forwarding the events from another handler
		; for the control.
		WM_KEYDOWN(wParam, lParam)
		{
			if (!this._Enabled)
				return
			
			; Get the name of the key using the virtual key code. The key's scan
			; code is not used here, but is available in bits 16-23 of lParam and
			; could be used in future versions for greater reliability.
			Key := GetKeyName(Format("vk{:02x}", wParam))
			
			; Treat Numpad variants the same as the equivalent standard keys
			Key := StrReplace(Key, "Numpad")
			
			; Handle presses meant to interact with the dialog, such as
			; navigational, confirmational, or dismissive commands.
			if (this.Visible)
			{
				if (Key == "Tab" || Key == "Enter")
					return False, this._Complete()
				else if (Key == "Up")
					return False, this.SelectUp()
				else if (Key == "Down")
					return False, this.SelectDown()
			}
			
			; Ignore standalone modifier presses, and some modified regular presses
			if Key in Shift,Control,Alt
				return
			
			; Reset on presses with the control modifier
			if GetKeyState("Control")
				return "", this.Fragment := ""
			
			; Subtract from the end of fragment on backspace
			if (Key == "Backspace")
				return "", this.Fragment := SubStr(this.Fragment, 1, -1)
			
			; Apply Shift and CapsLock
			if GetKeyState("Shift")
				Key := StrReplace(Key, "-", "_")
			if (GetKeyState("Shift") ^ GetKeyState("CapsLock", "T"))
				Key := Format("{:U}", Key)
			
			; Reset on unwanted presses -- Allow numbers but not at beginning
			if !(Key ~= "^[A-Za-z_]$" || (this.Fragment != "" && Key ~= "^[0-9]$"))
				return "", this.Fragment := ""
			
			; Record the starting position of new fragments
			if (this.Fragment == "")
			{
				CoordMode, Caret, % this.WineVer ? "Client" : "Screen"
				
				; Round "" to 0, which can prevent errors in the unlikely case that
				; input is received while the control is not focused.
				this.CaretX := Round(A_CaretX), this.CaretY := Round(A_CaretY)
			}
			
			; Update fragment with the press
			this.Fragment .= Key
		}
		
		; Triggers a rebuild of the word list from the RichCode control's contents
		BuildWordList()
		{
			if (!this._Enabled)
				return
			
			; Replace non-word chunks with delimiters
			List := RegExReplace(this.Parent.RichCode.Value, "\W+", "|")
			
			; Ignore numbers at the beginning of words
			List := RegExReplace(List, "\b[0-9]+")
			
			; Ignore words that are too small
			List := RegExReplace(List, "\b\w{1," this.MinWordLen-1 "}\b")
			
			; Append default entries, remove duplicates, and save the list
			List .= this.DefaultWordList
			Sort, List, U D| Z
			this.WordList := "|" Trim(List, "|")
		}
		
		; Moves the selected item in the dialog up one position
		SelectUp()
		{
			GuiControlGet, Selected,, % this.hListBox
			if (--Selected < 1)
				Selected := this.MaxSuggestions
			GuiControl, Choose, % this.hListBox, %Selected%
		}
		
		; Moves the selected item in the dialog down one position
		SelectDown()
		{
			GuiControlGet, Selected,, % this.hListBox
			if (++Selected > this.MaxSuggestions)
				Selected := 1
			GuiControl, Choose, % this.hListBox, %Selected%
		}
	}
}
class WinEvents ; static class
{
	static _ := WinEvents.AutoInit()
	
	AutoInit()
	{
		this.Table := []
		OnMessage(2, this.Destroy.bind(this))
	}
	
	Register(ID, HandlerClass, Prefix="Gui")
	{
		Gui, %ID%: +hWndhWnd +LabelWinEvents_
		this.Table[hWnd] := {Class: HandlerClass, Prefix: Prefix}
	}
	
	Unregister(ID)
	{
		Gui, %ID%: +hWndhWnd
		this.Table.Delete(hWnd)
	}
	
	Dispatch(Type, Params*)
	{
		Info := this.Table[Params[1]]
		return (Info.Class)[Info.Prefix . Type](Params*)
	}
	
	Destroy(wParam, lParam, Msg, hWnd)
	{
		this.Table.Delete(hWnd)
	}
}

WinEvents_Close(Params*) {
	return WinEvents.Dispatch("Close", Params*)
} WinEvents_Escape(Params*) {
	return WinEvents.Dispatch("Escape", Params*)
} WinEvents_Size(Params*) {
	return WinEvents.Dispatch("Size", Params*)
} WinEvents_ContextMenu(Params*) {
	return WinEvents.Dispatch("ContextMenu", Params*)
} WinEvents_DropFiles(Params*) {
	return WinEvents.Dispatch("DropFiles", Params*)
}

; Helper function, to make passing in expressions resulting in function objects easier
SetTimer(Label, Period)
{
	SetTimer, %Label%, %Period%
}

SendMessage(Msg, wParam, lParam, hWnd)
{
	; DllCall("SendMessage", "UPtr", hWnd, "UInt", Msg, "UPtr", wParam, "Ptr", lParam, "UPtr")
	SendMessage, Msg, wParam, lParam,, ahk_id %hWnd%
	return ErrorLevel
}

PostMessage(Msg, wParam, lParam, hWnd)
{
	PostMessage, Msg, wParam, lParam,, ahk_id %hWnd%
	return ErrorLevel
}

RichEdit_AddMargins(hRichEdit, x:=0, y:=0, w:=0, h:=0)
{
	static WineVer := DllCall("ntdll.dll\wine_get_version", "AStr")
	VarSetCapacity(RECT, 16, 0)
	if (x | y | w | h)
	{
		if WineVer
		{
			; Workaround for bug in Wine 3.0.2.
			; This code will need to be updated this code
			; after future Wine releases that fix it.
			NumPut(x, RECT,  0, "Int"), NumPut(y, RECT,  4, "Int")
			NumPut(w, RECT,  8, "Int"), NumPut(h, RECT, 12, "Int")
		}
		else
		{
			if !DllCall("GetClientRect", "UPtr", hRichEdit, "UPtr", &RECT, "UInt")
				throw Exception("Couldn't get RichEdit Client RECT")
			NumPut(x + NumGet(RECT,  0, "Int"), RECT,  0, "Int")
			NumPut(y + NumGet(RECT,  4, "Int"), RECT,  4, "Int")
			NumPut(w + NumGet(RECT,  8, "Int"), RECT,  8, "Int")
			NumPut(h + NumGet(RECT, 12, "Int"), RECT, 12, "Int")
		}
	}
	SendMessage(0xB3, 0, &RECT, hRichEdit)
}
