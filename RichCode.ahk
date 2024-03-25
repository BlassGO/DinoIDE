/*
	class RichCode({"TabSize": 4     ; Width of a tab in characters
		, "Indent": "`t"             ; What text to insert on indent
		, "FGColor": 0xRRGGBB        ; Foreground (text) color
		, "BGColor": 0xRRGGBB        ; Background color
		, "Font"                     ; Font to use
		: {"Typeface": "Courier New" ; Name of the typeface
			, "Size": 12             ; Font size in points
			, "Bold": False}         ; Bolded (True/False)
		
		
		; Whether to use the highlighter, or leave it as plain text
		, "UseHighlighter": True
		
		; Delay after typing before the highlighter is run
		, "HighlightDelay": 200
		
		; The highlighter function (FuncObj or name)
		; to generate the highlighted RTF. It will be passed
		; two parameters, the first being this settings array
		; and the second being the code to be highlighted
		, "Highlighter": Func("HighlightAHK")
		
		; The colors to be used by the highlighter function.
		; This is currently used only by the highlighter, not at all by the
		; RichCode class. As such, the RGB ordering is by convention only.
		; You can add as many colors to this array as you want.
		, "Colors"
		: [0xRRGGBB
			, 0xRRGGBB
			, 0xRRGGBB,
			, 0xRRGGBB]})
*/
class RichCode
{
	static Msftedit := DllCall("LoadLibrary", "Str", "Msftedit.dll")
	static IID_ITextDocument := "{8CC497C0-A1DF-11CE-8098-00AA0047BE5D}"
	static MenuItems := ["Cut", "Copy", "Paste", "Delete", "", "Select All", ""
		, "UPPERCASE", "lowercase", "TitleCase", "Indent", "UnIndent"]
	
	_Frozen := False
	
	; --- Static Methods ---
	
	BGRFromRGB(RGB)
	{
		return RGB>>16&0xFF | RGB&0xFF00 | RGB<<16&0xFF0000
	}
	
	; --- Properties ---
	
	Value[]
	{
		get {
			GuiControlGet, Code,, % this.hWnd
			return Code
		}
		
		set {
			FormatDinoStr(,,,,,{Escape: "\"})
			HighlightDino(,,,true)
			this.Modified:=false
			this.Highlight(Value)
			this.parent.UpdateStatusBar()
			return Value
		}
	}
	
	; TODO: reserve and reuse memory
	Selection[i:=0]
	{
		get {
			VarSetCapacity(CHARRANGE, 8, 0)
			this.SendMsg(0x434, 0, &CHARRANGE) ; EM_EXGETSEL
			Out := [NumGet(CHARRANGE, 0, "Int"), NumGet(CHARRANGE, 4, "Int")]
			return i ? Out[i] : Out
		}
		
		set {
			if i
				Temp := this.Selection, Temp[i] := Value, Value := Temp
			VarSetCapacity(CHARRANGE, 8, 0)
			NumPut(Value[1], &CHARRANGE, 0, "Int") ; cpMin
			NumPut(Value[2], &CHARRANGE, 4, "Int") ; cpMax
			this.SendMsg(0x437, 0, &CHARRANGE) ; EM_EXSETSEL
			return Value
		}
	}
	
	SelectedText[]
	{
		get {
			Selection := this.Selection, Length := Selection[2] - Selection[1]
			VarSetCapacity(Buffer, (Length + 1) * 2) ; +1 for null terminator
			if (this.SendMsg(0x43E, 0, &Buffer) > Length) ; EM_GETSELTEXT
				throw Exception("Text larger than selection! Buffer overflow!")
			Text := StrGet(&Buffer, Selection[2]-Selection[1], "UTF-16")
			return StrReplace(Text, "`r", "`n")
		}
		
		set {
			this.SendMsg(0xC2, 1, &Value) ; EM_REPLACESEL
			this.Selection[1] -= StrLen(Value)
			return Value
		}
	}
	
	EventMask[]
	{
		get {
			return this._EventMask
		}
		
		set {
			this._EventMask := Value
			this.SendMsg(0x445, 0, Value) ; EM_SETEVENTMASK
			return Value
		}
	}
	
	UndoSuspended[]
	{
		get {
			return this._UndoSuspended
		}
		
		set {
			try ; ITextDocument is not implemented in WINE
			{
				if Value
					this.ITextDocument.Undo(-9999995) ; tomSuspend
				else
					this.ITextDocument.Undo(-9999994) ; tomResume
			}
			return this._UndoSuspended := !!Value
		}
	}
	
	Frozen[]
	{
		get {
			return this._Frozen
		}
		
		set {
			if (Value && !this._Frozen)
			{
				try ; ITextDocument is not implemented in WINE
					this.ITextDocument.Freeze()
				catch
					GuiControl, -Redraw, % this.hWnd
			}
			else if (!Value && this._Frozen)
			{
				try ; ITextDocument is not implemented in WINE
					this.ITextDocument.Unfreeze()
				catch
					GuiControl, +Redraw, % this.hWnd
			}
			return this._Frozen := !!Value
		}
	}
	
	Modified[]
	{
		get {
			return this.SendMsg(0xB8, 0, 0) ; EM_GETMODIFY
		}
		
		set {
			this.SendMsg(0xB9, Value, 0) ; EM_SETMODIFY
			return Value
		}
	}
	
	; --- Construction, Destruction, Meta-Functions ---
	
	__New(Settings, Options:="", IDE:="")
	{
		static Test
		this.Settings := Settings
		FGColor := this.BGRFromRGB(Settings.FGColor)
		BGColor := this.BGRFromRGB(Settings.BGColor)
		this.parent:=IDE
		
		Gui, Add, Custom, ClassRichEdit50W hWndhWnd +0x5031b1c4 +E0x20000 %Options%
		this.hWnd := hWnd
		Gui, +LastFoundExist
      	this.GuiHwnd := WinExist()

		; Enable WordWrap in RichEdit control ("WordWrap" : true)
		if this.Settings.WordWrap
			SendMessage, 0x0448, 0, 0, , % "ahk_id " . This.HWND

		; Register for WM_COMMAND and WM_NOTIFY events
		; NOTE: this prevents garbage collection of
		; the class until the control is destroyed
		this.EventMask := 1 ; ENM_CHANGE
		CtrlEvent := this.CtrlEvent.Bind(this)
		GuiControl, +g, %hWnd%, %CtrlEvent%
		
		; Set background color
		this.SendMsg(0x443, 0, BGColor) ; EM_SETBKGNDCOLOR
		
		; Set character format
		VarSetCapacity(CHARFORMAT2, 116, 0)
		NumPut(116,                    CHARFORMAT2, 0,  "UInt")       ; cbSize      = sizeof(CHARFORMAT2)
		NumPut(0xE0000000,             CHARFORMAT2, 4,  "UInt")       ; dwMask      = CFM_COLOR|CFM_FACE|CFM_SIZE
		NumPut(FGColor,                CHARFORMAT2, 20, "UInt")       ; crTextColor = 0xBBGGRR
		NumPut(Settings.Font.Size*20,  CHARFORMAT2, 12, "UInt")       ; yHeight     = twips
		StrPut(Settings.Font.Typeface, &CHARFORMAT2+26, 32, "UTF-16") ; szFaceName  = TCHAR
		this.SendMsg(0x444, 0, &CHARFORMAT2) ; EM_SETCHARFORMAT
		
		; Set tab size to 4 for non-highlighted code
		VarSetCapacity(TabStops, 4, 0), NumPut(Settings.TabSize*4, TabStops, "UInt")
		this.SendMsg(0x0CB, 1, &TabStops) ; EM_SETTABSTOPS
		
		; Change text limit from 32,767 to max
		this.SendMsg(0x435, 0, -1) ; EM_EXLIMITTEXT
		
		; Bind for keyboard events
		; Use a pointer to prevent reference loop
		this.OnMessageBound := this.OnMessage.Bind(&this)
		OnMessage(0x100, this.OnMessageBound) ; WM_KEYDOWN
		OnMessage(0x205, this.OnMessageBound) ; WM_RBUTTONUP
		OnMessage(0x102, this.OnMessageBound) ; WM_CHAR
		
		; Bind the highlighter
		this.HighlightBound := this.Highlight.Bind(&this)
		
		; Create the right click menu
		this.MenuName := this.__Class . &this
		RCMBound := this.RightClickMenu.Bind(&this)
		for Index, Entry in this.MenuItems
			Menu, % this.MenuName, Add, %Entry%, %RCMBound%
		
		; Get the ITextDocument object
		VarSetCapacity(pIRichEditOle, A_PtrSize, 0)
		this.SendMsg(0x43C, 0, &pIRichEditOle) ; EM_GETOLEINTERFACE
		this.pIRichEditOle := NumGet(pIRichEditOle, 0, "UPtr")
		this.IRichEditOle := ComObject(9, this.pIRichEditOle, 1), ObjAddRef(this.pIRichEditOle)
		this.pITextDocument := ComObjQuery(this.IRichEditOle, this.IID_ITextDocument)
		this.ITextDocument := ComObject(9, this.pITextDocument, 1), ObjAddRef(this.pITextDocument)
	}
	
	RightClickMenu(ItemName, ItemPos, MenuName)
	{
		if !IsObject(this)
			this := Object(this)
		
		if (ItemName == "Cut")
			Clipboard := this.SelectedText, this.SelectedText := ""
		else if (ItemName == "Copy")
			Clipboard := this.SelectedText
		else if (ItemName == "Paste")
			this.SelectedText := Clipboard
		else if (ItemName == "Delete")
			this.SelectedText := ""
		else if (ItemName == "Select All")
			this.Selection := [0, -1]
		else if (ItemName == "UPPERCASE")
			this.SelectedText := Format("{:U}", this.SelectedText)
		else if (ItemName == "lowercase")
			this.SelectedText := Format("{:L}", this.SelectedText)
		else if (ItemName == "TitleCase")
			this.SelectedText := Format("{:T}", this.SelectedText)
		else if (ItemName == "Indent")
			this.IndentSelection()
		else if (ItemName == "UnIndent")
			this.IndentSelection(true)
	}
	
	__Delete()
	{
		; Release the ITextDocument object
		this.ITextDocument := "", ObjRelease(this.pITextDocument)
		this.IRichEditOle := "", ObjRelease(this.pIRichEditOle)
		
		; Release the OnMessage handlers
		OnMessage(0x100, this.OnMessageBound, 0) ; WM_KEYDOWN
		OnMessage(0x205, this.OnMessageBound, 0) ; WM_RBUTTONUP
		OnMessage(0x102, this.OnMessageBound, 0) ; WM_CHAR
		
		; Destroy the right click menu
		Menu, % this.MenuName, Delete
		
		HighlightBound := this.HighlightBound
		SetTimer, %HighlightBound%, Delete
	}
	
	; --- Event Handlers ---
	
	OnMessage(wParam, lParam, Msg, hWnd)
	{
		if !IsObject(this)
			this := Object(this)
		if (hWnd != this.hWnd)
			return
		
		if (Msg == 0x100) ; WM_KEYDOWN
		{
			if (wParam == GetKeyVK("Tab"))
			{
				; Indentation
				Selection := this.Selection
				if GetKeyState("Shift")
					this.IndentSelection(True) ; Reverse
				else if (Selection[2] - Selection[1]) ; Something is selected
					this.IndentSelection()
				else
				{
					; TODO: Trim to size needed to reach next TabSize
					this.SelectedText := this.Settings.Indent
					this.Selection[1] := this.Selection[2] ; Place cursor after
				}
				return False
			}
			else if (wParam == GetKeyVK("Escape")) ; Normally closes the window
				return False
			else if (wParam == GetKeyVK("v") && GetKeyState("Ctrl"))
			{
				this.SelectedText := Clipboard ; Strips formatting
				this.Selection[1] := this.Selection[2] ; Place cursor after
				return False
			}
		}
		else if (Msg == 0x205) ; WM_RBUTTONUP
		{
			Menu, % this.MenuName, Show
			return False
		}
		else if (Msg == 0x102) ; WM_CHAR
		{
			if (wParam=13) {
				Top := this.SendMsg(0x436, 0, this.Selection[1]) ; EM_EXLINEFROMCHAR
				ControlGet, Line, Line, % Top,, % "ahk_id" this.hWnd
				if RegExMatch(Line, "^[ `t]+", Indent)
					CursorPosBefore := this.Selection[1], len:=StrLen(Indent), this.SelectedText:=Indent, this.Selection := [CursorPosBefore+len, CursorPosBefore+len]
				return true
			}
			switch (Chr(wParam))
			{
				case "(":
					CursorPosBefore := this.Selection[1]
					this.SelectedText := "()"
					CursorPosAfter := CursorPosBefore + 1
					this.Selection := [CursorPosAfter, CursorPosAfter]
					return False
				case "[":
					CursorPosBefore := this.Selection[1]
					this.SelectedText := "[]"
					CursorPosAfter := CursorPosBefore + 1
					this.Selection := [CursorPosAfter, CursorPosAfter]
					return False
				case """":
					CursorPosBefore := this.Selection[1]
					this.SelectedText := """"""
					CursorPosAfter := CursorPosBefore + 1
					this.Selection := [CursorPosAfter, CursorPosAfter]
					return False
				case "%":
					CursorPosBefore := this.Selection[1]
					this.SelectedText := "%%"
					CursorPosAfter := CursorPosBefore + 1
					this.Selection := [CursorPosAfter, CursorPosAfter]
					return False
			}
			
		}
	}
	
	CtrlEvent(CtrlHwnd, GuiEvent, EventInfo, _ErrorLevel:="")
	{
		if (GuiEvent == "Normal" && EventInfo == 0x300) ; EN_CHANGE
		{
			; Delay until the user is finished changing the document
			HighlightBound := this.HighlightBound
			SetTimer, %HighlightBound%, % -Abs(this.Settings.HighlightDelay)
		}
	}
	
	; --- Methods ---
	
	; First parameter is taken as a replacement value
	; Variadic form is used to detect when a parameter is given,
	; regardless of content
	Highlight(NewVal*)
	{
		if !IsObject(this)
			this := Object(this)
		if !(this.Settings.UseHighlighter && this.Settings.Highlighter)
		{
			if NewVal.Length()
				GuiControl,, % this.hWnd, % NewVal[1]
			return
		}
		
		; Freeze the control while it is being modified, stop change event
		; generation, suspend the undo buffer, buffer any input events
		PrevFrozen := this.Frozen, this.Frozen := True
		PrevEventMask := this.EventMask, this.EventMask := 0 ; ENM_NONE
		PrevUndoSuspended := this.UndoSuspended, this.UndoSuspended := True
		PrevCritical := A_IsCritical
		Critical, 1000
		
		; Run the highlighter
		Highlighter := this.Settings.Highlighter
		RTF := %Highlighter%(this.Settings, NewVal.Length() ? NewVal[1] : this.Value)
		
		; "TRichEdit suspend/resume undo function"
		; https://stackoverflow.com/a/21206620
		
		; Save the rich text to a UTF-8 buffer
		VarSetCapacity(Buf, StrPut(RTF, "UTF-8"), 0)
		StrPut(RTF, &Buf, "UTF-8")
		
		; Set up the necessary structs
		VarSetCapacity(ZOOM,      8, 0) ; Zoom Level
		VarSetCapacity(POINT,     8, 0) ; Scroll Pos
		VarSetCapacity(CHARRANGE, 8, 0) ; Selection
		VarSetCapacity(SETTEXTEX, 8, 0) ; SetText Settings
		NumPut(1, SETTEXTEX, 0, "UInt") ; flags = ST_KEEPUNDO
		
		; Save the scroll and cursor positions, update the text,
		; then restore the scroll and cursor positions
		MODIFY := this.SendMsg(0xB8, 0, 0)    ; EM_GETMODIFY
		this.SendMsg(0x4E0, &ZOOM, &ZOOM+4)   ; EM_GETZOOM
		this.SendMsg(0x4DD, 0, &POINT)        ; EM_GETSCROLLPOS
		this.SendMsg(0x434, 0, &CHARRANGE)    ; EM_EXGETSEL
		this.SendMsg(0x461, &SETTEXTEX, &Buf) ; EM_SETTEXTEX
		this.SendMsg(0x437, 0, &CHARRANGE)    ; EM_EXSETSEL
		this.SendMsg(0x4DE, 0, &POINT)        ; EM_SETSCROLLPOS
		this.SendMsg(0x4E1, NumGet(ZOOM, "UInt")
			, NumGet(ZOOM, 4, "UInt"))        ; EM_SETZOOM
		this.SendMsg(0xB9, MODIFY, 0)         ; EM_SETMODIFY
		
		; Restore previous settings
		Critical, %PrevCritical%
		this.UndoSuspended := PrevUndoSuspended
		this.EventMask := PrevEventMask
		this.Frozen := PrevFrozen
	}
	
	IndentSelection(Reverse:=False, Indent:="")
	{
		; Freeze the control while it is being modified, stop change event
		; generation, buffer any input events
		PrevFrozen := this.Frozen, this.Frozen := True
		PrevEventMask := this.EventMask, this.EventMask := 0 ; ENM_NONE
		PrevCritical := A_IsCritical
		Critical, 1000
		
		if (Indent == "")
			Indent := this.Settings.Indent
		IndentLen := StrLen(Indent)
		
		; Select back to the start of the first line
		Min := this.Selection[1]
		Top := this.SendMsg(0x436, 0, Min) ; EM_EXLINEFROMCHAR
		TopLineIndex := this.SendMsg(0xBB, Top, 0) ; EM_LINEINDEX
		this.Selection[1] := TopLineIndex
		
		; TODO: Insert newlines using SetSel/ReplaceSel to avoid having to call
		; the highlighter again
		Text := this.SelectedText
		if Reverse
		{
			; Remove indentation appropriately
			Loop, Parse, Text, `n, `r
			{
				if (InStr(A_LoopField, Indent) == 1)
				{
					Out .= "`n" SubStr(A_LoopField, 1+IndentLen)
					if (A_Index == 1)
						Min -= IndentLen
				}
				else
					Out .= "`n" A_LoopField
			}
			this.SelectedText := SubStr(Out, 2)
			
			; Move the selection start back, but never onto the previous line
			this.Selection[1] := Min < TopLineIndex ? TopLineIndex : Min
		}
		else
		{
			; Add indentation appropriately
			Trailing := (SubStr(Text, 0) == "`n")
			Temp := Trailing ? SubStr(Text, 1, -1) : Text
			Loop, Parse, Temp, `n, `r
				Out .= "`n" Indent . A_LoopField
			this.SelectedText := SubStr(Out, 2) . (Trailing ? "`n" : "")
			
			; Move the selection start forward
			this.Selection[1] := Min + IndentLen
		}
		
		this.Highlight()
		
		; Restore previous settings
		Critical, %PrevCritical%
		this.EventMask := PrevEventMask
		
		; When content changes cause the horizontal scrollbar to disappear,
		; unfreezing causes the scrollbar to jump. To solve this, jump back
		; after unfreezing. This will cause a flicker when that edge case
		; occurs, but it's better than the alternative.
		VarSetCapacity(POINT, 8, 0)
		this.SendMsg(0x4DD, 0, &POINT) ; EM_GETSCROLLPOS
		this.Frozen := PrevFrozen
		this.SendMsg(0x4DE, 0, &POINT) ; EM_SETSCROLLPOS
	}
	
	; --- Helper/Convenience Methods ---
	
	SendMsg(Msg, wParam, lParam)
	{
		SendMessage, Msg, wParam, lParam,, % "ahk_id" this.hWnd
		return ErrorLevel
	}

	LoadFile(File, Mode = "Open") { ; Load file
		; File : file name
		; Mode : Open / Add / Insert
		;        Open   : Replace control's content
		;        Append : Append to conrol's content
		;        Insert : Insert at / replace current selection
		If !FileExist(File)
			Return False
		FileEncoding, UTF-8
		FileRead, Text, %File%
		If (Mode = "Open") {
			if this.Open_File && !this.FileClose()
				return 0
			This.Value := Text
			this.Add_Recent(File)
		} Else If (Mode = "Insert") {
			This.ReplaceSel(Text)
		} Else If (Mode = "Append") {
			This.SetSel(-1, -2)
			This.ReplaceSel(Text)
		}
		Return True
   }

   FileOpen(Opt:="") {
		If (File := this.FileDlg("O")) {
			GuiControl, Focus, % RC.HWND
			return this.LoadFile(File, Opt)
		}
		return 0
   }

   FileSave() {
		If !(this.Open_File) {
			Return this.FileSaveAs()
		}
		GuiControl, Focus, % this.HWND
		return this.SaveFile()
   }

   FileSaveAs(isnew:=false) {
		If (File := this.FileDlg("S")) {
			if isnew {
				if (this.Open_File && !this.FileClose())
					return 0
				this.Value:=":MAIN`n   "
				this.Selection:=[9,9]
			}
			result:=this.SaveFile(File)
		}
		GuiControl, Focus, % this.HWND
		return result
   }

   FileClose(refresh:=false) {
		result:=true
		If (this.Open_File) {
			If this.Modified {
				Gui, +OwnDialogs
				MsgBox, 35, Close File, Content has been modified!`nDo you want to save changes?
				IfMsgBox, Cancel
				{
					GuiControl, Focus, % this.HWND
					Return 0
				}
				IfMsgBox, Yes
					result:=this.FileSave()
			}
			if !refresh {
				Gui, +LastFound
				WinGetTitle, Title
				StringSplit, Title, Title, -, %A_Space%
				WinSetTitle, %Title1%
				this.Open_File:=this.Value:=""
			}
			GuiControl, Focus, % this.HWND
		}
		return result
   }

   Add_Recent(File:="") {
		global ini
		Menu, Opened, DeleteAll
		if (ini="")
			return
		if File {
			IniWrite, %File%, %ini%, GENERAL, last_opened
			Menu, Opened, Add, %File%, Open_Recent
			Gui, +LastFound
			WinGetTitle, Title
			StringSplit, Title, Title, -, %A_Space%
			WinSetTitle, %Title1% - %File%
			this.Open_File:=new_list:=File
			Arg_in(this.Open_File)
			Arg_OrigFilename(basename(this.Open_File))
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

   GetZoom() { ; Gets the current zoom ratio.
      ; Returns the zoom ratio in percent.
      ; EM_GETZOOM = 0x04E0
      VarSetCapacity(N, 4, 0), VarSetCapacity(D, 4, 0)
      SendMessage, 0x04E0, &N, &D, , % "ahk_id " . This.HWND
      N := NumGet(N, 0, "Int"), D := NumGet(D, 0, "Int")
      Return (N = 0) && (D = 0) ? 100 : Round(N / D * 100)
   }

   SetZoom(Ratio := "") { ; Sets the zoom ratio of a rich edit control.
      ; Ratio : Float value between 100/64 and 6400; a ratio of 0 turns zooming off.
      ; EM_SETZOOM = 0x4E1
      SendMessage, 0x4E1, % (Ratio > 0 ? Ratio : 100), 100, , % "ahk_id " . This.HWND
      Return ErrorLevel
   }

   ReplaceSel(Text := "") { ; Replaces the selected text with the specified text.
      ; EM_REPLACESEL = 0xC2
      SendMessage, 0xC2, 1, &Text, , % "ahk_id " . This.HWND
      Return ErrorLevel
   }

   SetSel(Start, End) { ; Selects a range of characters.
      ; Start : zero-based start index
      ; End   : zero-beased end index (-1 = end of text))
      ; EM_EXSETSEL = 0x0437
      VarSetCapacity(CR, 8, 0)
      NumPut(Start, CR, 0, "Int")
      NumPut(End,   CR, 4, "Int")
      SendMessage, 0x0437, 0, &CR, , % "ahk_id " . This.HWND
      Return ErrorLevel
   }

	SaveFile(File:="") { ; Save file
      ; File : file name
      ; Returns True on success, otherwise False.
	  static FileObj
      Gui, +OwnDialogs
	  FileEncoding, UTF-8
	  (File="")?File:=this.Open_File:add:=true
      If IsObject(FileObj := FileOpen(File, "w")) {
		 (add)?this.Add_Recent(File)
         FileObj.Write(this.Value) ? this.Modified:=false
         FileObj.Close(), FileObj:=""
      }
      Return !this.Modified
   }

   ; ===================================================================================================================
   ; ===================================================================================================================
   ; RICHEDIT COMMON DIALOGS ===========================================================================================
   ; ===================================================================================================================
   ; ===================================================================================================================
   ; Most of the following methods are based on DLG 5.01 by majkinetor
   ; http://www.autohotkey.com/board/topic/15836-module-dlg-501/
   ; ===================================================================================================================
   ; ===================================================================================================================
   FindText() { ; Find dialog box
		Static FINDMSGSTRING := "commdlg_FindReplace"
			, FR_DOWN := 1, FR_MATCHCASE := 4, FR_WHOLEWORD := 2
			, Buf := "", FR := "", Len := 256
			, FR_Size := A_PtrSize * 10
		Text := this.SelectedText
		VarSetCapacity(FR, FR_Size, 0)
		NumPut(FR_Size, FR, 0, "UInt")
		VarSetCapacity(Buf, Len, 0)
		If (Text && !RegExMatch(Text, "\W"))
			Buf := Text
		Offset := A_PtrSize
		NumPut(this.GuiHwnd, FR, Offset, "UPtr") ; hwndOwner
		OffSet += A_PtrSize * 2
		NumPut(FR_DOWN, FR, Offset, "UInt")	   ; Flags
		OffSet += A_PtrSize
		NumPut(&Buf, FR, Offset, "UPtr")	      ; lpstrFindWhat
		OffSet += A_PtrSize * 2
		NumPut(Len,	FR, Offset, "Short")       ; wFindWhatLen
		this.FindTextProc("Init", this.HWND, "")
		OnMessage(DllCall("User32.dll\RegisterWindowMessage", "Str", FINDMSGSTRING), "RichCode.FindTextProc")
		Return DllCall("Comdlg32.dll\FindTextW", "Ptr", &FR, "UPtr")
   }

   FindTextProc(L, M, H) { ; skipped wParam, can be found in "This" when called by system
      ; Find dialog callback procedure
      ; EM_FINDTEXTEXW = 0x047C, EM_EXGETSEL = 0x0434, EM_EXSETSEL = 0x0437, EM_SCROLLCARET = 0x00B7
      ; FR_DOWN = 1, FR_WHOLEWORD = 2, FR_MATCHCASE = 4,
   	  Static FINDMSGSTRING := "commdlg_FindReplace"
   	     , FR_DOWN := 1, FR_MATCHCASE := 4, FR_WHOLEWORD := 2 , FR_FINDNEXT := 0x8, FR_DIALOGTERM := 0x40
         , HWND := 0
      If (L = "Init") {
         HWND := M
         Return True
      }
      Flags := NumGet(L + 0, A_PtrSize * 3, "UInt")
      If (Flags & FR_DIALOGTERM) {
         OnMessage(DllCall("User32.dll\RegisterWindowMessage", "Str", FINDMSGSTRING), "")
         ControlFocus, , ahk_id %HWND%
         HWND := 0
         Return
      }
      VarSetCapacity(CR, 8, 0)
      SendMessage, 0x0434, 0, &CR, , ahk_id %HWND%
      Min := (Flags & FR_DOWN) ? NumGet(CR, 4, "Int") : NumGet(CR, 0, "Int")
      Max := (Flags & FR_DOWN) ? -1 : 0
      OffSet := A_PtrSize * 4
      Find := StrGet(NumGet(L + Offset, 0, "UPtr"))
      VarSetCapacity(FTX, 16 + A_PtrSize, 0)
      NumPut(Min, FTX, 0, "Int")
      NumPut(Max, FTX, 4, "Int")
      NumPut(&Find, FTX, 8, "Ptr")
      SendMessage, 0x047C, %Flags%, &FTX, , ahk_id %HWND%
      S := NumGet(FTX, 8 + A_PtrSize, "Int"), E := NumGet(FTX, 12 + A_PtrSize, "Int")
      If (S = -1) && (E = -1)
         MsgBox, 262208, Find, No text matches!
      Else {
         Min := (Flags & FR_DOWN) ? E : S
         SendMessage, 0x0437, 0, % (&FTX + 8 + A_PtrSize), , ahk_id %HWND%
         SendMessage, 0x00B7, 0, 0, , ahk_id %HWND%
      }
   }

   ReplaceText() { ; Replace dialog box
		Static FINDMSGSTRING := "commdlg_FindReplace"
			, FR_DOWN := 1, FR_MATCHCASE := 4, FR_WHOLEWORD := 2
			, FBuf := "", RBuf := "", FR := "", Len := 256
			, FR_Size := A_PtrSize * 10
		Text := this.SelectedText
		VarSetCapacity(FBuf, Len, 0)
		VarSetCapacity(RBuf, Len, 0)
		VarSetCapacity(FR, FR_Size, 0)
		NumPut(FR_Size, FR, 0, "UInt")
		If (Text && !RegExMatch(Text, "\W"))
			FBuf := Text
		Offset := A_PtrSize
		NumPut(this.GuiHwnd, FR, Offset, "UPtr") ; hwndOwner
		OffSet += A_PtrSize * 2
		NumPut(FR_DOWN, FR, Offset, "UInt")	   ; Flags
		OffSet += A_PtrSize
		NumPut(&FBuf, FR, Offset, "UPtr")      ; lpstrFindWhat
		OffSet += A_PtrSize
		NumPut(&RBuf, FR, Offset, "UPtr")      ; lpstrReplaceWith
		OffSet += A_PtrSize
		NumPut(Len,	FR, Offset, "Short")       ; wFindWhatLen
		NumPut(Len,	FR, Offset + 2, "Short")   ; wReplaceWithLen
		this.ReplaceTextProc("Init", this.HWND, "")
		OnMessage(DllCall("User32.dll\RegisterWindowMessage", "Str", FINDMSGSTRING), "RichCode.ReplaceTextProc")
		Return DllCall("Comdlg32.dll\ReplaceText", "Ptr", &FR, "UPtr")
   }

   ReplaceTextProc(L, M, H) { ; skipped wParam, can be found in "This" when called by system
      ; Replace dialog callback procedure
      ; EM_FINDTEXTEXW = 0x047C, EM_EXGETSEL = 0x0434, EM_EXSETSEL = 0x0437
      ; EM_REPLACESEL = 0xC2, EM_SCROLLCARET = 0x00B7
      ; FR_DOWN = 1, FR_WHOLEWORD = 2, FR_MATCHCASE = 4,
		Static FINDMSGSTRING := "commdlg_FindReplace"
			, FR_DOWN := 1, FR_MATCHCASE := 4, FR_WHOLEWORD := 2, FR_FINDNEXT := 0x8
			, FR_REPLACE := 0x10, FR_REPLACEALL=0x20, FR_DIALOGTERM := 0x40
			, HWND := 0, Min := "", Max := "", FS := "", FE := ""
			, OffFind := A_PtrSize * 4, OffRepl := A_PtrSize * 5
		If (L = "Init") {
			HWND := M, FS := "", FE := ""
			Return True
		}
		Flags := NumGet(L + 0, A_PtrSize * 3, "UInt")
		If (Flags & FR_DIALOGTERM) {
			OnMessage(DllCall("User32.dll\RegisterWindowMessage", "Str", FINDMSGSTRING), "")
			ControlFocus, , ahk_id %HWND%
			HWND := 0
			Return
		}
		If (Flags & FR_REPLACE) {
			IF (FS >= 0) && (FE >= 0) {
				SendMessage, 0xC2, 1, % NumGet(L + 0, OffRepl, "UPtr" ), , ahk_id %HWND%
				Flags |= FR_FINDNEXT
			} Else {
				Return
			}
		}
		If (Flags & FR_FINDNEXT) {
			VarSetCapacity(CR, 8, 0)
			SendMessage, 0x0434, 0, &CR, , ahk_id %HWND%
			Min := NumGet(CR, 4)
			FS := FE := ""
			Find := StrGet(NumGet(L + OffFind, 0, "UPtr"))
			VarSetCapacity(FTX, 16 + A_PtrSize, 0)
			NumPut(Min, FTX, 0, "Int")
			NumPut(-1, FTX, 4, "Int")
			NumPut(&Find, FTX, 8, "Ptr")
			SendMessage, 0x047C, %Flags%, &FTX, , ahk_id %HWND%
			S := NumGet(FTX, 8 + A_PtrSize, "Int"), E := NumGet(FTX, 12 + A_PtrSize, "Int")
			If (S = -1) && (E = -1)
				MsgBox, 262208, Replace, No text matches!
			Else {
				SendMessage, 0x0437, 0, % (&FTX + 8 + A_PtrSize), , ahk_id %HWND%
				SendMessage, 0x00B7, 0, 0, , ahk_id %HWND%
				FS := S, FE := E
			}
			Return
		}
		If (Flags & FR_REPLACEALL) {
			VarSetCapacity(CR, 8, 0)
			SendMessage, 0x0434, 0, &CR, , ahk_id %HWND%
			If (FS = "")
				FS := FE := 0
			DllCall("User32.dll\LockWindowUpdate", "Ptr", HWND)
			Find := StrGet(NumGet(L + OffFind, 0, "UPtr"))
			VarSetCapacity(FTX, 16 + A_PtrSize, 0)
			NumPut(FS, FTX, 0, "Int")
			NumPut(-1, FTX, 4, "Int")
			NumPut(&Find, FTX, 8, "Ptr")
			While (FS >= 0) && (FE >= 0) {
				SendMessage, 0x044F, %Flags%, &FTX, , ahk_id %HWND%
				FS := NumGet(FTX, A_PtrSize + 8, "Int"), FE := NumGet(FTX, A_PtrSize + 12, "Int")
				If (FS >= 0) && (FE >= 0) {
				SendMessage, 0x0437, 0, % (&FTX + 8 + A_PtrSize), , ahk_id %HWND%
				SendMessage, 0xC2, 1, % NumGet(L + 0, OffRepl, "UPtr" ), , ahk_id %HWND%
				NumPut(FE, FTX, 0, "Int")
				}
			}
			SendMessage, 0x0437, 0, &CR, , ahk_id %HWND%
			DllCall("User32.dll\LockWindowUpdate", "Ptr", 0)
			Return
		}
   }

   FileDlg(Mode, File := "") { ; Open and save as dialog box
   	  ; ===================================================================================================================
      ; Mode : O = Open, S = Save
      ; File : optional file name
		Static OFN_ALLOWMULTISELECT := 0x200,    OFN_EXTENSIONDIFFERENT := 0x400, OFN_CREATEPROMPT := 0x2000
			, OFN_DONTADDTORECENT := 0x2000000, OFN_FILEMUSTEXIST := 0x1000,     OFN_FORCESHOWHIDDEN := 0x10000000
			, OFN_HIDEREADONLY := 0x4,          OFN_NOCHANGEDIR := 0x8,          OFN_NODEREFERENCELINKS := 0x100000
			, OFN_NOVALIDATE := 0x100,          OFN_OVERWRITEPROMPT := 0x2,      OFN_PATHMUSTEXIST := 0x800
			, OFN_READONLY := 0x1,              OFN_SHOWHELP := 0x10,            OFN_NOREADONLYRETURN := 0x8000
			, OFN_NOTESTFILECREATE := 0x10000,  OFN_ENABLEXPLORER := 0x80000
			, OFN_Size := (4 * 5) + (2 * 2) + (A_PtrSize * 16)
		Static FilterN1 := "DinoScript (.config)",   FilterP1 :=  "*.config", DefExt := "config", DefFilter := 1
			;, FilterN2 := "Text",       FilterP2 := "*.txt"
			;, FilterN3 := "AutoHotkey", FilterP3 := "*.ahk"
		SplitPath, File, Name, Dir
		Flags := OFN_ENABLEXPLORER
		Flags |= Mode = "O" ? OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY
							: OFN_OVERWRITEPROMPT
		VarSetCapacity(FileName, 1024, 0)
		FileName := Name, workdir:=A_WorkingDir
		LenN1 := (StrLen(FilterN1) + 1) * 2, LenP1 := (StrLen(FilterP1) + 1) * 2
		;LenN2 := (StrLen(FilterN2) + 1) * 2, LenP2 := (StrLen(FilterP2) + 1) * 2
		;LenN3 := (StrLen(FilterN3) + 1) * 2, LenP3 := (StrLen(FilterP3) + 1) * 2
		VarSetCapacity(Filter, LenN1 + LenP1 + 4, 0)
		Adr := &Filter
		StrPut(FilterN1, Adr)
		StrPut(FilterP1, Adr += LenN1)
		/*
		StrPut(FilterN2, Adr += LenP1)
		StrPut(FilterP2, Adr += LenN2)
		StrPut(FilterN3, Adr += LenP2)
		StrPut(FilterP3, Adr += LenN3)
		*/
		VarSetCapacity(OFN , OFN_Size, 0)      ; OPENFILENAME Structure
		NumPut(OFN_Size, OFN, 0, "UInt")
		Offset := A_PtrSize
		NumPut(this.GuiHwnd, OFN, Offset, "Ptr") ; HWND owner
		Offset += A_PtrSize * 2
		NumPut(&Filter, OFN, OffSet, "Ptr")    ; Pointer to FilterStruc
		OffSet += (A_PtrSize * 2) + 4
		OffFilter := Offset
		NumPut(DefFilter, OFN, Offset, "UInt") ; DefaultFilter Pair
		OffSet += 4
		NumPut(&FileName, OFN, OffSet, "Ptr")  ; lpstrFile / InitialisationFileName
		Offset += A_PtrSize
		NumPut(512, OFN, Offset, "UInt")       ; MaxFile / lpstrFile length
		OffSet += A_PtrSize * 3
		NumPut(&Dir, OFN, Offset, "Ptr")       ; StartDir
		Offset += A_PtrSize * 2
		NumPut(Flags, OFN, Offset, "UInt")     ; Flags
		Offset += 8
		NumPut(&DefExt, OFN, Offset, "Ptr")    ; DefaultExt
		R := Mode = "S" ? DllCall("Comdlg32.dll\GetSaveFileNameW", "Ptr", &OFN, "UInt")
						: DllCall("Comdlg32.dll\GetOpenFileNameW", "Ptr", &OFN, "UInt")
		; Ensure preserving the current work path
		SetWorkingDir, % workdir
		If !(R)
			Return ""
		DefFilter := NumGet(OFN, OffFilter, "UInt")
		Return StrGet(&FileName)
   }

}
