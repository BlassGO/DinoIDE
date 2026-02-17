; DinoIDE Menu Definitions

InitializeMenus() {
	global Zoom, UIConfig
	
	Zoom := UIConfig.Zoom
	
	; Zoom menu
	Menu, Opened, Add
	Menu, %MENU_ZOOM%, Add, 200 `%, Zoom
	Menu, %MENU_ZOOM%, Add, 150 `%, Zoom
	Menu, %MENU_ZOOM%, Add, 125 `%, Zoom
	Menu, %MENU_ZOOM%, Add, 100 `%, Zoom100
	Menu, %MENU_ZOOM%, Check, 100 `%
	Menu, %MENU_ZOOM%, Add, 75 `%, Zoom
	Menu, %MENU_ZOOM%, Add, 50 `%, Zoom
	
	; File menu
	Menu, %MENU_FILE%, Add, &New, FileNew
	Menu, %MENU_FILE%, Add, &Open, FileOpen
	Menu, %MENU_FILE%, Add, &Open Recent, :Opened
	Menu, %MENU_FILE%, Add, &Open Script Dir, FileOpenDir
	Menu, %MENU_FILE%, Add, &Append, FileAppend
	Menu, %MENU_FILE%, Add, &Insert, FileInsert
	Menu, %MENU_FILE%, Add, &Close, FileClose
	Menu, %MENU_FILE%, Add, &Save, FileSave
	Menu, %MENU_FILE%, Add, Save &as, FileSaveAs
	Menu, %MENU_FILE%, Add, Version, Version 
	Menu, %MENU_FILE%, Add, &Exit, Close
	
	; Search menu
	Menu, %MENU_SEARCH%, Add, &Find-Replace, Find
	
	; Build menu
	Menu, %MENU_BUILD%, Add, RUN, RUN
	Menu, %MENU_BUILD%, Add
	Menu, %MENU_BUILD%, Add, Run with Terminal, RUN_WITH_TERMINAL
	Menu, %MENU_BUILD%, Add, Run with CMD, RUN_WITH_CMD
	Menu, %MENU_BUILD%, Add
	Menu, %MENU_BUILD%, Add, Initialize Console, INIT_CONSOLE
	Menu, %MENU_BUILD%, Add, Clear Console, CLEAR_CONSOLE
	Menu, %MENU_BUILD%, Add, Kill Console, KILL_CONSOLE

	; Examples menu
	Menu, %MENU_EXAMPLES%, Add
	LoadExampleMenus()
	
	; Comments menu
	Menu, %MENU_COMMENTS%, Add, Block Comment (Selected text), Comments
	Menu, %MENU_COMMENTS%, Add, Block Uncomment (Selected text), Comments
	Menu, %MENU_COMMENTS%, Add, Remove ALL Comments (Selected text), Comments
	
	; View menu
	Menu, %MENU_VIEW%, Add, Prediction, AutoComplete
	Menu, %MENU_VIEW%, Add, Highlighter, Highlighter
	Menu, %MENU_VIEW%, Add, Always On Top, ToggleOnTop
	Menu, %MENU_VIEW%, Add, &Zoom, :%MENU_ZOOM%
	Menu, %MENU_VIEW%, Check, Prediction
	Menu, %MENU_VIEW%, Check, Highlighter
	Menu, GuiMenu, Add, &File, :%MENU_FILE%
	Menu, GuiMenu, Add, &Search, :%MENU_SEARCH%
	Menu, GuiMenu, Add, &Build, :%MENU_BUILD%
	Menu, GuiMenu, Add, &View, :%MENU_VIEW%
	Menu, GuiMenu, Add, &Comments, :%MENU_COMMENTS%
	Menu, GuiMenu, Add, &Examples, :%MENU_EXAMPLES%

}

LoadExampleMenus() {
	try {
		Loop, Files, %ASSETS_PATH%\examples\*, D
		{
			item := A_LoopFileName
			Loop, % A_LoopFileFullPath . "\*.dino"
				Menu, %item%, Add, %A_LoopFileName%, Examples
			Menu, %MENU_EXAMPLES%, Add, %A_LoopFileName%, % ":" item
		}
	}
}
