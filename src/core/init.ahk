; DinoIDE Initialization

InitializeGUI() {
	global IDE, RC, GuiFont, GuiMenu, textz, style
	
	; Set up font
	GuiFont := GuiDefaultFont()
	textz := GuiFont.Size
	style := GuiFont.Name
	
	; Create main GUI
	Gui, %WINDOW_STYLE%
	Gui, Menu, GuiMenu
	
	; Initialize IDE
	IDE := new DinoIDE(Settings)
	RC := IDE.RichCode
}

SetupGUI() {
	global RC, IDE
	
	; Add extra menus
	RC.Add_Recent()
	Menu, GuiMenu, Add, &File, :%MENU_FILE%
	Menu, GuiMenu, Add, &Edit, % ":" RC.MenuName
	Menu, GuiMenu, Add, &Build, :%MENU_BUILD%
	Menu, GuiMenu, Add, &Search, :%MENU_SEARCH%
	Menu, GuiMenu, Add, &View, :%MENU_VIEW%
	Menu, GuiMenu, Add, &Comments, :%MENU_COMMENTS%
	Menu, GuiMenu, Add, &Examples, :%MENU_EXAMPLES%	
	; Set focus and maximize
	GuiControl, Focus, % RC.hWnd
	GoSub, ChangeLang
	WinMaximize, A
}
