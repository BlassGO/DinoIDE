FileAppend:
FileOpen:
FileInsert:
	RC.FileOpen(SubStr(A_ThisLabel, 5))
Return

FileOpenDir:
	if SCRIPT_PATH
		Run, explorer.exe /select`,"%SCRIPT_PATH%"
return

FileClose:
	RC.FileClose()
Return

FileSave:
	RC.FileSave()
Return

FileNew:
	RC.FileSaveAs(true)
return

FileSaveAs:
	RC.FileSaveAs()
Return

; Recent files handler
Open_Recent:
	If FileExist(A_ThismenuItem) {
		if !RC.FileClose()
			return
		RC.LoadFile(A_ThismenuItem), RC.PosFromFile(A_ThismenuItem)
	}
Return

; Example files handler
Examples:
	If FileExist(File := ASSETS_PATH "\examples\" . A_ThisMenu . "\" . A_ThismenuItem) {
		if !RC.FileClose()
			return
		RC.LoadFile(File), RC.PosFromFile(File)
	}
return
