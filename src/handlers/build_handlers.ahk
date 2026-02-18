RUN:
	if !RC.FileClose(true)
		return
	gosub INIT_CONSOLE
	cmd := """" . DINOCODE_EXE . """ """ . SCRIPT_PATH . """"
	IDE.Console.Clear()
	IDE.Console.SendCommand(cmd, true)
return