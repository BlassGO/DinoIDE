INIT_CONSOLE:
	if (!IDE.Console.IsActive()) {
		IDE.Console.Initialize()
	}
return

RUN_WITH_TERMINAL:
	IDE.Console.SwitchConsole("wt")
	gosub RUN
return

RUN_WITH_CMD:
	IDE.Console.SwitchConsole("cmd")
	gosub RUN
return

CLEAR_CONSOLE:
	IDE.Console.Clear()
return

KILL_CONSOLE:
	IDE.Console.Cleanup()
return
