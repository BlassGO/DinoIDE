
class Console {
    __New(parentIDE) {
        this.IDE := parentIDE
        this.hConsoleWindow := 0
        this.ConsoleProcess := 0
        this.GetConsole()
    }
    
    SetConsoleType(type) {
        local wtpath :=  "C:\Users\" . A_UserName . "\AppData\Local\Microsoft\WindowsApps\wt.exe"
        if (this.IsActive()) {
            this.Cleanup()
        }
        
        if (type == "wt") {
            if (FileExist(wtpath)) {
                this.ConsoleType := "wt"
                this.ConsolePath := """" wtpath . """ nt -p ""Windows PowerShell"""
                return true
            }
        }
        
        this.ConsoleType := "cmd"
        this.ConsolePath := A_ComSpec
        return true
    }
    
    GetConsole() {
        preferred := ConsoleConfig.PreferredConsole
        this.SetConsoleType(preferred)
        return this.ConsolePath
    }

    SwitchConsole(type) {
        currentType := this.ConsoleType
        if (currentType == type && this.IsActive()) {
            return true
        }
        
        if (this.IsActive()) {
            this.Cleanup()
        }
        
        this.SetConsoleType(type)
    }
    
    Initialize() {
        if (!this.ConsolePath) {
            return false
        }
        
        try {
            DetectHiddenWindows, On
            
            Run, % this.ConsolePath,, Hide, PID
            
            if (ErrorLevel) {
                return false
            }
            
            if (this.ConsoleType == "wt") {
                Sleep 1000
                this.hConsoleWindow := WinExist("ahk_class CASCADIA_HOSTING_WINDOW_CLASS")

                WinGet, TerminalPID, PID, % "ahk_id " this.hConsoleWindow
                this.ConsoleProcess := TerminalPID
            } else {
                WinWait, ahk_pid %PID%, , 3
                this.hConsoleWindow := WinExist("ahk_pid " . PID)
                this.ConsoleProcess := PID
            }
            
            DetectHiddenWindows, Off
            
            if (!this.hConsoleWindow) {
                Process, Close, %PID%
                return false
            }        
        
            if (this.EmbedInGUI()) {
                this.Clear()
                return true
            } else {
                Process, Close, %PID%
                return false
            }
            
        } catch e {
            return false
        }
    }
    
    EmbedInGUI() {
        if (!this.hConsoleWindow || !this.IDE.hMainWindow) {
            return false
        }
        
        try {
            loop 5
            {
                parentResult := DllCall("SetParent", "Ptr", this.hConsoleWindow, "Ptr", this.IDE.hMainWindow)
                if (parentResult != 0) {
                    break
                }
                Sleep 100
            }
            
            this.IDE.UpdateConsolePosition()
            
            WinSet, Style, % -(WS_POPUP|WS_CAPTION|WS_THICKFRAME)
            
            DllCall("ShowWindow", "Ptr", this.hConsoleWindow, "Int", 5)  ; SW_SHOW

            return true
            
        } catch e {
            return false
        }
    }
    
    SendCommand(command, isFullPath := false) {
        if (!this.hConsoleWindow) {
            return false
        }
        
        try {
            
            if (this.ConsoleType == "wt") {
                A_Clipboard := (isFullPath ? "& " : "") . command
                ControlSend, Windows.UI.Input.InputSite.WindowClass1, +{Insert}, % "ahk_id " this.hConsoleWindow
                Sleep 200
                ControlSend, Windows.UI.Input.InputSite.WindowClass1, {Enter}, % "ahk_id " this.hConsoleWindow
                ControlFocus, Windows.UI.Input.InputSite.WindowClass1, % "ahk_id " this.hConsoleWindow
            } else {
                ControlSend,, {Text}%command%, % "ahk_id " this.hConsoleWindow
                ControlSend,, {Enter}, % "ahk_id " this.hConsoleWindow
                ControlFocus,, % "ahk_id " this.hConsoleWindow
            }

            Sleep 200

            return true
        } catch e {
            return false
        }
    }
    
    Clear() {
        return this.SendCommand("cls")
    }
    
    IsActive() {
        if (!this.ConsoleProcess) {
            return false
        }
        
        Process, Exist, % this.ConsoleProcess
        return (ErrorLevel = this.ConsoleProcess)
    }
    
    Cleanup() {
        if (this.ConsoleProcess) {
            DllCall("SetParent", "Ptr", this.hConsoleWindow, "Ptr", 0)
            
            Process, Close, % this.ConsoleProcess
            this.ConsoleProcess := 0
        }
        
        this.hConsoleWindow := 0
    }
}
