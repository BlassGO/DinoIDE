; By @BlassGO
; Allows starting a console with custom sizing and positioning for debugging
print(Byref str:="", newline:=true, signal:="") {
    static status:=true, cid
    if (signal!="")
        status := signal
    if !status
        return
    if !cid
      cid:=startconsole()
    FileAppend, % str (newline ? "`n" : ""), *
}

enable_print() {
    print(,,true)
}

disable_print() {
    print(,,false)
}

WaitConsole() {
    WinWaitClose, % "ahk_id " startconsole()
}

SysGet(c, v:="") {
    SysGet, tmp, % c, % v
    return tmp
}

ReadConsole(Byref str:="") {
    static cid
    if !cid
      cid:=startconsole()
    stdin := FileOpen("*", "r `n"), stdout := FileOpen("*", "w `n")
    stdout.Write(str)
    stdout.Read(0) ; Flush the write buffer.
    input := RTrim(stdin.ReadLine(), "`n")
    stdout.Read(0) ; Flush the write buffer.
    return input
}

cmd(Byref opt*) {
    static cid
    if !cid
      cid:=startconsole()
    params:=""
    for count, value in opt
        params.=A_Space . ((value~="^\S+$") ? value:"""" . value . """")
    RunWait, % ComSpec . " /c """ . params . """",,UseErrorLevel
    return ErrorLevel
}

ExplorerSelect(path) {
    RunWait, %ComSpec% /c explorer.exe /select`,"%path%",,Hide UseErrorLevel
    return ErrorLevel
}

StartConsole(title:="", w:=false, h:=false, x:="", y:="") {
    static cid, last_title, last_w, last_h
    if !cid {
        title:=(title="")?"DinoConsole":title, w:=w?w:100, h:=h?h:30
        if !DllCall("AttachConsole", "int", -1)
            DllCall("AllocConsole")
        cid:=DllCall("GetConsoleWindow")
    }
    if (title!=""&&title!=last_title) {
        DllCall("SetConsoleTitle", "str", title)
        last_title:=title
    }
    if (w||h||x!=""||y!="") {
        w:=w?w:last_w, h:=h?h:last_h, x:=(x="")?(SysGet(78) - w * 8) // 2 : x, y:=(y="")?(SysGet(79) - h * 16) // 2 : y
        DllCall("SetConsoleScreenBufferSize", "ptr", DllCall("GetStdHandle", "int", -12), "int", w | (h << 16))
        DllCall("MoveWindow", "ptr", cid, "int", x, "int", y, "int", (w+2) * 8, "int", (h+2) * 16, "int", 1)
        last_w:=w, last_h:=h
    }
    return cid
}
