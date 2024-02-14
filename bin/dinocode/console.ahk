; By @BlassGO
; Allows starting a console with custom sizing and positioning for debugging
print(str:="",newline:=true) {
   start_console()
   FileAppend, % str (newline?"`n":""), *
}
wait_console(){
	WinWaitClose, % "ahk_id " start_console()
}
SysGet(c,v:=""){
   SysGet, tmp, % c, % v
   return tmp
}
read_console(str:=""){
    start_console()
    stdin:=FileOpen("*", "r `n"), stdout:=FileOpen("*", "w `n")
    stdout.Write(str)
    stdout.Read(0) ; Flush the write buffer.
    input:=RTrim(stdin.ReadLine(), "`n")
    stdout.Read(0) ; Flush the write buffer.
    return input
}
cmd(str:=""){
    start_console()
    Loop, parse, str, `n,`r 
      Run, %ComSpec% /c %A_LoopField%
}
start_console(title:="",w:=false,h:=false,x:="",y:=""){
   static cid, last_title, last_w, last_h
   if !(cid&&WinExist("ahk_id " cid)) {
      (!title) ? title:="DinoConsole"
      (!w) ? w:=100
      (!h) ? h:=30
      DllCall("AllocConsole")
      DllCall("AttachConsole")
      cid := DllCall("GetConsoleWindow")
   }
   if (title&&title!=last_title) {
      Dllcall("SetConsoleTitle", "str",title)
      last_title:=title
   }
   if (w||h||x!=""||y!="") {
      (!w) ? w:=last_w
      (!h) ? h:=last_h
      (x="") ? x:=(SysGet(78) - w * 8) // 2
      (y="") ? y:=(SysGet(79) - h * 16) // 2
      width := w + 2, height := h + 2
      consoleBuffer := DllCall("GetStdHandle", "int", -12, "ptr")
      DllCall("SetConsoleScreenBufferSize", "ptr", consoleBuffer, "int", w | (h << 16))
      DllCall("MoveWindow", "ptr", cid, "int", x, "int", y, "int", width*8, "int", height*16, "int", 1)
      last_w:=w,last_h:=h
   }
   return cid
}