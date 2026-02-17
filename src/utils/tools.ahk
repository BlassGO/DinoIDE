GuiDefaultFont() {
   VarSetCapacity(LF, szLF := 28 + (A_IsUnicode ? 64 : 32), 0) ; LOGFONT structure
   If DllCall("GetObject", "Ptr", DllCall("GetStockObject", "Int", 17, "Ptr"), "Int", szLF, "Ptr", &LF)
      Return {Name: StrGet(&LF + 28, 32), Size: Round(Abs(NumGet(LF, 0, "Int")) * (72 / A_ScreenDPI), 1)
            , Weight: NumGet(LF, 16, "Int"), Quality: NumGet(LF, 26, "UChar")}
   return False
}
GetFullPathName(path) {
   cc := DllCall("GetFullPathName", "str", path, "uint", 0, "ptr", 0, "ptr", 0, "uint")
   VarSetCapacity(buf, cc*(A_IsUnicode?2:1))
   DllCall("GetFullPathName", "str", path, "uint", cc, "str", buf, "ptr", 0, "uint")
   return buf
}

Arg_in(value) {
    global SCRIPT_PATH
    SCRIPT_PATH := value
}