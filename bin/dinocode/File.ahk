; By @BlassGO
; General library for file management

class File
{
    __New(file:="")
    {
        (file!="") ? this.path:=file
        this.encoding:="UTF-8-RAW"
        this.linebreak:="`n"
    }
    path[] {
        get {
            return this._path
        }
        set {
            cc := DllCall("GetFullPathName", "str", value, "uint", 0, "ptr", 0, "ptr", 0, "uint")
            VarSetCapacity(buf, cc*(A_IsUnicode?2:1))
            DllCall("GetFullPathName", "str", value, "uint", cc, "str", buf, "ptr", 0, "uint")
            this._path:=(buf="")?value:buf
            return this._path
        }
    }
    content[] {
        get {
            FileEncoding, % this.encoding
            FileRead, content, % this.path
            return content
        }
        set {
            this.Delete()
            FileEncoding, % this.encoding
            FileAppend, % value, % this.path
        }
    }
    line[i:=1] {
        get {
            Loop, read, % this.path
                if (A_Index=i)
                    return A_LoopReadLine
        }
    }
    lines[i:=1,j:=1] {
        get {
            _temp:=""
            Loop, read, % this.path
            {
                if (A_Index>=i&&A_Index<=j)
                    (_temp)?(_temp.=this.linebreak):false,_temp.=A_LoopReadLine
                else if (A_Index>j)
                    break
            }
            return _temp
        }
    }
    create() {
        (!this.exist()) ? this.content:=""
        return (ErrorLevel=0)
    }
    exist() {
        return (FileExist(this.path)~="[^D]") 
    }
    count() {
       _total:=0
       if (_temp:=this.content) {
            _total+=1, _pos:=0
            while,(_pos:=InStr(_temp,"`n",,_pos+1))
                _total++
       }
        return _total
    }
    delete() {
        try FileDelete, % this.path
        return (ErrorLevel=0)
    }
    deleteany() {
        try FileDelete, % this.path
        catch
           try FileRemoveDir, % this.path, 1
        return (ErrorLevel=0)
    }
    write(str) {
        FileEncoding, % this.encoding
        FileAppend, % str, % this.path
    }
    writeline(str:="") {
        FileEncoding, % this.encoding
        FileAppend, % str . this.linebreak, % this.path
    }
    writeafter(str,str2:="") {
        if (_info:=this.find(str,,"lines info")) {
            _temp:=this.content, _info:=StrSplit(_info, "`n"), _POS:=SubStr(_info[1],5), _LEN:=SubStr(_info[2],5)
            this.content:=RTrim(SubStr(_temp,1,_POS+_LEN),"`r`n") . this.linebreak . str2 . this.linebreak . LTrim(SubStr(_temp,_POS+_LEN+1),"`r`n")
        }
    }
    writebefore(str,str2:="") {
        if (_info:=this.find(str,,"lines info")) {
            _temp:=this.content, _info:=StrSplit(_info, "`n"), _POS:=SubStr(_info[1],5), _LEN:=SubStr(_info[2],5)
            this.content:=RTrim(SubStr(_temp,1,_POS-1),"`r`n") . ((_POS-1>0)?this.linebreak:"") . str2 . this.linebreak . LTrim(SubStr(_temp,_POS),"`r`n")
        }
    }
    writeraw(str:="",bytes:="",fill:=0) {
        FileEncoding, % this.encoding
        VarSetCapacity(Buffer,len:=(bytes!="")?bytes:StrLen(str), fill) 
        (str!="")?StrPut(str, &Buffer,StrReplace(this.encoding,"-RAW"))
        return FileOpen(this.path,"a").RawWrite(&Buffer, len)
    }
    read(chars:=0,offset:=0,origin:=0) {
        static File
        FileEncoding, % this.encoding
        File:=FileOpen(this.path, "r")
        if (File.Seek(offset,origin)) {
            _temp:=(chars=0)?File.read():File.read(chars) 
        }
        File.Close(), File:=""
        return _temp
    }
    readhex(bytes:=0,offset:=0,origin:=0) {
        static File
        FileEncoding, % this.encoding
        File:=FileOpen(this.path, "r"), (bytes=0) ? bytes:=File.Length
        if (File.Seek(offset,origin)) {
            if (readed:=File.RawRead(Buffer, bytes)) {
                Loop, % readed
                    hex.=Format("{:02X}", NumGet(&Buffer, A_Index-1, "UChar"))
            }
        }
        File.Close(), File:=""
        return hex
    }
    replaceHex(hexSearch, hexReplace:="", n:=0, chunkSize:=4096, offset:=0) {
        static File, _temp
        FileEncoding, % this.encoding
        File:=FileOpen(this.path, "r"), _temp:=FileOpen(this.path . "~", "w"), fileSize:=File.Length, len:=StrLen(hexSearch), len2:=StrLen(hexReplace), _return:=0
        ;(chunkSize=0) ? (chunkSize:=fileSize/1000, chunkSize:=Max(chunkSize, (StrLen(hexSearch)//2) * 2))
        File.Seek(0,0)
        bytelen3:=StrLen(hexReplace)//2
        this.Hex2Bin(bytereplace,hexReplace,bytelen3)
        if offset && !(File.RawRead(Buffer, offset)&&_temp.RawWrite(&Buffer, offset)) {
            return 0
        }
        While,!File.AtEOF {
            hex:=""
            if (readed:=File.RawRead(Buffer, chunkSize)) {
                if (n&&_return>=n) {
                    if (!_temp.RawWrite(&Buffer, chunkSize))
                       break
                    continue
                }
                Loop, % readed
                    hex.=Format("{:02X}", NumGet(&Buffer, A_Index-1, "UChar"))
            } else
                break
            _last:=pos:=1, _modified:=false
            while,(n=0||_return<n)&&(pos:=InStr(hex, hexSearch, false, pos)) {
                if (bytelen:=StrLen(before:=SubStr(hex,_last,pos-_last))//2) {
                    this.Hex2Bin(bytebefore,before,bytelen)
                    if !(_temp.RawWrite(&bytebefore, bytelen)) {
                        _return:=0
                        break 2
                    }
                }
                if (_temp.RawWrite(&bytereplace, bytelen3)!=bytelen3) {
                    _return:=0
                    break 2
                }
                _last:=pos+=len, _modified:=true, _return+=1
            }
            if _modified {
                if (bytelen2:=StrLen(after:=SubStr(hex,_last))//2) {
                    this.Hex2Bin(byteafter,after,bytelen2)
                    if !(_temp.RawWrite(&byteafter, bytelen2)) {
                        _return:=0
                        break
                    }
                }
            } else if !_temp.RawWrite(&Buffer, chunkSize) {
                _return:=0
                break
            }
        }
        File.Close(), _temp.Close()
        if _return {
            FileMove, % this.path . "~", % this.path, 1
        } else {
            FileDelete, % this.path . "~"
        }
        return _return
    }
    grep(str,str2:="",complete_extract:=true) {
        return this.find(str,str2,"lines " . ((complete_extract)?"complete_extract":"extract"))
    }
    grepAll(str,str2:="",complete_extract:=true) {
        return this.find(str,str2,"all lines " . ((complete_extract)?"complete_extract":"extract"))
    }
    replace(str,str2:="",limit:=-1) {
        this.content:=StrReplace(this.content, str, str2, n, limit)
        return n
    }
    find(str,str2:="",options:="") {
        orig_len:=StrLen(str), orig_len2:=StrLen(str2)
        if (options ~= "i)\ball\b")
            all := true
        if (options ~= "i)\binfo\b")
            info := true
        if (options ~= "i)\blines\b")
            as_lines := true
        if (options ~= "i)\bextract\b")
            extract := true, header:=false
        if (options ~= "i)\bcomplete_extract\b")
            extract := true, header:=true
        _last:=1
        FileEncoding, % this.encoding
        FileRead, content, % this.path
        as_lines ? tlen:=StrLen(content)
        while (_last>0) {
            len:=orig_len, len2:=orig_len2
            if (_at:=InStr(content,str,,_last)) {
                if as_lines {
                    (_startline:=InStr(content,"`n",,_at-tlen)) ? (len+=(_at-_startline)-1, _at:=_startline+1) : (_at:=1, len:=0)
                    (_endline:=InStr(content, "`n",, _at+len)) ? (len+=_endline-(_at+len)) : (len+=tlen-len)
                }
                if str2 {
                    if (_at2:=InStr(content,str2,,_at+len)) {
                        if as_lines {
                            (_startline:=InStr(content,"`n",,_at2-tlen)) ? (len2+=(_at2-_startline)-1, _at2:=_startline+1) : (_at2:=1, len2:=0)
                            (_endline:=InStr(content, "`n",, _at2+len2)) ? (len2+=_endline-(_at2+len2)) : (len2+=tlen-len2)
                        }
                        (extract) ? _last:=_at2+len2 : _last:=0
                        if info {
                            if all
                                result ? result.="`n" : false, result.= header ? ("POS=" . _at . "`nLEN=" . (_at2-_at)+len2) : ("POS=" . _at+len . "`nLEN=" . (_at2-_at)-len)
                            else
                                return header ? ("POS=" . _at . "`nLEN=" . (_at2-_at)+len2) : ("POS=" . _at+len . "`nLEN=" . (_at2-_at)-len)
                        } else if all {
                            result ? result.="`n" : false, result.=(extract) ? (header ? SubStr(content,_at,(_at2-_at)+len2) : SubStr(content,_at+len,(_at2-_at)-len)) : _at
                        } else {
                            return (extract) ? (header ? SubStr(content,_at,(_at2-_at)+len2) : SubStr(content,_at+len,(_at2-_at)-len)) : _at
                        }
                    } else {
                        _last:=0
                    }
                } else {
                    if extract {
                        (!as_lines) ? ((_endline:=InStr(content, "`n",, _at+len)) ? (len+=_endline-(_at+len)) : (len+=(tlen?tlen:(tlen:=StrLen(content)))-len)) : false
                        _last:=_at+len
                    } else {
                        _last:=0
                    }
                    if info {
                        if all
                            result ? result.="`n" : false, result.="POS=" . _at . "`nLEN=" . len
                        else
                            return "POS=" . _at . "`nLEN=" . len
                    } else if all {
                        result ? result.="`n" : false, result.=(extract) ? SubStr(content,_at,len) : _at
                    } else {
                        return (extract) ? SubStr(content,_at,len) : _at
                    }
                }
            } else {
                _last:=0
            }
        }
        return result
    }
    isBinFile(NumBytes:=32,Minimum:=4,complexunicode:=1) {
        static file
        FileEncoding, % this.encoding
        file:=FileOpen(this.path,"r")
        file.Position:=0 ;force position to 0 (zero)
        nbytes:=file.RawRead(rawbytes,NumBytes) ;read bytes
        file.Close(), file:="" ;close file
        
        if (nbytes < Minimum) ;recommended 4 minimum for unicode detection
            return 0 ;asume text file, if too short
        
        t:=0, i:=0, bytes:=[] ;Initialize vars
        
        loop % nbytes ;create c-style bytes array
            bytes[(A_Index-1)]:=Numget(&rawbytes,(A_Index-1),"UChar")
        
        ;determine BOM if possible/existant
        if (bytes[0]=0xFE && bytes[1]=0xFF)
            || (bytes[0]=0xFF && bytes[1]=0xFE)
            return 0 ;text Utf-16 BE/LE file
        if (bytes[0]=0xEF && bytes[1]=0xBB && bytes[2]=0xBF)
            return 0 ;text Utf-8 file
        if (bytes[0]=0x00 && bytes[1]=0x00
            && bytes[2]=0xFE && bytes[3]=0xFF)
            || (bytes[0]=0xFF && bytes[1]=0xFE
            && bytes[2]=0x00 && bytes[3]=0x00)
            return 0 ;text Utf-32 BE/LE file
            
        while(i<nbytes) {	
            ;// ASCII
            if( bytes[i] == 0x09 || bytes[i] == 0x0A || bytes[i] == 0x0D
                || (0x20 <= bytes[i] && bytes[i] <= 0x7E) ) {
                i += 1
                continue
            }
            ;// non-overlong 2-byte
            if( (0xC2 <= bytes[i] && bytes[i] <= 0xDF)
                && (0x80 <= bytes[i+1] && bytes[i+1] <= 0xBF) ) {
                i += 2
                continue
            }
            ;// excluding overlongs, straight 3-byte, excluding surrogates
            if( ( bytes[i] == 0xE0 && (0xA0 <= bytes[i+1] && bytes[i+1] <= 0xBF)
                && (0x80 <= bytes[i+2] && bytes[i+2] <= 0xBF) )
                || ( ((0xE1 <= bytes[i] && bytes[i] <= 0xEC)
                || bytes[i] == 0xEE || bytes[i] == 0xEF)
                && (0x80 <= bytes[i+1] && bytes[i+1] <= 0xBF)
                && (0x80 <= bytes[i+2] && bytes[i+2] <= 0xBF) 	)
                || ( bytes[i] == 0xED && (0x80 <= bytes[i+1] && bytes[i+1] <= 0x9F)
                && (0x80 <= bytes[i+2] && bytes[i+2] <= 0xBF) ) ) {
                i += 3
                continue
            }
            ;// planes 1-3, planes 4-15, plane 16
            if( ( bytes[i] == 0xF0 && (0x90 <= bytes[i+1] && bytes[i+1] <= 0xBF)
                && (0x80 <= bytes[i+2] && bytes[i+2] <= 0xBF)
                && (0x80 <= bytes[i+3] && bytes[i+3] <= 0xBF) )
                || ( (0xF1 <= bytes[i] && bytes[i] <= 0xF3)
                && (0x80 <= bytes[i+1] && bytes[i+1] <= 0xBF)
                && (0x80 <= bytes[i+2] && bytes[i+2] <= 0xBF)
                && (0x80 <= bytes[i+3] && bytes[i+3] <= 0xBF) )
                || ( bytes[i] == 0xF4 && (0x80 <= bytes[i+1] && bytes[i+1] <= 0x8F)
                && (0x80 <= bytes[i+2] && bytes[i+2] <= 0xBF)
                && (0x80 <= bytes[i+3] && bytes[i+3] <= 0xBF) ) ) {
                i += 4
                continue
            }
            t:=1
            break
        }
        
        if (t=0) ;the while-loop has no fails, then confirmed utf-8
            return 0
        ;else do nothing and check again with the classic method below
        
        loop, %nbytes% {
            if (bytes[(A_Index-1)]<9) or (bytes[(A_Index-1)]>126)
                or ((bytes[(A_Index-1)]<32) and (bytes[(A_Index-1)]>13))
                return 1
        }
        
        return 0
    }
    Hex2Bin(ByRef @bin, _hex, _byteNb=0) ; https://github.com/camerb/AHKs/blob/master/thirdParty/BinaryEncodingDecoding.ahk
    {
        local dataSize, granted, dataAddress, x

        ; Get size of data
        x := StrLen(_hex)
        dataSize := x // 2
        if (x = 0 or dataSize * 2 != x)
        {
            ; Invalid string, empty or odd number of digits
            ErrorLevel = Param
            Return -1
        }
        If (_byteNb < 1 or _byteNb > dataSize)
        {
            _byteNb := dataSize
        }
        ; Make enough room
        granted := VarSetCapacity(@bin, _byteNb, 0)
        if (granted < _byteNb)
        {
            ; Cannot allocate enough memory
            ErrorLevel = Mem=%granted%
            Return -1
        }
        dataAddress := &@bin

        Loop Parse, _hex
        {
            if (A_Index & 1 = 1)	; Odd
            {
                x := A_LoopField	; Odd digit
            }
            Else
            {
                ; Concatenate previous x and even digit, converted to hex
                x := "0x" . x . A_LoopField
                ; Store integer in memory
                DllCall("RtlFillMemory"
                        , "UInt", dataAddress
                        , "UInt", 1
                        , "UChar", x)
                dataAddress++
            }
        }

        Return _byteNb
    }
}