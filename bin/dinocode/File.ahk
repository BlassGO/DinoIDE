; By @BlassGO
; General library for file management

class File
{
    __New(file:="",encoding:="UTF-8-RAW",linebreak:="", EOLTranslation:=false)
    {
        (file!="")?this.path:=file
        (encoding!="")?this.encoding:=encoding
        this.linebreak:=linebreak, this.EOLTranslation:=EOLTranslation
    }
    EOLTranslation[] {
        get {
            return,this._eol
        }
        set {
            return,this._eol:=value?"":"*"
        }
    }
    encoding[] {
        get {
            return,this._encoding
        }
        set {
            value:=Trim(value)
            if (value~="^CP\d+|UTF-8|UTF-8-RAW|UTF-16|UTF-16-RAW$")
                this._encoding:=value
            else
                unexpected:="Invalid encoding -> " . value
            return,this._encoding
        }
    }
    linebreak[] {
        get {
            if (this._linebreak=""){
                try this._linebreak:=InStr(FileOpen(this.path, "r").ReadLine(),"`r`n")?"`r`n":"`n"
                catch e
                unexpected:="Cant read-->""" this.path """, Ended with: " e.message, this._linebreak:=""
            }
            return,this._linebreak
        }
        set {
            return,this._linebreak:=value
        }
    }
    path[] {
        get {
            return,this._path
        }
        set {
            cc:=DllCall("GetFullPathName","str",value,"uint",0,"ptr",0,"ptr",0,"uint"),VarSetCapacity(buf,cc*(A_IsUnicode?2:1)),DllCall("GetFullPathName","str",value,"uint",cc,"str",buf,"ptr",0,"uint")
            return,this._path:=(buf="")?value:buf
        }
    }
    content[] {
        get {
            FileEncoding,% this.encoding
            try FileRead,content,% this.path
            catch e
            unexpected:="Cant read-->""" this.path """, Ended with: " e.message
            return,content
        }
        set {
            this.Delete()
            FileEncoding,% this.encoding
            try FileAppend,%value%,% this._eol . this.path
            catch e
            unexpected:="Cant write in-->""" this.path """, Ended with: " e.message
        }
    }
    line(i:=1) {
        Loop,read,% this.path
        if (A_Index=i)
        return,A_LoopReadLine
    }
    lines(i:=1,j:=1) {
        static File,_CL:="`r`n"
        try{
        File:=FileOpen(this.path, "r"), count:=1, _temp:=""
        while,!File.AtEOF&&(count<i)
        File.ReadLine(),count+=1
        while,!File.AtEOF&&(count<=j)
        _temp.=File.ReadLine(),count+=1 
        }catch e
        unexpected:="Cant read-->""" this.path """, Ended with: " e.message
        File.Close(), File:=""
        return,RTrim(_temp,_CL)
    }
    LineStr(P, C:="", D:="") {
        return,LineStr(this.content,P,C,D)
    }
    create() {
        (!this.exist())?this.content:=""
        return,(ErrorLevel=0)
    }
    exist() {
        return,(FileExist(this.path)~="[^D]+") 
    }
    count() {
        static File
        try{
        File:=FileOpen(this.path, "r"),count:=0
        while,!File.AtEOF
        File.ReadLine(),count+=1
        }catch e
        unexpected:="Cant read-->""" this.path """, Ended with: " e.message
        File.Close(), File:=""
        return,count
    }
    delete() {
        try FileDelete,% this.path
        return,(ErrorLevel=0)
    }
    deleteany() {
        try FileDelete,% this.path
        catch
        try FileRemoveDir,% this.path,1
        return,(ErrorLevel=0)
    }
    write(str) {
        FileEncoding,% this.encoding
        try FileAppend,%str%,% this._eol . this.path
        catch e
        unexpected:="Cant write in: """ this.path """, Ended with: " e.message
        return,(ErrorLevel=0)
    }
    writeline(str:="") {
        FileEncoding,% this.encoding
        try FileAppend,% str . this.linebreak,% this._eol . this.path
        catch e
        unexpected:="Cant write in: """ this.path """, Ended with: " e.message
        return,(ErrorLevel=0)
    }
    writeafter(str,str2:="",limit:=1) {
        if (_info:=this.find(str,,"lines info")){
            _temp:=this.content, _info:=StrSplit(_info, "`n"), _POS:=SubStr(_info.1,5), _LEN:=SubStr(_info.2,5)
            this.content:=RTrim(SubStr(_temp,1,_POS+_LEN),"`r`n") . this.linebreak . str2 . this.linebreak . LTrim(SubStr(_temp,_POS+_LEN+1),"`r`n")
            return,1
        }
        return,0
    }
    writebefore(str,str2:="") {
        if (_info:=this.find(str,,"lines info")){
            _temp:=this.content, _info:=StrSplit(_info, "`n"), _POS:=SubStr(_info.1,5), _LEN:=SubStr(_info.2,5)
            this.content:=RTrim(SubStr(_temp,1,_POS-1),"`r`n") . ((_POS-1>0)?this.linebreak:"") . str2 . this.linebreak . LTrim(SubStr(_temp,_POS),"`r`n")
            return,1
        }
        return,0
    }
    remove(str,str2:="",limit:=0) {
        static _N:="`n", _RN:="`r`n"
        if (_info:=this.find(str,str2,"complete_extract lines info limit:" . limit)){
            _temp:=this.content, _ADJUST:=0
            Loop,Parse,_info,%_N%
            {
                if (Mod(A_Index,2)=0){
                    _POS:=(_POS-_ADJUST>0)?_POS-_ADJUST:1,_LEN:=SubStr(A_LoopField,5),_ENDED:=SubStr(_temp,_POS+_LEN+1,2)
                    if (_ENDED=_RN)
                    _LEN+=2
                    else if InStr(_ENDED,_N)
                    _LEN+=1
                    _temp:=SubStr(_temp,1,_POS-1) . SubStr(_temp,_POS+_LEN+1), _ADJUST+=_LEN+1
                }else
                _POS:=SubStr(A_LoopField,5)
            }
            this.content:=_temp
            return,1
        }
        return,0
    }
    writeraw(str:="",bytes:="",fill:=0) {
        FileEncoding,% this.encoding
        VarSetCapacity(Buffer,len:=(bytes!="")?bytes:StrLen(str), fill) 
        (str!="")?StrPut(str, &Buffer,StrReplace(this.encoding,"-RAW"))
        try return,FileOpen(this.path,"a").RawWrite(&Buffer, len)
        catch e
        unexpected:="Cant write in: """ this.path """, Ended with: " e.message
    }
    read(chars:=0,offset:=0,origin:=0) {
        static File
        FileEncoding,% this.encoding
        try{
        File:=FileOpen(this.path, "r")
        if File.Seek(offset,origin)
        _temp:=(chars=0)?File.read():File.read(chars) 
        }catch e
        unexpected:="Cant read-->""" this.path """, Ended with: " e.message
        File.Close(), File:=""
        return,_temp
    }
    readhex(bytes:=0,offset:=0,origin:=0) {
        static File,_:="UChar"
        FileEncoding,% this.encoding
        try{
        File:=FileOpen(this.path, "r"), (bytes=0)?bytes:=File.Length
        if File.Seek(offset,origin){
            if (readed:=File.RawRead(Buffer, bytes)){
                Loop,%readed%
                hex.=Format("{:02X}",NumGet(&Buffer,A_Index-1,_))
            }
        }
        }catch e
        unexpected:="Cant read-->""" this.path """, Ended with: " e.message
        File.Close(), File:=""
        return,hex
    }
    replaceHex(hexSearch, hexReplace:="", n:=0, chunkSize:=4096, offset:=0) {
        static File, _temp
        FileEncoding,% this.encoding
        try File:=FileOpen(this.path, "r")
        catch e{
            unexpected:="Cant read-->""" this.path """, Ended with: " e.message
            File.Close(), File:=""
            return
        }
        try _temp:=FileOpen(this.path . "~", "w")
        catch e{
            unexpected:="Cant write in: """ this.path "~"", Ended with: " e.message
            _temp.Close(), _temp:=""
            return
        }
        fileSize:=File.Length, len:=StrLen(hexSearch), len2:=StrLen(hexReplace), _return:=0
        ;(chunkSize=0) ? (chunkSize:=fileSize/1000, chunkSize:=Max(chunkSize, (StrLen(hexSearch)//2) * 2))
        File.Seek(0,0)
        bytelen3:=StrLen(hexReplace)//2
        this.Hex2Bin(bytereplace,hexReplace,bytelen3)
        if offset{
            while,offset>0{
                segmentSize:=Min(offset, chunkSize)
                if !(File.RawRead(Buffer, segmentSize)&&_temp.RawWrite(&Buffer, segmentSize))
                return 0
                offset-=segmentSize
            }
        }
        While,!File.AtEOF{
            hex:=""
            if (readed:=File.RawRead(Buffer, chunkSize)){
                if (n&&_return>=n){
                    if !(_temp.RawWrite(&Buffer, chunkSize))
                    break
                    continue
                }
                Loop, %readed%
                hex.=Format("{:02X}", NumGet(&Buffer, A_Index-1, "UChar"))
            } else
            break
            _last:=pos:=1, _modified:=false
            while,(n=0||_return<n)&&(pos:=InStr(hex, hexSearch, false, pos)){
                if (bytelen:=StrLen(before:=SubStr(hex,_last,pos-_last))//2){
                    this.Hex2Bin(bytebefore,before,bytelen)
                    if !(_temp.RawWrite(&bytebefore, bytelen)){
                        _return:=0
                        break 2
                    }
                }
                if (_temp.RawWrite(&bytereplace, bytelen3)!=bytelen3){
                    _return:=0
                    break 2
                }
                _last:=pos+=len, _modified:=true, _return+=1
            }
            if _modified{
                if (bytelen2:=StrLen(after:=SubStr(hex,_last))//2){
                    this.Hex2Bin(byteafter,after,bytelen2)
                    if !(_temp.RawWrite(&byteafter, bytelen2)){
                        _return:=0
                        break
                    }
                }
            }else if !_temp.RawWrite(&Buffer, chunkSize){
                _return:=0
                break
            }
        }
        File.Close(), _temp.Close()
        if _return
        FileMove, % this.path . "~", % this.path, 1
        else
        FileDelete, % this.path . "~"
        return,_return
    }
    grep(str,str2:="",complete_extract:=true) {
        return,this.find(str,str2,"lines " . ((complete_extract)?"complete_extract":"extract"))
    }
    grepAll(str,str2:="",complete_extract:=true) {
        return,this.find(str,str2,"all lines " . ((complete_extract)?"complete_extract":"extract"))
    }
    replace(str,str2:="",limit:=-1) {
        this.content:=StrReplace(this.content,str,str2,n,limit)
        return,n
    }
    find(str,str2:="",options:="") {
        static _R:="`r",_N:="`n"
        orig_len:=StrLen(str), orig_len2:=StrLen(str2), case_sense:=false
        Loop,Parse,options,%A_Space%,%A_Tab%
        {
            switch A_LoopField
            {
                case "all":
                    all:=true
                case "info":
                    info:=true
                case "lines":
                    as_lines:=true
                case "extract":
                    extract:=true, header:=false
                case "complete_extract":
                    extract:=true, header:=true
                case "case_sense":
                    case_sense:=true
                default:
                    if _opt:=InStr(A_LoopField,"pos:"){
                        _last:=SubStr(A_LoopField,_opt+4)
                        if _last is not integer
                        _last:=0
                    }else if _opt:=InStr(A_LoopField,"limit:"){
                        _limit:=SubStr(A_LoopField,_opt+6)
                        if _limit is integer
                        all:=true
                        else
                        _limit:=0
                    }
            }   
        }
        _last:=_last?_last:1
        FileEncoding, % this.encoding
        try FileRead, content, % this.path
        catch e{
        unexpected:="Cant read-->""" this.path """, Ended with: " e.message
        return
        }
        as_lines?tlen:=StrLen(content)
        while,_last>0{
            len:=orig_len, len2:=orig_len2
            if _at:=InStr(content,str,case_sense,_last){
                if as_lines
                (_startline:=InStr(content,_N,false,_at-tlen))?(len+=(_at-_startline)-1, _at:=_startline+1):(_at:=1, len:=0), (_endline:=InStr(content,_N,false, _at+len))?(len+=_endline-(_at+len)-(SubStr(content,_endline-1,1)=_R)):(len+=tlen-len)
                if str2=
                {
                    extract?(as_lines?false:((_endline:=InStr(content,_N,false, _at+len))?(len+=_endline-(_at+len)-(SubStr(content,_endline-1,1)=_R)):(len+=(tlen?tlen:(tlen:=StrLen(content)))-len)), _last:=_at+len):(_last:=0)
                    if info{
                        if all
                        result.=((result="")?"":_N) . "POS=" . _at . "`nLEN=" . len
                        else
                        return,"POS=" . _at . "`nLEN=" . len
                        
                    }else if all
                    result.=((result="")?"":_N) . (extract?SubStr(content,_at,len):_at)
                    else
                    return,extract?SubStr(content,_at,len):_at
                }else{
                    if _at2:=InStr(content,str2,case_sense,_at+len){
                        if as_lines
                        (_startline:=InStr(content,_N,false,_at2-tlen))?(len2+=(_at2-_startline)-1, _at2:=_startline+1):(_at2:=1, len2:=0), (_endline:=InStr(content,_N,false, _at2+len2))?(len2+=_endline-(_at2+len2)-(SubStr(content,_endline-1,1)=_R)):(len2+=tlen-len2)
                        _last:=extract?_at2+len2:0
                        if info{
                            if all
                            result.=((result="")?"":_N) . (header?("POS=" . _at . "`nLEN=" . (_at2-_at)+len2):("POS=" . _at+len . "`nLEN=" . (_at2-_at)-len))
                            else
                            return,header?("POS=" . _at . "`nLEN=" . (_at2-_at)+len2):("POS=" . _at+len . "`nLEN=" . (_at2-_at)-len)
                        }else if all
                        result.=((result="")?"":_N) . (extract?(header?SubStr(content,_at,(_at2-_at)+len2):SubStr(content,_at+len,(_at2-_at)-len)):_at)
                        else
                        return,extract?(header?SubStr(content,_at,(_at2-_at)+len2):SubStr(content,_at+len,(_at2-_at)-len)):_at
                    }else
                    _last:=0
                }
                if _limit&&(A_Index=_limit)
                break
            }else
            _last:=0
        }
        return,result
    }
    isReadable(NumBytes:=32,Minimum:=4,complexunicode:=1,staticfile:="") {
        static file,_:="UChar",$null:="",$r:=Chr(114)
        FileEncoding, % this.encoding
        try file:=FileOpen((staticfile=$null)?this.path:staticfile,$r),file.Position:=0,nbytes:=file.RawRead(rawbytes,NumBytes),file.Close(),file:=$null ;read bytes
        catch
        return,0
        
        if (nbytes<Minimum) ;recommended 4 minimum for unicode detection
        return,1 ;asume text file, if too short
        
        t:=0,i:=0,bytes:=[] ;Initialize vars
        
        loop % nbytes ;create c-style bytes array
        bytes[A_Index-1]:=Numget(&rawbytes,A_Index-1,_)
        
        ;determine BOM if possible/existant
        if (bytes.0=0xFE && bytes.1=0xFF)
            || (bytes.0=0xFF && bytes.1=0xFE)
            return,1 ;text Utf-16 BE/LE file
        if (bytes.0=0xEF && bytes.1=0xBB && bytes.2=0xBF)
            return,1 ;text Utf-8 file
        if (bytes.0=0x00 && bytes.1=0x00
            && bytes.2=0xFE && bytes.3=0xFF)
            || (bytes.0=0xFF && bytes.1=0xFE
            && bytes.2=0x00 && bytes.3=0x00)
            return,1 ;text Utf-32 BE/LE file
            
        while(i<nbytes){	
            ;// ASCII
            if( bytes[i] == 0x09 || bytes[i] == 0x0A || bytes[i] == 0x0D
                || (0x20 <= bytes[i] && bytes[i] <= 0x7E) ) {
                i+=1
                continue
            }
            ;// non-overlong 2-byte
            if( (0xC2 <= bytes[i] && bytes[i] <= 0xDF)
                && (0x80 <= bytes[i+1] && bytes[i+1] <= 0xBF) ) {
                i+=2
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
                i+=3
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
                i+=4
                continue
            }
            t:=1
            break
        }
        ;the while-loop has no fails, then confirmed utf-8
        if t=0
        return,1
        ;else do nothing and check again with the classic method below
        
        loop,%nbytes%{
            if (bytes[A_Index-1]<9) || (bytes[A_Index-1]>126)
                || ((bytes[A_Index-1]<32) && (bytes[A_Index-1]>13))
                return,0
        }
        
        return,1
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