; By @BlassGO
; General library for directory management

class Directory
{
    __New(dir:="")
    {
        (dir!="") ? this.path:=dir
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
    content(mode:="FDR") {
        _temp:=""
        Loop, Files, % this.path . "\*", % mode
            (_temp)?_temp.="`n":false,_temp.=A_LoopFileFullPath
        return _temp
    }
    create() {
        FileCreateDir, % this.path
        return (ErrorLevel=0)
    }
    createsub(name) {
        FileCreateDir, % this.path . "\" . name
        return (ErrorLevel=0)
    }
    exist() {
        return InStr(FileExist(this.path), "D")
    }
    items(mode:="FD") {
        _temp:=[]
        Loop, Files, % this.path . "\*", % mode
            _temp.Push({name: A_LoopFileName, dir:A_LoopFileDir, ext:A_LoopFileExt, path: A_LoopFileFullPath, isFolder:InStr(A_LoopFileAttrib, "D"), attrib:A_LoopFileAttrib, size:A_LoopFileSize, sizeKB:A_LoopFileSizeKB, sizeMB:A_LoopFileSizeMB, timemodified: A_LoopFileTimeModified, timecreated: A_LoopFileTimeCreated, timeaccessed: A_LoopFileTimeAccessed})
        return _temp
    }
    count(mode:="FD") {
       _total:=0
       Loop, Files, % this.path . "\*", % mode
            _total++
       return _total
    }
    delete() {
        try FileRemoveDir, % this.path, 1
        return (ErrorLevel=0)
    }
    deleteany() {
        try FileRemoveDir, % this.path, 1
        catch
           try FileDelete, % this.path
        return (ErrorLevel=0)
    }
    find(name,mode:="FDR",limit:="",partial:=false) {
        _temp:="", _count:=0
        Loop, Files, % this.path . "\*", % mode
        {
            if (pass:=(partial)?((InStr(A_LoopFileName,name)?A_LoopFileFullPath:"")):((A_LoopFileName=name)?A_LoopFileFullPath:"")) {
                _temp.=((_temp="")?"":"`n") . pass
                _count++
                if (limit&&_count>=limit)
                    break
            }
        }
        return _temp
    }
    copyhere(path,overwrite:=1) {
        if InStr(FileExist(path), "D") {
            SplitPath, path, name
            FileCopyDir, % path, % this.path . "\" name, % overwrite
        } else {
            FileCopy, % path, % this.path, % overwrite
        }
        return (ErrorLevel=0)
    }
    movehere(path,overwrite:=2) {
        if InStr(FileExist(path), "D") {
            SplitPath, path, name
            FileMoveDir, % path, % this.path . "\" name, % overwrite
        } else {
            FileMove, % path, % this.path, % overwrite?1:0
        }
        return (ErrorLevel=0)
    }
    findstr(str,str2:="",mode:="FR",limit:="") {
        _temp:="",_count:=0,fileobj:=New File()
        if IsObject(file) {
            Loop, Files, % this.path . "\*", % mode
            {
                fileobj.path:=A_LoopFileLongPath
                if fileobj.isBinFile()
                continue
                if (fileobj.Grep(str,str2)!="") {
                    _temp.=((_temp="")?"":"`n") . A_LoopFileLongPath
                    _count++
                    if (limit&&_count>=limit)
                        break
                }
            }
        }
        return _temp
    }
}