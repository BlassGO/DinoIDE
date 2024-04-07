/*
AutoIMG Lib for DinoCode
   This library provides additional functions for managing Android devices. It also includes patching functions for APKs/JARs
   It is based on the AutoIMG tool, but only provides specific functions for processing. Nothing graphic
   
   Main dependencies:
      - adb.exe
      - fastboot.exe
      - apktool.jar
   
   Base information:
      version = 1.5.0
      status = Beta
      build_date = 19.01.2024

   Usage:
      1. For correct operation, it is necessary to define the path of the dependencies.
         AutoIMG_Set "VAR = DATA" "VAR = DATA" "..."

         AutoIMG_Set "adb = tools\adb.exe" "fastboot = tools\fastboot.exe" "apktool = tools\apktool.jar" "zipalign = tools\zipalign.exe"
      2. Optionally you can change the destination file for general logs
         AutoIMG_Set "general_log = MyLog.log"

*/

AutoIMG_Set(params*) {
   global
   static set
   local count, value, _pos, _key, maps2, maps3
   for count, value in params
   {
      if (_pos:=InStr(value,"=")) {
         ((_key:=Trim(SubStr(value,1,_pos-1)))&&_key~="^current|fastboot|adb|apktool|sign|zipalign|7za|busybox|general_log$") ? %_key%:=Trim(SubStr(value,_pos+1))
      }
   }
   (!general_log) ? general_log:=A_WorkingDir . "\AutoIMG.log"
   if !set {
      CONFIG:=A_ScriptFullPath,TOOL:=HERE:=A_ScriptDir,wireless_IP:=wireless_PORT:=hidden_devices:=wireless_FORCE:=PATH:=TMP:=""
      shared_vars .= "|(?:^(?:general_log|exitcode|CONFIG|HERE|TOOL|TMP|PATH|serial|currentdevice|device_mode|device_connection|exist_device|hidden_devices|unlocked|fastbootd|current_slot|current_anti|wireless_IP|wireless_PORT|wireless_FORCE)$)"
      ; Add additional syntax definitions to DinoCode
	   maps2:=
       (join
          {  
			 press: {press: {literal:true, max:1}},
			 save: {save: {max:1, literal:true, at:1, atpos:true}, with: {support:true, max:1, at:2}, in: {support:true, max:1, at:3}},
			 install: {install: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}, with: {support:true, max:1, at:3}},
			 download: {download: {max:1, at:1, atpos:true}, in: {support:true, max:1, at:2}, with: {support:true, max:1, at:3}}
		  }
	   )
	   maps3:=
       (join
          {
			 push: {push: {max:1, at:1, atpos:true}, to: {support:true, max:1, at:2}},
			 update: {update: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}},
			 update_push: {update_push: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}},
			 update_ramdisk: {update_ramdisk: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}},
			 update_ramdisk_push: {update_ramdisk_push: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}},
			 update_kernel: {update_kernel: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}},
			 update_kernel_push: {update_kernel_push: {max:1, at:1, atpos:true}, in: {support:true, literal:true, max:1, at:2}},
			 install_recovery_ramdisk: {install_recovery_ramdisk: {max:1, literal:true}},
			 find_block: {find_block: {max:1, literal:true}},
			 apktool: {apktool: {literal:true}},
			 sign: {sign: {literal:true}},
			 zipalign: {zipalign: {literal:true}},
			 smali_kit: {smali_kit: {literal:true}},
			 7za: {7za: {literal:true}},
			 run_cmd_literal: {run_cmd_literal: {literal:true}},
			 shell_literal: {shell_literal: {literal:true}}
		  }
	   )
      for _key, value in maps2
          maps(_key, value)
      for _key, value in maps3
          maps(_key, value)
      set:=true
      FileDelete, % general_log
   }
}

; Extra Functions
Read_file(file,enc:="") {
   (!enc) ? enc:="UTF-8"
   try {
      FileEncoding, % enc
      FileRead, content, % file
   } catch {
      unexpected:="Cant read-->""" file """, Ended with: " ErrorLevel
   }
   return content
}
write_file(content, file, enc:="UTF-8-RAW") {
   try {
    if !Exist("folder",dest:=dirname(GetFullPathName(file)))
	FileCreateDir, % dest
    FileAppend, % content, % file, % enc
	return 1
   } catch {
    unexpected:="Cant write in: """ file """, Ended with: " ErrorLevel
	return 0
   }
}
JEE_StrUtf8BytesToText(ByRef vUtf8Bytes) {
	if A_IsUnicode
	{
		VarSetCapacity(vTemp, StrPut(vUtf8Bytes, "CP0"))
		StrPut(vUtf8Bytes, &vTemp, "CP0")
		return StrGet(&vTemp, "UTF-8")
	}
	else
		return StrGet(&vUtf8Bytes, "UTF-8")
}
FileExistFile(file) {
   return (FileExist(file)~="[^D]")
}
FileExistFolder(folder) {
   return InStr(FileExist(folder), "D")
}
CreateZipFile(sZip) {
   ; Idea by shajul http://www.autohotkey.com/forum/viewtopic.php?t=65401
	; I just ensure the use of UTF-8-RAW to avoid UTF-8 BOM leading bytes corrupting the ZIP (BlassGO)
   FileDelete, % sZip
   FileEncoding, UTF-8-RAW
   Header1 := "PK" . Chr(5) . Chr(6)
	VarSetCapacity(Header2, 18, 0)
	file := FileOpen(sZip,"w")
	file.Write(Header1)
	file.RawWrite(Header2,18)
	file.close()
   return 1
} 
zip(FilesToZip,sZip) {
   ; Idea by shajul http://www.autohotkey.com/forum/viewtopic.php?t=65401
	; Improved by me (BlassGO)
   global general_log
   if !(FileExistFile(sZip)||CreateZipFile(sZip)) {
      return 0
   }
   try {
      psh := ComObjCreate( "Shell.Application")
      zip := psh.Namespace(sZip := GetFullPathName(sZip))
      Loop, parse, FilesToZip, `;
      {  
         to_zip:=A_LoopField
         if (_type:=FileExist(to_zip)){
            InStr(_type, "D") ? to_zip .= SubStr(to_zip,0)="\" ? "*.*" : "\*.*"
            Loop, Files, %to_zip%, DF
            {
               folder := psh.Namespace(dirname(A_LoopFileLongPath))
               name := basename(A_LoopFileLongPath)
               file_size := folder.ParseName(name).ExtendedProperty("Size")
               write_file("`nZIP: Adding """ name """ in """ sZip """`n", general_log)
               zip.CopyHere(A_LoopFileLongPath, 4|16)
               Loop {
                  for file in zip.Items()
                  {
                     if (file.Name=name) {
                        if (file.IsFolder||zip.ParseName(name).ExtendedProperty("Size")=file_size)
                           break 2
                        break
                     }
                  }
               }
            }
         } else {
            write_file("`nZIP: Cant find file or folder: """ to_zip """`n", general_log)
            return 0
         }
      }
   } catch e {
	   MsgBox, % 262144 + 16, Zip, % "A fatal error occurred:`n`n" e.message "`n`n--> " e.what
      return 0
	}
   return 1
}
unzip(sZip, sUnz, options := "-o", resolve := false) {
   ; Idea by shajul http://www.autohotkey.com/forum/viewtopic.php?t=65401
	; Improved by me (BlassGO)
	static zip_in_use, last_path
   global general_log
	if !sUnz
	   sUnz := A_ScriptDir
	if !resolve {
	    if (options ~= "i)\binside-zip\b") {
		   RegexMatch(sZip, "i)^(.*?\.zip)\K.*", back_relative)
		   RegexMatch(sZip, "i)^(.*?\.zip)", sZip)
		}
		if FileExistFile(sZip) {
		   if !FileExistFolder(sUnz)
            FileCreateDir, % sUnz
		   sZip := GetFullPathName(sZip)
		   if back_relative
		      sZip .= back_relative
		   zip_in_use := sZip
		   last_path := sUnz
		} else {
		   write_file("`nUNZIP: Cant find file: """ sZip """`n", general_log)
		   return 0
		}
	}
	try {
		psh := ComObjCreate("Shell.Application")
		zip := psh.Namespace(sZip)
		back_opt := options
		if RegexMatch(options, "i)regex:\s*\K.*", regex) {
		   RegexMatch(options, "i)(.*?)(?=regex:)", options)
		} else if RegexMatch(options, "i)regex-name:\s*\K.*", regex) {
		   regex_name := true
		   RegexMatch(options, "i)(.*?)(?=regex-name:)", options)
		} else {
		   regex := ".*"
		}
		if (options ~= "i)\bfiles|-f\b")
		   allow_files := true
		if (options ~= "i)\bfolders|-d\b")
		   allow_folders := true
		if (options ~= "i)\boverwrite|force|-o\b")
		   overwrite := true
		if (options ~= "i)\bmix-path|-mix|-m\b")
		   mix := true
		if !allow_files&&!allow_folders {
		   allow_files := true
		   allow_folders := true
		}
		for file in zip.items
		{
			if (!allow_folders&&file.IsFolder) {
				unzip(file, sUnz, back_opt, true)
			} else if (allow_files&&allow_folders) || (allow_files&&!file.IsFolder) || (allow_folders&&file.IsFolder) {
				relative_to_zip := StrReplace(file.Path, zip_in_use "\")
				if (regex_name&&RegExMatch(file.Name, regex))||(!regex_name&&RegExMatch(relative_to_zip, regex)) {
					if mix {
					   dest := last_path "\" dirname(relative_to_zip, false)
					} else {
					   dest := last_path
					}
					if !FileExistFolder(dest)
					   FileCreateDir, % dest
					folder := psh.Namespace(GetFullPathName(dest))
					if overwrite || !FileExist(dest "\" file.Name) {
					   if (GetFullPathName(dest "\" file.Name)=current "\configs.ini") {
					      MsgBox, 262144, Unzip preferences, % " Attempting to replace:`n`n " current "\configs.ini`n`n This action is not allowed for your security"
                          return 0
					   }
					   file_size := file.ExtendedProperty("Size")
					   write_file("`nUNZIP: Extracting """ relative_to_zip """ in """ dest """`n", general_log)
					   folder.CopyHere(file, 4|16)
					   Loop {
						   if (FileExist(dest "\" file.Name) && (file.IsFolder||folder.ParseName(file.Name).ExtendedProperty("Size")=file_size)) {
							  break
						   }
					   }
					}
				}
			}
		}
	} catch e {
      InStr(e.what, "RegexMatch") ? e.message:="Check your Regex Pattern"
	   MsgBox, % 262144 + 16, Unzip, % "A fatal error occurred while extracting:`n""" zip_in_use """`n`n" e.message "`n`n--> " e.what
      return 0
	}
   (!resolve) ? (zip_in_use:="", last_path:="")
   return 1
}
check_content(options*) {
   global general_log
   if !FileExistFile(path:=GetFullPathName(options.Pop())) {
      write_file("CANT FIND: " path, general_log)
      return 0
   }
   psh := ComObjCreate("Shell.Application")
   while options.MaxIndex() {
      relative_path:=options.RemoveAt(1), name:=basename(relative_path, false), exist:=false
      relative_path:=path . "\" . dirname(relative_path, false)
      try {
         if psh.Namespace(relative_path).ParseName(name) {
            exist:=true
         } else {
            break
         }
      } catch e {
         write_file("ZIP: A fatal error occurred while reading: """ relative_path """--> " e.message "--> " e.what, general_log)
         return 0
      }
   }
   return exist
}
FileDeleteZip(options*) {
   global general_log
   if !FileExistFile(path:=GetFullPathName(options.Pop())) {
      write_file("CANT FIND: " path, general_log)
      return 0
   }
   psh := ComObjCreate("Shell.Application")
   dest := current "\tmp\FileDeleteZip"
   FileCreateDir, % dest
   folder := psh.Namespace(dest)
   while options.MaxIndex() {
      relative_path:=options.RemoveAt(1), name:=basename(relative_path, false)
      relative_path:=path . "\" . dirname(relative_path, false)
      try {
         if (file:=psh.Namespace(relative_path).ParseName(name)) {
            relative_to_zip := StrReplace(file.Path, path "\")
            file_size:=file.ExtendedProperty("Size")
            write_file("`nUNZIP: Deleting """ relative_to_zip """`n", general_log)
            folder.MoveHere(file, 4|16)
            Loop {
               if (FileExist(dest "\" file.Name) && (file.IsFolder||folder.ParseName(file.Name).ExtendedProperty("Size")=file_size)) {
                  break
               }
            }
            FileDelete, % dest "\" name
         } else {
            write_file("UNZIP: CANT FIND CONTENT: " name " in--> " relative_path, general_log)
            return 0
         }
      } catch e {
         write_file("UNZIP: A fatal error occurred while reading: """ relative_path """--> " e.message "--> " e.what, general_log)
         return 0
      }
   }
   FileRemoveDir, % dest, 1
   return exist
}
FileMoveZip(file,dest,zip) {
   global general_log
   if !FileExistFile(path:=GetFullPathName(zip)) {
      write_file("CANT FIND: " path, general_log)
      return 0
   }
   psh := ComObjCreate("Shell.Application")
   name:=basename(file, false)
   relative_path:=path . "\" . dirname(file, false)
   try {
      if (file:=psh.Namespace(relative_path).ParseName(name)) {
         if !FileExistFolder(dest)
            FileCreateDir, % dest
         relative_to_zip := StrReplace(file.Path, path "\")
         folder:=psh.Namespace(GetFullPathName(dest))
         file_size:=file.ExtendedProperty("Size")
         write_file("`nUNZIP: Moving """ relative_to_zip """ to """ dest """`n", general_log)
         folder.MoveHere(file, 4|16)
         Loop {
            if (FileExist(dest "\" file.Name) && (file.IsFolder||folder.ParseName(file.Name).ExtendedProperty("Size")=file_size)) {
               break
            }
         }
      } else {
         write_file("UNZIP: CANT FIND CONTENT: " name " in--> " relative_path, general_log)
         return 0
      }
   } catch e {
      write_file("UNZIP: A fatal error occurred while reading: """ relative_path """--> " e.message "--> " e.what, general_log)
      return 0
   }
   return 1
}
ensure_java(txt:="java version") {
   static passed
   if passed||InStr(run_cmd("java -version"),txt) {
      passed:=true
      return 1
   } else if question("Java","JAVA not detected`n`nDo you want to go to the official site?") {
      gotolink("https://www.java.com/en/")
   }
   unexpected:="Java not available"
   print(">> " unexpected)
   return 0
}
apktool(options*) {
   global apktool
   if !ensure_java()
      return 0
   return run_cmd("java -jar """ apktool """ " StrJoin(options,A_Space,true))
}
sign(options*) {
   global sign
   if !ensure_java()
      return 0
   return run_cmd("java -jar """ sign """ " StrJoin(options,A_Space,true))
}
zipalign(options*) {
   global zipalign
   return run_cmd("""" zipalign """ " StrJoin(options,A_Space,true))
}
7za(options*) {
   global 7za
   return run_cmd("""" 7za """ " StrJoin(options,A_Space,true))
}
run_cmd_literal(options*) {
   return run_cmd(options.RemoveAt(1) . " " . StrJoin(options,A_Space,true))
}
shell_literal(options*) {
   return shell(StrJoin(options,A_Space,true))
}
TrimLine(Byref content) {
    header:=InStr(content, "`n", false, 1), last:=InStr(content, "`n", false, 0)
    return SubStr(content,header+1, last?last-header-1:0)
}
WriteAfterLine(Byref content, Byref str, Byref str2:="", linebreak:="`n") {
   tlen:=StrLen(content), _LEN:=StrLen(str)
   if (_POS:=InStr(content,str))
   {
      (_startline:=InStr(content,"`n",,_POS-tlen)) ? (_LEN+=(_POS-_startline)-1, _POS:=_startline+1) : (_POS:=1, _LEN:=0)
      (_endline:=InStr(content, "`n",, _POS+_LEN)) ? (_LEN+=_endline-(_POS+_LEN)) : (_LEN+=tlen-_LEN)
      return RTrim(SubStr(content,1,_POS+_LEN),"`r`n") . linebreak . str2 . linebreak . LTrim(SubStr(content,_POS+_LEN+1),"`r`n")
   }
   return content
}
WriteBeforeLine(Byref content, Byref str, Byref str2:="", linebreak:="`n") {
   tlen:=StrLen(content), _LEN:=StrLen(str)
   if (_POS:=InStr(content,str))
   {
      (_startline:=InStr(content,"`n",,_POS-tlen)) ? (_LEN+=(_POS-_startline)-1, _POS:=_startline+1) : (_POS:=1, _LEN:=0)
      (_endline:=InStr(content, "`n",, _POS+_LEN)) ? (_LEN+=_endline-(_POS+_LEN)) : (_LEN+=tlen-_LEN)
      return RTrim(SubStr(content,1,_POS-1),"`r`n") . ((_POS-1>0)?linebreak:"") . str2 . linebreak . LTrim(SubStr(content,_POS),"`r`n")
   }
   return content
}
smali_kit(options*) {
   complete_extract:=true, result:=0, to_do:={}
   while options.MaxIndex() {
      if (SubStr(p:=options.RemoveAt(1),1,1)="-") {
         (SubStr(options[1],1,1)!="-") ? ((p2:=options.RemoveAt(1)) ? (SubStr(options[1],1,1)!="-") ? (p3:=options.RemoveAt(1)) : p3:="") : (p2:=p3:="")
         switch (p) {
            case "-dir","-d","-file","-f":
               dir:=p2
            case "-method","-m":
              method:=p2
            case "-print-path","-pp":
              print:=true, result:=""
            case "-replace","-r":
              to_do:={1: {rpl:p2, r:true}}
            case "-delete-method","-dm":
              to_do:={1: {rpl:"", r:true}}
            case "-remake","-re":
              to_do:={1: {rpl:p2, r:true}}, complete_extract:=false
            case "-replace-in-method","-rim":
              to_do.Push({orig:p2, rpl:p3}), complete_extract:=false
            case "-delete-in-method","-dim":
              to_do.Push({orig:p2}), complete_extract:=false
            case "-after-line","-al":
              to_do.Push({line:p2, add:p3, al:true}), complete_extract:=false
            case "-before-line","-bl":
              to_do.Push({line:p2, add:p3, bl:true}), complete_extract:=false
            case "-name","-n":
              opt.="name: " p2
            case "-static-name","-sn":
              opt.="static-name: " p2
            case "-check","-c":
              check:=true
         }
      }
   }
   (dir&&method) ? info:=StrSplit(find_str(dir,method,".end method","complete_extract recursive all lines info " opt),"`n")
   Loop % info.MaxIndex()
   {
      if InStr(info[A_Index], "PATH=") {
         path:=SubStr(info[A_Index],StrLen("PATH=")+1), pos:=SubStr(info[A_Index+1],StrLen("POS=")+1), len:=SubStr(info[A_Index+2],StrLen("LEN=")+1)
         content_tmp:=read_file(path)
         ((path=last_path)&&(addlen:=StrLen(content_tmp)-last_len)) ? (pos+=addlen, len+=addlen)
         check_method:=SubStr(content_tmp,pos,30)
         if (check_method~="^\.method ")&&!(check_method~="\.method abstract|\.method public abstract") {
            content:=print?"":(complete_extract?SubStr(content_tmp, pos, len):TrimLine(SubStr(content_tmp, pos, len)))
            if print {
               (result) ? result.="`n"
               result.=path
               continue
            }
            for cont, props in to_do
            {
               if props.r {
                  content:=props.rpl
               } else if (props.orig!="") {
                  content:=StrReplace(content, props.orig, props.rpl)
               } else if (props.line!="") {
                  content:=(props.bl)?WriteBeforeLine(content,props.line,props.add):WriteAfterLine(content,props.line,props.add)
               }
            }
            content:=StrReplace(content_tmp,SubStr(content_tmp, pos, len),complete_extract?content:SubStr(content_tmp,pos,InStr(content_tmp,"`n",false,pos)-pos) . "`n" . content . "`n.end method")
            if (content=content_tmp) {
               (check) ? print(">> Nothing: """ . path . """")
            } else {
               FileDelete(path)
               if write_file(content,path, "UTF-8-RAW") {
                  result:=1, last_len:=StrLen(content_tmp), last_path:=path, (check) ? print(">> Edited: """ . path . """")
               }
            }
         }
         content:=content_tmp:=""
      }
   }
   return result
}
find_str(path,str,str2:="",options:="") {
   ; By @BlassGO
   orig_len:=StrLen(str), orig_len2:=StrLen(str2), mode:="F", case_sense:=false
   RegexMatch(options, "i)encoding:\s*(CP\d+|UTF-8|UTF-8-RAW|UTF-16|UTF-16-RAW)", opt) ? options:=StrReplace(options, opt)
   enc:=opt1 ? opt1 : "UTF-8"
   if RegexMatch(options, "i)name:\s*(.*)", opt)
      options:=StrReplace(options, opt), name:=(opt1!="") ? opt1 : "", literal_name:=false
   else if RegexMatch(options, "i)static-name:\s*(.*)", opt) 
      options:=StrReplace(options, opt), name:=(opt1!="") ? opt1 : "", literal_name:=true
   FileEncoding, % enc
   if (options ~= "i)\bpattern\b")
		pattern := true
   if (options ~= "i)\ball\b")
		all := true
   if (options ~= "i)\binfo\b")
		info := true
   if (options ~= "i)\brecursive\b")
		mode .= "R"
   if (options ~= "i)\blines\b")
		as_lines := true
   if (options ~= "i)\bextract\b")
		extract := true, header:=false
   if (options ~= "i)\bcomplete_extract\b")
		extract := true, header:=true
   if (options ~= "i)\bcase_sense\b")
      case_sense := true
   (!pattern&&InStr(FileExist(path), "D")) ? (path .= SubStr(path,0)="\" ? "*.*" : "\*.*"):false
   Loop, Files, %path%, %mode%
   {  
      if isBinFile(A_LoopFileLongPath)||((name!="")?(literal_name?(name=A_LoopFileName):(InStr(A_LoopFileName,name))):false)
         continue
      _last:=1
      FileRead, content, % A_LoopFileLongPath
      as_lines ? tlen:=StrLen(content)
      while (_last>0) {
         len:=orig_len, len2:=orig_len2
         if (_at:=InStr(content,str,case_sense,_last)) {
            if as_lines {
               (_startline:=InStr(content,"`n",false,_at-tlen)) ? (len+=(_at-_startline)-1, _at:=_startline+1) : (_at:=1, len:=0)
               (_endline:=InStr(content, "`n",false, _at+len)) ? (len+=_endline-(_at+len)-(SubStr(content,_endline-1,1)="`r")) : (len+=tlen-len)
            }
            if (str2!="") {
               if (_at2:=InStr(content,str2,case_sense,_at+len)) {
                  if as_lines {
                     (_startline:=InStr(content,"`n",false,_at2-tlen)) ? (len2+=(_at2-_startline)-1, _at2:=_startline+1) : (_at2:=1, len2:=0)
                     (_endline:=InStr(content, "`n",false, _at2+len2)) ? (len2+=_endline-(_at2+len2)-(SubStr(content,_endline-1,1)="`r")) : (len2+=tlen-len2)
                  }
                  (extract) ? _last:=_at2+len2 : _last:=0
                  if info {
                     if all
                        result ? result.="`n" : false, result.= header ? ("PATH=" . A_LoopFileLongPath . "`nPOS=" . _at . "`nLEN=" . (_at2-_at)+len2) : ("PATH=" . A_LoopFileLongPath . "`nPOS=" . _at+len . "`nLEN=" . (_at2-_at)-len)
                     else
                        return header ? ("PATH=" . A_LoopFileLongPath . "`nPOS=" . _at . "`nLEN=" . (_at2-_at)+len2) : ("PATH=" . A_LoopFileLongPath . "`nPOS=" . _at+len . "`nLEN=" . (_at2-_at)-len)
                  } else if all {
                     result ? result.="`n" : false, result.=(extract) ? (header ? SubStr(content,_at,(_at2-_at)+len2) : SubStr(content,_at+len,(_at2-_at)-len)) : A_LoopFileLongPath
                  } else {
                     return (extract) ? (header ? SubStr(content,_at,(_at2-_at)+len2) : SubStr(content,_at+len,(_at2-_at)-len)) : A_LoopFileLongPath
                  }
               } else {
                  _last:=0
               }
            } else {
               if extract {
                  (!as_lines) ? ((_endline:=InStr(content, "`n",, _at+len)) ? (len+=_endline-(_at+len)-(SubStr(content,_endline-1,1)="`r")) : (len+=(tlen?tlen:(tlen:=StrLen(content)))-len)) : false
                  _last:=_at+len
               } else {
                  _last:=0
               }
               if info {
                  if all
                     result ? result.="`n" : false, result.="PATH=" . A_LoopFileLongPath . "`nPOS=" . _at . "`nLEN=" . len
                  else
                     return "PATH=" . A_LoopFileLongPath . "`nPOS=" . _at . "`nLEN=" . len
               } else if all {
                  result ? result.="`n" : false, result.=(extract) ? SubStr(content,_at,len) : A_LoopFileLongPath
               } else {
                  return (extract) ? SubStr(content,_at,len) : A_LoopFileLongPath
               }
            }
         } else {
            _last:=0
         }
      }
   }
   return result
}
isBinFile(Filename,NumBytes:=32,Minimum:=4,complexunicode:=1) {
	file:=FileOpen(Filename,"r")
	file.Position:=0 ;force position to 0 (zero)
	nbytes:=file.RawRead(rawbytes,NumBytes) ;read bytes
	file.Close() ;close file
	
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
get_serial(str, wireless := false) {
   if wireless||!RegexMatch(str, "`am)^([a-zA-Z]*[0-9][a-zA-Z0-9]+?)(?=[ `t])", serial)
      RegexMatch(str, "`am)^((\d{1,3}\.){3}\d{1,3}:\d+?)(?=[ `t])", serial)
   return serial
}
ubasename(str) {
   return RegExReplace(str, ".*/([^/]+)$", "$1")
}
wbasename(str) {
   return RegExReplace(str, ".*\\([^\\]+)$", "$1")
}
format_sh(str){
   return RegExReplace(RegExReplace(RegExReplace(str,"m`a)^\s+|\h+$|\s+(?-m)$"), "(if|do|then|else|elif|\{|\()[ \t]*\r?\n", "$1 "), "(;[ \t]*\r?\n)|(\r?\n)", ";")
}
sh(str, noroot:="", noescape:="") {
   return adb_shell(format_sh(str),noroot,noescape)
}
shell(action, noroot := "", noescape := "") {
   return RegExReplace(adb_shell(action, noroot, noescape), "m`a)^\s+|\h+$|\s+(?-m)$")
}
check_string(try, str, files := true) {
	(files&&FileExistFile(try)) ? try := read_file(try)
	return (try ~= str)
}
FileCreateDir(options*) {
   for key,dir in options {
      dir := GetFullPathName(dir)
      (A_Index=1) ? result:=1
      if !FileExistFolder(dir) {
         FileCreateDir, % dir
         (!result) ? result:=ErrorLevel
      }
   }
   return result
}
FileDelete(options*) {
   for key,dir in options {
      dir := GetFullPathName(dir)
      (A_Index=1) ? result:=1
      if (_type:=FileExist(dir)) {
         if InStr(_type, "D")
            FileRemoveDir, % dir, 1
         else
            FileDelete, % dir
         (!result) ? result:=ErrorLevel
      }
   }
   return result
}
FileMove(orig,dir) {
   GetFullPathName(dir) ? dir := GetFullPathName(dir)
   orig := GetFullPathName(orig)
   FileMove, % orig, % dir, 1
   return FileExist(dir)
}
run_cmd(code, seconds:="") {
   global exitcode, cmdpid
   exitcode:="", cmdoutput:=A_Temp . "\cmd_output"
   FileDelete, % cmdoutput
   DetectHiddenWindows, On
   if (seconds ~= "^[1-9][0-9]*$") {
	   SetTimer, cmd_output, % seconds * 1000
   }
   RunWait, %comspec% /c "%code% > `"%cmdoutput%`" 2>&1",, Hide UseErrorLevel, cmdpid
   exitcode:=ErrorLevel, result:=read_file(cmdoutput)
   Process, Exist, % cmdpid
   if ErrorLevel {
      Process, Close, % cmdpid
   }
   FileDelete, % cmdoutput
   return JEE_StrUtf8BytesToText(result),cmdpid:=""
   cmd_output:
      SetTimer, cmd_output, Off
      if (exitcode="") {
         Process, Close, % cmdpid
      }
   return
}
get_win_ver() {
   if (A_OSVersion ~= "^10\.0\.22")
      return 11
   if (A_OSVersion ~= "^10\.0\.21")
      return 11
   if (A_OSVersion ~= "^10\.0")
      return 10
   if (A_OSVersion ~= "^6\.3")
      return 8.1
   if (A_OSVersion ~= "^(6,2|6\.2)")
      return 8
   if (A_OSVersion ~= "^6\.1")
      return 7
   return Unknown
}
adb(action, seconds:="") {
   global adb
   return run_cmd("""" adb """" " " action, seconds)
}
adb_serial(action, this_serial := "", seconds:="") {
   global serial
   return (this_serial) ? adb("-s " this_serial " " action, seconds) : ((serial) ? adb("-s " serial " " action, seconds) : 0)
}
adb_shell(action, noroot := "", noescape := "") {
   global PATH
   (!noescape) ? action := StrReplace(action, "\", "\\")
   action := StrReplace("export PATH=""$PATH:" . PATH . """;" . StrReplace(action, "`r`n"), """", "\""")
   ensure_shell()
   return (!noroot&&InStr(adb_serial("shell ""[ -e \""$(command -v su)\"" ] && echo SU command support"""), "SU command")) ? adb_serial("shell ""su -c '" . action . "'""") : adb_serial("shell """ . action . """")
}
ensure_shell() {
   global adb, exist_device, device_mode, remember_mode, general_log, serial
   while !InStr(adb_serial("shell echo pwu"), "pwu")
   {
      if !attemps {
	     print(">> Waiting device shell")
		  attemps := true
	   }
      (!(serial&&check_active(serial))) ? find_device(1)
   }
}
fastboot(action, seconds := "") {
   global fastboot
   Process, Close, % basename(fastboot)
   return run_cmd("""" fastboot """" " " action, seconds)
}
fastboot_serial(action, this_serial := "", seconds:="") {
   global serial
   return (this_serial) ? fastboot("-s " this_serial " " action, seconds) : ((serial) ? fastboot("-s " serial " " action, seconds) : 0)
}
is_active(serial) {
   global exist_device, device_mode, remember_mode
   if !serial
      return 0
   if (remember_mode=="fastboot") {
      result := fastboot("devices")
	  RegexMatch(result, "`am)^(\Q" serial "\E)[ `t]+\S+", result)
	  if (serial!=get_serial(result))
	     result=error
   } else {
      result := adb_serial("get-state", serial)
   }
   if !result
      return 0
   if !InStr(result, "error") {
      exist_device=true
	  device_mode := remember_mode
      return 1
   } else {
      return 0
   }
}
check_active(serial) {
    if !serial
	   return 0
    if !is_active(serial) {
       print(">> Device was disconnected!")
	   return 0
    } else {
	   return 1
	}
}
get_bootloader_env() {
   global device_mode, exist_device, unlocked, fastbootd, secure, current_slot, super, serial, current_anti, native_parts, general_log
   unlocked =
   fastbootd =
   secure =
   current_slot =
   current_anti =
   super =
   if (serial && !check_active(serial))
      return 0
   if (device_mode!="fastboot")
      return 0
   get := fastboot_serial("getvar all")
   if !get
      return 0
   native_parts := {}
   huh := StrSplit(get, [A_Tab,")","`r","`n",":"])
   Loop % huh.MaxIndex()
   {
      huh[A_Index] := Trim(huh[A_Index], "`n" A_Space "`t")
   }
   Loop % huh.MaxIndex()
   {
        If (huh[A_Index]="unlocked") {
		    if (huh[A_Index+1]="yes") {
			   unlocked := true
			}
		}
		If (huh[A_Index]="is-userspace") {
		    if (huh[A_Index+1]="yes") {
			   fastbootd := true
			}
		}
	    If (huh[A_Index]="secure") {
		    if (huh[A_Index+1]="yes") {
			   secure := true
			}
        }
	    If (huh[A_Index]="current-slot") {
			if (huh[A_Index+1]="a") || (huh[A_Index+1]="b") {
               current_slot := "_" huh[A_Index+1]
			}
		}
		If (huh[A_Index]="anti") {
			if huh[A_Index+1] is integer
			{
               current_anti := huh[A_Index+1]
			}
		}
		If (huh[A_Index]="is-logical") {
			if (huh[A_Index+2]="yes") {
			   super := true
            try {
               native_parts.HasKey(huh[A_Index+1]) ? native_parts[huh[A_Index+1]].is_logical:=true : native_parts[huh[A_Index+1]]:={is_logical:true}
            } catch e {
               write_file("`nAbnormal partition: " . huh[A_Index+1] . "-->" . e.message . "`n",general_log)
            }
			}
	    }
		If (huh[A_Index]="partition-size") {
		   raw := huh[A_Index+2]
			SetFormat, Integer, D
			raw += 0
			if raw is integer
			{
            try {
               native_parts.HasKey(huh[A_Index+1]) ? native_parts[huh[A_Index+1]].size:=raw : native_parts[huh[A_Index+1]]:={size:raw}
               if (huh[A_Index+1] ~= "^(system|system_ext|vendor|product|odm)(_a|_b)$") && !native_parts[huh[A_Index+1]].is_logical {
                  super:=true
                  native_parts[huh[A_Index+1]].is_logical:=true
               } else if !super && (huh[A_Index+1]="super") {
                  super:=true
               }
            } catch e {
               write_file("`nAbnormal partition: " . huh[A_Index+1] . "-->" . e.message . "`n",general_log)
            }
			}
	    }
   }  
}
is_logical(part) {
   global native_parts
   return native_parts.HasKey(part) ? native_parts[part].is_logical : false
}
is_real(part) {
   global native_parts
   return native_parts.HasKey(part) 
}
get_size(part) {
   global native_parts
   return native_parts.HasKey(part) ? native_parts[part].size : false
}
free_logical(){
   global super
   global native_parts
   if !super
      return 0
   get_bootloader_env()
   subpart_size := 0, free_logical := 0, subpart_count:=0
   if !(super_size := get_size("super")) {
      print(">> Empty SUPER image!")
      return
   }
   for part, props in native_parts
       (props.is_logical) ? (subpart_count++, subpart_size+=props.size)
   if (super_size>subpart_size) {
       ; 1MB reserved for each subpart
       free_logical := (super_size - 1048576 * subpart_count) - subpart_size
   } else {
      print(">> Abnormal SUPER image!")
   }
   return (free_logical>0) ? free_logical : 0
}
normal_units(size) {
   in = GB
   size := size / 1024**3
   if (Floor(size)=0) {
      in = MB
	  size := size * 1024
   }
   if (Floor(size)=0) {
	  in = KB
	  size := size * 1024
   }
   size := Round(size, 2)
   if size is number
      return size . " " . in
   else
      return Unknown
}
optimize() {
   global cache, serial, general_log
   if (serial && !check_active(serial))
      return
   (!IsObject(cache)) ? cache:={commands:{}, parts:{}}
   Loop, parse, % adb_shell("ls -lR /dev/block 2>dev/null"), `n,`r 
   {
      if (chr:=SubStr(A_LoopField,1,1)) {
         if (chr="/") {
            dir:=Trim(A_LoopField,A_Space "`t:")
         } else if (chr~="^b|c|l$") {
            try {
               data:=StrSplit(Trim(A_LoopField,A_Space "`t"), A_Space)
               (chr="l") ? cache.parts[data[data.MaxIndex()-2]]:=data.Pop() : cache.parts[data[data.MaxIndex()]]:=dir "/" data.Pop()
               data:=""
            } catch e {
                write_file("`nCACHE: " e.message " -> " A_LoopField "`n",general_log)
            }
         }
      }
   }
   Loop, parse, % adb_shell("IFS="":""; for dir in $PATH; do echo $dir; ls -l $dir 2>/dev/null; done"), `n,`r 
   {
      if (chr:=SubStr(A_LoopField,1,1)) {
         if (chr="/") {
            dir:=Trim(A_LoopField,A_Space "`t")
         } else if (chr~="^-|l$") {
            try {
               data:=StrSplit(Trim(A_LoopField,A_Space "`t"), A_Space)
               if (chr="l") {
                  name:=data[data.MaxIndex()-2]
               } else {
                  name:=data.Pop()
                  (SubStr(name,0)="*") ? name:=SubStr(name,1,StrLen(name)-1)
               }
               (!(cache.commands.HasKey(name)||InStr(name,"/"))) ? cache.commands[name]:=dir "/" name
               data:=""
            } catch e {
                write_file("`nCACHE: " e.message " -> " A_LoopField "`n",general_log)
            }
         }
      }
   }
   return 1
}
find_device(attemps := 1, show_msg := "", show_box := "", show_panel := "", optimize:=true) {
   global exist_device, device_mode, device_connection, remember_mode, currentdevice, devices, serial, current, cache
   global wireless_IP, wireless_PORT, hidden_devices, wireless_FORCE
   global style, adb, mx, my, general_log
   static ip_check := "((^\s*((([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5]))\s*$)|(^\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\s*$))"
   exist_device=
   device_mode=
   device_connection=
   serial_n=0
   remember_mode=
   cache=
   if show_box||show_panel {
      if hidden_devices
	     print(">> Finding hidden device...")
	  else
         print(">> Finding device...")
   }
   Loop
   {
	   if (A_Index <= attemps) {
	      devices := []
		   currentdevice := ""
		   devicelist := ""
	      result := fastboot("devices")
		   if hidden_devices && !(result ~= "\bfastboot\b") {
			   if (result := fastboot("getvar serialno", 5)) {
			     Loop, Parse, result, `n
              {
			         if RegExMatch(A_LoopField, "serialno:[ `t]*\K\S+", hidden_serial) {
					      devicelist .= hidden_serial " fastboot `n"
					   }
			     }
				  result := devicelist
				  write_file("`n[HIDDEN DEVICES]`n" result "`n", general_log)
			   }
		   }
		   if !(result ~= "\bfastboot\b") {
		      result := adb("start-server")
		      if InStr(result, "daemon started successfully") {
			     write_file("`nADB: Started the server successfully`n[`n" result "`n]`n", general_log)
			  } else {
			     write_file("`nADB: Server in use, resetting...`n", general_log)
				  result := adb("kill-server")
				  result .= adb("start-server")
				  if !InStr(result, "daemon started successfully") {
				    write_file("`nADB: Could not start the server`n[`n" result "`n]`n", general_log)
                htmlmsg_img("HELP", "error.png", "Could not start ADB server`n`nCannot continue...")
					 return 0
				  }
			  }
			  if wireless_IP {
			      print(">> TCP/IP: Loading...")
			      if !(ip&&port) {
					  if RegExMatch(wireless_IP, "(\b(?:\d{1,3}\.){3}\d{1,3}\b)(?:\s*:\s*(\d+))?", ip) {
						 ip:=ip1,port:=ip2
					  } else if (wireless_IP ~= ip_check) {
					     ip := "[" wireless_IP "]"
					  } else {
                   htmlmsg_img("HELP", "error.png", "Invalid IP: """ wireless_IP """")
						 break
					  }
					  if !port {
						if (wireless_PORT ~= "^\d+$") {
						   port := wireless_PORT
						} else if wireless_PORT {
                     htmlmsg_img("HELP", "error.png", "Invalid PORT: """ wireless_PORT """")
						   break
						}
					  }
					  if (port="") {
						wireless_PORT := port := 5555
						with_usb := true
					  }
				  }
				  if with_usb {
				   (device_connection!="USB") ? htmlmsg_img("HELP", "usb.png", "Please connect your device via USB cable.")
					write_file("`nTCP/IP: Trying to define port", general_log)
					if InStr(adb_serial("tcpip " port), "restarting in TCP") {
					   write_file("`nTCP/IP: PORT was defined as """ port """ successfully`n", general_log)
					   with_usb := false
					   if (adb("connect " ip ":" port, 5) ~= "connected|already") {
					      print(">> TCP/IP: Connected")
					      write_file("`nTCP/IP: Connected to IP[" ip "] : PORT[" port "]`n", general_log)
						  serial := ip ":" port
					   }
					} else {
					   write_file("`nTCP/IP: Could not define port as """ port """`n", general_log)
					   show_msg := true
					}
				  } else if print(">> TCP/IP: Without USB?") && (adb("connect " ip ":" port, 5) ~= "connected|already") {
				    print(">> TCP/IP: Yeah, connected")
					write_file("`nTCP/IP: Connected to IP[" ip "] : PORT[" port "] without a USB connection`n", general_log)
					with_usb := false
					serial := ip ":" port
				  } else {
				    print(">> TCP/IP: With USB")
					with_usb:=true
				    if !increased {
					   attemps++
					   increased:=true
					} else {
					   write_file("`nTCP/IP: Device not booted or unauthorized`n", general_log)
                  htmlmsg_img("HELP", "dev_wir.png", "Could NOT establish wireless connection, insufficient permissions or invalid IP for the device.")
					   Process, Close, % basename(adb)
					}
				  }
			   }
		       result := adb("devices -l")
		   }
		   Loop, Parse, result, `n
           {
		        device_serial := get_serial(A_LoopField)
				if device_serial {
				   serial_n++
				   if !RegexMatch(A_LoopField, "model:\K\S+", name) {
				      if !RegexMatch(A_LoopField, "(device product|device):\K\S+", name)
					     name := "Device " serial_n
				   }
				   if get_serial(A_LoopField, true) {
				      connection := "WIFI"
				   } else {
				      connection := "USB"
				   }
				   devices.Insert({name: name, connection: connection, serial: device_serial})
				   if (serial && device_serial=serial) {
				      currentdevice := devices[devices.MaxIndex()]
				   }
				}
           }
		   if ((show_panel&&devices.MaxIndex())||(!currentdevice&&devices.MaxIndex()>1)) {
             Gui new4: Destroy 
		       Gui new4: Default 
		       Gui new4: -MinimizeBox +AlwaysOnTop
			   Gui new4: margin, %mx%, %my%
			   Gui new4: Font, s10, %style%
			   Gui new4: Add, Text, Y+0, Select a device:
			   Gui new4: Add, ListView, AltSubmit NoSortHdr -LV0x10 LV0x20 +Grid +LVS_FULLROWSELECT XS Y+10 hwndselect_device gselect_device,Device |Connection |Serial
			   LV_Delete()
			   Loop, % devices.MaxIndex()
			   {
			      LV_Add("", devices[A_Index].name, devices[A_Index].connection, devices[A_Index].serial)
			   }
			   Loop % LV_GetCount("Column")
                  LV_ModifyCol(A_Index, "AutoHdr Center")
			   Gui new4: Show, AutoSize Center, Devices
			   WinWaitClose, Devices
			   Gui new4: Destroy
		   } else if !devices.MaxIndex() {
		      continue
		   }
		   if !currentdevice {
		      currentdevice := devices[1]
		   }
		   serial := currentdevice.serial
		   exist_device=
         device_mode=
		   RegexMatch(result, "`am)^(\Q" serial "\E)[ `t]+\S+", result)
		   if (result ~= "\bfastboot\b") {
		      exist_device=true
			   device_mode=fastboot
		   } else {
		       if (result ~= "\bdevice\b") {
		          exist_device=true
				    device_mode=booted  
		       } else {
                  if (result ~= "\brecovery\b") {
		              exist_device=true
				        device_mode=recovery  
		          } else {
                     if (result ~= "\bsideload\b") {
		                 exist_device=true
				           device_mode=sideload   
		             } else {
                      if (result ~= "\bunauthorized\b") {
                         htmlmsg_img("HELP", "unauthorized.png", "A device was detected, but access is denied!`n<h3>Please fix it manually:</h3>`nYou can try turning USB debugging off and on.",,600)
                         print(">> Warning: Unauthorized device!")
				             break
		                } else {
                        if (result ~= "\boffline\b") {
                           htmlmsg_img("HELP", "turn_on.png", "A device was detected, but it is inactive!`n`nPlease Turn On the Screen of your device to restore the connection.",,600)
                           print(">> Warning: Offline device!")
                           break
                        } else {
                           continue
                        }
						    }
		             }
		          }
		       }
		   }
		   if exist_device {
		      device_connection:=currentdevice.connection
		      if !(with_usb&&wireless_FORCE) {
			      with_usb ? print(">> TCP/IP: Skipped")
				  write_file("`nDefault device----> Name[" currentdevice.name "] : Connection[" currentdevice.connection "] : Mode[" device_mode "] : Serial[" currentdevice.serial "]`n", general_log)
				  if show_box
					 print(">> Device detected!")
				  if device_mode {
					 remember_mode := device_mode
				  }
				  break
			  }
		   }
       } else {
	      if show_box {
		     print(">> Cant find any device")
		  }
		  if show_msg {
			  if wireless_IP {
               htmlmsg_img("HELP", "error.png", "HUH Can't connect TCP/IP: `n`nIP[" ip "] : PORT[" port "]")
			  } else {
               htmlmsg_img("HELP", "error.png", "HUH Can't detect your device")
			  }
		  }
		  break
	   }
   }
   if exist_device {
      if (device_mode="fastboot")
         get_bootloader_env()
      else if optimize && (device_mode!="sideload") {
         print(">> Optimizing: Please wait...")
         optimize()
         print(">> Optimizing: Done")
      }
      return serial
   } else {
      return 0
   }
   new4GuiClose:
   if !show_panel&&!currentdevice {
      htmlmsg("HELP", "Double click the device you want")
   } else {
	  Gui new4: Destroy
   }
   return
   select_device:
   If (A_GuiEvent = "DoubleClick") && devices[A_EventInfo] {
	  currentdevice := devices[A_EventInfo]
	  Gui new4: Destroy
	  if show_panel
	     print(">> Device: " currentdevice.name)
   }
   return
}
delete_partition(part) {
   global general_log
   global serial
   global fastbootd
   if (serial && !check_active(serial))
      return 0
   if !fastbootd {
      MsgBox, % 262144 + 64, HELP, You cant delete partitions from normal fastboot, only fastbootD
	  return 0
   }
   part := with_slot(part)
   if !is_real(part) {
      print(">> " part " partition does not exist!")
	  return 0
   }
   if is_logical(part) {
      print(">> Deleting " part "...")
      result := fastboot("delete-logical-partition " part " -s " serial)
   } else {
      print(">> No dynamic part: " part)
	  MsgBox, % 262144 + 64, HELP, You can only delete Dynamic Partitions
	  return 0
   }
   if (result ~= "i)\berror|failed\b") {
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % result
	  return 0
   }
   return 1
   ; Possible evalaluation of errors coming soon
}
create_partition(part, size) {
   global super
   global general_log
   global serial
   global fastbootd
   if (serial && !check_active(serial))
      return 0
   if !fastbootd {
      MsgBox, % 262144 + 64, HELP, You cant create partitions from normal fastboot, only fastbootD
	  return 0
   }
   if !super {
      print(">> No dynamic device")
	  MsgBox, % 262144 + 64, HELP, You can only create partitions on devices with Dynamic Partitions
	  return 0
   }
   if !size
      return 0
   print(">> Creating " part "...")
   result := fastboot("create-logical-partition " part " " size " -s " serial)
   if (result ~= "i)\berror|failed\b") {
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % result
	  return 0
   }
   return 1
   ; Possible evalaluation of errors coming soon
}
to_bytes(size) {
   in := RegExReplace(size, "^(?:[0-9]+(?:[.][0-9]+)?)([a-zA-Z]+)?$", "$1")
   size := RegExReplace(size, "^([0-9]+([.][0-9]+)?)([a-zA-Z]+)?$", "$1")
   if !size
      return
   if (in="GB") {
      size := size * 1024**3
   } else if (in="MB") {
	  size := size * 1024**2
   } else if (in="KB") {
	  size := size * 1024
   } else if (in!="B") {
      return
   }
   size := Floor(size)
   if size is integer
   {
      return size
   } else {
      return
   }
}
xiaomeme_test(img) {
   global current_anti
   global anti_notspam
   if !current_anti
      current_anti = 0
   if anti_notspam
      return 1
   path := dirname(img)
   name := basename(img)
   if FileExistFile(path "\anti_version.txt") {
      xiaomeme := true
	  anti := read_file(path "\anti_version.txt")
   } else If FileExistFile(path "\flash_all.bat") {
      xiaomeme := true
	  anti := RegExReplace(read_file(path "\flash_all.bat"), ".*CURRENT_ANTI_VER=([0-9]+).*", "$1", result)
	  if !result
	     anti =
   }
   anti := Trim(anti, "`n`r" A_Space "`t")
   if xiaomeme {
     if anti is integer
	 {
	    if (current_anti>anti) {
		   MsgBox, 262148, WARNING, % "Your Xiaomi device does not support this firmware (Anti Roll Back: " current_anti " > " anti ")`n`nRISK:    KILL YOUR DEVICE`nFILE:     " name "`n`nDo you really want to continue?"
	       IfMsgBox Yes
		   {
		     anti_notspam := true
		     return 1
		   } else {
		     return 0
		   }
		} else {
		   return 1
		}
	 } else {
	    MsgBox, 262148, WARNING, % "Xiaomi firmware was detected, but the Anti Roll Back number could not be obtained`n`nRISK:    Possibly kill your device`nFILE:     " name "`n`nDo you really want to continue?"
	    IfMsgBox Yes
		{
		  anti_notspam := true
		  return 1
		} else {
		  return 0
		}
	 }
   } else {
      return 1
   }
}
with_slot(part) {
   global current_slot
   static default_slot:="auto"
   default_slot := Trim(default_slot, "`n" A_Space "`t")
   if !(part ~= "_(a|b)$") {
      if (default_slot="disabled") {
         part := part
      } else if (default_slot="a") || (default_slot="b") {
	     if is_real(part "_" default_slot)
            part := part "_" default_slot
      } else if default_slot && (default_slot!="auto") && is_real(part default_slot) {
         part := part default_slot
      } else if current_slot && is_real(part current_slot) {
	     part := part current_slot
      }
   }
   return part
}
install_img(img, part, extra := "", extra2 := "") {
   global force_install
   global general_log
   global serial
   if (serial && !check_active(serial))
      return 0
   if (part="UPDATE FILE") {
      mode := "update " extra2 " """ img """"
   } else {
     part := with_slot(part)
	  mode := "flash " extra2 part " " """" img """"
	  if !is_real(part) {
         print(">> " part " partition does not exist!")
	     return 0
      }
	  if !xiaomeme_test(img) {
	     print(">> Aborted: Anti Roll Back")
	     return 0
	  }
   }
   print(">> Installing " part ": " basename(img))
   result := fastboot("--skip-reboot " extra " " mode " -s " serial)
   ; Evaluation of common erros
   if InStr(result, "error partition size") {
      write_file(result, general_log)
      print(">> Invalid partition size!")
      return 0
   } else if (result ~= "i)\berror|failed\b") {
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % result
	  return 0
   }
   return 1
}
ensure_fastboot() { 
   global device_mode, remember_mode, serial, default_mode, general_log
   if (serial && !check_active(serial))
      return 0
   if (device_mode!="fastboot") {
      /*
      if InStr(adb_shell("getprop | grep manufacturer"), "samsung") {
	     GuiControl, 1:Choose, default_mode, 2
	   }
      */
      print(">> Rebooting in fastboot...")
      (default_mode=1) ? mode:="bootloader" : mode:="fastboot"
      (ensure_shell||InStr(result:=adb_serial("reboot " . mode), "refused")) ? result .= adb_shell("reboot " . mode)
	   print(">> Waiting device")
   }
   find_device(20)
   if (device_mode="fastboot") {
      return 1
   } else {
      write_file(result, general_log)
      print(">> Oops! Cant reboot in fastboot")
	   MsgBox, % 262144 + 16, ERROR, Cant reboot in fastboot
      return 0
   }
}
reboot() {
   global exist_device, device_mode, remember_mode, general_log, serial, current, ensure_shell, fastbootd
   if (serial && !check_active(serial))
	   return
	if !exist_device {
	    MsgBox, % 262144 + 64, HELP, First find your device
	} else {
		print(">> Rebooting...")
		if (device_mode!="fastboot") {
		   (ensure_shell||InStr(adb_serial("reboot"), "refused")) ? adb_shell("reboot")
		} else {
		   fastboot_serial("reboot")
		}
      exist_device=
      device_mode=
		remember_mode = booted
	}
}
reboot_recovery() {
   global exist_device, device_mode, remember_mode, general_log, serial, current, ensure_shell
   if (serial && !check_active(serial))
	   return
	if !exist_device {
	    MsgBox, % 262144 + 64, HELP, First find your device
	    return
	} else {
		print(">> Rebooting in recovery...")
		if (device_mode!="fastboot") {
		   (ensure_shell||InStr(adb_serial("reboot recovery"), "refused")) ? adb_shell("reboot recovery")
		} else {
		   fastboot_serial("reboot recovery")
		}
      exist_device=
      device_mode=
		remember_mode = recovery
	}
}
reboot_fastboot() {
   global exist_device, device_mode, remember_mode, general_log, serial, current, ensure_shell, fastbootd
   if (serial && !check_active(serial))
	   return
	if !exist_device {
	  MsgBox, % 262144 + 64, HELP, First find your device
	  return
	} else {
		if (device_mode!="fastboot") {
		   ensure_fastboot()
		} else {
		   print(">> Rebooting in fastboot...")
		   (default_mode=1) ? fastboot_serial("reboot bootloader") : fastboot_serial("reboot fastboot")
		}
      exist_device=
      device_mode=
		find_device(20) ? (device_mode="fastboot") ? fastbootd ? print(">> Mode: fastbootd") : print(">> Mode: fastboot")
	}
}
ensure_sideload() {
   global exist_device, device_mode, remember_mode, general_log, serial, current
   while (device_mode!="sideload")
   {
      if (A_Index=1) {
         if (device_mode!="recovery") {
            reboot_recovery()
            print(">> Waiting for Recovery Mode")
            while (device_mode!="recovery") {
               (!(serial&&check_active(serial))) ? find_device(1)
            }
         }
         htmlmsg_img("HELP", "sideload.png", "Please start Sideload mode manually.")
         print(">> Waiting for Sideload Mode")
      }
      find_device(1,,,, false)
   }
}
format_data() {
   global device_mode, exist_device, serial, fastbootd
   if (serial && !check_active(serial))
      return 0
   print(">> Wiping userdata...")
   if (device_mode="fastboot") {
	   if fastbootd {
		  result := fastboot("-w" " -s " serial)
		  if (result ~= "i)\berror|failed\b") {
			 result := fastboot("erase userdata" " -s " serial)
		  }
	   } else {
		  result := fastboot("erase userdata" " -s " serial)
	   }
   } else {
      run_binary("twrp") && twrp := true
	  if twrp {
	     result := adb_shell("twrp format data || echo ERROR")
	  } else {
	     result := adb_shell("rm -rf /data/* || echo ERROR")
	  }
   }
   if (result ~= "i)\berror|failed\b") {
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % "A problem formatting the device storage!"
	  return 0
   }
   return 1
}
push(file, dest) {
   global exist_device, device_mode, general_log, ensure_recovery, serial
   if (serial && !check_active(serial))
      return 0
   if !exist_device
      return 0
   if !FileExist(file) {
      MsgBox, % 262144 + 16, ERROR, % "Cant find " file
      return 0
   }
   if (device_mode="fastboot") {
      reboot_recovery()
	   adb_shell("setprop sys.usb.config mtp,adb; setprop sys.usb.ffs.mtp.ready 1")
      ensure_shell()
   }
   result := adb_shell("[ ! -d " """" dest """" " ] && mkdir -p " """" dest """")
   print(">> Sending " basename(file) "...")
   ensure_shell()
   if (device_mode="recovery") {
      Sleep, 1000
      ensure_shell()
   }
   result .= adb_serial("push " """" file """" " " """" dest """")
   result .= adb_shell("[ ! -f " """" dest "/" basename(file) """" " ] && echo " """" "ERROR: Cant find " basename(file) """")
   if (result ~= "i)\berror|failed\b") {
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % "Unable to load " basename(file) " on the device"
	  return 0
   }
   return 1
}
pull(file, dest) {
   global exist_device, device_mode, general_log, ensure_recovery, serial
   if (serial && !check_active(serial))
      return 0
   if !exist_device
      return 0
   if (device_mode="fastboot") {
      reboot_recovery()
	   adb_shell("setprop sys.usb.config mtp,adb; setprop sys.usb.ffs.mtp.ready 1")
      ensure_shell()
   }
   if !exist_file(file) {
      MsgBox, % 262144 + 16, ERROR, % "Cant find " file " on the device"
      return 0
   }
   FileCreateDir(dest)
   print(">> Getting " basename(file) "...")
   ensure_shell()
   if (device_mode="recovery") {
      Sleep, 1000
      ensure_shell()
   }
   result .= adb_serial("pull " """" file """" " " """" dest """")
   if (result ~= "i)\berror|failed\b") || !FileExist(dest "\" basename(file)) {
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % "Unable to get " basename(file)
	  return 0
   }
   return 1
}
setup_busybox(to_dir, dest := "") {
   global busybox, general_log, busybox_work, device_mode, TMP
   busybox_work:=""
   if (device_mode="booted") {
      back_tmp:=TMP
      TMP:=ensure_tmp()
      if !push(busybox, TMP)
         return 0
      result := adb_shell("mkdir -p """ to_dir """ 2>/dev/null; mv -f " TMP "/" basename(busybox) " """ to_dir """")
      TMP:=back_tmp
   } else if !push(busybox, to_dir) {
      return 0
   }
   busybox_work := to_dir "/" basename(busybox)
   print(">> Busybox: Loading...")
   sbusybox=
   (
      if [ -f "%busybox_work%" ]; then
         chmod 777 "%busybox_work%"
         if [ -n "%dest%" ]; then
            rm -rf "%dest%"
            mkdir -p "%dest%"
            "%busybox_work%" --install -s "%dest%"
            [ ! -e "%dest%/unzip" ] && echo "ERROR: Cant setup busybox"
         else
            "%busybox_work%" unzip --help || echo "ERROR: Cant setup busybox"
         fi
      else
         echo "ERROR: Cant find busybox"
      fi
   )
   result.=sh(sbusybox)
   if (result ~= "i)\berror|failed\b") {
      busybox_work:=""
      write_file(result, general_log)
	   MsgBox, % 262144 + 16, ERROR, % "Cant setup " basename(busybox)
	   return 0
   }
   print(">> Busybox: Success")
   return 1
}
busybox(action) {
   global busybox_work
   if !busybox_work
      return 0
   result := adb_shell("""" busybox_work """" " " action)
   return result
}
exist_file(file) {
   ;Files, Folders, symlinks, ...
   global general_log
   result := adb_shell("[ -e " """" file """" " ] && echo File exist")
   if InStr(result, "File exist") {
      return 1
   } else {
      write_file(result, general_log)
      return 0
   }
}
exist_dir(dir) {
   global general_log
   result := adb_shell("[ -d " """" dir """" " ] && echo Dir exist")
   if InStr(result, "Dir exist") {
      return 1
   } else {
      write_file(result, general_log)
      return 0
   }
}
delete_file(file) {
   global general_log
   result .= adb_shell("rm -f " """" file """" " && echo Success")
   if InStr(result, "Success") {
      return 1
   } else {
      write_file(result, general_log)
      return 0
   }
}
delete_dir(dir) {
   global general_log
   result .= adb_shell("rm -rf " """" dir """" " && echo Success")
   if InStr(result, "Success") {
      return 1
   } else {
      write_file(result, general_log)
      return 0
   }
}
/*
Create_dir(dir) {
   global general_log
   result .= adb_shell("mkdir -p " """" dir """" " && echo Success")
   if InStr(result, "Success") {
      return 1
   } else {
      write_file(result, general_log)
      return 0
   }
}
copy(any, to) {
   global general_log
   if !exist_file(any) {
	   MsgBox, % 262144 + 16, ERROR, % "Cant find " any " on the device"
       return 0
   }
   result .= adb_shell("cp -rf " """" any """" " " """" to """" " && echo Success")
   if InStr(result, "Success") {
      return 1
   } else {
      MsgBox, % 262144 + 16, ERROR, % "Cant copy " any " in " to
      write_file(result, general_log)
      return 0
   }
}
*/
run_binary(binary, action := ""){
   global exist_device, device_mode, serial, general_log, cache
	if (serial && !check_active(serial))
      return 0
	if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The run_binary function is not supported from fastboot
	   return 0
	}
   if !IsObject(cache)
      optimize()
	if (cache.commands.HasKey(binary)&&at:=cache.commands[binary])||(at:=Trim(adb_shell("bin=$(command -v " binary "); [ -e ""$bin"" ] && echo $bin"),"`n`r" A_Space "`t")) {
      write_file("`n" Format("{:U}",at) " command support`n", general_log)
	} else {
      if action
	      MsgBox, % 262144 + 16, ERROR, % binary " command is not supported by device"
      return 0
	}
   return (action) ? adb_shell("""" at """ " action) : at
}
ensure_tmp(needexec := ""){
   global general_log, device_mode, TMP
   if needexec {
      if (TMP&&(TMP ~= "^/sdcard")) {
         result.="`nTMP: Unable to execute scripts in " TMP "`n"
      }
      using:="/tmp /dev/tmp__ /cache/tmp__ /mnt/tmp__ /data/tmp__ /data/local/tmp__"
   } else if (device_mode="booted") {
      if (TMP&&!(TMP ~= "^/sdcard")) {
         result .= "`nTMP: Unable to pass files to " TMP " because the device is booted"
      }
      using:="/sdcard/tmp"
   } else {
      using:="/tmp /dev/tmp__ /cache/tmp__ /mnt/tmp__ /data/tmp__ /data/local/tmp__"
   }
   stmp=
   (
      for TMP in %using%; do
            [ "$TMP" != "/tmp" ] && mkdir -p $TMP
            if [ -d "$TMP" ]; then
               echo "echo Im fine" > $TMP/tmp.sh
               chmod 777 $TMP/tmp.sh
               if [ -n "%needexec%" ]; then
                  if [ "$($TMP/tmp.sh)" = "Im fine" ]; then
                     rm -f $TMP/tmp.sh
                     echo "TMP == $TMP"
                     break
                  else
                     rm -rf $TMP
                     echo "TMP: $TMP UNSUPPORTED"
                  fi
               elif [ -f "$TMP/tmp.sh" ]; then
                  rm -f $TMP/tmp.sh
                  echo "TMP == $TMP"
                  break
               else
                  echo "TMP: $TMP is Read/Only"
               fi
            fi
      done
   )
   result.=TMP:=sh(stmp)
   if (_at:=InStr(TMP, "==")) {
      TMP:=Trim(SubStr(TMP, _at+2), "`n`r" A_Space "`t")
   } else {
      TMP:=""
		MsgBox, % 262144 + 16, ERROR, Cannot create a temp directory on the device
   }
   write_file(result, general_log)
   return TMP
}
flash_sideload_zip(zip) {
   global exist_device, device_mode, general_log, serial
   if (serial && !check_active(serial))
      return 0
   if !exist_device
      return 0
   if !FileExistFile(zip) {
      MsgBox, % 262144 + 16, ERROR, % "Cant find """ zip """"
      return 0
   }
   ensure_sideload()
   print(">> Installing " """" basename(zip) """")
   print(">> Please wait...")
   result:=adb_serial("sideload """ zip """")
   write_file(result, general_log)
   if (result ~= "i)error|failed") {
	   MsgBox, % 262144 + 16, ERROR, % result
	   return 0
   }
   return 1
}
flash_zip(zip) {
   global exist_device, device_mode, general_log, ensure_recovery, serial, busybox_work
   if (serial && !check_active(serial))
      return 0
   if !exist_device
      return 0
   if (ensure_recovery=1 && device_mode!="recovery")||(device_mode~="fastboot|sideload") {
      reboot_recovery()
	   adb_shell("setprop sys.usb.config mtp,adb; setprop sys.usb.ffs.mtp.ready 1")
      ensure_shell()
   }
   if !(TMP:=ensure_tmp(true))
      return 0
   if !exist_file(zip) {
      MsgBox, % 262144 + 16, ERROR, % "Cant find """ zip """ on the device"
      return 0
   }
   run_binary("twrp") && twrp := true
   run_binary("unzip") && unzip := true
   print(">> Installing " """" ubasename(zip) """")
   print(">> Please wait...")
   if twrp && unzip {
      result .= adb_shell("twrp install " """" zip """")
   } else {
      if !unzip && !(busybox_work||setup_busybox(TMP))
	     return 0
      print(">> Loading environment...")
      szip=
      (
         rm -rf "%TMP%/META-INF"
         if [ -n "%unzip%" ]; then
            unzip -qo "%zip%" META-INF/com/google/android/update-binary -d "%TMP%" 
         else
            "%busybox_work%" unzip -qo "%zip%" META-INF/com/google/android/update-binary -d "%TMP%" 
         fi
         if [ -f "%TMP%/META-INF/com/google/android/update-binary" ]; then
            sh "%TMP%/META-INF/com/google/android/update-binary" 3 3 "%zip%"
         else
            echo ERROR: Cant extract update-binary
         fi
      )
	   result .= "-------------" ubasename(zip) "-------------`n" 
	   result .= sh(szip)
	   result .= "------------------------------------------`n"
      if (result ~= "i)\berror:\b") {
	     write_file(result, general_log)
	     MsgBox, % 262144 + 16, ERROR, % "Cant execute " ubasename(zip)
	     return 0
	   }
   }
   write_file(result, general_log)
   return 1
}
flash_zip_push(zip) {
   global exist_device
   global device_mode
   global general_log
   global ensure_recovery
   global serial
   if (serial && !check_active(serial))
      return 0
   if !exist_device
      return 0
   if (ensure_recovery=1 && device_mode!="recovery")||(device_mode="fastboot") {
      reboot_recovery()
	   adb_shell("setprop sys.usb.config mtp,adb; setprop sys.usb.ffs.mtp.ready 1")
      ensure_shell()
   }
   TMP := ensure_tmp()
   if !TMP
      return 0
   if !push(zip, TMP)
      return 0
   if !flash_zip(TMP "/" basename(zip)) {
      return 0
   } else {
      delete_file(TMP "/" basename(zip))
	  return 1
   }
}
read_xml(url, time:=3000, hide:="") {
   global general_log
   ComObjError(false)
   get := ComObjCreate("WinHttp.WinHttpRequest.5.1")
   try {
      time ? get.SetTimeouts(time, time, time, time)
      get.Open("GET", url, false)
      get.Send()
      status := get.status
      if (status != 200) {
         if hide 
            url := ubasename(url)
         if !status
            status := "No internet connection"
         result := "`nUnable to read WEB content: """ url """, EXIT:" status "`n"
         write_file(result, general_log)
         return
      }
      result := get.responseText
      get.Quit
      return result
   } catch e {
      result := "`nError reading WEB content: """ e.message """`n"
      write_file(result, general_log)
      return
   }
}
web_config(url, time:=false, hide:="") {
   return load_config(read_xml(url,time,hide),,, FD_CURRENT)
}
get_hash(file, type){
   global general_log
   if (type = "MD5") {
		HashAlg:=1
   } else if (type = "MD2") {
		HashAlg:=2
   } else if (type = "SHA1") {
		HashAlg:=3
   } else if (type = "SHA256") {
		HashAlg:=4
   } else if (type = "SHA384") {
		HashAlg:=5
   } else if (type = "SHA512") {
		HashAlg:=6
   } else {
	  write_file("`nUnsupported hash type: " type "`n", general_log)
	  return
   }
   if !(hash := Crypt.Hash.FileHash(file,HashAlg,pwd:="",hmac_alg:=1)) {
      write_file("`nCant get hash of: " file "`n", general_log)
   } else {
      return hash
   }
}
download(url, to, option:="") {
   global general_log, adb, fastboot
   GetFullPathName(to) ? to := GetFullPathName(to)
   if RegExMatch(option, "\bMD2|MD5\b", hashtype) {
		checklen = 32
   } else if (option ~= "\bSHA1\b") {
        hashtype = SHA1
		checklen = 40
   } else if (option ~= "\bSHA256\b") {
        hashtype = SHA256
		checklen = 64
   } else if (option ~= "\bSHA384\b") {
        hashtype = SHA384
		checklen = 96
   } else if (option ~= "\bSHA512\b") {
        hashtype = SHA512
		checklen = 128
   }
   if (option ~= "i)\bforce\b") {
      force=true
   }
   if hashtype {
     RegExMatch(option, "\b[A-Fa-f0-9]{" checklen "}\b", hash)
	  if !hash {
	     MsgBox, % 262144 + 16, ERROR, % "No valid hash found for: " hashtype
		 return 0
	  }
   }
   bytes := HttpQueryInfo(url, 5)
   if bytes is not integer
   {
      write_file("`n" wbasename(to) " download ended with: " bytes "`n", general_log)
      MsgBox, % 262144 + 16, ERROR, % "Oops! no response from download server, try again later"
      return 0
   }
   if FileExist(to) {
      FileGetSize, currentbytes, % to
      if !force && (bytes=currentbytes) {
	     write_file("`n" basename(to) " already has " bytes " bytes, skipping...`n", general_log)
	     if hash {
		    if (hash=get_hash(to,hashtype)) {
			   write_file("`n" basename(to) " passed " hashtype " hash check, skipping...`n", general_log)
            return result
			} else {
			   write_file("`n" basename(to) " DID NOT pass " hashtype " hash check, removing...`n", general_log)
			   FileDelete, % to
			}
		 } else {
		    return 1
		 }
      } else {
	     FileDelete, % to
	  }
   }
   print(">> Downloading: """ wbasename(to)"""")
   SetTimer, progress, 250
   UrlDownloadToFile, % url, % to
   print()
   result := ErrorLevel
   SetTimer, progress, Off
   if result {
      MsgBox, % 262144 + 16, ERROR, % " Unable to download content: `n " url
	  result:=0
   } else {
      FileGetSize, currentbytes, % to
      if (bytes!=currentbytes) {
		 write_file("`n" basename(to) " has " currentbytes " of " bytes ", removing...`n", general_log)
		 FileDelete, % to
		 MsgBox, % 262144 + 16, ERROR, % """" wbasename(to) """ is corrupted (and was removed), please try again later "
		 result:=0
	  } else if hash {
	     if (hash=get_hash(to,hashtype)) {
			write_file("`n" basename(to) " passed " hashtype " hash check`n", general_log)
		 } else {
			write_file("`n" basename(to) " DID NOT pass " hashtype " hash check, removing...`n", general_log)
			FileDelete, % to
			MsgBox, % 262144 + 16, ERROR, % """" wbasename(to) """ did not pass the specified " hashtype " check and was removed"
			result:=0
	     }
      } else {
		 result:=1
	  }
   }
   return result
   progress:
      FileGetSize, currentbytes, % to
      If total := Round(currentbytes/bytes*100) {
         print( " " . total, false)
      }
   return
}
boot(img) {
   global exist_device
   global device_mode
   global general_log
   global serial
   if (serial && !check_active(serial))
      return 0
   if !exist_device
      return 0
   if !ensure_fastboot()
	  return 0
   if !FileExistFile(img) {
      unexpected := "Cant find " img
      return 0
   }
   print(">> Booting " basename(img))
   result := fastboot("boot " """" img """" " -s " serial)
   if (result ~= "i)\berror|failed\b") {
      write_file(result, general_log)
	  print(">> Cant boot " basename(img))
	  unexpected := "Some problem booting " basename(img)
	  return 0
   }
   adb_shell("setprop sys.usb.config mtp,adb; setprop sys.usb.ffs.mtp.ready 1")
   ensure_shell()
   return 1
}
update(img, dest, nofind := "") {
    global general_log
	global exist_device
    global device_mode
	global serial
	if (serial && !check_active(serial))
       return 0
	if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The update function is not supported from fastboot
	   return 0
	}
	run_binary("dd") && dd := true
	run_binary("cat") && cat := true
	if !dd && !cat {
	   MsgBox, % 262144 + 16, ERROR, update: No available actions found on the device
	   return 0 
	}
    if !exist_file(img) {
	   MsgBox, % 262144 + 16, ERROR, % "Cant find " img " on the device"
       return 0
	}
	if !nofind {
		dest := find_block(dest)
		if !dest {
		  print(">> Cant find dest partition")
		  return 0
		} 
	} else if !exist_file(dest) {
	   print(">> Invalid dest partition: " dest)
	   return 0
	}
   supdate=
   (
      if [ -n "%dd%" ] && dd if="%img%" of="%dest%"; then
         echo "%dest% updated successfully with dd"
      elif [ -n "%cat%" ] && cat "%img%" > "%dest%"; then
         echo "%dest% updated successfully with cat"
      else
         echo "ERROR: Cant install %img% in %dest%"
   )
   result:=sh(supdate)
   write_file(result, general_log)
	if InStr(result, "ERROR: ") {
	   print(">> Cant update " ubasename(img))
      MsgBox, % 262144 + 16, ERROR, % " Some problem updating " ubasename(img) " in " dest
	   return 0
	}
	return 1
}
update_push(img, dest, nofind := "") {
	TMP := ensure_tmp()
	if !TMP
	   return 0
    if !push(img, TMP)
       return 0
	if update(TMP "/" basename(img), dest, nofind) {
      adb_shell("rm -f """ TMP "/" basename(img) """")
	   return 1
	} else {
	   return 0
	}
}
get_cmdline(prop) {
   if !run_binary("cat") {
      MsgBox, % 262144 + 16, ERROR, get_cmdline: cat command not supported on device
	  return
   }
   get := Trim(adb_shell("temp=$(cat /proc/cmdline 2>/dev/null); try=${temp#*" prop "=}; if [ ""$temp"" = ""$try"" ]; then cat /proc/bootconfig 2>/dev/null; else echo ${try%% *}; fi"), "`n`r" A_Space "`t")
   if InStr(get, "`n") {
      get := RegExReplace(get, "(?:^\h*|.*\R\h*)\Q" prop "\E\h*=\h*(.*?)(?:\R|$).*", "$1", result)
      result ? get:=Trim(get, A_Space "`t") : get:=""
   }
   return get
}
get_slot() {
   global exist_device
   global device_mode
   global serial
   if (serial && !check_active(serial))
      return
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The get_slot function is not supported from fastboot
	   return
	}
   sslot=
   (
      temp=$(cat /proc/cmdline 2>/dev/null)
      for try in androidboot.slot_suffix androidboot.slot; do
         try=${temp#*$try=}
         if [ "$temp" != "$try" ]; then
            echo ${try`%`% *}
            exit
         fi
      done
      try=$(getprop ro.boot.slot_suffix 2>/dev/null)
      [ -n "$try" ] && echo $try || cat /proc/bootconfig 2>/dev/null 
   )
   slot := Trim(sh(sslot), "`n`r" A_Space "`t")
   if InStr(slot, "`n") {
      slot := RegExReplace(slot, "(?:^\h*|.*\R\h*)(?:\Qandroidboot.slot_suffix\E|\Qandroidboot.slot\E)\h*=\h*(.*?)(?:\R|$).*", "$1", result)
      (!result) ? slot:=""
   }
   if (slot:=Trim(slot, "`n`r" A_Space "`t")) && !InStr(slot, "_") {
      slot := "_" slot
   }
   return slot
}
find_block(name) {
   global exist_device, device_mode, serial, general_log, cache
   if (serial && !check_active(serial))
      return
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The find_block function is not supported from fastboot
	   return
	}
   if !IsObject(cache)
      optimize()
   if cache.parts.HasKey(name)
      return cache.parts[name]
   run_binary("find") && find := true
   slot := get_slot()
   if find {
	  try := adb_shell("if [ -e ""$(command -v head)"" ]; then part=$((find /dev/block \( -type b -o -type c -o -type l \) -iname " name " -o -iname " name slot " | head -n1) 2>/dev/null); elif [ -e ""$(command -v sed)"" ]; then part=$((find /dev/block \( -type b -o -type c -o -type l \) -iname " name " -o -iname " name slot " | sed -n ""1p;q"") 2>/dev/null); fi; [ -n ""$part"" -a -e ""$(command -v readlink)"" ] && echo $(readlink -f ""$part"") || echo $part", "", true)
	  try := Trim(try, "`n`r" A_Space "`t")
	  if try && exist_file(try) {
	     block := try
	  } else {
	     MsgBox, % 262144 + 16, ERROR, % "Cant find " name " partition"
	  }
   } else {
      MsgBox, % 262144 + 16, ERROR, find_block: find command not supported on device
	  return
   }
   return block
}
get_twrp_ramdisk(ramdisk) {
   global exist_device
   global device_mode
   global serial
   global general_log
   if (serial && !check_active(serial))
      return 0
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The get_twrp_ramdisk function is not supported from fastboot
	   return 0
   }
   run_binary("cpio") && cpio := true
   if !cpio {
	   MsgBox, % 262144 + 16, ERROR, get_twrp_ramdisk: No available actions found on the device: cpio
	   return 0 
   }
   if !exist_file("/ramdisk-files.txt") {
      MsgBox, % 262144 + 16, ERROR, Your current Recovery does not support building ramdisk A/B
	  return 0
   }
   result := run_binary("cpio", "-H newc -o < /ramdisk-files.txt > " """" ramdisk """")
   write_file(result, general_log)
   return 1
   ; Possible evalaluation of errors coming soon
}
update_ramdisk(ramdisk, part := ""){
   global exist_device
   global device_mode
   global serial
   global general_log
   if (serial && !check_active(serial))
      return 0
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The update_ramdisk function is not supported from fastboot
	   return 0
   }
   run_binary("magiskboot") && magiskboot := true
   if !magiskboot {
	   MsgBox, % 262144 + 16, ERROR, Your current Recovery does not support boot.img building: magiskboot is needed
	   return 0 
   }
   if !exist_file(ramdisk) {
	   MsgBox, % 262144 + 16, ERROR, % "Cant find " ramdisk " on the device"
       return 0
   }
   TMP := ensure_tmp()
   if !TMP
	  return 0
   TMP := TMP "/update_0001"
   if !part {
      print(">> Finding active boot partition")
      boot := find_block("boot")
   } else {
      print(">> Finding " part " partition")
      boot := find_block(part)
   }
   if !boot {
      print(">> Cant find dest partition")
	  return 0
   }
   boot_base := ubasename(boot)
   strymagisk=
   (
      if [ -f "%TMP%/kernel" ]; then
         MAGISK_ERROR=
         format=$(decompress kernel kernel_)
         [ -n "$format" ] && echo KERNEL with compression: $format || cp -f kernel kernel_
         rm -f kernel
         if [ -n "$NMAGISK_INSTALLED" ]; then
             magiskboot split kernel_
             if magiskboot hexpatch kernel_ 736B69705F696E697472616D6673 77616E745F696E697472616D6673; then
                if [ -n "$format" ]; then
                   magiskboot compress=$format kernel_ kernel
                else
                   cp -f kernel_ kernel
                fi
                echo MAGISK restored successfully in KERNEL
             else
                echo Kernel seems to need no changes for MAGISK, keeping stock kernel
                rm -f kernel
                rm -f kernel_
             fi
             if [ -f .magisk ]; then
                export $(cat .magisk)
                for fstab in dtb extra kernel_dtb recovery_dtbo; do
                    [ -f $fstab ] && magiskboot dtb $fstab patch
                done
             fi
         elif [ -n "$MAGISK_INSTALLED" ]; then
            if magiskboot hexpatch kernel_ 77616E745F696E697472616D6673 736B69705F696E697472616D6673; then
               if [ -n "$format" ]; then
                  magiskboot compress=$format kernel_ kernel
               else
                  cp -f kernel_ kernel
               fi
               echo MAGISK removed successfully in KERNEL
            else
               MAGISK_ERROR=true
               echo ERROR: Cant remove MAGISK from kernel
            fi
            
         else
            echo MAGISK not detected, keeping stock kernel
            rm -f kernel
            rm -f kernel_
         fi
         format=
      fi
   )
   sramdisk=
   (
      decompress() {
         try=$(magiskboot decompress "$1" "$2" 2>&1)
         f=${try#*format: [}
         f=${f`%`%]*}
         [ "$f" != "$try" -a "$f" != "raw" ] && echo $f
      }
      rm -rf "%TMP%"
      mkdir -p "%TMP%"
      if [ -d "%TMP%" ]; then
         cd "%TMP%" && magiskboot unpack -h -n "%boot%"
         if [ -f "%TMP%/ramdisk.cpio" ]; then
            format=$(decompress ramdisk.cpio ramdisk_.cpio)
            if [ -n "$format" ]; then
               magiskboot cpio ramdisk_.cpio test
               STATE=$?
            else
               magiskboot cpio ramdisk.cpio test
               STATE=$?
            fi
            [ $STATE = 1 ] && MAGISK_INSTALLED=true
            rm -f ramdisk.cpio
            rm -f ramdisk_.cpio
            if [ -n "$format" ]; then
               echo Ramdisk with compression: $format
               format2=$(decompress "%ramdisk%" ramdisk.cpio)
               if [ -n "$format2" ]; then
                  magiskboot cpio ramdisk.cpio test
                  NSTATE=$?
                  [ $NSTATE = 1 ] && magiskboot cpio ramdisk.cpio "extract .backup/.magisk .magisk"
                  magiskboot compress=$format ramdisk.cpio ramdisk_.cpio
               else
                  magiskboot cpio "%ramdisk%" test
                  NSTATE=$?
                  [ $NSTATE = 1 ] && magiskboot cpio "%ramdisk%" "extract .backup/.magisk .magisk"
                  magiskboot compress=$format "%ramdisk%" ramdisk_.cpio
               fi
            else
               format2=$(decompress "%ramdisk%" ramdisk_.cpio)
               [ -z "$format2" ] && cp -f "%ramdisk%" ramdisk_.cpio
               magiskboot cpio ramdisk_.cpio test
               NSTATE=$?
               [ $NSTATE = 1 ] && magiskboot cpio ramdisk_.cpio "extract .backup/.magisk .magisk"
            fi
            [ $NSTATE = 1 ] && NMAGISK_INSTALLED=true
            rm -f ramdisk.cpio
            cp -f ramdisk_.cpio ramdisk.cpio

            %strymagisk%

            [ -n "$MAGISK_ERROR" ] && exit
            if [ -f ramdisk.cpio ]; then
               magiskboot repack "%boot%"
               if [ -f new-boot.img ]; then
                  if dd if=new-boot.img of="%boot%" || cat new-boot.img > "%boot%"; then
                     echo Ramdisk updated successfully
                  else
                     echo "ERROR: Cant install boot.img in %boot%"
                  fi
               else
                  echo ERROR: Some problem building new boot.img
               fi
            else
               echo ERROR: Some problem updating new ramdisk
            fi
         else
            echo "ERROR: Some problem unpacking %boot_base%"
         fi
      else
         echo "ERROR: Cant create %TMP%"
      fi
   )
   print(">> Updating ramdisk: " boot_base)
   result:=sh(sramdisk)
   write_file(result, general_log)
   if InStr(result, "ERROR: ") {
	   MsgBox, % 262144 + 16, ERROR, Cant update ramdisk in %boot%
	   return 0
   }
   return 1
}
update_ramdisk_push(ramdisk, part := "") {
    global exist_device
    global device_mode
    global serial
    global general_log
    if (serial && !check_active(serial))
       return 0
    if (device_mode="fastboot") {
	    MsgBox, % 262144 + 16, ERROR, The update_ramdisk_push function is not supported from fastboot
	    return 0
    }
	TMP := ensure_tmp()
	if !TMP
	   return 0
    if !push(ramdisk, TMP)
       return 0
	if update_ramdisk(TMP "/" basename(ramdisk), part) {
	   if exist_file(TMP "/" basename(ramdisk))
	      delete_file(TMP "/" basename(ramdisk))
	   return 1
	} else {
	   return 0
	}
}
update_kernel(kernel, part := ""){
   global exist_device
   global device_mode
   global serial
   global general_log
   if (serial && !check_active(serial))
      return 0
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The update_ramdisk function is not supported from fastboot
	   return 0
   }
   run_binary("magiskboot") && magiskboot := true
   if !magiskboot {
	   MsgBox, % 262144 + 16, ERROR, Your current Recovery does not support kernel updating
	   return 0 
   }
   if !exist_file(kernel) {
	   MsgBox, % 262144 + 16, ERROR, % "Cant find " kernel " on the device"
       return 0
   }
   TMP := ensure_tmp()
   if !TMP
	  return 0
   TMP := TMP "/update_0001"
   if !part {
      print(">> Finding active boot partition")
      boot := find_block("boot")
   } else {
      print(">> Finding " part " partition")
      boot := find_block(part)
   }
   if !boot {
      print(">> Cant find dest partition")
	  return 0
   }
   boot_base := ubasename(boot)
   skernel=
   (
      decompress() {
         try=$(magiskboot decompress "$1" "$2" 2>&1)
         f=${try#*format: [}
         f=${f`%`%]*}
         [ "$f" != "$try" -a "$f" != "raw" ] && echo $f
      }
      rm -rf "%TMP%"
      mkdir -p "%TMP%"
      if [ -d "%TMP%" ]; then
         cd "%TMP%" && magiskboot unpack -h -n "%boot%"
         if [ -f "%TMP%/kernel" ]; then
            if [ -f "%TMP%/ramdisk.cpio" ]; then
               format=$(decompress ramdisk.cpio ramdisk_.cpio)
               if [ -n "$format" ]; then
                  magiskboot cpio ramdisk_.cpio test
                  STATE=$?
                  [ $STATE = 1 ] && magiskboot cpio ramdisk_.cpio "extract .backup/.magisk .magisk"
               else
                  magiskboot cpio ramdisk.cpio test
                  STATE=$?
                  [ $STATE = 1 ] && magiskboot cpio ramdisk.cpio "extract .backup/.magisk .magisk"
               fi
               [ $STATE = 1 ] && MAGISK_INSTALLED=true
               rm -f ramdisk.cpio
               rm -f ramdisk_.cpio
               format=
            fi
            format=$(decompress kernel kernel_)
            rm -f kernel
            rm -f kernel_    
            format2=$(decompress "%kernel%" kernel_)
            [ -z "$format2" ] && cp -f "%kernel%" kernel_
            if [ -n "$MAGISK_INSTALLED" ]; then
               magiskboot split kernel_
               if magiskboot hexpatch kernel_ 736B69705F696E697472616D6673 77616E745F696E697472616D6673; then
                  echo MAGISK restored successfully in NEW KERNEL
               else
                  echo NEW KERNEL seems to need no changes for MAGISK
               fi
               if [ -f .magisk ]; then
                  export $(cat .magisk)
                  for fstab in dtb extra kernel_dtb recovery_dtbo; do
                     [ -f $fstab ] && magiskboot dtb $fstab patch
                  done
               fi
            else
               echo MAGISK not detected, no additional patches required
            fi
            if [ -n "$format" ]; then
               echo KERNEL with compression: $format
               magiskboot compress=$format kernel_ kernel
            else
               cp -f kernel_ kernel
            fi
            if [ -f kernel ]; then
               magiskboot repack "%boot%"
               if [ -f new-boot.img ]; then
                  if dd if=new-boot.img of="%boot%" || cat new-boot.img > "%boot%"; then
                     echo Kernel updated successfully
                  else
                     echo "ERROR: Cant install boot.img in %boot%"
                  fi
               else
                  echo ERROR: Some problem building new boot.img
               fi
            else
               echo ERROR: Some problem updating new kernel
            fi
         else
            echo "ERROR: Some problem unpacking %boot_base%"
         fi
      else
         echo "ERROR: Cant create %TMP%"
      fi
   )
   print(">> Updating kernel: " boot_base)
   result:=sh(skernel)
   write_file(result, general_log)
   if InStr(result, "ERROR: ") {
	   MsgBox, % 262144 + 16, ERROR, Cant update kernel in %boot%
	   return 0
   }
   return 1
}
update_kernel_push(kernel, part := "") {
    global exist_device
    global device_mode
    global serial
    global general_log
    if (serial && !check_active(serial))
       return 0
    if (device_mode="fastboot") {
	    MsgBox, % 262144 + 16, ERROR, The update_kernel_push function is not supported from fastboot
	    return 0
    }
	TMP := ensure_tmp()
	if !TMP
	   return 0
    if !push(kernel, TMP)
       return 0
	if update_kernel(TMP "/" basename(kernel), part) {
	   if exist_file(TMP "/" basename(kernel))
	      delete_file(TMP "/" basename(kernel))
	   return 1
	} else {
	   return 0
	}
}
install_recovery_ramdisk(part := "") {
    global exist_device
    global device_mode
    global serial
    global general_log
    if (serial && !check_active(serial))
       return 0
    if (device_mode="fastboot") {
	    MsgBox, % 262144 + 16, ERROR, The install_recovery_ramdisk function is not supported from fastboot
	    return 0
    }
   TMP := ensure_tmp()
   if !TMP
	   return 0
   if !get_twrp_ramdisk(TMP "/twrp_.cpio")
	   return 0
   if !part {
      if !update_ramdisk(TMP "/twrp_.cpio")
	     return 0
   } else {
      if !update_ramdisk(TMP "/twrp_.cpio", part)
	     return 0
   }
   return 1
}
decompress(file, dest := ""){
   global exist_device
   global device_mode
   global serial 
   global general_log
   if (serial && !check_active(serial))
      return
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The decompress function is not supported from fastboot
	   return
   }
   run_binary("magiskboot") && magiskboot := true
   if !magiskboot {
	   MsgBox, % 262144 + 16, ERROR, Cant find magiskboot
	   return
   }
   if !exist_file(file) {
	   MsgBox, % 262144 + 16, ERROR, % "Cant find " file " on the device"
       return
   }
   base := "magiskboot decompress " """" file """"
   if dest
      base .= " " """" dest """"
   try := adb_shell(base)
   if !(try ~= "\braw\b"){
      format := Trim(RegExReplace(try, "(?:^|.*\R).*format: \[(.+?(?=\])).*", "$1",result), A_Space)
      (!result) ? format:=""
   }
   return format
}
recompress(file, format, dest := ""){
   global exist_device
   global device_mode
   global serial 
   global general_log
   if (serial && !check_active(serial))
      return 0
   if (device_mode="fastboot") {
	   MsgBox, % 262144 + 16, ERROR, The recompress function is not supported from fastboot
	   return 0
   }
   run_binary("magiskboot") && magiskboot := true
   if !magiskboot {
	   MsgBox, % 262144 + 16, ERROR, Cant find magiskboot
	   return 0
   }
   if !exist_file(file) {
	   MsgBox, % 262144 + 16, ERROR, % "Cant find " file " on the device"
       return 0
   }
   base := "magiskboot compress=" format " " """" file """"
   if dest
      base .= " " """" dest """"
   base .= " && echo Success"
   result := adb_shell(base)
   if !InStr(result, "Success"){
      write_file(result, general_log)
      MsgBox, % 262144 + 16, ERROR, % "Some problem compressing " file " as " format
	  return 0
   }
   return 1
}
save(value, with, in) {
   global HERE, TOOL, secure_user_info, general_log
   in := GetFullPathName(in)
   (!(value ~= "^[a-zA-Z_$][a-zA-Z0-9_$]*$")) ? unexpected := "Invalid variable name--->" value : InStr(with, "`n") ? unexpected := "Only single line preferences are allowed to be saved"
   if unexpected
      return 0
   if (secure_user_info && !(in ~= "^((\Q" HERE "\E)|(\Q" TOOL "\E))")) {
	  MsgBox, 262148, Save preferences, % " Attempting to save a file outside of common paths:`n`n " in "`n`n Do you want to allow it?"
	  IfMsgBox No
	  {
	     return 0
	  }
   }
   try {
      IniWrite, % with, % in, CUSTOM, % value
   } catch {
	  write_file("There was a problem saving the preference: " value "=" with, general_log)
	  return 0
   }
   return 1
}
gotolink(link) {
   if (link ~= "i)^(?:\b[a-z\d.-]+:\/\/[^<>\s]+|\b(?:(?:(?:[^\s!@#$%^&*()_=+[\]{}\|`;:'"",.<>/?]+)\.)+(?:ac|ad|aero|ae|af|ag|ai|al|am|an|ao|aq|arpa|ar|asia|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|biz|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|cat|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|coop|com|co|cr|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|edu|ee|eg|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gov|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|info|int|in|io|iq|ir|is|it|je|jm|jobs|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mil|mk|ml|mm|mn|mobi|mo|mp|mq|mr|ms|mt|museum|mu|mv|mw|mx|my|mz|name|na|nc|net|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|org|pa|pe|pf|pg|ph|pk|pl|pm|pn|pro|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tel|tf|tg|th|tj|tk|tl|tm|tn|to|tp|travel|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|xn--0zwm56d|xn--11b5bs3a9aj6g|xn--80akhbyknj4f|xn--9t4b11yi5a|xn--deba0ad|xn--g6w251d|xn--hgbk6aj7f53bba|xn--hlcj6aya9esc7a|xn--jxalpdlp|xn--kgbechtv|xn--zckzah|ye|yt|yu|za|zm|zw)|(?:(?:[0-9]|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.){3}(?:[0-9]|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5]))(?:[`;/][^#?<>\s]*)?(?:\?[^#<>\s]*)?(?:#[^<>\s]*)?(?!\w))$")    
   {
	  try {
		 Run, % link
	  } catch {
		 MsgBox, % 262144 + 16, ERROR,  % "Cant open link: `n`n" link
		 return 0
	  }
   } else {
      unexpected := "Invalid URL: " link
	  return 0
   }
   return 1
}
getsaved(file) {
   if FileExistFile(file) {
	  if RegexMatch(read_file(file), "is)(\[CUSTOM\]\K.*?)(?=(\R$)|\[)", section) {
		 Loop, parse, section, `n, `r
		 {
			if A_LoopField && (_pos := InStr(A_LoopField, "=")) {
				try
				   GLOBAL[SubStr(A_LoopField, 1, _pos - 1)] := SubStr(A_LoopField, _pos + 1)
				catch
				   return 0
			}
		 }
	  }
   } else {
       unexpected := "Cant find: " file
	   return 0
   }
   return 1
}
/*
delete(part) {
   global custom_parts
   try {
      custom_parts.Push({part:part, to_remove:true})
   } catch {
      unexpected := "Cant append partition: " part
      return 0
   }
   return 1
}
create(part, with) {
   global custom_parts
   if !(size := to_bytes(with)) {
	   unexpected := "Invalid SIZE: " with
	   return 0
   }
	try {
		custom_parts.Push({part:part, size:size})
	} catch {
       unexpected := "Cant append partition: " part
	    return 0
	}
	return 1
}
*/
install(file:="", in:="", with:="", on:="") {
   global install_files
   (!IsObject(install_files)) ? install_files:=[]
   if !(on&&!file) {
      if !FileExistFile(file) {
         unexpected := "Cant find file: " file
         return 0
      }
   }
   in:=Trim(in)
   (in="ZIP FILE") ? is_zip:=true
   (in="SIDELOAD FILE") ? is_sdl_zip:=true
   (in="RAMDISK FILE") ? is_ramdisk:=true
	try {
      if on {
         (file) ? install_files[on].file:=file
         (in) ? install_files[on].part:=in
         (is_zip||is_ramdisk||is_sdl_zip) ? (install_files[on].is_zip:=is_zip, install_files[on].is_ramdisk:=is_ramdisk, install_files[on].is_sdl_zip:=is_sdl_zip)
      } else {
         install_files.Push({file:file, part:in, extra:Trim(with), is_zip:is_zip, is_ramdisk:is_ramdisk, is_sdl_zip:is_sdl_zip, install:true})
      }
	} catch {
      unexpected := "Cant append file: " file
      return 0
	}
	return 1
}
wipe_env(reset := false) {
   global install_files
   FD_CURRENT ? load_config(,,,,,true)
   install_files := [], custom_parts := []
}
; Msgbox HTML based
htmlmsg_img(params*) {
   img:=params.RemoveAt(2)
   return htmlmsg(params*)
}
htmlmsg(title:="", content:="", w:="auto", h:="auto", append_container:="") {
   MsgBox, 262144, % title, % content
}