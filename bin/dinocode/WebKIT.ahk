;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HttpQueryInfo Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Source: post by olfen "DllCall: HttpQueryInfo - Get HTTP headers"
;                       http://www.autohotkey.com/forum/post-64567.html#64567
;
; For flag info, see: http://msdn.microsoft.com/en-us/library/aa385351(VS.85).aspx

HttpQueryInfo(URL, QueryInfoFlag=21, Proxy="", ProxyBypass="") {
hModule := DllCall("LoadLibrary", "str", dll := "wininet.dll")

; Adapt for build by 0x150||ISO
ver := ( A_IsUnicode && !RegExMatch( A_AhkVersion, "\d+\.\d+\.4" ) ? "W" : "A" )
InternetOpen := dll "\InternetOpen" ver
HttpQueryInfo := dll "\HttpQueryInfo" ver
InternetOpenUrl := dll "\InternetOpenUrl" ver

If (Proxy != "")
AccessType=3
Else
AccessType=1

io_hInternet := DllCall( InternetOpen
, "str", "" 
, "uint", AccessType
, "str", Proxy
, "str", ProxyBypass
, "uint", 0) ;dwFlags
If (ErrorLevel != 0 or io_hInternet = 0) {
DllCall("FreeLibrary", "uint", hModule)
return, -1
}

iou_hInternet := DllCall( InternetOpenUrl
, "uint", io_hInternet
, "str", url
, "str", ""
, "uint", 0
, "uint", 0x80000000
, "uint", 0)
If (ErrorLevel != 0 or iou_hInternet = 0) {
DllCall("FreeLibrary", "uint", hModule)
return, -1
}

VarSetCapacity(buffer, 1024, 0)
VarSetCapacity(buffer_len, 4, 0)

Loop, 5
{
  hqi := DllCall( HttpQueryInfo
  , "uint", iou_hInternet
  , "uint", QueryInfoFlag
  , "uint", &buffer
  , "uint", &buffer_len
  , "uint", 0)
  If (hqi = 1) {
    hqi=success
    break
  }
}

IfNotEqual, hqi, success, SetEnv, res, timeout

If (hqi = "success") {
p := &buffer
Loop
{
  l := DllCall("lstrlen", "UInt", p)
  VarSetCapacity(tmp_var, l+1, 0)
  DllCall("lstrcpy", "Str", tmp_var, "UInt", p)
  p += l + 1 
  res := res . tmp_var
  If (*p = 0)
  Break
}
}

DllCall("wininet\InternetCloseHandle",  "uint", iou_hInternet)
DllCall("wininet\InternetCloseHandle",  "uint", io_hInternet)
DllCall("FreeLibrary", "uint", hModule)

return, res
}

;-------------------------------------------------------------------------
; JSON for AutoHotkey
; Copyright (c) 2018-2022 Kurt McKee <contactme@kurtmckee.org>
; The code is licensed under the terms of the MIT license.
; https://github.com/kurtmckee/ahk_json

; VERSION = "2.0"


json_escape(blob)
{
    hexadecimal := "0123456789abcdef"

    escapes := {}
    escapes["`b"] := "\b"
    escapes["`f"] := "\f"
    escapes["`n"] := "\n"
    escapes["`r"] := "\r"
    escapes["`t"] := "\t"
    escapes["/"] := "\/"
    escapes["\"] := "\\"
    escapes[""""] := "\"""


    loop, % strlen(blob)
    {
        character := substr(blob, a_index, 1)
        value := ord(character)

        ; Use simple escapes for reserved characters.
        if (instr("`b`f`n`r`t/\""", character))
        {
            escaped_blob .= escapes[character]
        }

        ; Allow ASCII characters through without modification.
        else if (value >= 32 and value <= 126)
        {
            escaped_blob .= character
        }

        ; Use Unicode escapes for everything else.
        else
        {
            hex1 := substr(hexadecimal, ((value & 0xF000) >> 12) + 1, 1)
            hex2 := substr(hexadecimal, ((value & 0xF00) >> 8) + 1, 1)
            hex3 := substr(hexadecimal, ((value & 0xF0) >> 4) + 1, 1)
            hex4 := substr(hexadecimal, ((value & 0xF) >> 0) + 1, 1)
            escaped_blob .= "\u" . hex1 . hex2 . hex3 . hex4
        }
    }

    return escaped_blob
}


json_unescape(blob)
{
    escapes := {}
    escapes["b"] := "`b"
    escapes["f"] := "`f"
    escapes["n"] := "`n"
    escapes["r"] := "`r"
    escapes["t"] := "`t"
    escapes["/"] := "/"
    escapes["\"] := "\"
    escapes[""""] := """"


    index := 1
    loop
    {
        if (index > strlen(blob))
        {
            break
        }

        character := substr(blob, index, 1)
        next_character := substr(blob, index + 1, 1)
        if (character != "\")
        {
            unescaped_blob .= character
        }
        else if (instr("bfnrt/\""", next_character))
        {
            unescaped_blob .= escapes[next_character]
            index += 1
        }
        else if (next_character == "u")
        {
            unicode_character := chr("0x" . substr(blob, index + 2, 4))
            unescaped_blob .= unicode_character
            index += 5
        }

        index += 1
    }

    return unescaped_blob
}


json_get_object_type(object)
{
    if (not isobject(object))
    {
        return "string"
    }

    ; Identify the object type and return either "dict" or "list".
    object_type := "list"
    if (object.length() == 0)
    {
        object_type := "dict"
    }
    for key in object
    {
        ; The current AutoHotkey list implementation will loop through its
        ; indexes in order from least to greatest. If the object can be
        ; represented as a list, each key will match the a_index variable.
        ; However, if it is a sparse list (that is, if it has non-consective
        ; list indexes) then it must be represented as a dict.
        if (key != a_index)
        {
            object_type := "dict"
        }
    }

    return object_type
}


json_dump(info)
{
    ; Differentiate between a list and a dictionary.
    object_type := json_get_object_type(info)

    if (object_type == "string")
    {
        return """" . json_escape(info) . """"
    }
    else if (object_type == "number")
    {
        return info
    }

    for key, value in info
    {
        ; Only include a key if this is a dictionary.
        if (object_type == "dict")
        {
            escaped_key := json_escape(key)
            blob .= """" . escaped_key . """: "
        }

        if (isobject(value))
        {
            blob .= json_dump(value) . ", "
        }
        else
        {
            escaped_value := json_escape(value)
            blob .= """" . escaped_value . """, "
        }
    }

    ; Remove the final trailing comma.
    if (substr(blob, -1, 2) == ", ")
    {
        blob := substr(blob, 1, -2)
    }

    ; Wrap the string in brackets or braces, as appropriate.
    if (object_type == "list")
    {
        blob := "[" . blob . "]"
    }
    else
    {
        blob := "{" . blob . "}"
    }

    return blob
}


extract_key(blob, index_left)
{
    index_right := index_left + 1
    loop, % (strlen(blob) - index_left + 1)
    {
        if (substr(blob, index_right, 1) == "\")
        {
            index_right += 1
        }
        else if (substr(blob, index_right, 1) == """")
        {
            break
        }
        else if (index_right > strlen(blob))
        {
            throw "UNCLOSED_STRING: No closing quotation mark found"
        }
        index_right += 1
    }

    result := {}
    result["index_left"] := index_left
    result["index_right"] := index_right
    result["value_type"] := "str"
    ; Exclude quotation marks from the value.
    result["value"] := json_unescape(substr(blob, index_left + 1, index_right - index_left - 1))
    return result
}


extract_value(blob, index_left)
{
    ; Burn through whitespace.
    loop
    {
        if (instr("`b`f`n`r`t ", substr(blob, index_left, 1)))
        {
            index_left += 1
        }
        else
        {
            break
        }
    }

    ; Booleans
    if (substr(blob, index_left, 4) == "true")
    {
        result := {}
        result["index_left"] := index_left
        result["index_right"] := index_left + 3
        result["value_type"] := "bool"
        result["value"] := true
        return result
    }
    if (substr(blob, index_left, 5) == "false")
    {
        result := {}
        result["index_left"] := index_left
        result["index_right"] := index_left + 4
        result["value_type"] := "bool"
        result["value"] := false
        return result
    }

    ; Null
    if (substr(blob, index_left, 4) == "null")
    {
        result := {}
        result["index_left"] := index_left
        result["index_right"] := index_left + 3
        result["value_type"] := "null"
        result["value"] := false
        return result
    }

    ; Numbers
    if (instr("-0123456789", substr(blob, index_left, 1)))
    {
        index_right := index_left
        loop, % (strlen(blob) - index_left + 1)
        {
            index_right += 1
            if (not instr("0123456789eE-+.", substr(blob, index_right, 1)))
            {
                break
            }
        }

        result := {}
        result["index_left"] := index_left
        result["index_right"] := index_right - 1
        result["value_type"] := "number"
        result["value"] := substr(blob, index_left, index_right - index_left)
        return result
    }

    ; Strings
    if (substr(blob, index_left, 1) == """")
    {
        return extract_key(blob, index_left)
    }


    ; Arrays
    if (substr(blob, index_left, 1) == "[")
    {
        original_index_left := index_left
        index_left += 1
        array_values := []
        loop
        {
            ; Burn through non-value characters.
            loop
            {
                if (index_left > strlen(blob))
                {
                    throw "UNCLOSED_ARRAY: No closing bracket was found!"
                }
                else if (instr("`b`f`n`r`t ", substr(blob, index_left, 1)))
                {
                    index_left += 1
                }
                else if (substr(blob, index_left, 1) == ",")
                {
                    index_left += 1
                    break
                }
                else if (substr(blob, index_left, 1) == "]")
                {
                    ; "]" marks the end of the array.
                    break, 2
                }
                else
                {
                    ; Do not increment index_left. This is the start of a value.
                    break
                }
            }

            array_item := extract_value(blob, index_left)
            index_left := array_item["index_right"] + 1
            array_values.push(array_item["value"])
        }

        result := {}
        result["index_left"] := original_index_left
        result["index_right"] := index_left
        result["value_type"] := "array"
        result["value"] := array_values
        return result
    }

    ; Objects
    if (substr(blob, index_left, 1) == "{")
    {
        original_index_left := index_left
        index_left += 1
        object_contents := {}
        loop
        {
            ; Burn through non-key characters.
            loop
            {
                if (index_left > strlen(blob))
                {
                    throw "UNCLOSED_OBJECT: No closing brace was found!"
                }
                else if (instr("`b`f`n`r`t ,", substr(blob, index_left, 1)))
                {
                    index_left += 1
                }
                else if (substr(blob, index_left, 1) == """")
                {
                    ; Do not increment index_left. This is the start of a key.
                    break
                }
                else if (substr(blob, index_left, 1) == "}")
                {
                    ; "}" marks the end of the object.
                    break, 2
                }
                else
                {
                    throw "UNRECOGNIZED_TEXT: Unrecognized character at " . index_left
                }
            }

            object_key := extract_key(blob, index_left)
            index_left := object_key["index_right"] + 1

            ; Burn through non-value characters.
            loop
            {
                if (index_left > strlen(blob))
                {
                    throw "UNCLOSED_OBJECT: No closing brace was found!"
                }
                else if (instr("`b`f`n`r`t ", substr(blob, index_left, 1)))
                {
                    index_left += 1
                }
                else if (substr(blob, index_left, 1) == ":")
                {
                    ; ":" separates keys from values.
                    ; Increment one more time, then exit the loop.
                    index_left += 1
                    break
                }
                else
                {
                    throw "UNRECOGNIZED_TEXT: Unrecognized character or value"
                }
            }

            object_value := extract_value(blob, index_left)
            index_left := object_value["index_right"] + 1

            object_contents[object_key["value"]] := object_value["value"]
        }

        result := {}
        result["index_left"] := original_index_left
        result["index_right"] := index_left
        result["value_type"] := "object"
        result["value"] := object_contents
        return result
    }

    throw "UNRECOGNIZED_TEXT: Unrecognized character or value"
}

LoadJSON(blob)
{
    blob_length := strlen(blob)
    index_left := 0
    index_right := 0

    if !blob_length
        return

    value := extract_value(blob, 1)

    ; Confirm there is no remaining text.
    loop, % (strlen(blob) - value["index_right"])
    {
        if (not instr("`b`f`n`r`t ", substr(blob, value["index_right"] + a_index, 1)))
        {
            throw % "INVALID_JSON: Trailing character at position " . (value["index_right"] + a_index)
        }
    }

    return value["value"]
}

; By Coco and Getfree --> https://www.autohotkey.com/board/topic/95262-obj-json-obj/
BuildJson(obj) 
{
    str := "" , array := true
    for k in obj {
        if (k == A_Index)
            continue
        array := false
        break
    }
    for a, b in obj
        str .= (array ? "" : """" a """: ") . (IsObject(b) ? BuildJson(b) : IsNumber(b) ? b : """" b """") . ", "	
    str := RTrim(str, " ,")
    return (array ? "[" str "]" : "{" str "}")
}

;-------------------------------------------------------------------------
; Extra by @BlassGO
;-------------------------------------------------------------------------
WebCode(url, ResolveTimeout:=0, ConnectTimeout:=60000, SendTimeout:=30000, ReceiveTimeout:=30000) {
   return load_config(responseText(url,ResolveTimeout, ConnectTimeout, SendTimeout, ReceiveTimeout),,, FD_CURRENT)
}
XMLParser(str) {
    try {
    ComObjError(false),xml:=ComObjCreate("MSXML2.DOMDocument.6.0"), xml.async:=false
    return xml.loadXML(str)?xml:(xml:="")
    } catch e
    xml:="",unexpected:="Error converting XML to an Object -> " e.message
}
ResponseText(url, ResolveTimeout:=0, ConnectTimeout:=60000, SendTimeout:=30000, ReceiveTimeout:=30000) {
    ComObjError(false),httpRequest:=ComObjCreate("WinHttp.WinHttpRequest.5.1")
    try {
        httpRequest.SetTimeouts(ResolveTimeout, ConnectTimeout, SendTimeout, ReceiveTimeout)
        httpRequest.Open("GET", url, false)
        httpRequest.Send()
        status:=httpRequest.status
        if (status!=200)
        unexpected:="Unable to read WEB content -> " (status ? "HTTP_Error[" status "]" : "No internet connection")
        return httpRequest.ResponseText, httpRequest:=""
    } catch e {
        httpRequest:="", unexpected:="Error reading WEB content -> " e.message
    }
}
ResponseXML(url, ResolveTimeout:=0, ConnectTimeout:=60000, SendTimeout:=30000, ReceiveTimeout:=30000) {
    PlainXML:=responseText(url,ResolveTimeout, ConnectTimeout, SendTimeout, ReceiveTimeout)
    return unexpected?"":XMLParser(PlainXML)
}
ResponseJSON(url, ResolveTimeout:=0, ConnectTimeout:=60000, SendTimeout:=30000, ReceiveTimeout:=30000) {
    PlainJSON:=responseText(url,ResolveTimeout, ConnectTimeout, SendTimeout, ReceiveTimeout)
    return unexpected?"":LoadJSON(PlainJSON)
}
UrlDownloadToFile(url,to) {
    try UrlDownloadToFile, % url, % to
    catch e
    unexpected:="Could not download file -> " e.message
    return (ErrorLevel=0)
}