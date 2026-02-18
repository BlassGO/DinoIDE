; DinoIDE Settings Configuration

; IDE Version
global ide_version := "v1.4.0"

; Supported languages and sample codes
global Codes :=
( LTrim Join Comments
{
	"DinoCode": {
		"Highlighter": "HighlightDino",
		"FileDefault": DEFAULT_EXAMPLE
	}
}
)

; RichCode control settings
global Settings :=
( LTrim Join Comments
{
	"TabSize": 4,
	"Indent": "`t",
	"FGColor": 0xEDEDCD,
	"BGColor": 0x2A2C2A,
	"Font": {"Typeface": "Consolas", "Size": 11},
	"WordWrap": False,
	"Multiline": True,
	
	"UseHighlighter": True,
	"HighlightDelay": 400,
	"Gutter": {
		"Width": 35,
		"FGColor": 0x9FAFAF,
		"BGColor": 0x262626
	},
	"Colors": {
		"Comments":     0x7F9F7F,
		"Functions":    0x7CC8CF,
		"Multiline":    0x7F9F7F,
		"Numbers":      0xCB8DD9,
		"Punctuation":  0xEF5A37,
		"Strings":      0xDAC491,
		
		; DinoCode specific colors
		"Builtins":     0xBB8FCE,
		"Variables":    0xC8E723,
		"Signals":      0xC87227,
		"Opers":        0xC87227,
		"Flow":         0x52E0E4,
		"TAGs":         0xF79B57,
		"TAGMAIN":      0xF2300A,
		"Errors":       0xFF00FF
	},
	"UseAutoComplete": True,
	"ACListRebuildDelay": 500
}
)

; UI Configuration
global UIConfig :=
( LTrim Join Comments
{
	"Zoom": "100 %",
	"DefaultFont": "Consolas",
	"DefaultFontSize": 11
}
)

; Console Configuration
global ConsoleConfig :=
( LTrim Join Comments
{
	"PreferredConsole": "cmd"
}
)
