; Paths
global CURRENT_DIR := A_ScriptDir
, ASSETS_PATH := CURRENT_DIR "\assets"
, SRC_PATH := CURRENT_DIR "\src"

; File paths
global DINOCODE_EXE := ASSETS_PATH "\dinocode\dinocode.exe"
, PREDICTION_PATH := ASSETS_PATH "\config\prediction.txt"
, CONFIG_PATH := ASSETS_PATH "\config\config.ini"
, DEFAULT_EXAMPLE := ASSETS_PATH "\examples\hello_world.dino"
, SCRIPT_PATH := ""

; Menu names
global MENU_ZOOM := "Zoom"
, MENU_FILE := "File"
, MENU_EDIT := "Edit"
, MENU_BUILD := "Build"
, MENU_SEARCH := "Search"
, MENU_VIEW := "View"
, MENU_COMMENTS := "Comments"
, MENU_EXAMPLES := "Examples"
, MENU_LIBRARIES := "Libraries"

; Window settings
global WINDOW_TITLE := "DinoIDE"
, WINDOW_STYLE := "+Resize +LastFound -AlwaysOnTop"

; Colors
global COLOR_BG := 0x2A2C2A
, COLOR_FG := 0xEDEDCD
, COLOR_WHITE := 0xFFFFFF

; Window styles
global WS_POPUP := 0x80000000
, WS_CAPTION := 0xC00000
, WS_THICKFRAME := 0x40000
, WS_EX_CLIENTEDGE := 0x200
, WS_CHILD := 0x40000000
, WS_CLIPCHILDREN := 0x2000000
, SWP_NOACTIVATE := 0x10
, SWP_SHOWWINDOW := 0x40
, SWP_NOSENDCHANGING := 0x400
