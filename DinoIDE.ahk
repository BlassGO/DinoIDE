#NoEnv
#SingleInstance, ignore
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%

; Include core modules
#include src\core\constants.ahk
#include src\core\init.ahk
#include config\settings.ahk

; Include UI modules
#include src\ui\gui_resize.ahk
#include src\ui\icon.ahk

; Include core functionality
#include src\core\DinoIDE.ahk
#include src\core\RichCode.ahk

; Include utilities
#include src\utils\obj_dump.ahk
#include src\utils\tools.ahk

; Include highlighters
#include src\highlighters\DinoCode.ahk

; AHK2 compilation configurations
;@Ahk2Exe-SetName         DinoIDE
;@Ahk2Exe-SetDescription  Official DinoCode editor
;@Ahk2Exe-SetCopyright    Copyright (c) since 2024
;@Ahk2Exe-SetCompanyName  by @BlassGO
;@Ahk2Exe-SetMainIcon assets\icon.ico
;@Ahk2Exe-AddResource assets\icon.ico, 160
;@Ahk2Exe-AddResource assets\icon.ico, 206
;@Ahk2Exe-AddResource assets\icon.ico, 207
;@Ahk2Exe-AddResource assets\icon.ico, 208

; Initialize
InitializeMenus()
InitializeGUI()
SetupGUI()
return

; Include handlers
#include src\handlers\file_handlers.ahk
#include src\handlers\console_handlers.ahk
#include src\handlers\build_handlers.ahk
#include src\handlers\ui_handlers.ahk
#include src\ui\menus.ahk

