# Sistema de Ejecución Asíncrona - DinoIDE

## Overview

Este sistema implementa ejecución asíncrona de comandos en procesos separados para evitar que la GUI se bloquee durante la ejecución de compilaciones o comandos largos.

## Arquitectura

### Componentes Principales

1. **AsyncProcessManager** (`src/core/async_process_manager.ahk`)
   - Maneja la creación y monitoreo de procesos asíncronos
   - Implementa comunicación IPC entre procesos
   - Gestiona la limpieza de recursos

2. **Async Executor** (`src/handlers/async_executor.ahk`)
   - Script separado que ejecuta los comandos
   - Captura stdout/stderr en tiempo real
   - Notifica al proceso principal vía mensajes Windows

3. **Build Handlers** (`src/handlers/build_handlers.ahk`)
   - Integración con el sistema de build existente
   - Handlers para detener y monitorear procesos

## Características

### ✅ Ejecución No Bloqueante
- Los comandos se ejecutan en procesos completamente separados
- La GUI permanece totalmente responsiva
- Soporte para múltiples procesos concurrentes

### ✅ Comunicación en Tiempo Real
- Salida stdout/stderr se muestra en tiempo real
- Mensajes de error y estado inmediatos
- Notificaciones de completion

### ✅ Gestión de Recursos
- Limpieza automática de archivos temporales
- Terminación controlada de procesos
- Cleanup al cerrar la aplicación

### ✅ Fallback y Robustez
- Fallback a modo síncrono si el sistema asíncrono falla
- Manejo de errores y excepciones
- Recuperación automática

## Uso

### Ejecutar Comando Asíncrono
```autohotkey
; Desde el IDE
IDE.RunCommandWithOutput("comando", "directorio_de_trabajo")

; O directamente
processId := IDE.AsyncManager.RunAsync("comando", "directorio")
```

### Monitorear Procesos
```autohotkey
; Verificar si hay procesos activos
if (IDE.AsyncManager.HasRunningProcesses()) {
    count := IDE.AsyncManager.GetProcessCount()
}

; Detener todos los procesos
IDE.AsyncManager.StopAll()
```

### Menú Integrado
- **Build → Just RUN**: Ejecución asíncrona
- **Build → Stop Async Processes**: Detener todos los procesos
- **Build → Show Process Status**: Mostrar estado actual

## Flujo de Ejecución

1. **Inicio**: `RunCommandWithOutput()` es llamado
2. **Creación**: `AsyncManager.RunAsync()` crea proceso separado
3. **Ejecución**: `async_executor.ahk` ejecuta el comando
4. **Comunicación**: Salida se envía vía mensajes Windows
5. **Monitoreo**: `AsyncManager` monitorea el proceso
6. **Completion**: Notificación de término y cleanup

## Mensajes IPC

Se usan mensajes Windows para comunicación:

- `WM_USER+1 (0x4001)`: Output normal
- `WM_USER+2 (0x4002)`: Salida de error  
- `WM_USER+3 (0x4003)`: Notificación de término

Formato del mensaje: `tipo|datos`

## Archivos Temporales

Los archivos temporales se guardan en:
- `%TEMP%\dinoide_output_[processId].txt`
- `%TEMP%\dinoide_error_[processId].txt`

Se eliminan automáticamente al terminar el proceso.

## Configuración

No requiere configuración adicional. El sistema se activa automáticamente cuando está disponible.

## Solución de Problemas

### Si la ejecución asíncrona no funciona:
1. Verificar que `async_executor.ahk` exista
2. Comprobar permisos de ejecución
3. Revisar logs de error en consola

### Si procesos no se detienen:
1. Usar "Stop Async Processes" del menú
2. Reiniciar la aplicación (limpia todo)
3. Verificar procesos en Task Manager

## Beneficios vs SetTimer

| Característica | SetTimer | AsyncProcessManager |
|---------------|----------|-------------------|
| Bloqueo GUI | ❌ Sí bloquea | ✅ No bloquea |
| Proceso separado | ❌ Mismo proceso | ✅ Proceso separado |
| Comunicación | ❌ Variables globales | ✅ IPC estructurado |
| Monitoreo | ❌ Limitado | ✅ Completo |
| Cleanup | ❌ Manual | ✅ Automático |
| Escalabilidad | ❌ Single thread | ✅ Multi-proceso |

## Notas Técnicas

- Compatible con Windows Vista+
- Requiere AutoHotkey v1.1+
- Usa `WScript.Shell` para ejecución
- Mensajes Windows para IPC
- Archivos temporales para buffer

## Futuras Mejoras

- [ ] Soporte para pipes nombrados
- [ ] Timeout configurable
- [ ] Prioridad de procesos
- [ ] Historial de comandos
- [ ] Integración con terminal externa
