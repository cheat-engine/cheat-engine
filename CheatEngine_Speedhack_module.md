```markdown
# Sub-Module: Speedhack

## Purpose/Functionality

The Speedhack sub-module allows users to alter the perceived flow of time within a target application, effectively making it run faster or slower. This is achieved by intercepting calls to common system timing functions and modifying the values they return or operate on.

## Structure and Mechanism

The speedhack functionality in Cheat Engine is primarily orchestrated by the `Cheat Engine/speedhack2.pas` unit, specifically the `TSpeedhack` class. It does not rely on a `speedhack.lpr` project file for its core Pascal logic; rather, it uses pre-compiled helper DLLs.

**Key Components:**

1.  **`speedhack2.pas` (Pascal Orchestration Unit):**
    *   **`TSpeedhack` class:**
        *   **Initialization (`constructor Create`):**
            *   Checks for external speedhack handlers registered via callbacks.
            *   If no external handler, it proceeds to inject a helper DLL into the target process:
                *   Windows: `speedhack-i386.dll` or `speedhack-x86_64.dll`.
                *   macOS: `libspeedhack.dylib`.
            *   After DLL injection, it uses Cheat Engine's **Auto Assembler** to generate and inject code that hooks various system timing functions in the target process. These hooks redirect calls to functions exported by the injected helper DLL.
                *   **Hooked functions (platform-dependent):**
                    *   Windows: `GetTickCount`, `timeGetTime`, `QueryPerformanceCounter`, `GetTickCount64`.
                    *   macOS: `gettimeofday`, `mach_absolute_time`.
                    *   Linux (via ceserver): `gettimeofday`, `clock_gettime`.
        *   **Setting Speed (`setSpeed(speed: single)`):**
            *   Again, checks for external callback handlers first.
            *   For local processes, it generates a small Auto Assembler script. This script calls an `InitializeSpeedhack` function within the injected helper DLL, passing the desired speed. This function in the helper DLL is responsible for setting the actual time scaling factor.
            *   For networked processes (via ceserver), it sends a command to the server to set the speed.
        *   **Cleanup (`destructor Destroy`):** Resets the speed to 1.0 but intentionally does not unhook the API calls, as rapidly changing time direction can crash some games.
    *   **Callback System (`registerSpeedhackCallbacks`, `unregisterSpeedhackCallbacks`):**
        *   Provides an interface for plugins or Lua scripts to register their own custom speedhack activation and speed-setting routines, allowing them to override the default mechanism.

2.  **Helper DLLs (e.g., `speedhack-i386.dll`, `speedhack-x86_64.dll`, `libspeedhack.dylib`):**
    *   These are pre-compiled native libraries (likely C/C++) that are injected into the target process. Their source code is not directly part of `speedhack2.pas`.
    *   **Exported Functions:** They export functions that the Auto Assembler hooks point to (e.g., `speedhackversion_GetTickCount`, `speedhackversion_QueryPerformanceCounter`) and a configuration function (e.g., `InitializeSpeedhack`).
    *   **Core Logic:**
        *   `InitializeSpeedhack(float new_speed)`: Stores the `new_speed` factor internally.
        *   `speedhackversion_GetTickCount()` (and similar): When called by the hooked game, this function retrieves the actual system time, applies the stored speed factor to it (e.g., by scaling elapsed time or adjusting return values), and then returns the modified time to the caller.

**Interaction Flow:**

1.  Cheat Engine user enables speedhack or changes speed.
2.  `TSpeedhack.Create` (if first time) or `TSpeedhack.setSpeed` is called.
3.  The helper DLL is injected (if not already present).
4.  Auto Assembler scripts hook system timing functions in the target, redirecting them to the helper DLL.
5.  `setSpeed` causes a call to `InitializeSpeedhack` in the helper DLL within the target process, setting the desired speed multiplier.
6.  When the game calls a hooked timing function (e.g., `GetTickCount`), it's diverted to the corresponding function in the helper DLL.
7.  The helper DLL function applies the speed multiplier to the time value and returns the modified value to the game.

## Build Process

*   The Pascal logic in `speedhack2.pas` is part of the main Cheat Engine Lazarus project.
*   The helper DLLs (`speedhack-*.dll`, `libspeedhack.dylib`) are separate C/C++ projects, compiled independently (likely using Visual Studio for Windows DLLs and Xcode/GCC for macOS dylib). The README indicates these are part of the secondary projects to be compiled.

## Interface with Main Cheat Engine Application

*   The `TSpeedhack` class in `speedhack2.pas` is instantiated and used by the main Cheat Engine form (`MainUnit.pas`) when the user interacts with the speedhack UI elements (checkbox, slider, edit box).
*   The main application provides the paths to the helper DLLs.
*   It uses its Auto Assembler capabilities to dynamically generate and inject the necessary hooks.

This design separates the orchestration logic (Pascal in CE) from the low-level time manipulation and API interception (C/C++ in the helper DLLs), leveraging CE's powerful Auto Assembly engine for the hooking process.
```
