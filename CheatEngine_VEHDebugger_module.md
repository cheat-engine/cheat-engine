```markdown
# Sub-Module: VEH Debugger

## Purpose/Functionality

The VEH (Vectored Exception Handling) Debugger provides an alternative debugging interface for Cheat Engine. Instead of relying solely on the standard Windows Debugging API (like `DebugActiveProcess` and the debug event loop), it utilizes Vectored Exception Handling, a mechanism in Windows that allows applications to register handlers at the beginning of the exception handling chain.

This approach offers several potential advantages:
*   **Stealth:** Can be less detectable by some anti-debugging techniques that specifically target the standard Windows Debug API.
*   **Control:** Provides a different way to intercept and handle exceptions like breakpoints, single-step traps, and access violations.
*   **Flexibility:** Allows for custom handling of exceptions directly within the context of the target process via an injected DLL.

## Structure and Mechanism

The VEH Debugger functionality is primarily managed by the `TVEHDebugInterface` class within `Cheat Engine/VEHDebugger.pas`, which works in conjunction with injected helper DLLs (`vehdebug-i386.dll` and `vehdebug-x86_64.dll`).

**Key Components:**

1.  **`VEHDebugger.pas` (Pascal Orchestration Unit):**
    *   **`TVEHDebugInterface` class:** Inherits from `TDebuggerInterface`.
        *   **Initialization (`DebugActiveProcess` method):**
            *   Selects the appropriate bitness (`vehdebug-i386.dll` or `vehdebug-x86_64.dll`).
            *   Creates a **shared memory region** using a named file mapping object. A unique GUID is generated for the mapping name. This region is defined by the `TVEHDebugSharedMem` structure (from `VEHDebugSharedMem.pas`).
            *   Creates two named **event objects** (`HasDebugEvent` and `HasHandledDebugEvent`) for synchronization.
            *   Duplicates the handles for the file mapping and events into the target process.
            *   Injects the helper `vehdebug-*.dll` into the target process.
            *   Uses Cheat Engine's Auto Assembler to execute a small bootstrap script in the target process. This script calls an exported function (e.g., `InitializeVEH`) within the injected `vehdebug-*.dll`, passing the duplicated handles and the name of the shared memory mapping.
        *   **Shared Memory (`VEHDebugView` of type `PVEHDebugSharedMem`):** This is the primary communication channel. It's used to:
            *   Transfer debug event details (exception code, address, thread ID, CPU context) from the injected DLL to Cheat Engine.
            *   Send continuation status from Cheat Engine back to the DLL.
            *   Store a heartbeat value and version information.
        *   **Event Synchronization:**
            *   `HasDebugEvent`: Signaled by the injected DLL when a debug event occurs in the target. CE waits on this.
            *   `HasHandledDebugEvent`: Signaled by CE after it has processed the debug event and is ready for the target thread to continue. The injected DLL waits on this.
        *   **Debug Event Loop (`WaitForDebugEvent` method):**
            *   Waits for `HasDebugEvent` to be signaled.
            *   Upon signaling, reads the exception information and CPU context from the `VEHDebugView` shared memory.
            *   Translates this information into the standard `TDebugEvent` structure used by Cheat Engine's core debugger logic.
            *   Also handles "injected events" (like simulated thread create/destroy notifications if the VEH DLL sends them specially).
        *   **Continuing Execution (`ContinueDebugEvent` method):**
            *   Writes the desired continuation status (e.g., `DBG_CONTINUE`, `DBG_EXCEPTION_NOT_HANDLED`) into `VEHDebugView`.
            *   Signals `HasHandledDebugEvent`.
        *   **Context Management (`GetThreadContext`, `SetThreadContext` methods):**
            *   When an exception is caught by the VEH DLL, the CPU context of the faulting thread is typically saved into the `VEHDebugView.CurrentContext` field in shared memory.
            *   CE's `GetThreadContext` and `SetThreadContext` methods for this debugger interface read from or write to this shared memory copy of the context, rather than always using the OS `Get/SetThreadContext` API calls, which can be more reliable or stealthy in certain scenarios.
        *   **Heartbeat Thread (`THeartBeat`):**
            *   A thread within CE that periodically updates a `heartbeat` counter in the shared memory. The injected DLL can monitor this to ensure CE is still active.
            *   It also performs a version check against `VEHDebugView.VEHVersion` to ensure compatibility between CE and the injected DLL.
        *   **Detaching (`DebugActiveProcessStop` method):**
            *   Uses Auto Assembler to call an `UnloadVEH` function in the injected DLL to clean up the VEH handler and resources.
            *   Terminates the heartbeat thread and releases shared resources.

2.  **Helper DLLs (`vehdebug-i386.dll`, `vehdebug-x86_64.dll`):**
    *   These are native C/C++ DLLs (source not directly in `VEHDebugger.pas`, compiled by `vehdebug.lpr` as per README).
    *   **`InitializeVEH` function (exported):**
        *   Called by the CE-injected bootstrap code.
        *   Receives handles to shared memory and synchronization events.
        *   Maps the shared memory (`VEHDebugView`).
        *   Registers a Vectored Exception Handler using `AddVectoredExceptionHandler(1, VehHandlerCallback)`. The `1` makes it the first handler to be called.
    *   **`VehHandlerCallback` function (the Vectored Exception Handler):**
        *   This function is invoked by the system when an exception occurs in any thread of the target process.
        *   It filters exceptions to decide which ones to report (e.g., breakpoints, single steps, access violations).
        *   If an exception is of interest:
            *   It populates the `VEHDebugView` shared memory structure with details from the `EXCEPTION_POINTERS` (exception code, address, CPU context via `ContextRecord`).
            *   It signals the `HasDebugEvent` to wake up Cheat Engine.
            *   It then waits on the `HasHandledDebugEvent`.
            *   Once CE signals back, it reads the `ContinueMethod` from `VEHDebugView` and returns the appropriate status (e.g., `EXCEPTION_CONTINUE_EXECUTION` if CE handled it, or `EXCEPTION_CONTINUE_SEARCH` to pass it to other handlers).
    *   **`UnloadVEH` function (exported):**
        *   Called by CE when detaching.
        *   Unregisters the Vectored Exception Handler using `RemoveVectoredExceptionHandler`.
        *   Releases resources.

## Build Process

*   The Pascal logic in `VEHDebugger.pas` is part of the main Cheat Engine Lazarus project.
*   The helper DLLs (`vehdebug-*.dll`) are compiled from the `vehdebug.lpr` project (and its associated C/C++ source files, which are not directly visible here but are implied by the `.lpr` being for DLLs).

## Interface with Main Cheat Engine Application

*   The `TVEHDebugInterface` class is registered with Cheat Engine's debugger core as one of the available debugging methods.
*   When selected by the user, CE instantiates `TVEHDebugInterface` and uses its methods (`DebugActiveProcess`, `WaitForDebugEvent`, etc.) to control the debugging session.
*   Communication relies heavily on the custom shared memory structure (`TVEHDebugSharedMem`) and synchronization events.

This architecture allows CE to implement a debugger that operates differently from the standard Windows Debug API, offering an alternative way to intercept and manage exceptions in the target process.
```
