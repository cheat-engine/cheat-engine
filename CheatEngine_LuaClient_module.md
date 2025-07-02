```markdown
# Sub-Module: Lua Client (`luaclient.lpr`)

## Purpose/Functionality

The `luaclient.lpr` project, as mentioned in the README, compiles into 32-bit and 64-bit DLLs that provide "{$luacode} capability". This suggests that the Lua Client sub-module is designed to execute Lua scripts in a specific context, likely within the target process space.

The primary functionalities are hypothesized to be:

*   **In-Process Lua Execution:** Allows Lua scripts, particularly those embedded within other Cheat Engine features like Auto Assembler scripts (using a directive like `{$luacode} ... {$endluacode}`), to be executed directly within the memory space of the target application.
*   **Direct Memory Access:** Scripts running via `luaclient.dll` in the target process would have direct access to the target's memory, potentially allowing for faster or more complex manipulations than scripts running solely within the main Cheat Engine process and using Read/WriteProcessMemory.
*   **Interfacing with Target Application APIs:** Lua code running in the target could more easily call functions or interact with data structures within that application.
*   **Extended Scripting Context:** It might provide a slightly different or more specialized Lua environment tailored for in-process tasks, possibly with a subset or superset of the functions available in the main Cheat Engine Lua engine.

## Structure

*   **`luaclient.lpr`**: The Lazarus Project file used to compile the `luaclient.dll` (in 32-bit and 64-bit versions).
*   **Associated Pascal Units (Hypothesized):** While not directly identified by `ls` with a `luaclient*.pas` name, the `.lpr` file would reference one or more Pascal units that contain the actual implementation. These units would handle:
    *   DLL entry points (`DllMain`).
    *   Embedding a Lua state (or connecting to one).
    *   An interface (e.g., exported functions, IPC mechanism) to receive Lua code strings from the main Cheat Engine application.
    *   Execution of the received Lua code.
    *   Potentially, a way to return results or errors back to Cheat Engine.

## Mechanism (Hypothesized)

1.  **Compilation:** `luaclient.lpr` is compiled into `luaclient-i386.dll` and `luaclient-x86_64.dll`.
2.  **Injection:** When Cheat Engine (e.g., through an Auto Assembler script containing a `{$luacode}` block) needs to execute Lua code via this client, it injects the appropriate bitness version of `luaclient.dll` into the target process if it's not already loaded.
3.  **Code Transfer & Execution:**
    *   The main Cheat Engine application extracts the Lua script from the `{$luacode}` directive.
    *   This script string is then transferred to the injected `luaclient.dll` within the target process. This could be done via:
        *   Remote thread creation with a pointer to the script in shared memory.
        *   An Inter-Process Communication (IPC) mechanism like named pipes or shared memory, where the DLL runs a server to listen for script execution requests.
    *   The `luaclient.dll` receives the script, executes it using its embedded Lua state, and performs the actions defined in the script directly within the target process's context.
4.  **Interface with Main Lua Engine:** The capabilities exposed within the `luaclient.dll`'s Lua environment might be a subset of those in `LuaHandler.pas` or tailored for in-process operations. It might also have mechanisms to call back into the main Cheat Engine process for certain services if needed, though this is less likely for performance-critical in-process scripts.

The "{$luacode}" directive itself would be parsed by the component invoking it (e.g., the Auto Assembler in `autoassembler.pas`), which then uses the Lua Client DLL system to execute the embedded Lua code.

## Build Process

*   Compiled using Lazarus IDE via the `luaclient.lpr` project file.
*   Separate 32-bit and 64-bit DLLs are produced.

## Interface with Main Cheat Engine Application

*   **Initiation:** The main CE application (specifically components like the Auto Assembler) identifies `{$luacode}` blocks.
*   **Control:** CE injects `luaclient.dll` and sends the script to it for execution.
*   **Communication (Hypothesized):** Likely involves creating a remote thread, or a simple IPC mechanism if the DLL sets up a listener. The exact method would be detailed in the Pascal units associated with `luaclient.lpr` and the parts of CE that use this feature.

The primary benefit of this sub-module is to provide a sandboxed or direct execution environment for Lua within the target process, distinct from the main Lua engine running within Cheat Engine itself. This is crucial for tasks requiring high performance memory access or direct interaction with the target's internal state.
```
