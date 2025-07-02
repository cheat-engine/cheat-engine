```markdown
# Sub-Module: .NET Components

Cheat Engine includes several sub-modules dedicated to interacting with applications running on the .NET Framework or .NET Core. These components allow users to inspect .NET metadata, compile C# code on-the-fly, and interact with live .NET objects and methods.

## Components Overview

1.  **DotNetCompiler (`Cheat Engine/DotNetCompiler/`)**
    *   **Purpose:** Enables on-the-fly compilation of C# code from within Cheat Engine, typically triggered by Lua scripts using a `cscompile` command.
    *   **Structure:**
        *   `CSCompiler/CSCompiler.sln`: Visual Studio solution.
        *   `CSCompiler/CSCompiler/CSCompiler.csproj`: C# project file.
        *   `CSCompiler/CSCompiler/Class1.cs`: Contains the `Compiler` class.
    *   **Mechanism:**
        *   The `Compiler` class uses `System.CodeDom.Compiler.CodeDomProvider` (specifically for "CSharp") to compile C# source code provided as a string.
        *   It can compile to an output assembly file (`.dll`).
        *   Supports adding assembly references and specifying a core assembly (e.g., for targeting specific .NET Framework versions).
        *   The main Cheat Engine application communicates with this component by loading it (likely as an out-of-process COM server or by launching the EXE which then exposes an interface). The `Compiler.NewCompiler(string parameters)` static method is an entry point that receives an address from CE. It then populates a structure at that address with function pointers to its `CompileCode`, `AddAssemblyReference`, `SetCoreAssembly`, and `Release` methods. This allows CE's Pascal code to call these C# methods.
    *   **Build:** Compiled via Visual Studio using `CSCompiler.sln`.

2.  **DotNetDataCollector (`Cheat Engine/DotNetDataCollector/`)**
    *   **Purpose:** Collects metadata and symbol information from target .NET processes. This includes information about AppDomains, assemblies, modules, types (classes, structs, enums), methods, fields, and parameters. It can also enumerate live objects on the garbage-collected heap.
    *   **Structure:**
        *   `DotNetDataCollector.sln`: Visual Studio solution.
        *   `DotNetDataCollector/DotNetDataCollector.vcxproj`: C++ project file for an executable.
        *   `DotNetDataCollector.cpp`: Contains `wWinMain`, indicating it's an EXE.
        *   `PipeServer.h`, `PipeServer.cpp`: Implements a named pipe server for communication with Cheat Engine.
        *   `MyICLRDebuggingLibraryProvider.cpp/h`, `MyICorDebugDataTarget.cpp/h`: Implementations for .NET Debugging API interfaces.
    *   **Mechanism:**
        *   Runs as a separate executable (`DotNetDataCollector.exe`).
        *   Cheat Engine launches this EXE, passing a pipe name as a command-line argument.
        *   It uses the .NET Debugging API (`ICLRDebugging`, `ICorDebugProcess`, `IMetaDataImport`, etc.) to attach to the target .NET process (non-invasively or with debugging rights) and query its metadata.
        *   Communication with Cheat Engine occurs over a named pipe. `PipeServer.h` defines commands like `CMD_ENUMDOMAINS`, `CMD_ENUMMODULELIST`, `CMD_ENUMTYPEDEFS`, `CMD_GETALLOBJECTS`, etc. CE sends these commands, and `DotNetDataCollector` responds with the requested data.
    *   **Build:** Compiled via Visual Studio using `DotNetDataCollector.sln`.

3.  **DotNetInvasiveDataCollector (`Cheat Engine/DotNetInvasiveDataCollector/`)**
    *   **Purpose:** Provides more "invasive" ways to interact with .NET applications, focusing on runtime aspects like JIT-compiled code and live object manipulation. The README mentions "runtime JIT support."
    *   **Structure:**
        *   `DotNetInvasiveDataCollector.sln`: Visual Studio solution.
        *   `DotNetInvasiveDataCollector/DotNetInterface.csproj`: C# class library project.
        *   `DotNetInterface2/DotNetInterface2.csproj`: Another C# class library project (its exact distinct role from `DotNetInterface` needs deeper inspection, but it's part of the same system).
        *   `PipeServer.cs`: Implements a named pipe server within this managed DLL.
    *   **Mechanism:**
        *   This component is a managed DLL that is likely injected into the target .NET process by Cheat Engine.
        *   Once injected, its `PipeServer.Init(string parameters)` method is called by CE, providing a pipe handle. The DLL then starts a `NamedPipeServerStream` on this handle to listen for commands from CE.
        *   Key functionalities exposed via pipe commands (from `PipeServer.cs`):
            *   `INITMODULELIST`: Enumerates loaded .NET modules within the target process.
            *   `GETMETHODENTRYPOINT`: Uses `MethodBase.MethodHandle.GetFunctionPointer()` to get the native memory address of a JIT-compiled method. This is crucial for hooking or analyzing JITted code.
            *   `GETFIELDTYPENAME`, `GETFIELDVALUE`, `SETFIELDVALUE`: Uses .NET Reflection (`FieldInfo.GetValue`, `FieldInfo.SetValue`) to interact with fields of live objects.
            *   `LOADMODULE`: Loads a .NET assembly into the target process.
            *   `WRAPOBJECT`, `UNWRAPOBJECT`: Manages `GCHandle`s to ensure .NET objects are not garbage collected while CE is referencing them, and to pass object references between the unmanaged CE world and the managed target.
            *   `INVOKEMETHOD`: Uses `MethodBase.Invoke` to execute methods on objects within the target process.
    *   **Build:** Compiled via Visual Studio using `DotNetInvasiveDataCollector.sln`.

## Interaction and Workflow

1.  **User Action/Lua Script:** The user, through the CE interface or a Lua script, requests a .NET-related operation (e.g., inspect a .NET application, compile C# code, call a .NET method).
2.  **Cheat Engine Orchestration:**
    *   **For C# Compilation:** CE calls the `DotNetCompiler` component (likely its exported functions discovered via the `NewCompiler` initialization).
    *   **For Metadata/Symbol Collection:** CE launches `DotNetDataCollector.exe` with a unique pipe name and sends commands (e.g., `CMD_TARGETPROCESS`, `CMD_ENUMTYPEDEFS`) over the pipe to retrieve information.
    *   **For Invasive Operations (JIT info, live object manipulation):**
        *   CE injects the `DotNetInvasiveDataCollector` managed DLL into the target .NET process.
        *   CE calls an exported initialization function in the injected DLL (like `PipeServer.Init`), passing a pipe handle.
        *   CE then sends commands (e.g., `GETMETHODENTRYPOINT`, `GETFIELDVALUE`) over this pipe to the injected DLL, which executes them using .NET Reflection or other runtime services within the target process.
3.  **Communication:**
    *   `DotNetCompiler`: Function pointers passed via a shared memory structure.
    *   `DotNetDataCollector` (C++ EXE): Named Pipes.
    *   `DotNetInvasiveDataCollector` (C# DLL): Named Pipes.

## Dependencies

*   **Cheat Engine Main Application:** These .NET components are tools used by the main CE application to extend its capabilities into the .NET realm.
*   **.NET Framework/Core:** The target application must be a .NET application. The specific versions supported might depend on how these tools are compiled and the .NET APIs they use.
*   **Windows API:** For process interaction, pipe communication, and DLL injection.

These components collectively provide Cheat Engine with powerful tools to analyze, modify, and script .NET applications, ranging from metadata inspection to runtime code manipulation and live object interaction.
```
