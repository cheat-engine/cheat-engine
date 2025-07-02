```markdown
# Sub-Module: Java Inspection (`cejvmti.sln`)

## Purpose/Functionality

The Java Inspection sub-module, built using the `cejvmti.sln` Visual Studio solution, provides Cheat Engine with capabilities to inspect and interact with applications running on the Java Virtual Machine (JVM). It leverages the Java Virtual Machine Tool Interface (JVMTI), a native programming interface that allows tools to query the state and control the execution of applications running in the JVM.

Key functionalities likely include:

*   **JVM Inspection:** Gathering information about the running JVM, such as loaded classes, object instances, and thread states.
*   **Class Information:** Retrieving details about Java classes, including their fields, methods, and inheritance hierarchy.
*   **Object Inspection:** Examining the values of fields within Java objects on the heap.
*   **Method Hooking/Interception:** Potentially intercepting calls to Java methods or modifying their bytecode on-the-fly. This could be used for understanding program flow or altering behavior.
*   **Heap Analysis:** Walking the Java heap to find specific object instances.
*   **Event Notification:** Receiving notifications for JVM events like class loading, method entry/exit, garbage collection, etc.

## Structure (Hypothesized)

*   **`cejvmti.sln`**: The Visual Studio solution file used to build the native JVMTI agent DLLs.
*   **Source Files (Likely C/C++):** The solution would contain C or C++ source files that implement the JVMTI agent. These files would include:
    *   Implementation of `Agent_OnLoad`: The entry point called by the JVM when the agent is loaded. This function typically initializes the agent, gets the `jvmtiEnv*` pointer, and registers for desired capabilities and events.
    *   JVMTI Event Callback Functions: Functions to handle events like `VMInit`, `ClassPrepare`, `MethodEntry`, etc.
    *   Functions that use the `jvmtiEnv*` interface to perform inspection tasks (e.g., `GetClassSignature`, `GetLoadedClasses`, `GetObjectSize`, `GetFieldName`, `GetMethodName`).
    *   IPC (Inter-Process Communication) Code: Logic to communicate with the main Cheat Engine application (e.g., using named pipes or shared memory) to receive commands and send back data.
*   **Output DLLs:** The build process produces 32-bit and 64-bit native DLLs (e.g., `cejvmti32.dll`, `cejvmti64.dll`).

## Mechanism (Hypothesized)

1.  **Agent Loading:**
    *   Cheat Engine identifies that the target process is a Java application.
    *   It then loads the appropriate bitness version of the `cejvmti.dll` into the target Java process. This can be done in a few ways:
        *   By starting the JVM with specific agent arguments (e.g., `-agentpath:path/to/cejvmti.dll=options`).
        *   By using the Java Attach API to dynamically load the agent into an already running JVM.
2.  **Agent Initialization (`Agent_OnLoad`):**
    *   Once loaded, the JVM calls the `Agent_OnLoad` function in the `cejvmti.dll`.
    *   Inside `Agent_OnLoad`, the agent:
        *   Obtains the `jvmtiEnv*` environment pointer, which is the primary interface to JVMTI functions.
        *   Sets required JVMTI capabilities (e.g., `can_access_local_variables`, `can_get_bytecodes`, `can_tag_objects`).
        *   Registers callbacks for JVMTI events it's interested in (e.g., `JVMTI_EVENT_CLASS_PREPARE` to be notified when classes are loaded).
        *   Establishes an IPC channel (e.g., creates a named pipe server) to communicate with the main Cheat Engine application.
3.  **Communication with Cheat Engine:**
    *   The main Cheat Engine application (Pascal side) acts as a client to the IPC channel established by the `cejvmti.dll`.
    *   CE sends commands to the agent (e.g., "list all classes," "get fields for class X," "find instances of object Y").
4.  **JVMTI Operations:**
    *   The `cejvmti.dll`, upon receiving a command from CE, uses the `jvmtiEnv*` functions to perform the requested operation within the target JVM.
    *   For example, to list classes, it might use `GetLoadedClasses`. To get field values, it might use `GetClassFields` and then JVMTI functions to access object memory (or JNI, if appropriate).
5.  **Data Return:** The results of these operations are then sent back to the main Cheat Engine application via the IPC channel.
6.  **Event Handling:** If event callbacks are registered, the JVMTI agent will receive notifications from the JVM (e.g., when a new class is loaded). It can then process this information or forward it to Cheat Engine.

## Build Process

*   The `cejvmti.dll` (both 32-bit and 64-bit versions) is compiled using Visual Studio via the `cejvmti.sln` solution.
*   This requires a C/C++ compiler and the JDK headers (for `jvmti.h`).

## Interface with Main Cheat Engine Application

*   The main CE application initiates the loading of the `cejvmti.dll` into the target Java process.
*   It communicates with the injected DLL using a custom IPC protocol (likely over named pipes).
*   CE provides UI elements (e.g., a Java inspection window) that send requests to the agent and display the data received from it.
*   Pascal units within Cheat Engine would handle the client-side of this IPC and translate the data into a user-friendly format.

This sub-module allows Cheat Engine to extend its powerful inspection and manipulation tools to the realm of Java applications, providing capabilities similar to what it offers for native applications or .NET applications.
```
