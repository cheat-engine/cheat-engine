```markdown
# Sub-Module: DBKKernel - Ultimap & Ultimap2

The Ultimap features within DBKKernel provide advanced, low-level execution tracing capabilities, primarily designed to be less detectable than traditional debugging methods. They leverage CPU hardware features for tracing. There are two distinct versions: Ultimap (original, using Intel BTS) and Ultimap2 (using Intel Processor Trace).

## 1. Ultimap (Original - Branch Trace Store)

*   **Files:** `DBKKernel/ultimap.c`, `DBKKernel/ultimap.h`, `DBKKernel/amd64/ultimapa.asm`
*   **Integrated into:** `DBKDrvr.sys` (main kernel driver).

### Purpose/Functionality

Ultimap uses Intel's Branch Trace Store (BTS) feature to record branches (jumps, calls, returns, interrupts) taken by a target process. This allows for reconstruction of the execution path.

*   **Branch Tracing:** Captures `FromAddress`, `ToAddress` for each branch.
*   **Process-Specific Tracing:** Can be targeted to a specific process using its CR3 value (though VMX is mentioned as "required" in comments, suggesting DBVM might assist in setting this up reliably across all cores for a specific CR3).
*   **Data Collection:** Branch records are stored in a CPU-specific buffer (DS_AREA). When this buffer is full (or nearly full), a Performance Monitoring Interrupt (PMI) is triggered.
*   **User-Mode Interface:** Provides mechanisms for a user-mode application (Cheat Engine) to start/stop tracing, receive the collected branch data, and signal when data has been processed.

### Structure and Mechanism

1.  **Initialization (`ultimap()` function):**
    *   Called by Cheat Engine via an IOCTL to `DBKDrvr.sys`.
    *   Parameters include the target process's CR3, the desired `IA32_DEBUGCTL` MSR value (to enable LBR/BTS flags), and the size of the DS_AREA (Debug Store Area) per CPU.
    *   **DS_AREA Setup:** For each CPU, a `DS_AREA_MANAGEMENT64` structure and associated buffer for BTS records are allocated. The `IA32_DS_AREA` MSR on each CPU is (or would be, if not using VMX) set to point to this buffer.
    *   **PMI Hooking:** The driver hooks the Performance Monitoring Interrupt (PMI) by calling `HalSetSystemInformation(HalProfileSourceInterruptHandler, ...)` to redirect PMI to a custom handler (`perfmon_hook`). The Local APIC's LVT Performance Monitor register is configured to enable the PMI when the BTS buffer is full.
    *   **Enabling BTS:** The `IA32_DEBUGCTL` MSR is configured on each relevant CPU to enable BTS (e.g., setting the `BTINT` (Branch Trace Interrupt), `BTS` (Branch Trace Store), and `TR` (Trace Messages Enable) bits). This is often done via VMX if DBVM is active to ensure correct context-specific tracing.
    *   **Data Transfer Setup:** A pool of data blocks and synchronization events (`DataReadyEvent`, `DataHandledEvent` via `ULTIMAPEVENT` and `ULTIMAPDATAEVENT` structures) are initialized to manage the flow of BTS data to user mode. A semaphore (`DataBlockSemaphore`) controls access to these blocks.

2.  **Trace Collection & PMI Handling (`perfmon_hook` / `perfmon_interrupt_centry`):**
    *   When a CPU's BTS buffer fills up, it triggers a PMI.
    *   The `perfmon_hook` handler is invoked.
    *   It copies the collected branch records from the CPU's `DS_AREA` (specifically, the BTS buffer part) into a kernel buffer allocated from a pool.
    *   It resets the BTS index register in the `DS_AREA` so the CPU can continue tracing.
    *   It then signals the corresponding `DataReadyEvent` for the data block now containing the trace.

3.  **User-Mode Interaction (Cheat Engine):**
    *   CE calls `ultimap_waitForData()`, which waits on the array of `DataReadyEvent`s.
    *   When an event is signaled, `ultimap_waitForData` identifies which data block is ready.
    *   It maps the kernel buffer (containing the BTS data for that block) into CE's user-mode address space using an MDL (Memory Descriptor List).
    *   CE processes the branch records.
    *   CE calls `ultimap_continue()`, which unmaps the memory, frees the kernel buffer, and releases the data block and semaphore, allowing another PMI to be processed for that block.

4.  **Control:**
    *   `ultimap_disable()`: Stops tracing, unhooks PMI, frees resources.
    *   `ultimap_pause()`/`ultimap_resume()`: Likely use VMX functions (`vmx_ultimap_pause`/`resume`) to temporarily halt/restart tracing on all CPUs.
    *   `ultimap_flushBuffers()`: Manually triggers the collection of current data from all CPU BTS buffers.

## 2. Ultimap2 (Intel Processor Trace)

*   **Files:** `DBKKernel/ultimap2/UltimapDrvr.c`, `DBKKernel/ultimap2/UltimapDrvr.h`, `DBKKernel/ultimap2/apic.c`, `DBKKernel/ultimap2.h` (interface for DBKDrvr)
*   **Implemented as:** A separate kernel driver (`Ultimap2.sys`, inferred).

### Purpose/Functionality

Ultimap2 utilizes **Intel Processor Trace (Intel PT)**, a significantly more advanced hardware tracing feature than BTS. Intel PT can generate a highly compressed trace of execution, allowing for reconstruction of the exact program flow, including timing information, CR3 changes, and more.

*   **Detailed Execution Tracing:** Captures a much richer set of execution data compared to BTS.
*   **Hardware-Based:** Relies on dedicated CPU hardware for tracing, making it efficient and potentially stealthy.
*   **Complex Configuration:** Intel PT requires configuring several MSRs (`IA32_RTIT_CTL`, `IA32_RTIT_OUTPUT_BASE`, etc.) and setting up memory buffers (ToPA - Table of Physical Addresses - or Single Range Output).
*   **Data Decoding:** The raw trace data from Intel PT needs specialized decoders (like `libipt`) to be converted into a human-readable execution log.

### Structure and Mechanism (Partially Inferred)

1.  **Driver Entry (`UltimapDrvr.c` - `DriverEntry`):**
    *   **CPU Feature Check:** Explicitly checks for Intel CPU and the Intel PT feature flag (CPUID.7.0.EBX[25]). If not supported, the driver fails to load.
    *   **Device Creation:** Creates a device object (e.g., `\\.\Ultimap2Device`) for communication with user-mode Cheat Engine.
    *   Initializes APIC helper functions via `setup_APIC_BASE()`.

2.  **User-Mode Control (via IOCTLs to `Ultimap2.sys`):**
    *   Cheat Engine would send commands to:
        *   Start/Stop Intel PT tracing for a specific process or system-wide.
        *   Configure Intel PT parameters (buffer sizes, ToPA tables, filters like CR3 filtering, address range filtering).
        *   Retrieve collected trace data.

3.  **Intel PT Configuration and Management (within `Ultimap2.sys`):**
    *   **MSR Configuration:** The driver writes to Intel PT MSRs to enable tracing, define output buffers (e.g., ToPA tables), and set filters.
    *   **Buffer Management:** Manages the memory buffers where the CPU writes trace data. This involves allocating these buffers and providing their physical addresses to the CPU via MSRs. ToPA tables allow for multiple non-contiguous buffers.
    *   **Interrupt Handling:** Configures interrupts (PMI, NMI, or other interrupts via APIC) to be triggered when trace buffers are full or other PT events occur. The `DBKKernel/ultimap2/apic.c` suggests direct APIC manipulation for interrupt management.
    *   The `UnregisterUltimapPMI()` in `UnloadDriver` suggests PMI is used.

4.  **Data Retrieval and Processing:**
    *   When trace buffers fill, the interrupt handler in `Ultimap2.sys` would:
        *   Signal user-mode Cheat Engine.
        *   Make the trace data available (e.g., by mapping buffers or copying data).
    *   Cheat Engine would then retrieve this raw trace data and use a library (like `libipt`, which is present in `DBKKernel/libipt/`) to decode it into a meaningful execution log.

5.  **Disabling (`DisableUltimap2()`):**
    *   Disables Intel PT by clearing control MSRs.
    *   Unregisters interrupt handlers.
    *   Frees allocated buffers and other resources.

## Relationship and Dependencies

*   **DBKKernel:**
    *   The original Ultimap is part of `DBKDrvr.sys`.
    *   `DBKDrvr.sys` also includes `ultimap2.h`, suggesting it might have high-level functions to interact with `Ultimap2.sys` if it's loaded, or it might be able to use some of Ultimap2's APIC functionalities.
*   **Cheat Engine Main Application:** Interacts with these kernel components via IOCTLs to control tracing and retrieve data. `frmUltimapUnit.pas` and `frmUltimap2Unit.pas` are the UI frontends for these features.
*   **DBVM (Optional but likely for some use-cases):** While these features can leverage direct hardware access from kernel mode, DBVM could be used in conjunction to:
    *   Provide even greater stealth for MSR configurations.
    *   Ensure tracing is strictly confined to a specific guest process CR3 without interference from other processes if the hardware filtering is insufficient or too complex to manage from the driver alone.
    *   Handle VM Exits caused by MSR accesses to PT MSRs if the guest is not permitted to access them directly.
*   **CPU Hardware:** Heavily dependent on Intel-specific CPU features (BTS for Ultimap, Intel PT for Ultimap2).

Both Ultimap versions represent sophisticated tracing tools that give Cheat Engine deep insight into program execution, valuable for reverse engineering and bypassing protections. Ultimap2, using Intel PT, is the more powerful and modern of the two.
```
