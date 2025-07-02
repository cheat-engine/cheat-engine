```markdown
# Sub-Module: DBVM - Bootsector & VM Loader

This sub-module describes the traditional (legacy BIOS-based) boot process for the Dark Byte Virtual Machine (DBVM). It involves a multi-stage loading process that starts from the boot sector of a disk image (`vmdisk.img` or `vmcd.iso`) and culminates in launching the DBVM hypervisor.

## Components

1.  **Boot Sector (`dbvm/bootsector/bootloader.asm`)**
    *   **Language:** 16-bit x86 Assembly.
    *   **Purpose:** This is the very first code executed when booting from a DBVM disk image. Its primary responsibility is to load the second-stage loader (`vmloader`) into memory.
    *   **Functionality:**
        *   **Initialization:** Performs minimal hardware initialization, sets up basic segment registers (DS, ES, SS), and a stack in low memory.
        *   **Disk Parameter Detection:** Uses BIOS INT 13h services to determine disk geometry (sectors per track, number of heads).
        *   **Loading Second Stage:** Reads the `vmloader` program from subsequent sectors of the boot disk into a fixed memory location (typically `0x30000`, which is `3000h:0000h`). It displays progress (dots) on the screen during loading.
        *   **Boot Drive Information:** Stores the BIOS boot drive number for later use by the loader.
        *   **Execution Transfer:** After successfully loading `vmloader`, it performs a far jump to the entry point of `vmloader` (e.g., `JMP 3000h:0000h`).
    *   **Build:** Assembled into a 512-byte boot sector image. The `dbvm/imagemaker/` tool is responsible for placing this boot sector and the `vmloader` onto the final disk image.

2.  **VM Loader (`dbvm/vmloader/vmloader.asm` and `dbvm/vmloader/vmloaderc.c`)**
    *   This is the second-stage loader, executed after `bootloader.asm`. It handles more complex tasks to prepare for and load the main DBVM hypervisor.

    *   **`vmloader.asm` (16-bit Real Mode -> 32-bit Protected Mode):**
        *   **Purpose:** Gathers essential system information, transitions the CPU to 32-bit protected mode, and then calls the C portion of the loader.
        *   **Functionality:**
            *   **Entry Point (`amain`):** Starts execution at the address where `bootloader.asm` loaded it (e.g., `0x30000`).
            *   **System Information Gathering:**
                *   Uses BIOS INT 15h, EAX=E820h to get the system's physical memory map (Address Range Descriptors - ARDs). This map is critical for finding available high memory to load the VMM.
                *   Checks and enables the A20 line to allow access to memory above 1MB.
            *   **GDT Setup:** Defines and loads a Global Descriptor Table (GDT) using `LGDT`. This GDT includes descriptors for 16-bit and 32-bit code and data segments, necessary for protected mode and later for transitioning to 64-bit long mode.
            *   **Transition to Protected Mode:** Sets the PE (Protection Enable) bit in the CR0 register.
            *   **Jump to 32-bit Code:** Executes a far jump to a 32-bit code segment within `vmloader.asm`.
            *   **32-bit Initialization:** Sets up 32-bit segment registers (DS, ES, FS, GS, SS) and the stack pointer.
            *   **Call C Loader:** Calls the `_vmloader_main` function, which is the entry point of `vmloaderc.c`.
            *   **`gotoVMM` Routine:** An assembly routine callable from C. It performs the final steps to launch the 64-bit VMM: loads the VMM's CR3 (page table base), enables PAE/PSE, sets Long Mode Enable (LME) in the EFER MSR, re-enables paging under VMM's control, and does a far jump to the VMM's 64-bit entry point.
            *   **`readsectorasm`:** A helper function to read disk sectors using INT 13h, involving temporary switches back to real mode if called from protected mode.

    *   **`vmloaderc.c` (32-bit C Code):**
        *   **`_vmloader_main()`:**
            *   **Find High Memory:** Parses the ARD (memory map) to find the highest available block of physical RAM large enough to contain the DBVM VMM code (size defined by `VMMSIZE` macro).
            *   **Load VMM:** Reads the main VMM binary (from `dbvm/vmm/`) from the disk into the identified high memory region. The starting sector of the VMM on disk is typically stored in the boot sector of the VMM image itself (which `vmloader.asm` would have loaded first to get this info).
            *   **Setup VMM Paging:**
                *   Allocates physical memory pages immediately following the loaded VMM code for the VMM's initial page tables (PML4, PDPT, Page Directories, Page Tables).
                *   Populates these tables to identity-map the lower portion of physical memory (e.g., 0-4MB for BIOS, transition code) and to map the physical memory where the VMM was loaded to its designated virtual address (e.g., `0x00400000`). Also maps memory for the VMM's initial stack.
            *   **Prepare `INITVARS`:** Fills a specific structure (named `INITVARS`, located at a fixed offset like `0x10` within the loaded VMM image) with crucial physical addresses and configuration data needed by the VMM to initialize itself. This includes:
                *   `loadedOS = 0` (indicating DBVM is booting directly, not hyperjacking).
                *   `vmmstart` (physical address of VMM code).
                *   `pagedirlvl4` (physical address of VMM's PML4 table).
                *   `nextstack` (initial stack pointer for VMM).
                *   Information about any extra contiguous memory allocated.
            *   **Initiate VMM Launch:** Calls the `gotoVMM` assembly routine in `vmloader.asm` to perform the final CPU mode switches and jump to the VMM's 64-bit entry point.

## Boot Flow Summary

1.  **BIOS Execution:** The system BIOS loads the 512-byte boot sector (`bootloader.asm`) from the DBVM disk image into memory at `0x7C00` and jumps to it.
2.  **Stage 1 Loader (`bootloader.asm`):**
    *   Sets up a basic 16-bit real-mode environment.
    *   Loads the second-stage loader (`vmloader` - `vmloader.asm` and linked C code) from the disk into memory at address `0x30000`.
    *   Jumps to `0x30000` to start `vmloader`.
3.  **Stage 2 Loader (`vmloader.asm` - 16-bit part):**
    *   Gathers system memory map (ARDs using INT 15h, E820h).
    *   Enables the A20 line.
    *   Initializes and loads a GDT.
    *   Switches the CPU into 32-bit protected mode.
    *   Jumps to its own 32-bit code segment.
4.  **Stage 2 Loader (`vmloader.asm` calls `vmloaderc.c` - 32-bit part):**
    *   `_vmloader_main` in C determines the highest free physical memory region.
    *   Loads the main DBVM hypervisor code (from `dbvm/vmm/`) from disk into this high memory.
    *   Constructs initial page tables for the VMM.
    *   Populates the `INITVARS` structure within the loaded VMM image with necessary physical addresses (VMM code location, VMM PML4, VMM stack).
    *   Calls the `gotoVMM` assembly routine.
5.  **Final Transition (`vmloader.asm` - `gotoVMM` routine):**
    *   Enables PAE and Long Mode (if 64-bit target).
    *   Loads CR3 with the VMM's PML4 address.
    *   Re-enables paging (now controlled by VMM's page tables).
    *   Performs a far jump to the VMM's 64-bit entry point (e.g., virtual address `0x00400000`).
6.  **DBVM Execution:** The DBVM hypervisor (`dbvm/vmm/main.c`) begins execution.

This sequence allows DBVM to gain control of the hardware from a cold boot, setting itself up as the host environment before any conventional operating system kernel is loaded.
```
