# Fine-Grained Fault Tolerance (FGFT) - Reproducible Research Guide

## Paper Reference
**"Fine-Grained Fault Tolerance using Device Checkpoints"**
Published in ASPLOS '13
Authors: Asim Kadav, Matthew J. Renzelmann, Michael M. Swift
University of Wisconsin-Madison

---

## Table of Contents

1. [Overview](#overview)
2. [System Architecture](#system-architecture)
3. [Prerequisites](#prerequisites)
4. [Installation & Setup](#installation--setup)
5. [Step-by-Step: Building FGFT](#step-by-step-building-fgft)
6. [How FGFT Works](#how-fgft-works)
7. [Running Experiments](#running-experiments)
8. [Understanding the Output](#understanding-the-output)
9. [Troubleshooting](#troubleshooting)
10. [Extending FGFT](#extending-fgft)

---

## Overview

### What is FGFT?

Fine-Grained Fault Tolerance (FGFT) is a system that isolates driver code at the granularity of a **single entry point** and executes driver code as **transactions** with automatic rollback on failure.

### Key Features (Per ASPLOS'13 Paper)

1. **Device Checkpoint/Restore**: Refactors power management (suspend/resume) code to checkpoint device register state
2. **Code Splitting**: Automatically splits driver code into kernel stubs and user-mode isolated components
3. **Software Fault Isolation (SFI)**: Instruments memory accesses to detect faults
4. **Marshaling**: Copies driver and kernel state across isolation boundaries
5. **Transactional Execution**: Entry points execute atomically with rollback on failure
6. **Fault Recovery**: Automatically restores device and kernel state on driver failures

### Current Status

- ✅ **Implemented**: Code splitting, SFI generation, marshaling, checkpoint/restore
- ⚠️ **Partial**: Transactional semantics, fault recovery (undo log)
- 📋 **Target**: Linux Kernel 2.6.29 (migration to 5.0 in progress)

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Original Device Driver                       │
│                    (e.g., e1000_main.c)                         │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
                    ┌────────────────┐
                    │  CIL Pipeline  │ ◄── Annotations (.annot.txt)
                    │  (OCaml Tool)  │
                    └────────┬───────┘
                             │
            ┌────────────────┼────────────────┐
            ▼                ▼                ▼
    ┌───────────────┐ ┌────────────┐ ┌──────────────┐
    │  Kernel Stub  │ │  SFI Copy  │ │ Merged Copy  │
    │  (.kernel.c)  │ │  (.sym.c)  │ │ (.merged.c)  │
    └───────┬───────┘ └─────┬──────┘ └──────┬───────┘
            │               │                │
            ▼               ▼                ▼
    ┌───────────────┐ ┌────────────┐ ┌──────────────┐
    │ driver-stub.ko│ │driver-sfi.ko│ │  Analysis   │
    └───────┬───────┘ └─────┬──────┘ └──────────────┘
            │               │
            └───────┬───────┘
                    │
                    ▼
         ┌────────────────────┐
         │   mischelp.ko      │ ◄── Transaction Manager
         │  (IPC + Tracking)  │     (Checkpoint/Restore)
         └────────────────────┘
```

### Transaction Flow

```
Entry Point Call
    │
    ├─► [1] CHECKPOINT: Save device state (pci_save_state)
    │
    ├─► [2] EXECUTE: Call SFI-instrumented driver function
    │       │
    │       ├─► Marshal arguments (copy kernel → user)
    │       ├─► Execute in isolated context
    │       ├─► Track allocations (klog)
    │       ├─► Detect faults (logRead/logWrite)
    │       └─► Demarshal results (copy user → kernel)
    │
    ├─► [3a] SUCCESS: Return normally
    │       └─► Commit klog
    │
    └─► [3b] FAILURE: Fault detected
            ├─► ROLLBACK: Restore device state (pci_restore_state)
            ├─► UNDO: Free allocations, release locks (klog rollback)
            └─► RETURN: Error to caller
```

---

## Prerequisites

### Hardware Requirements
- x86_64 CPU
- 4GB+ RAM
- 20GB+ disk space

### Software Requirements

**For Kernel 2.6.29 (Current):**
- Linux build environment (Ubuntu 10.04 or compatible)
- GCC 4.3-4.6
- OCaml 3.10-3.12
- Make, binutils, module-init-tools
- Kernel 2.6.29 source tree (with UML support)

**For Kernel 5.0 (Migration):**
- Modern Linux (Ubuntu 18.04+)
- GCC 7.x-9.x
- OCaml 4.x
- See `KERNEL_5.0_MIGRATION_PLAN.md` for details

---

## Installation & Setup

### Step 1: Clone Repository

```bash
git clone https://github.com/asimkadav/fgft.git
cd fgft
```

### Step 2: Install OCaml and Build Tools

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install -y \
    ocaml \
    ocaml-native-compilers \
    build-essential \
    linux-headers-$(uname -r) \
    libncurses5-dev \
    bc \
    flex \
    bison \
    libelf-dev
```

### Step 3: Build CIL (C Intermediate Language)

```bash
cd cil
./configure
make
cd ..
```

**Verify CIL installation:**
```bash
./cil/bin/cilly --version
```

### Step 4: Set Up Kernel Source Tree

**Option A: Use provided kernel (2.6.29)**
```bash
# Download and extract kernel 2.6.29
mkdir -p compiled_images
cd compiled_images
wget https://cdn.kernel.org/pub/linux/kernel/v2.6/linux-2.6.29.tar.xz
tar -xf linux-2.6.29.tar.xz
mv linux-2.6.29 2.6.29-uml-ipc
cd 2.6.29-uml-ipc

# Configure for UML (User-Mode Linux)
make ARCH=um defconfig
make ARCH=um menuconfig  # Enable required drivers

# Build kernel
make ARCH=um -j$(nproc)
cd ../../
```

**Option B: Use existing kernel headers**
```bash
# Modify Makefiles to point to /lib/modules/$(uname -r)/build
# See KERNEL_5.0_MIGRATION_PLAN.md for details
```

---

## Step-by-Step: Building FGFT

### Overview of Build Process

FGFT build has **6 major stages**:

1. **CIL Build**: Compile the code transformation tool
2. **Merge**: Combine all driver source files into one
3. **Analyze**: Extract entry points and propagate annotations
4. **Split**: Generate kernel and user-mode versions
5. **Instrument**: Add SFI checks
6. **Compile**: Build kernel modules

---

### Stage 1: Choose a Driver

Currently supported drivers (14 total):

**Network Drivers:**
- `e1000` - Intel Gigabit Ethernet (recommended for testing)
- `r8169` - Realtek 8169 Gigabit Ethernet
- `r8139too` - Realtek 8139 Fast Ethernet
- `forcedeth` - NVIDIA nForce Ethernet
- `pegasus` - Pegasus USB Ethernet

**Sound Drivers:**
- `cmipci` - C-Media PCI Audio
- `ca0106` - Creative CA0106 Audio
- `ens1371` - Ensoniq ES1371 AudioPCI
- `usb-audio` - USB Audio Devices

**Other:**
- `psmousebase` - PS/2 Mouse
- `usb-storage` - USB Storage
- `usb_test` - USB Test Driver
- `pci_test` - PCI Test Driver

**Edit `ipc_drivers/Makefile` lines 124-127:**
```makefile
export BECONS=false
export DRIVER_SOUND=false
export DRIVER_NAME=e1000
export DRIVER_SRC=e1000_ethtool.c e1000_main.c e1000_hw.c
```

---

### Stage 2: Build Misc Device (IPC Layer)

```bash
cd ipc_miscdevice
make clean
make
```

**Output**: `mischelp.ko` - Kernel module providing:
- IPC communication between kernel and user-space
- NOOKS object tracking
- Marshaling support
- Transaction management
- Checkpoint/restore invocation

**Verify:**
```bash
ls -lh mischelp.ko
file mischelp.ko  # Should show: Linux kernel module
```

---

### Stage 3: Build Driver with CIL Transformation

```bash
cd ../ipc_drivers
make clean

# This will:
# 1. Merge driver sources
# 2. Run CIL analysis
# 3. Generate split versions
# 4. Compile kernel modules
make all
```

**Detailed Build Stages** (what happens inside `make all`):

#### 3.1 Merge Source Files
```bash
# Invokes CIL with --merge flag
# Combines all .c files into DRIVER_NAME.merged.c
```
**Output**: `e1000/e1000.merged.c`

#### 3.2 Find Entry Points
```bash
# CIL analyzes merged file to find driver entry points:
# - probe/remove functions
# - Interrupt handlers
# - Network operations (ndo_*)
# - Timer callbacks
# - Power management (suspend/resume)
```
**Output**: `e1000/e1000.entry_points.txt`

**Example entry_points.txt:**
```
e1000_probe probe_pci
e1000_remove remove_pci
e1000_open ndo_open
e1000_xmit_frame ndo_start_xmit
e1000_intr interrupt_handler
e1000_suspend suspend_pm
e1000_resume resume_pm
```

#### 3.3 Annotation Propagation
```bash
# CIL propagates user/kern annotations through call graph
# Determines which functions run in kernel vs. user mode
# Uses modif_annots.h for manual annotations
```
**Output**: `e1000/e1000.all_annots.txt`

**Example all_annots.txt:**
```
e1000_probe USER
e1000_xmit_frame USER
netif_start_queue KERN
pci_read_config_word KERN
kmalloc KERN
```

#### 3.4 Generate Kernel Stub
```bash
# CIL generates kernel-side code
# Creates callouts to user-mode SFI component
```
**Output**: `e1000/e1000.kernel.c`

**Example kernel stub:**
```c
// In e1000.kernel.c
int e1000_open(struct net_device *netdev) {
    // Marshal arguments
    struct req_args args;
    args.function_id = FIFO_E1000_OPEN;
    fill_marshbuf("e1000_open", args.data, ...);

    // Call into SFI component via ioctl
    ioctl(misc_fd, IOCTL_DISPATCH, &args);

    // Demarshal return value
    return fetch_marshbuf(...);
}
```

#### 3.5 Generate SFI Version
```bash
# CIL generates user-mode SFI instrumented code
# Adds logRead/logWrite checks before memory accesses
```
**Output**: `e1000/e1000.sym.c`

**Example SFI instrumentation:**
```c
// In e1000.sym.c
void e1000_update_stats(struct e1000_adapter *adapter) {
    // Original: stats->rx_packets = ioread32(hw->mmio + RX_PKT);

    // Instrumented:
    void *addr = hw->mmio + RX_PKT;
    if (logRead(addr, "rx_packets", "e1000_update_stats", __FILE__, __LINE__) != 0) {
        odft_record_failure();
        return;
    }
    stats->rx_packets = ioread32(addr);
}
```

#### 3.6 Compile Kernel Modules
```bash
# Compile kernel stub
gcc -c e1000.kernel.c -o e1000.kernel.o
# Link with kernel build system
make -C /path/to/kernel M=$(PWD) modules
```
**Outputs**:
- `e1000/e1000-stub.ko` - Kernel stub module
- `e1000/e1000-sfi.ko` - SFI instrumented module (if user-mode daemon)

---

### Stage 4: Verify Build

```bash
# Check that all three critical files exist:
ls -lh ipc_drivers/e1000/e1000-stub.ko
ls -lh ipc_drivers/e1000/e1000-sfi.ko
ls -lh ipc_miscdevice/mischelp.ko
```

**Expected output:**
```
-rw-r--r-- 1 user user  245K e1000-stub.ko
-rw-r--r-- 1 user user  312K e1000-sfi.ko
-rw-r--r-- 1 user user   89K mischelp.ko
```

---

### Stage 5: Load Modules

```bash
# Must be root
sudo su

# Load misc device first (provides IPC)
insmod ipc_miscdevice/mischelp.ko

# Load driver stub
insmod ipc_drivers/e1000/e1000-stub.ko

# Verify
lsmod | grep -E "mischelp|e1000"
dmesg | tail -20
```

**Expected dmesg output:**
```
[  123.456] mischelp: Module loaded
[  123.457] mischelp: Created device /dev/mischelp
[  123.458] e1000: Intel(R) PRO/1000 Network Driver - FGFT version
[  123.459] e1000: Checkpoint support enabled
```

---

### Stage 6: Test Basic Functionality

```bash
# Check if network interface appeared
ip link show

# Look for e1000 interface (e.g., eth0)
# Try to bring it up
ip link set eth0 up
ip addr add 192.168.1.100/24 dev eth0

# Test connectivity
ping -c 3 192.168.1.1
```

---

## How FGFT Works

### 1. Device Checkpoint Implementation

**Paper Claim**: "We refactor a driver's power management code to provide device checkpoint and restore functions."

**Implementation**:

Drivers already have suspend/resume functions for power management. FGFT extracts the device state saving portion into separate `checkpoint()` and `restore()` functions.

**Example - E1000 Driver:**

**`ipc_drivers/e1000/e1000_main.c:4696-4712`**
```c
int e1000_checkpoint(struct pci_dev *pdev) {
    if (pdev == NULL) {
        pdev = e1000_pdev;  // Global device pointer
    }
    struct net_device *netdev = pci_get_drvdata(pdev);
    int retval = 0;

    // Save PCI configuration registers
    retval = pci_save_state(pdev);  // <-- Key: saves device registers

    return retval;
}
```

**`ipc_drivers/e1000/e1000_main.c:4815-4843`**
```c
int e1000_restore(struct pci_dev *pdev) {
    if (pdev == NULL) {
        pdev = e1000_pdev;
    }
    struct net_device *netdev = pci_get_drvdata(pdev);
    struct e1000_adapter *adapter = netdev_priv(netdev);
    struct e1000_hw *hw = &adapter->hw;

    if (netif_running(netdev)) {
        WARN_ON(test_bit(__E1000_RESETTING, &adapter->flags));
        e1000_down(adapter);  // Stop device
    }

    // Restore PCI configuration registers
    pci_restore_state(pdev);  // <-- Key: restores device registers

    e1000_reset(adapter);     // Reset device to known state
    ew32(WUS, ~0);

    if (netif_running(netdev))
        e1000_up(adapter);    // Restart device

    return 0;
}
```

**What is Checkpointed:**
- PCI configuration space (BARs, command register, etc.)
- Device-specific registers (in driver private structures)
- DMA ring buffer pointers
- Interrupt state

**What `pci_save_state()` does:**
- Reads all PCI config space (256 bytes for PCI, 4KB for PCIe)
- Stores in kernel memory
- Used by PM subsystem for suspend/resume

---

### 2. Transaction Mechanism

**Paper Claim**: "Entry points execute as transactions with automatic rollback."

**Implementation - `ipc_miscdevice/misc.c:462-552`:**

```c
void disp_user(char *function_name, struct req_args *misc_fnargs) {
    char fn_name[100], ckpt_fn_name[100], restore_fn_name[100];

    // === PHASE 1: CHECKPOINT ===
    // Save processor state
    __save_processor_state(&orig_context);  // Line 459
    crash_setup_regs(&orig_context.regs, NULL);

    // Invoke driver checkpoint function
    if (ckpt_fn_addr == NULL) {
        sprintf(ckpt_fn_name, "%s_stub:%s_checkpoint", ODFT_DRIVER_NAME, ODFT_DRIVER_NAME);
        ckpt_fn_addr = (int (*)) kallsyms_lookup_name(ckpt_fn_name);
    }
    ckpt_fn_addr(NULL);  // Line 508: Save device state

    // === PHASE 2: EXECUTE ===
    // Check if failure already recorded (from fault handler)
    if ((function_name == NULL) || (odft_failure_record == 1)) {
        printk("Recovery start\n");
        goto sfi_failed;
    }

    // Look up SFI wrapper function
    sprintf(fn_name, "%s_sfi:__MARSH_WRAP__%s", ODFT_DRIVER_NAME, function_name);
    sfi_fn = kallsyms_lookup_name(fn_name);

    // Call SFI instrumented function
    sfi_fn(misc_fnargs->data, &ret);  // Line 518

    return;  // Success path

sfi_failed:
    // === PHASE 3: ROLLBACK ===
    printk("Transaction failed, rolling back\n");

    // Rollback kernel state (undo allocations, locks)
    odft_rollback_klog();  // Line 527

    // Restore device state
    if (restore_fn_addr == NULL) {
        sprintf(restore_fn_name, "%s_stub:%s_restore", ODFT_DRIVER_NAME, ODFT_DRIVER_NAME);
        restore_fn_addr = (int (*)) kallsyms_lookup_name(restore_fn_name);
    }
    restore_fn_addr(NULL);  // Line 546: Restore device registers

    // Clear failure flag
    odft_failure_record = 0;
}
```

---

### 3. Fault Detection

**Paper Claim**: "SFI module detects memory access violations."

**Implementation - `common/wrappers_sfi.c:77-122`:**

```c
int logWrite(void *addr, char *what, char *where, char *file, int line) {
    void *checkaddr = addr;

    // Optional: Fault injection for testing
    if (odft_pointer_to_flip() > 0) {
        pointer_dereferenced++;
        if (pointer_dereferenced == odft_pointer_to_flip()) {
            checkaddr = addr - 180;  // Corrupt pointer
        }
    }

    // Check 1: NULL pointer detection
    if (addr == 0x0) {
        printk("FAULT: NULL pointer write at %s:%d in %s\n", file, line, where);
        odft_record_failure();
        self_destruct();
        return 75;
    }

    // Check 2: Range validation
    // Returns:
    // 0 = not in range table (invalid)
    // 1 = in kernel range (invalid from user-mode)
    // 2+ = in driver range (valid)
    if (odft_check_range_hash(where, checkaddr) < 2) {
        printk("FAULT: Invalid memory access at %p in %s\n", checkaddr, where);
        odft_record_failure();
        self_destruct();
        return 75;
    }

    return 0;  // Valid access
}
```

**Range Table** (`ipc_miscdevice/hashtable/range_tracker.c:76-89`):
```c
// Tracks all memory allocations per function
int odft_insert_range_hash(const char *fnname, void *start, unsigned long size) {
    struct rkey key;
    struct rvalue *value;

    key.fnname = fnname;
    key.start = start;
    key.end = start + size;

    value = malloc(sizeof(struct rvalue));
    value->fnname = strdup(fnname);
    value->start = start;
    value->end = start + size;

    hashtable_insert(range_hash, &key, value);
    return 0;
}
```

---

### 4. Kernel Undo Log

**Paper Claim**: "System maintains undo log to rollback kernel state."

**Implementation - `ipc_miscdevice/hashtable/klog.c:77-110, 174-202`:**

```c
// Log kernel function calls for undo
int odft_insert_klog_hash(const char *fnname, void *args, int argslen,
                          void *retval, int retvallen) {
    struct hkey key;
    struct hvalue *value;

    // Create log entry
    key.fnname = fnname;
    key.klogid = current_klogid++;

    value = kmalloc(sizeof(struct hvalue), GFP_ATOMIC);
    value->fnname = strdup(fnname);
    value->args = kmalloc(argslen, GFP_ATOMIC);
    memcpy(value->args, args, argslen);
    value->retval = kmalloc(retvallen, GFP_ATOMIC);
    memcpy(value->retval, retval, retvallen);

    hashtable_insert(klog_hash, &key, value);
    return 0;
}

// Rollback logged operations
void odft_rollback_klog(void) {
    struct hvalue *hval;
    int klogid = 1;

    // Iterate through all log entries
    while ((hval = odft_check_klog_hash("asim", klogid++)) != NULL) {
        // Undo allocations
        if (strcmp(hval->fnname, "kmalloc") == 0) {
            kfree(hval->retval);  // Free the allocation
        }

        // Undo locks
        if (strcmp(hval->fnname, "mutex_lock") == 0) {
            mutex_unlock(hval->args);  // Release the lock
        }

        if (strcmp(hval->fnname, "spin_lock_irqsave") == 0) {
            unsigned long flags = *(unsigned long *)hval->retval;
            spinlock_t *lock = (spinlock_t *)hval->args;
            spin_unlock_irqrestore(lock, flags);
        }

        // Add more undo operations as needed
    }
}
```

**Currently Tracked Operations:**
- ✅ `kmalloc` / `kfree`
- ✅ `mutex_lock` / `mutex_unlock`
- ✅ `spin_lock_irqsave` / `spin_unlock_irqrestore`
- ❌ DMA operations (not implemented)
- ❌ File operations (not implemented)
- ❌ Device register writes (not implemented)

---

### 5. Marshaling Data Across Boundaries

**Paper Claim**: "System marshals driver and kernel data structures."

**Implementation - `common/slave_master_ud_md_marshaling.h:4-23`:**

```c
// Marshal data into buffer
void fill_marshbuf(const char *fnname, void *_buf_, int *_off_,
                   int size, void *data) {
    memcpy(&((char *)_buf_)[*_off_], data, size);
    *_off_ += size;
}

// Marshal pointer (with object tracking)
void fill_marshbuf_ptr(const char *fnname, void *_buf_, int *_off_,
                       int size, void *data, void *ptr) {
    // Look up kernel pointer in NOOKS table
    void *user_ptr = odft_nooks_kern_to_user(ptr);
    memcpy(&((char *)_buf_)[*_off_], &user_ptr, sizeof(void *));
    *_off_ += sizeof(void *);
}

// Demarshal data from buffer
void fetch_marshbuf(const char *fnname, void *_buf_, int *_off_,
                    int size, void *data) {
    memcpy(data, &((char *)_buf_)[*_off_], size);
    *_off_ += size;
}

// Demarshal pointer (with object tracking)
void *fetch_marshbuf_ptr(const char *fnname, void *_buf_, int *_off_) {
    void *user_ptr;
    memcpy(&user_ptr, &((char *)_buf_)[*_off_], sizeof(void *));
    *_off_ += sizeof(void *);

    // Translate user pointer to kernel pointer
    return odft_nooks_user_to_kern(user_ptr);
}
```

**Example Generated Marshaling Code** (`ipc_drivers/e1000/e1000.sym.c`):
```c
void __MARSH_WRAP__e1000_xmit_frame(void *_buf_, struct marshret_struct *_ret_) {
    struct sk_buff *skb;
    struct net_device *netdev;
    int _off_ = 0;

    // === DEMARSHAL ARGUMENTS ===
    // Unmarshal skb pointer
    skb = fetch_marshbuf_ptr("e1000_xmit_frame", _buf_, &_off_);

    // Unmarshal netdev pointer
    netdev = fetch_marshbuf_ptr("e1000_xmit_frame", _buf_, &_off_);

    // === CALL ACTUAL FUNCTION ===
    int retval = e1000_xmit_frame(skb, netdev);

    // === MARSHAL RETURN VALUE ===
    _off_ = 0;
    fill_marshbuf("e1000_xmit_frame", _ret_->data, &_off_, sizeof(int), &retval);
    _ret_->len = _off_;
}
```

---

## Running Experiments

### Experiment 1: Fault-Free Operation

**Goal**: Verify basic functionality without faults.

```bash
# Load modules
sudo insmod ipc_miscdevice/mischelp.ko
sudo insmod ipc_drivers/e1000/e1000-stub.ko

# Configure network
sudo ip link set eth0 up
sudo ip addr add 10.0.0.1/24 dev eth0

# Test traffic
ping -c 100 10.0.0.2

# Check logs
dmesg | grep -E "checkpoint|restore|FAULT"
```

**Expected**: No faults, checkpoints called before each entry point.

---

### Experiment 2: Injected Fault Recovery

**Goal**: Inject a fault and verify recovery.

**Enable fault injection in SFI:**
```bash
# Edit common/wrappers_sfi.c
# Set pointer_to_flip = 50 (corrupt 50th pointer access)

# Rebuild
cd ipc_drivers/e1000
make clean && make

# Reload modules
sudo rmmod e1000-stub mischelp
sudo insmod ../../ipc_miscdevice/mischelp.ko
sudo insmod e1000-stub.ko

# Generate traffic to trigger fault
ping -c 200 10.0.0.2
```

**Expected dmesg output:**
```
[  345.123] FAULT: Invalid memory access at 0xffff8800abcd1234 in e1000_clean_rx
[  345.124] odft_record_failure: Fault detected, initiating recovery
[  345.125] Transaction failed, rolling back
[  345.126] odft_rollback_klog: Rolling back 15 operations
[  345.127] e1000_restore: Restoring device state
[  345.128] Recovery complete
```

---

### Experiment 3: Checkpoint Overhead Measurement

**Goal**: Measure performance overhead of checkpointing.

**Baseline (without FGFT):**
```bash
# Load original driver
sudo modprobe e1000

# Benchmark
iperf3 -c 10.0.0.2 -t 60
```

**With FGFT:**
```bash
# Load FGFT driver
sudo insmod ipc_miscdevice/mischelp.ko
sudo insmod ipc_drivers/e1000/e1000-stub.ko

# Benchmark
iperf3 -c 10.0.0.2 -t 60
```

**Metrics to collect:**
- Throughput (Mbps)
- Latency (ms)
- CPU usage (%)
- Checkpoint time (from dmesg timestamps)

---

## Understanding the Output

### Build Output Files

```
ipc_drivers/e1000/
├── e1000.merged.c              # Merged source (all .c files combined)
├── e1000.entry_points.txt      # List of entry points found
├── e1000.all_annots.txt        # USER/KERN annotations for all functions
├── e1000.kernel.c              # Generated kernel stub code
├── e1000.sym.c                 # Generated SFI instrumented code
├── e1000-stub.ko               # Compiled kernel module (stub)
├── e1000-sfi.ko                # Compiled SFI module (optional)
├── output_merged.txt           # Merge stage log
├── output_entry_points.txt     # Entry point analysis log
├── output_all_annots.txt       # Annotation propagation log
└── output_sym.txt              # SFI generation log
```

### Runtime Logs (dmesg)

**Checkpoint invocation:**
```
[  100.234] disp_user: Calling checkpoint for e1000
[  100.235] e1000_checkpoint: Saved device state
```

**Successful transaction:**
```
[  100.456] disp_user: Calling e1000_xmit_frame
[  100.457] __MARSH_WRAP__e1000_xmit_frame: Marshaling arguments
[  100.458] e1000_xmit_frame: Transmitting packet
[  100.459] disp_user: Transaction completed successfully
```

**Failed transaction with recovery:**
```
[  200.567] logWrite: FAULT detected at 0xdeadbeef in e1000_clean_rx
[  200.568] odft_record_failure: Setting failure flag
[  200.569] disp_user: Recovery start
[  200.570] odft_rollback_klog: Undoing 5 kmalloc operations
[  200.571] odft_rollback_klog: Releasing 2 locks
[  200.572] e1000_restore: Restoring PCI state
[  200.573] e1000_restore: Resetting device
[  200.574] disp_user: Recovery complete
```

---

## Troubleshooting

### Build Issues

**Problem**: CIL compilation fails
```
Error: ocamlopt not found
```
**Solution**: Install OCaml native compiler
```bash
sudo apt-get install ocaml-native-compilers
```

---

**Problem**: Kernel headers not found
```
fatal error: linux/module.h: No such file or directory
```
**Solution**: Install kernel headers
```bash
sudo apt-get install linux-headers-$(uname -r)
```

---

**Problem**: `kallsyms_lookup_name` undefined
```
WARNING: "kallsyms_lookup_name" undefined!
```
**Solution**: Enable CONFIG_KALLSYMS in kernel
```bash
cd /path/to/kernel/source
make menuconfig
# Navigate to: General setup -> Configure standard kernel features
# Enable: [*] Load all symbols for debugging/ksymoops
make && make modules
```

---

### Runtime Issues

**Problem**: Module load fails
```
insmod: ERROR: could not insert module mischelp.ko: Invalid module format
```
**Solution**: Kernel version mismatch
```bash
# Check kernel version
uname -r
# Rebuild module for current kernel
cd ipc_miscdevice
make clean
make KERNEL_VERSION=$(uname -r)
```

---

**Problem**: Device not found after loading
```
ip: Device "eth0" does not exist
```
**Solution**: Check dmesg for errors
```bash
dmesg | tail -50
# Look for probe failures or resource conflicts
```

---

**Problem**: Checkpoint function not found
```
Failed to find the checkpoint function for e1000_stub:e1000_checkpoint
```
**Solution**: Ensure checkpoint function is exported
```bash
# In e1000_main.c, add:
EXPORT_SYMBOL(e1000_checkpoint);
EXPORT_SYMBOL(e1000_restore);

# Rebuild driver
make clean && make
```

---

**Problem**: Immediate kernel panic on fault
```
Kernel panic - not syncing: Failed to restore device
```
**Solution**: Disable panic, use printk
```bash
# Edit ipc_miscdevice/misc.c
# Replace panic() with:
printk(KERN_ERR "Failed to restore device, continuing\n");
return -EIO;
```

---

## Extending FGFT

### Adding a New Driver

**Step 1**: Copy driver source to `ipc_drivers/newdriver/`

**Step 2**: Create annotation file `newdriver.annot.txt`
```
# Manual annotations (if CIL misclassifies)
function_name USER
another_function KERN
```

**Step 3**: Add checkpoint/restore functions
```c
int newdriver_checkpoint(void) {
    // Save device-specific state
    pci_save_state(my_pdev);

    // Save driver-specific registers
    save_my_device_registers();

    return 0;
}

int newdriver_restore(void) {
    // Restore PCI state
    pci_restore_state(my_pdev);

    // Reset device
    reset_my_device();

    // Restore driver registers
    restore_my_device_registers();

    return 0;
}

EXPORT_SYMBOL(newdriver_checkpoint);
EXPORT_SYMBOL(newdriver_restore);
```

**Step 4**: Update `ipc_drivers/Makefile`
```makefile
export DRIVER_NAME=newdriver
export DRIVER_SRC=newdriver_main.c newdriver_hw.c
export BECONS=false
export DRIVER_SOUND=false
```

**Step 5**: Update `ipc_miscdevice/misc.c`
```c
#define ODFT_DRIVER_NAME "newdriver"
```

**Step 6**: Build
```bash
cd ipc_drivers
make clean
make all
```

---

### Adding New Fault Checks

**Edit `common/wrappers_sfi.c`:**
```c
int logWrite(void *addr, char *what, char *where, char *file, int line) {
    // Existing checks...

    // NEW: Check for wild pointers
    if ((unsigned long)addr < 0x10000) {
        printk("FAULT: Suspicious low address %p at %s:%d\n", addr, file, line);
        odft_record_failure();
        return -1;
    }

    // NEW: Check for kernel-space access from user-mode
    if (is_kernel_addr(addr) && in_user_context()) {
        printk("FAULT: Kernel address access from user mode\n");
        odft_record_failure();
        return -1;
    }

    return 0;
}
```

---

### Improving Recovery

**Add more undo operations in `ipc_miscdevice/hashtable/klog.c`:**
```c
void odft_rollback_klog(void) {
    // Existing rollbacks...

    // NEW: Undo DMA mappings
    if (strcmp(hval->fnname, "dma_map_single") == 0) {
        dma_addr_t dma_addr = *(dma_addr_t *)hval->retval;
        struct device *dev = (struct device *)hval->args;
        dma_unmap_single(dev, dma_addr, ...);
    }

    // NEW: Undo file opens
    if (strcmp(hval->fnname, "filp_open") == 0) {
        struct file *file = (struct file *)hval->retval;
        filp_close(file, NULL);
    }
}
```

---

## Performance Tuning

### Reducing Checkpoint Overhead

**Option 1**: Lazy checkpointing (only checkpoint when driver state changes)
```c
static int device_state_dirty = 0;

void mark_state_dirty(void) {
    device_state_dirty = 1;
}

int newdriver_checkpoint(void) {
    if (!device_state_dirty)
        return 0;  // Skip checkpoint if state unchanged

    pci_save_state(my_pdev);
    device_state_dirty = 0;
    return 0;
}
```

**Option 2**: Incremental checkpointing (only save changed registers)
```c
int newdriver_checkpoint(void) {
    // Only save registers that changed since last checkpoint
    if (reg_modified[0])
        saved_regs[0] = read_reg(0);
    if (reg_modified[1])
        saved_regs[1] = read_reg(1);
    // ...
}
```

---

### Reducing SFI Overhead

**Option 1**: Selective instrumentation (only instrument critical paths)
```c
// In CIL annotation file:
hot_function NOKERN  # Don't instrument (performance-critical)
slow_function USER   # Full instrumentation
```

**Option 2**: Batch checks
```c
// Instead of checking every access:
int logWrite_batch(void *start, size_t len, ...) {
    // Check entire range once
    if (!is_range_valid(start, len)) {
        odft_record_failure();
        return -1;
    }
    return 0;
}
```

---

## Citation

If you use FGFT in your research, please cite:

```bibtex
@inproceedings{kadav2013fgft,
  title={Fine-Grained Fault Tolerance using Device Checkpoints},
  author={Kadav, Asim and Renzelmann, Matthew J and Swift, Michael M},
  booktitle={Proceedings of the 18th International Conference on Architectural Support for Programming Languages and Operating Systems (ASPLOS)},
  pages={473--484},
  year={2013},
  organization={ACM}
}
```

---

## License

GPL v2 - See individual file headers for details.

---

## Contact & Support

- **Original Author**: Asim Kadav (University of Wisconsin-Madison)
- **Repository**: https://github.com/asimkadav/fgft
- **Issues**: https://github.com/asimkadav/fgft/issues

---

## Appendix A: Complete Example Walkthrough

### Building and Testing E1000 Driver

**Terminal Session:**
```bash
# 1. Build CIL
cd fgft/cil
./configure && make
cd ..

# 2. Configure for E1000
cd ipc_drivers
vim Makefile  # Set DRIVER_NAME=e1000

# 3. Build misc device
cd ../ipc_miscdevice
make clean && make
# Output: mischelp.ko created

# 4. Build E1000 driver
cd ../ipc_drivers
make clean && make 2>&1 | tee build.log
# Output: e1000/e1000-stub.ko and e1000/e1000-sfi.ko created

# 5. Verify build
ls -lh e1000/*.ko
ls -lh ../ipc_miscdevice/mischelp.ko

# 6. Load modules
sudo insmod ../ipc_miscdevice/mischelp.ko
sudo insmod e1000/e1000-stub.ko

# 7. Check loaded
lsmod | grep -E "mischelp|e1000"
dmesg | tail -20

# 8. Configure network
sudo ip link set eth0 up
sudo ip addr add 192.168.1.100/24 dev eth0

# 9. Test
ping -c 10 192.168.1.1

# 10. Monitor checkpoints
dmesg | grep checkpoint

# 11. Unload
sudo rmmod e1000_stub
sudo rmmod mischelp
```

---

**Document Version**: 2.0
**Last Updated**: 2025-10-21
**Status**: Complete reproducible guide per ASPLOS'13 paper
