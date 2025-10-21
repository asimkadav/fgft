Fine-Grained Fault Tolerance (FGFT)
====================================

**ASPLOS'13 Paper**: "Fine-Grained Fault Tolerance using Device Checkpoints"
**Authors**: Asim Kadav, Matthew J. Renzelmann, Michael M. Swift
**Institution**: University of Wisconsin-Madison

## Overview

Fine-Grained Fault Tolerance (FGFT) isolates driver code at the granularity of a **single entry point** and executes driver code as **transactions** with automatic rollback on driver failures.

### Key Features

- **Device Checkpoint/Restore**: Refactors power management code to checkpoint device register state
- **Code Splitting**: CIL-based tool generates kernel stubs and SFI-instrumented user-mode code
- **Software Fault Isolation**: Instruments memory accesses to detect faults at runtime
- **Transactional Execution**: Entry points execute as transactions with automatic rollback
- **Fault Recovery**: Kernel undo log + device state restoration on failure

## Quick Start

```bash
# 1. Build CIL transformation tool
cd cil && ./configure && make && cd ..

# 2. Build IPC/transaction layer
cd ipc_miscdevice && make && cd ..

# 3. Build a driver (e.g., e1000)
cd ipc_drivers && make clean && make

# 4. Load modules
sudo insmod ipc_miscdevice/mischelp.ko
sudo insmod ipc_drivers/e1000/e1000-stub.ko
```

## Documentation

📖 **[README_REPRODUCIBLE.md](README_REPRODUCIBLE.md)** - Complete step-by-step guide to reproduce the ASPLOS'13 paper
🔧 **[KERNEL_5.0_MIGRATION_PLAN.md](KERNEL_5.0_MIGRATION_PLAN.md)** - Plan for modernizing to Linux kernel 5.0
🔬 **[CIL_TRANSFORMATION_PIPELINE.md](CIL_TRANSFORMATION_PIPELINE.md)** - Detailed CIL code transformation flow

## Repository Structure

```
/cil/                   - C Intermediate Language (OCaml-based code transformation tool)
/ipc_miscdevice/        - Kernel module (transaction manager, IPC, object tracking)
/ipc_drivers/           - Driver implementations (14 drivers supported)
  ├── e1000/            - Intel Gigabit Ethernet
  ├── r8169/            - Realtek 8169 Ethernet
  ├── cmipci/           - C-Media PCI Audio
  └── .../              - See README_REPRODUCIBLE.md for full list
/common/                - Shared headers and marshaling code
/kernel/                - Kernel fault handler integration
```

## Supported Drivers (14 Total)

**Network**: e1000, r8169, r8139too, forcedeth, pegasus
**Sound**: cmipci, ca0106, ens1371, usb-audio
**Other**: psmousebase, usb-storage, pci_test, usb_test

## How It Works

```
Entry Point → Checkpoint → Execute (SFI) → Success ✓
                                         ↓ Failure ✗
                         Restore Device ← Rollback Kernel State
```

See [README_REPRODUCIBLE.md](README_REPRODUCIBLE.md) for detailed explanation.

## System Requirements

**Current (Kernel 2.6.29)**:
- GCC 4.3-4.6
- OCaml 3.10-3.12
- Linux kernel 2.6.29 source tree

**Migration Target (Kernel 5.0)**:
- GCC 7.x-9.x
- OCaml 4.x
- See [KERNEL_5.0_MIGRATION_PLAN.md](KERNEL_5.0_MIGRATION_PLAN.md)

## Citation

```bibtex
@inproceedings{kadav2013fgft,
  title={Fine-Grained Fault Tolerance using Device Checkpoints},
  author={Kadav, Asim and Renzelmann, Matthew J and Swift, Michael M},
  booktitle={ASPLOS '13},
  year={2013}
}
```

## License

GPL v2

## Contact

- **Original Repository**: https://github.com/asimkadav/fgft
- **Issues**: https://github.com/asimkadav/fgft/issues
