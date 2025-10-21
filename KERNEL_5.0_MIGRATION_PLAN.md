# Kernel 5.0 Migration Plan for FGFT

## Executive Summary

This document provides a comprehensive plan to modernize the Fine-Grained Fault Tolerance (FGFT) codebase from Linux kernel 2.6.29 (released March 2009) to Linux kernel 5.0 (released March 2019). This is a **10-year kernel gap** with significant API changes.

---

## 1. Current State Analysis

### 1.1 Current Kernel Version
- **Target**: Linux 2.6.29 (2009)
- **Architecture**: x86_64 (UML - User Mode Linux)
- **Build Path**: `../compiled_images/2.6.29-uml-ipc`

### 1.2 Files Requiring Updates

| Component | Files | Critical APIs Used |
|-----------|-------|-------------------|
| **Misc Device** | `ipc_miscdevice/*.c` | ioctl, suspend.h, kallsyms |
| **Network Drivers** | `ipc_drivers/{e1000,r8169,r8139too,forcedeth,pegasus}/*.c` | netdev, pci, pm_ops |
| **Sound Drivers** | `ipc_drivers/{cmipci,ca0106,ens1371,usb-audio}/*.c` | ALSA, pci, pm_ops |
| **USB Drivers** | `ipc_drivers/{pegasus,usb-storage,usb-audio}/*.c` | USB core, URBs |
| **Input Drivers** | `ipc_drivers/psmousebase/*.c` | serio, input |
| **Kernel Hooks** | `kernel/fault.c` | page fault handler |
| **Build System** | `*/Makefile` | Kbuild, module params |

---

## 2. Major Kernel API Changes (2.6.29 → 5.0)

### 2.1 MODULE INIT/EXIT MACROS (CRITICAL)

**Deprecated in 2.6.29, Removed in 3.8:**
```c
// OLD (2.6.29)
__devinit, __devexit, __devinitdata, __devexitdata
__devexit_p()

// NEW (5.0)
// Simply remove these macros
```

**Migration Required:**
- **Files Affected**: All driver files (40+ files)
- **Action**: Remove all instances of `__devinit`, `__devexit`, `__devexit_p()`
- **Example**:
  ```c
  // OLD
  static int __devinit e1000_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
  static void __devexit e1000_remove(struct pci_dev *pdev)
  .remove = __devexit_p(e1000_remove),

  // NEW
  static int e1000_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
  static void e1000_remove(struct pci_dev *pdev)
  .remove = e1000_remove,
  ```

---

### 2.2 POWER MANAGEMENT API CHANGES (CRITICAL)

**Deprecated in 2.6.32, Removed in 3.2:**
```c
// OLD (2.6.29)
static int driver_suspend(struct pci_dev *pdev, pm_message_t state);
static int driver_resume(struct pci_dev *pdev);
pci_set_power_state(pdev, pci_choose_state(pdev, state));

// NEW (5.0) - Use dev_pm_ops
static int driver_suspend(struct device *dev);
static int driver_resume(struct device *dev);
static SIMPLE_DEV_PM_OPS(driver_pm_ops, driver_suspend, driver_resume);
```

**Migration Required:**
- **Files Affected**: All drivers with checkpoint/restore (e1000, r8169, cmipci, etc.)
- **Impact**: **CRITICAL** - Checkpoint/restore mechanism depends on this
- **Action**: Convert to `dev_pm_ops` structure
- **Example**:
  ```c
  // OLD
  static struct pci_driver e1000_driver = {
      .name     = e1000_driver_name,
      .id_table = e1000_pci_tbl,
      .probe    = e1000_probe,
      .remove   = __devexit_p(e1000_remove),
      .suspend  = e1000_suspend,
      .resume   = e1000_resume,
  };

  // NEW
  static int e1000_pm_suspend(struct device *dev) {
      struct pci_dev *pdev = to_pci_dev(dev);
      return e1000_suspend(pdev);
  }

  static int e1000_pm_resume(struct device *dev) {
      struct pci_dev *pdev = to_pci_dev(dev);
      return e1000_resume(pdev);
  }

  static SIMPLE_DEV_PM_OPS(e1000_pm_ops, e1000_pm_suspend, e1000_pm_resume);

  static struct pci_driver e1000_driver = {
      .name     = e1000_driver_name,
      .id_table = e1000_pci_tbl,
      .probe    = e1000_probe,
      .remove   = e1000_remove,
      .driver.pm = &e1000_pm_ops,
  };
  ```

---

### 2.3 NETWORK DRIVER API CHANGES

**Net Device Operations (changed in 2.6.31):**
```c
// OLD (2.6.29)
dev->open = &e1000_open;
dev->stop = &e1000_close;
dev->hard_start_xmit = &e1000_xmit_frame;
dev->get_stats = &e1000_get_stats;
dev->set_multicast_list = &e1000_set_multi;
dev->do_ioctl = &e1000_ioctl;
dev->tx_timeout = &e1000_tx_timeout;
dev->change_mtu = &e1000_change_mtu;

// NEW (5.0) - Use net_device_ops
static const struct net_device_ops e1000_netdev_ops = {
    .ndo_open               = e1000_open,
    .ndo_stop               = e1000_close,
    .ndo_start_xmit         = e1000_xmit_frame,
    .ndo_get_stats          = e1000_get_stats,
    .ndo_set_rx_mode        = e1000_set_multi,  // renamed
    .ndo_do_ioctl           = e1000_ioctl,
    .ndo_tx_timeout         = e1000_tx_timeout,
    .ndo_change_mtu         = e1000_change_mtu,
    .ndo_validate_addr      = eth_validate_addr,
    .ndo_set_mac_address    = eth_mac_addr,
};

netdev->netdev_ops = &e1000_netdev_ops;
```

**NAPI Changes:**
```c
// OLD (2.6.29)
netif_rx_schedule(dev, &adapter->napi);
netif_rx_complete(dev, &adapter->napi);

// NEW (5.0)
napi_schedule(&adapter->napi);
napi_complete_done(&adapter->napi, work_done);
```

**Migration Required:**
- **Files Affected**: e1000, r8169, r8139too, forcedeth, pegasus
- **Action**: Create `net_device_ops` structures

---

### 2.4 PCI API CHANGES

**PCI Enable Device:**
```c
// OLD (2.6.29)
pci_enable_device(pdev);

// NEW (5.0) - More specific
pci_enable_device_mem(pdev);  // For MMIO only
// or
pci_enable_device(pdev);      // For both MMIO and I/O
```

**DMA Mapping:**
```c
// OLD (2.6.29)
pci_set_dma_mask(pdev, DMA_BIT_MASK(64));
pci_set_consistent_dma_mask(pdev, DMA_BIT_MASK(64));

// NEW (5.0)
dma_set_mask_and_coherent(&pdev->dev, DMA_BIT_MASK(64));
```

---

### 2.5 TIMER API CHANGES

**Timer Setup (changed in 4.15):**
```c
// OLD (2.6.29)
void init_timer(struct timer_list *timer);
timer->function = callback;
timer->data = (unsigned long)priv;
setup_timer(timer, callback, (unsigned long)priv);

// NEW (5.0)
void timer_setup(struct timer_list *timer,
                 void (*callback)(struct timer_list *),
                 unsigned int flags);

// Callback signature changed:
// OLD: void callback(unsigned long data)
// NEW: void callback(struct timer_list *t)
```

**Migration Required:**
- **Files Affected**: All drivers using timers
- **Action**: Convert timer callbacks to new signature using `from_timer()`

---

### 2.6 IOCTL CHANGES

**File Operations:**
```c
// OLD (2.6.29)
static int misc_ioctl(struct inode *inode, struct file *file,
                      unsigned int cmd, unsigned long arg);

.ioctl = misc_ioctl,

// NEW (5.0)
static long misc_ioctl(struct file *file,
                       unsigned int cmd, unsigned long arg);

.unlocked_ioctl = misc_ioctl,
.compat_ioctl = misc_ioctl,  // For 32-bit apps on 64-bit kernel
```

**Migration Required:**
- **Files Affected**: `ipc_miscdevice/misc.c`, `ipc_miscdevice/mischelper.c`
- **Action**: Convert to `unlocked_ioctl`

---

### 2.7 MEMORY ALLOCATION CHANGES

**GFP Flags:**
```c
// OLD (2.6.29)
GFP_KERNEL, GFP_ATOMIC still valid

// NEW (5.0)
// Added: GFP_NOWAIT, __GFP_ACCOUNT, etc.
// No breaking changes for basic usage
```

**Action**: No changes required for basic kmalloc/kfree usage

---

### 2.8 LOCK API CHANGES (MINIMAL)

**Spinlocks:**
```c
// OLD and NEW are compatible
spin_lock_irqsave(&lock, flags);
spin_unlock_irqrestore(&lock, flags);
```

**Action**: No changes required

---

### 2.9 USB API CHANGES

**URB Submission:**
```c
// OLD (2.6.29)
usb_submit_urb(urb, GFP_KERNEL);
usb_kill_urb(urb);

// NEW (5.0)
// Same API, compatible
```

**USB Driver Structure:**
```c
// OLD (2.6.29)
.suspend = usb_suspend,
.resume = usb_resume,

// NEW (5.0)
.suspend = usb_suspend,
.resume = usb_resume,
.reset_resume = usb_reset_resume,  // Added
```

---

### 2.10 SOUND (ALSA) API CHANGES

**Minimal Changes:**
- Most ALSA APIs are stable
- `snd_card_create()` vs `snd_card_new()` (changed in 2.6.30)
- PM ops changed to `dev_pm_ops`

**Migration Required:**
- **Files Affected**: cmipci, ca0106, ens1371, usb-audio
- **Action**: Verify ALSA API compatibility, update PM ops

---

### 2.11 KPROBE/MMIO TRACE CHANGES

**Kernel Fault Handler:**
```c
// OLD (2.6.29) - Direct fault handler modification
// In kernel/fault.c

// NEW (5.0)
// May require kprobes or kernel function hooking
// Check if direct modification still works
```

**Action**: Test and verify fault.c integration with 5.0

---

### 2.12 KALLSYMS CHANGES

**Symbol Lookup:**
```c
// OLD and NEW compatible
kallsyms_lookup_name("symbol_name");
```

**However:** In 5.7+, `kallsyms_lookup_name` is no longer exported
- **Workaround for 5.7+**: Use kprobes to get address

**Action**: For 5.0, no changes needed

---

### 2.13 PROCESSOR STATE SAVE/RESTORE

**Current Code:**
```c
#include <linux/suspend.h>
extern __restore_processor_state(struct saved_context *);
extern __save_processor_state(struct saved_context *);
```

**Issue**: These are arch-specific internal functions
- May not be exported in 5.0
- Header location may change

**Action**:
1. Check if still available in arch/x86/include/asm/suspend_64.h
2. May need to use alternative like `struct pt_regs` only
3. Test if symbols are exported

---

## 3. Build System Changes

### 3.1 Kbuild Changes

**Module Build:**
```makefile
# OLD (2.6.29)
obj-m := module.o
module-objs := file1.o file2.o

# NEW (5.0) - Preferred
obj-m := module.o
module-y := file1.o file2.o
```

**Kernel Source Path:**
```makefile
# OLD
KERNEL_VERSION = 2.6.29-uml-ipc
$(MAKE) -C ../compiled_images/$(KERNEL_VERSION) M=`pwd` modules

# NEW
KERNEL_VERSION = 5.0.0
$(MAKE) -C /lib/modules/$(KERNEL_VERSION)/build M=$(PWD) modules
```

---

### 3.2 CIL/OCaml Compatibility

**CIL Version:**
- Current: CIL 1.3.6-1.7 (circa 2006-2010)
- May need updates for newer GCC preprocessor output

**Action**:
1. Test CIL with kernel 5.0 headers
2. May need to update CIL to handle newer C constructs
3. Check if newer GCC versions (7.x-9.x) are compatible

---

## 4. Migration Strategy

### Phase 1: Preparation (Week 1)
1. ✅ Document all deprecated APIs
2. ✅ Create migration checklist
3. Set up kernel 5.0 build environment
4. Clone codebase to migration branch

### Phase 2: Build System Update (Week 1-2)
1. Update Makefiles for kernel 5.0
2. Update kernel header paths
3. Test CIL compatibility with 5.0 headers
4. Update module build scripts

### Phase 3: Core API Updates (Week 2-3)
1. **Remove deprecated macros**: `__devinit`, `__devexit`, `__devexit_p()`
2. **Update PM ops**: Convert to `dev_pm_ops` (CRITICAL for checkpoint/restore)
3. **Update ioctl**: Convert to `unlocked_ioctl`
4. **Verify processor state save/restore**: Check symbol availability

### Phase 4: Driver-Specific Updates (Week 3-5)
1. **Network Drivers** (e1000, r8169, r8139too, forcedeth, pegasus):
   - Convert to `net_device_ops`
   - Update NAPI calls
   - Update PM ops
2. **Sound Drivers** (cmipci, ca0106, ens1371, usb-audio):
   - Update ALSA APIs
   - Update PM ops
3. **USB Drivers** (pegasus, usb-storage, usb-audio):
   - Verify USB API compatibility
4. **Input Drivers** (psmousebase):
   - Verify serio API compatibility

### Phase 5: Kernel Integration (Week 5-6)
1. Update `kernel/fault.c` for 5.0
2. Test fault handler integration
3. Verify `kallsyms_lookup_name` availability

### Phase 6: Testing & Validation (Week 6-8)
1. Build all drivers for kernel 5.0
2. Test checkpoint/restore functionality
3. Test SFI instrumentation
4. Test fault injection
5. Validate transaction mechanism
6. Performance testing

---

## 5. Detailed File-by-File Changes

### 5.1 ipc_miscdevice/misc.c

**Lines 28-33: Processor State Functions**
```c
// CURRENT (2.6.29)
#include <linux/suspend.h>
extern __restore_processor_state(struct saved_context *);
extern __save_processor_state(struct saved_context *);
static struct saved_context orig_context;

// UPDATED (5.0)
#include <asm/suspend.h>  // Arch-specific header
// Check if symbols are exported, if not:
// Use pt_regs only or implement alternative
```

**Lines 600+: IOCTL Function**
```c
// CURRENT (2.6.29)
static int misc_ioctl(struct inode *inode, struct file *file,
                     unsigned int cmd, unsigned long arg)

static const struct file_operations misc_fops = {
    .owner = THIS_MODULE,
    .ioctl = misc_ioctl,
};

// UPDATED (5.0)
static long misc_ioctl(struct file *file,
                      unsigned int cmd, unsigned long arg)

static const struct file_operations misc_fops = {
    .owner = THIS_MODULE,
    .unlocked_ioctl = misc_ioctl,
    .compat_ioctl = misc_ioctl,
};
```

---

### 5.2 ipc_drivers/e1000/e1000_main.c

**Module Init/Exit:**
```c
// Remove __devinit, __devexit throughout file
// Line ~4600:
static int e1000_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
static void e1000_remove(struct pci_dev *pdev)
```

**Power Management:**
```c
// Add PM ops wrapper
static int e1000_pm_suspend(struct device *dev) {
    struct pci_dev *pdev = to_pci_dev(dev);
    return e1000_suspend(pdev, PMSG_SUSPEND);
}

static int e1000_pm_resume(struct device *dev) {
    struct pci_dev *pdev = to_pci_dev(dev);
    return e1000_resume(pdev);
}

static SIMPLE_DEV_PM_OPS(e1000_pm_ops, e1000_pm_suspend, e1000_pm_resume);

static struct pci_driver e1000_driver = {
    .name     = e1000_driver_name,
    .id_table = e1000_pci_tbl,
    .probe    = e1000_probe,
    .remove   = e1000_remove,
    .driver.pm = &e1000_pm_ops,
    .shutdown = e1000_shutdown,
    .err_handler = &e1000_err_handler
};
```

**Net Device Ops:**
```c
static const struct net_device_ops e1000_netdev_ops = {
    .ndo_open               = e1000_open,
    .ndo_stop               = e1000_close,
    .ndo_start_xmit         = e1000_xmit_frame,
    .ndo_get_stats          = e1000_get_stats,
    .ndo_set_rx_mode        = e1000_set_multi,
    .ndo_set_mac_address    = e1000_set_mac,
    .ndo_tx_timeout         = e1000_tx_timeout,
    .ndo_change_mtu         = e1000_change_mtu,
    .ndo_do_ioctl           = e1000_ioctl,
    .ndo_validate_addr      = eth_validate_addr,
    .ndo_vlan_rx_add_vid    = e1000_vlan_rx_add_vid,
    .ndo_vlan_rx_kill_vid   = e1000_vlan_rx_kill_vid,
#ifdef CONFIG_NET_POLL_CONTROLLER
    .ndo_poll_controller    = e1000_netpoll,
#endif
};

// In probe function:
netdev->netdev_ops = &e1000_netdev_ops;
```

---

### 5.3 Similar Changes for Other Drivers

Apply same patterns to:
- **r8169**: PM ops + net_device_ops
- **r8139too**: PM ops + net_device_ops
- **forcedeth**: PM ops + net_device_ops
- **pegasus**: PM ops + net_device_ops
- **cmipci**: PM ops (dev_pm_ops)
- **ca0106**: PM ops (dev_pm_ops)
- **ens1371**: PM ops (dev_pm_ops)
- **psmousebase**: PM ops if applicable

---

## 6. Testing Checklist

### 6.1 Build Tests
- [ ] CIL compiles with kernel 5.0 headers
- [ ] All drivers build without errors
- [ ] Kernel modules (mischelp.ko, *-stub.ko, *-sfi.ko) build successfully
- [ ] No deprecated API warnings

### 6.2 Functional Tests
- [ ] Module loading/unloading works
- [ ] Checkpoint function is called correctly
- [ ] Restore function is called on failure
- [ ] SFI instrumentation works
- [ ] Marshaling/demarshaling works
- [ ] NOOKS object tracking works
- [ ] Fault injection triggers recovery

### 6.3 Driver-Specific Tests
- [ ] e1000: Network traffic works, checkpoint/restore works
- [ ] r8169: Network traffic works, checkpoint/restore works
- [ ] cmipci: Sound playback works, checkpoint/restore works
- [ ] Other drivers: Basic functionality + checkpoint/restore

---

## 7. Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Processor state save/restore unavailable** | CRITICAL | Use pt_regs only, or implement custom save |
| **PM ops changes break checkpoint** | CRITICAL | Thorough testing of dev_pm_ops conversion |
| **CIL incompatible with 5.0 headers** | HIGH | Update CIL or preprocess headers |
| **kallsyms_lookup_name removed (5.7+)** | MEDIUM | Use kprobes workaround |
| **Net device ops complex migration** | MEDIUM | Careful testing of all network ops |
| **Timer API changes** | LOW | Convert systematically |

---

## 8. Estimated Effort

| Phase | Effort | Duration |
|-------|--------|----------|
| Preparation | 20 hours | 1 week |
| Build System | 30 hours | 1-2 weeks |
| Core API Updates | 40 hours | 1-2 weeks |
| Driver Updates | 80 hours | 2-3 weeks |
| Kernel Integration | 30 hours | 1-2 weeks |
| Testing | 60 hours | 2-3 weeks |
| **Total** | **260 hours** | **8-12 weeks** |

---

## 9. Next Steps

1. Create git branch: `kernel-5.0-migration`
2. Set up kernel 5.0 build environment
3. Start with Phase 1: Build system updates
4. Update one driver completely as prototype (e1000)
5. Use prototype as template for other drivers
6. Commit incrementally with detailed commit messages

---

## 10. References

- [Linux Kernel Driver API Changes](https://www.kernel.org/doc/html/latest/driver-api/index.html)
- [Linux Device Drivers, 3rd Edition](https://lwn.net/Kernel/LDD3/)
- [Kernel Newbies: API Changes](https://kernelnewbies.org/LinuxVersions)
- [LWN.net Kernel Articles](https://lwn.net/Kernel/)

---

**Document Version**: 1.0
**Date**: 2025-10-21
**Author**: Claude Code Review
