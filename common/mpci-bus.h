#ifndef MPCI_BUS_H
#define MPCI_BUS_H

/*
 * Definitions for the virtual PCI bus.
 *
 * $Id: pcibus.h,v 1.4 2004/08/20 18:49:44 corbet Exp $
 */

#include <linux/device.h>
#include <linux/mod_devicetable.h>
#include <linux/pci_ids.h>
#include <linux/pci_regs.h>
#include <linux/ioport.h>

///////////////////////////////////////////////////////////////////////////////
// Forward declarations
///////////////////////////////////////////////////////////////////////////////
extern struct bus_type pci_bus_type;

///////////////////////////////////////////////////////////////////////////////
// Type definitions
///////////////////////////////////////////////////////////////////////////////
typedef int __bitwise pci_power_t;

/*
 *  For PCI devices, the region numbers are assigned this way:
 */
enum {
    /* #0-5: standard PCI resources */
    PCI_STD_RESOURCES,
    PCI_STD_RESOURCE_END = 5,
    
    /* #6: expansion ROM resource */
    PCI_ROM_RESOURCE,
    
    /* resources assigned to buses behind the bridge */
#define PCI_BRIDGE_RESOURCE_NUM 4
    
    PCI_BRIDGE_RESOURCES,
    PCI_BRIDGE_RESOURCE_END = PCI_BRIDGE_RESOURCES +
    PCI_BRIDGE_RESOURCE_NUM - 1,
    
    /* total resources associated with a PCI device */
    PCI_NUM_RESOURCES,
    
    /* preserve this for compatibility */
    DEVICE_COUNT_RESOURCE
};

struct pci_dynids {
    spinlock_t lock;            /* protects list, index */
    struct list_head list;      /* for IDs added at runtime */
};

/*
 * The PCI driver type.
 */

struct pci_driver {
    char *__attribute__((nullterm)) name; // MJR
    const struct pci_device_id *id_table;
    
    int  (*probe)  (struct pci_dev *dev, const struct pci_device_id *id);   /* New device inserted */
    void (*remove) (struct pci_dev *dev);   /* Device removed (NULL if not a hot-plug capable driver) */
    int  (*suspend) (struct pci_dev *dev, pm_message_t state);      /* Device suspended */
    int  (*suspend_late) (struct pci_dev *dev, pm_message_t state);
    int  (*resume_early) (struct pci_dev *dev);
    int  (*resume) (struct pci_dev *dev);                   /* Device woken up */
    void (*shutdown) (struct pci_dev *dev);

    struct pci_error_handlers *err_handler;
    struct device_driver driver;
    struct pci_dynids dynids;
};

struct pci_bus {
    unsigned char   number;         /* bus number */
    unsigned char   primary;        /* number of primary bridge */
    unsigned char   secondary;      /* number of secondary bridge */
    unsigned char   subordinate;    /* max number of subordinate buses */
    char            name[48];
};

/**
 * DEFINE_PCI_DEVICE_TABLE - macro used to describe a pci device table
 * @_table: device table name
 *
 * This macro is used to create a struct pci_device_id array (a device table)
 * in a generic manner.
 */
#define DEFINE_PCI_DEVICE_TABLE(_table)                         \
    const struct pci_device_id _table[] __devinitconst

/*
 * A device type for things "plugged" into the PCI bus.
 */

//
// TODO: we should really have a way of tracing the entire state of
// the PCI configuration space or something like that.  e.g. why'
// do we need capabilities as well as a resource arrays -- where do
// these come from?
//
struct pci_dev {
    char *__attribute__((nullterm)) name; // MJR
    struct pci_driver *driver;
    struct pci_bus *bus;
    struct pci_bus *subordinate;
    struct device dev;

    unsigned int    devfn;          /* encoded device & function index */
    unsigned short  vendor;
    unsigned short  device;
    unsigned short  subsystem_vendor;
    unsigned short  subsystem_device;
    unsigned int    class;          /* 3 bytes: (base,sub,prog-if) */
    u8              revision;       /* PCI revision, low byte of class word */
    //u8              hdr_type;       /* PCI header type (`multi' flag masked out) */
    //u8              pcie_type;      /* PCI-E device/port type */
    //u8              rom_base_reg;   /* which config register controls the ROM */
    //u8              pin;            /* which interrupt pin this device uses */

    u64             dma_mask;       /* Mask of the bits of bus address this
                                       device implements.  Normally this is
                                       0xffffffff.  You only need to change
                                       this if your device has broken DMA
                                       or supports 64-bit transfers.  */
    int             pm_cap;         /* PM capability offset in the
                                       configuration space */
    
    /*
     * Instead of touching interrupt line and base address registers
     * directly, use the values stored here. They might be different!
     */
    unsigned int    irq;
    struct resource resource[DEVICE_COUNT_RESOURCE]; /* I/O and memory regions + expansion ROMs */
    int capabilities[32]; // MJR Hack, See PCI_CAP_XXX identifiers
};

///////////////////////////////////////////////////////////////////////////////
// Macros and functions
///////////////////////////////////////////////////////////////////////////////

#define to_pci_driver(drv) container_of(drv, struct pci_driver, driver);
#define to_pci_dev(dev) container_of(dev, struct pci_dev, dev);

#define pci_resource_start(dev, bar)    ((dev)->resource[(bar)].start)
#define pci_resource_end(dev, bar)      ((dev)->resource[(bar)].end)
#define pci_resource_flags(dev, bar)    ((dev)->resource[(bar)].flags)
#define pci_resource_len(dev,bar) \
    ((pci_resource_start((dev), (bar)) == 0 &&                          \
      pci_resource_end((dev), (bar)) ==                                 \
      pci_resource_start((dev), (bar))) ? 0 :                           \
                                                                        \
     (pci_resource_end((dev), (bar)) -                                  \
      pci_resource_start((dev), (bar)) + 1))

// Exported Functions Section 1
int pci_bus_read_config_byte(struct pci_bus *bus, unsigned int devfn, int where, u8 *val);
int pci_bus_read_config_word(struct pci_bus *bus, unsigned int devfn, int where, u16 *val);
int pci_bus_read_config_dword(struct pci_bus *bus, unsigned int devfn, int where, u32 *val);
int pci_bus_write_config_byte(struct pci_bus *bus, unsigned int devfn, int where, u8 val);
int pci_bus_write_config_word(struct pci_bus *bus, unsigned int devfn, int where, u16 val);
int pci_bus_write_config_dword(struct pci_bus *bus, unsigned int devfn, int where, u32 val);
#define pci_read_config_byte(dev, where, val)                   \
    pci_bus_read_config_byte(dev->bus, dev->devfn, where, val)
#define pci_read_config_word(dev, where, val)                   \
    pci_bus_read_config_word(dev->bus, dev->devfn, where, val)
#define pci_read_config_dword(dev, where, val)                  \
    pci_bus_read_config_dword(dev->bus, dev->devfn, where, val)
#define pci_write_config_byte(dev, where, val)                  \
    pci_bus_write_config_byte(dev->bus, dev->devfn, where, val)
#define pci_write_config_word(dev, where, val)                  \
    pci_bus_write_config_word(dev->bus, dev->devfn, where, val)
#define pci_write_config_dword(dev, where, val)                         \
    pci_bus_write_config_dword(dev->bus, dev->devfn, where, val)

// Exported Functions Section 2
void pci_set_master(struct pci_dev *dev);
void pci_clear_master(struct pci_dev *dev);
int pci_enable_device(struct pci_dev *dev);
int pci_disable_device(struct pci_dev *dev);
int pci_set_dma_mask(struct pci_dev *dev, u64 mask);
int pci_set_consistent_dma_mask(struct pci_dev *dev, u64 mask);
int pci_request_regions(struct pci_dev *, const char *__attribute__((nullterm)) res_name);
void pci_release_regions(struct pci_dev *);

// Exported Functions Section 3
void *pci_get_drvdata(struct pci_dev *pdev);
void pci_set_drvdata(struct pci_dev *pdev, void *data);
static inline const char *pci_name(struct pci_dev *pdev) {
    return dev_name(&pdev->dev);
}

int pci_set_power_state(struct pci_dev *dev, pci_power_t state);
pci_power_t pci_choose_state(struct pci_dev *dev, pm_message_t state);
int pci_enable_wake(struct pci_dev *dev, pci_power_t state, int enable);

// Exported Functions Section 4
int pci_register_device(struct pci_dev *);
void pci_unregister_device(struct pci_dev *);
int __pci_register_driver(struct pci_driver *, struct module *, const char * __attribute__((nullterm)));
#define pci_register_driver(driver) __pci_register_driver(driver, THIS_MODULE, KBUILD_MODNAME)
void pci_unregister_driver(struct pci_driver *);

// Exported Functions Section 5
int pci_select_bars(struct pci_dev *dev, unsigned long flags);
int pci_find_capability(struct pci_dev *dev, int cap);
void pci_dma_sync_single_for_device(struct pci_dev *dev, dma_addr_t dma_addr,
                              size_t size, int direction);
int pci_request_selected_regions(struct pci_dev *pdev, int bars,
                           const char *res_name);
void pci_release_selected_regions(struct pci_dev *pdev, int bars);
int pci_enable_device_mem(struct pci_dev *dev);     
void pci_disable_msi(struct pci_dev *dev);
unsigned long pci_ioremap_bar(struct pci_dev *pdev, int bar);
int pci_set_mwi(struct pci_dev *dev);
void pci_clear_mwi(struct pci_dev *dev);
int pci_enable_msi(struct pci_dev* dev);
int pci_channel_offline(struct pci_dev *pdev);
int pcix_get_mmrbc(struct pci_dev *dev);
void pci_dma_sync_single_for_cpu(struct pci_dev *pdev, dma_addr_t dma_handle,
                                     size_t size, int direction);
int pcix_set_mmrbc(struct pci_dev *dev, int mmrbc);

// Added for cmipci
int pci_dev_present (const struct pci_device_id *ids);

//
// Added for tg3
//
int pci_save_state(struct pci_dev *dev);
struct pci_dev *pci_dev_get(struct pci_dev *dev);
void pci_dev_put(struct pci_dev *dev);
struct pci_dev *pci_get_device(unsigned int vendor, unsigned int device, struct pci_dev *from);
pci_power_t pci_target_state(struct pci_dev *dev);
int pci_restore_state(struct pci_dev *dev);
bool pci_pme_capable(struct pci_dev *dev, pci_power_t state);
struct pci_dev * pci_get_slot(struct pci_bus *bus, unsigned int devfn);
int pcie_set_readrq(struct pci_dev *dev, int rq);
void *pci_alloc_consistent(struct pci_dev *hwdev, size_t size, dma_addr_t *dma_handle);

//
// Do we need these
//
void iounmap(volatile void __iomem *addr);
void mmiowb(void);

typedef unsigned int __bitwise pci_ers_result_t;

enum pci_ers_result {
    /* no result/none/not supported in device driver */
    PCI_ERS_RESULT_NONE = (__force pci_ers_result_t) 1,

    /* Device driver can recover without slot reset */
    PCI_ERS_RESULT_CAN_RECOVER = (__force pci_ers_result_t) 2,

    /* Device driver wants slot to be reset. */
    PCI_ERS_RESULT_NEED_RESET = (__force pci_ers_result_t) 3,

    /* Device has completely failed, is unrecoverable */
    PCI_ERS_RESULT_DISCONNECT = (__force pci_ers_result_t) 4,

    /* Device driver is fully recovered and
     * operational */
    PCI_ERS_RESULT_RECOVERED = (__force pci_ers_result_t) 5,
};

typedef unsigned int __bitwise pci_channel_state_t;

enum pci_channel_state {
    /* I/O channel is in normal state */
    pci_channel_io_normal = (__force pci_channel_state_t) 1,

    /* I/O to channel is blocked */
    pci_channel_io_frozen = (__force pci_channel_state_t) 2,

    /* PCI card is dead */
    pci_channel_io_perm_failure = (__force pci_channel_state_t) 3,
};

/* PCI bus error event callbacks */
struct pci_error_handlers {
    /* PCI bus error detected on this device */
    pci_ers_result_t (*error_detected)(struct pci_dev *dev,
            enum pci_channel_state error);

    /* MMIO has been re-enabled, but not DMA */
    pci_ers_result_t (*mmio_enabled)(struct pci_dev *dev);

    /* PCI Express link has been reset */
    pci_ers_result_t (*link_reset)(struct pci_dev *dev);

    /* PCI slot has been reset */
    pci_ers_result_t (*slot_reset)(struct pci_dev *dev);

    /* Device driver may resume normal operations */
    void (*resume)(struct pci_dev *dev);
};


typedef unsigned int __bitwise pcie_reset_state_t;

enum pcie_reset_state {
    /* Reset is NOT asserted (Use to deassert reset) */
    pcie_deassert_reset = (__force pcie_reset_state_t) 1,

    /* Use #PERST to reset PCI-E device */
    pcie_warm_reset = (__force pcie_reset_state_t) 2,

    /* Use PCI-E Hot Reset to reset device */
    pcie_hot_reset = (__force pcie_reset_state_t) 3
};

  
#define PCI_D0      ((pci_power_t __force) 0)
#define PCI_D1      ((pci_power_t __force) 1)
#define PCI_D2      ((pci_power_t __force) 2)
#define PCI_D3hot   ((pci_power_t __force) 3)
#define PCI_D3cold  ((pci_power_t __force) 4)
#define PCI_UNKNOWN ((pci_power_t __force) 5)
#define PCI_POWER_ERROR ((pci_power_t __force) -1)

#define PCI_PM_D2_DELAY 200
#define PCI_PM_D3_WAIT  10
#define PCI_PM_BUS_WAIT 50
  
#define PCI_DEVICE(vend,dev) \
    .vendor = (vend), .device = (dev), \
    .subvendor = PCI_ANY_ID, .subdevice = PCI_ANY_ID

// From pci_64.h in arch directory
#define DECLARE_PCI_UNMAP_ADDR(ADDR_NAME)       \
    dma_addr_t ADDR_NAME;
#define DECLARE_PCI_UNMAP_LEN(LEN_NAME)         \
    __u32 LEN_NAME;
#define pci_unmap_addr(PTR, ADDR_NAME)          \
    ((PTR)->ADDR_NAME)
#define pci_unmap_addr_set(PTR, ADDR_NAME, VAL) \
    (((PTR)->ADDR_NAME) = (VAL))
#define pci_unmap_len(PTR, LEN_NAME)            \
    ((PTR)->LEN_NAME)
#define pci_unmap_len_set(PTR, LEN_NAME, VAL)   \
    (((PTR)->LEN_NAME) = (VAL))

/* This defines the direction arg to the DMA mapping routines. */
#define PCI_DMA_BIDIRECTIONAL   0
#define PCI_DMA_TODEVICE        1
#define PCI_DMA_FROMDEVICE      2
#define PCI_DMA_NONE            3

#endif
