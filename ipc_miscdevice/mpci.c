/*
 * A virtual bus for LDD sample code devices to plug into.  This
 * code is heavily borrowed from drivers/base/sys.c
 *
 * Copyright (C) 2001 Alessandro Rubini and Jonathan Corbet
 * Copyright (C) 2001 O'Reilly & Associates
 *
 * The source code in this file can be freely used, adapted,
 * and redistributed in source or binary form, so long as an
 * acknowledgment appears in derived source files.  The citation
 * should list that the code comes from the book "Linux Device
 * Drivers" by Alessandro Rubini and Jonathan Corbet, published
 * by O'Reilly & Associates.   No warranty is attached;
 * we cannot take responsibility for errors or fitness for use.
 *
 */
/* $Id: lddbus.c,v 1.9 2004/09/26 08:12:27 gregkh Exp $ */

#include <linux/device.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/string.h>
#include <linux/moduleparam.h>
#include "../common/mpci-bus.h"

MODULE_AUTHOR("Matthew Renzelmann");
MODULE_LICENSE("Dual BSD/GPL");
static char *Version = "$Revision: 10^(-100) $";

static int pci_hotplug(struct device *dev, struct kobj_uevent_env *env);
static int pci_match(struct device *dev, struct device_driver *driver);
static const struct pci_device_id *pci_match_one_device(const struct pci_device_id *, const struct pci_dev *);
static const struct pci_device_id *pci_match_id(const struct pci_device_id *ids, struct pci_dev *dev);
static const struct pci_device_id *pci_match_device(struct pci_driver *drv, struct pci_dev *dev);
static int __pci_device_probe(struct pci_driver *drv, struct pci_dev *pci_dev);
static int pci_device_probe(struct device * dev);
static int pci_device_remove(struct device * dev);
static void pci_bus_release(struct device *dev);
static ssize_t show_bus_version(struct bus_type *bus, char *buf);
static void pci_bus_release(struct device *dev);
static void pci_dev_release(struct device *dev);
static ssize_t store_new_id(struct device_driver *driver, const char *buf, size_t count);
static void pci_free_dynids(struct pci_driver *drv);
static int __init pci_bus_init(void);
static void pci_bus_exit(void);

//
// Command line parameters that govern creation of the bogus
// PCI device.  Note that the defaults specified here will
// create a bogus 8139too that's good enough to initialize
// the driver.
//
// I have almost no clue what any of this should be.
// These values seem reasonable for the 8139too driver :)
// Feel free to override via command line.
//
static char *p_device_name = "8139too";
module_param(p_device_name, charp, S_IRUGO);
MODULE_PARM_DESC(p_device_name, "Name of fake PCI device.  Default \"8139too\"");

static int p_devfn = 0;
module_param(p_devfn, int, S_IRUGO);
MODULE_PARM_DESC(p_devfn, "Device devfn - The encoded device and function index.  Default 0.");

static int p_vendor = PCI_VENDOR_ID_REALTEK;
module_param(p_vendor, int, S_IRUGO);
MODULE_PARM_DESC(p_vendor, "Default PCI_VENDOR_ID_REALTEK.  See include/linux/pci_ids.h");

static int p_device = PCI_DEVICE_ID_REALTEK_8139;
module_param(p_device, int, S_IRUGO);
MODULE_PARM_DESC(p_device, "Default PCI_DEVICE_ID_REALTEK_8139.  See include/linux/pci_ids.h");

static int p_subsystem_vendor = PCI_VENDOR_ID_REALTEK;
module_param(p_subsystem_vendor, int, S_IRUGO);
MODULE_PARM_DESC(p_subsystem_vendor, "Default PCI_VENDOR_ID_REALTEK.  See include/linux/pci_ids.h");

static int p_subsystem_device = PCI_DEVICE_ID_REALTEK_8139;
module_param(p_subsystem_device, int, S_IRUGO);
MODULE_PARM_DESC(p_subsystem_device, "Default PCI_DEVICE_ID_REALTEK_8139.  See include/linux/pci_ids.h");

static int p_class = PCI_CLASS_NETWORK_ETHERNET;
module_param(p_class, int, S_IRUGO);
MODULE_PARM_DESC(p_class, "Default PCI_CLASS_NETWORK_ETHERNET.  See include/linux/pci_ids.h");

// For 8139too: Keep less than 0x20
static int p_revision = 0x2;
module_param(p_revision, int, S_IRUGO);
MODULE_PARM_DESC(p_revision, "The revision ID.  Default = 0x1.");

static int p_irq = 10;
module_param(p_irq, int, S_IRUGO);
MODULE_PARM_DESC(p_irq, "The device IRQ.  Default = 10.");

static int p_resource_start_total = 0;
static unsigned long p_resource_start[5] = { -1, -1, -1, -1, -1 };
module_param_array(p_resource_start, ulong, &p_resource_start_total, S_IRUGO);
MODULE_PARM_DESC(p_resource_start, "Array of resource start addresses.");

static int p_resource_end_total = 0;
static unsigned long p_resource_end[5] = { -1, -1, -1, -1, -1 };
module_param_array(p_resource_end, ulong, &p_resource_end_total, S_IRUGO);
MODULE_PARM_DESC(p_resource_end, "Array of resource end addresses.");

static int p_resource_flags_total = 0;
static unsigned long p_resource_flags[5] = { -1, -1, -1, -1, -1 };
module_param_array(p_resource_flags, ulong, &p_resource_flags_total, S_IRUGO);
MODULE_PARM_DESC(p_resource_flags, "Array of resource flags.  See include/linux/ioport.h");

static int p_capability_total = 0;
static int p_capability[5] = { -1, -1, -1, -1, -1 };
module_param_array(p_capability, int, &p_capability_total, S_IRUGO);
MODULE_PARM_DESC(p_capability, "Array of capability identifiers, see include/linux/pci_regs.h, PCI_CAP_XXX.");

static int p_capability_value_total = 0;
static int p_capability_value[5] = { -1, -1, -1, -1, -1 };
module_param_array(p_capability_value, int, &p_capability_value_total, S_IRUGO);
MODULE_PARM_DESC(p_capability_value, "Array of capability start indexes");

// For store_new_id
struct pci_dynid {
    struct list_head node;
    struct pci_device_id id;
};

struct device pci_bus = {
    .bus_id   = "pci0",
    .release  = pci_bus_release
};

/*
 * And the bus type.
 */
struct bus_type pci_bus_type = {
    .name = "pci",
    .match = pci_match,
    .uevent = pci_hotplug,
    .probe = pci_device_probe,
    .remove = pci_device_remove
};

// Our phony PCI device
static struct pci_dev permanent_pci_device;

/*
  MJR TODO
static struct class pcibus_class = {
    .name           = "pci_bus",
    .dev_release    = &pci_bus_release,
};
*/

static BUS_ATTR(version, S_IRUGO, show_bus_version, NULL);
static DRIVER_ATTR(new_id, S_IWUSR, NULL, store_new_id);

///////////////////////////////////////////////////////////////////////////////

/*
 * Respond to hotplug events.
 */
static int pci_hotplug(struct device *dev, struct kobj_uevent_env *env) {
    printk ("%s\r\n", __FUNCTION__);
    return 0;
}

/*
 * Match PCI devices to drivers.  Just do a simple name test.
 */
static int pci_match(struct device *dev, struct device_driver *driver) {
    printk ("pci_match test: %s %s %s!!!\r\n", __FUNCTION__, dev->bus_id, driver->name);
    return !strncmp(dev->bus_id, driver->name, strlen(driver->name));
}

/**
 * pci_match_one_device - Tell if a PCI device structure has a matching
 *                        PCI device id structure
 * @id: single PCI device id structure to match
 * @dev: the PCI device structure to match against
 *
 * Returns the matching pci_device_id structure or %NULL if there is no match.
 */
static const struct pci_device_id *
pci_match_one_device(const struct pci_device_id *id, const struct pci_dev *dev)
{
    if ((id->vendor == PCI_ANY_ID || id->vendor == dev->vendor) &&
        (id->device == PCI_ANY_ID || id->device == dev->device) &&
        (id->subvendor == PCI_ANY_ID || id->subvendor == dev->subsystem_vendor) &&
        (id->subdevice == PCI_ANY_ID || id->subdevice == dev->subsystem_device) &&
        !((id->class ^ dev->class) & id->class_mask)) {
        printk ("%s matched!  Vendor: %d Subvendor: %d\r\n", __FUNCTION__, id->vendor, id->subvendor);
        return id;
    }
    printk ("No match: \n"
            "id->vendor %d ?= dev->vendor %d\n"
            "id->device %d ?= dev->device %d\n"
            "id->subvendor %d ?= dev->subsystem_vendor %d\n"
            "id->subdevice %d ?= dev->subsystem_device %d\n",
            id->vendor, dev->vendor,
            id->device, dev->device,
            id->subvendor, dev->subsystem_vendor,
            id->subdevice, dev->subsystem_device);
    return NULL;
}

/**
 * pci_match_id - See if a pci device matches a given pci_id table
 * @ids: array of PCI device id structures to search in
 * @dev: the PCI device structure to match against.
 *
 * Used by a driver to check whether a PCI device present in the
 * system is in its list of supported devices.  Returns the matching
 * pci_device_id structure or %NULL if there is no match.
 *
 * Deprecated, don't use this as it will not catch any dynamic ids
 * that a driver might want to check for.
 */
static const struct pci_device_id *pci_match_id(const struct pci_device_id *ids,
                                                struct pci_dev *dev)
{
    if (ids) {
        while (ids->vendor || ids->subvendor || ids->class_mask) {
            if (pci_match_one_device(ids, dev)) {
                printk ("%s matched! Vendor %d Subvendor %d\r\n", __FUNCTION__,
                        ids->vendor, ids->subvendor);
                return ids;
            }
            ids++;
        }
    }
    return NULL;
}

/**
 * pci_match_device - Tell if a PCI device structure has a matching PCI device id structure
 * @drv: the PCI driver to match against
 * @dev: the PCI device structure to match against
 *
 * Used by a driver to check whether a PCI device present in the
 * system is in its list of supported devices.  Returns the matching
 * pci_device_id structure or %NULL if there is no match.
 */
static const struct pci_device_id *pci_match_device(struct pci_driver *drv,
                                                    struct pci_dev *dev)
{
    struct pci_dynid *dynid;

    /* Look at the dynamic ids first, before the static ones */
    spin_lock(&drv->dynids.lock);
    list_for_each_entry(dynid, &drv->dynids.list, node) {
        if (pci_match_one_device(&dynid->id, dev)) {
            spin_unlock(&drv->dynids.lock);
            return &dynid->id;
        }
    }
    spin_unlock(&drv->dynids.lock);

    return pci_match_id(drv->id_table, dev);
}

/**
 * __pci_device_probe()
 * @drv: driver to call to check if it wants the PCI device
 * @pci_dev: PCI device being probed
 *
 * returns 0 on success, else error.
 * side-effect: pci_dev->driver is set to drv when drv claims pci_dev.
 */
static int
__pci_device_probe(struct pci_driver *drv, struct pci_dev *pci_dev)
{
    const struct pci_device_id *id;
    int error = 0;

    if (!pci_dev->driver && drv->probe) {
        error = -ENODEV;

        id = pci_match_device(drv, pci_dev);
        if (id) {
            printk ("PCI subsystem: Calling driver probe function %p %d\r\n", drv->probe, pci_dev->vendor);
            error = drv->probe (pci_dev, id);
        }
        printk ("PCI subsystem:  probe error %d\n", error);
        if (error >= 0) {
            pci_dev->driver = drv;
            error = 0;
        }
    } else {
        printk ("PCI subsystem:  probe no match %d\n", error);
    }
    return error;
}

static int pci_device_probe(struct device * dev)
{
    int error = 0;
    struct pci_driver *drv;
    struct pci_dev *pci_dev;

    drv = to_pci_driver(dev->driver);
    pci_dev = to_pci_dev(dev);
    pci_dev_get(pci_dev);
    error = __pci_device_probe(drv, pci_dev);
    if (error)
        pci_dev_put(pci_dev);

    return error;
}

static int pci_device_remove(struct device * dev)
{
    struct pci_dev *pci_dev;
    struct pci_driver * drv;

    pci_dev = to_pci_dev(dev);
    drv = pci_dev->driver;

    if (drv) {
        if (drv->remove)
            drv->remove(pci_dev);
        pci_dev->driver = NULL;
    }

    pci_dev_put(pci_dev);
    return 0;
}

/*
 * The PCI bus device.
 */
static void pci_bus_release(struct device *dev) {
    printk(KERN_DEBUG "pcibus release\n");
}

/*
 * Export a simple attribute.
 */
static ssize_t show_bus_version(struct bus_type *bus, char *buf) {
    return snprintf(buf, PAGE_SIZE, "%s\n", Version);
}

/*
 * PCI devices.
 */

/*
 * For now, no references to PCIbus devices go out which are not
 * tracked via the module reference count, so we use a no-op
 * release function.
 */
static void pci_dev_release(struct device *dev) {
}

/**
 * store_new_id - add a new PCI device ID to this driver and re-probe devices
 * @driver: target device driver
 * @buf: buffer for scanning device ID data
 * @count: input size
 *
 * Adds a new dynamic pci device ID to this driver,
 * and causes the driver to probe for all devices again.
 */
static ssize_t
store_new_id(struct device_driver *driver, const char *buf, size_t count)
{
    struct pci_dynid *dynid;
    struct pci_driver *pdrv;
    const struct pci_device_id *ids;
    __u32 vendor, device, subvendor=PCI_ANY_ID,
        subdevice=PCI_ANY_ID, class=0, class_mask=0;
    unsigned long driver_data=0;
    int fields=0;
    int retval=0;

    pdrv = to_pci_driver(driver);
    ids = pdrv->id_table;

    printk ("Fields: %s\r\n", buf);
    fields = sscanf(buf, "%x %x %x %x %x %x %lx",
                    &vendor, &device, &subvendor, &subdevice,
                    &class, &class_mask, &driver_data);
    if (fields < 2)
        return -EINVAL;

    /* Only accept driver_data values that match an existing id_table
       entry */
    if (ids) {
        retval = -EINVAL;
        while (ids->vendor || ids->subvendor || ids->class_mask) {
            if (driver_data == ids->driver_data) {
                retval = 0;
                break;
            }
            ids++;
        }
        if (retval)     /* No match */
            return retval;
    }

    dynid = kzalloc(sizeof(*dynid), GFP_KERNEL);
    if (!dynid)
        return -ENOMEM;

    dynid->id.vendor = vendor;
    dynid->id.device = device;
    dynid->id.subvendor = subvendor;
    dynid->id.subdevice = subdevice;
    dynid->id.class = class;
    dynid->id.class_mask = class_mask;
    dynid->id.driver_data = driver_data;

    spin_lock(&pdrv->dynids.lock);
    list_add_tail(&dynid->node, &pdrv->dynids.list);
    spin_unlock(&pdrv->dynids.lock);

    if (get_driver(&pdrv->driver)) {
        retval = driver_attach(&pdrv->driver);
        put_driver(&pdrv->driver);
    }

    if (retval)
        return retval;
    return count;
}

static void pci_free_dynids(struct pci_driver *drv) {
    struct pci_dynid *dynid, *n;
    
    spin_lock(&drv->dynids.lock);
    list_for_each_entry_safe(dynid, n, &drv->dynids.list, node) {
        list_del(&dynid->node);
        kfree(dynid);
    }
    spin_unlock(&drv->dynids.lock);
}

static int __init pci_bus_init(void)
{
    int ret;

    // For setting up the bogus PCI resources:
    int i;
    int total_resources = -1;
    int total_capabilities = -1;
    static char *resource_phony = "PhonyPIO";
    
    if (p_resource_start_total != p_resource_end_total ||
        p_resource_start_total != p_resource_flags_total ||
        p_capability_total != p_capability_value_total) {
        printk ("Incorrect command line usage.  See code :)\n");
        printk ("\n");
        printk ("Summary of potential issue #1:\n");
        printk ("The idea is that p_resource_start, p_resource_end, and p_resource_flags\n");
        printk ("are arrays of numbers, and the number of elements in each array must be\n");
        printk ("the same.\n");
        printk ("\n");
        printk ("Summary of potential issue #2:\n");
        printk ("The capability / capability value arrays must also match in length\n");
        return -EINVAL;
    }

    printk ("p_device_name: %s\n", p_device_name);
    printk ("p_devfn: %d\n", p_devfn);
    printk ("p_vendor: %x\n", p_vendor);
    printk ("p_device: %x\n", p_device);
    printk ("p_subsystem_vendor: %x\n", p_subsystem_vendor);
    printk ("p_subsystem_device: %x\n", p_subsystem_device);
    printk ("p_class: %x\n", p_class);
    printk ("p_revision: %x\n", p_revision);
    printk ("p_irq: %d\n", p_irq);
    printk ("p_resource_start_total: %d\n", p_resource_start_total);
    printk ("p_resource_end_total: %d\n", p_resource_end_total);
    printk ("p_resource_flags_total: %d\n", p_resource_flags_total);
    printk ("p_resource_start: %lx %lx %lx %lx %lx\n",
            p_resource_start[0],
            p_resource_start[1],
            p_resource_start[2],
            p_resource_start[3],
            p_resource_start[4]);
    printk ("p_resource_end: %lx %lx %lx %lx %lx\n",
            p_resource_end[0],
            p_resource_end[1],
            p_resource_end[2],
            p_resource_end[3],
            p_resource_end[4]);
    printk ("p_resource_flags: %lx %lx %lx %lx %lx\n",
            p_resource_flags[0],
            p_resource_flags[1],
            p_resource_flags[2],
            p_resource_flags[3],
            p_resource_flags[4]);
    printk ("p_capability: %x %x %x %x %x\n",
            p_capability[0],
            p_capability[1],
            p_capability[2],
            p_capability[3],
            p_capability[4]);
    printk ("p_capability_value: %x %x %x %x %x\n",
            p_capability_value[0],
            p_capability_value[1],
            p_capability_value[2],
            p_capability_value[3],
            p_capability_value[4]);

    if (p_resource_start[0] == -1 ||
        p_resource_flags[0] == -1) {
        printk ("Specify p_resource_start and so forth.\n");
        return -EINVAL;
    }

    // The total number of resources in our bogus PCI device.
    total_resources = p_resource_start_total;
    total_capabilities = p_capability_total;

    ret = bus_register(&pci_bus_type);
    if (ret)
        return ret;
    if (bus_create_file(&pci_bus_type, &bus_attr_version))
        printk(KERN_NOTICE "Unable to create version attribute\n");
    ret = device_register(&pci_bus);
    if (ret)
        printk(KERN_NOTICE "Unable to register pci0\n");

    memset(&permanent_pci_device, 0, sizeof(struct pci_dev));
    permanent_pci_device.name = p_device_name;
    permanent_pci_device.devfn = p_devfn;
    permanent_pci_device.vendor = p_vendor;
    permanent_pci_device.device = p_device;
    permanent_pci_device.subsystem_vendor = p_subsystem_vendor;
    permanent_pci_device.subsystem_device = p_subsystem_device;
    permanent_pci_device.class = p_class; // Or is it &pcibus_class ??? MJR TODO
    permanent_pci_device.revision = p_revision;
    permanent_pci_device.irq = p_irq; // MJR TODO This will become an issue.

    for (i = 0; i < total_resources; i++) {
        permanent_pci_device.resource[i].start = p_resource_start[i];
        permanent_pci_device.resource[i].end = p_resource_end[i];
        permanent_pci_device.resource[i].flags = p_resource_flags[i];
        permanent_pci_device.resource[i].name = resource_phony;
        permanent_pci_device.resource[i].parent = NULL;
        permanent_pci_device.resource[i].sibling = NULL;
        permanent_pci_device.resource[i].child = NULL;
    }

    for (i = 0; i < total_capabilities; i++) {
        permanent_pci_device.capabilities[p_capability[i]] = p_capability_value[i];
    }

    // Create our very own phony device
    pci_register_device (&permanent_pci_device);
    return ret;
}

static void pci_bus_exit(void)
{
    pci_unregister_device (&permanent_pci_device);
    device_unregister(&pci_bus);
    bus_unregister(&pci_bus_type);
}

//
// Exported Functions Section 1
//
// These functions are all implemented in user mode
//

//
// Exported Functions Section 2
//
void pci_set_master(struct pci_dev *dev) {
    return;
}

void pci_clear_master(struct pci_dev *dev) {
    return;
}

int pci_enable_device(struct pci_dev *dev) {
    return 0;
}

int pci_disable_device(struct pci_dev *dev) {
    return 0;
}

int pci_set_dma_mask(struct pci_dev *dev, u64 mask) {
    return 0;
}

int pci_set_consistent_dma_mask(struct pci_dev *dev, u64 mask) {
    return 0;
}

int pci_request_regions(struct pci_dev *pdev, const char *res_name) {
    return 0;
}

void pci_release_regions(struct pci_dev *pdev) {
    return;
}

//
// Exported Functions Section 3
//
void *pci_get_drvdata(struct pci_dev *pdev) {
    return dev_get_drvdata(&pdev->dev);
}

void pci_set_drvdata(struct pci_dev *pdev, void *data) {
    dev_set_drvdata(&pdev->dev, data);
}

int pci_set_power_state(struct pci_dev *dev, pci_power_t state) {
    return 0;
}

pci_power_t pci_choose_state(struct pci_dev *dev, pm_message_t state) {
    return 0;
}

int pci_enable_wake(struct pci_dev *dev, pci_power_t state, int enable) {
    return 0;
}

//
// Exported Functions Section 4
//
int pci_register_device(struct pci_dev *pcidev) {
    printk ("Registering device!!!\r\n");
    pcidev->dev.bus = &pci_bus_type;
    pcidev->dev.parent = &pci_bus;
    pcidev->dev.release = pci_dev_release;
    strncpy(pcidev->dev.bus_id, pcidev->name, BUS_ID_SIZE);
    return device_register(&pcidev->dev);
}

void pci_unregister_device(struct pci_dev *pcidev) {
    device_unregister(&pcidev->dev);
}

/*
 * Crude driver interface.
 */
int __pci_register_driver(struct pci_driver *drv, struct module *owner, const char *mod_name)
{
    int ret;

    drv->driver.name = drv->name;
    drv->driver.bus = &pci_bus_type;
    drv->driver.mod_name = mod_name;

    spin_lock_init(&drv->dynids.lock);
    INIT_LIST_HEAD(&drv->dynids.list);

    printk ("%s device: %d\r\n", __FUNCTION__, permanent_pci_device.vendor);

    ret = driver_register(&drv->driver);
    if (ret)
        return ret;

    if (drv->probe != NULL)
        ret = driver_create_file(&drv->driver, &driver_attr_new_id);
    else
        driver_unregister (&drv->driver);

    printk ("%s device done: %d\r\n", __FUNCTION__, permanent_pci_device.vendor);
    
    return ret;
}

void pci_unregister_driver(struct pci_driver *driver)
{
    driver_remove_file(&driver->driver, &driver_attr_new_id);
    driver_unregister(&driver->driver);
    pci_free_dynids(driver);
}

//More PCI functions for e1000

int pci_select_bars(struct pci_dev *dev, unsigned long flags) {
    return 0;
}

// TODO Major hack
// We want our fake device to support some capabilities but not others.
// For our drivers, returning 0 works in all cases except tg3, which requires
// the PCI_CAP_ID_PM capability.
int pci_find_capability(struct pci_dev *dev, int cap) {
    int retval;
    if (cap >= 32 || cap < 0) {
        panic ("Failed to call pci_find_capability -- note that our implementation sucks and needs to be improved anyway.  Why not spend a few minutes doing that now :)");
    }

    retval = permanent_pci_device.capabilities[cap];
    printk ("%s called with cap %d, returning %d\n", __func__, cap, retval);
    return retval;
}

void pci_dma_sync_single_for_device(struct pci_dev *dev, dma_addr_t dma_addr,
                                    size_t size, int direction) {
    return;
}

int pci_request_selected_regions(struct pci_dev *pdev, int bars,
        const char *res_name) {
    return 0;
}

void pci_release_selected_regions(struct pci_dev *pdev, int bars) {
    return;
}

int pci_enable_device_mem(struct pci_dev *dev) {
    return 0;
}

void pci_disable_msi(struct pci_dev *dev) {
    return;
}

unsigned long pci_ioremap_bar(struct pci_dev *pdev, int bar) {
    //return 0xffffc20014940000;
    unsigned long retval = permanent_pci_device.resource[bar].start;
    if (retval == -1) {
        panic ("Need to initialize the p_resource_start stuff");
    }
    printk ("pci_ioremap_bar: returning %lx\n", retval);
    return retval;
}

int pci_set_mwi(struct pci_dev *dev) {
    return 0;
}

void pci_clear_mwi(struct pci_dev *dev) {
    return;
}

int pci_enable_msi(struct pci_dev* dev) {
    return 0;
}

int pci_channel_offline(struct pci_dev *pdev) {
    return 0;
}

int pcix_get_mmrbc(struct pci_dev *dev) {
    return 0;
}

void pci_dma_sync_single_for_cpu(struct pci_dev *pdev, dma_addr_t dma_handle,
                                 size_t size, int direction) {
    return;
}

int pcix_set_mmrbc(struct pci_dev *dev, int mmrbc) {
    return 0;
}

int pci_dev_present (const struct pci_device_id *ids) {
    return 0;
}

//
// More functions for tg3
//

int pci_save_state(struct pci_dev *dev) {
    return 0;
}

/**
 * pci_dev_get - increments the reference count of the pci device structure
 * @dev: the device being referenced
 *
 * Each live reference to a device should be refcounted.
 *
 * Drivers for PCI devices should normally record such references in
 * their probe() methods, when they bind to a device, and release
 * them by calling pci_dev_put(), in their disconnect() methods.
 *
 * A pointer to the device with the incremented reference counter is returned.
 */
struct pci_dev *pci_dev_get(struct pci_dev *dev)
{
    if (dev)
        get_device(&dev->dev);
    return dev;
}

/**
 * pci_dev_put - release a use of the pci device structure
 * @dev: device that's been disconnected
 *
 * Must be called when a user of a device is finished with it.  When the last
 * user of the device calls this function, the memory of the device is freed.
 */
void pci_dev_put(struct pci_dev *dev)
{
    if (dev)
        put_device(&dev->dev);
}

/**
 * pci_get_device - begin or continue searching for a PCI device by vendor/device id
 * @vendor: PCI vendor id to match, or %PCI_ANY_ID to match all vendor ids
 * @device: PCI device id to match, or %PCI_ANY_ID to match all device ids
 * @from: Previous PCI device found in search, or %NULL for new search.
 *
 * Iterates through the list of known PCI devices.  If a PCI device is
 * found with a matching @vendor and @device, the reference count to the
 * device is incremented and a pointer to its device structure is returned.
 * Otherwise, %NULL is returned.  A new search is initiated by passing %NULL
 * as the @from argument.  Otherwise if @from is not %NULL, searches continue
 * from next device on the global list.  The reference count for @from is
 * always decremented if it is not %NULL.
 */
struct pci_dev *pci_get_device(unsigned int vendor, unsigned int device, struct pci_dev *from)
{
    return NULL;
}

pci_power_t pci_target_state(struct pci_dev *dev)
{
    return PCI_D3hot;
}

/** 
 * pci_restore_state - Restore the saved state of a PCI device
 * @dev: - PCI device that we're dealing with
 */
int pci_restore_state(struct pci_dev *dev)
{
    return 0;
}

/**
 * pci_pme_capable - check the capability of PCI device to generate PME#
 * @dev: PCI device to handle.
 * @state: PCI state from which device will issue PME#.
 */
bool pci_pme_capable(struct pci_dev *dev, pci_power_t state)
{
    return 0;
}

struct pci_dev * pci_get_slot(struct pci_bus *bus, unsigned int devfn)
{
    return NULL;
}

int pcie_set_readrq(struct pci_dev *dev, int rq)
{
    return 0;
}


//
// Do we need these
//
void iounmap(volatile void __iomem *addr) {
    return;
}

void mmiowb(void) {
    ;
}

// Exported Functions Section 1
//EXPORT_SYMBOL(pci_bus_read_config_byte);
//EXPORT_SYMBOL(pci_bus_read_config_word);
//EXPORT_SYMBOL(pci_bus_read_config_dword);
//EXPORT_SYMBOL(pci_bus_write_config_byte);
//EXPORT_SYMBOL(pci_bus_write_config_word);
//EXPORT_SYMBOL(pci_bus_write_config_dword);
//EXPORT_SYMBOL(pci_read_config_byte);
//EXPORT_SYMBOL(pci_read_config_word);
//EXPORT_SYMBOL(pci_read_config_dword);
//EXPORT_SYMBOL(pci_write_config_byte);
//EXPORT_SYMBOL(pci_write_config_word);
//EXPORT_SYMBOL(pci_write_config_dword);

// Exported Functions Section 2
EXPORT_SYMBOL(pci_set_master);
EXPORT_SYMBOL(pci_clear_master);
EXPORT_SYMBOL(pci_enable_device);
EXPORT_SYMBOL(pci_disable_device);
EXPORT_SYMBOL(pci_set_dma_mask);
EXPORT_SYMBOL(pci_set_consistent_dma_mask);
EXPORT_SYMBOL(pci_request_regions);
EXPORT_SYMBOL(pci_release_regions);

// Exported Functions Section 3
EXPORT_SYMBOL(pci_get_drvdata);
EXPORT_SYMBOL(pci_set_drvdata);
EXPORT_SYMBOL(pci_set_power_state);
EXPORT_SYMBOL(pci_choose_state);
EXPORT_SYMBOL(pci_enable_wake);

// Exported Functions Section 4
EXPORT_SYMBOL(pci_register_device);
EXPORT_SYMBOL(pci_unregister_device);
EXPORT_SYMBOL(__pci_register_driver);
EXPORT_SYMBOL(pci_unregister_driver);

// Exported Functions Section 5
EXPORT_SYMBOL(pci_select_bars);
EXPORT_SYMBOL(pci_find_capability);   
EXPORT_SYMBOL(pci_dma_sync_single_for_device); 
EXPORT_SYMBOL(pci_request_selected_regions);
EXPORT_SYMBOL(pci_release_selected_regions);
EXPORT_SYMBOL(pci_enable_device_mem);
EXPORT_SYMBOL(pci_disable_msi); 
EXPORT_SYMBOL(pci_ioremap_bar);
EXPORT_SYMBOL(pci_set_mwi); 
EXPORT_SYMBOL(pci_clear_mwi); 
EXPORT_SYMBOL(pci_enable_msi); 
EXPORT_SYMBOL(pci_channel_offline);
EXPORT_SYMBOL(pcix_get_mmrbc);
EXPORT_SYMBOL(pci_dma_sync_single_for_cpu);
EXPORT_SYMBOL(pcix_set_mmrbc);

// Added for cmipci
EXPORT_SYMBOL(pci_dev_present);

// Added for tg3
EXPORT_SYMBOL(pci_save_state);
EXPORT_SYMBOL(pci_dev_get);
EXPORT_SYMBOL(pci_dev_put);
EXPORT_SYMBOL(pci_get_device);
EXPORT_SYMBOL(pci_target_state);
EXPORT_SYMBOL(pci_restore_state);
EXPORT_SYMBOL(pci_pme_capable);
EXPORT_SYMBOL(pci_get_slot);
EXPORT_SYMBOL(pcie_set_readrq);

// Do we need these:
EXPORT_SYMBOL(iounmap);
EXPORT_SYMBOL(mmiowb);

module_init(pci_bus_init);
module_exit(pci_bus_exit);
