// See Makefile in the ipc_drivers/usb_test/kernel subdirectory.
// #define REAL_KERNEL_MODULE

#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/delay.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/ethtool.h>
#include <linux/mii.h>

#include <linux/usb.h>

#ifndef REAL_KERNEL_MODULE
    #define WRAPPERS_SYM_DRIVER_STEP1
    #include "../../common/wrappers_usb.h" // Wrappers--need them preprocessed
#endif

#include <linux/module.h>
#include <asm/byteorder.h>
#include <asm/uaccess.h>
#include <net/sch_generic.h>
#define REAL_KERNEL_MODULE

#ifdef REAL_KERNEL_MODULE
    #define uprintk printk
    #define mprintk printk
#else REAL_KERNEL_MODULE
    #include "modif_annots.h"
    int MJR_middle;
#endif

#define DRIVER_VERSION "v0.6.14 (2006/09/27)"
#define DRIVER_AUTHOR "Bucky"
#define DRIVER_DESC "USB TEST driver"

static const char driver_name[] = "usb_test";

#define	VENDOR_ADMTEK		0x07a6
static struct usb_device_id usb_test_ids[] = {
    {
        .match_flags = USB_DEVICE_ID_MATCH_DEVICE,
        .idVendor = VENDOR_ADMTEK,
        .idProduct = 0x8511
    },
    /* Terminating entry */
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
};

MODULE_AUTHOR(DRIVER_AUTHOR);
MODULE_DESCRIPTION(DRIVER_DESC);
MODULE_LICENSE("GPL");

MODULE_DEVICE_TABLE(usb, usb_test_ids);

#define COMPLEX_MODE

#ifdef COMPLEX_MODE
typedef struct pegasus {
    struct usb_device       *usb;
    struct urb              *ctrl_urb;
    struct usb_ctrlrequest  dr;
    wait_queue_head_t       ctrl_wait;
} pegasus_t;

static void ctrl_callback(struct urb *urb)
{
    pegasus_t *pegasus = urb->context;
    int status = urb->status;

    if (!pegasus)
        return;

    switch (status) {
        case 0:
            mprintk ("Callback completed fine.\n");
            break;
        case -EINPROGRESS:
            return;
        case -ENOENT:
            break;
        default:
            break;
    }
    //wake_up(&pegasus->ctrl_wait);
}

static int alloc_urbs(pegasus_t * pegasus)
{
    pegasus->ctrl_urb = usb_alloc_urb(0, GFP_KERNEL);
    if (!pegasus->ctrl_urb) {
        return 0;
    }
    return 1;
}

static int set_register(pegasus_t * pegasus, __u16 indx, __u8 data)
{
    int ret;
    //char *tmp;
    static char tmp;
    //DECLARE_WAITQUEUE(wait, current);

    //tmp = kmalloc(1, GFP_KERNEL);
    //if (!tmp) {
    //    return -ENOMEM;
    //}

    memcpy(&tmp, &data, 1);
    //add_wait_queue(&pegasus->ctrl_wait, &wait);
    //set_current_state(TASK_UNINTERRUPTIBLE);
    //schedule();
    //remove_wait_queue(&pegasus->ctrl_wait, &wait);
    //set_current_state(TASK_RUNNING);

    pegasus->dr.bRequestType = 1;
    pegasus->dr.bRequest = 1;
    pegasus->dr.wValue = 3;
    pegasus->dr.wIndex = 0;
    pegasus->dr.wLength = 1;
    pegasus->ctrl_urb->transfer_buffer_length = 1;

    usb_fill_control_urb(pegasus->ctrl_urb, pegasus->usb,
                         usb_sndctrlpipe(pegasus->usb, 0),
                         (char *) &pegasus->dr,
                         tmp, 1, ctrl_callback, &pegasus);

    //add_wait_queue(&pegasus->ctrl_wait, &wait);
    //set_current_state(TASK_UNINTERRUPTIBLE);

    mprintk ("Submitting urb...\n");
    if ((ret = usb_submit_urb(pegasus->ctrl_urb, GFP_ATOMIC))) {
        goto out;
    }

    //schedule();
  out:
    //remove_wait_queue(&pegasus->ctrl_wait, &wait);
    kfree(tmp);

    mprintk ("Done with set_register.\n");
    return ret;
}
#endif

static int usb_test_probe(struct usb_interface *intf,
                          const struct usb_device_id *id)
{
    struct usb_device *dev = interface_to_usbdev(intf);
    uprintk ("Probe CALLED(usb_test_probe) for devnum %d product %s. \n\n", dev->devnum, dev->product);
    pegasus_t *pegasus = kmalloc (sizeof (pegasus_t), GFP_KERNEL);
    if (pegasus == NULL) {
        uprintk ("Error: NULL");
        return -ENOMEM;
    }

    usb_get_dev(dev);
#ifdef COMPLEX_MODE
    alloc_urbs (pegasus);
    pegasus->usb = dev;
    
    //init_waitqueue_head(&pegasus->ctrl_wait);
    set_register(pegasus, 0, 0);
#endif
    usb_put_dev(dev);
    uprintk ("Probe returns.\n");
    return 0;
}

static void usb_test_disconnect(struct usb_interface *intf)
{
    void *data = usb_get_intfdata(intf);
    usb_set_intfdata(intf, NULL);
    usb_put_dev(interface_to_usbdev(intf));
}

static struct usb_driver usb_test_driver = {
    .name = driver_name,
    .probe = usb_test_probe,
    .disconnect = usb_test_disconnect,
    .id_table = usb_test_ids,
};

static int __init usb_test_init(void)
{
    uprintk("%s: %s, " DRIVER_DESC "\n", driver_name, DRIVER_VERSION);
    return usb_register(&usb_test_driver);
}

static void __exit usb_test_exit(void)
{
    usb_deregister(&usb_test_driver);
}

module_init(usb_test_init);
module_exit(usb_test_exit);
