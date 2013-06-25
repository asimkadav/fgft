#include <linux/module.h>

#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/miscdevice.h>
#include <linux/errno.h>
#include <linux/vmalloc.h>
#include <linux/mm.h>
#include <linux/uaccess.h>
#include <linux/interrupt.h>
#include <linux/sched.h>

#include <linux/dma-mapping.h>
#include <linux/string.h> // For marshaling/demarshaling

#include "modif_annots.h"

#define WRAPPERS_SYM_DRIVER_STEP1
#include "../../common/wrappers_sym.h" // MJR Wrappers--need them preprocessed

MODULE_LICENSE("GPL");

static int use_io = 0;

module_param(use_io, int, 0);
MODULE_PARM_DESC(use_io, "Force use of I/O access mode. 0=MMIO 1=PIO"); 

static struct miscdevice mjr_driver;
ssize_t mjr_driver_write (struct file *, const char __user *, size_t, loff_t *);
int mjr_driver_ioctl(struct inode *, struct file *, unsigned int, unsigned long);

int random_func (void) {
    return 5;
}

struct blah {
    void *fn_pointer;
};

static struct blah test_struct = {
    .fn_pointer = (void *)&random_func,
};

// Implemented in the misc device
// Used to test kernel replay functionality.
int test_driver_function (int *x);

// Used to test passing symbolic values to kernel
void test_driver_function2 (unsigned char x);

struct file_operations mjr_driver_fops =
{
    .owner = THIS_MODULE,
    .write = mjr_driver_write,
    .ioctl = mjr_driver_ioctl
};

ssize_t mjr_driver_write (struct file *f, const char *data, size_t length, loff_t *offset) {
    unsigned char read_data;

    read_data = inb (0);
    if (read_data == 0) {
        uprintk (KERN_ERR "******* Device type 0\r\n");
    } else if (read_data > 0 && read_data < 10) {
        uprintk (KERN_ERR "******* Device type middle %d\r\n", read_data);
    } else {
        uprintk (KERN_ERR "******* Device type tail %d\r\n", read_data);
    }

    return length;
}

static int func(unsigned char concrete) {
    unsigned char b;
    int x, ret;

    ///////////////////////////////////
    concrete = test_struct.fn_pointer;
    ///////////////////////////////////

    b = (unsigned char) inb (0) + (unsigned char) concrete;
   
    ret = test_driver_function (&x);
    uprintk (KERN_ERR "ret: %d, x: %d\n", ret, x);

    switch (b) {
        case 1:
            uprintk (KERN_ERR "********* Byte 1 read, calling kernel to print the value\n");
            test_driver_function2 (b);
            break;
        case 2:
            uprintk (KERN_ERR "********* Byte 2 read\n");
            ret = test_driver_function (&x);
            if (ret == 5) {
                uprintk (KERN_ERR "ret = 5\n");
            } else {
                uprintk (KERN_ERR "ret != 5\n");
            }
            uprintk (KERN_ERR "ret: %d, x: %d\n", ret, x);
            //b -= 2;
            //uprintk (KERN_ERR "Result %d\n", 100 / b); // Crash!  Oh noes!
            break;
        case 3:
            uprintk (KERN_ERR "********* Byte 3 read\n");
            ret = test_driver_function (&x);
            uprintk (KERN_ERR "ret: %d, x: %d\n", ret, x);
            break;
        case 4:
            uprintk (KERN_ERR "********* Byte 4 read\n");
            break;
        default:
            if ((b & 128) == 0) {
                uprintk (KERN_ERR "********* b&128\n");
            } else {
                uprintk (KERN_ERR "********* !b&128\n");
            }
            break;
    }

    uprintk (KERN_ERR "Returning\n");

    return b;
}

int mjr_driver_ioctl(struct inode *inode, struct file *fp, unsigned int cmd, unsigned long arg){
    int rc=0;
    uprintk (KERN_ERR "The command was %d, arg was %d\n", cmd, arg);
    switch (cmd) {
        case (1): {
            func (arg);
            break;
        }

        case (2): {
            char *local_buf = kmalloc (128, GFP_KERNEL);
            char *omg_a_string = kmalloc (128, GFP_KERNEL);
            copy_from_user(local_buf, (void *) arg, 128);
            strcpy (omg_a_string, local_buf);
            uprintk (KERN_ERR "A string: %s\n", omg_a_string);
            break;
        }
        default:
            break;
    }
    return rc;
}

static const char *driver_name = "mjr_driver";

int __init mjr_driver_init(void) {
    int retval;
    char data;
    
    // Initialize linux kernel junk
    mjr_driver.minor = 47; // Magic number, I have no idea if this is a good choice.
    mjr_driver.name = driver_name; // Shows up in /dev as this.
    mjr_driver.fops = &mjr_driver_fops;
    retval = misc_register(&mjr_driver);

    uprintk (KERN_ERR "use_io: %d\n", use_io);

    return retval;
}

void __exit mjr_driver_cleanup(void){
    int number = misc_deregister(&mjr_driver);
    if (number < 0) {
        uprintk (KERN_ERR "misc_deregister failed. %d\n", number);
    }
}

module_init(mjr_driver_init)
module_exit(mjr_driver_cleanup)
