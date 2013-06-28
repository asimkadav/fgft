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

#include <linux/module.h>
#include <linux/kernel.h>

MODULE_AUTHOR("Matthew Renzelmann");
MODULE_LICENSE("Dual BSD/GPL");

// For some reason, the generated code doesn't work with UML sometimes.
// e.g.
//
// overflow in relocation type 11 val 83dd802c
// `module_format' likely not compiled with -mcmodel=kernel
// insmod: error inserting './module-format.ko': -1 Invalid module format
//

char *x;

int module_format_init (void) {
    x = "Hello there!";
    printk ("%s\r\n", x);
    return 0;
}

void module_format_exit (void) {
    return;
}

module_init(module_format_init);
module_exit(module_format_exit);
