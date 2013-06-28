#include "common_h.h"
#include "misc.h"
#include "../common/slave_master_ud_md_marshaling.h"
#include "../common/MJR_external_functions.h"

#include "wrappers_misc.h"


//
// Returns the current jiffies value.
//
// This is necessary because jiffies is always being
// updated in the kernel, and marshaling this value
// has no meaning.
//
// We have to do a transform in the user driver to
// call this wrapper instead.
//
void handle_jiffies (struct req_args *inarg)
{
    unsigned long long tmp_jiffies;
    inarg->length = sizeof (unsigned long long);
    kfree (inarg->data);
    inarg->data = kmalloc (GFP_KERNEL, inarg->length);
    tmp_jiffies = jiffies;
    memcpy (inarg->data, &tmp_jiffies, sizeof (jiffies));
}

void handle_printk (struct req_args *inarg)
{
    printk((const char *) inarg->data);
}
