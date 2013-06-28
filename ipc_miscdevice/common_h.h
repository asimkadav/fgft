// The idea is simply to include this file rather than each of these other files individually.
// We use this file throughout the miscellaneous device source code.

#include "../common/testing_ep.h"
#include "../common/uprintk.h"
#include "../common/master_md_nooks_api.h"
#include "nooks/nooks-i.h"
#include "nooks/nooks-range-query.h"

#include <linux/module.h>

#include <linux/sched.h>
#include <linux/fs.h>
#include <linux/miscdevice.h>
#include <linux/errno.h>
#include <linux/vmalloc.h>
#include <linux/mm.h>
#include <linux/uaccess.h>
#include <linux/interrupt.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/workqueue.h>

#include <linux/semaphore.h>
#include <linux/io.h>
#include <linux/dma-mapping.h>
