#ifndef SLAVE_TOP_H
#define SLAVE_TOP_H

#include "slave_master_ud_md.h"
#include "slave_master_ud_md_marshaling.h"
#include "wrappers_nooks.h"

#define _spin_lock_irqsave spin_lock_irqsave_asim
#define _spin_unlock_irqrestore spin_unlock_irqrestore_asim

#define _spin_lock_irq spin_lock_irq_asim
//#define _spin_unlock_irq spin_unlock_irq_asim
#define __raw_spin_unlock __raw_spin_unlock_asim

// spin_lock_asim definitiosn here

//#include "custom_klee.h"
//#include "check_routines.h"
//#include "syscall_replacements.h"

//#include "MJR_external_functions.h"

#define WRAPPERS_SYM_DRIVER_STEP2

// Additionally present here so "original" C driver code
// compiles/runs in user.
#define const /* */

#define CONFIG_BASE_SMALL 0

static void kfree(void * addr)	{
 //DO nothing
}


extern unsigned long int strtoul(const char *nptr, char **endptr, int base);
extern unsigned long strlen(const char *s);
#ifdef __KERNEL__
extern int sprintf(char * buf, const char * fmt, ...);
#endif

// Defines DEMARSHBUF_FREE
#include "demarshbuf_free.h"

#endif
