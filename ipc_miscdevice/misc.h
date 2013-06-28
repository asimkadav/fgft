#ifndef _MISCDEVICE_H
#define _MISCDEVICE_H

// Driver number
#define MISC_MINOR	45

// ioctl definitions
#include "../common/ud_md.h"

// req_args definition
#include "../common/slave_master_ud_md.h"

// Register definitions
#define REGISTER_FROM_KERNEL                   7361
#define REGISTER_TO_KERNEL                     7362
#define REGISTER_THREADS_IN_USER               7364
#define REGISTER_LOCK_COUNT                    7365
#define REGISTER_REQUEST_IRQ                   7366
#define REGISTER_DONE_DEMARSHALING             7367
#define REGISTER_NOOKS_HT                      7368

// Register function typedefs, corresponding to each
// of the above entries
typedef void (*pUnblock)(struct req_args *);
typedef int* (*pDispKern)(char *, void *);

// Function prototypes
void unblock_user_thread (char *function_name, struct req_args *);
void * check_and_set_odft_failure_record(int pid); 
extern int register_odft_failure_record (int (*odft_fn)(int));
extern int register_odft_mm_failure_record (int (*odft_fn)(int));

#define DUMMYTYP 1 /* TODO: Must change this */
#define FUNCTIONTYP 1

// Fault injection op-codes
//
#define FLIP_POINTERS 0x101
#define CORRUPT_STACK 0x102
#define REALLY_CORRUPT_STACK 0x103
#define RESET_COUNTS 0x104

#endif
