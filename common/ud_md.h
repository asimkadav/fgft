// This file is included both by the
// miscellaneous device and within host-ud.c

#ifndef UD_MD_H
#define UD_MD_H

#include "slave_master_ud_md.h"

// Object tracking
typedef struct {
    void *userptr;
    void *kernptr;
} ot_args;

typedef struct {
    void *userptr;
    int array_numelts;
} array_numelts_args;

typedef struct {
    unsigned long olduserptr;
    unsigned long newkernptr;
    unsigned long newuserptr;
    int new_number_of_elt;
    int sizeof_each_elt;
} range_args;

#define WRAPPERS_ALLOC_GET_FREE_PAGES            1
//#define WRAPPERS_ALLOC_FREE_PAGES                2
#define WRAPPERS_ALLOC_ALLOC_ETHERDEV_MQ         3
#define WRAPPERS_ALLOC_DEV_ALLOC_SKB             4
#define WRAPPERS_ALLOC_NETDEV_ALLOC_SKB          5
#define WRAPPERS_ALLOC_KFREE_SKB                 6
#define WRAPPERS_ALLOC_DEV_KFREE_SKB_ANY         7
#define WRAPPERS_ALLOC_COPY_TO_USER             11
#define WRAPPERS_ALLOC_COPY_FROM_USER           12
#define WRAPPERS_ALLOC_CURRENT_THREAD_INFO      13
#define WRAPPERS_ALLOC_VIRT_TO_PAGE             14
#define WRAPPERS_ALLOC_INIT_WORK                15
#define WRAPPERS_ALLOC_INIT_DELAYED_WORK        16

// The following sets of FIFOS are used in the function_id
// field of the req_args structure.

// Miscellaneous wrappers
#define FIFO_RETURN                            20

#define FIFO_WRAPPER_PRINTK                    30
#define FIFO_WRAPPER_JIFFIES                   31

// Strictly kernel -> user.  Means we're done, no
// more info from user will be read.
#define FIFO_EXIT                               99

// Nooks calls
#define FIFO_NOOKS_XLATE_U2K                   100
#define FIFO_NOOKS_XLATE_K2U                   101
#define FIFO_NOOKS_ADD_TO_HASH                 102
#define FIFO_NOOKS_DEL_FROM_HASH               103
#define FIFO_NOOKS_DEL_FROM_HASH_REVERSE       104
#define FIFO_NOOKS_REGISTER_USERFN             105
#define FIFO_NOOKS_MEMORY_ASSOC                106
#define FIFO_NOOKS_GET_ARRAY_NUMELTS_REVERSE   107
#define FIFO_NOOKS_STORE_ARRAY_NUMELTS_REVERSE 108
#define FIFO_NOOKS_RANGE_UPDATE                109
#define FIFO_NOOKS_RANGE_FREE                  110
#define FIFO_NOOKS_RANGE_ADD                   111
#define FIFO_NOOKS_DEBUG_TRANSLATE_U2K         112

//
// SymDrive-specific testing
// Strictly kernel -> user
//
#define SYMDRIVE_TESTING                       200

// Other #defines / not function_ids
#define INVALID_PTR 0xffffffffffffffff /* 64-bit */

#endif
