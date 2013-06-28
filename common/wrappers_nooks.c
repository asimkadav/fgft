#include <linux/string.h>
#include <linux/gfp.h>
#include <linux/mm.h>
#include "wrappers_nooks.h"
#include "ud_md.h"
#include "slave_master_ud_md.h"
#include "full_slab_verify.h"

#define NULL 0x0

static void MJR_call_kern(unsigned long fid, size_t size, ot_args * oargsp)
{
  struct req_args  out;
  //full_slab_verify();
  out.function_id = fid;
  out.length = size;
  out.data = oargsp;
  dispatch_user_request("MJR_call_kern_sent",  &out);
  return;
}


void
nooks_ot_register_userfn (void * userfnptr,
                          unsigned long fncode)
{
    ot_args args;

    args.userptr = userfnptr;
    args.kernptr = (void *) fncode;

    MJR_call_kern (FIFO_NOOKS_REGISTER_USERFN, sizeof (ot_args), &args);
}

// TODO: investigate replacing the nooks_alloc_arraymem_user
// with a single FIFO call to reduce number of user/kernel
// crosses

/**********************************************************
 *
 * nooks_ot_lookup_user
 *
 * Lookup userptr associated with a kernptr. For a failed
 * translation, we need to allocate memory in the userspace
 * and then make a call to enter a new value into the OT.
 *
 *********************************************************/
void
nooks_ot_lookup_user (void * kernptr,
                      void ** userptr,
                      int size)
{
    ot_args args;

#ifdef USE_KLEE
    if (Sym_should_disp_kern (__func__, __LINE__) == 0) {
        return;
    }
#endif

    // Translating a kernel pointer with any of these characteristics
    // never makes sense
    if (
        (kernptr >= ((void *) -4095) ||
         kernptr <= ((void *)  4095) ||
         kernptr == ((void *) 0x6b6b6b6b6b6b6b6b)
         ) 
        &&
        kernptr != 0x0
        )
    {
        printk ("%s: Bizarre pointer translation, user %p kernel %p\n",
                 __func__, *userptr, kernptr);
        panic("wrapper_nooks failed.");
    }

    if (kernptr) {
        /* A call to translate the pointer */
        args.kernptr = kernptr;
        args.userptr = (void *)INVALID_PTR;

        MJR_call_kern (FIFO_NOOKS_XLATE_K2U, sizeof (ot_args), &args);

        /* At this point, we have the return value from the FIFO */
        if (args.userptr == (void *)INVALID_PTR) {
            /* kmalloc new memory, and create a new entry in the OT */
            if (size <= 0) {
                printk ("Failed to translate pointer: kern %p user %p size %d, allocating 1 byte\n",
                         kernptr, *userptr, size);
                size = 1;
            }

            *userptr = kmalloc(GFP_ATOMIC, size);
            if (*userptr == NULL) {
                printk ("nooks_ot_lookup_user: failed to allocate memory %d\n", size);
                printk ("      ptrs: %p %p\n", kernptr, *userptr);
                panic("wrapper_nooks failed.");
            }
            
            memset(*userptr, 0, size);
            //printk ("nooks_ot_lookup_user:  Alloc'd new memory, kern: 0x%x, user: 0x%x\n", kernptr, *userptr);

            args.kernptr = kernptr;
            args.userptr = *userptr;

            MJR_call_kern (FIFO_NOOKS_ADD_TO_HASH, sizeof (ot_args), &args);
        }
        else {
            /* We have a successful conversion */
            *userptr = args.userptr;
        }
    }
    else {
        /* A NULL kernptr will return a NULL userptr */
        *userptr = 0U;
    }

    return;
}

/**********************************************************
 *
 * nooks_ot_alloc_arraymem_user
 *
 * Allocate a new memory region with arrsize bytes; copy
 * the contents of the old array from kernptr to the new
 * location; modify userptr to point to the new region.
 *
 * NOTES:
 * 1. Need size info to copy old array's contents to
 *    new region.
 * 2. Check sizes appropriately when copying.
 * 3. FIXME: Currently not storing size info, but just
 *    using a memcpy of arrsize bytes. WILL cause a buffer
 *    overflow. FIX: add size info to OT.
 *
 *********************************************************/
void
nooks_ot_alloc_arraymem_user (void ** userptr,
                              int new_number_of_elt,
                              int sizeof_each_elt,
                              int rangestore)
{
    ot_args args;
    array_numelts_args ar_args;
    range_args r_args;
    void *newarr = 0U;
    void *kernptr = 0U;
    int old_number_of_elt;
    int min_number_of_elt;
    bool force_allocate = 0;

    // Non arrays = 1:  all pointers have one element.
    memset (&ar_args, 0, sizeof (array_numelts_args));
    ar_args.userptr = *userptr;
    ar_args.array_numelts = 0;
    MJR_call_kern (FIFO_NOOKS_GET_ARRAY_NUMELTS_REVERSE, sizeof (array_numelts_args), &ar_args);

    if (ar_args.array_numelts == -1) {
        //printk ("nooks_ot_alloc_arraymem_user: ar_args.array_numelts still -1!  This usually means\n");
        //printk ("that the array was allocated via a wrapper.  We'll proceed, and NOT reallocate memory.\n");
        //printk ("Possible BUG in nooks_ot_alloc_arraymem_user / wrapper allocation (1)\n");
        //printk ("problem in wrappers_nooks.\n");
        ar_args.array_numelts = new_number_of_elt;
        force_allocate = 1;
        //return;
    }

    if (ar_args.array_numelts == 0) {
        //printk ("Possible BUG in nooks_ot_alloc_arraymem_user / wrapper allocation (2)\n");
        //printk ("**TODO We will return and allocate memory.  See wrappers_nooks.c and fix this.\n");
        ar_args.array_numelts = new_number_of_elt;
        force_allocate = 1;
        //exit (1);
        //return;
    }

    old_number_of_elt = ar_args.array_numelts;
    min_number_of_elt = (new_number_of_elt > old_number_of_elt) ? old_number_of_elt : new_number_of_elt;

    //printk ("nooks_ot_alloc_arraymem_user:  old_number_of_elt: %d,"
      //       " new_number_of_elt: %d, sizeof_each_elt: %d, rangestore: %d, min_number_of_elt: %d\n",
        //     old_number_of_elt, new_number_of_elt, sizeof_each_elt, rangestore, min_number_of_elt);

    if (new_number_of_elt < 0) {
        //printk ("nooks_ot_alloc_arraymem_user: BUG new_number_of_elts < 0!  Not doing anything!\n");
        panic("wrapper_nooks failed.");
    }

    if ((old_number_of_elt != new_number_of_elt) || (force_allocate == 1)) {
        // Allocate a new array of the appropriate size.
        //printk ("Allocating %d bytes and copying %d bytes.\n", new_number_of_elt, min_number_of_elt);
        newarr = kcalloc(new_number_of_elt, sizeof_each_elt, GFP_ATOMIC);
        if (newarr == NULL) {
            printk ("BUG Memory allocation in user daemon failed!"
                     "  Sizes: %d, %d\n", new_number_of_elt, sizeof_each_elt);
            panic("wrapper_nooks failed.");
        }

        // Copy only as many bytes as are actually valid.
        memcpy (newarr, *userptr, min_number_of_elt * sizeof_each_elt);

        // Remove the old association with userptr
        //  If forcing to allocate previous entry does not exist..
        if (force_allocate == 0 )   {
            args.userptr = *userptr;
            args.kernptr = 0U;
            MJR_call_kern (FIFO_NOOKS_DEL_FROM_HASH_REVERSE, sizeof (ot_args), &args);
        }

        // Store the address of the kernel data structure.
        kernptr = args.kernptr;

        // Create a new association (kernptr, newarr);
        // Has no effect on args structure
        args.userptr = newarr;
        args.kernptr = kernptr; // Not needed, but lets be clear
        MJR_call_kern (FIFO_NOOKS_ADD_TO_HASH, sizeof (ot_args), &args);

        // kernptr is base of old array in kernel
        // *userptr is base of old userptr array

        if (rangestore == 1) {
            // First, delete all the old entries.  We deleted the first
            // one, above.

            r_args.olduserptr = (unsigned long) *userptr;
            r_args.newuserptr = (unsigned long) newarr;
            r_args.newkernptr = (unsigned long) kernptr;
            r_args.new_number_of_elt = new_number_of_elt;
            r_args.sizeof_each_elt = sizeof_each_elt;

            printk ("FIFO_NOOKS_RANGE_UPDATE: userptr: %p, newarr: %p, kernptr: %p,"
                     " new_number_of_elt: %d, sizeof_each_elt: %d\n",
                     *userptr,
                     newarr,
                     kernptr,
                     new_number_of_elt,
                     sizeof_each_elt);

            MJR_call_kern (FIFO_NOOKS_RANGE_UPDATE, sizeof (range_args), &r_args);
        }

        // Store the number of elements in the array.
        nooks_ot_store_array_numelts_reverse (newarr, new_number_of_elt);

        // Assign newarr to *kernptr;
        *userptr = newarr;
        //printk ("nooks_ot_alloc_arraymem_user:  New mapping established:"
          //       " kern %p, user %p\n", kernptr, *userptr);
    } else {
        // userptr is unmodified
        return;
    }

    return;
}

/**********************************************************
 * nooks_ot_store_array_numelts
 *
 * Specialized function for updating the number of elements
 * associated with a specific pointer to array.
 *
 * The idea is that each pointer in the OT has an
 * associated number of elements.  In many cases, this
 * number is 1.  If we ever need to change this value,
 * call this function.
 *********************************************************/
void
nooks_ot_store_array_numelts_reverse (void *userptr,
                                      int new_number_of_elt)
{
    array_numelts_args ar_args;

#ifdef USE_KLEE
    if (Sym_should_disp_kern (__func__, __LINE__) == 0) {
        return;
    }
#endif

    memset (&ar_args, 0, sizeof (array_numelts_args));
    ar_args.userptr = userptr;
    ar_args.array_numelts = new_number_of_elt;

    MJR_call_kern (FIFO_NOOKS_STORE_ARRAY_NUMELTS_REVERSE, sizeof (ar_args), &ar_args);
}

/**********************************************************
 *
 * nooks_ot_storeoffset_user
 *
 * Given a userptr and an offset, lookup the mapping for
 * userptr in the object tracker. If no mapping exists, then
 * do nothing. If a mapping exists (and say it is kernptr)
 * then do the following:
 * -> Search old (kernptr+off, userptr'+off) association.
 * -> If userptr+off != userptr'+off, then memcpy from
 *    userptr'+off to userptr+off.
 * -> Delete the (kernptr+off, userptr'+off) association.
 * -> Add the (kernptr+off,userptr+off) assocation.
 *
 *********************************************************/
void
nooks_ot_storeoffset_user(void * userptr,
                          int offset,
                          int size)
{
    ot_args args;
    void *kernptr;
    void *kernptr_plus_offset;
    void *userptr_plus_offset;
    void *old_userptr_plus_offset = (void *) INVALID_PTR;


    // Check first if we already have an entry for userptr + offset.
    userptr_plus_offset = userptr + offset;
    args.kernptr = (void *)INVALID_PTR;
    args.userptr = userptr_plus_offset;
    MJR_call_kern (FIFO_NOOKS_XLATE_U2K, sizeof (ot_args), &args);

    // No entry exists already for userptr + offset.
    if (args.kernptr == (void *) INVALID_PTR) {
        // Check if we have an entry for userptr.
        args.kernptr = (void *) INVALID_PTR;
        args.userptr = userptr;
        MJR_call_kern (FIFO_NOOKS_XLATE_U2K, sizeof (ot_args), &args);

        // If an entry exists, create the offset entry.
        if (args.kernptr != (void *) INVALID_PTR) {
            kernptr = args.kernptr;
            kernptr_plus_offset = kernptr + offset;

            // Search and delete any old associations of
            // kernptr_plus_offset.
            args.kernptr = kernptr_plus_offset;
            args.userptr = (void *)INVALID_PTR;
            MJR_call_kern (FIFO_NOOKS_DEL_FROM_HASH, sizeof (ot_args), &args);

            old_userptr_plus_offset = args.userptr;
            if ((old_userptr_plus_offset != (void *) INVALID_PTR) &&
                (userptr_plus_offset != old_userptr_plus_offset)) {
                memcpy(userptr_plus_offset, old_userptr_plus_offset, size);
            }

            args.kernptr = kernptr_plus_offset;
            args.userptr = userptr_plus_offset;
            MJR_call_kern (FIFO_NOOKS_ADD_TO_HASH, sizeof (ot_args), &args);
        }
        else {
            printk ("nooks_ot_storeoffset_user Possible BUG!!\n");
        }
    }

    return;
}

///////////////////////////
// Associates the given user level pointer with
// the given kernel level pointer.  Used
// with functions like dma_alloc_coherent and ioremap.
///////////////////////////
void nooks_ot_kern_memory_assoc (void * userptr, void * kernptr) {
    ot_args args;

#ifdef USE_KLEE
    if (Sym_should_disp_kern (__func__, __LINE__) == 0) {
        return;
    }
#endif

    // To be filled in shortly
    args.kernptr = kernptr;
    args.userptr = userptr;
    MJR_call_kern (FIFO_NOOKS_MEMORY_ASSOC, sizeof (ot_args), &args);
}

#if 0

///////////////////////////
// Given:
//  base:  the parameter passed to ioremap
//  length:  the parameter passed to ioremap
//  kernptr:  the return value from ioremap
// Does this:
//  - Memory maps the specified addresses into the process address space.
//  - Establishes a mapping in the nooks OT between the physical IOMEM
//    addresses and the user-level virtual address
// Returns:
//  A user-level pointer that can be used to access the
//  IO memory in question.
///////////////////////////
void *nooks_ot_establish_iomem (unsigned long base, unsigned long length, void *kernptr) {
    void *vadr;

    // Perform the memory mapping
    vadr = mmap(0, length, PROT_READ | PROT_WRITE, MAP_SHARED, devmem_fd, base);
    printk ("nooks_establish_iomem: vadr: 0x%x devmem_fd: %d, base: 0x%x, length: %u, kernptr: 0x%x\n",
             vadr, devmem_fd, base, length, kernptr);
    if (vadr == (void *) -1)
    {
        printk ("nooks_establish_iomem: mmap failed.  errno: %d\n", errno);
        exit (6);
    }

    // Establish nooks OT mapping.
    nooks_ot_kern_memory_assoc (vadr, kernptr);

    return vadr;
}

#endif

///////////////////////////////
//
// Add a new range to the range query
// structure.  Used by the allocator
// wrappers.
//
//////////////////////////////
void nooks_ot_add_range (unsigned long kern_ptr_base,
                         unsigned long user_ptr_base,
                         int number_of_elt,
                         int sizeof_each_elt)
{
    range_args args;

#ifdef USE_KLEE
    if (Sym_should_disp_kern (__func__, __LINE__) == 0) {
        return;
    }
#endif

    args.olduserptr = 0;
    args.newkernptr = kern_ptr_base;
    args.newuserptr = user_ptr_base;
    args.new_number_of_elt = number_of_elt;
    args.sizeof_each_elt = sizeof_each_elt;
    MJR_call_kern (FIFO_NOOKS_RANGE_UPDATE, sizeof (range_args), &args);
}

/////////////////////////////////////////////////
// Frees an already-created range.
// Useful if we're creating lots of ranges
// since the memory leaks would be too extensive
// if we never freed them.
/////////////////////////////////////////////////
void nooks_ot_free_range (unsigned long kern_ptr_base,
                          unsigned long user_ptr_base)
{
    range_args args;

#ifdef USE_KLEE
    if (Sym_should_disp_kern (__func__, __LINE__) == 0) {
        return;
    }
#endif

    args.olduserptr = 0;
    args.newkernptr = kern_ptr_base;
    args.newuserptr = user_ptr_base;
    args.new_number_of_elt = 0;
    args.sizeof_each_elt = 0;
    MJR_call_kern (FIFO_NOOKS_RANGE_FREE, sizeof (range_args), &args);
}

//
// DEBUG function, just returns the user->mapping if any.
// Return NULL if no mapping exists.
//
void *nooks_debug_translate_u2k (void *user_ptr) {
    unsigned long ptr = (unsigned long) user_ptr;

#ifdef USE_KLEE
    if (Sym_should_disp_kern (__func__, __LINE__) == 0) {
        return NULL;
    }
#endif

    MJR_call_kern (FIFO_NOOKS_DEBUG_TRANSLATE_U2K, sizeof (void *), &ptr);
    return (void *) ptr;
}
