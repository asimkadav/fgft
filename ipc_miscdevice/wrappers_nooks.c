#include "common_h.h"
#include "misc.h"

#include "../common/MJR_external_functions.h"
#include "wrappers_nooks.h"

extern pnooks_hash_table_t g_nooks_table;


/**********************************************************
 *
 * handle_nooks_xlate_u2k
 *
 * Find the kernel pointer corresponding to a userpointer
 *
 *********************************************************/
void handle_nooks_xlate_u2k (struct req_args *inarg) {
    ot_args *args;
    int res;
    void *tmpkernptr = NULL;
    unsigned long range_kern_pointer = 0;

    if (inarg->length != sizeof (ot_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }
    
    args = (ot_args *) inarg->data;

    if (args->userptr == 0) {
        args->kernptr = 0;
        return;
    }

    res = nooks_find_in_hash_reverse (g_nooks_table,
                                      DUMMYTYP,
                                      args->userptr,
                                      &tmpkernptr);
    if (res == -ENOENT) {
        tmpkernptr = NULL;
        nooks_range_lookup_kern (g_nooks_table->range_table,
                                 (unsigned long) args->userptr,
                                 &range_kern_pointer);
        if (range_kern_pointer == 0) {
            args->kernptr = (void *) INVALID_PTR;
            //args->userptr = inarg.userptr;
            //uprintk ("<mischelp:%s> range lookup found nothing\n", __func__);
        } else {
            args->kernptr = (void *) range_kern_pointer;
            //args->userptr = inarg.userptr;
            //uprintk ("<mischelp:%s> range lookup found 0x%lx\n", __func__, range_kern_pointer);
        }
    }
    else {
        args->kernptr = tmpkernptr;
        //args->userptr = inarg.userptr;
    }

    //uprintk("<mischelp:%s> userptr: %p, kernptr: %p\n", __func__, args->userptr, args->kernptr);
}

/**********************************************************
 *
 * handle_nooks_xlate_k2u
 * Handles IOCTL_NOOKS_XLATE_K2U
 *
 * Function that translates a kernpointer to a userpointer.
 * If we don't have an entry, we return an INVALID_PTR.
 * 
 *********************************************************/
void handle_nooks_xlate_k2u (struct req_args *inarg) {
    ot_args *args;
    int res;
    void *tmpusrptr = NULL;
    unsigned long range_user_pointer;

    if (inarg->length != sizeof (ot_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }
    
    args = (ot_args *) inarg->data;

    if (args->kernptr == 0) {
        args->userptr = 0;
        return;
    }

    res = nooks_find_in_hash (g_nooks_table, 
                              DUMMYTYP, 
                              args->kernptr, 
                              &tmpusrptr);

    if (res == -ENOENT) {
        range_user_pointer = 0;
        nooks_range_lookup_user (g_nooks_table->range_table,
                                 (unsigned long) args->kernptr,
                                 &range_user_pointer);
        if (range_user_pointer == 0) {
            args->userptr = (void *) INVALID_PTR;
            // args->kernptr = inarg.kernptr;
            //uprintk ("<mischelp:%s> range lookup found nothing\n", __func__);
        } else {
            args->userptr = (void *) range_user_pointer;
            // args->kernptr = inarg.kernptr;
            uprintk ("<mischelp:%s> range lookup found 0x%lx\n", __func__, range_user_pointer);
        }
    } 
    else {
        args->userptr = tmpusrptr;
        // args->kernptr = inarg.kernptr;
    }
    
    //uprintk("<mischelp:%s> userptr: %p, kernptr: %p\n",
      //      __func__, args->userptr, args->kernptr);
    return;
}

/**********************************************************
 * Perform basic validation of a pair of pointers
 * for nooks_add_to_hash
 *********************************************************/
static void validate_pointer_pair (void *userptr, void *kernptr) {
    if (
        kernptr >= ((void *) -4095) ||
        kernptr <= ((void *)  4095) ||
        userptr >= ((void *) -4095) ||
        userptr <= ((void *)  4095) ||
        kernptr == ((void *) 0x6b6b6b6b6b6b6b6b) ||
        userptr == ((void *) 0x6b6b6b6b6b6b6b6b)
        )
    {
        panic ("%s:  Trying to add fascinating pointers to hash: user %p kern %p\n",
               __func__, userptr, kernptr);
    }
}

/**********************************************************
 *
 * handle_nooks_add_to_hash
 * Handles IOCTL_NOOKS_ADD_TO_HASH
 *
 * Create a new entry in the nooks hash table.
 * 
 *********************************************************/
void handle_nooks_add_to_hash (struct req_args *inarg) {
    ot_args *args;
    int res;

    if (inarg->length != sizeof (ot_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }
    
    args = (ot_args *) inarg->data;

    if (args == NULL) {
        panic ("%s:  No arguments passed\n", __func__);
    }
    
    validate_pointer_pair(args->userptr, args->kernptr);

   	
    res = nooks_add_to_hash (g_nooks_table,
                             DUMMYTYP,
                             args->kernptr,
                             args->userptr);
    

    //uprintk("<mischelp:%s> userptr: %p, kernptr: %p\n",
      //      __func__, args->userptr, args->kernptr);
    
    if (res == -ENOMEM) {
        panic("BUG in %s 2\n", __func__);
    }
}

/**********************************************************
 *
 * handle_nooks_del_from_hash.
 * Handles IOCTL_NOOKS_DEL_FROM_HASH
 *
 * Delete the entry associated with a kernpointer, and get
 * the user pointer associated with this kernpointer.
 * 
 *********************************************************/
void handle_nooks_del_from_hash (struct req_args *inarg) {
    ot_args *args;
    int res;

    if (inarg->length != sizeof (ot_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }

    args = (ot_args *) inarg->data;

    // userptr is not used as input.
    if (args->userptr != (void *) INVALID_PTR) {
        panic ("%s BUG userptr != INVALID_PTR, ignoring...\n", __func__);
    }
    
    res = nooks_del_from_hash (g_nooks_table,
                               DUMMYTYP,
                               args->kernptr,
                               &args->userptr);
    
//    uprintk("%s userptr: %p, kernptr: %p\n", __func__,
  //          args->userptr, args->kernptr);
}

/**********************************************************
 *
 * handle_nooks_del_from_hash_reverse.
 * Handles IOCTL_NOOKS_DEL_FROM_HASH_REVERSE
 *
 * Delete the entry associated with a userpointer, and get
 * the kernel pointer associated with this userpointer.
 * 
 *********************************************************/
void handle_nooks_del_from_hash_reverse (struct req_args *inarg) {
    ot_args *args;

    if (inarg->length != sizeof (ot_args)) {
        panic("BUG in handle_nooks_del_from_hash_reverse\n");
    }

    args = (ot_args *) inarg->data;
    
    // kernptr is not used as input.
    if (args->kernptr != 0) {
        panic ("%s BUG kernptr != NULL...\n", __func__);
    }
    
    nooks_del_from_hash_reverse (g_nooks_table,
                                 DUMMYTYP,
                                 args->userptr,
                                 &args->kernptr);
    
    //uprintk("<mischelp:%s> userptr: %p, kernptr: %p\n", __func__,
      //      args->userptr, args->kernptr);
}

/**********************************************************
 *
 * handle_nooks_register_userfn
 * IOCTL_NOOKS_REGISTER_USERFN
 *
 * This implements the user half of the protocol to register
 * function pointers. First, search for an entry with the
 * function code as data (as would have been entered by the
 * kernel half of the protocol, had it run first). If such
 * an entry exists, record the key value. Else set the key
 * value to be the function code itself. Then create an entry
 * with (key, userfnptr)
 *
 *********************************************************/
void handle_nooks_register_userfn (struct req_args *inarg) {
    ot_args *args;
    int res;
    void *userfnptr;
    void *kernfnptr;
    unsigned long fncode;
    
    uprintk("Registering function pointers: User Half of Protocol\n");

    args = (ot_args *) inarg->data;

    userfnptr = args->userptr;
    fncode = (unsigned long) args->kernptr;

    uprintk ("handle_nooks_register_userfn, fncode: %lu\n", fncode);
    uprintk ("handle_nooks_register_userfn, userfnptr: %p\n", userfnptr);

    // Search for entry in hashtab with FNCODE as DATA.
    res = nooks_del_from_hash_reverse(g_nooks_table,
                                      FUNCTIONTYP,
                                      (void *) fncode,
                                      &kernfnptr);
    uprintk ("handle_nooks_register_userfn, all done with nooks_del_from_hash_reverse\n");

    if (res == -ENOENT) {
        // NO entry was found. Set kernfnptr to be fncode.
        kernfnptr = (void *) fncode;
    }

    validate_pointer_pair(kernfnptr, userfnptr);

    // Create a new entry (kernfnptr, userfnptr);
    res = nooks_add_to_hash (g_nooks_table,
                             FUNCTIONTYP,
                             kernfnptr,
                             userfnptr);

    //uprintk("<mischelp:%s> userfnptr: %p, kernfnptr: %p\n", __func__, userfnptr, kernfnptr);

    if (res == -ENOMEM) {
        panic("BUG in handle_nooks_register_userfn\n");
    }
}

//
// Userdaemon encountered a memory allocation.
// First, the user-daemon calls the appropriate kernel
// memory allocation function, e.g. dma_alloc_coherent.
// Then, the userdaemon memory maps that address into
// its address space.  Finally, the userdaemon registers
// the user/kernel mapping by calling in here.
//
void handle_nooks_memory_assoc (struct req_args *inarg) {
    ot_args *args;
    //uprintk("<mischelp:%s> Establishing memory association\n", __func__);

    if (inarg->length != sizeof (ot_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }

    args = (ot_args *) inarg->data;
    
    if (nooks_ot_kern_memory_assoc (g_nooks_table, args->userptr, args->kernptr) != 0) {
        panic("<mischelp:%s> BUG 2\n", __func__);
    }

    // No need to copy anything back.
}

//
// Userdaemon called alloc_arraymem_user, so it needs to see how
// big the array is currently.  We call the nooks_get_array_numelts_reverse
// function to get this information
//
// Given a user pointer, tells how many elements are in the array.
//
void handle_nooks_get_array_numelts_reverse (struct req_args *inarg) {
    array_numelts_args *args;
    //printk ("<mischelp:%s>\n", __func__);

    if (inarg->length != sizeof (array_numelts_args)) {
        panic ("%s: length mismatch %d %lu\r\n", __func__, inarg->length, sizeof (array_numelts_args));
    }

    args = (array_numelts_args *) inarg->data;

    if (nooks_get_array_numelts_reverse (g_nooks_table, DUMMYTYP, args->userptr, &args->array_numelts) != 0) {
        nooks_range_dump (g_nooks_table->range_table);
        //dump_stack();
        printk("PANIC::FIX THIS::<mischelp:%s> BUG 2, array_numelts currently %d\n",
              __func__, args->array_numelts);
        // TODO This function and related code needs a redesign.
        //printk ("<mischelp:%s> Warning: could not find array size.\n", __func__);
    }
}

//
// Like handle_nooks_get_array_numelts, but stores the number of elements.
//
void handle_nooks_store_array_numelts_reverse (struct req_args *inarg) {
    array_numelts_args *args;
    uprintk ("<mischelp:%s>\n", __func__);

    if (inarg->length != sizeof (array_numelts_args)) {
        panic ("<mischelp:%s>: length mismatch %d %lu\r\n", __func__, inarg->length, sizeof (array_numelts_args));
    }

    args = (array_numelts_args *) inarg->data;

    if (nooks_store_array_numelts_reverse (g_nooks_table, DUMMYTYP, args->userptr, args->array_numelts) != 0) {
        panic("<mischelp:%s> BUG 2\n", __func__);
    }
}

// Called to remove an old range and add a new range.
void handle_nooks_range_update (struct req_args *inarg) {
    range_args *args;
    int newsize;

    if (inarg->length != sizeof (range_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }

    args = (range_args *) inarg->data;

    nooks_range_remove (g_nooks_table->range_table,
                        NOOKS_RANGE_REMOVE_IGNORE,
                        args->olduserptr);
    //nooks_range_dump (g_nooks_table->range_table);

    newsize = args->new_number_of_elt * args->sizeof_each_elt;
    nooks_range_add (g_nooks_table->range_table,
                     args->newkernptr,
                     args->newuserptr,
                     newsize);
    //nooks_range_dump (g_nooks_table->range_table);
}

void handle_nooks_range_free (struct req_args *inarg) {
    range_args *args;
    
    if (inarg->length != sizeof (range_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }

    args = (range_args *) inarg->data;
    if (args->newkernptr == 0 && args->newuserptr == 0) {
        panic ("Need to specify at least the kernel pointer or the user pointer of the range to free");
    }

    nooks_range_remove(g_nooks_table->range_table,
                       args->newkernptr,
                       args->newuserptr);
}

// Called to add a new range.  Used by the various allocator wrappers,
// e.g. dma_alloc_coherent.
// The input is a pointer to a range_args structure.
// The olduserptr field of that structure must be NULL!
void handle_nooks_range_add (struct req_args *inarg) {
    range_args *args;

    if (inarg->length != sizeof (range_args)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }

    args = (range_args *) inarg->data;

    if (args->olduserptr != 0) {
        panic("<mischelp:%s> BUG 2\n", __func__);
    }

    nooks_range_add (g_nooks_table->range_table,
                     args->newkernptr,
                     args->newuserptr,
                     args->new_number_of_elt * args->sizeof_each_elt);
    // TODO: could perform a consistency check here:  ensure the new range
    // doesn't overlap any existing range or pointers.  This must be the case,
    // or there is a bug either in our code, or in the driver code (e.g. it
    // forgot to free some structure).
}

//////////////////////////////
// DEBUG function
//////////////////////////////
// Input: User pointer
// Output: Kern pointer
void handle_nooks_debug_translate_u2k (struct req_args *inarg) {
    unsigned long *arg;
    int res;
    unsigned long kernptr;

    if (inarg->length != sizeof (void *)) {
        panic ("Unknown data quantity received in %s, size %d\n", __func__, inarg->length);
    }

    arg = (unsigned long *) inarg->data;

    res = nooks_ot_lookup_kern_no_create (g_nooks_table,
                                          (void *) *arg,
                                          (void **) &kernptr);

    if (res == -ENOENT) {
        kernptr = 0;
    }

    memcpy (inarg->data, &kernptr, sizeof (void *));
}

/*
EXPORT_SYMBOL(handle_nooks_debug_translate_u2k);
EXPORT_SYMBOL(handle_nooks_range_free);
EXPORT_SYMBOL(handle_nooks_range_update);
EXPORT_SYMBOL(handle_nooks_xlate_u2k);
EXPORT_SYMBOL(handle_nooks_xlate_k2u);
EXPORT_SYMBOL(handle_nooks_add_to_hash);
EXPORT_SYMBOL(handle_nooks_del_from_hash);
EXPORT_SYMBOL(handle_nooks_register_userfn);
EXPORT_SYMBOL(handle_nooks_get_array_numelts_reverse);
EXPORT_SYMBOL(handle_nooks_store_array_numelts_reverse);
*/
