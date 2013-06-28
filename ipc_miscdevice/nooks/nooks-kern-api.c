/* Microdriver object tracker
 *
 * nooks-kern-api.c -- API to the object tracker. 
 *
 * Vinod Ganapathy
*/

#include "nooks-i.h"
#include <linux/module.h>

#include "../../common/master_md_nooks_api.h"
#include "../../common/uprintk.h"
#include "../../common/full_slab_verify.h"

#include "nooks-range-query.h"

#define NOOKS_LOOKASIDE_HIGHWATER 0 /* We don't need this */
#define INVALID_PTR 0xffffffff

#define DUMMYTYP 1 /* TODO: Must change this */
#define FUNCTIONTYP 1

/**********************************************************
 *
 * nooks_ot_create_new_table 
 * 
 * Create and initialize a new hash table of a given size. 
 * 
 *********************************************************/
void *
nooks_ot_create_new_table (int size)
{
    pnooks_hash_table_t newtable = NULL;
    pnooks_lookaside_list_t newlookaside = NULL;
    
    // Create and initialize a new lookaside list
    newlookaside = (pnooks_lookaside_list_t)
        kmalloc(sizeof(nooks_lookaside_list_t), GFP_KERNEL);
    if (newlookaside == NULL) {
        panic ("Could not create nooks hash table 1\n");
    }
    
    nooks_mm_init_lookaside(newlookaside,
                            NOOKS_LOOKASIDE_HIGHWATER, // Should not matter to us. 
                            sizeof(nooks_hash_entry_t),
                            1);
    
    // Create and initialize new table
    newtable = (pnooks_hash_table_t) kmalloc(sizeof(nooks_hash_table_t), GFP_KERNEL);
    if (newtable == NULL) {
        panic ("Could not create nooks hash table 2\n");
    }
    
    if (nooks_create_hash_table (newlookaside, newtable, size, 1) != 0) {
        panic ("Could not create nooks hash table 3\n");
    }
    
    newtable->range_table = nooks_create_range_query_list ();
    
    return newtable;
}

void nooks_ot_free_table (void *void_table) {
    pnooks_hash_table_t table = (pnooks_hash_table_t) void_table;
    nooks_free_range_query_list (table->range_table);
    nooks_destroy_hash_table (table);
    kfree (table->lookaside);
    kfree (table);
}

/**********************************************************
 *
 * nooks_ot_lookup_kern 
 * 
 * Lookup kernptr associated with a userptr. For a failed
 * translation, alloc a memory region of size bytes, and 
 * modify kernptr to point to this region.
 * 
 *********************************************************/

void 
nooks_ot_lookup_kern (void *table, 
                      void * userptr,
                      void ** kernptr,
                      int size)

{
    int res;
    if (kernptr) {
        if (userptr == 0) {
            *kernptr = 0;
            return;
        }
    
        res = nooks_find_in_hash_reverse (table, DUMMYTYP, userptr, kernptr);
        if (res == -ENOENT) {
            // Do the slow path lookup.
            nooks_range_lookup_kern (((pnooks_hash_table_t) table)->range_table,
                                     (unsigned long) userptr,
                                     (unsigned long *) kernptr);

            if (*kernptr == NULL) {
                if (size <= 0) {
                    uprintk ("Failed to translate pointer: kern %p user %p size %d, allocating 1 byte\n",
                             *kernptr, userptr, size);
                    size = 1;
                }

                //dump_stack ();
                /* A pointer was created in user mode but it was not in the
                 * object tracker.
                 */
                //printk ("NOTE: All allocations should be taking place via a wrapper, req %d.\n", size);

                // Create a new entry.
                // Should we zero it out?
                // CURRENTLY: We 0 it because allocations do not always take place
                // with a wrapper, so is user-mode code does kzalloc, then we
                // will allocate memory on the way in and execute this code,
                // and we will need it zero'd out as well.
                if ((size < 0 ) || (size > 50000))  {
                    panic ("Incorrect size in kernptr.\n");
                }
                *kernptr = kmalloc(size, GFP_ATOMIC); // Always 0 for simplicity

                if (*kernptr == NULL) {
                    panic ("Failed to allocate memory in %s\n", __func__);
                }
                /*  XXX ASIM KADAV reveresed kernel and user pointers for his
                 *  project. He has no idea how well it works but it seems to
                 *  work well on his sample of n experiments where n = 1;
                //uprintk ("nooks_ot_lookup_kern:  New mapping established: kern %p, user %p\n",
                  //       *kernptr, userptr);

                res = nooks_add_to_hash (table, DUMMYTYP, *kernptr, userptr);
                */
                //uprintk ("nooks_ot_lookup_kern:  New mapping established: kern %p, user %p\n",
                  //      userptr, *kernptr);    
                res = nooks_add_to_hash (table, DUMMYTYP, userptr, *kernptr);
                
                if (res == -ENOMEM) {
                    panic("nooks htab out of memory");
                }
            }
        }
    }
    //printk ("Verifying slabs..\n");
    //full_slab_verify(); 
    return;
}

/**********************************************************
 *
 * Current implementation:
 * Handles variable-length arrays and strings.
 * We simply allocate the memory requested and create
 * the mapping.  Add a range if necessary, delete any old
 * range.
 * 
 * Obsolete comments:
 * nooks_ot_alloc_arraymem_kern
 *
 * Allocate a new memory region with arrsize bytes; copy 
 * the contents of the old array from kernptr to the new
 * location; modify kernptr to point to the new region.
 *
 * NOTES: 
 * 1. Need size info to copy old array's contents to
 *    new region.
 * 2. Check sizes appropriately when copying.
 *
 *********************************************************/
void
nooks_ot_alloc_arraymem_kern (void *table,
                              void ** kernptr,
                              int new_number_of_elt,
                              int sizeof_each_elt,
                              int rangestore)
{
    int res;
    void *newarr = 0U;
    void *userptr = 0U;
    void ** temp;	
    // TODO Need to deal with this function
    // This check was here just to consider character arrays only but we're using it
    // more broadly still. :(  I hate this part of the code.
    //if (sizeof_each_elt != 1) {
    //    panic ("%s failure 4: sizeof_each_elt: %d\n", __func__, sizeof_each_elt);
    //}
    
    // Allocate a new array of the appropriate size.
    //
    //
    	

    //printk ("*********Asim Kadav forced a return. If this is used anywhere else \ 
    //	apart from array de-marshaling then the code is forced to return as a NO-OP.\n\n\n"); 
    return;

kernptr = temp;
    newarr = kmalloc(new_number_of_elt * sizeof_each_elt, GFP_ATOMIC);
    if (newarr == NULL) {
        // Hope we succeed a second time w/o triggering a soft lockup
        uprintk ("%s: Failure to allocate memory via GFP_ATOMIC, retrying via GFP_KERNEL.\n", __func__);
        uprintk ("%s: Soft lockups are possible ....\n", __func__);
        newarr = kmalloc(new_number_of_elt * sizeof_each_elt, GFP_KERNEL);
        if (newarr == NULL) {
            panic ("%s failure 1\n", __func__);
        }
    }

    // Remove the old association with *kernptr.
    res = nooks_del_from_hash (table, DUMMYTYP, *kernptr, &userptr);
    if (res == -ENOENT) {
        panic("%s failure 1a, entry %p. Check generated code\n", __func__, *kernptr);
    }     
    
    res = nooks_add_to_hash (table, DUMMYTYP, newarr, userptr);
    if (res == -ENOMEM) {
        panic("%s failure 2\n", __func__);
    }

    if (rangestore == 1) {
        if (rangestore != 0) {
            uprintk ("\n\n\n");
            uprintk ("===============================================\n");
            uprintk ("%s MAJOR WARNING, rangestore: %d\n", __func__, rangestore);
            uprintk ("===============================================\n");
            uprintk ("\n\n\n");
        }

        //uprintk ("nooks_ot_alloc_arraymem_kern Deleting range from list...\n");
        nooks_range_remove (((pnooks_hash_table_t) table)->range_table,
                            (unsigned long) *kernptr,
                            NOOKS_RANGE_REMOVE_IGNORE);

        //uprintk ("nooks_ot_alloc_arraymem_kern Adding range to list...\n");
        nooks_range_add (((pnooks_hash_table_t) table)->range_table,
                         (unsigned long) newarr,
                         (unsigned long) userptr,
                         new_number_of_elt * sizeof_each_elt);
    }

    *kernptr = newarr;
    //uprintk ("%s:  New mapping established: kern %p, user %p\n", __func__, *kernptr, userptr);
    return;

    /*
    void *newarr = 0U;
    void *userptr = 0U;
    int res;
    //int i;
    int old_number_of_elt = -1;
    int min_number_of_elt = -1;
    panic ("This function should be obsolete.  It does not make sense to allocate memory in user mode.");
    
    // Non arrays = 0
    // Only arrays store size != 0
    res = nooks_get_array_numelts (table, DUMMYTYP, *kernptr, &old_number_of_elt); // New function call
    if (res != 0) {
        panic ("nooks_get_array_numelts returned %d, so nooks_ot_alloc_arraymem_kern failed\n", res);
    }
    
    min_number_of_elt = (new_number_of_elt > old_number_of_elt) ? old_number_of_elt : new_number_of_elt;
    
    uprintk ("<kernel> nooks_ot_alloc_arraymem_kern:  old_number_of_elt: %d, new_number_of_elt: %d, sizeof_each_elt: %d, rangestore: %d\n", old_number_of_elt, new_number_of_elt, sizeof_each_elt, rangestore);
    
    if (new_number_of_elt < 0) {
        panic ("<kernel> ERROR new_number_of_elt < 0 [was: %d]!  Not doing anything!\n", new_number_of_elt);
    }
    
    if (old_number_of_elt != new_number_of_elt) {
        // Allocate a new array of the appropriate size.
        newarr = kmalloc(new_number_of_elt * sizeof_each_elt, GFP_KERNEL);
        
        // Copy only as many bytes as are actually valid.
        memcpy (newarr, *kernptr, min_number_of_elt * sizeof_each_elt);
        
        // Remove the old association with *kernptr.
        res = nooks_del_from_hash (table, DUMMYTYP, *kernptr, &userptr);
        if (res == -ENOENT) {
            panic("No entry for %p. This is impossible\n", *kernptr);
        }
        
        // Create a new association (newarr, userptr);
        res = nooks_add_to_hash (table, DUMMYTYP, newarr, userptr);
        if (res == -ENOMEM) {
            panic("nooks htab out of memory\n");
        }
        
        // *kernptr is base of old array
        // userptr is base of old userptr array
        
        // First, delete all the old entries.  We deleted the first
        // one, above.
        if (rangestore == 1) {
            uprintk ("nooks_ot_alloc_arraymem_kern Deleting range from list...\n");
            nooks_range_remove (((pnooks_hash_table_t) table)->range_table,
                                (unsigned long) *kernptr,
                                NOOKS_RANGE_REMOVE_IGNORE);
            //nooks_range_dump (table->range_table);
            
            uprintk ("nooks_ot_alloc_arraymem_kern Adding range to list...\n");
            nooks_range_add (((pnooks_hash_table_t) table)->range_table,
                             (unsigned long) newarr,
                             (unsigned long) userptr,
                             new_number_of_elt * sizeof_each_elt);
            //nooks_range_dump (table->range_table);
        }
        
        // New
        nooks_store_array_numelts (table, DUMMYTYP, *kernptr, new_number_of_elt);
        
        // Assign newarr to *kernptr;
        *kernptr = newarr;
        //uprintk ("nooks_ot_alloc_arraymem_kern:  New mapping established: kern %p, user %p\n", *kernptr, userptr);
    } else {
        // kernptr is unmodified
      return;
    }
    
    return;
    */
}




/**********************************************************
 *
 * nooks_ot_register_kernfn 
 *
 * Create an entry with the kernel function's address. Passed
 * as input to this function are the kernel function's address
 * and its function code, that is used to synchronize the entry
 * with its usermode counterpart.
 *
 * If an entry is found with the functioncode as key, then 
 * delete that entry, and create an entry with (kernfnaddr, data).
 * Else just create an entry with (kernfnaddr, fncode).
 *
 *********************************************************/
void 
nooks_ot_register_kernfn (void *table,
                          void * kernfnptr,
                          unsigned long fncode) 
{
  void *userfnptr;
  int res;


  //uprintk("Registering function pointers: Kernel Half of Protocol %lu %p\n", fncode, kernfnptr);

  if (kernfnptr == 0) {
      return;
  }
  
  // Search for an entry in the hash table with fncode as the
  // key. If entry exists, delete it, and fetch the data.
  res = nooks_del_from_hash (table, FUNCTIONTYP, (void *) fncode, &userfnptr);
  if (res == -ENOENT) {
    // No entry was found. Set userfnptr to fncode.
    userfnptr = (void *) fncode;
  } 
  // Create a new entry (kernfnptr, userfnptr);
  res = nooks_add_to_hash (table, FUNCTIONTYP, kernfnptr, userfnptr);
  if (res == -ENOMEM) {
    panic("nooks htab out of memory");
  }

  return;
}




/**********************************************************
 *
 * nooks_ot_storeoffset_kern
 *
 * Given a kernlptr and an offset, lookup the mapping for 
 * kernptr in the object tracker. If no mapping exists for 
 * kernptr, then do nothing. If a mapping exists (say it is
 * userptr), then do the following:
 * -> Search old (kernptr'+off, userptr+off) association.
 * -> If kernptr'+off != kernptr+off, then memcpy from
 *    kernptr'+off to kernptr+off.
 * -> Delete the (kernptr'+off, userptr+off) association.
 * -> Add the (kernptr+off, userptr+off) association.
 *
 *********************************************************/
void
nooks_ot_storeoffset_kern(void *table,
                          void * kernptr,
                          int offset, 
                          int size)
{
    void *userptr;
    void *kernptr_plus_offset;
    void *userptr_plus_offset;
    void *old_kernptr_plus_offset = (void *) INVALID_PTR;
    int res;

    if (kernptr == 0) {
        return;
    }


    // Check first if we already have an entry for kernptr + offset.
    kernptr_plus_offset = kernptr + offset;
    
    res = nooks_find_in_hash(table, 
                             DUMMYTYP,
                             kernptr_plus_offset,   
                             &userptr_plus_offset);
    
    // No entry exists already for kernptr + offset.
    if (res == -ENOENT) {
        // Check if we have an entry for kernptr.
        res = nooks_find_in_hash (table, DUMMYTYP, kernptr, &userptr);
        // If an entry exists, then create the offset entry.
        if (res != -ENOENT) {
            userptr_plus_offset = userptr + offset;
            
            // Search and del any old associations of userptr_plus_offset.     
            res = nooks_del_from_hash_reverse (table,
                                               DUMMYTYP,
                                               userptr_plus_offset,
                                               &old_kernptr_plus_offset);
            
            // If there was an old association, copy data from old location
            // to the new location.
            if ((res != -ENOENT) && 
                (kernptr_plus_offset != old_kernptr_plus_offset)) {
                memcpy(kernptr_plus_offset, old_kernptr_plus_offset, size);
            }
            
            res = nooks_add_to_hash (table, 
                                     DUMMYTYP, 
                                     kernptr_plus_offset,
                                     userptr_plus_offset);
            
            if (res == -ENOMEM) {
                panic("nooks htab is out of memory");
            }
        }
    }
    
    return;
}

int
nooks_ot_lookup_kern_no_create (void *table,
                                void * userptr,
                                void ** kernptr)
{
    int res = 0;

    // TODO Maybe check if userptr != NULL ?
    
    if (kernptr) {
        uprintk ("nooks_ot_lookup_kern_no_create:  userptr %p\n", userptr);
        res = nooks_find_in_hash_reverse (table, DUMMYTYP, userptr, kernptr);
        if (res == -ENOENT) {
            // Do the slow path lookup.
            nooks_range_lookup_kern (((pnooks_hash_table_t) table)->range_table,
                                     (unsigned long) userptr,
                                     (unsigned long *) kernptr);

            if (*kernptr == NULL) {
                //uprintk("<mischelp:nooks_ot_lookup_kern_no_create> no entry found.\n");
            } else {
                uprintk ("nooks_ot_lookup_kern_no_create:  kernptr found via range query: %p\n", *kernptr);
                res = 0;
            }
        }
    }

    return res;
}

int
nooks_ot_kern_memory_assoc (void *table,
                            void * userptr,
                            void * kernptr)
{
    int res = 0;

    if (userptr == NULL)    {
        printk ("NULL user pointer in nooks_ot_kern_memory_assoc.\n");
    }
    // TODO Maybe check if userptr != NULL ?

    if (kernptr) {
        res = nooks_add_to_hash (table, DUMMYTYP, kernptr, userptr);
        if (res == -ENOMEM) {
            panic("nooks htab out of memory");
        }
    }
    return res;
}

void 
nooks_ot_dump_histogram_kern (void *table)
{
    nooks_dump_hash_histogram(table);
    return;  
}

int nooks_ot_dump_table_size (void *table)
{
    pnooks_hash_entry_t entry = NULL;
    nooks_hash_enum_t context = {0};
    int count = 0;
    
    while (nooks_enumerate_in_hash(table,
                                   &context,
                                   &entry) == 0) {
        count++;
    }

    return count;
}

EXPORT_SYMBOL(nooks_ot_create_new_table);
EXPORT_SYMBOL(nooks_ot_free_table);
EXPORT_SYMBOL(nooks_ot_lookup_kern);
EXPORT_SYMBOL(nooks_ot_alloc_arraymem_kern);
EXPORT_SYMBOL(nooks_ot_register_kernfn);
EXPORT_SYMBOL(nooks_ot_storeoffset_kern);
EXPORT_SYMBOL(nooks_ot_lookup_kern_no_create);
EXPORT_SYMBOL(nooks_ot_kern_memory_assoc);
EXPORT_SYMBOL(nooks_dump_hash);
EXPORT_SYMBOL(nooks_dump_hash_histogram);

/* TODO: wrappers to free entries from the OT */
