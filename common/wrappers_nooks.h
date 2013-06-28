#ifndef _NOOKS_OT_USER_API_H_
#define _NOOKS_OT_USER_API_H_

void 
nooks_ot_lookup_user (void *kernptr, void **userptr, int size);

void 
nooks_ot_alloc_arraymem_user (void ** userptr,
                              int new_number_of_elt,
                              int sizeof_each_elt,
                              int rangestore);

void
nooks_ot_store_array_numelts_reverse (void *userptr,
                                      int new_number_of_elt);

void 
nooks_ot_register_userfn (void *userfnptr,
                          unsigned long fncode);

void
nooks_ot_storeoffset_user(void *userptr,
                          int offset,
                          int size);

void
nooks_ot_kern_memory_assoc (void *userptr,
                            void *kernptr);

void *
nooks_ot_establish_iomem (unsigned long base,
                       unsigned long length,
                       void *kernptr);

void
nooks_ot_add_range (unsigned long kern_ptr_base,
                    unsigned long user_ptr_base,
                    int number_of_elt,
                    int sizeof_each_elt);

void
nooks_ot_free_range (unsigned long kern_ptr_base,
                     unsigned long user_ptr_base);

void *
nooks_debug_translate_u2k (void *userptr);

#endif /* _NOOKS_OT_USER_API_H_ */
