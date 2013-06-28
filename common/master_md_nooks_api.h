#ifndef _NOOKS_OT_KERN_API_H_
#define _NOOKS_OT_KERN_API_H_

void *
nooks_ot_create_new_table (int size);

void
nooks_ot_free_table (void *table);

void 
nooks_ot_lookup_kern (void *table,
                      void * userptr, 
                      void ** kernptr, 
                      int size);

void 
nooks_ot_alloc_arraymem_kern (void *table,
                              void ** kernptr, 
                              int new_number_of_elt,
                              int sizeof_each_elt,
                              int rangestore);

void 
nooks_ot_register_kernfn (void *table,
                          void * kernfnptr,
                          unsigned long fncode); 

void
nooks_ot_storeoffset_kern(void *table,
                          void * kernptr,
                          int offset,
                          int size);

int
nooks_ot_lookup_kern_no_create (void *table,
                                void * userptr,
                                void ** kernptr);

int
nooks_ot_kern_memory_assoc (void *table,
                            void * userptr,
                            void * kernptr);

int
nooks_ot_dump_table_size (void *table);

void
nooks_ot_dump_histogram_kern (void *table);

#endif /* _NOOKS_OT_API_H_ */
