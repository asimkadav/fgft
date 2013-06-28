#ifndef _LINUX_NOOKS_HASH_H
#define _LINUX_NOOKS_HASH_H

#include "nooks-i.h"


typedef struct nooks_hash_entry {
  struct list_head next;
  struct list_head next_back;
  void * key;
  nooks_resource_type_t entry_type;
  void * data;
  int array_numelts; // MJR
} nooks_hash_entry_t, *pnooks_hash_entry_t;

struct nooks_range_query_list; // MJR

typedef struct nooks_hash_table {
  unsigned int size;
  unsigned int mask;
  unsigned int shift;
  int dual;
  struct list_head * table;
    spinlock_t lock;
  pnooks_lookaside_list_t lookaside;
  struct nooks_range_query_list *range_table; // MJR
} nooks_hash_table_t, *pnooks_hash_table_t;


//
// Stolen from fs/buffer.c
//

extern unsigned int nooks_hash_shift;
extern unsigned int nooks_hash_mask;

#define nooks_hashfn(_ptr_, _shift_)	\
	 (((_ptr_)<<(_shift_ - 6)) ^ ((_ptr_) >> 13) ^ \
	  ((_ptr_) << (_shift_ - 12)))
#define nooks_hash(_table_, _ptr_) & (table->table[(nooks_hashfn(((unsigned long) _ptr_), (_table_)->shift) & (_table_)->mask)])





int nooks_create_hash_table(pnooks_lookaside_list_t list,
			    pnooks_hash_table_t table, 
			    int size,
			    int dual);

int nooks_destroy_hash_table(pnooks_hash_table_t table);


int
nooks_add_to_hash(pnooks_hash_table_t table, 
		  nooks_resource_type_t entry_type,
		  void * key, 
		  void * data);

int
nooks_del_from_hash(pnooks_hash_table_t table, 
		    nooks_resource_type_t entry_type,
		    void * key, 
		    void ** data);

int
nooks_del_from_hash_reverse(pnooks_hash_table_t table, 
			    nooks_resource_type_t entry_type,
			    void * data, 
			    void ** key);

int 
nooks_find_in_hash(pnooks_hash_table_t table, 
		   nooks_resource_type_t entry_type,
		   void * key, 
		   void ** data);

int
nooks_find_in_hash_reverse(pnooks_hash_table_t table, 
			   nooks_resource_type_t entry_type,
			   void * data, 
			   void ** key);


/* //  MJR
int 
nooks_replace_in_hash(pnooks_hash_table_t table, 
		      nooks_resource_type_t entry_type,
		      void * key, 
		      void * data,
		      void ** old_data);
*/

// MJR -------->
int
nooks_get_array_numelts (pnooks_hash_table_t table,
                         nooks_resource_type_t entry_type,
                         void *key, // kernptr
                         int *array_numelts);

int
nooks_store_array_numelts (pnooks_hash_table_t table,
                           nooks_resource_type_t entry_type,
                           void *key, // kernptr
                           int array_numelts);

int
nooks_get_array_numelts_reverse (pnooks_hash_table_t table,
                                 nooks_resource_type_t entry_type,
                                 void *data, // userptr
                                 int *array_numelts);

int
nooks_store_array_numelts_reverse (pnooks_hash_table_t table,
                                   nooks_resource_type_t entry_type,
                                   void *data, // userptr
                                   int array_numelts);
// <---------- MJR

typedef struct nooks_hash_enum {
  struct list_head * next;
  int index;
} nooks_hash_enum_t, *pnooks_hash_enum_t;

int
nooks_enumerate_in_hash(pnooks_hash_table_t table,
			pnooks_hash_enum_t context,
			pnooks_hash_entry_t * entry);

// MJR
//void
//nooks_delete_entry_from_hash(pnooks_hash_table_t table,
//			     pnooks_hash_entry_t entry);

void
nooks_dump_hash(pnooks_hash_table_t table);

void
nooks_dump_hash_histogram(pnooks_hash_table_t table);

// MJR
//int
//nooks_hash_empty(pnooks_hash_table_t table);

// MJR added this:
void
nooks_mm_init_lookaside(pnooks_lookaside_list_t list,
                        int highwater,
                        int size,
                        int flags);

void nooks_verify_integrity(pnooks_hash_table_t table);

#endif // NOOKS_HASH_H

