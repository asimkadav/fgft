/*
 * nooks-hash.c -- open hash table for nooks
 *
 * Copyright (C) 2001 Mike Swift
 *
 * The source code in this file can be freely used, adapted,
 * and redistributed in source or binary form, so long as an
 * acknowledgment appears in derived source files.  
 * No warranty is attached;
 * we cannot take responsibility for errors or fitness for use.
 *
 */

#include "nooks-i.h"
#include "../../common/uprintk.h"

/* Protos for functions in nooks-pool.c */
void *nooks_mm_lookaside_alloc(pnooks_lookaside_list_t list);
void nooks_mm_lookaside_free(pnooks_lookaside_list_t list, void *ptr);


int nooks_create_hash_table(pnooks_lookaside_list_t list, 
			    pnooks_hash_table_t table, 
			    int size,
			    int dual)
{
  int i;
  spin_lock_init(&table->lock);
  table->size = size;
  table->lookaside = list;


  table->mask = size - 1;
  table->size = size;
  table->shift = 0;
  table->dual = dual;
  
  while ((size >>= 1) != 0) {
    table->shift ++;
  }

  //
  // Double the space for dual tables
  //

  if (dual) {
    size = 2 * table->size;
  } else {
    size = table->size;
  }
 
  table->table = (struct list_head *) 
      kmalloc(size * sizeof(struct list_head), GFP_KERNEL);
		
  if (table->table == NULL) {
    return(-ENOMEM);
  }
  for (i = 0; i < size; i++) {
    INIT_LIST_HEAD(&table->table[i]);
  }
  
  return(0);
}

//EXPORT_SYMBOL (nooks_create_hash_table);

int nooks_destroy_hash_table(pnooks_hash_table_t table)
{
  int i;
  struct list_head *l;
  pnooks_hash_entry_t entry;

  for (i = 0; i < table->size; i++) {
    l = &table->table[i];
    while (!list_empty(l)) {
      entry = list_entry(l->next,nooks_hash_entry_t, next);
      list_del(&entry->next);
      nooks_mm_lookaside_free(table->lookaside, entry);
    }
  }
  kfree(table->table);
  return 0; // MJR
}

//EXPORT_SYMBOL (nooks_destroy_hash_table);

int
nooks_add_to_hash(pnooks_hash_table_t table, 
		  nooks_resource_type_t entry_type,
		  void * key, 
		  void * data)
{
  struct list_head *l;
  pnooks_hash_entry_t entry;
  unsigned long flags;

  entry = (pnooks_hash_entry_t) nooks_mm_lookaside_alloc(table->lookaside);
  if (entry == NULL) {
    return(-ENOMEM);
  }
  entry->key = key;
  entry->data = data;
  entry->entry_type = entry_type;
  entry->array_numelts = 1; // New.  All pointers have one element by default
  l = nooks_hash(table, key);
  spin_lock_irqsave(&table->lock, flags);
  list_add(&entry->next, l);
  if (table->dual) {
    l = nooks_hash(table, data) + table->size;
    list_add(&entry->next_back, l);
  }
    
#ifdef NOOKS_KEEP_STATS
  table->hash_size++;
  table->hash_adds++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(0);  
}

//EXPORT_SYMBOL (nooks_add_to_hash);

int
nooks_del_from_hash(pnooks_hash_table_t table, 
		    nooks_resource_type_t entry_type,
		    void * key, 
		    void ** data)
{
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  l = nooks_hash(table, key);
  list_for_each(next, l) {
    entry = list_entry(next, nooks_hash_entry_t, next);
    if ((entry->key == key) && (entry->entry_type == entry_type)) {
      if (data != NULL) {
	*data = entry->data;
      }
      list_del(&entry->next);
      if (table->dual) {
	list_del(&entry->next_back);
      }
#ifdef NOOKS_KEEP_STATS
      table->hash_hits++;
      table->hash_size--;
#endif // NOOKS_KEEP_STATS
      spin_unlock_irqrestore(&table->lock, flags);
      nooks_mm_lookaside_free(table->lookaside, entry);
      return(0);
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

//EXPORT_SYMBOL(nooks_del_from_hash);

int 
nooks_find_in_hash(pnooks_hash_table_t table, 
		   nooks_resource_type_t entry_type,
		   void * key, 
		   void ** data)
{
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);
  l = nooks_hash(table, key);
  list_for_each(next, l) {
    entry = list_entry(next, nooks_hash_entry_t, next);
    if ((entry->key == key) && (entry->entry_type == entry_type)) {
      if (data != NULL) {
	*data = entry->data;
      }
#ifdef NOOKS_KEEP_STATS
      table->hash_hits++;
#endif // NOOKS_KEEP_STATS
      spin_unlock_irqrestore(&table->lock, flags);
      return(0);
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

//EXPORT_SYMBOL (nooks_find_in_hash);

int
nooks_del_from_hash_reverse(pnooks_hash_table_t table, 
			    nooks_resource_type_t entry_type,
			    void * data, 
			    void ** key)
{
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;
  int i;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  //
  // If we have a dual table, look in the reverse path
  //
  if (table->dual) {
    l = nooks_hash(table, data) + table->size;
    list_for_each(next, l) {
      entry = list_entry(next, nooks_hash_entry_t, next_back);
      if ((entry->data == data) && (entry->entry_type == entry_type)) {
	if (key != NULL) {
	  *key = entry->key;
	}
	list_del(&entry->next);
	list_del(&entry->next_back);
#ifdef NOOKS_KEEP_STATS
	table->hash_hits++;
	table->hash_size--;
#endif // NOOKS_KEEP_STATS
	spin_unlock_irqrestore(&table->lock, flags);
	nooks_mm_lookaside_free(table->lookaside, entry);
	return(0);
      }
    }
  } else {
    //
    // Slowly search all entries
    //
    for (i = 0; i < table->size; i++) {
      l = &table->table[i];
      list_for_each(next, l) {
	entry = list_entry(next, nooks_hash_entry_t, next);
	if ((entry->data == data) && (entry->entry_type == entry_type)) {
	  if (key != NULL) {
	    *key = entry->key;
	  }
	  list_del(&entry->next);
#ifdef NOOKS_KEEP_STATS
	  table->hash_hits++;
	  table->hash_size--;
#endif // NOOKS_KEEP_STATS
          spin_unlock_irqrestore(&table->lock, flags);
	  nooks_mm_lookaside_free(table->lookaside, entry);
	  return(0);
	}
      }
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
	spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

//EXPORT_SYMBOL (nooks_del_from_hash_reverse);

int
nooks_find_in_hash_reverse(pnooks_hash_table_t table, 
			   nooks_resource_type_t entry_type,
			   void * data, 
			   void ** key)
{
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;
  int i;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  //
  // If we have a dual table, look in the reverse half
  //
  if (table->dual) {
    l = nooks_hash(table, data) + table->size;
    list_for_each(next, l) {
      entry = list_entry(next, nooks_hash_entry_t, next_back);
      if ((entry->data == data) && (entry->entry_type == entry_type)) {
	if (key != NULL) {
	  *key = entry->key;
	}
#ifdef NOOKS_KEEP_STATS
	table->hash_hits++;
#endif // NOOKS_KEEP_STATS
	spin_unlock_irqrestore(&table->lock, flags);
	return(0);
      }
    }
  } else {
    //
    // Look in the forward path by checking all entries
    //
    for (i = 0; i < table->size; i++) {
      l = &table->table[i];
      list_for_each(next, l) {
	entry = list_entry(next, nooks_hash_entry_t, next);
	if ((entry->data == data) && (entry->entry_type == entry_type)) {
	  if (key != NULL) {
	    *key = entry->key;
	  }
#ifdef NOOKS_KEEP_STATS
	  table->hash_hits++;
#endif // NOOKS_KEEP_STATS
          spin_unlock_irqrestore(&table->lock, flags);
	  return(0);
	}
      }
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

//EXPORT_SYMBOL(nooks_find_in_hash_reverse);

///////////////////////////////////////////////////////////////////////////////
// MJR
// Copy/paste/tweak of nooks_find_in_hash
// Finds the number of array elements associated with this entry.
// All entries will return "0" unless someone calls the
// nooks_store_array_numelts function first.
///////////////////////////////////////////////////////////////////////////////
int
nooks_get_array_numelts (pnooks_hash_table_t table,
                         nooks_resource_type_t entry_type,
                         void *key, // kernptr
                         int *array_numelts) {
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;

  unsigned long flags;

  *array_numelts = -1;
  
  spin_lock_irqsave(&table->lock, flags);
  l = nooks_hash(table, key);
  list_for_each(next, l) {
    entry = list_entry(next, nooks_hash_entry_t, next);
    if ((entry->key == key) && (entry->entry_type == entry_type)) {
      // MJR
      // size should not be NULL
      *array_numelts = entry->array_numelts;
      uprintk ("nooks_get_array_numelts: found size %d for kernel ptr %p\n", *array_numelts, key);
      
#ifdef NOOKS_KEEP_STATS
      table->hash_hits++;
#endif // NOOKS_KEEP_STATS
      spin_unlock_irqrestore(&table->lock, flags);
      return(0);
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

///////////////////////////////////////////////////////////////////////////////
// This function is a copy/paste/tweak of nooks_get_array_numelts,
// which, in turn, is a copy/paste/tweak of nooks_find_in_hash.
// Store the number of elements associated with this array.
///////////////////////////////////////////////////////////////////////////////
int
nooks_store_array_numelts (pnooks_hash_table_t table,
                           nooks_resource_type_t entry_type,
                           void *key,
                           int array_numelts) {
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);
  l = nooks_hash(table, key);
  list_for_each(next, l) {
    entry = list_entry(next, nooks_hash_entry_t, next);
    if ((entry->key == key) && (entry->entry_type == entry_type)) {
      // MJR
      entry->array_numelts = array_numelts;
      uprintk ("nooks_store_array_numelts: storing size %d for kern ptr %p\n", array_numelts, key);
      
#ifdef NOOKS_KEEP_STATS
      table->hash_hits++;
#endif // NOOKS_KEEP_STATS
      spin_unlock_irqrestore(&table->lock, flags);
      return(0);
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}


///////////////////////////////////////////////////////////////////////////////
// MJR copy/paste/tweak of nooks_find_in_hash_reverse
// Gets the number of elements given a userptr.
// Called from user space, not from the kernel.
///////////////////////////////////////////////////////////////////////////////
int
nooks_get_array_numelts_reverse (pnooks_hash_table_t table,
                                 nooks_resource_type_t entry_type,
                                 void *data, // userptr
                                 int *array_numelts) {
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;
  int i;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  *array_numelts = -1;

  //
  // If we have a dual table, look in the reverse half
  //
  if (table->dual) {
    l = nooks_hash(table, data) + table->size;
    list_for_each(next, l) {
      entry = list_entry(next, nooks_hash_entry_t, next_back);
      if ((entry->data == data) && (entry->entry_type == entry_type)) {
        // MJR
        // num_elts should not be NULL
        *array_numelts = entry->array_numelts;
        uprintk ("nooks_get_array_numelts_reverse: found size %d for %p\n", *array_numelts, data);
	
#ifdef NOOKS_KEEP_STATS
	table->hash_hits++;
#endif // NOOKS_KEEP_STATS
	spin_unlock_irqrestore(&table->lock, flags);
	return(0);
      }
    }
  } else {
    //
    // Look in the forward path by checking all entries
    //
    for (i = 0; i < table->size; i++) {
      l = &table->table[i];
      list_for_each(next, l) {
	entry = list_entry(next, nooks_hash_entry_t, next);
	if ((entry->data == data) && (entry->entry_type == entry_type)) {
          // MJR
          // num_elts should not be NULL
          *array_numelts = entry->array_numelts;
          uprintk ("nooks_get_array_numelts_reverse: found size %d for %p\n", *array_numelts, data);

#ifdef NOOKS_KEEP_STATS
	  table->hash_hits++;
#endif // NOOKS_KEEP_STATS
          spin_unlock_irqrestore(&table->lock, flags);
	  return(0);
	}
      }
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

///////////////////////////////////////////////////////////////////////////////
// MJR copy/paste/tweak of nooks_find_in_hash_reverse
// Stores the number of elements given a userptr.
// Called from user space, not from the kernel.
///////////////////////////////////////////////////////////////////////////////
int
nooks_store_array_numelts_reverse (pnooks_hash_table_t table,
                                   nooks_resource_type_t entry_type,
                                   void *data, // userptr
                                   int array_numelts) {
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;
  int i;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  //
  // If we have a dual table, look in the reverse half
  //
  if (table->dual) {
    l = nooks_hash(table, data) + table->size;
    list_for_each(next, l) {
      entry = list_entry(next, nooks_hash_entry_t, next_back);
      if ((entry->data == data) && (entry->entry_type == entry_type)) {
        // MJR
        entry->array_numelts = array_numelts;
        uprintk ("nooks_store_array_numelts_reverse: storing size %d for user ptr %p\n", array_numelts, data);
	
#ifdef NOOKS_KEEP_STATS
	table->hash_hits++;
#endif // NOOKS_KEEP_STATS
	spin_unlock_irqrestore(&table->lock, flags);
	return(0);
      }
    }
  } else {
    //
    // Look in the forward path by checking all entries
    //
    for (i = 0; i < table->size; i++) {
      l = &table->table[i];
      list_for_each(next, l) {
	entry = list_entry(next, nooks_hash_entry_t, next);
	if ((entry->data == data) && (entry->entry_type == entry_type)) {
          // MJR
          entry->array_numelts = array_numelts;
          uprintk ("nooks_store_array_numelts: storing size %d for user ptr %p\n", array_numelts, data);

#ifdef NOOKS_KEEP_STATS
	  table->hash_hits++;
#endif // NOOKS_KEEP_STATS
          spin_unlock_irqrestore(&table->lock, flags);
	  return(0);
	}
      }
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

/*
int 
nooks_replace_in_hash(pnooks_hash_table_t table, 
		      nooks_resource_type_t entry_type,
		      void * key, 
		      void * data,
		      void ** old_data)
{
  struct list_head *l;
  struct list_head * next;
  pnooks_hash_entry_t entry;

  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  l = nooks_hash(table, key);
  list_for_each(next, l) {
    entry = list_entry(next, nooks_hash_entry_t, next);
    if ((entry->key == key) && (entry->entry_type == entry_type)) {
      if (old_data != NULL) {
	*old_data = entry->data;
	entry->data = data;
      }
#ifdef NOOKS_KEEP_STATS
      table->hash_hits++;
#endif // NOOKS_KEEP_STATS
      spin_unlock_irqrestore(&table->lock, flags);
      return(0);
    }
  }
#ifdef NOOKS_KEEP_STATS
  table->hash_misses++;
#endif // NOOKS_KEEP_STATS
  spin_unlock_irqrestore(&table->lock, flags);
  return(-ENOENT);
}

//EXPORT_SYMBOL(nooks_replace_in_hash);
*/

int
nooks_enumerate_in_hash(pnooks_hash_table_t table,
			pnooks_hash_enum_t context,
			pnooks_hash_entry_t * entry)
{
  int index = 0;
  struct list_head * list;
  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  //
  // Find the entry to return
  //
  index = context->index;
  if (context->next == NULL) {
    for (list = &table->table[index]; 
	 ((index < table->size) && (list_empty(list))); 
	  index++, list = &table->table[index])
      ;

    if (index == table->size) {
	spin_unlock_irqrestore(&table->lock, flags);
        return(-ENOENT);
    }
    
    *entry = list_entry( list->next, nooks_hash_entry_t, next);
  } else {
    *entry = list_entry(context->next, nooks_hash_entry_t, next);
    list = &table->table[context->index];
  }
  
  if ((*entry)->next.next != list) {
    context->index = index;
    context->next = (*entry)->next.next;
    if ((context->next == NULL)  || (context->next->next == NULL) ||
	(context->next->prev == NULL)) {
      BUG();
    }
  } else {
    context->index = index + 1;
    context->next = NULL;
  }
  
  spin_unlock_irqrestore(&table->lock, flags);
  return(0);
}

//EXPORT_SYMBOL(nooks_enumerate_in_hash);

/*

void 
nooks_delete_entry_from_hash(pnooks_hash_table_t table,			     
			     pnooks_hash_entry_t entry)
{
  unsigned long flags;
  spin_lock_irqsave(&table->lock, flags);

  list_del(&entry->next);
  if (table->dual) {
    list_del(&entry->next_back);
  }
  nooks_mm_lookaside_free(table->lookaside, entry);
  spin_unlock_irqrestore(&table->lock, flags);
}

//EXPORT_SYMBOL (nooks_delete_entry_from_hash);
*/


void
nooks_dump_hash(pnooks_hash_table_t table)
{
  nooks_hash_enum_t context = {0};
  pnooks_hash_entry_t entry = NULL;
  int i = 0;

  //
  // Traverse the active skbs and free them now
  //

  while (nooks_enumerate_in_hash(table,
				 &context,
				 &entry) == 0) {
    uprintk("%d: type %d value %p data %p\n",
	   i, entry->entry_type, entry->key, entry->data);
    i++;
  }
    
}

//EXPORT_SYMBOL (nooks_dump_hash);

void
nooks_dump_hash_histogram(pnooks_hash_table_t table)
{
  int histogram[10];
  nooks_hash_enum_t context = {0};
  pnooks_hash_entry_t entry = NULL;
  int i = 0;

  memset(histogram, 0, sizeof(histogram));
  //
  // Traverse the active skbs and free them now
  //

  while (nooks_enumerate_in_hash(table,
				 &context,
				 &entry) == 0) {
    if (entry->entry_type < 10) {
      histogram[entry->entry_type]++;
    } else {
      histogram[0]++;
    }
  }
  for (i = 0; i < 10; i++) {
    uprintk("type %d: %d\n",i,histogram[i]);
  }
    
}

//EXPORT_SYMBOL (nooks_dump_hash_histogram);

/*
int
nooks_hash_empty(pnooks_hash_table_t table)
{
  struct list_head *l;
  int i;
  unsigned long flags;
  int empty = 1;

  spin_lock_irqsave(&table->lock, flags);

  for (i = 0; i < table->size; i++) {
    l = &table->table[i];
    if (!list_empty(l)) {
      empty = 0;
      break;
    }
  }
  spin_unlock_irqrestore(&table->lock, flags);
  return(empty);

}
*/

//EXPORT_SYMBOL (nooks_hash_empty);

void nooks_verify_integrity(pnooks_hash_table_t table)
{
  pnooks_hash_entry_t entry;
  nooks_hash_enum_t context = {0};

  while (nooks_enumerate_in_hash(table,
				 &context,
				 &entry) == 0) {
      //printk ("entry: %p\r\n", entry);
      if (entry == NULL) {
          panic ("Problem detected:  NULL entry in hash table\r\n");
      }
  }
}
