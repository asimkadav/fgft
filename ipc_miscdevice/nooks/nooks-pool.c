/*
 * nooks-pool.c -- lookaside lists for nooks
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


/**********************************************************
 *
 * nooks_mm_init_lookaside 
 *
 * initialize a lookaside list
 *
 *********************************************************/

void
nooks_mm_init_lookaside(pnooks_lookaside_list_t list,
			int highwater,
			int size,
			int flags)
{
  list->list = NULL;
  list->highwater = highwater;
  list->size = size;
  list->qlen = 0;
  list->flags = flags;
  spin_lock_init(&list->lock);
}

/**********************************************************
 *
 * nooks_mm_lookaside_alloc
 *
 * Alloc element off a lookaside list
 *
 *********************************************************/

void *
nooks_mm_lookaside_alloc(pnooks_lookaside_list_t list)
{
  unsigned long flags;
  pnooks_lookaside_elem_t elem;
	
  spin_lock_irqsave(&list->lock, flags);

	elem = list->list;
  if (elem != NULL) {
    list->list = elem->next;
    list->qlen--;
    spin_unlock_irqrestore(&list->lock, flags);
    return(elem);
  }
  spin_unlock_irqrestore(&list->lock, flags);

  return(kmalloc(list->size, GFP_ATOMIC));
}

/**********************************************************
 *
 * nooks_mm_lookaside_free
 *
 * Free an element to a lookaside list
 *
 *********************************************************/

void
nooks_mm_lookaside_free(pnooks_lookaside_list_t list, void * ptr)
{
  unsigned long flags;
  pnooks_lookaside_elem_t elem = (pnooks_lookaside_elem_t) ptr;
  spin_lock_irqsave(&list->lock, flags);

#if NOOKS_DEBUG2
  //
  // check for duplicates
  //
  {
    pnooks_lookaside_elem_t next = list->list;
    while (next != NULL) {
      if (next == ptr) {
          BUG();
      }
      next = next->next;
    }
  }
#endif
#define in_irq() (0)

  if (in_irq() || (list->qlen < list->highwater)) {
    list->qlen++;
    elem->next = list->list;
    list->list = elem;
    spin_unlock_irqrestore(&list->lock, flags);
    return;
  }
  spin_unlock_irqrestore(&list->lock, flags);

  return(kfree(elem));
}

void
nooks_mm_free_lookaside(pnooks_lookaside_list_t list)
{
   pnooks_lookaside_elem_t next = list->list;
   pnooks_lookaside_elem_t safe;
   while (next != NULL) {
     safe = next->next;
     kfree(next);
     next = safe;
   }
   list->list = NULL;
}
