#ifndef _LINUX_NOOKS_I_H
#define _LINUX_NOOKS_I_H

#define NOOKS_DEBUG2 0

#include <linux/types.h>
#include <linux/slab.h>
#include <linux/module.h>
#define NOOKS_ERR printk

#include "list.h"
#include "errno.h"

typedef enum nooks_resource_type {
  fasync_e = 1,
  waitqueue_e,
} nooks_resource_type_t, *pnooks_resource_type_t;

typedef struct nooks_lookaside_elem {
  struct nooks_lookaside_elem * next;
} nooks_lookaside_elem_t, *pnooks_lookaside_elem_t;

typedef struct nooks_lookaside_list {
  pnooks_lookaside_elem_t list;
  int qlen;
  spinlock_t lock;
  int highwater;
  size_t size;
  int flags;
} nooks_lookaside_list_t, *pnooks_lookaside_list_t;

#include "nooks-hash.h"

typedef struct nooks_resource_list {
  nooks_hash_table_t hash;
} nooks_resource_list_t, *pnooks_resource_list_t;
 
#endif /* _LINUX_NOOKS_I_H */
