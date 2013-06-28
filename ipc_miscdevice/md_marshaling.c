#include "../common/slave_master_ud_md_marshaling.h"
#include "common_h.h"
//#include "syscall_replacements.h"

#define PRINT(...) printk(__VA_ARGS__) /* Printing function */
//#define MJR_alloc(x) kmalloc(x, GFP_KERNEL)   /* Allocator function */
//#define FREE(x)  kfree(x)     /* Free function */
//#define ALLOC(x) vmalloc(x)   /* Allocator function */
//#define FREE(x)  vfree(x)     /* Free function */
#define TERMINATE() panic("Terminated!  md_marshaling")

// Used only in user-mode during symbolic fork execution
//#define SYMEXEC_FILL()
//#define SYMEXEC_FETCH()
//#define SYMEXEC_FETCH_PTR()

extern pnooks_hash_table_t g_nooks_table;
#define NOOKS_OT_LOOKUP(pre_translate, post_translate, size) \
    nooks_ot_lookup_kern (g_nooks_table, pre_translate, post_translate, size)

#include "../common/slave_master_ud_md_marshaling.c"



EXPORT_SYMBOL(fill_marshbuf);
EXPORT_SYMBOL(fill_marshbuf_ptr);
EXPORT_SYMBOL(fetch_marshbuf);
EXPORT_SYMBOL(fetch_marshbuf_ptr);

