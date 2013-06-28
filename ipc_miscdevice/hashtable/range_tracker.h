#include <linux/module.h> 


int odft_init_range_hashtable(void);
int odft_insert_range_hash (const char *, void *, int);
int odft_check_range_hash (const char *, void *);


EXPORT_SYMBOL(int odft_init_range_hashtable(void));
EXPORT_SYMBOL(odft_insert_range_hash);
EXPORT_SYMBOL(odft_check_range_hash);
EXPORT_SYMBOL(odft_truncate_range_hashtable);
