// Additional subroutines for range hashing
//
//
// Keys are function names, values are address, range pairs.
//

#include "hashtable.h"
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/module.h>

static struct hashtable *range_hash = 0x0;

struct hkey {
    const char *fnname; 
    void *startaddr;
    int sizeof_struct;
};

// protection 1 = read
// 2 = write  

struct hvalue {
    int protect ;
};

//#define odft_hash_debug


static unsigned int hash_from_key_fn (void *k) {
    struct hkey *key = (struct hkey *) k;
    int csum = 0;
    int i = 0;
#ifdef odft_hash_debug
    printk ("Generating cksum for %s %p.\n", key->fnname, key->startaddr);
#endif
    while ((key->fnname[i]) && (key->fnname[i] !='\0'))  {
        csum += key->fnname[i];
        i++; 
    }

    //printk ("csum is %d.\n", csum);
    return (unsigned int) csum;

}

static int hash_keys_equal_fn (void *k1, void *k2) {
    struct hkey *key1 = (struct hkey *) k1;
    struct hkey *key2 = (struct hkey *) k2;
    //printk ("Checking equivalence for %p(%d) %p.\n", key1->startaddr,key1->sizeof_struct, key2->startaddr);
    if ((strcmp(key1->fnname,key2->fnname) == 0) && 
            (key1->startaddr <= key2->startaddr) && 
            (key1->startaddr + key1->sizeof_struct > key2->startaddr)) {
#ifdef odft_hash_debug 	    
  	printk ("Match %p %p\n", key1->startaddr, key2->startaddr);
#endif	
  	return 1;
    }

    //printk ("No match..\n"); 	
    return 0;
}


int odft_init_range_hashtable(void)  {
  int rc = 0;
  range_hash = create_hashtable(16, hash_from_key_fn, hash_keys_equal_fn);
  if (range_hash == NULL)
      return -1;
  //printk ("Rangehash hashtable created.\n");
  return rc;
}


int odft_insert_range_hash(const char * fnname, void * startaddr, int size, int protect)  {
   struct hkey *key;
   struct hvalue *value;

   key = (struct hkey *) kmalloc (sizeof (struct hkey), GFP_ATOMIC);
   value = (struct hvalue *) kmalloc (sizeof (struct hvalue), GFP_ATOMIC);
   key->fnname = fnname;
   key->startaddr = startaddr;
   key->sizeof_struct = size;
   value->protect = protect;

   //printk ("Inserting %p, size:%d protect:%d  pair.\n", key->startaddr, key->sizeof_struct, protect);
   return hashtable_insert(range_hash, key, value);
}

int odft_truncate_range_hashtable(void) {
  hashtable_destroy(range_hash, 1);
  //printk ("Range hash truncated.\n"); 
  return odft_init_range_hashtable (); 
}

int odft_truncate_range_hashtable_by_function(const char* fnname)   {

    return 0;
}
 
int odft_check_range_hash(const char * fnname, void * startaddr)    {
  
  struct hvalue *value;
  struct hkey key;

  key.fnname = fnname;
  key.startaddr = startaddr;
  key.sizeof_struct = 0;
  value = (struct hvalue *) hashtable_search (range_hash, &key);

  if (value == NULL) {
      //printk ("value not found in range hash .\n");
      return -1;
  }
  else 
      return value->protect;

}

int odft_delete_range_hash(const char * fnname, void * startaddr)    {
 struct hvalue *value;
 struct hkey key;

 key.fnname = fnname;
 key.startaddr = startaddr;
 key.sizeof_struct = 0;

 value = (struct hvalue *) hashtable_remove (range_hash, &key);

 if (value != NULL) {
    kfree(value);
 }
}

int odft_delete_range_hash_byname(const char * fnname)    {
 struct hvalue *value=NULL;
 struct hkey key;

 key.fnname = fnname;
 key.startaddr = 0;
 key.sizeof_struct = 0;

 value = (struct hvalue *) hashtable_remove_byhash (range_hash, &key);
 while (value != NULL)	{
	//printk ("got %d.\n", value->present); 
	kfree(value);	
 	value = (struct hvalue *) hashtable_remove_byhash (range_hash, &key);
 }
}




EXPORT_SYMBOL(odft_delete_range_hash_byname);
EXPORT_SYMBOL(odft_delete_range_hash); 
EXPORT_SYMBOL(odft_init_range_hashtable);
EXPORT_SYMBOL(odft_insert_range_hash);
EXPORT_SYMBOL(odft_check_range_hash);
EXPORT_SYMBOL(odft_truncate_range_hashtable);
