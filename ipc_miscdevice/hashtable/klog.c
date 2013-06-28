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
#include <linux/spinlock.h>

static struct hashtable *klog_hash = 0x0;
static int klogid = 0;
static spinlock_t klog_lock;

struct hkey {
    const char *fnname; 
    int klogid;
};

struct hvalue {
    const char *fnname;
    int klogid;
    void *args;
    int   argslen;
    void *retval;
    int   retvallen;
};

struct hvalue * odft_check_klog_hash(const char * fnname, int klogid);
struct hvalue * odft_delete_klog_hash(const char * fnname, int klogid);

static unsigned int hash_from_key_fn (void *k) {
    struct hkey *key = (struct hkey *) k;
    //int csum = 0;
    //int i = 0;

    /*
    //printk ("Generating cksum for %s %p.\n", key->fnname, key->startaddr);
    while ((key->fnname[i]) && (key->fnname[i] !='\0'))  {
        csum += key->fnname[i];
        i++; 
    }
    */
    //printk ("csum is %d.\n", key->klogid);
    return key->klogid;
}

static int hash_keys_equal_fn (void *k1, void *k2) {
    struct hkey *key1 = (struct hkey *) k1;
    struct hkey *key2 = (struct hkey *) k2;

    printk ("Checking equivalence for %s(%d) and %s(%d)", key1->fnname, key1->klogid, key2->fnname,
                                                                    key2->klogid);
    if ((strcmp(key1->fnname,key2->fnname) == 0) || (key1->klogid == key2->klogid)) {
        return 1;
    }

    return 0;
}


int odft_init_klog_hashtable(void)  {
  int rc = 0;
  klog_hash = create_hashtable(16, hash_from_key_fn, hash_keys_equal_fn);
  if (klog_hash == NULL)
      return -1;
  spin_lock_init(&klog_lock);
  klogid = 0;
  //printk ("klog hashtable created.\n");
  return rc;
}


int odft_insert_klog_hash(const char * fnname, void * args, int argslen,
                                void *retval, int retvallen)  {
   struct hkey *key;
   unsigned long flags;
   struct hvalue *value;
   int rc = 0;
   key = (struct hkey *) kmalloc (sizeof (struct hkey), GFP_ATOMIC);
   value = (struct hvalue *) kmalloc (sizeof (struct hvalue), GFP_ATOMIC);

   // TODO add spinlock
   
   //printk ("attempting to insert key for %s.\n", fnname);
   
   value->fnname = kmalloc(strlen(fnname), GFP_ATOMIC);
   key->fnname = kmalloc(strlen(fnname), GFP_ATOMIC);
   strcpy(key->fnname, fnname);
   strcpy(value->fnname, fnname);
   //where is this freed?
   value->args=args;
   value->argslen = argslen;
   value->retval = retval;
   value->retvallen = retvallen;  
   value->klogid = klogid;
   
   spin_lock_irqsave(&klog_lock, flags);
   klogid++;
   key->klogid = klogid;
   value->klogid = klogid;
   rc =  hashtable_insert(klog_hash, key, value);
   spin_unlock_irqrestore(&klog_lock, flags); 
   printk ("Inserted %s %p %d %p %d with status %d.\n",
           fnname, args, argslen, retval, retvallen, rc);
   return rc;
}

int odft_truncate_klog_hashtable(void) {
  hashtable_destroy(klog_hash, 1);
  return odft_init_klog_hashtable ();
}

int odft_truncate_klog_hashtable_by_function(const char* fnname)   {
  int klogid = 1; 
  struct hvalue * hval = NULL;
  //printk ("Attempting to delete log.\n");

  hval = odft_check_klog_hash("asim", klogid);
  while (hval != NULL) {
    printk ("Deletig hash calues %s.\n", hval->fnname);
    odft_delete_klog_hash (hval->fnname, klogid);	
    klogid++;
    hval = odft_check_klog_hash("asim", klogid);
  }

  //printk ("kLog delete complete.\n");


  return 0;
}
 
struct hvalue * odft_check_klog_hash(const char * fnname, int klogid)    {
  
  struct hvalue *value = NULL;
  struct hkey key;

  key.fnname = fnname;
  key.klogid = klogid;
  value = (struct hvalue *) hashtable_search (klog_hash, &key);

  if (value == NULL) {
      //printk ("value not found in range hash .\n");
      return 0x0;
  }
  else 
  return value;

}

struct hvalue * odft_delete_klog_hash(const char * fnname, int klogid)    {
  
  struct hvalue *value = NULL;
  struct hkey key;

  key.fnname = fnname;
  key.klogid = klogid;
  value = (struct hvalue *) hashtable_remove (klog_hash, &key);

  if (value == NULL) {
      //printk ("value not found in range hash .\n");
      return 0x0;
  }
  else  {
    kfree(value->fnname);
    kfree(value);
  }
}


void odft_rollback_klog(void)    {
  struct hvalue * hval = NULL;
  int klogid = 1;

//  printk ("Attempting to replay log.\n");

  hval = odft_check_klog_hash("asim", klogid);
  while (hval != NULL) {
    printk ("Retrieved %s.\n", hval->fnname);

    if (strcmp(hval->fnname, "kmalloc")  == 0)    
        kfree(hval->retval);
/*    
    if (strcmp(hval->fnname, "spin_lock_irqsave"  == 0))
    	spin_unlock_irqrestore(hval->args, hval->retval);
*/

  if (strcmp(hval->fnname, "skb_alloc_buff")  == 0)    
        kfree(hval->retval);

    if (strcmp(hval->fnname, "mutex_lock") == 0)
    	mutex_unlock(hval->args);

    klogid++;
    hval = odft_check_klog_hash("asim", klogid);
  }

  //printk ("Log replay complete.\n");
}


void odft_free_locks(void)    {
	struct hvalue * hval = NULL;
	int klogid = 1;

 	//printk ("Freeing locks.\n");

	hval = odft_check_klog_hash("asim", klogid);
	while (hval != NULL) {
 	      printk ("Retrieved %s.\n", hval->fnname);

		if (strcmp(hval->fnname, "spin_lock_irqsave")  == 0) {
			printk ("Will now free %p. \n", hval->args);
			spin_unlock_irqrestore(hval->args, hval->retval);
		}

		if (strcmp(hval->fnname, "spin_lock_irq")  == 0) {
			printk ("raw: Will now free %p. \n", hval->args);
			__raw_spin_unlock (& ((spinlock_t *)hval->args)->raw_lock ); 
			//spin_unlock_irq((spinlock_t *) hval->args);
		}
		klogid++;
		hval = odft_check_klog_hash("asim", klogid);
	}

}
// loops forever if things go wrong
int  odft_hold_and_acquire_lock(spinlock_t *lock)	{

	struct hvalue * hval = NULL;
	int klogid = 1;

	//printk ("Checking locks.\n");

	if (klog_hash == NULL)
		return 1;

	hval = odft_check_klog_hash("asim", klogid);
	while (hval != NULL) {
		printk ("Retrieved %s.\n", hval->fnname);

		if (strcmp(hval->fnname, "spin_lock_irqsave")  == 0) {
			if (lock == hval->args)
				return 0;	
		}

		if (strcmp(hval->fnname, "spin_lock_irq")  == 0) {
			if (lock == hval->args)
			return 0;	
		}

		klogid++;
		hval = odft_check_klog_hash("asim", klogid);
	}
       return 1; 	
}

EXPORT_SYMBOL(odft_hold_and_acquire_lock);
EXPORT_SYMBOL(odft_delete_klog_hash); 
EXPORT_SYMBOL(odft_init_klog_hashtable);
EXPORT_SYMBOL(odft_insert_klog_hash);
EXPORT_SYMBOL(odft_rollback_klog);
EXPORT_SYMBOL(odft_check_klog_hash);
EXPORT_SYMBOL(odft_truncate_klog_hashtable);
EXPORT_SYMBOL(odft_free_locks);
