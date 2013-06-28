#include "wrappers_sfi.h"
#define ODFT_ENABLE_SFI_CHECKS

#define FI_PRINT_DEBUG



#ifdef ODFT_INJECT_FAULTS
void odft_reset_fi_counts () {
  stack_corrupted = 0;
  stack_really_corrupted = 0;
  pointer_dereferenced = 0;
}
#endif

void logStackFrame(char *func ) {
#ifdef ODFT_INJECT_FAULTS
  if (odft_stack_to_corrupt() > 0)
  	stack_corrupted ++; 

  if (odft_stack_to_really_corrupt() > 0)
	stack_really_corrupted ++;
 #endif
  return;
}

int logStackVar(char *var ) {
#ifdef ODFT_INJECT_FAULTS
	if ((odft_stack_to_corrupt() > 0 )  &&
		(odft_stack_to_corrupt() == stack_corrupted))  {
		*var = NULL;	
		printk ("Stack variables set to null.\n");
	}

	if ((odft_stack_to_really_corrupt() > 0 )  &&
		(odft_stack_to_really_corrupt() == stack_really_corrupted))  {
		memcpy (*var, "ASIM", 64); 
		printk ("Stack really corrupted.\n");
	}
	return;
#endif
	//not used for now...
}

/* This is used to log allocators but wwe also use it to log synch calls. Most of the
 * calls in drivers are to alloc and sync functions. SO causing a domain switch seems 
 * too much work.
 */

int logAlloc(void *addr , int size , char *fn, char *file, char *allocfn,  int line ) {

    if (strcmp (allocfn, "mutex_lock" == 0))	{
	odft_insert_klog_hash(allocfn, addr, sizeof(void *),
				addr, sizeof(void *)); 	
    }

    else	{		

	    // Insert in the range hash.
	    odft_insert_range_hash (fn, addr, size);

	    // Insert in the klog hash.
	    odft_insert_klog_hash(allocfn, size, sizeof(size_t),
			    addr, sizeof(void*));
    }
}

void logFree(void *addr , char *fn, char *file , char *allocfn, int line )   {
 // Delete from range hash 
 odft_delete_range_hash (fn, addr);

 // Remove from the klog hash
 // Search and delete the corresponsing alloc call
 return;
}

int logWrite(void *addr , char *what, char *where, char *file , int line ) {
    // In case of failure, restore kernel/device state and
    // rollback
    void *checkaddr = addr;

    //printk ("Checking the validity of (%s) pointer %p stored in %p.\n", what, checkaddr, addr);

    //printk ("Checking the validity of %s  pointer %p.\n", what, addr); 
#ifdef ODFT_INJECT_FAULTS
     // Only start counting when instructed to inject faults
     if (odft_pointer_to_flip()  > 0)   {
        pointer_dereferenced++;
     }
#ifdef FI_PRINT_DEBUG
     printk ("Write Pointer %s deref count %d. Waiting for %d. \n", what, pointer_dereferenced, odft_pointer_to_flip());
#endif 
     if ((pointer_dereferenced == odft_pointer_to_flip()) && (odft_pointer_to_flip() > 0))  {
        checkaddr = addr - 180;
        printk ("Pointer flipped.\n");
     }
#endif

    if ((checkaddr == NULL) && (checkaddr < 4096))   {
         printk ("ADDR IS NULL.\n");
        printk ("Invalid pointer %p(%s)- return.\n", addr, what);
         odft_record_failure;
	 self_destruct();
         return(75);
    }
    //if ((addr == 0x0) || odft_check_range_hash(where, addr) == -1) {  //;  (address_in_range(addr)) == 0)  {
    if ((addr == 0x0) || odft_check_range_hash(where, checkaddr) < 2) {  //;  (address_in_range(addr)) == 0)  {
        //printk ("Invalid pointer %p(%s)- return.\n", addr, what);
        // Call recovery here
        // Can we invoke recovery 
#ifdef ODFT_ENABLE_SFI_CHECKS
        printk ("Invalid pointer %p(%s)- return.\n", addr, what);
	odft_record_failure();
        self_destruct();
        return (75);
#endif	
    }


    //printk ("Pointer valid.\n");
    return 0;
}

int logRead(void *addr , char *what , char * where, char *file , int line ) {
    // In case of failure, restore kernel/device state and
    // rollback
    void *checkaddr =  addr;

    //printk ("Checking the validity of (%s)  pointer %p stored in %p.\n", what, checkaddr, addr);
    //printk ("Checking the validity of %s  pointer %p.\n", what, addr); 

#ifdef ODFT_INJECT_FAULTS
     if (odft_pointer_to_flip() > 0) {
        pointer_dereferenced++;
     }
#ifdef FI_PRINT_DEBUG
     printk ("Read Pointer(%s)  deref count %d. Waiting for %d. \n", what, pointer_dereferenced, odft_pointer_to_flip());
#endif 
     if ((pointer_dereferenced == odft_pointer_to_flip()) && (odft_pointer_to_flip() > 0))  {
        checkaddr = (char *) addr - 800;
        printk ("Read Pointer flipped %s:%s(%d) to %p.\n", what, where, line, checkaddr);
     }
#endif

    if ((checkaddr == NULL) && (checkaddr < 4096))   {
        printk ("ADDR IS NULL.\n");
        printk ("Invalid pointer %p(%s)- return.\n", addr, what);
        odft_record_failure();
        self_destruct();
        return(75);    
    }


    if ((addr == 0x0) || odft_check_range_hash(where, checkaddr) == -1) {  //;  (address_in_range(addr)) == 0)  {
        //printk ("Invalid pointer %p(%s)- return.\n", addr, what);
        // Call recovery here
        // Can we invoke recovery 
        // invoke_recovery();
#ifdef ODFT_ENABLE_SFI_CHECKS        
	odft_record_failure();
        printk ("Invalid pointer %p(%s)- return.\n", addr, what);
        self_destruct();
        return (75);
#endif	
        //return 0;
    }
    
    //printk ("Pointer valid.\n");
    return 0;
}

void * stackguard_get_ra() {};
void stackguard_set_ra(void *new_ra) {};
/* You must provide an implementation for functions that get and set the
 *   * return address. Such code is unfortunately architecture specific.
 *     */
struct stackguard_stack {
    void * data;
    struct stackguard_stack * next;
} * stackguard_stack;

void stackguard_push(void *ra) {
    void * old = stackguard_stack;
    stackguard_stack = (struct stackguard_stack *) kmalloc( sizeof(stackguard_stack), 208U);
    stackguard_stack->data = ra;
    stackguard_stack->next = old;
}

void * stackguard_pop() {
    void * ret = stackguard_stack->data;
    void * next = stackguard_stack->next;
    kfree(stackguard_stack);
    stackguard_stack->next = next;
    return ret;
}

void *heapifymalloc(int sizetoalloc)  {
 // TODO Add to SFI here!   
 void * returnaddr;
 returnaddr = kmalloc(sizetoalloc, 208U); 
 odft_insert_range_hash("e1000_get_regs", returnaddr, sizetoalloc); 
}

void heapifyfree (void * addr) {
    //kfree (addr);
}

// Save state before entering SFI
void save_device_state(void)
{

  return;

}

// Restore state 
void recover_device_state(void)  {

   return;
}

void self_destruct(void)    {
    printk ("SFI failed.\n");

    //printk ("Invoking general protction fault %d.\n", 1/0);
    //printk ("Returning from self_dstruct.\n"); 	
    return;	
}

/*
int 
odft_unwind_task(struct task_struct * task)
{
    struct siginfo info;
    unsigned long flags;

    info.si_signo = SIGCRASH;
    info.si_errno = -EINTR; // BUGBUG: should be better
    info.si_code = SI_KERNEL;
    info.si_pid = current->pid;
    info.si_uid = current->uid;

    //
    //  // Make sure the task can receive SIGCRASH
    //    //
    //      // BUGBUG: this is probably unsafe
    //        //
    spin_lock_irqsave(&task->sighand->siglock,
            flags);
    sigdelset(&task->blocked, SIGCRASH);
    recalc_sigpending_nooks(task);
    spin_unlock_irqrestore(&task->sighand->siglock,
            flags);

    send_sig_info(SIGCRASH, &info, task);
    return(0);
}
*/
