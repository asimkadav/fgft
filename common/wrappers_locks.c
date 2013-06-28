//void __spin_lock_init(spinlock_t *lock , char const   *name , struct lock_class_key *key ) ;
//#line  22 "include/linux/spinlock_api_smp.h"
//void _spin_lock(spinlock_t *lock ) ;
//#line  29
//static void _spin_lock_bh(spinlock_t *lock )  __attribute__((__section__(".spinlock.text"))) ;
//#line  32
//void _spin_lock_irq(spinlock_t *lock ) ;
//#line  35

#include <linux/kernel.h>
#include <linux/spinlock.h>

extern int odft_insert_klog_hash(const char *, void *, int, void *, int);


unsigned long spin_lock_irqsave_asim(spinlock_t *lock, unsigned long flags) {
	spin_lock_irqsave(lock, flags);
	printk ("Recording lock %p.\n", lock);
	odft_insert_klog_hash ("spin_lock_irqsave", lock, sizeof(void *), flags, sizeof(int));  
	return;
}

unsigned long spin_lock_irq_asim(spinlock_t *lock) {
	spin_lock_irq(lock);
	printk ("****Recording lock %p.\n", lock);
	odft_insert_klog_hash ("spin_lock_irq", lock, sizeof(void *), 0, sizeof(int));  
	return;
}

unsigned long spin_unlock_irq_asim(spinlock_t *lock) {
	return;
}

unsigned long spin_lock_bh_asim(spinlock_t *lock, unsigned long flags) {
	//spin_lock_bh(lock, flags);
	return;
}

void __raw_spin_unlock_asim (raw_spinlock_t *lock)	{
	// so weird?
	printk ("***Not freeing lock %p.\n", lock);
	return;
}



//void _spin_unlock(spinlock_t *lock ) ;
//static void _spin_unlock_bh(spinlock_t *lock )  __attribute__((__section__(".spinlock.text"))) ;
//#line  53
void spin_unlock_irqrestore_asim(spinlock_t *lock, unsigned long flags) {
	printk ("Not freeing lock %p\n", lock); 
	// Locks are released at end of transaction 
	//spin_unlock_irq(lock);	
	return;
}


unsigned long spin_unlock_asim(spinlock_t *lock, unsigned long flags) {
	//spin_lock(lock, flags);
	return;
}

unsigned long spin_unlock_bh_asim(spinlock_t *lock, unsigned long flags) {
	//spin_lock(lock, flags);
	return;
}


//void _spin_unlock_irqrestore(spinlock_t *lock , unsigned long flags );
