#define MODIFANNOT(X)  MICRODRIVERS__MODIF_ ## X
#define MODIFIES(var)  MICRODRIVERS__DUMMY((void *) var)
#define MODIFIES_ADDROF(var) MICRODRIVERS__DUMMY((void *) &var)
//#define MODIFIES(var)        /**/
//#define MODIFIES_ADDROF(var) /**/

void MICRODRIVERS__DUMMY(void *x) {
    return;
}

/* DONE */
void * MODIFANNOT(kmem_cache_zalloc) 
  (struct kmem_cache *a, gfp_t b) { 
  return 0; 
}

/* DONE */
unsigned long MODIFANNOT(_spin_lock_irqsave) 
    (spinlock_t *lock) {
    return 0;  
}

/* DONE */
void MODIFANNOT(_spin_unlock_irqrestore) 
    (spinlock_t *lock, unsigned long flags) {
    return;  
}

/* DONE */
void MODIFANNOT(pci_disable_device)
    (struct pci_dev *dev) {
    MODIFIES(dev->bus);
    MODIFIES(dev->devfn);
    return;
}

/* DONE */
void MODIFANNOT(pci_set_master) 
    (struct pci_dev *dev) {
    MODIFIES(dev->bus);
    MODIFIES(dev->devfn);
    MODIFIES(dev->dev.bus_id);
    return;
}

/* TODO */
void MODIFANNOT(__release_region)
    (struct resource * a, resource_size_t b, resource_size_t c) {
    
    return;  
}

/* TODO */
void MODIFANNOT(__spin_lock_init) 
  (spinlock_t *lock, const char *name, struct lock_class_key *key) {
  return;  
}

/* TODO */
struct resource * MODIFANNOT(__request_region) 
    (struct resource * a, resource_size_t start,
     resource_size_t n, const char *name) {
    return 0;   
}
   
/* TODO */
int MODIFANNOT(pci_enable_device) 
    (struct pci_dev *dev) {
    
    return 0;  
}

/* TODO */
void MODIFANNOT(pci_unregister_driver)
    (struct pci_driver *drv) {
    MODIFIES_ADDROF(drv->driver);
    return;  
}

/* DONE */
void MODIFANNOT(_spin_unlock) 
    (spinlock_t *lock) {
    return;  
}

/* DONE */
void MODIFANNOT(_spin_lock) 
    (spinlock_t *lock) {
    return;
}
  
void MODIFANNOT(_spin_lock_bh)
    (spinlock_t *lock) {
    return;  
}

/* TODO */
int MODIFANNOT(__pci_register_driver)
    (struct pci_driver *drv, struct module *owner) {
    MODIFIES(drv->driver.name);
    MODIFIES(drv->name);
    MODIFIES(drv->driver.bus);
    MODIFIES(drv->driver.owner);
    MODIFIES_ADDROF(drv->dynids.lock);
    MODIFIES_ADDROF(drv->dynids.list);
    MODIFIES_ADDROF(drv->driver);
    MODIFIES(drv->probe);
    
    return 0;  
}

/* DONE */
int MODIFANNOT(request_irq)
    (unsigned int a,
     irqreturn_t (*handler)(int, void *, struct pt_regs *),
     unsigned long b, const char * c, void * d) {
    return 0;   
}

/* DONE */
void MODIFANNOT(_spin_unlock_bh) 
  (spinlock_t *lock) {
  return;  
}

// Locking function
void MODIFANNOT(_spin_lock_irq) (spinlock_t *lock) {
    return;
}

// Locking function
void MODIFANNOT(mutex_unlock) (struct mutex *lock) {
    return;
}

// Locking function
void MODIFANNOT(mutex_lock) (struct mutex *lock) {
    return;
}

// Locking function
void MODIFANNOT(_spin_unlock_irq) (spinlock_t *lock) {
    return;
}

void MODIFANNOT(__wake_up) (wait_queue_head_t *q, unsigned int mode,
                            int nr_exclusive, void *key) {
    return;
}

void MODIFANNOT(pci_release_regions) (struct pci_dev *pdev) {
    return;
}

void MODIFANNOT(free_irq) (unsigned int irq, void *dev_id) {
    return;
}

int MODIFANNOT(pci_set_power_state) (struct pci_dev *dev, pci_power_t state) {
    return 0;
}

int MODIFANNOT(pci_save_state) (struct pci_dev *dev) {
    return 0;
}

int MODIFANNOT(pci_restore_state) (struct pci_dev *dev) {
    return 0;
}

int MODIFANNOT(pci_bus_read_config_byte) (struct pci_bus *bus,
                                          unsigned int devfn,
                                          int where,
                                          u8 *val) {
    return 0;
}

void MODIFANNOT(__mutex_init) (struct mutex *lock, const char *name, struct lock_class_key *key) {
    return;
}

int MODIFANNOT(pci_bus_read_config_word) (struct pci_bus *bus,
                                          unsigned int devfn,
                                          int where,
                                          u16 *val) {
    return 0;
}

int MODIFANNOT(pci_request_regions) (struct pci_dev *pdev,
                                     const char *res_name) {
    return 0;
}

int MODIFANNOT(device_create_file)(struct device *dev, struct device_attribute *attr) {
    MODIFIES(attr->attr.mode);
    MODIFIES(attr->attr.name);
    //MODIFIES(dev->kobj.sd);
    return 0;
}
    
int MODIFANNOT(hrtimer_get_res)(const clockid_t which_clock, struct timespec *tp) {
    MODIFIES(tp->tv_sec);
    MODIFIES(tp->tv_nsec);
    return 0;
}

struct proc_dir_entry *MODIFANNOT(create_proc_entry)(char const *name, mode_t mode, struct proc_dir_entry *parent) {
    MODIFIES(name);
    MODIFIES(mode);
    MODIFIES(parent);
    MODIFIES(parent->name);
    return NULL;
}

struct proc_dir_entry *MODIFANNOT(proc_symlink)(const char *name,
                                                struct proc_dir_entry *parent, const char *dest) {
    MODIFIES(parent->name); // Also ensures that the return value name is marshaled.
    return NULL;
}

int MODIFANNOT(logWrite) (void *addr , char *what, char *where, char *file , int line ) {
	return 0;
}

int MODIFANNOT(logRead) (void *addr , char *what, char *where, char *file , int line ) {
	return 0;
}

int MODIFANNOT(logStackFrame) (char *func ) {
	return 0;
}

int MODIFANNOT (logAlloc) (void *addr , int size , char *fn, char *file , char *allocfn, int line ) {
	return 0;
}


