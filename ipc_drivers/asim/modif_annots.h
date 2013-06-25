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


void MODIFANNOT(__mutex_init) (struct mutex *lock, const char *name, struct lock_class_key *key) {
    return;
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
    
struct thread_info *MODIFANNOT(MJR_current_thread_info)(void) {
    struct thread_info *blah = NULL;
    MODIFIES(blah->task);
    MODIFIES(blah->task->pid);
    return blah;
}

struct proc_dir_entry *MODIFANNOT(create_proc_entry)(char const *name,
                                                     mode_t mode,
                                                     struct proc_dir_entry *parent) {
    MODIFIES(name);
    MODIFIES(mode);
    MODIFIES(parent);
    MODIFIES(parent->name);
    return NULL;
}

struct proc_dir_entry *MODIFANNOT(proc_symlink)(const char *name,
                                                struct proc_dir_entry *parent,
                                                const char *dest) {
    MODIFIES(parent->name); // Also ensures that the return value name is marshaled.
    return NULL;
}

struct usb_interface *MODIFANNOT(usb_ifnum_to_if)(const struct usb_device *dev,
                                                  unsigned ifnum) {
    struct usb_interface *retval =  NULL;
    struct usb_host_endpoint *endpoint = NULL;

    MODIFIES(retval->altsetting);
    MODIFIES(retval->altsetting->desc.bLength);
    MODIFIES(retval->altsetting->desc.bDescriptorType);
    MODIFIES(retval->altsetting->desc.bInterfaceNumber);
    MODIFIES(retval->altsetting->desc.bAlternateSetting);
    MODIFIES(retval->altsetting->desc.bNumEndpoints);
    MODIFIES(retval->altsetting->desc.bInterfaceClass);
    MODIFIES(retval->altsetting->desc.bInterfaceSubClass);
    MODIFIES(retval->altsetting->desc.bInterfaceProtocol);
    MODIFIES(retval->altsetting->desc.iInterface);
    MODIFIES(retval->altsetting->endpoint);
    MODIFIES(retval->altsetting->string);
    MODIFIES(retval->altsetting->extra);
    MODIFIES(retval->altsetting->extralen);

    // Needed for parse_audio_endpoints
    MODIFIES(retval->cur_altsetting);
    MODIFIES(retval->num_altsetting);
    MODIFIES(retval->minor);
    MODIFIES(retval->condition);
    MODIFIES(retval->is_active);
    MODIFIES(retval->sysfs_files_created);
    MODIFIES(retval->ep_devs_created);
    MODIFIES(retval->unregistering);
    MODIFIES(retval->needs_remote_wakeup);
    MODIFIES(retval->needs_altsetting0);
    MODIFIES(retval->needs_binding);
    MODIFIES(retval->reset_running);
    MODIFIES(retval->usb_dev);
    MODIFIES(retval->pm_usage_cnt);
    
    MODIFIES(endpoint->desc.bLength);
    MODIFIES(endpoint->desc.bDescriptorType);
    MODIFIES(endpoint->desc.bEndpointAddress);
    MODIFIES(endpoint->desc.bmAttributes);
    MODIFIES(endpoint->desc.wMaxPacketSize);
    MODIFIES(endpoint->desc.bInterval);
    MODIFIES(endpoint->desc.bRefresh);
    MODIFIES(endpoint->desc.bSynchAddress);
    
    MODIFIES(endpoint->extra);
    MODIFIES(endpoint->extralen);
    MODIFIES(endpoint->enabled);
    return retval;
}

                                                
int MODIFANNOT(usb_register_driver)(struct usb_driver *new_driver, struct module *owner,
                                    const char *mod_name) {
    MODIFIES(new_driver->name);
    MODIFIES(new_driver->probe);
    MODIFIES(new_driver->disconnect);
    MODIFIES(new_driver->ioctl);
    MODIFIES(new_driver->suspend);
    MODIFIES(new_driver->resume);
    MODIFIES(new_driver->reset_resume);
    MODIFIES(new_driver->pre_reset);
    MODIFIES(new_driver->post_reset);
    MODIFIES(new_driver->drvwrap.driver.name);
    MODIFIES(new_driver->drvwrap.driver.bus);
    return 0;
}

int MODIFANNOT(usb_driver_claim_interface)(struct usb_driver *driver,
                                           struct usb_interface *iface, void *priv) {
    MODIFIES(iface->dev.driver_data);
    return 0;
}
