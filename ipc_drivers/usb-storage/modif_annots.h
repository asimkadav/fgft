#define MODIFANNOT(X)  MICRODRIVERS__MODIF_ ## X
#define MODIFIES(var)  MICRODRIVERS__DUMMY((void *) var)
#define MODIFIES_ADDROF(var) MICRODRIVERS__DUMMY((void *) &var)
 

void MODIFANNOT(complete) (struct completion *x) {;}

void MODIFANNOT(usb_free_urb) (struct urb *urb)  {;}

int MODIFANNOT(kthread_stop)(struct task_struct *k)  {return 0;}

void MODIFANNOT(scsi_host_put) (struct Scsi_Host *t)  {;}

    void MODIFANNOT(usb_buffer_free) 
    (struct usb_device *dev, 
     size_t size, 
     void *addr,
     dma_addr_t dma)
{;}

void MODIFANNOT(_spin_lock_irq) (spinlock_t *lock) {
        return;
}

void MODIFANNOT(_spin_unlock_irq) (spinlock_t *lock) {
        return;
}

void MODIFANNOT(scsi_remove_host) (struct Scsi_Host * host) {;}

void MODIFANNOT(__wake_up)(wait_queue_head_t *q, unsigned int mode,
            int nr_exclusive, void *key)   {
    return; 
}

void MODIFANNOT(wait_for_completion) (struct completion * comp)
{;}

int MODIFANNOT(scsi_add_host) (struct Scsi_Host * sh, struct device * dev)
{return 0;}

void MODIFANNOT(init_waitqueue_head) (wait_queue_head_t *q)
{;}

int MODIFANNOT(usb_submit_urb) (struct urb *urb, gfp_t mem_flags)
{return 0;}

void MODIFANNOT(usb_kill_urb) (struct urb *urb)
{;}

unsigned long MODIFANNOT(wait_for_completion_interruptible_timeout)  (struct completion *x, unsigned long timeout)
{return 0;}

int MODIFANNOT(usb_unlink_urb) (struct urb *urb)
{return 0;}

void MODIFANNOT(finish_wait) (wait_queue_head_t *q, wait_queue_t *wait)
{;}

void MODIFANNOT(prepare_to_wait) (wait_queue_head_t *q, wait_queue_t *wait, int state)
{;}

struct urb * MODIFANNOT(usb_alloc_urb) (int iso_packets, gfp_t mem_flags)
{return NULL;}

int MODIFANNOT(wake_up_process) (struct task_struct *tsk)
{return 0;}

int MODIFANNOT(kthread_create_usb) (int (*threadfn)(void *data), void *data, const char namefmt[]) 
{return 0;}

void MODIFANNOT(__mutex_init) (struct mutex *lock)  {
    return;
}

void * MODIFANNOT(usb_buffer_alloc)  (struct usb_device *dev, size_t size, gfp_t mem_flags, dma_addr_t *dma)
{return NULL;}

struct Scsi_Host * MODIFANNOT(scsi_host_alloc) (struct scsi_host_template * temp, int unknown)
{return NULL;}

void MODIFANNOT(mutex_lock) (struct mutex *lock)
{;}

void MODIFANNOT(mutex_unlock) (struct mutex *lock)
{;}

void MODIFANNOT(scsi_report_bus_reset) (struct Scsi_Host * unknow, int unknown ) {;}

int MODIFANNOT(usb_lock_device_for_reset) (struct usb_device *udev, const struct usb_interface *iface)
{return 0;}

void MODIFANNOT(complete_and_exit) (struct completion *comp, long code)
{;}

struct scatterlist * MODIFANNOT(sg_next)  (struct scatterlist * sl)
{return NULL;}

int MODIFANNOT(usb_register_driver) (struct usb_driver * drv, struct module * mod, const char * str)
{return 0;}

void MODIFANNOT(usb_sg_wait) (struct usb_sg_request *io)
{;}

void MODIFANNOT(usb_sg_cancel) (struct usb_sg_request *io)
{;}

int MODIFANNOT(usb_sg_init)  (struct usb_sg_request   *io,
 struct usb_device       *dev,
 unsigned                pipe,
 unsigned                period,
 struct scatterlist      *sg,
 int                     nents,
 size_t                  length,
 gfp_t                   mem_flags
)
{return 0;}

void MODIFANNOT(up) (struct semaphore *sem)
{;}

int MODIFANNOT(usb_reset_device) (struct usb_device *dev)
{return 0;}

void MODIFANNOT(scsi_eh_prep_cmnd) (struct scsi_cmnd *scmd,
        struct scsi_eh_save *ses, unsigned char *cmnd,
        int cmnd_size, unsigned sense_bytes)
{;}

void MODIFANNOT(scsi_eh_restore_cmnd) (struct scsi_cmnd* scmd,
        struct scsi_eh_save *ses)
{;}

void MODIFANNOT(scsi_report_device_reset) (struct Scsi_Host * sh, int u1, int u2)
{;}

void MODIFANNOT(blk_queue_max_sectors) (struct request_queue * rq, unsigned int id) {;}

void MODIFANNOT(scsi_scan_host) (struct Scsi_Host * sh)
{;}

void MODIFANNOT(blk_queue_bounce_limit) (struct request_queue *q, u64 dma_addr)
{;}

void MODIFANNOT(usb_deregister) (struct usb_driver *driver)
{;}

int MODIFANNOT(wait_for_completion_interruptible) (struct completion *x)
{return 0;}

void MODIFANNOT(blk_queue_update_dma_alignment) (struct request_queue * rq, int mask)
{;}


