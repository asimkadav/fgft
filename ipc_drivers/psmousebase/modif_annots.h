#define MODIFANNOT(X)  MICRODRIVERS__MODIF_ ## X
#define MODIFIES(var)  MICRODRIVERS__DUMMY((void *) var)
#define MODIFIES_ADDROF(var) MICRODRIVERS__DUMMY((void *) &var)

/*
 * The externally visible workqueue abstraction is an array of
 * per-CPU workqueues:
 */
struct workqueue_struct {
    struct cpu_workqueue_struct *cpu_wq;
    const char *name;
    struct list_head list;  /* Empty if single thread */
};


void MICRODRIVERS__DUMMY(void *x) {
  return;
}

// Does not modify anything.
const char *MODIFANNOT(dev_driver_string) (struct device *dev) {
    return 0;
}

int MODIFANNOT(pci_bus_write_config_word) (struct pci_bus *bus, unsigned int devfn, int where, u16 val) {
    return 0;
}

/* DONE */
void MODIFANNOT(_spin_lock) (spinlock_t *lock) {
    return;
}

/* DONE */
void MODIFANNOT(__spin_lock_init)
  (spinlock_t *lock, const char *name, struct lock_class_key *key) {
  return;
}

// DONE
void MODIFANNOT(_spin_lock_irq) (spinlock_t *lock) {
    return;
}

/* DONE */
void MODIFANNOT(_spin_lock_irqsave) (spinlock_t *lock) {
    return;
}

/* DONE */
void MODIFANNOT(_spin_unlock)
  (spinlock_t *lock) {
  return;
}

void MODIFANNOT(_spin_unlock_irq) (spinlock_t *lock) {
    return;
}

/* DONE */
void MODIFANNOT(_spin_unlock_irqrestore)
  (spinlock_t *lock, unsigned long flags) {
  return;
}

/* TODO */
int MODIFANNOT(pci_bus_read_config_byte)
    (struct pci_bus *bus,
     unsigned int devfn,
     int where,
     u8 *val)
{
    MODIFIES(val);
    return 0;
}

/* TODO */
int MODIFANNOT(pci_bus_read_config_word)
    (struct pci_bus *bus,
     unsigned int devfn,
     int where,
     u16 *val)
{
    MODIFIES(val);
    return 0;
}

/* TODO */
void MODIFANNOT(pci_clear_mwi) (struct pci_dev *dev) {
    return;
}

/* DONE */
int MODIFANNOT(request_irq)
    (unsigned int a,
     irqreturn_t (*handler)(int, void *, struct pt_regs *),
     unsigned long b, const char * c, void * d) {
    return 0;
}

void MODIFANNOT(free_irq)
    (unsigned int irq , struct net_device *dev )
{
    return;
}

// MJR NEW FOR USENIX:

int MODIFANNOT(ps2_command)
    (struct ps2dev *ps2dev, unsigned char *param, int command)
{
    MODIFIES(ps2dev->flags);
    MODIFIES(ps2dev->cmdcnt);
    MODIFIES(ps2dev->cmdbuf);
    MODIFIES(param);
    return 0;
}

/* E1000 */
void MODIFANNOT(init_timer)(struct timer_list *timer)
{
//      MODIFIES(timer->entry.next);
//      MODIFIES(timer->base);
}

/* E1000 */
struct workqueue_struct *MODIFANNOT(__create_workqueue)(const char *name, int singlethread)
{
    return NULL;
}

/* E1000 */
void MODIFANNOT(flush_workqueue)(struct workqueue_struct *wq)
{
    return;
}

/* E1000 */
void MODIFANNOT(destroy_workqueue)(struct workqueue_struct *wq)
{
    return;
}

void MODIFANNOT(ps2_cmd_aborted)(struct ps2dev *ps2dev) {
    MODIFIES(ps2dev->flags);
}

void MODIFANNOT(ps2_drain)(struct ps2dev *ps2dev, int maxbytes, int timeout) {
    MODIFIES(ps2dev->cmdbuf);
    MODIFIES(ps2dev->flags);
    MODIFIES(ps2dev->cmdcnt);
}

int MODIFANNOT(ps2_handle_ack)(struct ps2dev *ps2dev, unsigned char data) {
    MODIFIES(ps2dev->nak);
    MODIFIES(ps2dev->flags);
    return 0;
}

int MODIFANNOT(ps2_handle_response)(struct ps2dev *ps2dev, unsigned char data) {
    MODIFIES(ps2dev->cmdbuf);
    MODIFIES(ps2dev->flags);
    MODIFIES(ps2dev->cmdcnt);
    return 0;
}
    
void MODIFANNOT(ps2_init)(struct ps2dev *ps2dev, struct serio *serio) {
    MODIFIES(ps2dev->serio);
}

int MODIFANNOT(ps2_sendbyte)(struct ps2dev *ps2dev, unsigned char byte, int timeout) {
    MODIFIES(ps2dev->nak);
    MODIFIES(ps2dev->flags);
    return 0;
}
    
