#ifndef TESTING_STATE_H
#define TESTING_STATE_H

#include "hashtable.h"

///////////////////////////////////////////////////////////////////////////////
// Should we execute this check?
///////////////////////////////////////////////////////////////////////////////
#define check_routine()                                         \
    do {                                                        \
        uprintk ("Checking %s %s %d\n", __func__, fn, prepost); \
    } while (0)

///////////////////////////////////////////////////////////////////////////////
//
// What do we do if an assertion fails? Continue along other paths
//
///////////////////////////////////////////////////////////////////////////////
void tidy_exit(int code);
#define assert_detail(x, msg, ...)                                      \
    if (!(x)) {                                                         \
        uprintk ("Assertion failure in function %s:%d from %s\n",  __func__, __LINE__, fn); \
        uprintk (msg, ## __VA_ARGS__);                                  \
        tidy_exit (1);                                                  \
    }

#define assert(x) assert_detail(x, "%s\n", fn)

///////////////////////////////////////////////////////////////////////////////
//
// Driver state
// If you add fields, be sure to initialize them in:
// initialize_driver_state
// Free everything in:
// free_driver_state
//
///////////////////////////////////////////////////////////////////////////////
enum call_state {
    CALL_UNDEFINED = 0,
    NOT_CALLED = 1,
    IN_PROGRESS = 2,
    CALLED_OK = 3,
    CALLED_FAILED = 4
};

enum device_state {
    DEVICE_STATE_UNDEFINED = 0,
    DEVICE_STATE_DISABLED = 1,
    DEVICE_STATE_ENABLED = 2
};

//
// TODO: We're conflating preemption with interrupts.
// Not exactly the same thing.  Preemption may be disabled
// but interrupts may still be enabled.
// Compare local_irq_save() with preempt_disable()
//
enum blocking_context {
    BLOCKING_UNDEFINED = 0,
    BLOCKING_YES = 1, // blocking allowed
    BLOCKING_NO = 2  // blocking not allowed
};

enum user_context {
    USER_UNDEFINED = 0, // Unknown
    USER_YES = 1, // Execution path is such that user can invoke it
    USER_NO = 2 // Execution path is such that use cannot invoke it
};

enum driver_bus_type {
    DRIVER_UNKNOWN_BUS = 0,
    DRIVER_PCI,
    DRIVER_USB,
    DRIVER_MISC,
};

enum driver_class_type {
    DRIVER_UNKNOWN_CLASS = 0,
    DRIVER_NET,
    DRIVER_SND
};

#define MAX_NESTING 16

struct global_state_struct {
    enum blocking_context blocking_context[MAX_NESTING];
    int blocking_context_current;

    enum user_context user_context[MAX_NESTING];
    int user_context_current;

    /* enum called_from_user user_context; */
    struct hashtable *objs;
};

struct pci_driver_state_struct {
    int pci;
    
    enum call_state registered;
    enum call_state unregistered;

    enum device_state enabled;
    
    enum call_state probed;
};

struct usb_driver_state_struct {
    int usb;
    
    enum call_state registered;
    enum call_state unregistered;
    enum call_state probed;
};

struct misc_driver_state_struct  {
    int misc;
    
    enum call_state registered;
    enum call_state unregistered;
    enum call_state probed;
};

struct ndo_driver_state_struct {
    int net;
    
    enum call_state opened;
    int trans_start_check;

    enum call_state register_netdev;
    enum call_state unregister_netdev;
    enum call_state capable;

    int queue_state;
};

struct sound_driver_state_struct {
    int sound;
};

///////////////////////////////////////////////////////////////////////////////
//
// Hash table management
//
///////////////////////////////////////////////////////////////////////////////

#ifndef DISABLE_E1000_CHECKS
enum OBJECT_ORIGIN
{
    ORIGIN_UNDEFINED = 0,

    ORIGIN_KMALLOC = 1,
    ORIGIN_VMALLOC = 2,
    ORIGIN_GET_FREE_PAGES = 4,
    ORIGIN_DMA_ALLOC_COHERENT = 8,
    ORIGIN_PCI_ALLOC_CONSISTENT = 16,

    ORIGIN_PCI_MAP_SINGLE = 32,
    ORIGIN_PCI_MAP_PAGE = 64,
    ORIGIN_SKB_DMA_MAP = 128,
    ORIGIN_ALLOC_ETHERDEV_MQ = 256,
    ORIGIN_ALLOC_URB = 512,
    ORIGIN_DEVICE_REGISTER = 1024,
    ORIGIN_GET_DEVICE = 2048,

    ORIGIN_SPIN_LOCK = 4096,
    ORIGIN_MUTEX = 8192,
    ORIGIN_RWSEM = 16384,
    ORIGIN_RWLOCK = 32768,

    ORIGIN_IOREMAP = 65536,
    ORIGIN_DEV_ALLOC_SKB = 65536 * 2
};
#else
enum OBJECT_ORIGIN
{
    ORIGIN_UNDEFINED = 0,

    ORIGIN_KMALLOC = 1,
    ORIGIN_VMALLOC = 2,
    ORIGIN_GET_FREE_PAGES = 4,
    ORIGIN_DMA_ALLOC_COHERENT = 8,
    ORIGIN_PCI_ALLOC_CONSISTENT = 16,

    ORIGIN_PCI_MAP_SINGLE = 32, // For E1000
    ORIGIN_PCI_MAP_PAGE = 32, // For E1000
    ORIGIN_SKB_DMA_MAP = 128,
    ORIGIN_ALLOC_ETHERDEV_MQ = 256,
    ORIGIN_ALLOC_URB = 512,
    ORIGIN_DEVICE_REGISTER = 1024,
    ORIGIN_GET_DEVICE = 2048,

    ORIGIN_SPIN_LOCK = 4096,
    ORIGIN_MUTEX = 8192,
    ORIGIN_RWSEM = 16384,
    ORIGIN_RWLOCK = 32768,

    ORIGIN_IOREMAP = 65536,
    ORIGIN_DEV_ALLOC_SKB = 65536 * 2
};
#endif

#if !defined (DISABLE_CA0106_CHECKS) && !defined (DISABLE_ENS1371_CHECKS) && !defined (DISABLE_USB_AUDIO_CHECKS) && !defined (DISABLE_CMIPCI_CHECKS)
enum LOCK_TYPE
{
    LOCK_UNDEFINED = 0,

    SPIN_LOCK = 1,
    SPIN_LOCK_BH = 2,
    SPIN_LOCK_IRQSAVE = 3,
    SPIN_LOCK_IRQ = 4,
    
    MUTEX = 5,
    RWSEM = 6,
    RWLOCK = 7,

    GENERIC_LOCK = 8,
    NUM_LOCK_TYPES = 9
};
#else
enum LOCK_TYPE
{
    LOCK_UNDEFINED = 0,

    SPIN_LOCK = 1, // Treat these all the same.  We have issues in snd_timer_interrupt otherwise.
    SPIN_LOCK_BH = 1,
    SPIN_LOCK_IRQSAVE = 1,
    SPIN_LOCK_IRQ = 1,
    
    MUTEX = 2,
    RWSEM = 3,
    RWLOCK = 4,

    GENERIC_LOCK = 5,
    NUM_LOCK_TYPES = 6
};
#endif

struct hkey {
    unsigned long long object;
};

struct hvalue {
    const char *origin_str; // Description of origin.
    enum OBJECT_ORIGIN origin; // ORIGIN_blah
    unsigned long size; // Size of object or -1 if unknown/irrelevant
    int new_object; // 1 if new, 0 if not new.

    // See lock allocation / acquision / release code
    char lock_state[NUM_LOCK_TYPES]; // either 0 or 1 for each type of lock
};

//
// Some helper functions for object tracking
//
const char *ORIGIN_TO_STR(enum OBJECT_ORIGIN origin);
void objs_key (struct hkey *key,
               unsigned long long object);
struct hkey *objs_key_ptr (unsigned long long object);
struct hvalue *objs_value (const char *origin_str,
                           enum OBJECT_ORIGIN origin,
                           unsigned long size,
                           int new_object);
void objs_insert (const char *fn,
                  struct hkey *key,
                  struct hvalue *value);
struct hvalue *objs_search (struct hkey *key);
struct hvalue *objs_remove (struct hkey *key);
void objs_enum_print (void);
void objs_enum_print_new (void);

//
// Managing the state we're in -- blocking / nonblocking / user-invokable etc
//
enum driver_bus_type driver_bus (const char *fn);
enum driver_class_type driver_class (const char *fn);
void set_driver_bus (const char *fn, enum driver_bus_type type);
void set_driver_class (const char *fn, enum driver_class_type type);
void set_device_state (const char *fn,
                       enum device_state *cur_state,
                       enum device_state new_state);
void set_call_state (const char *fn,
                     enum call_state *state,
                     enum call_state new_state);
void set_kernel_state (const char *fn,
                       int reset,
                       int retval,
                       enum blocking_context blocking_context,
                       enum user_context user_context);
void push_kernel_state (const char *fn,
                        enum blocking_context blocking_context);
void pop_kernel_state (const char *fn);
enum blocking_context kernel_blocking_state (void);
enum user_context kernel_user_state (void);
int can_call_interrupt_handlers (void);

//
// Setup / shutdown functions
//
void initialize_driver_state (void);
void free_driver_state (void);

//
// Memory allocations
//
void assert_allocated_weak (const char *fn,
                            int prepost,
                            unsigned long long object,
                            unsigned long minsize);
void assert_allocated (const char *fn,
                       int prepost,
                       enum OBJECT_ORIGIN origin,
                       unsigned long long object);
void generic_allocator (const char *fn,
                        int prepost,
                        unsigned long long object,
                        unsigned long object_size,
                        enum OBJECT_ORIGIN origin);
void generic_free (const char *fn,
                   int prepost,
                   unsigned long long object,
                   enum OBJECT_ORIGIN origin);

//
// Locking protocol
//
void verify_all_locks_released(const char *fn);

void generic_lock_allocator (const char *fn,
                             int prepost,
                             const void *lock,
                             enum OBJECT_ORIGIN origin);
void generic_lock_state (const char *fn,
                         int prepost,
                         const void *lock,
                         enum OBJECT_ORIGIN origin,
                         enum LOCK_TYPE lock_type,
                         int add_this);

//
// IOCTL checking
//
void ioctl_called_capable(const char *fn,
                          int prepost,
                          int retval,
                          int cmd);
void call_capable(const char *fn,
                  int prepost,
                  int retval,
                  int capability);

//
// Memory flag testing
//
void mem_flags_test(const char *fn,
                    unsigned int blocking_no,
                    unsigned int blocking_yes,
                    unsigned int mem_flags);
#endif
