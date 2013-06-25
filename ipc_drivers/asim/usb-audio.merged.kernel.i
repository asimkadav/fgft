# 1 "usb-audio.merged.kernel.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "usb-audio.merged.kernel.c"




typedef signed char __s8;

typedef unsigned char __u8;

typedef short __s16;

typedef unsigned short __u16;

typedef int __s32;

typedef unsigned int __u32;

typedef long long __s64;

typedef unsigned long long __u64;

typedef signed char s8;

typedef unsigned char u8;

typedef unsigned short u16;

typedef unsigned int u32;

typedef long long s64;

typedef unsigned long long u64;

typedef unsigned short umode_t;

typedef u64 dma_addr_t;

typedef unsigned int __kernel_mode_t;

typedef unsigned long __kernel_nlink_t;

typedef long __kernel_off_t;

typedef int __kernel_pid_t;

typedef unsigned int __kernel_uid_t;

typedef unsigned int __kernel_gid_t;

typedef unsigned long __kernel_size_t;

typedef long __kernel_ssize_t;

typedef long __kernel_time_t;

typedef long __kernel_clock_t;

typedef int __kernel_timer_t;

typedef int __kernel_clockid_t;

typedef long long __kernel_loff_t;

typedef __kernel_uid_t __kernel_uid32_t;

typedef __kernel_gid_t __kernel_gid32_t;

typedef __u32 __kernel_dev_t;

typedef __kernel_dev_t dev_t;

typedef __kernel_mode_t mode_t;

typedef __kernel_nlink_t nlink_t;

typedef __kernel_off_t off_t;

typedef __kernel_pid_t pid_t;

typedef __kernel_timer_t timer_t;

typedef __kernel_clockid_t clockid_t;

typedef __kernel_uid32_t uid_t;

typedef __kernel_gid32_t gid_t;

typedef __kernel_loff_t loff_t;

typedef __kernel_size_t size_t;

typedef __kernel_ssize_t ssize_t;

typedef __kernel_time_t time_t;

typedef __kernel_clock_t clock_t;

typedef __s8 int8_t;

typedef __s16 int16_t;

typedef __u32 u_int32_t;

typedef __s32 int32_t;

typedef __u8 uint8_t;

typedef __u16 uint16_t;

typedef __u32 uint32_t;

typedef unsigned long sector_t;

typedef unsigned long blkcnt_t;

typedef __u16 __le16;

typedef unsigned int gfp_t;

typedef unsigned int fmode_t;

struct __anonstruct_atomic_t_7 {
   int volatile counter ;
};

typedef struct __anonstruct_atomic_t_7 atomic_t;

struct __anonstruct_atomic64_t_8 {
   long volatile counter ;
};

typedef struct __anonstruct_atomic64_t_8 atomic64_t;

struct module;

struct task_struct;

struct mm_struct;

struct pt_regs {
   unsigned long r15 ;
   unsigned long r14 ;
   unsigned long r13 ;
   unsigned long r12 ;
   unsigned long bp ;
   unsigned long bx ;
   unsigned long r11 ;
   unsigned long r10 ;
   unsigned long r9 ;
   unsigned long r8 ;
   unsigned long ax ;
   unsigned long cx ;
   unsigned long dx ;
   unsigned long si ;
   unsigned long di ;
   unsigned long orig_ax ;
   unsigned long ip ;
   unsigned long cs ;
   unsigned long flags ;
   unsigned long sp ;
   unsigned long ss ;
};

struct kernel_vm86_regs {
   struct pt_regs pt ;
   unsigned short es ;
   unsigned short __esh ;
   unsigned short ds ;
   unsigned short __dsh ;
   unsigned short fs ;
   unsigned short __fsh ;
   unsigned short gs ;
   unsigned short __gsh ;
};

union __anonunion____missing_field_name_9 {
   struct pt_regs *regs ;
   struct kernel_vm86_regs *vm86 ;
};

struct math_emu_info {
   long ___orig_eip ;
   union __anonunion____missing_field_name_9 __annonCompField4 ;
};

typedef __builtin_va_list __gnuc_va_list;

typedef __gnuc_va_list va_list;

struct bug_entry {
   int bug_addr_disp ;
   int file_disp ;
   unsigned short line ;
   unsigned short flags ;
};

struct completion;

struct pid;

typedef unsigned long pgdval_t;

typedef unsigned long pgprotval_t;

struct page;

struct __anonstruct_pgd_t_13 {
   pgdval_t pgd ;
};

typedef struct __anonstruct_pgd_t_13 pgd_t;

struct __anonstruct_pgprot_t_14 {
   pgprotval_t pgprot ;
};

typedef struct __anonstruct_pgprot_t_14 pgprot_t;

struct __anonstruct____missing_field_name_18 {
   unsigned int a ;
   unsigned int b ;
};

struct __anonstruct____missing_field_name_19 {
   u16 limit0 ;
   u16 base0 ;
   unsigned int base1 : 8 ;
   unsigned int type : 4 ;
   unsigned int s : 1 ;
   unsigned int dpl : 2 ;
   unsigned int p : 1 ;
   unsigned int limit : 4 ;
   unsigned int avl : 1 ;
   unsigned int l : 1 ;
   unsigned int d : 1 ;
   unsigned int g : 1 ;
   unsigned int base2 : 8 ;
};

union __anonunion____missing_field_name_17 {
   struct __anonstruct____missing_field_name_18 __annonCompField6 ;
   struct __anonstruct____missing_field_name_19 __annonCompField7 ;
};

struct desc_struct {
   union __anonunion____missing_field_name_17 __annonCompField8 ;
} __attribute__((__packed__)) ;

struct ds_context;

struct bts_tracer;

struct exec_domain;

struct map_segment;

struct exec_domain {
   char const *name ;
   void (*handler)(int , struct pt_regs * ) ;
   unsigned char pers_low ;
   unsigned char pers_high ;
   unsigned long *signal_map ;
   unsigned long *signal_invmap ;
   struct map_segment *err_map ;
   struct map_segment *socktype_map ;
   struct map_segment *sockopt_map ;
   struct map_segment *af_map ;
   struct module *module ;
   struct exec_domain *next ;
};

struct cpumask {
   unsigned long bits[((255UL + 8UL * sizeof(long )) - 1UL) / (8UL * sizeof(long ))] ;
};

typedef struct cpumask cpumask_t;

struct thread_struct;

struct i387_fsave_struct {
   u32 cwd ;
   u32 swd ;
   u32 twd ;
   u32 fip ;
   u32 fcs ;
   u32 foo ;
   u32 fos ;
   u32 st_space[20] ;
   u32 status ;
};

struct __anonstruct____missing_field_name_28 {
   u64 rip ;
   u64 rdp ;
};

struct __anonstruct____missing_field_name_29 {
   u32 fip ;
   u32 fcs ;
   u32 foo ;
   u32 fos ;
};

union __anonunion____missing_field_name_27 {
   struct __anonstruct____missing_field_name_28 __annonCompField9 ;
   struct __anonstruct____missing_field_name_29 __annonCompField10 ;
};

union __anonunion____missing_field_name_30 {
   u32 padding1[12] ;
   u32 sw_reserved[12] ;
};

struct i387_fxsave_struct {
   u16 cwd ;
   u16 swd ;
   u16 twd ;
   u16 fop ;
   union __anonunion____missing_field_name_27 __annonCompField11 ;
   u32 mxcsr ;
   u32 mxcsr_mask ;
   u32 st_space[32] ;
   u32 xmm_space[64] ;
   u32 padding[12] ;
   union __anonunion____missing_field_name_30 __annonCompField12 ;
} __attribute__((__aligned__(16))) ;

struct i387_soft_struct {
   u32 cwd ;
   u32 swd ;
   u32 twd ;
   u32 fip ;
   u32 fcs ;
   u32 foo ;
   u32 fos ;
   u32 st_space[20] ;
   u8 ftop ;
   u8 changed ;
   u8 lookahead ;
   u8 no_update ;
   u8 rm ;
   u8 alimit ;
   struct math_emu_info *info ;
   u32 entry_eip ;
};

struct xsave_hdr_struct {
   u64 xstate_bv ;
   u64 reserved1[2] ;
   u64 reserved2[5] ;
} __attribute__((__packed__)) ;

struct xsave_struct {
   struct i387_fxsave_struct i387 ;
   struct xsave_hdr_struct xsave_hdr ;
} __attribute__((__packed__, __aligned__(64))) ;

union thread_xstate {
   struct i387_fsave_struct fsave ;
   struct i387_fxsave_struct fxsave ;
   struct i387_soft_struct soft ;
   struct xsave_struct xsave ;
};

struct kmem_cache;

struct thread_struct {
   struct desc_struct tls_array[3] ;
   unsigned long sp0 ;
   unsigned long sp ;
   unsigned long usersp ;
   unsigned short es ;
   unsigned short ds ;
   unsigned short fsindex ;
   unsigned short gsindex ;
   unsigned long ip ;
   unsigned long fs ;
   unsigned long gs ;
   unsigned long debugreg0 ;
   unsigned long debugreg1 ;
   unsigned long debugreg2 ;
   unsigned long debugreg3 ;
   unsigned long debugreg6 ;
   unsigned long debugreg7 ;
   unsigned long cr2 ;
   unsigned long trap_no ;
   unsigned long error_code ;
   union thread_xstate *xstate ;
   unsigned long *io_bitmap_ptr ;
   unsigned long iopl ;
   unsigned int io_bitmap_max ;
   unsigned long debugctlmsr ;
   struct ds_context *ds_ctx ;
   unsigned int bts_ovfl_signal ;
};

struct __anonstruct_mm_segment_t_31 {
   unsigned long seg ;
};

typedef struct __anonstruct_mm_segment_t_31 mm_segment_t;

struct list_head {
   struct list_head * __attribute__((__recursive__)) next ;
   struct list_head * __attribute__((__recursive__)) prev ;
};

struct hlist_node;

struct hlist_head {
   struct hlist_node *first ;
};

struct hlist_node {
   struct hlist_node *next ;
   struct hlist_node **pprev ;
};

struct timespec;

struct compat_timespec;

struct __anonstruct____missing_field_name_33 {
   unsigned long arg0 ;
   unsigned long arg1 ;
   unsigned long arg2 ;
   unsigned long arg3 ;
};

struct __anonstruct_futex_34 {
   u32 *uaddr ;
   u32 val ;
   u32 flags ;
   u32 bitset ;
   u64 time ;
};

struct __anonstruct_nanosleep_35 {
   clockid_t index ;
   struct timespec *rmtp ;
   struct compat_timespec *compat_rmtp ;
   u64 expires ;
};

struct pollfd;

struct __anonstruct_poll_36 {
   struct pollfd *ufds ;
   int nfds ;
   int has_timeout ;
   unsigned long tv_sec ;
   unsigned long tv_nsec ;
};

union __anonunion____missing_field_name_32 {
   struct __anonstruct____missing_field_name_33 __annonCompField13 ;
   struct __anonstruct_futex_34 futex ;
   struct __anonstruct_nanosleep_35 nanosleep ;
   struct __anonstruct_poll_36 poll ;
};

struct restart_block {
   long (*fn)(struct restart_block * ) ;
   union __anonunion____missing_field_name_32 __annonCompField14 ;
};

typedef atomic64_t atomic_long_t;

struct thread_info {
   struct task_struct *task ;
   struct exec_domain *exec_domain ;
   __u32 flags ;
   __u32 status ;
   __u32 cpu ;
   int preempt_count ;
   mm_segment_t addr_limit ;
   struct restart_block restart_block ;
   void *sysenter_return ;
};

struct raw_spinlock {
   unsigned int slock ;
};

typedef struct raw_spinlock raw_spinlock_t;

struct __anonstruct_raw_rwlock_t_37 {
   unsigned int lock ;
};

typedef struct __anonstruct_raw_rwlock_t_37 raw_rwlock_t;

struct lockdep_map;

struct stack_trace {
   unsigned int nr_entries ;
   unsigned int max_entries ;
   unsigned long *entries ;
   int skip ;
};

struct lockdep_subclass_key {
   char __one_byte ;
} __attribute__((__packed__)) ;

struct lock_class_key {
   struct lockdep_subclass_key subkeys[8UL] ;
};

struct lock_class {
   struct list_head hash_entry ;
   struct list_head lock_entry ;
   struct lockdep_subclass_key *key ;
   unsigned int subclass ;
   unsigned int dep_gen_id ;
   unsigned long usage_mask ;
   struct stack_trace usage_traces[9] ;
   struct list_head locks_after ;
   struct list_head locks_before ;
   unsigned int version ;
   unsigned long ops ;
   char const *name ;
   int name_version ;
};

struct lockdep_map {
   struct lock_class_key *key ;
   struct lock_class *class_cache ;
   char const *name ;
};

struct held_lock {
   u64 prev_chain_key ;
   unsigned long acquire_ip ;
   struct lockdep_map *instance ;
   struct lockdep_map *nest_lock ;
   unsigned int class_idx : 13 ;
   unsigned int irq_context : 2 ;
   unsigned int trylock : 1 ;
   unsigned int read : 2 ;
   unsigned int check : 2 ;
   unsigned int hardirqs_off : 1 ;
};

struct __anonstruct_spinlock_t_38 {
   raw_spinlock_t raw_lock ;
   unsigned int magic ;
   unsigned int owner_cpu ;
   void *owner ;
   struct lockdep_map dep_map ;
};

typedef struct __anonstruct_spinlock_t_38 spinlock_t;

struct __anonstruct_rwlock_t_39 {
   raw_rwlock_t raw_lock ;
   unsigned int magic ;
   unsigned int owner_cpu ;
   void *owner ;
   struct lockdep_map dep_map ;
};

typedef struct __anonstruct_rwlock_t_39 rwlock_t;

struct __wait_queue;

typedef struct __wait_queue wait_queue_t;

struct __wait_queue {
   unsigned int flags ;
   void *private ;
   int (*func)(wait_queue_t *wait , unsigned int mode , int sync , void *key ) ;
   struct list_head task_list ;
};

struct __wait_queue_head {
   spinlock_t lock ;
   struct list_head task_list ;
};

typedef struct __wait_queue_head wait_queue_head_t;

struct __anonstruct_nodemask_t_41 {
   unsigned long bits[(((unsigned long )(1 << 6) + 8UL * sizeof(long )) - 1UL) / (8UL * sizeof(long ))] ;
};

typedef struct __anonstruct_nodemask_t_41 nodemask_t;

struct mutex {
   atomic_t count ;
   spinlock_t wait_lock ;
   struct list_head wait_list ;
   struct thread_info *owner ;
   char const *name ;
   void *magic ;
   struct lockdep_map dep_map ;
};

struct mutex_waiter {
   struct list_head list ;
   struct task_struct *task ;
   struct mutex *lock ;
   void *magic ;
};

struct rw_semaphore;

struct rw_semaphore {
   __s32 activity ;
   spinlock_t wait_lock ;
   struct list_head wait_list ;
   struct lockdep_map dep_map ;
};

struct file;

struct device;

struct pm_message {
   int event ;
};

typedef struct pm_message pm_message_t;

struct dev_pm_ops {
   int (*prepare)(struct device *dev ) ;
   void (*complete)(struct device *dev ) ;
   int (*suspend)(struct device *dev ) ;
   int (*resume)(struct device *dev ) ;
   int (*freeze)(struct device *dev ) ;
   int (*thaw)(struct device *dev ) ;
   int (*poweroff)(struct device *dev ) ;
   int (*restore)(struct device *dev ) ;
   int (*suspend_noirq)(struct device *dev ) ;
   int (*resume_noirq)(struct device *dev ) ;
   int (*freeze_noirq)(struct device *dev ) ;
   int (*thaw_noirq)(struct device *dev ) ;
   int (*poweroff_noirq)(struct device *dev ) ;
   int (*restore_noirq)(struct device *dev ) ;
};

enum dpm_state {
    DPM_INVALID = 0,
    DPM_ON = 1,
    DPM_PREPARING = 2,
    DPM_RESUMING = 3,
    DPM_SUSPENDING = 4,
    DPM_OFF = 5,
    DPM_OFF_IRQ = 6
} ;

struct dev_pm_info {
   pm_message_t power_state ;
   unsigned int can_wakeup : 1 ;
   unsigned int should_wakeup : 1 ;
   enum dpm_state status ;
   struct list_head entry ;
};

struct __anonstruct_mm_context_t_90 {
   void *ldt ;
   int size ;
   struct mutex lock ;
   void *vdso ;
};

typedef struct __anonstruct_mm_context_t_90 mm_context_t;

struct vm_area_struct;

struct timespec {
   time_t tv_sec ;
   long tv_nsec ;
};

union ktime {
   s64 tv64 ;
};

typedef union ktime ktime_t;

struct tvec_base;

struct timer_list {
   struct list_head entry ;
   unsigned long expires ;
   void (*function)(unsigned long ) ;
   void *data ;
   struct tvec_base *base ;
};

struct hrtimer;

enum hrtimer_restart;

struct work_struct;

struct work_struct {
   atomic_long_t data ;
   struct list_head entry ;
   void (*func)(struct work_struct *work ) ;
   struct lockdep_map lockdep_map ;
};

struct delayed_work {
   struct work_struct work ;
   struct timer_list timer ;
};

struct kobject;

struct attribute {
   char const *name ;
   struct module *owner ;
   mode_t mode ;
};

struct attribute_group {
   char const *name ;
   mode_t (*is_visible)(struct kobject * , struct attribute * , int ) ;
   struct attribute **attrs ;
};

struct sysfs_ops {
   ssize_t (*show)(struct kobject * , struct attribute * , char * ) ;
   ssize_t (*store)(struct kobject * , struct attribute * , char const * , size_t ) ;
};

struct sysfs_dirent;

struct kref {
   atomic_t refcount ;
};

struct kset;

struct kobj_type;

struct kobject {
   char const *name ;
   struct list_head entry ;
   struct kobject *parent ;
   struct kset *kset ;
   struct kobj_type *ktype ;
   struct sysfs_dirent *sd ;
   struct kref kref ;
   unsigned int state_initialized : 1 ;
   unsigned int state_in_sysfs : 1 ;
   unsigned int state_add_uevent_sent : 1 ;
   unsigned int state_remove_uevent_sent : 1 ;
};

struct kobj_type {
   void (*release)(struct kobject *kobj ) ;
   struct sysfs_ops *sysfs_ops ;
   struct attribute **default_attrs ;
};

struct kobj_uevent_env {
   char *envp[32] ;
   int envp_idx ;
   char buf[2048] ;
   int buflen ;
};

struct kset_uevent_ops {
   int (*filter)(struct kset *kset , struct kobject *kobj ) ;
   char const *(*name)(struct kset *kset , struct kobject *kobj ) ;
   int (*uevent)(struct kset *kset , struct kobject *kobj , struct kobj_uevent_env *env ) ;
};

struct kset {
   struct list_head list ;
   spinlock_t list_lock ;
   struct kobject kobj ;
   struct kset_uevent_ops *uevent_ops ;
};

struct kmem_cache_cpu {
   void **freelist ;
   struct page *page ;
   int node ;
   unsigned int offset ;
   unsigned int objsize ;
};

struct kmem_cache_node {
   spinlock_t list_lock ;
   unsigned long nr_partial ;
   unsigned long min_partial ;
   struct list_head partial ;
   atomic_long_t nr_slabs ;
   atomic_long_t total_objects ;
   struct list_head full ;
};

struct kmem_cache_order_objects {
   unsigned long x ;
};

struct kmem_cache {
   unsigned long flags ;
   int size ;
   int objsize ;
   int offset ;
   struct kmem_cache_order_objects oo ;
   struct kmem_cache_node local_node ;
   struct kmem_cache_order_objects max ;
   struct kmem_cache_order_objects min ;
   gfp_t allocflags ;
   int refcount ;
   void (*ctor)(void * ) ;
   int inuse ;
   int align ;
   char const *name ;
   struct list_head list ;
   struct kobject kobj ;
   int remote_node_defrag_ratio ;
   struct kmem_cache_node *node[1 << 6] ;
   struct kmem_cache_cpu *cpu_slab[255] ;
};

struct snd_usb_audio_quirk;

struct usb_device_id {
   __u16 match_flags ;
   __u16 idVendor ;
   __u16 idProduct ;
   __u16 bcdDevice_lo ;
   __u16 bcdDevice_hi ;
   __u8 bDeviceClass ;
   __u8 bDeviceSubClass ;
   __u8 bDeviceProtocol ;
   __u8 bInterfaceClass ;
   __u8 bInterfaceSubClass ;
   __u8 bInterfaceProtocol ;
   struct snd_usb_audio_quirk *driver_info ;
};

struct usb_device_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __le16 bcdUSB ;
   __u8 bDeviceClass ;
   __u8 bDeviceSubClass ;
   __u8 bDeviceProtocol ;
   __u8 bMaxPacketSize0 ;
   __le16 idVendor ;
   __le16 idProduct ;
   __le16 bcdDevice ;
   __u8 iManufacturer ;
   __u8 iProduct ;
   __u8 iSerialNumber ;
   __u8 bNumConfigurations ;
} __attribute__((__packed__)) ;

struct usb_config_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __le16 wTotalLength ;
   __u8 bNumInterfaces ;
   __u8 bConfigurationValue ;
   __u8 iConfiguration ;
   __u8 bmAttributes ;
   __u8 bMaxPower ;
} __attribute__((__packed__)) ;

struct usb_interface_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __u8 bInterfaceNumber ;
   __u8 bAlternateSetting ;
   __u8 bNumEndpoints ;
   __u8 bInterfaceClass ;
   __u8 bInterfaceSubClass ;
   __u8 bInterfaceProtocol ;
   __u8 iInterface ;
} __attribute__((__packed__)) ;

struct usb_endpoint_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __u8 bEndpointAddress ;
   __u8 bmAttributes ;
   __le16 wMaxPacketSize ;
   __u8 bInterval ;
   __u8 bRefresh ;
   __u8 bSynchAddress ;
} __attribute__((__packed__)) ;

struct usb_interface_assoc_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __u8 bFirstInterface ;
   __u8 bInterfaceCount ;
   __u8 bFunctionClass ;
   __u8 bFunctionSubClass ;
   __u8 bFunctionProtocol ;
   __u8 iFunction ;
} __attribute__((__packed__)) ;

enum usb_device_speed {
    USB_SPEED_UNKNOWN = 0,
    USB_SPEED_LOW = 1,
    USB_SPEED_FULL = 2,
    USB_SPEED_HIGH = 3,
    USB_SPEED_VARIABLE = 4
} ;

enum usb_device_state {
    USB_STATE_NOTATTACHED = 0,
    USB_STATE_ATTACHED = 1,
    USB_STATE_POWERED = 2,
    USB_STATE_UNAUTHENTICATED = 3,
    USB_STATE_RECONNECTING = 4,
    USB_STATE_DEFAULT = 5,
    USB_STATE_ADDRESS = 6,
    USB_STATE_CONFIGURED = 7,
    USB_STATE_SUSPENDED = 8
} ;

struct kernel_cap_struct {
   __u32 cap[2] ;
};

typedef struct kernel_cap_struct kernel_cap_t;

struct dentry;

struct rb_node {
   unsigned long rb_parent_color ;
   struct rb_node *rb_right ;
   struct rb_node *rb_left ;
} __attribute__((__aligned__(sizeof(long )))) ;

struct rb_root {
   struct rb_node *rb_node ;
};

struct prio_tree_node;

struct raw_prio_tree_node {
   struct prio_tree_node *left ;
   struct prio_tree_node *right ;
   struct prio_tree_node *parent ;
};

struct prio_tree_node {
   struct prio_tree_node *left ;
   struct prio_tree_node *right ;
   struct prio_tree_node *parent ;
   unsigned long start ;
   unsigned long last ;
};

struct prio_tree_root {
   struct prio_tree_node *prio_tree_node ;
   unsigned short index_bits ;
   unsigned short raw ;
};

struct completion {
   unsigned int done ;
   wait_queue_head_t wait ;
};

struct address_space;

typedef atomic_long_t mm_counter_t;

struct __anonstruct____missing_field_name_99 {
   u16 inuse ;
   u16 objects ;
};

union __anonunion____missing_field_name_98 {
   atomic_t _mapcount ;
   struct __anonstruct____missing_field_name_99 __annonCompField15 ;
};

struct __anonstruct____missing_field_name_101 {
   unsigned long private ;
   struct address_space *mapping ;
};

union __anonunion____missing_field_name_100 {
   struct __anonstruct____missing_field_name_101 __annonCompField17 ;
   spinlock_t ptl ;
   struct kmem_cache *slab ;
   struct page *first_page ;
};

union __anonunion____missing_field_name_102 {
   unsigned long index ;
   void *freelist ;
};

struct page {
   unsigned long flags ;
   atomic_t _count ;
   union __anonunion____missing_field_name_98 __annonCompField16 ;
   union __anonunion____missing_field_name_100 __annonCompField18 ;
   union __anonunion____missing_field_name_102 __annonCompField19 ;
   struct list_head lru ;
};

struct __anonstruct_vm_set_104 {
   struct list_head list ;
   void *parent ;
   struct vm_area_struct *head ;
};

union __anonunion_shared_103 {
   struct __anonstruct_vm_set_104 vm_set ;
   struct raw_prio_tree_node prio_tree_node ;
};

struct anon_vma;

struct vm_operations_struct;

struct mempolicy;

struct vm_area_struct {
   struct mm_struct *vm_mm ;
   unsigned long vm_start ;
   unsigned long vm_end ;
   struct vm_area_struct *vm_next ;
   pgprot_t vm_page_prot ;
   unsigned long vm_flags ;
   struct rb_node vm_rb ;
   union __anonunion_shared_103 shared ;
   struct list_head anon_vma_node ;
   struct anon_vma *anon_vma ;
   struct vm_operations_struct *vm_ops ;
   unsigned long vm_pgoff ;
   struct file *vm_file ;
   void *vm_private_data ;
   unsigned long vm_truncate_count ;
   struct mempolicy *vm_policy ;
};

struct core_thread {
   struct task_struct *task ;
   struct core_thread *next ;
};

struct core_state {
   atomic_t nr_threads ;
   struct core_thread dumper ;
   struct completion startup ;
};

struct mm_struct {
   struct vm_area_struct *mmap ;
   struct rb_root mm_rb ;
   struct vm_area_struct *mmap_cache ;
   unsigned long (*get_unmapped_area)(struct file *filp , unsigned long addr , unsigned long len ,
                                      unsigned long pgoff , unsigned long flags ) ;
   void (*unmap_area)(struct mm_struct *mm , unsigned long addr ) ;
   unsigned long mmap_base ;
   unsigned long task_size ;
   unsigned long cached_hole_size ;
   unsigned long free_area_cache ;
   pgd_t *pgd ;
   atomic_t mm_users ;
   atomic_t mm_count ;
   int map_count ;
   struct rw_semaphore mmap_sem ;
   spinlock_t page_table_lock ;
   struct list_head mmlist ;
   mm_counter_t _file_rss ;
   mm_counter_t _anon_rss ;
   unsigned long hiwater_rss ;
   unsigned long hiwater_vm ;
   unsigned long total_vm ;
   unsigned long locked_vm ;
   unsigned long shared_vm ;
   unsigned long exec_vm ;
   unsigned long stack_vm ;
   unsigned long reserved_vm ;
   unsigned long def_flags ;
   unsigned long nr_ptes ;
   unsigned long start_code ;
   unsigned long end_code ;
   unsigned long start_data ;
   unsigned long end_data ;
   unsigned long start_brk ;
   unsigned long brk ;
   unsigned long start_stack ;
   unsigned long arg_start ;
   unsigned long arg_end ;
   unsigned long env_start ;
   unsigned long env_end ;
   unsigned long saved_auxv[44] ;
   cpumask_t cpu_vm_mask ;
   mm_context_t context ;
   unsigned int faultstamp ;
   unsigned int token_priority ;
   unsigned int last_interval ;
   unsigned long flags ;
   struct core_state *core_state ;
   spinlock_t ioctx_lock ;
   struct hlist_head ioctx_list ;
   struct file *exe_file ;
   unsigned long num_exe_file_vmas ;
};

typedef unsigned long cputime_t;

struct rcu_head {
   struct rcu_head *next ;
   void (*func)(struct rcu_head *head ) ;
};

struct sem_undo_list;

struct sem_undo_list {
   atomic_t refcnt ;
   spinlock_t lock ;
   struct list_head list_proc ;
};

struct sysv_sem {
   struct sem_undo_list *undo_list ;
};

struct siginfo;

struct __anonstruct_sigset_t_105 {
   unsigned long sig[1] ;
};

typedef struct __anonstruct_sigset_t_105 sigset_t;

typedef void __signalfn_t(int );

typedef __signalfn_t *__sighandler_t;

typedef void __restorefn_t(void);

typedef __restorefn_t *__sigrestore_t;

struct sigaction {
   __sighandler_t sa_handler ;
   unsigned long sa_flags ;
   __sigrestore_t sa_restorer ;
   sigset_t sa_mask ;
};

struct k_sigaction {
   struct sigaction sa ;
};

union sigval {
   int sival_int ;
   void *sival_ptr ;
};

typedef union sigval sigval_t;

struct __anonstruct__kill_107 {
   pid_t _pid ;
   uid_t _uid ;
};

struct __anonstruct__timer_108 {
   timer_t _tid ;
   int _overrun ;
   char _pad[sizeof(uid_t ) - sizeof(int )] ;
   sigval_t _sigval ;
   int _sys_private ;
};

struct __anonstruct__rt_109 {
   pid_t _pid ;
   uid_t _uid ;
   sigval_t _sigval ;
};

struct __anonstruct__sigchld_110 {
   pid_t _pid ;
   uid_t _uid ;
   int _status ;
   clock_t _utime ;
   clock_t _stime ;
};

struct __anonstruct__sigfault_111 {
   void *_addr ;
};

struct __anonstruct__sigpoll_112 {
   long _band ;
   int _fd ;
};

union __anonunion__sifields_106 {
   int _pad[(128UL - 4UL * sizeof(int )) / sizeof(int )] ;
   struct __anonstruct__kill_107 _kill ;
   struct __anonstruct__timer_108 _timer ;
   struct __anonstruct__rt_109 _rt ;
   struct __anonstruct__sigchld_110 _sigchld ;
   struct __anonstruct__sigfault_111 _sigfault ;
   struct __anonstruct__sigpoll_112 _sigpoll ;
};

struct siginfo {
   int si_signo ;
   int si_errno ;
   int si_code ;
   union __anonunion__sifields_106 _sifields ;
};

typedef struct siginfo siginfo_t;

struct user_struct;

struct sigpending {
   struct list_head list ;
   sigset_t signal ;
};

struct vfsmount;

struct path {
   struct vfsmount *mnt ;
   struct dentry *dentry ;
};

struct fs_struct {
   atomic_t count ;
   rwlock_t lock ;
   int umask ;
   struct path root ;
   struct path pwd ;
};

enum pid_type {
    PIDTYPE_PID = 0,
    PIDTYPE_PGID = 1,
    PIDTYPE_SID = 2,
    PIDTYPE_MAX = 3
} ;

struct pid_namespace;

struct upid {
   int nr ;
   struct pid_namespace *ns ;
   struct hlist_node pid_chain ;
};

struct pid {
   atomic_t count ;
   unsigned int level ;
   struct hlist_head tasks[3] ;
   struct rcu_head rcu ;
   struct upid numbers[1] ;
};

struct pid_link {
   struct hlist_node node ;
   struct pid *pid ;
};

struct prop_local_single {
   unsigned long events ;
   unsigned long period ;
   int shift ;
   spinlock_t lock ;
};

struct __anonstruct_seccomp_t_115 {

};

typedef struct __anonstruct_seccomp_t_115 seccomp_t;

struct plist_head {
   struct list_head prio_list ;
   struct list_head node_list ;
   spinlock_t *lock ;
};

struct rt_mutex_waiter;

struct rlimit {
   unsigned long rlim_cur ;
   unsigned long rlim_max ;
};

struct hrtimer_clock_base;

struct hrtimer_cpu_base;

enum hrtimer_restart {
    HRTIMER_NORESTART = 0,
    HRTIMER_RESTART = 1
} ;

struct hrtimer {
   struct rb_node node ;
   ktime_t _expires ;
   ktime_t _softexpires ;
   enum hrtimer_restart (*function)(struct hrtimer * ) ;
   struct hrtimer_clock_base *base ;
   unsigned long state ;
   struct list_head cb_entry ;
};

struct hrtimer_clock_base {
   struct hrtimer_cpu_base *cpu_base ;
   clockid_t index ;
   struct rb_root active ;
   struct rb_node *first ;
   ktime_t resolution ;
   ktime_t (*get_time)(void) ;
   ktime_t softirq_time ;
};

struct hrtimer_cpu_base {
   spinlock_t lock ;
   struct hrtimer_clock_base clock_base[2] ;
};

struct task_io_accounting {

};

struct latency_record {
   unsigned long backtrace[12] ;
   unsigned int count ;
   unsigned long time ;
   unsigned long max ;
};

struct nsproxy;

typedef int32_t key_serial_t;

typedef uint32_t key_perm_t;

struct key;

struct seq_file;

struct signal_struct;

struct cred;

struct key_type;

struct keyring_list;

struct key_user;

union __anonunion_type_data_170 {
   struct list_head link ;
   unsigned long x[2] ;
   void *p[2] ;
};

union __anonunion_payload_171 {
   unsigned long value ;
   void *data ;
   struct keyring_list *subscriptions ;
};

struct key {
   atomic_t usage ;
   key_serial_t serial ;
   struct rb_node serial_node ;
   struct key_type *type ;
   struct rw_semaphore sem ;
   struct key_user *user ;
   void *security ;
   time_t expiry ;
   uid_t uid ;
   gid_t gid ;
   key_perm_t perm ;
   unsigned short quotalen ;
   unsigned short datalen ;
   unsigned long flags ;
   char *description ;
   union __anonunion_type_data_170 type_data ;
   union __anonunion_payload_171 payload ;
};

struct inode;

struct group_info {
   atomic_t usage ;
   int ngroups ;
   int nblocks ;
   gid_t small_block[32] ;
   gid_t *blocks[0] ;
};

struct thread_group_cred {
   atomic_t usage ;
   pid_t tgid ;
   spinlock_t lock ;
   struct key *session_keyring ;
   struct key *process_keyring ;
   struct rcu_head rcu ;
};

struct cred {
   atomic_t usage ;
   uid_t uid ;
   gid_t gid ;
   uid_t suid ;
   gid_t sgid ;
   uid_t euid ;
   gid_t egid ;
   uid_t fsuid ;
   gid_t fsgid ;
   unsigned int securebits ;
   kernel_cap_t cap_inheritable ;
   kernel_cap_t cap_permitted ;
   kernel_cap_t cap_effective ;
   kernel_cap_t cap_bset ;
   unsigned char jit_keyring ;
   struct key *thread_keyring ;
   struct key *request_key_auth ;
   struct thread_group_cred *tgcred ;
   void *security ;
   struct user_struct *user ;
   struct group_info *group_info ;
   struct rcu_head rcu ;
};

struct futex_pi_state;

struct robust_list_head;

struct bio;

struct user_namespace;

struct io_event {
   __u64 data ;
   __u64 obj ;
   __s64 res ;
   __s64 res2 ;
};

struct iovec {
   void *iov_base ;
   __kernel_size_t iov_len ;
};

struct kioctx;

union __anonunion_ki_obj_173 {
   void *user ;
   struct task_struct *tsk ;
};

struct kiocb {
   struct list_head ki_run_list ;
   unsigned long ki_flags ;
   int ki_users ;
   unsigned int ki_key ;
   struct file *ki_filp ;
   struct kioctx *ki_ctx ;
   int (*ki_cancel)(struct kiocb * , struct io_event * ) ;
   ssize_t (*ki_retry)(struct kiocb * ) ;
   void (*ki_dtor)(struct kiocb * ) ;
   union __anonunion_ki_obj_173 ki_obj ;
   __u64 ki_user_data ;
   wait_queue_t ki_wait ;
   loff_t ki_pos ;
   void *private ;
   unsigned short ki_opcode ;
   size_t ki_nbytes ;
   char *ki_buf ;
   size_t ki_left ;
   struct iovec ki_inline_vec ;
   struct iovec *ki_iovec ;
   unsigned long ki_nr_segs ;
   unsigned long ki_cur_seg ;
   struct list_head ki_list ;
   struct file *ki_eventfd ;
};

struct aio_ring_info {
   unsigned long mmap_base ;
   unsigned long mmap_size ;
   struct page **ring_pages ;
   spinlock_t ring_lock ;
   long nr_pages ;
   unsigned int nr ;
   unsigned int tail ;
   struct page *internal_pages[8] ;
};

struct kioctx {
   atomic_t users ;
   int dead ;
   struct mm_struct *mm ;
   unsigned long user_id ;
   struct hlist_node list ;
   wait_queue_head_t wait ;
   spinlock_t ctx_lock ;
   int reqs_active ;
   struct list_head active_reqs ;
   struct list_head run_list ;
   unsigned int max_reqs ;
   struct aio_ring_info ring_info ;
   struct delayed_work wq ;
   struct rcu_head rcu_head ;
};

struct sighand_struct {
   atomic_t count ;
   struct k_sigaction action[64] ;
   spinlock_t siglock ;
   wait_queue_head_t signalfd_wqh ;
};

struct pacct_struct {
   int ac_flag ;
   long ac_exitcode ;
   unsigned long ac_mem ;
   cputime_t ac_utime ;
   cputime_t ac_stime ;
   unsigned long ac_minflt ;
   unsigned long ac_majflt ;
};

struct task_cputime {
   cputime_t utime ;
   cputime_t stime ;
   unsigned long long sum_exec_runtime ;
};

struct thread_group_cputimer {
   struct task_cputime cputime ;
   int running ;
   spinlock_t lock ;
};

union __anonunion____missing_field_name_174 {
   pid_t pgrp __attribute__((__deprecated__)) ;
   pid_t __pgrp ;
};

union __anonunion____missing_field_name_175 {
   pid_t session __attribute__((__deprecated__)) ;
   pid_t __session ;
};

struct tty_struct;

struct taskstats;

struct tty_audit_buf;

struct signal_struct {
   atomic_t count ;
   atomic_t live ;
   wait_queue_head_t wait_chldexit ;
   struct task_struct *curr_target ;
   struct sigpending shared_pending ;
   int group_exit_code ;
   int notify_count ;
   struct task_struct *group_exit_task ;
   int group_stop_count ;
   unsigned int flags ;
   struct list_head posix_timers ;
   struct hrtimer real_timer ;
   struct pid *leader_pid ;
   ktime_t it_real_incr ;
   cputime_t it_prof_expires ;
   cputime_t it_virt_expires ;
   cputime_t it_prof_incr ;
   cputime_t it_virt_incr ;
   struct thread_group_cputimer cputimer ;
   struct task_cputime cputime_expires ;
   struct list_head cpu_timers[3] ;
   union __anonunion____missing_field_name_174 __annonCompField20 ;
   struct pid *tty_old_pgrp ;
   union __anonunion____missing_field_name_175 __annonCompField21 ;
   int leader ;
   struct tty_struct *tty ;
   cputime_t utime ;
   cputime_t stime ;
   cputime_t cutime ;
   cputime_t cstime ;
   cputime_t gtime ;
   cputime_t cgtime ;
   unsigned long nvcsw ;
   unsigned long nivcsw ;
   unsigned long cnvcsw ;
   unsigned long cnivcsw ;
   unsigned long min_flt ;
   unsigned long maj_flt ;
   unsigned long cmin_flt ;
   unsigned long cmaj_flt ;
   unsigned long inblock ;
   unsigned long oublock ;
   unsigned long cinblock ;
   unsigned long coublock ;
   struct task_io_accounting ioac ;
   unsigned long long sum_sched_runtime ;
   struct rlimit rlim[16] ;
   struct pacct_struct pacct ;
   struct taskstats *stats ;
   unsigned int audit_tty ;
   struct tty_audit_buf *tty_audit_buf ;
};

struct user_struct {
   atomic_t __count ;
   atomic_t processes ;
   atomic_t files ;
   atomic_t sigpending ;
   atomic_t inotify_watches ;
   atomic_t inotify_devs ;
   atomic_t epoll_watches ;
   unsigned long mq_bytes ;
   unsigned long locked_shm ;
   struct key *uid_keyring ;
   struct key *session_keyring ;
   struct hlist_node uidhash_node ;
   uid_t uid ;
   struct user_namespace *user_ns ;
};

struct backing_dev_info;

struct reclaim_state;

struct sched_info {
   unsigned long pcount ;
   unsigned long long run_delay ;
   unsigned long long last_arrival ;
   unsigned long long last_queued ;
   unsigned int bkl_count ;
};

struct task_delay_info {
   spinlock_t lock ;
   unsigned int flags ;
   struct timespec blkio_start ;
   struct timespec blkio_end ;
   u64 blkio_delay ;
   u64 swapin_delay ;
   u32 blkio_count ;
   u32 swapin_count ;
   struct timespec freepages_start ;
   struct timespec freepages_end ;
   u64 freepages_delay ;
   u32 freepages_count ;
};

enum cpu_idle_type {
    CPU_IDLE = 0,
    CPU_NOT_IDLE = 1,
    CPU_NEWLY_IDLE = 2,
    CPU_MAX_IDLE_TYPES = 3
} ;

struct sched_group {
   struct sched_group *next ;
   unsigned int __cpu_power ;
   u32 reciprocal_cpu_power ;
   unsigned long cpumask[] ;
};

enum sched_domain_level {
    SD_LV_NONE = 0,
    SD_LV_SIBLING = 1,
    SD_LV_MC = 2,
    SD_LV_CPU = 3,
    SD_LV_NODE = 4,
    SD_LV_ALLNODES = 5,
    SD_LV_MAX = 6
} ;

struct sched_domain {
   struct sched_domain *parent ;
   struct sched_domain *child ;
   struct sched_group *groups ;
   unsigned long min_interval ;
   unsigned long max_interval ;
   unsigned int busy_factor ;
   unsigned int imbalance_pct ;
   unsigned int cache_nice_tries ;
   unsigned int busy_idx ;
   unsigned int idle_idx ;
   unsigned int newidle_idx ;
   unsigned int wake_idx ;
   unsigned int forkexec_idx ;
   int flags ;
   enum sched_domain_level level ;
   unsigned long last_balance ;
   unsigned int balance_interval ;
   unsigned int nr_balance_failed ;
   u64 last_update ;
   unsigned int lb_count[3] ;
   unsigned int lb_failed[3] ;
   unsigned int lb_balanced[3] ;
   unsigned int lb_imbalance[3] ;
   unsigned int lb_gained[3] ;
   unsigned int lb_hot_gained[3] ;
   unsigned int lb_nobusyg[3] ;
   unsigned int lb_nobusyq[3] ;
   unsigned int alb_count ;
   unsigned int alb_failed ;
   unsigned int alb_pushed ;
   unsigned int sbe_count ;
   unsigned int sbe_balanced ;
   unsigned int sbe_pushed ;
   unsigned int sbf_count ;
   unsigned int sbf_balanced ;
   unsigned int sbf_pushed ;
   unsigned int ttwu_wake_remote ;
   unsigned int ttwu_move_affine ;
   unsigned int ttwu_move_balance ;
   char *name ;
   unsigned long span[] ;
};

struct io_context;

struct audit_context;

struct pipe_inode_info;

struct rq;

struct sched_class {
   struct sched_class const *next ;
   void (*enqueue_task)(struct rq *rq , struct task_struct *p , int wakeup ) ;
   void (*dequeue_task)(struct rq *rq , struct task_struct *p , int sleep ) ;
   void (*yield_task)(struct rq *rq ) ;
   void (*check_preempt_curr)(struct rq *rq , struct task_struct *p , int sync ) ;
   struct task_struct *(*pick_next_task)(struct rq *rq ) ;
   void (*put_prev_task)(struct rq *rq , struct task_struct *p ) ;
   int (*select_task_rq)(struct task_struct *p , int sync ) ;
   unsigned long (*load_balance)(struct rq *this_rq , int this_cpu , struct rq *busiest ,
                                 unsigned long max_load_move , struct sched_domain *sd ,
                                 enum cpu_idle_type idle , int *all_pinned , int *this_best_prio ) ;
   int (*move_one_task)(struct rq *this_rq , int this_cpu , struct rq *busiest , struct sched_domain *sd ,
                        enum cpu_idle_type idle ) ;
   void (*pre_schedule)(struct rq *this_rq , struct task_struct *task ) ;
   void (*post_schedule)(struct rq *this_rq ) ;
   void (*task_wake_up)(struct rq *this_rq , struct task_struct *task ) ;
   void (*set_cpus_allowed)(struct task_struct *p , struct cpumask const *newmask ) ;
   void (*rq_online)(struct rq *rq ) ;
   void (*rq_offline)(struct rq *rq ) ;
   void (*set_curr_task)(struct rq *rq ) ;
   void (*task_tick)(struct rq *rq , struct task_struct *p , int queued ) ;
   void (*task_new)(struct rq *rq , struct task_struct *p ) ;
   void (*switched_from)(struct rq *this_rq , struct task_struct *task , int running ) ;
   void (*switched_to)(struct rq *this_rq , struct task_struct *task , int running ) ;
   void (*prio_changed)(struct rq *this_rq , struct task_struct *task , int oldprio ,
                        int running ) ;
};

struct load_weight {
   unsigned long weight ;
   unsigned long inv_weight ;
};

struct sched_entity {
   struct load_weight load ;
   struct rb_node run_node ;
   struct list_head group_node ;
   unsigned int on_rq ;
   u64 exec_start ;
   u64 sum_exec_runtime ;
   u64 vruntime ;
   u64 prev_sum_exec_runtime ;
   u64 last_wakeup ;
   u64 avg_overlap ;
   u64 wait_start ;
   u64 wait_max ;
   u64 wait_count ;
   u64 wait_sum ;
   u64 sleep_start ;
   u64 sleep_max ;
   s64 sum_sleep_runtime ;
   u64 block_start ;
   u64 block_max ;
   u64 exec_max ;
   u64 slice_max ;
   u64 nr_migrations ;
   u64 nr_migrations_cold ;
   u64 nr_failed_migrations_affine ;
   u64 nr_failed_migrations_running ;
   u64 nr_failed_migrations_hot ;
   u64 nr_forced_migrations ;
   u64 nr_forced2_migrations ;
   u64 nr_wakeups ;
   u64 nr_wakeups_sync ;
   u64 nr_wakeups_migrate ;
   u64 nr_wakeups_local ;
   u64 nr_wakeups_remote ;
   u64 nr_wakeups_affine ;
   u64 nr_wakeups_affine_attempts ;
   u64 nr_wakeups_passive ;
   u64 nr_wakeups_idle ;
};

struct sched_rt_entity {
   struct list_head run_list ;
   unsigned long timeout ;
   unsigned int time_slice ;
   int nr_cpus_allowed ;
   struct sched_rt_entity *back ;
};

struct linux_binfmt;

struct files_struct;

struct compat_robust_list_head;

struct task_struct {
   long volatile state ;
   void *stack ;
   atomic_t usage ;
   unsigned int flags ;
   unsigned int ptrace ;
   int lock_depth ;
   int prio ;
   int static_prio ;
   int normal_prio ;
   unsigned int rt_priority ;
   struct sched_class const *sched_class ;
   struct sched_entity se ;
   struct sched_rt_entity rt ;
   unsigned char fpu_counter ;
   s8 oomkilladj ;
   unsigned int btrace_seq ;
   unsigned int policy ;
   cpumask_t cpus_allowed ;
   struct sched_info sched_info ;
   struct list_head tasks ;
   struct mm_struct *mm ;
   struct mm_struct *active_mm ;
   struct linux_binfmt *binfmt ;
   int exit_state ;
   int exit_code ;
   int exit_signal ;
   int pdeath_signal ;
   unsigned int personality ;
   unsigned int did_exec : 1 ;
   pid_t pid ;
   pid_t tgid ;
   struct task_struct *real_parent ;
   struct task_struct *parent ;
   struct list_head children ;
   struct list_head sibling ;
   struct task_struct *group_leader ;
   struct list_head ptraced ;
   struct list_head ptrace_entry ;
   struct bts_tracer *bts ;
   void *bts_buffer ;
   size_t bts_size ;
   struct pid_link pids[3] ;
   struct list_head thread_group ;
   struct completion *vfork_done ;
   int *set_child_tid ;
   int *clear_child_tid ;
   cputime_t utime ;
   cputime_t stime ;
   cputime_t utimescaled ;
   cputime_t stimescaled ;
   cputime_t gtime ;
   cputime_t prev_utime ;
   cputime_t prev_stime ;
   unsigned long nvcsw ;
   unsigned long nivcsw ;
   struct timespec start_time ;
   struct timespec real_start_time ;
   unsigned long min_flt ;
   unsigned long maj_flt ;
   struct task_cputime cputime_expires ;
   struct list_head cpu_timers[3] ;
   struct cred const *real_cred ;
   struct cred const *cred ;
   struct mutex cred_exec_mutex ;
   char comm[16] ;
   int link_count ;
   int total_link_count ;
   struct sysv_sem sysvsem ;
   unsigned long last_switch_timestamp ;
   unsigned long last_switch_count ;
   struct thread_struct thread ;
   struct fs_struct *fs ;
   struct files_struct *files ;
   struct nsproxy *nsproxy ;
   struct signal_struct *signal ;
   struct sighand_struct *sighand ;
   sigset_t blocked ;
   sigset_t real_blocked ;
   sigset_t saved_sigmask ;
   struct sigpending pending ;
   unsigned long sas_ss_sp ;
   size_t sas_ss_size ;
   int (*notifier)(void *priv ) ;
   void *notifier_data ;
   sigset_t *notifier_mask ;
   struct audit_context *audit_context ;
   uid_t loginuid ;
   unsigned int sessionid ;
   seccomp_t seccomp ;
   u32 parent_exec_id ;
   u32 self_exec_id ;
   spinlock_t alloc_lock ;
   spinlock_t pi_lock ;
   struct plist_head pi_waiters ;
   struct rt_mutex_waiter *pi_blocked_on ;
   struct mutex_waiter *blocked_on ;
   u64 curr_chain_key ;
   int lockdep_depth ;
   unsigned int lockdep_recursion ;
   struct held_lock held_locks[48UL] ;
   void *journal_info ;
   struct bio *bio_list ;
   struct bio **bio_tail ;
   struct reclaim_state *reclaim_state ;
   struct backing_dev_info *backing_dev_info ;
   struct io_context *io_context ;
   unsigned long ptrace_message ;
   siginfo_t *last_siginfo ;
   struct task_io_accounting ioac ;
   struct robust_list_head *robust_list ;
   struct compat_robust_list_head *compat_robust_list ;
   struct list_head pi_state_list ;
   struct futex_pi_state *pi_state_cache ;
   struct mempolicy *mempolicy ;
   short il_next ;
   atomic_t fs_excl ;
   struct rcu_head rcu ;
   struct pipe_inode_info *splice_pipe ;
   struct task_delay_info *delays ;
   int make_it_fail ;
   struct prop_local_single dirties ;
   int latency_record_count ;
   struct latency_record latency_record[32] ;
   unsigned long timer_slack_ns ;
   unsigned long default_timer_slack_ns ;
   struct list_head *scm_work_list ;
   unsigned long trace ;
};

struct proc_dir_entry;

struct klist_node;

struct klist {
   spinlock_t k_lock ;
   struct list_head k_list ;
   void (*get)(struct klist_node * ) ;
   void (*put)(struct klist_node * ) ;
} __attribute__((__aligned__(4))) ;

struct klist_node {
   void *n_klist ;
   struct list_head n_node ;
   struct kref n_ref ;
};

struct kstat {
   u64 ino ;
   dev_t dev ;
   umode_t mode ;
   unsigned int nlink ;
   uid_t uid ;
   gid_t gid ;
   dev_t rdev ;
   loff_t size ;
   struct timespec atime ;
   struct timespec mtime ;
   struct timespec ctime ;
   unsigned long blksize ;
   unsigned long long blocks ;
};

typedef __u64 Elf64_Addr;

typedef __u16 Elf64_Half;

typedef __u32 Elf64_Word;

typedef __u64 Elf64_Xword;

struct elf64_sym {
   Elf64_Word st_name ;
   unsigned char st_info ;
   unsigned char st_other ;
   Elf64_Half st_shndx ;
   Elf64_Addr st_value ;
   Elf64_Xword st_size ;
};

typedef struct elf64_sym Elf64_Sym;

struct kernel_param;

struct kparam_string;

struct kparam_array;

union __anonunion____missing_field_name_182 {
   void *arg ;
   struct kparam_string const *str ;
   struct kparam_array const *arr ;
};

struct kernel_param {
   char const *name ;
   unsigned int perm ;
   int (*set)(char const *val , struct kernel_param *kp ) ;
   int (*get)(char *buffer , struct kernel_param *kp ) ;
   union __anonunion____missing_field_name_182 __annonCompField22 ;
};

struct kparam_string {
   unsigned int maxlen ;
   char *string ;
};

struct kparam_array {
   unsigned int max ;
   unsigned int *num ;
   int (*set)(char const *val , struct kernel_param *kp ) ;
   int (*get)(char *buffer , struct kernel_param *kp ) ;
   unsigned int elemsize ;
   void *elem ;
};

struct marker;

typedef void marker_probe_func(void *probe_private , void *call_private , char const *fmt ,
                               va_list *args );

struct marker_probe_closure {
   marker_probe_func *func ;
   void *probe_private ;
};

struct marker {
   char const *name ;
   char const *format ;
   char state ;
   char ptype ;
   void (*call)(struct marker const *mdata , void *call_private , ...) ;
   struct marker_probe_closure single ;
   struct marker_probe_closure *multi ;
   char const *tp_name ;
   void *tp_cb ;
} __attribute__((__aligned__(8))) ;

struct tracepoint;

struct tracepoint {
   char const *name ;
   int state ;
   void **funcs ;
} __attribute__((__aligned__(32))) ;

struct mod_arch_specific {

};

struct kernel_symbol {
   unsigned long value ;
   char const *name ;
};

struct module_attribute {
   struct attribute attr ;
   ssize_t (*show)(struct module_attribute * , struct module * , char * ) ;
   ssize_t (*store)(struct module_attribute * , struct module * , char const * ,
                    size_t count ) ;
   void (*setup)(struct module * , char const * ) ;
   int (*test)(struct module * ) ;
   void (*free)(struct module * ) ;
};

struct module_param_attrs;

struct module_kobject {
   struct kobject kobj ;
   struct module *mod ;
   struct kobject *drivers_dir ;
   struct module_param_attrs *mp ;
};

struct exception_table_entry;

enum module_state {
    MODULE_STATE_LIVE = 0,
    MODULE_STATE_COMING = 1,
    MODULE_STATE_GOING = 2
} ;

struct module_sect_attrs;

struct module_notes_attrs;

struct module {
   enum module_state state ;
   struct list_head list ;
   char name[64UL - sizeof(unsigned long )] ;
   struct module_kobject mkobj ;
   struct module_attribute *modinfo_attrs ;
   char const *version ;
   char const *srcversion ;
   struct kobject *holders_dir ;
   struct kernel_symbol const *syms ;
   unsigned long const *crcs ;
   unsigned int num_syms ;
   unsigned int num_gpl_syms ;
   struct kernel_symbol const *gpl_syms ;
   unsigned long const *gpl_crcs ;
   struct kernel_symbol const *gpl_future_syms ;
   unsigned long const *gpl_future_crcs ;
   unsigned int num_gpl_future_syms ;
   unsigned int num_exentries ;
   struct exception_table_entry *extable ;
   int (*init)(void) ;
   void *module_init ;
   void *module_core ;
   unsigned int init_size ;
   unsigned int core_size ;
   unsigned int init_text_size ;
   unsigned int core_text_size ;
   struct mod_arch_specific arch ;
   unsigned int taints ;
   unsigned int num_bugs ;
   struct list_head bug_list ;
   struct bug_entry *bug_table ;
   Elf64_Sym *symtab ;
   unsigned int num_symtab ;
   char *strtab ;
   struct module_sect_attrs *sect_attrs ;
   struct module_notes_attrs *notes_attrs ;
   void *percpu ;
   char *args ;
   struct marker *markers ;
   unsigned int num_markers ;
   struct tracepoint *tracepoints ;
   unsigned int num_tracepoints ;
   struct list_head modules_which_use_me ;
   struct task_struct *waiter ;
   void (*exit)(void) ;
   char *refptr ;
};

struct device_driver;

struct semaphore {
   spinlock_t lock ;
   unsigned int count ;
   struct list_head wait_list ;
};

struct dma_mapping_ops;

struct dev_archdata {
   void *acpi_handle ;
   struct dma_mapping_ops *dma_ops ;
   void *iommu ;
};

struct driver_private;

struct class;

struct class_private;

struct bus_type;

struct bus_type_private;

struct bus_attribute {
   struct attribute attr ;
   ssize_t (*show)(struct bus_type *bus , char *buf ) ;
   ssize_t (*store)(struct bus_type *bus , char const *buf , size_t count ) ;
};

struct device_attribute;

struct driver_attribute;

struct bus_type {
   char const *name ;
   struct bus_attribute *bus_attrs ;
   struct device_attribute *dev_attrs ;
   struct driver_attribute *drv_attrs ;
   int (*match)(struct device *dev , struct device_driver *drv ) ;
   int (*uevent)(struct device *dev , struct kobj_uevent_env *env ) ;
   int (*probe)(struct device *dev ) ;
   int (*remove)(struct device *dev ) ;
   void (*shutdown)(struct device *dev ) ;
   int (*suspend)(struct device *dev , pm_message_t state ) ;
   int (*suspend_late)(struct device *dev , pm_message_t state ) ;
   int (*resume_early)(struct device *dev ) ;
   int (*resume)(struct device *dev ) ;
   struct dev_pm_ops *pm ;
   struct bus_type_private *p ;
};

struct device_driver {
   char const *name ;
   struct bus_type *bus ;
   struct module *owner ;
   char const *mod_name ;
   int (*probe)(struct device *dev ) ;
   int (*remove)(struct device *dev ) ;
   void (*shutdown)(struct device *dev ) ;
   int (*suspend)(struct device *dev , pm_message_t state ) ;
   int (*resume)(struct device *dev ) ;
   struct attribute_group **groups ;
   struct dev_pm_ops *pm ;
   struct driver_private *p ;
};

struct driver_attribute {
   struct attribute attr ;
   ssize_t (*show)(struct device_driver *driver , char *buf ) ;
   ssize_t (*store)(struct device_driver *driver , char const *buf , size_t count ) ;
};

struct class_attribute;

struct class {
   char const *name ;
   struct module *owner ;
   struct class_attribute *class_attrs ;
   struct device_attribute *dev_attrs ;
   struct kobject *dev_kobj ;
   int (*dev_uevent)(struct device *dev , struct kobj_uevent_env *env ) ;
   void (*class_release)(struct class *class ) ;
   void (*dev_release)(struct device *dev ) ;
   int (*suspend)(struct device *dev , pm_message_t state ) ;
   int (*resume)(struct device *dev ) ;
   struct dev_pm_ops *pm ;
   struct class_private *p ;
};

struct device_type;

struct class_attribute {
   struct attribute attr ;
   ssize_t (*show)(struct class *class , char *buf ) ;
   ssize_t (*store)(struct class *class , char const *buf , size_t count ) ;
};

struct device_type {
   char const *name ;
   struct attribute_group **groups ;
   int (*uevent)(struct device *dev , struct kobj_uevent_env *env ) ;
   void (*release)(struct device *dev ) ;
   int (*suspend)(struct device *dev , pm_message_t state ) ;
   int (*resume)(struct device *dev ) ;
   struct dev_pm_ops *pm ;
};

struct device_attribute {
   struct attribute attr ;
   ssize_t (*show)(struct device *dev , struct device_attribute *attr , char *buf ) ;
   ssize_t (*store)(struct device *dev , struct device_attribute *attr , char const *buf ,
                    size_t count ) ;
};

struct device_dma_parameters {
   unsigned int max_segment_size ;
   unsigned long segment_boundary_mask ;
};

struct dma_coherent_mem;

struct device {
   struct klist klist_children ;
   struct klist_node knode_parent ;
   struct klist_node knode_driver ;
   struct klist_node knode_bus ;
   struct device * __attribute__((__recursive__)) parent ;
   struct kobject kobj ;
   char bus_id[20] ;
   unsigned int uevent_suppress : 1 ;
   char const *init_name ;
   struct device_type *type ;
   struct semaphore sem ;
   struct bus_type *bus ;
   struct device_driver *driver ;
   void *driver_data ;
   void *platform_data ;
   struct dev_pm_info power ;
   int numa_node ;
   u64 *dma_mask ;
   u64 coherent_dma_mask ;
   struct device_dma_parameters *dma_parms ;
   struct list_head dma_pools ;
   struct dma_coherent_mem *dma_mem ;
   struct dev_archdata archdata ;
   dev_t devt ;
   spinlock_t devres_lock ;
   struct list_head devres_head ;
   struct klist_node knode_class ;
   struct class *class ;
   struct attribute_group **groups ;
   void (*release)(struct device *dev ) ;
};

struct nameidata;

struct qstr {
   unsigned int hash ;
   unsigned int len ;
   unsigned char const *name ;
};

union __anonunion_d_u_184 {
   struct list_head d_child ;
   struct rcu_head d_rcu ;
};

struct dentry_operations;

struct super_block;

struct dentry {
   atomic_t d_count ;
   unsigned int d_flags ;
   spinlock_t d_lock ;
   int d_mounted ;
   struct inode *d_inode ;
   struct hlist_node d_hash ;
   struct dentry *d_parent ;
   struct qstr d_name ;
   struct list_head d_lru ;
   union __anonunion_d_u_184 d_u ;
   struct list_head d_subdirs ;
   struct list_head d_alias ;
   unsigned long d_time ;
   struct dentry_operations *d_op ;
   struct super_block *d_sb ;
   void *d_fsdata ;
   unsigned char d_iname[32] ;
};

struct dentry_operations {
   int (*d_revalidate)(struct dentry * , struct nameidata * ) ;
   int (*d_hash)(struct dentry * , struct qstr * ) ;
   int (*d_compare)(struct dentry * , struct qstr * , struct qstr * ) ;
   int (*d_delete)(struct dentry * ) ;
   void (*d_release)(struct dentry * ) ;
   void (*d_iput)(struct dentry * , struct inode * ) ;
   char *(*d_dname)(struct dentry * , char * , int ) ;
};

struct radix_tree_node;

struct radix_tree_root {
   unsigned int height ;
   gfp_t gfp_mask ;
   struct radix_tree_node *rnode ;
};

struct fiemap_extent {
   __u64 fe_logical ;
   __u64 fe_physical ;
   __u64 fe_length ;
   __u64 fe_reserved64[2] ;
   __u32 fe_flags ;
   __u32 fe_reserved[3] ;
};

struct export_operations;

struct poll_table_struct;

struct kstatfs;

struct iattr {
   unsigned int ia_valid ;
   umode_t ia_mode ;
   uid_t ia_uid ;
   gid_t ia_gid ;
   loff_t ia_size ;
   struct timespec ia_atime ;
   struct timespec ia_mtime ;
   struct timespec ia_ctime ;
   struct file *ia_file ;
};

struct if_dqblk {
   __u64 dqb_bhardlimit ;
   __u64 dqb_bsoftlimit ;
   __u64 dqb_curspace ;
   __u64 dqb_ihardlimit ;
   __u64 dqb_isoftlimit ;
   __u64 dqb_curinodes ;
   __u64 dqb_btime ;
   __u64 dqb_itime ;
   __u32 dqb_valid ;
};

struct if_dqinfo {
   __u64 dqi_bgrace ;
   __u64 dqi_igrace ;
   __u32 dqi_flags ;
   __u32 dqi_valid ;
};

struct fs_disk_quota {
   __s8 d_version ;
   __s8 d_flags ;
   __u16 d_fieldmask ;
   __u32 d_id ;
   __u64 d_blk_hardlimit ;
   __u64 d_blk_softlimit ;
   __u64 d_ino_hardlimit ;
   __u64 d_ino_softlimit ;
   __u64 d_bcount ;
   __u64 d_icount ;
   __s32 d_itimer ;
   __s32 d_btimer ;
   __u16 d_iwarns ;
   __u16 d_bwarns ;
   __s32 d_padding2 ;
   __u64 d_rtb_hardlimit ;
   __u64 d_rtb_softlimit ;
   __u64 d_rtbcount ;
   __s32 d_rtbtimer ;
   __u16 d_rtbwarns ;
   __s16 d_padding3 ;
   char d_padding4[8] ;
};

struct fs_qfilestat {
   __u64 qfs_ino ;
   __u64 qfs_nblks ;
   __u32 qfs_nextents ;
};

typedef struct fs_qfilestat fs_qfilestat_t;

struct fs_quota_stat {
   __s8 qs_version ;
   __u16 qs_flags ;
   __s8 qs_pad ;
   fs_qfilestat_t qs_uquota ;
   fs_qfilestat_t qs_gquota ;
   __u32 qs_incoredqs ;
   __s32 qs_btimelimit ;
   __s32 qs_itimelimit ;
   __s32 qs_rtbtimelimit ;
   __u16 qs_bwarnlimit ;
   __u16 qs_iwarnlimit ;
};

struct dquot;

typedef __kernel_uid32_t qid_t;

typedef long long qsize_t;

struct mem_dqblk {
   qsize_t dqb_bhardlimit ;
   qsize_t dqb_bsoftlimit ;
   qsize_t dqb_curspace ;
   qsize_t dqb_ihardlimit ;
   qsize_t dqb_isoftlimit ;
   qsize_t dqb_curinodes ;
   time_t dqb_btime ;
   time_t dqb_itime ;
};

struct quota_format_type;

struct mem_dqinfo {
   struct quota_format_type *dqi_format ;
   int dqi_fmt_id ;
   struct list_head dqi_dirty_list ;
   unsigned long dqi_flags ;
   unsigned int dqi_bgrace ;
   unsigned int dqi_igrace ;
   qsize_t dqi_maxblimit ;
   qsize_t dqi_maxilimit ;
   void *dqi_priv ;
};

struct dquot {
   struct hlist_node dq_hash ;
   struct list_head dq_inuse ;
   struct list_head dq_free ;
   struct list_head dq_dirty ;
   struct mutex dq_lock ;
   atomic_t dq_count ;
   wait_queue_head_t dq_wait_unused ;
   struct super_block *dq_sb ;
   unsigned int dq_id ;
   loff_t dq_off ;
   unsigned long dq_flags ;
   short dq_type ;
   struct mem_dqblk dq_dqb ;
};

struct quota_format_ops {
   int (*check_quota_file)(struct super_block *sb , int type ) ;
   int (*read_file_info)(struct super_block *sb , int type ) ;
   int (*write_file_info)(struct super_block *sb , int type ) ;
   int (*free_file_info)(struct super_block *sb , int type ) ;
   int (*read_dqblk)(struct dquot *dquot ) ;
   int (*commit_dqblk)(struct dquot *dquot ) ;
   int (*release_dqblk)(struct dquot *dquot ) ;
};

struct dquot_operations {
   int (*initialize)(struct inode * , int ) ;
   int (*drop)(struct inode * ) ;
   int (*alloc_space)(struct inode * , qsize_t , int ) ;
   int (*alloc_inode)(struct inode const * , qsize_t ) ;
   int (*free_space)(struct inode * , qsize_t ) ;
   int (*free_inode)(struct inode const * , qsize_t ) ;
   int (*transfer)(struct inode * , struct iattr * ) ;
   int (*write_dquot)(struct dquot * ) ;
   struct dquot *(*alloc_dquot)(struct super_block * , int ) ;
   void (*destroy_dquot)(struct dquot * ) ;
   int (*acquire_dquot)(struct dquot * ) ;
   int (*release_dquot)(struct dquot * ) ;
   int (*mark_dirty)(struct dquot * ) ;
   int (*write_info)(struct super_block * , int ) ;
};

struct quotactl_ops {
   int (*quota_on)(struct super_block * , int , int , char * , int ) ;
   int (*quota_off)(struct super_block * , int , int ) ;
   int (*quota_sync)(struct super_block * , int ) ;
   int (*get_info)(struct super_block * , int , struct if_dqinfo * ) ;
   int (*set_info)(struct super_block * , int , struct if_dqinfo * ) ;
   int (*get_dqblk)(struct super_block * , int , qid_t , struct if_dqblk * ) ;
   int (*set_dqblk)(struct super_block * , int , qid_t , struct if_dqblk * ) ;
   int (*get_xstate)(struct super_block * , struct fs_quota_stat * ) ;
   int (*set_xstate)(struct super_block * , unsigned int , int ) ;
   int (*get_xquota)(struct super_block * , int , qid_t , struct fs_disk_quota * ) ;
   int (*set_xquota)(struct super_block * , int , qid_t , struct fs_disk_quota * ) ;
};

struct quota_format_type {
   int qf_fmt_id ;
   struct quota_format_ops *qf_ops ;
   struct module *qf_owner ;
   struct quota_format_type *qf_next ;
};

struct quota_info {
   unsigned int flags ;
   struct mutex dqio_mutex ;
   struct mutex dqonoff_mutex ;
   struct rw_semaphore dqptr_sem ;
   struct inode *files[2] ;
   struct mem_dqinfo info[2] ;
   struct quota_format_ops *ops[2] ;
};

struct writeback_control;

union __anonunion_arg_190 {
   char *buf ;
   void *data ;
};

struct __anonstruct_read_descriptor_t_189 {
   size_t written ;
   size_t count ;
   union __anonunion_arg_190 arg ;
   int error ;
};

typedef struct __anonstruct_read_descriptor_t_189 read_descriptor_t;

struct address_space_operations {
   int (*writepage)(struct page *page , struct writeback_control *wbc ) ;
   int (*readpage)(struct file * , struct page * ) ;
   void (*sync_page)(struct page * ) ;
   int (*writepages)(struct address_space * , struct writeback_control * ) ;
   int (*set_page_dirty)(struct page *page ) ;
   int (*readpages)(struct file *filp , struct address_space *mapping , struct list_head *pages ,
                    unsigned int nr_pages ) ;
   int (*write_begin)(struct file * , struct address_space *mapping , loff_t pos ,
                      unsigned int len , unsigned int flags , struct page **pagep ,
                      void **fsdata ) ;
   int (*write_end)(struct file * , struct address_space *mapping , loff_t pos , unsigned int len ,
                    unsigned int copied , struct page *page , void *fsdata ) ;
   sector_t (*bmap)(struct address_space * , sector_t ) ;
   void (*invalidatepage)(struct page * , unsigned long ) ;
   int (*releasepage)(struct page * , gfp_t ) ;
   ssize_t (*direct_IO)(int , struct kiocb * , struct iovec const *iov , loff_t offset ,
                        unsigned long nr_segs ) ;
   int (*get_xip_mem)(struct address_space * , unsigned long , int , void ** , unsigned long * ) ;
   int (*migratepage)(struct address_space * , struct page * , struct page * ) ;
   int (*launder_page)(struct page * ) ;
   int (*is_partially_uptodate)(struct page * , read_descriptor_t * , unsigned long ) ;
};

struct address_space {
   struct inode *host ;
   struct radix_tree_root page_tree ;
   spinlock_t tree_lock ;
   unsigned int i_mmap_writable ;
   struct prio_tree_root i_mmap ;
   struct list_head i_mmap_nonlinear ;
   spinlock_t i_mmap_lock ;
   unsigned int truncate_count ;
   unsigned long nrpages ;
   unsigned long writeback_index ;
   struct address_space_operations const *a_ops ;
   unsigned long flags ;
   struct backing_dev_info *backing_dev_info ;
   spinlock_t private_lock ;
   struct list_head private_list ;
   struct address_space *assoc_mapping ;
} __attribute__((__aligned__(sizeof(long )))) ;

struct hd_struct;

struct gendisk;

struct block_device {
   dev_t bd_dev ;
   struct inode *bd_inode ;
   struct super_block *bd_super ;
   int bd_openers ;
   struct mutex bd_mutex ;
   struct semaphore bd_mount_sem ;
   struct list_head bd_inodes ;
   void *bd_holder ;
   int bd_holders ;
   struct list_head bd_holder_list ;
   struct block_device *bd_contains ;
   unsigned int bd_block_size ;
   struct hd_struct *bd_part ;
   unsigned int bd_part_count ;
   int bd_invalidated ;
   struct gendisk *bd_disk ;
   struct list_head bd_list ;
   struct backing_dev_info *bd_inode_backing_dev_info ;
   unsigned long bd_private ;
   int bd_fsfreeze_count ;
   struct mutex bd_fsfreeze_mutex ;
};

struct inode_operations;

struct file_operations;

struct file_lock;

struct cdev;

union __anonunion____missing_field_name_191 {
   struct pipe_inode_info *i_pipe ;
   struct block_device *i_bdev ;
   struct cdev *i_cdev ;
};

struct dnotify_struct;

struct inode {
   struct hlist_node i_hash ;
   struct list_head i_list ;
   struct list_head i_sb_list ;
   struct list_head i_dentry ;
   unsigned long i_ino ;
   atomic_t i_count ;
   unsigned int i_nlink ;
   uid_t i_uid ;
   gid_t i_gid ;
   dev_t i_rdev ;
   u64 i_version ;
   loff_t i_size ;
   struct timespec i_atime ;
   struct timespec i_mtime ;
   struct timespec i_ctime ;
   unsigned int i_blkbits ;
   blkcnt_t i_blocks ;
   unsigned short i_bytes ;
   umode_t i_mode ;
   spinlock_t i_lock ;
   struct mutex i_mutex ;
   struct rw_semaphore i_alloc_sem ;
   struct inode_operations const *i_op ;
   struct file_operations const *i_fop ;
   struct super_block *i_sb ;
   struct file_lock *i_flock ;
   struct address_space *i_mapping ;
   struct address_space i_data ;
   struct dquot *i_dquot[2] ;
   struct list_head i_devices ;
   union __anonunion____missing_field_name_191 __annonCompField23 ;
   int i_cindex ;
   __u32 i_generation ;
   unsigned long i_dnotify_mask ;
   struct dnotify_struct *i_dnotify ;
   struct list_head inotify_watches ;
   struct mutex inotify_mutex ;
   unsigned long i_state ;
   unsigned long dirtied_when ;
   unsigned int i_flags ;
   atomic_t i_writecount ;
   void *i_security ;
   void *i_private ;
};

struct fown_struct {
   rwlock_t lock ;
   struct pid *pid ;
   enum pid_type pid_type ;
   uid_t uid ;
   uid_t euid ;
   int signum ;
};

struct file_ra_state {
   unsigned long start ;
   unsigned int size ;
   unsigned int async_size ;
   unsigned int ra_pages ;
   int mmap_miss ;
   loff_t prev_pos ;
};

union __anonunion_f_u_192 {
   struct list_head fu_list ;
   struct rcu_head fu_rcuhead ;
};

struct file {
   union __anonunion_f_u_192 f_u ;
   struct path f_path ;
   struct file_operations const *f_op ;
   atomic_long_t f_count ;
   unsigned int f_flags ;
   fmode_t f_mode ;
   loff_t f_pos ;
   struct fown_struct f_owner ;
   struct cred const *f_cred ;
   struct file_ra_state f_ra ;
   u64 f_version ;
   void *f_security ;
   void *private_data ;
   struct list_head f_ep_links ;
   spinlock_t f_ep_lock ;
   struct address_space *f_mapping ;
};

typedef struct files_struct *fl_owner_t;

struct file_lock_operations {
   void (*fl_copy_lock)(struct file_lock * , struct file_lock * ) ;
   void (*fl_release_private)(struct file_lock * ) ;
};

struct lock_manager_operations {
   int (*fl_compare_owner)(struct file_lock * , struct file_lock * ) ;
   void (*fl_notify)(struct file_lock * ) ;
   int (*fl_grant)(struct file_lock * , struct file_lock * , int ) ;
   void (*fl_copy_lock)(struct file_lock * , struct file_lock * ) ;
   void (*fl_release_private)(struct file_lock * ) ;
   void (*fl_break)(struct file_lock * ) ;
   int (*fl_mylease)(struct file_lock * , struct file_lock * ) ;
   int (*fl_change)(struct file_lock ** , int ) ;
};

struct nlm_lockowner;

struct nfs_lock_info {
   u32 state ;
   struct nlm_lockowner *owner ;
   struct list_head list ;
};

struct nfs4_lock_state;

struct nfs4_lock_info {
   struct nfs4_lock_state *owner ;
};

struct fasync_struct;

struct __anonstruct_afs_194 {
   struct list_head link ;
   int state ;
};

union __anonunion_fl_u_193 {
   struct nfs_lock_info nfs_fl ;
   struct nfs4_lock_info nfs4_fl ;
   struct __anonstruct_afs_194 afs ;
};

struct file_lock {
   struct file_lock *fl_next ;
   struct list_head fl_link ;
   struct list_head fl_block ;
   fl_owner_t fl_owner ;
   unsigned char fl_flags ;
   unsigned char fl_type ;
   unsigned int fl_pid ;
   struct pid *fl_nspid ;
   wait_queue_head_t fl_wait ;
   struct file *fl_file ;
   loff_t fl_start ;
   loff_t fl_end ;
   struct fasync_struct *fl_fasync ;
   unsigned long fl_break_time ;
   struct file_lock_operations *fl_ops ;
   struct lock_manager_operations *fl_lmops ;
   union __anonunion_fl_u_193 fl_u ;
};

struct fasync_struct {
   int magic ;
   int fa_fd ;
   struct fasync_struct *fa_next ;
   struct file *fa_file ;
};

struct file_system_type;

struct super_operations;

struct xattr_handler;

struct mtd_info;

struct super_block {
   struct list_head s_list ;
   dev_t s_dev ;
   unsigned long s_blocksize ;
   unsigned char s_blocksize_bits ;
   unsigned char s_dirt ;
   unsigned long long s_maxbytes ;
   struct file_system_type *s_type ;
   struct super_operations const *s_op ;
   struct dquot_operations *dq_op ;
   struct quotactl_ops *s_qcop ;
   struct export_operations const *s_export_op ;
   unsigned long s_flags ;
   unsigned long s_magic ;
   struct dentry *s_root ;
   struct rw_semaphore s_umount ;
   struct mutex s_lock ;
   int s_count ;
   int s_need_sync_fs ;
   atomic_t s_active ;
   void *s_security ;
   struct xattr_handler **s_xattr ;
   struct list_head s_inodes ;
   struct list_head s_dirty ;
   struct list_head s_io ;
   struct list_head s_more_io ;
   struct hlist_head s_anon ;
   struct list_head s_files ;
   struct list_head s_dentry_lru ;
   int s_nr_dentry_unused ;
   struct block_device *s_bdev ;
   struct mtd_info *s_mtd ;
   struct list_head s_instances ;
   struct quota_info s_dquot ;
   int s_frozen ;
   wait_queue_head_t s_wait_unfrozen ;
   char s_id[32] ;
   void *s_fs_info ;
   fmode_t s_mode ;
   struct mutex s_vfs_rename_mutex ;
   u32 s_time_gran ;
   char *s_subtype ;
   char *s_options ;
   struct list_head s_async_list ;
};

struct fiemap_extent_info {
   unsigned int fi_flags ;
   unsigned int fi_extents_mapped ;
   unsigned int fi_extents_max ;
   struct fiemap_extent *fi_extents_start ;
};

struct file_operations {
   struct module *owner ;
   loff_t (*llseek)(struct file * , loff_t , int ) ;
   ssize_t (*read)(struct file * , char * , size_t , loff_t * ) ;
   ssize_t (*write)(struct file * , char const * , size_t , loff_t * ) ;
   ssize_t (*aio_read)(struct kiocb * , struct iovec const * , unsigned long ,
                       loff_t ) ;
   ssize_t (*aio_write)(struct kiocb * , struct iovec const * , unsigned long ,
                        loff_t ) ;
   int (*readdir)(struct file * , void * , int (*)(void * , char const * , int ,
                                                   loff_t , u64 , unsigned int ) ) ;
   unsigned int (*poll)(struct file * , struct poll_table_struct * ) ;
   int (*ioctl)(struct inode * , struct file * , unsigned int , unsigned long ) ;
   long (*unlocked_ioctl)(struct file * , unsigned int , unsigned long ) ;
   long (*compat_ioctl)(struct file * , unsigned int , unsigned long ) ;
   int (*mmap)(struct file * , struct vm_area_struct * ) ;
   int (*open)(struct inode * , struct file * ) ;
   int (*flush)(struct file * , fl_owner_t id ) ;
   int (*release)(struct inode * , struct file * ) ;
   int (*fsync)(struct file * , struct dentry * , int datasync ) ;
   int (*aio_fsync)(struct kiocb * , int datasync ) ;
   int (*fasync)(int , struct file * , int ) ;
   int (*lock)(struct file * , int , struct file_lock * ) ;
   ssize_t (*sendpage)(struct file * , struct page * , int , size_t , loff_t * ,
                       int ) ;
   unsigned long (*get_unmapped_area)(struct file * , unsigned long , unsigned long ,
                                      unsigned long , unsigned long ) ;
   int (*check_flags)(int ) ;
   int (*flock)(struct file * , int , struct file_lock * ) ;
   ssize_t (*splice_write)(struct pipe_inode_info * , struct file * , loff_t * , size_t ,
                           unsigned int ) ;
   ssize_t (*splice_read)(struct file * , loff_t * , struct pipe_inode_info * , size_t ,
                          unsigned int ) ;
   int (*setlease)(struct file * , long , struct file_lock ** ) ;
};

struct inode_operations {
   int (*create)(struct inode * , struct dentry * , int , struct nameidata * ) ;
   struct dentry *(*lookup)(struct inode * , struct dentry * , struct nameidata * ) ;
   int (*link)(struct dentry * , struct inode * , struct dentry * ) ;
   int (*unlink)(struct inode * , struct dentry * ) ;
   int (*symlink)(struct inode * , struct dentry * , char const * ) ;
   int (*mkdir)(struct inode * , struct dentry * , int ) ;
   int (*rmdir)(struct inode * , struct dentry * ) ;
   int (*mknod)(struct inode * , struct dentry * , int , dev_t ) ;
   int (*rename)(struct inode * , struct dentry * , struct inode * , struct dentry * ) ;
   int (*readlink)(struct dentry * , char * , int ) ;
   void *(*follow_link)(struct dentry * , struct nameidata * ) ;
   void (*put_link)(struct dentry * , struct nameidata * , void * ) ;
   void (*truncate)(struct inode * ) ;
   int (*permission)(struct inode * , int ) ;
   int (*setattr)(struct dentry * , struct iattr * ) ;
   int (*getattr)(struct vfsmount *mnt , struct dentry * , struct kstat * ) ;
   int (*setxattr)(struct dentry * , char const * , void const * , size_t , int ) ;
   ssize_t (*getxattr)(struct dentry * , char const * , void * , size_t ) ;
   ssize_t (*listxattr)(struct dentry * , char * , size_t ) ;
   int (*removexattr)(struct dentry * , char const * ) ;
   void (*truncate_range)(struct inode * , loff_t , loff_t ) ;
   long (*fallocate)(struct inode *inode , int mode , loff_t offset , loff_t len ) ;
   int (*fiemap)(struct inode * , struct fiemap_extent_info * , u64 start , u64 len ) ;
};

struct super_operations {
   struct inode *(*alloc_inode)(struct super_block *sb ) ;
   void (*destroy_inode)(struct inode * ) ;
   void (*dirty_inode)(struct inode * ) ;
   int (*write_inode)(struct inode * , int ) ;
   void (*drop_inode)(struct inode * ) ;
   void (*delete_inode)(struct inode * ) ;
   void (*put_super)(struct super_block * ) ;
   void (*write_super)(struct super_block * ) ;
   int (*sync_fs)(struct super_block *sb , int wait ) ;
   int (*freeze_fs)(struct super_block * ) ;
   int (*unfreeze_fs)(struct super_block * ) ;
   int (*statfs)(struct dentry * , struct kstatfs * ) ;
   int (*remount_fs)(struct super_block * , int * , char * ) ;
   void (*clear_inode)(struct inode * ) ;
   void (*umount_begin)(struct super_block * ) ;
   int (*show_options)(struct seq_file * , struct vfsmount * ) ;
   int (*show_stats)(struct seq_file * , struct vfsmount * ) ;
   ssize_t (*quota_read)(struct super_block * , int , char * , size_t , loff_t ) ;
   ssize_t (*quota_write)(struct super_block * , int , char const * , size_t ,
                          loff_t ) ;
   int (*bdev_try_to_free_page)(struct super_block * , struct page * , gfp_t ) ;
};

struct file_system_type {
   char const *name ;
   int fs_flags ;
   int (*get_sb)(struct file_system_type * , int , char const * , void * , struct vfsmount * ) ;
   void (*kill_sb)(struct super_block * ) ;
   struct module *owner ;
   struct file_system_type *next ;
   struct list_head fs_supers ;
   struct lock_class_key s_lock_key ;
   struct lock_class_key s_umount_key ;
   struct lock_class_key i_lock_key ;
   struct lock_class_key i_mutex_key ;
   struct lock_class_key i_mutex_dir_key ;
   struct lock_class_key i_alloc_sem_key ;
};

struct usb_device;

struct usb_driver;

struct wusb_dev;

struct ep_device;

struct usb_host_endpoint {
   struct usb_endpoint_descriptor desc ;
   struct list_head urb_list ;
   void *hcpriv ;
   struct ep_device *ep_dev ;
   unsigned char * __attribute__((__blob__("extralen"))) extra ;
   int extralen ;
   int enabled ;
};

struct usb_host_interface {
   struct usb_interface_descriptor desc ;
   struct usb_host_endpoint * __attribute__((__blob__(Obfuscate_Nonstub_usb_host_interface("desc")))) endpoint ;
   char *string ;
   unsigned char * __attribute__((__blob__("extralen"))) extra ;
   int extralen ;
};

enum usb_interface_condition {
    USB_INTERFACE_UNBOUND = 0,
    USB_INTERFACE_BINDING = 1,
    USB_INTERFACE_BOUND = 2,
    USB_INTERFACE_UNBINDING = 3
} ;

struct usb_interface {
   struct usb_host_interface *altsetting ;
   struct usb_host_interface *cur_altsetting ;
   unsigned int num_altsetting ;
   struct usb_interface_assoc_descriptor *intf_assoc ;
   int minor ;
   enum usb_interface_condition condition ;
   unsigned int is_active : 1 ;
   unsigned int sysfs_files_created : 1 ;
   unsigned int ep_devs_created : 1 ;
   unsigned int unregistering : 1 ;
   unsigned int needs_remote_wakeup : 1 ;
   unsigned int needs_altsetting0 : 1 ;
   unsigned int needs_binding : 1 ;
   unsigned int reset_running : 1 ;
   struct device dev ;
   struct device *usb_dev ;
   int pm_usage_cnt ;
   struct work_struct reset_ws ;
};

struct usb_interface_cache {
   unsigned int num_altsetting ;
   struct kref ref ;
   struct usb_host_interface altsetting[0] ;
};

struct usb_host_config {
   struct usb_config_descriptor desc ;
   char *string ;
   struct usb_interface_assoc_descriptor *intf_assoc[16] ;
   struct usb_interface *interface[32] ;
   struct usb_interface_cache *intf_cache[32] ;
   unsigned char *extra ;
   int extralen ;
};

struct usb_devmap {
   unsigned long devicemap[128UL / (8UL * sizeof(unsigned long ))] ;
};

struct mon_bus;

struct usb_bus {
   struct device *controller ;
   int busnum ;
   char const *bus_name ;
   u8 uses_dma ;
   u8 otg_port ;
   unsigned int is_b_host : 1 ;
   unsigned int b_hnp_enable : 1 ;
   int devnum_next ;
   struct usb_devmap devmap ;
   struct usb_device *root_hub ;
   struct list_head bus_list ;
   int bandwidth_allocated ;
   int bandwidth_int_reqs ;
   int bandwidth_isoc_reqs ;
   struct dentry *usbfs_dentry ;
   struct device *dev ;
   struct mon_bus *mon_bus ;
   int monitored ;
};

struct usb_tt;

struct usb_device {
   int devnum ;
   char devpath[16] ;
   enum usb_device_state state ;
   enum usb_device_speed speed ;
   struct usb_tt *tt ;
   int ttport ;
   unsigned int toggle[2] ;
   struct usb_device *parent ;
   struct usb_bus * __attribute__((__recursive__)) bus ;
   struct usb_host_endpoint ep0 ;
   struct device __attribute__((__recursive__)) dev ;
   struct usb_device_descriptor descriptor ;
   struct usb_host_config *config ;
   struct usb_host_config *actconfig ;
   struct usb_host_endpoint *ep_in[16] ;
   struct usb_host_endpoint *ep_out[16] ;
   char **rawdescriptors ;
   unsigned short bus_mA ;
   u8 portnum ;
   u8 level ;
   unsigned int can_submit : 1 ;
   unsigned int discon_suspended : 1 ;
   unsigned int persist_enabled : 1 ;
   unsigned int have_langid : 1 ;
   unsigned int authorized : 1 ;
   unsigned int authenticated : 1 ;
   unsigned int wusb : 1 ;
   int string_langid ;
   char *product ;
   char *manufacturer ;
   char *serial ;
   struct list_head filelist ;
   struct device *usb_classdev ;
   struct dentry *usbfs_dentry ;
   int maxchild ;
   struct usb_device *children[31] ;
   int pm_usage_cnt ;
   u32 quirks ;
   atomic_t urbnum ;
   unsigned long active_duration ;
   struct delayed_work autosuspend ;
   struct work_struct autoresume ;
   struct mutex pm_mutex ;
   unsigned long last_busy ;
   int autosuspend_delay ;
   unsigned long connect_time ;
   unsigned int auto_pm : 1 ;
   unsigned int do_remote_wakeup : 1 ;
   unsigned int reset_resume : 1 ;
   unsigned int autosuspend_disabled : 1 ;
   unsigned int autoresume_disabled : 1 ;
   unsigned int skip_sys_resume : 1 ;
   struct wusb_dev *wusb_dev ;
};

struct usb_dynids {
   spinlock_t lock ;
   struct list_head list ;
};

struct usbdrv_wrap {
   struct device_driver driver ;
   int for_devices ;
};

struct usb_driver {
   char const *name ;
   int (*probe)(struct usb_interface *intf , struct usb_device_id const *id ) ;
   void (*disconnect)(struct usb_interface *intf ) ;
   int (*ioctl)(struct usb_interface *intf , unsigned int code , void *buf ) ;
   int (*suspend)(struct usb_interface *intf , pm_message_t message ) ;
   int (*resume)(struct usb_interface *intf ) ;
   int (*reset_resume)(struct usb_interface *intf ) ;
   int (*pre_reset)(struct usb_interface *intf ) ;
   int (*post_reset)(struct usb_interface *intf ) ;
   struct usb_device_id const *id_table ;
   struct usb_dynids dynids ;
   struct usbdrv_wrap drvwrap ;
   unsigned int no_dynamic_id : 1 ;
   unsigned int supports_autosuspend : 1 ;
   unsigned int soft_unbind : 1 ;
};

struct usb_iso_packet_descriptor {
   unsigned int offset ;
   unsigned int length ;
   unsigned int actual_length ;
   int status ;
};

struct urb;

struct usb_anchor {
   struct list_head urb_list ;
   wait_queue_head_t wait ;
   spinlock_t lock ;
   unsigned int poisoned : 1 ;
};

struct urb {
   struct kref kref ;
   void *hcpriv ;
   atomic_t use_count ;
   atomic_t reject ;
   int unlinked ;
   struct list_head urb_list ;
   struct list_head anchor_list ;
   struct usb_anchor *anchor ;
   struct usb_device * __attribute__((__recursive__)) dev ;
   struct usb_host_endpoint *ep ;
   unsigned int pipe ;
   int status ;
   unsigned int transfer_flags ;
   u8 *transfer_buffer ;
   dma_addr_t transfer_dma ;
   int transfer_buffer_length ;
   int actual_length ;
   unsigned char *setup_packet ;
   dma_addr_t setup_dma ;
   int start_frame ;
   int number_of_packets ;
   int interval ;
   int error_count ;
   u8 * __attribute__((__recursive__, __noderef__)) context ;
   void (*complete)(struct urb * ) ;
   struct usb_iso_packet_descriptor iso_frame_desc[0] ;
};

typedef int snd_device_type_t;

typedef int snd_device_state_t;

struct snd_device;

struct snd_device_ops {
   int (*dev_free)(struct snd_device *dev ) ;
   int (*dev_register)(struct snd_device *dev ) ;
   int (*dev_disconnect)(struct snd_device *dev ) ;
};

struct snd_card;

struct snd_device {
   struct list_head list ;
   struct snd_card *card ;
   snd_device_state_t state ;
   snd_device_type_t type ;
   void * __attribute__((__noderef__)) device_data ;
   struct snd_device_ops *ops ;
};

struct snd_monitor_file {
   struct file *file ;
   struct snd_monitor_file *next ;
   struct file_operations const *disconnected_f_op ;
   struct list_head shutdown_list ;
};

struct snd_info_entry;

struct snd_shutdown_f_ops;

struct snd_mixer_oss;

struct snd_card {
   int number ;
   char id[16] ;
   char driver[16] ;
   char shortname[32] ;
   char longname[80] ;
   char mixername[80] ;
   char components[128] ;
   struct module *module ;
   void *private_data ;
   void (*private_free)(struct snd_card *card ) ;
   struct list_head devices ;
   unsigned int last_numid ;
   struct rw_semaphore controls_rwsem ;
   rwlock_t ctl_files_rwlock ;
   int controls_count ;
   int user_ctl_count ;
   struct list_head controls ;
   struct list_head ctl_files ;
   struct snd_info_entry *proc_root ;
   struct snd_info_entry *proc_id ;
   struct proc_dir_entry *proc_root_link ;
   struct snd_monitor_file *files ;
   struct snd_shutdown_f_ops *s_f_ops ;
   spinlock_t files_lock ;
   int shutdown ;
   int free_on_last_close ;
   wait_queue_head_t shutdown_sleep ;
   struct device *dev ;
   unsigned int power_state ;
   struct mutex power_lock ;
   wait_queue_head_t power_sleep ;
   struct snd_mixer_oss *mixer_oss ;
   int mixer_oss_change_count ;
};

struct pollfd {
   int fd ;
   short events ;
   short revents ;
};

struct exception_table_entry {
   unsigned long insn ;
   unsigned long fixup ;
};

struct poll_table_struct {
   void (*qproc)(struct file * , wait_queue_head_t * , struct poll_table_struct * ) ;
};

typedef struct poll_table_struct poll_table;

struct snd_info_buffer {
   char *buffer ;
   unsigned int curr ;
   unsigned int size ;
   unsigned int len ;
   int stop ;
   int error ;
};

struct snd_info_entry_text {
   void (*read)(struct snd_info_entry *entry , struct snd_info_buffer *buffer ) ;
   void (*write)(struct snd_info_entry *entry , struct snd_info_buffer *buffer ) ;
};

struct snd_info_entry_ops {
   int (*open)(struct snd_info_entry *entry , unsigned short mode , void **file_private_data ) ;
   int (*release)(struct snd_info_entry *entry , unsigned short mode , void *file_private_data ) ;
   long (*read)(struct snd_info_entry *entry , void *file_private_data , struct file *file ,
                char *buf , unsigned long count , unsigned long pos ) ;
   long (*write)(struct snd_info_entry *entry , void *file_private_data , struct file *file ,
                 char const *buf , unsigned long count , unsigned long pos ) ;
   long long (*llseek)(struct snd_info_entry *entry , void *file_private_data , struct file *file ,
                       long long offset , int orig ) ;
   unsigned int (*poll)(struct snd_info_entry *entry , void *file_private_data , struct file *file ,
                        poll_table *wait ) ;
   int (*ioctl)(struct snd_info_entry *entry , void *file_private_data , struct file *file ,
                unsigned int cmd , unsigned long arg ) ;
   int (*mmap)(struct snd_info_entry *entry , void *file_private_data , struct inode *inode ,
               struct file *file , struct vm_area_struct *vma ) ;
};

union __anonunion_c_198 {
   struct snd_info_entry_text text ;
   struct snd_info_entry_ops *ops ;
};

struct snd_info_entry {
   char const * __attribute__((__nullterm__)) name ;
   mode_t mode ;
   long size ;
   unsigned short content ;
   union __anonunion_c_198 c ;
   struct snd_info_entry *parent ;
   struct snd_card *card ;
   struct module *module ;
   void * __attribute__((__noderef__)) private_data ;
   void (*private_free)(struct snd_info_entry *entry ) ;
   struct proc_dir_entry *p ;
   struct mutex access ;
   struct list_head children ;
   struct list_head list ;
};

typedef unsigned long snd_pcm_uframes_t;

typedef long snd_pcm_sframes_t;

typedef int snd_pcm_access_t;

typedef int snd_pcm_format_t;

typedef int snd_pcm_subformat_t;

typedef int snd_pcm_state_t;

union snd_pcm_sync_id {
   unsigned char id[16] ;
   unsigned short id16[8] ;
   unsigned int id32[4] ;
};

typedef int snd_pcm_hw_param_t;

struct snd_interval {
   unsigned int min ;
   unsigned int max ;
   unsigned int openmin : 1 ;
   unsigned int openmax : 1 ;
   unsigned int integer : 1 ;
   unsigned int empty : 1 ;
};

struct snd_mask {
   u_int32_t bits[8] ;
};

struct snd_pcm_hw_params {
   unsigned int flags ;
   struct snd_mask masks[3] ;
   struct snd_mask mres[5] ;
   struct snd_interval intervals[12] ;
   struct snd_interval ires[9] ;
   unsigned int rmask ;
   unsigned int cmask ;
   unsigned int info ;
   unsigned int msbits ;
   unsigned int rate_num ;
   unsigned int rate_den ;
   snd_pcm_uframes_t fifo_size ;
   unsigned char reserved[64] ;
};

struct snd_pcm_mmap_status {
   snd_pcm_state_t state ;
   int pad1 ;
   snd_pcm_uframes_t hw_ptr ;
   struct timespec tstamp ;
   snd_pcm_state_t suspended_state ;
};

struct snd_pcm_mmap_control {
   snd_pcm_uframes_t appl_ptr ;
   snd_pcm_uframes_t avail_min ;
};

struct snd_dma_device {
   int type ;
   struct device *dev ;
};

struct snd_dma_buffer {
   struct snd_dma_device dev ;
   unsigned char *area ;
   dma_addr_t addr ;
   size_t bytes ;
   void *private_data ;
};

struct vm_fault {
   unsigned int flags ;
   unsigned long pgoff ;
   void *virtual_address ;
   struct page *page ;
};

struct vm_operations_struct {
   void (*open)(struct vm_area_struct *area ) ;
   void (*close)(struct vm_area_struct *area ) ;
   int (*fault)(struct vm_area_struct *vma , struct vm_fault *vmf ) ;
   int (*page_mkwrite)(struct vm_area_struct *vma , struct page *page ) ;
   int (*access)(struct vm_area_struct *vma , unsigned long addr , void *buf , int len ,
                 int write ) ;
   int (*set_policy)(struct vm_area_struct *vma , struct mempolicy *new ) ;
   struct mempolicy *(*get_policy)(struct vm_area_struct *vma , unsigned long addr ) ;
   int (*migrate)(struct vm_area_struct *vma , nodemask_t const *from , nodemask_t const *to ,
                  unsigned long flags ) ;
};

struct snd_pcm_oss_setup {
   char *task_name ;
   unsigned int disable : 1 ;
   unsigned int direct : 1 ;
   unsigned int block : 1 ;
   unsigned int nonblock : 1 ;
   unsigned int partialfrag : 1 ;
   unsigned int nosilence : 1 ;
   unsigned int buggyptr : 1 ;
   unsigned int periods ;
   unsigned int period_size ;
   struct snd_pcm_oss_setup *next ;
};

struct snd_pcm_plugin;

struct snd_pcm_oss_runtime {
   unsigned int params : 1 ;
   unsigned int prepare : 1 ;
   unsigned int trigger : 1 ;
   unsigned int sync_trigger : 1 ;
   int rate ;
   int format ;
   unsigned int channels ;
   unsigned int fragshift ;
   unsigned int maxfrags ;
   unsigned int subdivision ;
   size_t period_bytes ;
   size_t period_frames ;
   size_t period_ptr ;
   unsigned int periods ;
   size_t buffer_bytes ;
   size_t bytes ;
   size_t mmap_bytes ;
   char *buffer ;
   size_t buffer_used ;
   struct mutex params_lock ;
   struct snd_pcm_plugin *plugin_first ;
   struct snd_pcm_plugin *plugin_last ;
   unsigned int prev_hw_ptr_interrupt ;
};

struct snd_pcm_substream;

struct snd_pcm_oss_substream {
   unsigned int oss : 1 ;
   struct snd_pcm_oss_setup setup ;
};

struct snd_pcm_oss_stream {
   struct snd_pcm_oss_setup *setup_list ;
   struct mutex setup_mutex ;
   struct snd_info_entry *proc_entry ;
};

struct snd_pcm_oss {
   int reg ;
   unsigned int reg_mask ;
};

struct snd_pcm_hardware {
   unsigned int info ;
   u64 formats ;
   unsigned int rates ;
   unsigned int rate_min ;
   unsigned int rate_max ;
   unsigned int channels_min ;
   unsigned int channels_max ;
   size_t buffer_bytes_max ;
   size_t period_bytes_min ;
   size_t period_bytes_max ;
   unsigned int periods_min ;
   unsigned int periods_max ;
   size_t fifo_size ;
};

struct snd_pcm_ops {
   int (*open)(struct snd_pcm_substream *substream ) ;
   int (*close)(struct snd_pcm_substream *substream ) ;
   int (*ioctl)(struct snd_pcm_substream *substream , unsigned int cmd , void *arg ) ;
   int (*hw_params)(struct snd_pcm_substream *substream , struct snd_pcm_hw_params *params ) ;
   int (*hw_free)(struct snd_pcm_substream *substream ) ;
   int (*prepare)(struct snd_pcm_substream *substream ) ;
   int (*trigger)(struct snd_pcm_substream *substream , int cmd ) ;
   snd_pcm_uframes_t (*pointer)(struct snd_pcm_substream *substream ) ;
   int (*copy)(struct snd_pcm_substream *substream , int channel , snd_pcm_uframes_t pos ,
               void *buf , snd_pcm_uframes_t count ) ;
   int (*silence)(struct snd_pcm_substream *substream , int channel , snd_pcm_uframes_t pos ,
                  snd_pcm_uframes_t count ) ;
   struct page *(*page)(struct snd_pcm_substream *substream , unsigned long offset ) ;
   int (*mmap)(struct snd_pcm_substream *substream , struct vm_area_struct *vma ) ;
   int (*ack)(struct snd_pcm_substream *substream ) ;
};

struct snd_pcm_hw_rule;

struct snd_pcm_hw_rule {
   unsigned int cond ;
   int (*func)(struct snd_pcm_hw_params *params , struct snd_pcm_hw_rule *rule ) ;
   int var ;
   int deps[4] ;
   void *private ;
};

struct snd_pcm_hw_constraints {
   struct snd_mask masks[3] ;
   struct snd_interval intervals[12] ;
   unsigned int rules_num ;
   unsigned int rules_all ;
   struct snd_pcm_hw_rule *rules ;
};

struct snd_pcm_hw_constraint_list {
   unsigned int count ;
   unsigned int *list ;
   unsigned int mask ;
};

struct snd_pcm_runtime {
   struct snd_pcm_substream *trigger_master ;
   struct timespec trigger_tstamp ;
   int overrange ;
   snd_pcm_uframes_t avail_max ;
   snd_pcm_uframes_t hw_ptr_base ;
   snd_pcm_uframes_t hw_ptr_interrupt ;
   snd_pcm_access_t access ;
   snd_pcm_format_t format ;
   snd_pcm_subformat_t subformat ;
   unsigned int rate ;
   unsigned int channels ;
   snd_pcm_uframes_t period_size ;
   unsigned int periods ;
   snd_pcm_uframes_t buffer_size ;
   snd_pcm_uframes_t min_align ;
   size_t byte_align ;
   unsigned int frame_bits ;
   unsigned int sample_bits ;
   unsigned int info ;
   unsigned int rate_num ;
   unsigned int rate_den ;
   int tstamp_mode ;
   unsigned int period_step ;
   snd_pcm_uframes_t start_threshold ;
   snd_pcm_uframes_t stop_threshold ;
   snd_pcm_uframes_t silence_threshold ;
   snd_pcm_uframes_t silence_size ;
   snd_pcm_uframes_t boundary ;
   snd_pcm_uframes_t silence_start ;
   snd_pcm_uframes_t silence_filled ;
   union snd_pcm_sync_id sync ;
   struct snd_pcm_mmap_status *status ;
   struct snd_pcm_mmap_control *control ;
   wait_queue_head_t sleep ;
   struct fasync_struct *fasync ;
   void * __attribute__((__recursive__)) private_data ;
   void (*private_free)(struct snd_pcm_runtime *runtime ) ;
   struct snd_pcm_hardware hw ;
   struct snd_pcm_hw_constraints hw_constraints ;
   void (*transfer_ack_begin)(struct snd_pcm_substream *substream ) ;
   void (*transfer_ack_end)(struct snd_pcm_substream *substream ) ;
   unsigned int timer_resolution ;
   int tstamp_type ;
   unsigned char *dma_area ;
   dma_addr_t dma_addr ;
   size_t dma_bytes ;
   struct snd_dma_buffer *dma_buffer_p ;
   struct snd_pcm_oss_runtime oss ;
};

struct snd_pcm_group {
   spinlock_t lock ;
   struct list_head substreams ;
   int count ;
};

struct snd_pcm;

struct snd_pcm_str;

struct snd_timer;

struct snd_pcm_substream {
   struct snd_pcm *pcm ;
   struct snd_pcm_str *pstr ;
   void *private_data ;
   int number ;
   char name[32] ;
   int stream ;
   char latency_id[20] ;
   size_t buffer_bytes_max ;
   struct snd_dma_buffer dma_buffer ;
   unsigned int dma_buf_id ;
   size_t dma_max ;
   struct snd_pcm_ops *ops ;
   struct snd_pcm_runtime * __attribute__((__recursive__)) runtime ;
   struct snd_timer *timer ;
   unsigned int timer_running : 1 ;
   spinlock_t timer_lock ;
   struct snd_pcm_substream *next ;
   struct list_head link_list ;
   struct snd_pcm_group self_group ;
   struct snd_pcm_group *group ;
   void *file ;
   int ref_count ;
   atomic_t mmap_count ;
   unsigned int f_flags ;
   void (*pcm_release)(struct snd_pcm_substream * ) ;
   struct snd_pcm_oss_substream oss ;
   struct snd_info_entry *proc_root ;
   struct snd_info_entry *proc_info_entry ;
   struct snd_info_entry *proc_hw_params_entry ;
   struct snd_info_entry *proc_sw_params_entry ;
   struct snd_info_entry *proc_status_entry ;
   struct snd_info_entry *proc_prealloc_entry ;
   struct snd_info_entry *proc_prealloc_max_entry ;
   unsigned int hw_opened : 1 ;
};

struct snd_pcm_str {
   int stream ;
   struct snd_pcm *pcm ;
   unsigned int substream_count ;
   unsigned int substream_opened ;
   struct snd_pcm_substream *substream ;
   struct snd_pcm_oss_stream oss ;
   struct snd_info_entry *proc_root ;
   struct snd_info_entry *proc_info_entry ;
};

struct snd_pcm {
   struct snd_card *card ;
   struct list_head list ;
   int device ;
   unsigned int info_flags ;
   unsigned short dev_class ;
   unsigned short dev_subclass ;
   char id[64] ;
   char name[80] ;
   struct snd_pcm_str streams[2] ;
   struct mutex open_mutex ;
   wait_queue_head_t open_wait ;
   void * __attribute__((__recursive__)) private_data ;
   void (*private_free)(struct snd_pcm *pcm ) ;
   struct device *dev ;
   struct snd_pcm_oss oss ;
};

typedef int read_proc_t(char *page , char **start , off_t off , int count , int *eof ,
                        void *data );

typedef int write_proc_t(struct file *file , char const *buffer , unsigned long count ,
                         void *data );

struct proc_dir_entry {
   unsigned int low_ino ;
   unsigned short namelen ;
   char const *name ;
   mode_t mode ;
   nlink_t nlink ;
   uid_t uid ;
   gid_t gid ;
   loff_t size ;
   struct inode_operations const *proc_iops ;
   struct file_operations const *proc_fops ;
   struct module *owner ;
   struct proc_dir_entry *next ;
   struct proc_dir_entry *parent ;
   struct proc_dir_entry *subdir ;
   void *data ;
   read_proc_t *read_proc ;
   write_proc_t *write_proc ;
   atomic_t count ;
   int pde_users ;
   spinlock_t pde_unload_lock ;
   struct completion *pde_unload_completion ;
   struct list_head pde_openers ;
};

struct snd_usb_audio {
   int index ;
   struct usb_device *dev ;
   struct snd_card *card ;
   u32 usb_id ;
   int shutdown ;
   int num_interfaces ;
   int num_suspended_intf ;
   struct list_head pcm_list ;
   int pcm_devs ;
   struct list_head midi_list ;
   int next_midi_device ;
   struct list_head mixer_list ;
};

struct snd_usb_audio_quirk {
   char const * __attribute__((__nullterm__)) vendor_name ;
   char const * __attribute__((__recursive__)) product_name ;
   int16_t ifnum ;
   uint16_t type ;
   void const * __attribute__((__recursive__)) data ;
};

struct snd_usb_midi_endpoint_info {
   int8_t out_ep ;
   uint8_t out_interval ;
   int8_t in_ep ;
   uint8_t in_interval ;
   uint16_t out_cables ;
   uint16_t in_cables ;
};

struct audioformat {
   struct list_head list ;
   snd_pcm_format_t format ;
   unsigned int channels ;
   unsigned int fmt_type ;
   unsigned int frame_size ;
   int iface ;
   unsigned char altsetting ;
   unsigned char altset_idx ;
   unsigned char attributes ;
   unsigned char endpoint ;
   unsigned char ep_attr ;
   unsigned int maxpacksize ;
   unsigned int rates ;
   unsigned int rate_min ;
   unsigned int rate_max ;
   unsigned int nr_rates ;
   unsigned int *rate_table ;
};

struct snd_usb_substream;

struct snd_urb_ctx {
   struct urb * __attribute__((__recursive__)) urb ;
   unsigned int buffer_size ;
   struct snd_usb_substream *subs ;
   int index ;
   int packets ;
};

struct snd_urb_ops {
   int (*prepare)(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                  struct urb *u ) ;
   int (*retire)(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                 struct urb *u ) ;
   int (*prepare_sync)(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                       struct urb *u ) ;
   int (*retire_sync)(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                      struct urb *u ) ;
};

struct snd_usb_stream;

struct snd_usb_substream {
   struct snd_usb_stream * __attribute__((__recursive__)) stream ;
   struct usb_device *dev ;
   struct snd_pcm_substream * __attribute__((__recursive__)) pcm_substream ;
   int direction ;
   int interface ;
   int endpoint ;
   struct audioformat *cur_audiofmt ;
   unsigned int cur_rate ;
   unsigned int period_bytes ;
   unsigned int format ;
   unsigned int datapipe ;
   unsigned int syncpipe ;
   unsigned int datainterval ;
   unsigned int syncinterval ;
   unsigned int freqn ;
   unsigned int freqm ;
   unsigned int freqmax ;
   unsigned int phase ;
   unsigned int maxpacksize ;
   unsigned int maxframesize ;
   unsigned int curpacksize ;
   unsigned int curframesize ;
   unsigned int fill_max : 1 ;
   unsigned int fmt_type ;
   unsigned int packs_per_ms ;
   unsigned int running : 1 ;
   unsigned int hwptr_done ;
   unsigned int transfer_done ;
   unsigned long active_mask ;
   unsigned long unlink_mask ;
   unsigned int nurbs ;
   struct snd_urb_ctx dataurb[8] ;
   struct snd_urb_ctx syncurb[4] ;
   char * __attribute__((__nullterm__)) syncbuf ;
   dma_addr_t sync_dma ;
   u64 formats ;
   unsigned int num_formats ;
   struct list_head fmt_list ;
   struct snd_pcm_hw_constraint_list rate_list ;
   spinlock_t lock ;
   struct snd_urb_ops ops ;
};

struct snd_usb_stream {
   struct snd_usb_audio *chip ;
   struct snd_pcm * __attribute__((__recursive__)) pcm ;
   int pcm_index ;
   unsigned int fmt_type ;
   struct snd_usb_substream substream[2] ;
   struct list_head list ;
};

struct snd_usb_midi_in_endpoint;

struct snd_usb_midi_out_endpoint;

struct usb_protocol_ops {
   void (*input)(struct snd_usb_midi_in_endpoint * , uint8_t * , int ) ;
   void (*output)(struct snd_usb_midi_out_endpoint * ) ;
   void (*output_packet)(struct urb * , uint8_t , uint8_t , uint8_t , uint8_t ) ;
   void (*init_out_endpoint)(struct snd_usb_midi_out_endpoint * ) ;
   void (*finish_out_endpoint)(struct snd_usb_midi_out_endpoint * ) ;
};

typedef int (*quirk_func_t)(struct snd_usb_audio * , struct usb_interface * , struct snd_usb_audio_quirk const * );

enum hrtimer_restart;

struct tasklet_struct {
   struct tasklet_struct *next ;
   unsigned long state ;
   atomic_t count ;
   void (*func)(unsigned long ) ;
   unsigned long data ;
};

struct snd_seq_device {
   struct snd_card *card ;
   int device ;
   char id[32] ;
   char name[80] ;
   int argsize ;
   void *driver_data ;
   int status ;
   void *private_data ;
   void (*private_free)(struct snd_seq_device *device ) ;
   struct list_head list ;
};

struct snd_rawmidi;

struct snd_rawmidi_substream;

struct snd_seq_port_info;

struct snd_rawmidi_ops {
   int (*open)(struct snd_rawmidi_substream *substream ) ;
   int (*close)(struct snd_rawmidi_substream *substream ) ;
   void (*trigger)(struct snd_rawmidi_substream *substream , int up ) ;
   void (*drain)(struct snd_rawmidi_substream *substream ) ;
};

struct snd_rawmidi_global_ops {
   int (*dev_register)(struct snd_rawmidi *rmidi ) ;
   int (*dev_unregister)(struct snd_rawmidi *rmidi ) ;
   void (*get_port_info)(struct snd_rawmidi *rmidi , int number , struct snd_seq_port_info *info ) ;
};

struct snd_rawmidi_runtime {
   unsigned int drain : 1 ;
   unsigned int oss : 1 ;
   unsigned char *buffer ;
   size_t buffer_size ;
   size_t appl_ptr ;
   size_t hw_ptr ;
   size_t avail_min ;
   size_t avail ;
   size_t xruns ;
   spinlock_t lock ;
   wait_queue_head_t sleep ;
   void (*event)(struct snd_rawmidi_substream *substream ) ;
   struct tasklet_struct tasklet ;
   void * __attribute__((__recursive__)) private_data ;
   void (*private_free)(struct snd_rawmidi_substream *substream ) ;
};

struct snd_rawmidi_str;

struct snd_rawmidi_substream {
   struct list_head __attribute__((__noderef__)) list ;
   int stream ;
   int number ;
   unsigned int opened : 1 ;
   unsigned int append : 1 ;
   unsigned int active_sensing : 1 ;
   int use_count ;
   size_t bytes ;
   struct snd_rawmidi * __attribute__((__recursive__)) rmidi ;
   struct snd_rawmidi_str *pstr ;
   char name[32] ;
   struct snd_rawmidi_runtime * __attribute__((__recursive__)) runtime ;
   struct snd_rawmidi_ops *ops ;
};

struct snd_rawmidi_str {
   unsigned int substream_count ;
   unsigned int substream_opened ;
   struct list_head __attribute__((__noderef__)) substreams ;
};

struct snd_rawmidi {
   struct snd_card *card ;
   struct list_head list ;
   unsigned int device ;
   unsigned int info_flags ;
   char id[64] ;
   char name[80] ;
   int ossreg ;
   struct snd_rawmidi_global_ops *ops ;
   struct snd_rawmidi_str streams[2] ;
   void * __attribute__((__recursive__, __noderef__)) private_data ;
   void (*private_free)(struct snd_rawmidi *rmidi ) ;
   struct mutex open_mutex ;
   wait_queue_head_t open_wait ;
   struct snd_info_entry *dev ;
   struct snd_info_entry *proc_entry ;
   struct snd_seq_device *seq_dev ;
};

struct snd_seq_addr {
   unsigned char client ;
   unsigned char port ;
};

struct snd_seq_port_info {
   struct snd_seq_addr addr ;
   char name[64] ;
   unsigned int capability ;
   unsigned int type ;
   int midi_channels ;
   int midi_voices ;
   int synth_voices ;
   int read_use ;
   int write_use ;
   void *kernel ;
   unsigned int flags ;
   unsigned char time_queue ;
   char reserved[59] ;
};

struct usb_ms_header_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __u8 bDescriptorSubtype ;
   __u8 bcdMSC[2] ;
   __le16 wTotalLength ;
} __attribute__((__packed__)) ;

struct usb_ms_endpoint_descriptor {
   __u8 bLength ;
   __u8 bDescriptorType ;
   __u8 bDescriptorSubtype ;
   __u8 bNumEmbMIDIJack ;
   __u8 baAssocJackID[0] ;
} __attribute__((__packed__)) ;

struct snd_usb_midi_endpoint;

struct snd_usb_midi_endpoint {
   struct snd_usb_midi_out_endpoint *out ;
   struct snd_usb_midi_in_endpoint *in ;
};

struct snd_usb_midi {
   struct snd_usb_audio *chip ;
   struct usb_interface *iface ;
   struct snd_usb_audio_quirk const *quirk ;
   struct snd_rawmidi * __attribute__((__recursive__)) rmidi ;
   struct usb_protocol_ops *usb_protocol_ops ;
   struct list_head __attribute__((__recursive__)) list ;
   struct timer_list error_timer ;
   spinlock_t disc_lock ;
   struct snd_usb_midi_endpoint endpoints[2] ;
   unsigned long input_triggered ;
   unsigned char disconnected ;
};

struct usbmidi_out_port {
   struct snd_usb_midi_out_endpoint *ep ;
   struct snd_rawmidi_substream * __attribute__((__recursive__)) substream ;
   int active ;
   uint8_t cable ;
   uint8_t state ;
   uint8_t data[2] ;
};

struct snd_usb_midi_out_endpoint {
   struct snd_usb_midi * __attribute__((__recursive__)) umidi ;
   struct urb * __attribute__((__recursive__)) urb ;
   int urb_active ;
   int max_transfer ;
   struct tasklet_struct tasklet ;
   spinlock_t buffer_lock ;
   struct usbmidi_out_port ports[16] ;
   int current_port ;
};

struct usbmidi_in_port {
   struct snd_rawmidi_substream *substream ;
   u8 running_status_length ;
};

struct snd_usb_midi_in_endpoint {
   struct snd_usb_midi *umidi ;
   struct urb * __attribute__((__recursive__)) urb ;
   struct usbmidi_in_port ports[16] ;
   u8 seen_f5 ;
   u8 error_resubmit ;
   int current_port ;
};

struct port_info {
   u32 id ;
   short port ;
   short voices ;
   char const * __attribute__((__nullterm__)) name ;
   unsigned int seq_flags ;
};

struct x8664_pda {
   struct task_struct *pcurrent ;
   unsigned long data_offset ;
   unsigned long kernelstack ;
   unsigned long oldrsp ;
   int irqcount ;
   unsigned int cpunumber ;
   char *irqstackptr ;
   short nodenumber ;
   short in_bootmem ;
   unsigned int __softirq_pending ;
   unsigned int __nmi_count ;
   short mmu_state ;
   short isidle ;
   struct mm_struct *active_mm ;
   unsigned int apic_timer_irqs ;
   unsigned int irq0_irqs ;
   unsigned int irq_resched_count ;
   unsigned int irq_call_count ;
   unsigned int irq_tlb_count ;
   unsigned int irq_thermal_count ;
   unsigned int irq_threshold_count ;
   unsigned int irq_spurious_count ;
} __attribute__((__aligned__((1) << (7) ))) ;

enum hrtimer_restart;

struct usb_ctrlrequest {
   __u8 bRequestType ;
   __u8 bRequest ;
   __le16 wValue ;
   __le16 wIndex ;
   __le16 wLength ;
} __attribute__((__packed__)) ;

struct snd_aes_iec958 {
   unsigned char status[24] ;
   unsigned char subcode[147] ;
   unsigned char pad ;
   unsigned char dig_subframe[4] ;
};

struct snd_hwdep_dsp_status {
   unsigned int version ;
   unsigned char id[32] ;
   unsigned int num_dsps ;
   unsigned int dsp_loaded ;
   unsigned int chip_ready ;
   unsigned char reserved[16] ;
};

struct snd_hwdep_dsp_image {
   unsigned int index ;
   unsigned char name[64] ;
   unsigned char *image ;
   size_t length ;
   unsigned long driver_data ;
};

typedef int snd_ctl_elem_type_t;

typedef int snd_ctl_elem_iface_t;

struct snd_ctl_elem_id {
   unsigned int numid ;
   snd_ctl_elem_iface_t iface ;
   unsigned int device ;
   unsigned int subdevice ;
   unsigned char name[44] ;
   unsigned int index ;
};

struct __anonstruct_integer_215 {
   long min ;
   long max ;
   long step ;
};

struct __anonstruct_integer64_216 {
   long long min ;
   long long max ;
   long long step ;
};

struct __anonstruct_enumerated_217 {
   unsigned int items ;
   unsigned int item ;
   char name[64] ;
};

union __anonunion_value_214 {
   struct __anonstruct_integer_215 integer ;
   struct __anonstruct_integer64_216 integer64 ;
   struct __anonstruct_enumerated_217 enumerated ;
   unsigned char reserved[128] ;
};

union __anonunion_dimen_218 {
   unsigned short d[4] ;
   unsigned short *d_ptr ;
};

struct snd_ctl_elem_info {
   struct snd_ctl_elem_id id ;
   snd_ctl_elem_type_t type ;
   unsigned int access ;
   unsigned int count ;
   pid_t owner ;
   union __anonunion_value_214 value ;
   union __anonunion_dimen_218 dimen ;
   unsigned char reserved[64UL - 4UL * sizeof(unsigned short )] ;
};

union __anonunion_integer_220 {
   long value[128] ;
   long *value_ptr ;
};

union __anonunion_integer64_221 {
   long long value[64] ;
   long long *value_ptr ;
};

union __anonunion_enumerated_222 {
   unsigned int item[128] ;
   unsigned int *item_ptr ;
};

union __anonunion_bytes_223 {
   unsigned char data[512] ;
   unsigned char *data_ptr ;
};

union __anonunion_value_219 {
   union __anonunion_integer_220 integer ;
   union __anonunion_integer64_221 integer64 ;
   union __anonunion_enumerated_222 enumerated ;
   union __anonunion_bytes_223 bytes ;
   struct snd_aes_iec958 iec958 ;
};

struct snd_ctl_elem_value {
   struct snd_ctl_elem_id id ;
   unsigned int indirect : 1 ;
   union __anonunion_value_219 value ;
   struct timespec tstamp ;
   unsigned char reserved[128UL - sizeof(struct timespec )] ;
};

struct snd_kcontrol;

typedef int snd_kcontrol_info_t(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_info *uinfo );

typedef int snd_kcontrol_get_t(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol );

typedef int snd_kcontrol_put_t(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol );

typedef int snd_kcontrol_tlv_rw_t(struct snd_kcontrol *kcontrol , int op_flag , unsigned int size ,
                                  unsigned int *tlv );

union __anonunion_tlv_228 {
   snd_kcontrol_tlv_rw_t *c ;
   unsigned int const *p ;
};

struct snd_kcontrol_new {
   snd_ctl_elem_iface_t iface ;
   unsigned int device ;
   unsigned int subdevice ;
   unsigned char *name ;
   unsigned int index ;
   unsigned int access ;
   unsigned int count ;
   snd_kcontrol_info_t *info ;
   snd_kcontrol_get_t *get ;
   snd_kcontrol_put_t *put ;
   union __anonunion_tlv_228 tlv ;
   unsigned long private_value ;
};

struct snd_ctl_file;

struct snd_kcontrol_volatile {
   struct snd_ctl_file *owner ;
   pid_t owner_pid ;
   unsigned int access ;
};

union __anonunion_tlv_229 {
   snd_kcontrol_tlv_rw_t *c ;
   unsigned int const *p ;
};

struct snd_kcontrol {
   struct list_head list ;
   struct snd_ctl_elem_id id ;
   unsigned int count ;
   snd_kcontrol_info_t *info ;
   snd_kcontrol_get_t *get ;
   snd_kcontrol_put_t *put ;
   union __anonunion_tlv_229 tlv ;
   unsigned long private_value ;
   char *private_data ;
   void (*private_free)(struct snd_kcontrol *kcontrol ) ;
   struct snd_kcontrol_volatile vd[0] ;
};

struct usb_mixer_interface;

struct usb_mixer_elem_info;

struct snd_ctl_file {
   struct list_head list ;
   struct snd_card *card ;
   pid_t pid ;
   int prefer_pcm_subdevice ;
   int prefer_rawmidi_subdevice ;
   wait_queue_head_t change_sleep ;
   spinlock_t read_lock ;
   struct fasync_struct *fasync ;
   int subscribed ;
   struct list_head events ;
};

struct snd_hwdep;

struct snd_hwdep_ops {
   long long (*llseek)(struct snd_hwdep *hw , struct file *file , long long offset ,
                       int orig ) ;
   long (*read)(struct snd_hwdep *hw , char *buf , long count , loff_t *offset ) ;
   long (*write)(struct snd_hwdep *hw , char const *buf , long count , loff_t *offset ) ;
   int (*open)(struct snd_hwdep *hw , struct file *file ) ;
   int (*release)(struct snd_hwdep *hw , struct file *file ) ;
   unsigned int (*poll)(struct snd_hwdep *hw , struct file *file , poll_table *wait ) ;
   int (*ioctl)(struct snd_hwdep *hw , struct file *file , unsigned int cmd , unsigned long arg ) ;
   int (*ioctl_compat)(struct snd_hwdep *hw , struct file *file , unsigned int cmd ,
                       unsigned long arg ) ;
   int (*mmap)(struct snd_hwdep *hw , struct file *file , struct vm_area_struct *vma ) ;
   int (*dsp_status)(struct snd_hwdep *hw , struct snd_hwdep_dsp_status *status ) ;
   int (*dsp_load)(struct snd_hwdep *hw , struct snd_hwdep_dsp_image *image ) ;
};

struct snd_hwdep {
   struct snd_card *card ;
   struct list_head list ;
   int device ;
   char id[32] ;
   char name[80] ;
   int iface ;
   char oss_dev[32] ;
   int oss_type ;
   int ossreg ;
   struct snd_hwdep_ops ops ;
   wait_queue_head_t open_wait ;
   void *private_data ;
   void (*private_free)(struct snd_hwdep *hwdep ) ;
   struct mutex open_mutex ;
   int used ;
   unsigned int dsp_loaded ;
   unsigned int exclusive : 1 ;
};

struct rc_config {
   u32 usb_id ;
   u8 offset ;
   u8 length ;
   u8 packet_length ;
   u8 min_packet_length ;
   u8 mute_mixer_id ;
   u32 mute_code ;
} __attribute__((__noderef__, __address_space__(2))) ;

struct usb_mixer_interface {
   struct snd_usb_audio *chip ;
   unsigned int ctrlif ;
   struct list_head list ;
   unsigned int ignore_ctl_error ;
   struct urb *urb ;
   struct usb_mixer_elem_info **id_elems ;
   struct rc_config const *rc_cfg ;
   unsigned long rc_hwdep_open ;
   u32 rc_code ;
   wait_queue_head_t rc_waitq ;
   struct urb *rc_urb ;
   struct usb_ctrlrequest *rc_setup_packet ;
   u8 rc_buffer[6] ;
   u8 audigy2nx_leds[3] ;
};

struct usb_audio_term {
   int id ;
   int type ;
   int channels ;
   unsigned int chconfig ;
   int name ;
};

struct usbmix_name_map;

struct usbmix_selector_map;

struct mixer_build {
   struct snd_usb_audio *chip ;
   struct usb_mixer_interface *mixer ;
   unsigned char * __attribute__((__expfld__(buflen))) buffer ;
   unsigned int buflen ;
   unsigned long unitbitmap[((256UL + 8UL * sizeof(long )) - 1UL) / (8UL * sizeof(long ))] ;
   struct usb_audio_term oterm ;
   struct usbmix_name_map const *map ;
   struct usbmix_selector_map const *selector_map ;
};

struct usb_mixer_elem_info {
   struct usb_mixer_interface *mixer ;
   struct usb_mixer_elem_info * __attribute__((__recursive__)) next_id_elem ;
   struct snd_ctl_elem_id *elem_id ;
   unsigned int id ;
   unsigned int control ;
   unsigned int cmask ;
   int channels ;
   int val_type ;
   int min ;
   int max ;
   int res ;
   u8 initialized ;
};

struct usbmix_name_map {
   int id ;
   char const *name ;
   int control ;
};

struct usbmix_selector_map {
   int id ;
   int count ;
   char const **names ;
};

struct usbmix_ctl_map {
   u32 id ;
   struct usbmix_name_map const *map ;
   struct usbmix_selector_map const *selector_map ;
   int ignore_ctl_error ;
};

struct iterm_name_combo {
   int type ;
   char * __attribute__((__nullterm__)) name ;
} __attribute__((__noderef__, __address_space__(2))) ;

struct usb_feature_control_info {
   char const * __attribute__((__nullterm__)) name ;
   unsigned int type ;
};

struct procunit_value_info {
   int control ;
   char *suffix ;
   int val_type ;
   int min_value ;
};

struct procunit_info {
   int type ;
   char *name ;
   struct procunit_value_info *values ;
};

struct sb_jack {
   int unitid ;
   char const *name ;
};







#pragma merger(0,"./usbaudio_annotated.i","-Wall,-Wundef,-Wstrict-prototypes,-Wno-trigraphs,-fno-strict-aliasing,-fno-common,-Werror-implicit-function-declaration,-Os,-m64,-mtune=generic,-mno-red-zone,-mcmodel=kernel,-funit-at-a-time,-maccumulate-outgoing-args,-pipe,-Wno-sign-compare,-fno-asynchronous-unwind-tables,-mno-sse,-mno-mmx,-mno-sse2,-mno-3dnow,-Wframe-larger-than=2048,-fno-stack-protector,-fno-omit-frame-pointer,-fno-optimize-sibling-calls,-g,-Wdeclaration-after-statement,-Wno-pointer-sign,-fwrapv,-fno-dwarf2-cfi-asm,-g,-Wall,-Wno-attributes,-Wno-unknown-pragmas")

__inline static void ( __attribute__((__always_inline__)) set_bit)(unsigned int nr ,
                                                                   unsigned long volatile *addr )
{


  {

  __asm__ volatile (".section .smp_locks,\"a\"\n"
                       " "
                       ".balign 8"
                       " "
                       "\n"
                       " "
                       ".quad"
                       " "
                       "661f\n"
                       ".previous\n"
                       "661:\n\tlock; "
                       "bts %1,%0": "+m" (*((long volatile *)addr)): "Ir" (nr): "memory");

  return;
}
}

__inline static void ( __attribute__((__always_inline__)) clear_bit)(int nr , unsigned long volatile *addr )
{


  {

  __asm__ volatile (".section .smp_locks,\"a\"\n"
                       " "
                       ".balign 8"
                       " "
                       "\n"
                       " "
                       ".quad"
                       " "
                       "661f\n"
                       ".previous\n"
                       "661:\n\tlock; "
                       "btr %1,%0": "+m" (*((long volatile *)addr)): "Ir" (nr));

  return;
}
}

__inline static int ( __attribute__((__always_inline__)) test_and_set_bit)(int nr ,
                                                                           unsigned long volatile *addr )
{
  int oldbit ;

  {

  __asm__ volatile (".section .smp_locks,\"a\"\n"
                       " "
                       ".balign 8"
                       " "
                       "\n"
                       " "
                       ".quad"
                       " "
                       "661f\n"
                       ".previous\n"
                       "661:\n\tlock; "
                       "bts %2,%1\n\t"
                       "sbb %0,%0": "=r" (oldbit), "+m" (*((long volatile *)addr)): "Ir" (nr): "memory");

  return (oldbit);
}
}

__inline static int ( __attribute__((__always_inline__)) variable_test_bit)(int nr ,
                                                                            unsigned long const volatile *addr )
{
  int oldbit ;

  {

  __asm__ volatile ("bt %2,%1\n\t"
                       "sbb %0,%0": "=r" (oldbit): "m" (*((unsigned long *)addr)),
                       "Ir" (nr));

  return (oldbit);
}
}

__inline static int ( __attribute__((__always_inline__)) ffs)(int x )
{
  int r ;

  {

  __asm__ ("bsfl %1,%0\n\t"
            "cmovzl %2,%0": "=r" (r): "rm" (x), "r" (-1));

  return (r + 1);
}
}

extern void ( warn_slowpath)(char const *file , int line ,
                                                     char const *fmt , ...) ;

extern int ( sprintf)(char *buf , char const *fmt , ...) ;

extern int ( snprintf)(char *buf , size_t size , char const *fmt
                                               , ...) ;

extern int ( printk)(char const *fmt , ...) ;

extern unsigned long __phys_addr(unsigned long ) ;

__inline static int ( __attribute__((__always_inline__)) get_order)(unsigned long size ) __attribute__((__const__)) ;

__inline static int ( __attribute__((__always_inline__)) get_order)(unsigned long size )
{
  int order ;

  {

  size = (size - 1UL) >> 11;

  order = -1;

  while (1) {

    size >>= 1;

    order ++;

    if (! size) {

      break;
    }
  }

  return (order);
}
}

extern void *__memcpy(void *to , void const *from , size_t len ) ;

extern void *memset(void *s , int c , size_t n ) ;

extern char *strcpy(char *dest , char const *src ) ;

extern size_t strlcpy(char * , char const * , size_t ) ;

extern size_t strlcat(char * , char const * , __kernel_size_t ) ;

extern void *kmemdup(void const *src , size_t len , gfp_t gfp ) ;

__inline static void ( __attribute__((__always_inline__)) INIT_LIST_HEAD)(struct list_head *list )
{


  {

  list->next = (struct list_head * __attribute__((__recursive__)) )list;

  list->prev = (struct list_head * __attribute__((__recursive__)) )list;

  return;
}
}

extern void __list_add(struct list_head *new , struct list_head *prev , struct list_head *next ) ;

__inline static void ( __attribute__((__always_inline__)) list_add)(struct list_head *new ,
                                                                    struct list_head *head )
{


  {

  __list_add(new, head, (struct list_head *)head->next);

  return;
}
}

__inline static void ( __attribute__((__always_inline__)) list_add_tail)(struct list_head *new ,
                                                                         struct list_head *head )
{


  {

  __list_add(new, (struct list_head *)head->prev, head);

  return;
}
}

extern void list_del(struct list_head *entry ) ;

extern void __spin_lock_init(spinlock_t *lock , char const *name , struct lock_class_key *key ) ;

extern void _spin_lock(spinlock_t *lock ) __attribute__((__section__(".spinlock.text"))) ;

extern unsigned long _spin_lock_irqsave(spinlock_t *lock ) __attribute__((__section__(".spinlock.text"))) ;

extern void _spin_unlock(spinlock_t *lock ) __attribute__((__section__(".spinlock.text"))) ;

extern void _spin_unlock_irqrestore(spinlock_t *lock , unsigned long flags ) __attribute__((__section__(".spinlock.text"))) ;

extern void __wake_up(wait_queue_head_t *q , unsigned int mode , int nr , void *key ) ;

extern void mutex_lock_nested(struct mutex *lock , unsigned int subclass ) ;

extern void mutex_unlock(struct mutex *lock ) ;

extern unsigned long __get_free_pages(gfp_t gfp_mask , unsigned int order ) ;

extern void free_pages(unsigned long addr , unsigned int order ) ;

extern void kfree(void const * ) ;

extern unsigned long volatile jiffies __attribute__((__section__(".data"))) ;

extern unsigned long msecs_to_jiffies(unsigned int m ) ;

extern void *__kmalloc(size_t size , gfp_t flags ) ;

__inline static void *( __attribute__((__always_inline__)) kmalloc)(size_t size ,
                                                                    gfp_t flags )
{
  void *tmp___2 ;

  {

  tmp___2 = __kmalloc(size, flags);

  return (tmp___2);
}
}

__inline static void *( __attribute__((__always_inline__)) kcalloc)(size_t n , size_t size ,
                                                                    gfp_t flags )
{
  void *tmp ;

  {

  if (size != 0UL) {

    if (n > 0xffffffffffffffffUL / size) {

      return ((void *)0);
    }
  }

  tmp = __kmalloc(n * size, flags | 32768U);

  return (tmp);
}
}

__inline static void *( __attribute__((__always_inline__)) kzalloc)(size_t size ,
                                                                    gfp_t flags )
{
  void *tmp ;

  {

  tmp = kmalloc(size, flags | 32768U);

  return (tmp);
}
}

extern long schedule_timeout_uninterruptible(long timeout ) ;

extern int param_set_int(char const *val , struct kernel_param *kp ) ;

extern int param_get_int(char *buffer , struct kernel_param *kp ) ;

extern int param_set_charp(char const *val , struct kernel_param *kp ) ;

extern int param_get_charp(char *buffer , struct kernel_param *kp ) ;

extern int param_set_bool(char const *val , struct kernel_param *kp ) ;

extern int param_get_bool(char *buffer , struct kernel_param *kp ) ;

extern int param_array_set(char const *val , struct kernel_param *kp ) ;

extern int param_array_get(char *buffer , struct kernel_param *kp ) ;

int init_module(void) ;

void cleanup_module(void) ;

extern struct module __this_module ;

__inline static void *( __attribute__((__always_inline__)) dev_get_drvdata)(struct device const *dev )
{


  {

  return ((void *)dev->driver_data);
}
}

__inline static void ( __attribute__((__always_inline__)) dev_set_drvdata)(struct device *dev ,
                                                                           void *data )
{


  {

  dev->driver_data = data;

  return;
}
}

unsigned long Nonstub_usb_host_interface(struct usb_interface_descriptor desc ) ;

unsigned long Nonstub_get_size(unsigned long size ) ;

__inline static void *( __attribute__((__always_inline__)) usb_get_intfdata)(struct usb_interface *intf )
{
  void *tmp ;

  {

  tmp = dev_get_drvdata((struct device const *)(& intf->dev));

  return (tmp);
}
}

__inline static void ( __attribute__((__always_inline__)) usb_set_intfdata)(struct usb_interface *intf ,
                                                                            void *data )
{


  {

  dev_set_drvdata(& intf->dev, data);

  return;
}
}

extern int usb_driver_claim_interface(struct usb_driver *driver , struct usb_interface *iface ,
                                      void *priv ) ;

__inline static int ( __attribute__((__always_inline__)) usb_interface_claimed)(struct usb_interface *iface )
{


  {

  return ((unsigned long )iface->dev.driver != (unsigned long )((void *)0));
}
}

extern struct usb_interface *usb_ifnum_to_if(struct usb_device const *dev , unsigned int ifnum ) ;

__inline static int ( __attribute__((__always_inline__)) usb_make_path)(struct usb_device *dev ,
                                                                        char *buf ,
                                                                        size_t size )
{
  int actual ;
  int tmp ;

  {

  actual = snprintf(buf, size, "usb-%s-%s", (dev->bus)->bus_name, dev->devpath);

  if (actual >= (int )size) {

    tmp = -1;
  } else {

    tmp = actual;
  }

  return (tmp);
}
}

extern int usb_register_driver(struct usb_driver * , struct module * , char const * __attribute__((__nullterm__)) ) ;

__inline static int ( __attribute__((__always_inline__)) usb_register)(struct usb_driver *driver )
{
  int tmp ;

  {

  tmp = usb_register_driver(driver, & __this_module, (char const * __attribute__((__nullterm__)) )"usb_audio");

  return (tmp);
}
}

extern void usb_deregister(struct usb_driver * ) ;

extern struct urb *usb_alloc_urb(int iso_packets , gfp_t mem_flags ) ;

extern void usb_free_urb(struct urb *urb ) ;

extern int usb_submit_urb(struct urb *urb , gfp_t mem_flags ) ;

extern int usb_unlink_urb(struct urb *urb ) ;

extern void *usb_buffer_alloc(struct usb_device *dev , size_t size , gfp_t mem_flags ,
                              dma_addr_t *dma ) ;

extern void usb_buffer_free(struct usb_device *dev , size_t size , void *addr , dma_addr_t dma ) ;

extern int usb_control_msg(struct usb_device *dev , unsigned int pipe , __u8 request ,
                           __u8 requesttype , __u16 value , __u16 index , void *data ,
                           __u16 size , int timeout ) ;

extern int usb_get_descriptor(struct usb_device *dev , unsigned char desctype , unsigned char descindex ,
                              void *buf , int size ) ;

extern int usb_string(struct usb_device *dev , int index , char * __attribute__((__exp__(Nonstub_get_size("size")))) buf ,
                      size_t size ) ;

extern int usb_reset_configuration(struct usb_device *dev ) ;

extern int usb_set_interface(struct usb_device *dev , int ifnum , int alternate ) ;

__inline static unsigned int ( __attribute__((__always_inline__)) __create_pipe)(struct usb_device *dev ,
                                                                                 unsigned int endpoint )
{


  {

  return ((unsigned int )(dev->devnum << 8) | (endpoint << 15));
}
}

__inline static void ( __attribute__((__always_inline__)) snd_power_change_state)(struct snd_card *card ,
                                                                                  unsigned int state )
{


  {

  card->power_state = state;

  __wake_up(& card->power_sleep, 3U, 1, (void *)0);

  return;
}
}

extern struct snd_card *snd_card_new(int idx , char const * __attribute__((__nullterm__)) id ,
                                     struct module *module , int extra_size ) ;

extern int snd_card_disconnect(struct snd_card *card ) ;

extern int snd_card_free(struct snd_card *card ) ;

extern int snd_card_free_when_closed(struct snd_card *card ) ;

extern int snd_card_register(struct snd_card *card ) ;

extern int snd_component_add(struct snd_card *card , char const * __attribute__((__nullterm__)) component ) ;

extern int snd_device_new(struct snd_card *card , snd_device_type_t type , void *device_data ,
                          struct snd_device_ops *ops ) ;

__inline static int ( __attribute__((__always_inline__)) __snd_bug_on)(int cond )
{


  {

  return (0);
}
}

extern int ( snd_iprintf)(struct snd_info_buffer *buffer ,
                                                  char *fmt , ...) ;

extern int snd_card_proc_new(struct snd_card *card , char const * __attribute__((__nullterm__)) name ,
                             struct snd_info_entry **entryp ) ;

__inline static void ( __attribute__((__always_inline__)) snd_info_set_text_ops)(struct snd_info_entry *entry ,
                                                                                 void *private_data ,
                                                                                 void (*read)(struct snd_info_entry * ,
                                                                                              struct snd_info_buffer * ) )
{


  {

  entry->private_data = (void * __attribute__((__noderef__)) )private_data;

  entry->c.text.read = read;

  return;
}
}

extern int snd_pcm_new(struct snd_card *card , char * __attribute__((__nullterm__)) id ,
                       int device , int playback_count , int capture_count , struct snd_pcm **rpcm ) ;

extern int snd_pcm_new_stream(struct snd_pcm *pcm , int stream , int substream_count ) ;

extern int snd_pcm_stop(struct snd_pcm_substream *substream , int status ) ;

extern int snd_pcm_suspend_all(struct snd_pcm *pcm ) ;

__inline static snd_pcm_sframes_t ( __attribute__((__always_inline__)) bytes_to_frames)(struct snd_pcm_runtime *runtime ,
                                                                                        ssize_t size )
{


  {

  return ((size * 8L) / (ssize_t )runtime->frame_bits);
}
}

__inline static struct snd_mask *( __attribute__((__always_inline__)) hw_param_mask)(struct snd_pcm_hw_params *params ,
                                                                                     snd_pcm_hw_param_t var )
{


  {

  return (& params->masks[var]);
}
}

__inline static struct snd_interval *( __attribute__((__always_inline__)) hw_param_interval)(struct snd_pcm_hw_params *params ,
                                                                                             snd_pcm_hw_param_t var )
{


  {

  return (& params->intervals[var - 8]);
}
}

extern int snd_pcm_hw_constraint_minmax(struct snd_pcm_runtime *runtime , snd_pcm_hw_param_t var ,
                                        unsigned int min , unsigned int max ) ;

extern int snd_pcm_hw_constraint_list(struct snd_pcm_runtime *runtime , unsigned int cond ,
                                      snd_pcm_hw_param_t var , struct snd_pcm_hw_constraint_list *l ) ;

extern int snd_pcm_format_physical_width(snd_pcm_format_t format ) ;

extern void snd_pcm_set_ops(struct snd_pcm *pcm , int direction , struct snd_pcm_ops *ops ) ;

extern int snd_pcm_lib_ioctl(struct snd_pcm_substream *substream , unsigned int cmd ,
                             void *arg ) ;

extern void snd_pcm_period_elapsed(struct snd_pcm_substream *substream ) ;

extern unsigned int snd_pcm_rate_to_rate_bit(unsigned int rate ) ;

__inline static unsigned int ( __attribute__((__always_inline__)) snd_mask_min)(struct snd_mask const *mask )
{
  int i ;
  int tmp ;

  {

  i = 0;

  while (i < 2) {

    if (mask->bits[i]) {

      tmp = ffs((int )mask->bits[i]);

      return ((unsigned int )((tmp - 1) + (i << 5)));
    }

    i ++;
  }

  return (0U);
}
}

__inline static int ( __attribute__((__always_inline__)) snd_mask_test)(struct snd_mask const *mask ,
                                                                        unsigned int val )
{


  {

  return ((int )(mask->bits[val >> 5] & (1U << (val & 31U))));
}
}

__inline static int ( __attribute__((__always_inline__)) snd_interval_checkempty)(struct snd_interval const *i )
{
  int tmp ;

  {

  if (i->min > i->max) {

    tmp = 1;
  } else

  if (i->min == i->max) {

    if (i->openmin) {

      tmp = 1;
    } else

    if (i->openmax) {

      tmp = 1;
    } else {

      tmp = 0;
    }
  } else {

    tmp = 0;
  }

  return (tmp);
}
}

unsigned int snd_usb_combine_bytes(unsigned char *bytes , int size ) ;

void *snd_usb_find_desc(void *descstart , int desclen , void *after , u8 dtype ) ;

void *snd_usb_find_csint_desc(void *buffer , int buflen , void *after , u8 dsubtype ) ;

int snd_usb_ctl_msg(struct usb_device *dev , unsigned int pipe , __u8 request , __u8 requesttype ,
                    __u16 value , __u16 index___0 , void *data , __u16 size , int timeout ) ;

int snd_usb_create_mixer(struct snd_usb_audio *chip , int ctrlif , int ignore_error ) ;

void snd_usb_mixer_disconnect(struct list_head *p ) ;

int snd_usb_create_midi_interface(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                  struct snd_usb_audio_quirk const *quirk ) ;

void snd_usbmidi_disconnect(struct list_head *p ) ;

unsigned long Nonstub_get_size(unsigned long size )
{


  {

  return (size);
}
}

unsigned long Nonstub_usb_host_interface(struct usb_interface_descriptor desc )
{


  {

  return ((unsigned long )desc.bNumEndpoints);
}
}

static char const __mod_author74[36] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'a', (char const )'u', (char const )'t', (char const )'h',
        (char const )'o', (char const )'r', (char const )'=', (char const )'T',
        (char const )'a', (char const )'k', (char const )'a', (char const )'s',
        (char const )'h', (char const )'i', (char const )' ', (char const )'I',
        (char const )'w', (char const )'a', (char const )'i', (char const )' ',
        (char const )'<', (char const )'t', (char const )'i', (char const )'w',
        (char const )'a', (char const )'i', (char const )'@', (char const )'s',
        (char const )'u', (char const )'s', (char const )'e', (char const )'.',
        (char const )'d', (char const )'e', (char const )'>', (char const )'\000'};

static char const __mod_description75[22] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'d', (char const )'e', (char const )'s', (char const )'c',
        (char const )'r', (char const )'i', (char const )'p', (char const )'t',
        (char const )'i', (char const )'o', (char const )'n', (char const )'=',
        (char const )'U', (char const )'S', (char const )'B', (char const )' ',
        (char const )'A', (char const )'u', (char const )'d', (char const )'i',
        (char const )'o', (char const )'\000'};

static char const __mod_license76[12] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'l', (char const )'i', (char const )'c', (char const )'e',
        (char const )'n', (char const )'s', (char const )'e', (char const )'=',
        (char const )'G', (char const )'P', (char const )'L', (char const )'\000'};

static int index[32] =

  { -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1};

static char *id[32] =

  { (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0),
        (char *)((void *)0), (char *)((void *)0), (char *)((void *)0), (char *)((void *)0)};

static int enable[32] =

  { 1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1,
        1, 1, 1, 1};

static int vid[32] =

  { -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1};

static int pid[32] =

  { -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1,
        -1, -1, -1, -1};

static int nrpacks = 8;

static int async_unlink = 1;

static int device_setup[32] ;

static int ignore_ctl_error ;

static struct kparam_array const __param_arr_index = {(unsigned int )(sizeof(index) / sizeof(index[0]) + (sizeof(char [1]) - 1UL)),
    (unsigned int *)((void *)0), & param_set_int, & param_get_int, (unsigned int )sizeof(index[0]),
    (void *)(index)};

static char const __param_str_index[6] = { (char const )'i', (char const )'n', (char const )'d', (char const )'e',
        (char const )'x', (char const )'\000'};

static struct kernel_param const __param_index __attribute__((__used__, __unused__,
__section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_index, 292U, & param_array_set, & param_array_get, {.arr = & __param_arr_index}};

static char const __mod_indextype91[28] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'i', (char const )'n', (char const )'d',
        (char const )'e', (char const )'x', (char const )':', (char const )'a',
        (char const )'r', (char const )'r', (char const )'a', (char const )'y',
        (char const )' ', (char const )'o', (char const )'f', (char const )' ',
        (char const )'i', (char const )'n', (char const )'t', (char const )'\000'};

static char const __mod_index92[50] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'i', (char const )'n', (char const )'d',
        (char const )'e', (char const )'x', (char const )':', (char const )'I',
        (char const )'n', (char const )'d', (char const )'e', (char const )'x',
        (char const )' ', (char const )'v', (char const )'a', (char const )'l',
        (char const )'u', (char const )'e', (char const )' ', (char const )'f',
        (char const )'o', (char const )'r', (char const )' ', (char const )'t',
        (char const )'h', (char const )'e', (char const )' ', (char const )'U',
        (char const )'S', (char const )'B', (char const )' ', (char const )'a',
        (char const )'u', (char const )'d', (char const )'i', (char const )'o',
        (char const )' ', (char const )'a', (char const )'d', (char const )'a',
        (char const )'p', (char const )'t', (char const )'e', (char const )'r',
        (char const )'.', (char const )'\000'};

static struct kparam_array const __param_arr_id = {(unsigned int )(sizeof(id) / sizeof(id[0]) + (sizeof(char [1]) - 1UL)), (unsigned int *)((void *)0),
    & param_set_charp, & param_get_charp, (unsigned int )sizeof(id[0]), (void *)(id)};

static char const __param_str_id[3] = { (char const )'i', (char const )'d', (char const )'\000'};

static struct kernel_param const __param_id __attribute__((__used__, __unused__,
__section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_id, 292U, & param_array_set, & param_array_get, {.arr = & __param_arr_id}};

static char const __mod_idtype93[27] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'i', (char const )'d', (char const )':',
        (char const )'a', (char const )'r', (char const )'r', (char const )'a',
        (char const )'y', (char const )' ', (char const )'o', (char const )'f',
        (char const )' ', (char const )'c', (char const )'h', (char const )'a',
        (char const )'r', (char const )'p', (char const )'\000'};

static char const __mod_id94[45] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'i', (char const )'d', (char const )':',
        (char const )'I', (char const )'D', (char const )' ', (char const )'s',
        (char const )'t', (char const )'r', (char const )'i', (char const )'n',
        (char const )'g', (char const )' ', (char const )'f', (char const )'o',
        (char const )'r', (char const )' ', (char const )'t', (char const )'h',
        (char const )'e', (char const )' ', (char const )'U', (char const )'S',
        (char const )'B', (char const )' ', (char const )'a', (char const )'u',
        (char const )'d', (char const )'i', (char const )'o', (char const )' ',
        (char const )'a', (char const )'d', (char const )'a', (char const )'p',
        (char const )'t', (char const )'e', (char const )'r', (char const )'.',
        (char const )'\000'};

static struct kparam_array const __param_arr_enable = {(unsigned int )(sizeof(enable) / sizeof(enable[0]) + (sizeof(char [1]) - 1UL)),
    (unsigned int *)((void *)0), & param_set_bool, & param_get_bool, (unsigned int )sizeof(enable[0]),
    (void *)(enable)};

static char const __param_str_enable[7] = { (char const )'e', (char const )'n', (char const )'a', (char const )'b',
        (char const )'l', (char const )'e', (char const )'\000'};

static struct kernel_param const __param_enable __attribute__((__used__, __unused__,
__section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_enable, 292U, & param_array_set, & param_array_get, {.arr = & __param_arr_enable}};

static char const __mod_enabletype95[30] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'e', (char const )'n', (char const )'a',
        (char const )'b', (char const )'l', (char const )'e', (char const )':',
        (char const )'a', (char const )'r', (char const )'r', (char const )'a',
        (char const )'y', (char const )' ', (char const )'o', (char const )'f',
        (char const )' ', (char const )'b', (char const )'o', (char const )'o',
        (char const )'l', (char const )'\000'};

static char const __mod_enable96[38] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'e', (char const )'n', (char const )'a',
        (char const )'b', (char const )'l', (char const )'e', (char const )':',
        (char const )'E', (char const )'n', (char const )'a', (char const )'b',
        (char const )'l', (char const )'e', (char const )' ', (char const )'U',
        (char const )'S', (char const )'B', (char const )' ', (char const )'a',
        (char const )'u', (char const )'d', (char const )'i', (char const )'o',
        (char const )' ', (char const )'a', (char const )'d', (char const )'a',
        (char const )'p', (char const )'t', (char const )'e', (char const )'r',
        (char const )'.', (char const )'\000'};

static struct kparam_array const __param_arr_vid = {(unsigned int )(sizeof(vid) / sizeof(vid[0]) + (sizeof(char [1]) - 1UL)), (unsigned int *)((void *)0),
    & param_set_int, & param_get_int, (unsigned int )sizeof(vid[0]), (void *)(vid)};

static char const __param_str_vid[4] = { (char const )'v', (char const )'i', (char const )'d', (char const )'\000'};

static struct kernel_param const __param_vid __attribute__((__used__, __unused__,
__section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_vid, 292U, & param_array_set, & param_array_get, {.arr = & __param_arr_vid}};

static char const __mod_vidtype97[26] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'v', (char const )'i', (char const )'d',
        (char const )':', (char const )'a', (char const )'r', (char const )'r',
        (char const )'a', (char const )'y', (char const )' ', (char const )'o',
        (char const )'f', (char const )' ', (char const )'i', (char const )'n',
        (char const )'t', (char const )'\000'};

static char const __mod_vid98[45] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'v', (char const )'i', (char const )'d',
        (char const )':', (char const )'V', (char const )'e', (char const )'n',
        (char const )'d', (char const )'o', (char const )'r', (char const )' ',
        (char const )'I', (char const )'D', (char const )' ', (char const )'f',
        (char const )'o', (char const )'r', (char const )' ', (char const )'t',
        (char const )'h', (char const )'e', (char const )' ', (char const )'U',
        (char const )'S', (char const )'B', (char const )' ', (char const )'a',
        (char const )'u', (char const )'d', (char const )'i', (char const )'o',
        (char const )' ', (char const )'d', (char const )'e', (char const )'v',
        (char const )'i', (char const )'c', (char const )'e', (char const )'.',
        (char const )'\000'};

static struct kparam_array const __param_arr_pid = {(unsigned int )(sizeof(pid) / sizeof(pid[0]) + (sizeof(char [1]) - 1UL)), (unsigned int *)((void *)0),
    & param_set_int, & param_get_int, (unsigned int )sizeof(pid[0]), (void *)(pid)};

static char const __param_str_pid[4] = { (char const )'p', (char const )'i', (char const )'d', (char const )'\000'};

static struct kernel_param const __param_pid __attribute__((__used__, __unused__,
__section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_pid, 292U, & param_array_set, & param_array_get, {.arr = & __param_arr_pid}};

static char const __mod_pidtype99[26] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'p', (char const )'i', (char const )'d',
        (char const )':', (char const )'a', (char const )'r', (char const )'r',
        (char const )'a', (char const )'y', (char const )' ', (char const )'o',
        (char const )'f', (char const )' ', (char const )'i', (char const )'n',
        (char const )'t', (char const )'\000'};

static char const __mod_pid100[46] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'p', (char const )'i', (char const )'d',
        (char const )':', (char const )'P', (char const )'r', (char const )'o',
        (char const )'d', (char const )'u', (char const )'c', (char const )'t',
        (char const )' ', (char const )'I', (char const )'D', (char const )' ',
        (char const )'f', (char const )'o', (char const )'r', (char const )' ',
        (char const )'t', (char const )'h', (char const )'e', (char const )' ',
        (char const )'U', (char const )'S', (char const )'B', (char const )' ',
        (char const )'a', (char const )'u', (char const )'d', (char const )'i',
        (char const )'o', (char const )' ', (char const )'d', (char const )'e',
        (char const )'v', (char const )'i', (char const )'c', (char const )'e',
        (char const )'.', (char const )'\000'};

static char const __param_str_nrpacks[8] =

  { (char const )'n', (char const )'r', (char const )'p', (char const )'a',
        (char const )'c', (char const )'k', (char const )'s', (char const )'\000'};

static struct kernel_param const __param_nrpacks __attribute__((__used__, __unused__,
__section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_nrpacks, 420U, & param_set_int, & param_get_int, {(void *)(& nrpacks)}};

static char const __mod_nrpackstype101[21] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'n', (char const )'r', (char const )'p',
        (char const )'a', (char const )'c', (char const )'k', (char const )'s',
        (char const )':', (char const )'i', (char const )'n', (char const )'t',
        (char const )'\000'};

static char const __mod_nrpacks102[45] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'n', (char const )'r', (char const )'p',
        (char const )'a', (char const )'c', (char const )'k', (char const )'s',
        (char const )':', (char const )'M', (char const )'a', (char const )'x',
        (char const )'.', (char const )' ', (char const )'n', (char const )'u',
        (char const )'m', (char const )'b', (char const )'e', (char const )'r',
        (char const )' ', (char const )'o', (char const )'f', (char const )' ',
        (char const )'p', (char const )'a', (char const )'c', (char const )'k',
        (char const )'e', (char const )'t', (char const )'s', (char const )' ',
        (char const )'p', (char const )'e', (char const )'r', (char const )' ',
        (char const )'U', (char const )'R', (char const )'B', (char const )'.',
        (char const )'\000'};

static char const __param_str_async_unlink[13] =

  { (char const )'a', (char const )'s', (char const )'y', (char const )'n',
        (char const )'c', (char const )'_', (char const )'u', (char const )'n',
        (char const )'l', (char const )'i', (char const )'n', (char const )'k',
        (char const )'\000'};

static struct kernel_param const __param_async_unlink __attribute__((__used__,
__unused__, __section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_async_unlink, 292U, & param_set_bool, & param_get_bool, {(void *)(& async_unlink)}};

static char const __mod_async_unlinktype103[27] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'a', (char const )'s', (char const )'y',
        (char const )'n', (char const )'c', (char const )'_', (char const )'u',
        (char const )'n', (char const )'l', (char const )'i', (char const )'n',
        (char const )'k', (char const )':', (char const )'b', (char const )'o',
        (char const )'o', (char const )'l', (char const )'\000'};

static char const __mod_async_unlink104[41] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'a', (char const )'s', (char const )'y',
        (char const )'n', (char const )'c', (char const )'_', (char const )'u',
        (char const )'n', (char const )'l', (char const )'i', (char const )'n',
        (char const )'k', (char const )':', (char const )'U', (char const )'s',
        (char const )'e', (char const )' ', (char const )'a', (char const )'s',
        (char const )'y', (char const )'n', (char const )'c', (char const )' ',
        (char const )'u', (char const )'n', (char const )'l', (char const )'i',
        (char const )'n', (char const )'k', (char const )' ', (char const )'m',
        (char const )'o', (char const )'d', (char const )'e', (char const )'.',
        (char const )'\000'};

static struct kparam_array const __param_arr_device_setup = {(unsigned int )(sizeof(device_setup) / sizeof(device_setup[0]) + (sizeof(char [1]) - 1UL)),
    (unsigned int *)((void *)0), & param_set_int, & param_get_int, (unsigned int )sizeof(device_setup[0]),
    (void *)(device_setup)};

static char const __param_str_device_setup[13] =

  { (char const )'d', (char const )'e', (char const )'v', (char const )'i',
        (char const )'c', (char const )'e', (char const )'_', (char const )'s',
        (char const )'e', (char const )'t', (char const )'u', (char const )'p',
        (char const )'\000'};

static struct kernel_param const __param_device_setup __attribute__((__used__,
__unused__, __section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_device_setup, 292U, & param_array_set, & param_array_get, {.arr = & __param_arr_device_setup}};

static char const __mod_device_setuptype105[35] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'d', (char const )'e', (char const )'v',
        (char const )'i', (char const )'c', (char const )'e', (char const )'_',
        (char const )'s', (char const )'e', (char const )'t', (char const )'u',
        (char const )'p', (char const )':', (char const )'a', (char const )'r',
        (char const )'r', (char const )'a', (char const )'y', (char const )' ',
        (char const )'o', (char const )'f', (char const )' ', (char const )'i',
        (char const )'n', (char const )'t', (char const )'\000'};

static char const __mod_device_setup106[53] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'d', (char const )'e', (char const )'v',
        (char const )'i', (char const )'c', (char const )'e', (char const )'_',
        (char const )'s', (char const )'e', (char const )'t', (char const )'u',
        (char const )'p', (char const )':', (char const )'S', (char const )'p',
        (char const )'e', (char const )'c', (char const )'i', (char const )'f',
        (char const )'i', (char const )'c', (char const )' ', (char const )'d',
        (char const )'e', (char const )'v', (char const )'i', (char const )'c',
        (char const )'e', (char const )' ', (char const )'s', (char const )'e',
        (char const )'t', (char const )'u', (char const )'p', (char const )' ',
        (char const )'(', (char const )'i', (char const )'f', (char const )' ',
        (char const )'n', (char const )'e', (char const )'e', (char const )'d',
        (char const )'e', (char const )'d', (char const )')', (char const )'.',
        (char const )'\000'};

static char const __param_str_ignore_ctl_error[17] =

  { (char const )'i', (char const )'g', (char const )'n', (char const )'o',
        (char const )'r', (char const )'e', (char const )'_', (char const )'c',
        (char const )'t', (char const )'l', (char const )'_', (char const )'e',
        (char const )'r', (char const )'r', (char const )'o', (char const )'r',
        (char const )'\000'};

static struct kernel_param const __param_ignore_ctl_error __attribute__((__used__,
__unused__, __section__("__param"), __aligned__(sizeof(void *)))) = {__param_str_ignore_ctl_error, 292U, & param_set_bool, & param_get_bool, {(void *)(& ignore_ctl_error)}};

static char const __mod_ignore_ctl_errortype107[31] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'t', (char const )'y', (char const )'p', (char const )'e',
        (char const )'=', (char const )'i', (char const )'g', (char const )'n',
        (char const )'o', (char const )'r', (char const )'e', (char const )'_',
        (char const )'c', (char const )'t', (char const )'l', (char const )'_',
        (char const )'e', (char const )'r', (char const )'r', (char const )'o',
        (char const )'r', (char const )':', (char const )'b', (char const )'o',
        (char const )'o', (char const )'l', (char const )'\000'};

static char const __mod_ignore_ctl_error109[78] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'p', (char const )'a', (char const )'r', (char const )'m',
        (char const )'=', (char const )'i', (char const )'g', (char const )'n',
        (char const )'o', (char const )'r', (char const )'e', (char const )'_',
        (char const )'c', (char const )'t', (char const )'l', (char const )'_',
        (char const )'e', (char const )'r', (char const )'r', (char const )'o',
        (char const )'r', (char const )':', (char const )'I', (char const )'g',
        (char const )'n', (char const )'o', (char const )'r', (char const )'e',
        (char const )' ', (char const )'e', (char const )'r', (char const )'r',
        (char const )'o', (char const )'r', (char const )'s', (char const )' ',
        (char const )'f', (char const )'r', (char const )'o', (char const )'m',
        (char const )' ', (char const )'U', (char const )'S', (char const )'B',
        (char const )' ', (char const )'c', (char const )'o', (char const )'n',
        (char const )'t', (char const )'r', (char const )'o', (char const )'l',
        (char const )'l', (char const )'e', (char const )'r', (char const )' ',
        (char const )'f', (char const )'o', (char const )'r', (char const )' ',
        (char const )'m', (char const )'i', (char const )'x', (char const )'e',
        (char const )'r', (char const )' ', (char const )'i', (char const )'n',
        (char const )'t', (char const )'e', (char const )'r', (char const )'f',
        (char const )'a', (char const )'c', (char const )'e', (char const )'s',
        (char const )'.', (char const )'\000'};

void snd_usbmidi_us122l_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                              int buffer_length ) ;

void snd_usbmidi_us122l_output(struct snd_usb_midi_out_endpoint *ep ) ;

struct usb_protocol_ops snd_usbmidi_122l_ops = {& snd_usbmidi_us122l_input, & snd_usbmidi_us122l_output, (void (*)(struct urb * ,
                                                                       uint8_t ,
                                                                       uint8_t ,
                                                                       uint8_t ,
                                                                       uint8_t ))0,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0, (void (*)(struct snd_usb_midi_out_endpoint * ))0};

void snd_usbmidi_standard_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                                int buffer_length ) ;

void snd_usbmidi_standard_output(struct snd_usb_midi_out_endpoint *ep ) ;

void snd_usbmidi_output_standard_packet(struct urb *urb , uint8_t p0 , uint8_t p1 ,
                                        uint8_t p2 , uint8_t p3 ) ;

struct usb_protocol_ops snd_usbmidi_standard_ops = {& snd_usbmidi_standard_input, & snd_usbmidi_standard_output, & snd_usbmidi_output_standard_packet,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0, (void (*)(struct snd_usb_midi_out_endpoint * ))0};

void snd_usbmidi_midiman_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                               int buffer_length ) ;

void snd_usbmidi_output_midiman_packet(struct urb *urb , uint8_t p0 , uint8_t p1 ,
                                       uint8_t p2 , uint8_t p3 ) ;

struct usb_protocol_ops snd_usbmidi_midiman_ops = {& snd_usbmidi_midiman_input, & snd_usbmidi_standard_output, & snd_usbmidi_output_midiman_packet,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0, (void (*)(struct snd_usb_midi_out_endpoint * ))0};

void snd_usbmidi_maudio_broken_running_status_input(struct snd_usb_midi_in_endpoint *ep ,
                                                    uint8_t *buffer , int buffer_length ) ;

struct usb_protocol_ops snd_usbmidi_maudio_broken_running_status_ops = {& snd_usbmidi_maudio_broken_running_status_input, & snd_usbmidi_standard_output,
    & snd_usbmidi_output_standard_packet, (void (*)(struct snd_usb_midi_out_endpoint * ))0,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0};

void snd_usbmidi_cme_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                           int buffer_length ) ;

struct usb_protocol_ops snd_usbmidi_cme_ops = {& snd_usbmidi_cme_input, & snd_usbmidi_standard_output, & snd_usbmidi_output_standard_packet,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0, (void (*)(struct snd_usb_midi_out_endpoint * ))0};

void snd_usbmidi_emagic_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                              int buffer_length ) ;

void snd_usbmidi_emagic_output(struct snd_usb_midi_out_endpoint *ep ) ;

void snd_usbmidi_emagic_init_out(struct snd_usb_midi_out_endpoint *ep ) ;

void snd_usbmidi_emagic_finish_out(struct snd_usb_midi_out_endpoint *ep ) ;

struct usb_protocol_ops snd_usbmidi_emagic_ops = {& snd_usbmidi_emagic_input, & snd_usbmidi_emagic_output, (void (*)(struct urb * ,
                                                                       uint8_t ,
                                                                       uint8_t ,
                                                                       uint8_t ,
                                                                       uint8_t ))0,
    & snd_usbmidi_emagic_init_out, & snd_usbmidi_emagic_finish_out};

void snd_usbmidi_novation_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                                int buffer_length ) ;

void snd_usbmidi_novation_output(struct snd_usb_midi_out_endpoint *ep ) ;

struct usb_protocol_ops snd_usbmidi_novation_ops = {& snd_usbmidi_novation_input, & snd_usbmidi_novation_output, (void (*)(struct urb * ,
                                                                           uint8_t ,
                                                                           uint8_t ,
                                                                           uint8_t ,
                                                                           uint8_t ))0,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0, (void (*)(struct snd_usb_midi_out_endpoint * ))0};

void snd_usbmidi_raw_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                           int buffer_length ) ;

void snd_usbmidi_raw_output(struct snd_usb_midi_out_endpoint *ep ) ;

struct usb_protocol_ops snd_usbmidi_raw_ops = {& snd_usbmidi_raw_input, & snd_usbmidi_raw_output, (void (*)(struct urb * , uint8_t ,
                                                                 uint8_t , uint8_t ,
                                                                 uint8_t ))0, (void (*)(struct snd_usb_midi_out_endpoint * ))0,
    (void (*)(struct snd_usb_midi_out_endpoint * ))0};

static int ignore_interface_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                  struct snd_usb_audio_quirk const *quirk ) ;

static int create_composite_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                  struct snd_usb_audio_quirk const *quirk ) ;

static int create_standard_audio_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                       struct snd_usb_audio_quirk const *quirk ) ;

static int create_fixed_stream_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                     struct snd_usb_audio_quirk const *quirk ) ;

static int create_ua1000_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                               struct snd_usb_audio_quirk const *quirk ) ;

static int create_ua101_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                              struct snd_usb_audio_quirk const *quirk ) ;

static int create_uaxx_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                             struct snd_usb_audio_quirk const *quirk ) ;

static quirk_func_t const quirk_funcs[16] =

  { (quirk_func_t const )(& ignore_interface_quirk), (quirk_func_t const )(& create_composite_quirk), (quirk_func_t const )(& snd_usb_create_midi_interface), (quirk_func_t const )(& snd_usb_create_midi_interface),
        (quirk_func_t const )(& snd_usb_create_midi_interface), (quirk_func_t const )(& snd_usb_create_midi_interface), (quirk_func_t const )(& snd_usb_create_midi_interface), (quirk_func_t const )(& snd_usb_create_midi_interface),
        (quirk_func_t const )(& snd_usb_create_midi_interface), (quirk_func_t const )(& snd_usb_create_midi_interface), (int (* const )(struct snd_usb_audio * , struct usb_interface * , struct snd_usb_audio_quirk const * ))0, (quirk_func_t const )(& create_standard_audio_quirk),
        (quirk_func_t const )(& create_fixed_stream_quirk), (quirk_func_t const )(& create_ua1000_quirk), (quirk_func_t const )(& create_ua101_quirk), (quirk_func_t const )(& create_uaxx_quirk)};

static struct mutex register_mutex = {{(int volatile )1}, {{0U}, 3735899821U, 4294967295U, (void *)-1L, {(struct lock_class_key *)0,
                                                                         (struct lock_class *)0,
                                                                         "register_mutex.wait_lock"}},
    {(struct list_head * __attribute__((__recursive__)) )(& register_mutex.wait_list),
     (struct list_head * __attribute__((__recursive__)) )(& register_mutex.wait_list)},
    (struct thread_info *)0, (char const *)0, (void *)(& register_mutex), {(struct lock_class_key *)0,
                                                                             (struct lock_class *)0,
                                                                             "register_mutex"}};

static struct snd_usb_audio * __attribute__((__noderef__, __address_space__(2))) usb_chip[32] ;

__inline static unsigned int ( __attribute__((__always_inline__)) get_usb_full_speed_rate)(unsigned int rate )
{


  {

  return (((rate << 13) + 62U) / 125U);
}
}

__inline static unsigned int ( __attribute__((__always_inline__)) get_usb_high_speed_rate)(unsigned int rate )
{


  {

  return (((rate << 10) + 62U) / 125U);
}
}

static int prepare_capture_sync_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                    struct urb *urb )
{
  unsigned char *cp ;
  struct snd_urb_ctx *ctx ;

  {

  cp = urb->transfer_buffer;

  ctx = (struct snd_urb_ctx *)urb->context;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  urb->iso_frame_desc[0].length = 3U;

  urb->iso_frame_desc[0].offset = 0U;

  *(cp + 0) = (unsigned char )(subs->freqn >> 2);

  *(cp + 1) = (unsigned char )(subs->freqn >> 10);

  *(cp + 2) = (unsigned char )(subs->freqn >> 18);

  return (0);
}
}

static int prepare_capture_sync_urb_hs(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                       struct urb *urb )
{
  unsigned char *cp ;
  struct snd_urb_ctx *ctx ;

  {

  cp = urb->transfer_buffer;

  ctx = (struct snd_urb_ctx *)urb->context;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  urb->iso_frame_desc[0].length = 4U;

  urb->iso_frame_desc[0].offset = 0U;

  *(cp + 0) = (unsigned char )subs->freqn;

  *(cp + 1) = (unsigned char )(subs->freqn >> 8);

  *(cp + 2) = (unsigned char )(subs->freqn >> 16);

  *(cp + 3) = (unsigned char )(subs->freqn >> 24);

  return (0);
}
}

static int retire_capture_sync_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                   struct urb *urb )
{


  {

  return (0);
}
}

static int prepare_capture_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                               struct urb *urb )
{
  int i ;
  int offs ;
  struct snd_urb_ctx *ctx ;

  {

  ctx = (struct snd_urb_ctx *)urb->context;

  offs = 0;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  i = 0;

  while (i < ctx->packets) {

    urb->iso_frame_desc[i].offset = (unsigned int )offs;

    urb->iso_frame_desc[i].length = subs->curpacksize;

    offs = (int )((unsigned int )offs + subs->curpacksize);

    i ++;
  }

  urb->transfer_buffer_length = offs;

  urb->number_of_packets = ctx->packets;

  return (0);
}
}

static int retire_capture_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                              struct urb *urb )
{
  unsigned long flags ;
  unsigned char *cp ;
  int i ;
  unsigned int stride ;
  unsigned int len ;
  unsigned int oldptr ;
  int period_elapsed ;
  unsigned int cnt ;
  unsigned int blen ;
  size_t __len ;
  void *__ret ;
  size_t __len___0 ;
  void *__ret___0 ;
  size_t __len___1 ;
  void *__ret___1 ;

  {

  period_elapsed = 0;

  stride = runtime->frame_bits >> 3;

  i = 0;

  while (i < urb->number_of_packets) {

    cp = urb->transfer_buffer + urb->iso_frame_desc[i].offset;

    if (urb->iso_frame_desc[i].status) {

      printk("<3>frame %d active: %d\n", i, urb->iso_frame_desc[i].status);
    }

    len = urb->iso_frame_desc[i].actual_length / stride;

    if (! len) {

      goto __Cont;
    }

    while (1) {

      flags = _spin_lock_irqsave(& subs->lock);

      break;
    }

    oldptr = subs->hwptr_done;

    subs->hwptr_done += len;

    if ((snd_pcm_uframes_t )subs->hwptr_done >= runtime->buffer_size) {

      subs->hwptr_done = (unsigned int )((snd_pcm_uframes_t )subs->hwptr_done - runtime->buffer_size);
    }

    subs->transfer_done += len;

    if ((snd_pcm_uframes_t )subs->transfer_done >= runtime->period_size) {

      subs->transfer_done = (unsigned int )((snd_pcm_uframes_t )subs->transfer_done - runtime->period_size);

      period_elapsed = 1;
    }

    while (1) {

      _spin_unlock_irqrestore(& subs->lock, flags);

      break;
    }

    if ((snd_pcm_uframes_t )(oldptr + len) > runtime->buffer_size) {

      cnt = (unsigned int )(runtime->buffer_size - (snd_pcm_uframes_t )oldptr);

      blen = cnt * stride;

      __len = (size_t )blen;

      __ret = __builtin_memcpy((void *)(runtime->dma_area + oldptr * stride), (void const *)cp,
                               __len);

      __len___0 = (size_t )(len * stride - blen);

      __ret___0 = __builtin_memcpy((void *)runtime->dma_area, (void const *)(cp + blen),
                                   __len___0);
    } else {

      __len___1 = (size_t )(len * stride);

      __ret___1 = __builtin_memcpy((void *)(runtime->dma_area + oldptr * stride),
                                   (void const *)cp, __len___1);
    }
    __Cont:

    i ++;
  }

  if (period_elapsed) {

    snd_pcm_period_elapsed((struct snd_pcm_substream *)subs->pcm_substream);
  }

  return (0);
}
}

static int retire_paused_capture_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                     struct urb *urb )
{


  {

  return (0);
}
}

static int prepare_playback_sync_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                     struct urb *urb )
{
  struct snd_urb_ctx *ctx ;

  {

  ctx = (struct snd_urb_ctx *)urb->context;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  urb->iso_frame_desc[0].length = 3U;

  urb->iso_frame_desc[0].offset = 0U;

  return (0);
}
}

static int prepare_playback_sync_urb_hs(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                        struct urb *urb )
{
  struct snd_urb_ctx *ctx ;

  {

  ctx = (struct snd_urb_ctx *)urb->context;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  urb->iso_frame_desc[0].length = 4U;

  urb->iso_frame_desc[0].offset = 0U;

  return (0);
}
}

static int retire_playback_sync_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                    struct urb *urb )
{
  unsigned int f ;
  unsigned long flags ;

  {

  if (urb->iso_frame_desc[0].status == 0) {

    if (urb->iso_frame_desc[0].actual_length == 3U) {

      f = (((unsigned int )*(urb->transfer_buffer) | ((unsigned int )*(urb->transfer_buffer + 1) << 8)) | ((unsigned int )*(urb->transfer_buffer + 2) << 16)) << 2;

      if (f >= subs->freqn - subs->freqn / 8U) {

        if (f <= subs->freqmax) {

          while (1) {

            flags = _spin_lock_irqsave(& subs->lock);

            break;
          }

          subs->freqm = f;

          while (1) {

            _spin_unlock_irqrestore(& subs->lock, flags);

            break;
          }
        }
      }
    }
  }

  return (0);
}
}

static int retire_playback_sync_urb_hs(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                       struct urb *urb )
{
  unsigned int f ;
  unsigned long flags ;

  {

  if (urb->iso_frame_desc[0].status == 0) {

    if (urb->iso_frame_desc[0].actual_length == 4U) {

      f = ((((unsigned int )*(urb->transfer_buffer) | ((unsigned int )*(urb->transfer_buffer + 1) << 8)) | ((unsigned int )*(urb->transfer_buffer + 2) << 16)) | ((unsigned int )*(urb->transfer_buffer + 3) << 24)) & 268435455U;

      if (f >= subs->freqn - subs->freqn / 8U) {

        if (f <= subs->freqmax) {

          while (1) {

            flags = _spin_lock_irqsave(& subs->lock);

            break;
          }

          subs->freqm = f;

          while (1) {

            _spin_unlock_irqrestore(& subs->lock, flags);

            break;
          }
        }
      }
    }
  }

  return (0);
}
}

static int retire_playback_sync_urb_hs_emu(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                           struct urb *urb )
{
  unsigned int f ;
  unsigned long flags ;

  {

  if (urb->iso_frame_desc[0].status == 0) {

    if (urb->iso_frame_desc[0].actual_length == 4U) {

      f = ((((unsigned int )*(urb->transfer_buffer) | ((unsigned int )*(urb->transfer_buffer + 1) << 8)) | ((unsigned int )*(urb->transfer_buffer + 2) << 16)) | ((unsigned int )*(urb->transfer_buffer + 3) << 24)) & 268435455U;

      f >>= subs->datainterval;

      if (f >= subs->freqn - subs->freqn / 8U) {

        if (f <= subs->freqmax) {

          while (1) {

            flags = _spin_lock_irqsave(& subs->lock);

            break;
          }

          subs->freqm = f;

          while (1) {

            _spin_unlock_irqrestore(& subs->lock, flags);

            break;
          }
        }
      }
    }
  }

  return (0);
}
}

static int snd_usb_audio_next_packet_size(struct snd_usb_substream *subs )
{
  unsigned int _min1 ;
  unsigned int _min2 ;
  unsigned int tmp ;

  {

  if (subs->fill_max) {

    return ((int )subs->maxframesize);
  } else {

    subs->phase = (subs->phase & 65535U) + (subs->freqm << subs->datainterval);

    _min1 = subs->phase >> 16;

    _min2 = subs->maxframesize;

    if (_min1 < _min2) {

      tmp = _min1;
    } else {

      tmp = _min2;
    }

    return ((int )tmp);
  }
}
}

static int prepare_nodata_playback_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                       struct urb *urb )
{
  unsigned int i ;
  unsigned int offs ;
  unsigned int counts ;
  struct snd_urb_ctx *ctx ;
  int stride ;
  int tmp ;
  int tmp___0 ;

  {

  ctx = (struct snd_urb_ctx *)urb->context;

  stride = (int )(runtime->frame_bits >> 3);

  offs = 0U;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  urb->number_of_packets = (int )subs->packs_per_ms;

  i = 0U;

  while (i < subs->packs_per_ms) {

    tmp = snd_usb_audio_next_packet_size(subs);

    counts = (unsigned int )tmp;

    urb->iso_frame_desc[i].offset = offs * (unsigned int )stride;

    urb->iso_frame_desc[i].length = counts * (unsigned int )stride;

    offs += counts;

    i ++;
  }

  urb->transfer_buffer_length = (int )(offs * (unsigned int )stride);

  if ((subs->cur_audiofmt)->format == 1) {

    tmp___0 = 128;
  } else {

    tmp___0 = 0;
  }

  memset((void *)urb->transfer_buffer, tmp___0, (size_t )(offs * (unsigned int )stride));

  return (0);
}
}

static int prepare_playback_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                                struct urb *urb )
{
  int i ;
  int stride ;
  int offs ;
  unsigned int counts ;
  unsigned long flags ;
  int period_elapsed ;
  struct snd_urb_ctx *ctx ;
  int tmp ;
  unsigned int len ;
  size_t __len ;
  void *__ret ;
  size_t __len___0 ;
  void *__ret___0 ;
  size_t __len___1 ;
  void *__ret___1 ;

  {

  period_elapsed = 0;

  ctx = (struct snd_urb_ctx *)urb->context;

  stride = (int )(runtime->frame_bits >> 3);

  offs = 0;

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )(ctx->subs)->dev;

  urb->number_of_packets = 0;

  while (1) {

    flags = _spin_lock_irqsave(& subs->lock);

    break;
  }

  i = 0;

  while (i < ctx->packets) {

    tmp = snd_usb_audio_next_packet_size(subs);

    counts = (unsigned int )tmp;

    urb->iso_frame_desc[i].offset = (unsigned int )(offs * stride);

    urb->iso_frame_desc[i].length = counts * (unsigned int )stride;

    offs = (int )((unsigned int )offs + counts);

    (urb->number_of_packets) ++;

    subs->transfer_done += counts;

    if ((snd_pcm_uframes_t )subs->transfer_done >= runtime->period_size) {

      subs->transfer_done = (unsigned int )((snd_pcm_uframes_t )subs->transfer_done - runtime->period_size);

      period_elapsed = 1;

      if (subs->fmt_type == 2U) {

        if (subs->transfer_done > 0U) {

          offs = (int )((unsigned int )offs - subs->transfer_done);

          counts -= subs->transfer_done;

          urb->iso_frame_desc[i].length = counts * (unsigned int )stride;

          subs->transfer_done = 0U;
        }

        i ++;

        if (i < ctx->packets) {

          urb->iso_frame_desc[i].offset = (unsigned int )(offs * stride);

          urb->iso_frame_desc[i].length = 0U;

          (urb->number_of_packets) ++;
        }

        break;
      }
    }

    if (period_elapsed) {

      if (((unsigned int )i & (subs->packs_per_ms - 1U)) == subs->packs_per_ms - 1U) {

        break;
      }
    }

    i ++;
  }

  if ((snd_pcm_uframes_t )(subs->hwptr_done + (unsigned int )offs) > runtime->buffer_size) {

    len = (unsigned int )(runtime->buffer_size - (snd_pcm_uframes_t )subs->hwptr_done);

    __len = (size_t )(len * (unsigned int )stride);

    __ret = __builtin_memcpy((void *)urb->transfer_buffer, (void const *)(runtime->dma_area + subs->hwptr_done * (unsigned int )stride),
                             __len);

    __len___0 = (size_t )(((unsigned int )offs - len) * (unsigned int )stride);

    __ret___0 = __builtin_memcpy((void *)(urb->transfer_buffer + len * (unsigned int )stride),
                                 (void const *)runtime->dma_area, __len___0);
  } else {

    __len___1 = (size_t )(offs * stride);

    __ret___1 = __builtin_memcpy((void *)urb->transfer_buffer, (void const *)(runtime->dma_area + subs->hwptr_done * (unsigned int )stride),
                                 __len___1);
  }

  subs->hwptr_done += (unsigned int )offs;

  if ((snd_pcm_uframes_t )subs->hwptr_done >= runtime->buffer_size) {

    subs->hwptr_done = (unsigned int )((snd_pcm_uframes_t )subs->hwptr_done - runtime->buffer_size);
  }

  while (1) {

    _spin_unlock_irqrestore(& subs->lock, flags);

    break;
  }

  urb->transfer_buffer_length = offs * stride;

  if (period_elapsed) {

    snd_pcm_period_elapsed((struct snd_pcm_substream *)subs->pcm_substream);
  }

  return (0);
}
}

static int retire_playback_urb(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime ,
                               struct urb *urb )
{


  {

  return (0);
}
}

static struct snd_urb_ops ( __attribute__((__noderef__, __address_space__(2))) audio_urb_ops)[2] = { {& prepare_nodata_playback_urb,
      & retire_playback_urb, & prepare_playback_sync_urb, & retire_playback_sync_urb},
        {& prepare_capture_urb,
      & retire_capture_urb, & prepare_capture_sync_urb, & retire_capture_sync_urb}};

static struct snd_urb_ops ( __attribute__((__noderef__, __address_space__(2))) audio_urb_ops_high_speed)[2] = { {& prepare_nodata_playback_urb,
      & retire_playback_urb, & prepare_playback_sync_urb_hs, & retire_playback_sync_urb_hs},
        {& prepare_capture_urb,
      & retire_capture_urb, & prepare_capture_sync_urb_hs, & retire_capture_sync_urb}};

static void snd_complete_urb(struct urb *urb )
{
  struct snd_urb_ctx *ctx ;
  struct snd_usb_substream *subs ;
  struct snd_pcm_substream *substream ;
  int err ;
  int tmp ;

  {

  ctx = (struct snd_urb_ctx *)urb->context;

  subs = ctx->subs;

  substream = (struct snd_pcm_substream *)(ctx->subs)->pcm_substream;

  err = 0;

  if (subs->running) {

    tmp = (*(subs->ops.retire))(subs, (struct snd_pcm_runtime *)substream->runtime,
                                urb);

    if (tmp) {

      goto _L;
    } else {

      goto _L___0;
    }
  } else
  _L___0:

  if (! subs->running) {

    goto _L;
  } else {

    err = (*(subs->ops.prepare))(subs, (struct snd_pcm_runtime *)substream->runtime,
                                 urb);

    if (err < 0) {

      goto _L;
    } else {

      err = usb_submit_urb(urb, 32U);

      if (err < 0) {
        _L:

        clear_bit(ctx->index, (unsigned long volatile *)(& subs->active_mask));

        if (err < 0) {

          printk("<3>cannot submit urb (err = %d)\n", err);

          snd_pcm_stop(substream, 4);
        }
      }
    }
  }

  return;
}
}

static void snd_complete_sync_urb(struct urb *urb )
{
  struct snd_urb_ctx *ctx ;
  struct snd_usb_substream *subs ;
  struct snd_pcm_substream *substream ;
  int err ;
  int tmp ;

  {

  ctx = (struct snd_urb_ctx *)urb->context;

  subs = ctx->subs;

  substream = (struct snd_pcm_substream *)(ctx->subs)->pcm_substream;

  err = 0;

  if (subs->running) {

    tmp = (*(subs->ops.retire_sync))(subs, (struct snd_pcm_runtime *)substream->runtime,
                                     urb);

    if (tmp) {

      goto _L;
    } else {

      goto _L___0;
    }
  } else
  _L___0:

  if (! subs->running) {

    goto _L;
  } else {

    err = (*(subs->ops.prepare_sync))(subs, (struct snd_pcm_runtime *)substream->runtime,
                                      urb);

    if (err < 0) {

      goto _L;
    } else {

      err = usb_submit_urb(urb, 32U);

      if (err < 0) {
        _L:

        clear_bit(ctx->index + 16, (unsigned long volatile *)(& subs->active_mask));

        if (err < 0) {

          printk("<3>cannot submit sync urb (err = %d)\n", err);

          snd_pcm_stop(substream, 4);
        }
      }
    }
  }

  return;
}
}

static struct page *snd_pcm_get_vmalloc_page(struct snd_pcm_substream *subs , unsigned long offset )
{
  void *pageptr ;
  unsigned long tmp ;

  {

  pageptr = (void *)((subs->runtime)->dma_area + offset);

  tmp = __phys_addr((unsigned long )pageptr);

  return ((struct page *)0xffffe20000000000UL + (tmp >> 12));
}
}

static int snd_pcm_alloc_vmalloc_buffer(struct snd_pcm_substream *subs , size_t size )
{
  struct snd_pcm_runtime *runtime ;
  int new_size ;
  int tmp ;
  int tmp___0 ;
  unsigned long tmp___1 ;

  {

  runtime = (struct snd_pcm_runtime *)subs->runtime;

  tmp = get_order(size);

  new_size = (1 << tmp) * 4096;

  printk("MJR Added this: Size: %d, new-size: %d\n", size, new_size);

  if (runtime->dma_area) {

    if (runtime->dma_bytes >= (size_t )new_size) {

      return (0);
    }

    free_pages((unsigned long )runtime->dma_area, 4294967295U);
  }

  tmp___0 = get_order(size);

  tmp___1 = __get_free_pages(208U, (unsigned int )tmp___0);

  runtime->dma_area = (unsigned char *)tmp___1;

  if (! runtime->dma_area) {

    return (-12);
  }

  runtime->dma_bytes = (size_t )new_size;

  return (0);
}
}

static int snd_pcm_free_vmalloc_buffer(struct snd_pcm_substream *subs )
{
  struct snd_pcm_runtime *runtime ;

  {

  runtime = (struct snd_pcm_runtime *)subs->runtime;

  free_pages((unsigned long )runtime->dma_area, 4294967295U);

  runtime->dma_area = (unsigned char *)((void *)0);

  return (0);
}
}

static int deactivate_urbs(struct snd_usb_substream *subs , int force , int can_sleep )
{
  unsigned int i ;
  struct urb *u ;
  int tmp ;
  int tmp___1 ;
  struct urb *u___0 ;
  int tmp___2 ;
  int tmp___4 ;

  {

  subs->running = 0U;

  if (! force) {

    if (((subs->stream)->chip)->shutdown) {

      return (-77);
    }
  }

  i = 0U;

  while (i < subs->nurbs) {

    tmp___1 = variable_test_bit((int )i, (unsigned long const volatile *)(& subs->active_mask));

    if (tmp___1) {

      tmp = test_and_set_bit((int )i, (unsigned long volatile *)(& subs->unlink_mask));

      if (! tmp) {

        u = (struct urb *)subs->dataurb[i].urb;

        usb_unlink_urb(u);
      }
    }

    i ++;
  }

  if (subs->syncpipe) {

    i = 0U;

    while (i < 4U) {

      tmp___4 = variable_test_bit((int )(i + 16U), (unsigned long const volatile *)(& subs->active_mask));

      if (tmp___4) {

        tmp___2 = test_and_set_bit((int )(i + 16U), (unsigned long volatile *)(& subs->unlink_mask));

        if (! tmp___2) {

          u___0 = (struct urb *)subs->syncurb[i].urb;

          usb_unlink_urb(u___0);
        }
      }

      i ++;
    }
  }

  return (0);
}
}

static char const *usb_error_string(int err )
{


  {

  switch (err) {
  case -19:

  return ("no device");
  case -2:

  return ("endpoint not enabled");
  case -32:

  return ("endpoint stalled");
  case -28:

  return ("not enough bandwidth");
  case -108:

  return ("device disabled");
  case -113:

  return ("device suspended");
  case -22:
  case -11:
  case -27:
  case -90:

  return ("internal error");
  default:

  return ("unknown error");
  }
}
}

static int start_urbs(struct snd_usb_substream *subs , struct snd_pcm_runtime *runtime )
{
  unsigned int i ;
  int err ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  char const *tmp___3 ;
  char const *tmp___4 ;

  {

  if (((subs->stream)->chip)->shutdown) {

    return (-77);
  }

  i = 0U;

  while (i < subs->nurbs) {

    tmp = __snd_bug_on(0);

    if (tmp) {

      return (-22);
    }

    tmp___0 = (*(subs->ops.prepare))(subs, runtime, (struct urb *)subs->dataurb[i].urb);

    if (tmp___0 < 0) {

      printk("<3>cannot prepare datapipe for urb %d\n", i);

      goto __error;
    }

    i ++;
  }

  if (subs->syncpipe) {

    i = 0U;

    while (i < 4U) {

      tmp___1 = __snd_bug_on(0);

      if (tmp___1) {

        return (-22);
      }

      tmp___2 = (*(subs->ops.prepare_sync))(subs, runtime, (struct urb *)subs->syncurb[i].urb);

      if (tmp___2 < 0) {

        printk("<3>cannot prepare syncpipe for urb %d\n", i);

        goto __error;
      }

      i ++;
    }
  }

  subs->active_mask = 0UL;

  subs->unlink_mask = 0UL;

  subs->running = 1U;

  i = 0U;

  while (i < subs->nurbs) {

    err = usb_submit_urb((struct urb *)subs->dataurb[i].urb, 32U);

    if (err < 0) {

      tmp___3 = usb_error_string(err);

      printk("<3>cannot submit datapipe for urb %d, error %d: %s\n", i, err, tmp___3);

      goto __error;
    }

    set_bit(i, (unsigned long volatile *)(& subs->active_mask));

    i ++;
  }

  if (subs->syncpipe) {

    i = 0U;

    while (i < 4U) {

      err = usb_submit_urb((struct urb *)subs->syncurb[i].urb, 32U);

      if (err < 0) {

        tmp___4 = usb_error_string(err);

        printk("<3>cannot submit syncpipe for urb %d, error %d: %s\n", i, err, tmp___4);

        goto __error;
      }

      set_bit(i + 16U, (unsigned long volatile *)(& subs->active_mask));

      i ++;
    }
  }

  return (0);
  __error:

  deactivate_urbs(subs, 0, 0);

  return (-32);
}
}

static int wait_clear_urbs(struct snd_usb_substream *subs )
{
  unsigned long end_time ;
  unsigned long tmp ;
  unsigned int i ;
  int alive ;
  int tmp___1 ;
  int tmp___3 ;

  {

  tmp = msecs_to_jiffies(1000U);

  end_time = (unsigned long )(jiffies + (unsigned long volatile )tmp);

  while (1) {

    alive = 0;

    i = 0U;

    while (i < subs->nurbs) {

      tmp___1 = variable_test_bit((int )i, (unsigned long const volatile *)(& subs->active_mask));

      if (tmp___1) {

        alive ++;
      }

      i ++;
    }

    if (subs->syncpipe) {

      i = 0U;

      while (i < 4U) {

        tmp___3 = variable_test_bit((int )(i + 16U), (unsigned long const volatile *)(& subs->active_mask));

        if (tmp___3) {

          alive ++;
        }

        i ++;
      }
    }

    if (! alive) {

      break;
    }

    schedule_timeout_uninterruptible(1L);

    if (! ((long )jiffies - (long )end_time < 0L)) {

      break;
    }
  }

  if (alive) {

    printk("<3>timeout: still %d active urbs..\n", alive);
  }

  return (0);
}
}

static snd_pcm_uframes_t snd_usb_pcm_pointer(struct snd_pcm_substream *substream )
{
  struct snd_usb_substream *subs ;
  snd_pcm_uframes_t hwptr_done ;

  {

  subs = (struct snd_usb_substream *)(substream->runtime)->private_data;

  _spin_lock(& subs->lock);

  hwptr_done = (snd_pcm_uframes_t )subs->hwptr_done;

  _spin_unlock(& subs->lock);

  return (hwptr_done);
}
}

static int snd_usb_pcm_playback_trigger(struct snd_pcm_substream *substream , int cmd )
{
  struct snd_usb_substream *subs ;
  int tmp ;

  {

  subs = (struct snd_usb_substream *)(substream->runtime)->private_data;

  switch (cmd) {
  case 1:
  case 4:

  subs->ops.prepare = & prepare_playback_urb;

  return (0);
  case 0:

  tmp = deactivate_urbs(subs, 0, 0);

  return (tmp);
  case 3:

  subs->ops.prepare = & prepare_nodata_playback_urb;

  return (0);
  default:

  return (-22);
  }
}
}

static int snd_usb_pcm_capture_trigger(struct snd_pcm_substream *substream , int cmd )
{
  struct snd_usb_substream *subs ;
  int tmp ;
  int tmp___0 ;

  {

  subs = (struct snd_usb_substream *)(substream->runtime)->private_data;

  switch (cmd) {
  case 1:

  subs->ops.retire = & retire_capture_urb;

  tmp = start_urbs(subs, (struct snd_pcm_runtime *)substream->runtime);

  return (tmp);
  case 0:

  tmp___0 = deactivate_urbs(subs, 0, 0);

  return (tmp___0);
  case 3:

  subs->ops.retire = & retire_paused_capture_urb;

  return (0);
  case 4:

  subs->ops.retire = & retire_capture_urb;

  return (0);
  default:

  return (-22);
  }
}
}

static void release_urb_ctx(struct snd_urb_ctx *u )
{


  {

  if (u->urb) {

    if (u->buffer_size) {

      usb_buffer_free((u->subs)->dev, (size_t )u->buffer_size, (void *)(u->urb)->transfer_buffer,
                      (u->urb)->transfer_dma);
    }

    usb_free_urb((struct urb *)u->urb);

    u->urb = (struct urb * __attribute__((__recursive__)) )((void *)0);
  }

  return;
}
}

static void release_substream_urbs(struct snd_usb_substream *subs , int force )
{
  int i ;

  {

  deactivate_urbs(subs, force, 1);

  wait_clear_urbs(subs);

  i = 0;

  while (i < 8) {

    release_urb_ctx(& subs->dataurb[i]);

    i ++;
  }

  i = 0;

  while (i < 4) {

    release_urb_ctx(& subs->syncurb[i]);

    i ++;
  }

  usb_buffer_free(subs->dev, (size_t )16, (void *)subs->syncbuf, subs->sync_dma);

  subs->syncbuf = (char * __attribute__((__nullterm__)) )((void *)0);

  subs->nurbs = 0U;

  return;
}
}

static int init_substream_urbs(struct snd_usb_substream *subs , unsigned int period_bytes ,
                               unsigned int rate , unsigned int frame_bits )
{
  unsigned int maxsize ;
  unsigned int n ;
  unsigned int i ;
  int is_playback ;
  unsigned int npacks[8] ;
  unsigned int urb_packs ;
  unsigned int total_packs ;
  unsigned int packs_per_ms ;
  unsigned int _max1 ;
  unsigned int _max2 ;
  unsigned int tmp ;
  unsigned int _min1 ;
  unsigned int _min2 ;
  unsigned int tmp___0 ;
  unsigned int minsize ;
  unsigned int _max1___0 ;
  unsigned int _max2___0 ;
  unsigned int tmp___1 ;
  struct snd_urb_ctx *u ;
  struct urb *tmp___2 ;
  void *tmp___3 ;
  void *tmp___4 ;
  struct snd_urb_ctx *u___0 ;
  struct urb *tmp___5 ;

  {

  is_playback = subs->direction == 0;

  if ((unsigned int )(subs->dev)->speed == 2U) {

    subs->freqn = get_usb_full_speed_rate(rate);
  } else {

    subs->freqn = get_usb_high_speed_rate(rate);
  }

  subs->freqm = subs->freqn;

  if (subs->maxpacksize) {

    maxsize = subs->maxpacksize;

    subs->freqmax = maxsize / (frame_bits >> 3) << (16U - subs->datainterval);
  } else {

    subs->freqmax = subs->freqn + (subs->freqn >> 2);

    maxsize = (subs->freqmax + 65535U) * (frame_bits >> 3) >> (16U - subs->datainterval);
  }

  subs->phase = 0U;

  if (subs->fill_max) {

    subs->curpacksize = subs->maxpacksize;
  } else {

    subs->curpacksize = maxsize;
  }

  if ((unsigned int )(subs->dev)->speed == 3U) {

    packs_per_ms = (unsigned int )(8 >> subs->datainterval);
  } else {

    packs_per_ms = 1U;
  }

  subs->packs_per_ms = packs_per_ms;

  if (is_playback) {

    urb_packs = (unsigned int )nrpacks;

    _max1 = urb_packs;

    _max2 = 1U;

    if (_max1 > _max2) {

      tmp = _max1;
    } else {

      tmp = _max2;
    }

    urb_packs = tmp;

    _min1 = urb_packs;

    _min2 = 20U;

    if (_min1 < _min2) {

      tmp___0 = _min1;
    } else {

      tmp___0 = _min2;
    }

    urb_packs = tmp___0;
  } else {

    urb_packs = 1U;
  }

  urb_packs *= packs_per_ms;

  if (is_playback) {

    minsize = (subs->freqn >> (16U - subs->datainterval)) * (frame_bits >> 3);

    if (subs->syncpipe) {

      minsize -= minsize >> 3;
    }

    _max1___0 = minsize;

    _max2___0 = 1U;

    if (_max1___0 > _max2___0) {

      tmp___1 = _max1___0;
    } else {

      tmp___1 = _max2___0;
    }

    minsize = tmp___1;

    total_packs = ((period_bytes + minsize) - 1U) / minsize;

    total_packs = ((total_packs + packs_per_ms) - 1U) & ~ (packs_per_ms - 1U);

    if (total_packs < 2U * packs_per_ms) {

      total_packs = 2U * packs_per_ms;
    }
  } else {

    total_packs = 8U * urb_packs;
  }

  subs->nurbs = ((total_packs + urb_packs) - 1U) / urb_packs;

  if (subs->nurbs > 8U) {

    subs->nurbs = 8U;

    total_packs = 8U * urb_packs;
  }

  n = total_packs;

  i = 0U;

  while (i < subs->nurbs) {

    if (n > urb_packs) {

      npacks[i] = urb_packs;
    } else {

      npacks[i] = n;
    }

    n -= urb_packs;

    i ++;
  }

  if (subs->nurbs <= 1U) {

    subs->nurbs = 2U;

    npacks[0] = (total_packs + 1U) / 2U;

    npacks[1] = total_packs - npacks[0];
  } else

  if (npacks[subs->nurbs - 1U] < packs_per_ms) {

    if (subs->nurbs > 2U) {

      npacks[0] += npacks[subs->nurbs - 1U];

      (subs->nurbs) --;
    } else {

      subs->nurbs = 2U;

      npacks[0] = (total_packs + 1U) / 2U;

      npacks[1] = total_packs - npacks[0];
    }
  }

  i = 0U;

  while (i < subs->nurbs) {

    u = & subs->dataurb[i];

    u->index = (int )i;

    u->subs = subs;

    u->packets = (int )npacks[i];

    u->buffer_size = maxsize * (unsigned int )u->packets;

    if (subs->fmt_type == 2U) {

      (u->packets) ++;
    }

    tmp___2 = usb_alloc_urb(u->packets, 208U);

    u->urb = (struct urb * __attribute__((__recursive__)) )tmp___2;

    if (! u->urb) {

      goto out_of_memory;
    }

    tmp___3 = usb_buffer_alloc(subs->dev, (size_t )u->buffer_size, 208U, & (u->urb)->transfer_dma);

    (u->urb)->transfer_buffer = (u8 *)tmp___3;

    if (! (u->urb)->transfer_buffer) {

      goto out_of_memory;
    }

    (u->urb)->pipe = subs->datapipe;

    (u->urb)->transfer_flags = 6U;

    (u->urb)->interval = 1 << subs->datainterval;

    (u->urb)->context = (u8 * __attribute__((__recursive__, __noderef__)) )u;

    (u->urb)->complete = & snd_complete_urb;

    i ++;
  }

  if (subs->syncpipe) {

    tmp___4 = usb_buffer_alloc(subs->dev, (size_t )16, 208U, & subs->sync_dma);

    subs->syncbuf = (char * __attribute__((__nullterm__)) )tmp___4;

    if (! subs->syncbuf) {

      goto out_of_memory;
    }

    i = 0U;

    while (i < 4U) {

      u___0 = & subs->syncurb[i];

      u___0->index = (int )i;

      u___0->subs = subs;

      u___0->packets = 1;

      tmp___5 = usb_alloc_urb(1, 208U);

      u___0->urb = (struct urb * __attribute__((__recursive__)) )tmp___5;

      if (! u___0->urb) {

        goto out_of_memory;
      }

      (u___0->urb)->transfer_buffer = (u8 *)(subs->syncbuf + i * 4U);

      (u___0->urb)->transfer_dma = subs->sync_dma + (dma_addr_t )(i * 4U);

      (u___0->urb)->transfer_buffer_length = 4;

      (u___0->urb)->pipe = subs->syncpipe;

      (u___0->urb)->transfer_flags = 6U;

      (u___0->urb)->number_of_packets = 1;

      (u___0->urb)->interval = 1 << subs->syncinterval;

      (u___0->urb)->context = (u8 * __attribute__((__recursive__, __noderef__)) )u___0;

      (u___0->urb)->complete = & snd_complete_sync_urb;

      i ++;
    }
  }

  return (0);
  out_of_memory:

  release_substream_urbs(subs, 0);

  return (-12);
}
}

static struct audioformat *find_format(struct snd_usb_substream *subs , unsigned int format ,
                                       unsigned int rate , unsigned int channels )
{
  struct list_head *p ;
  struct audioformat *found ;
  int cur_attr ;
  int attr ;
  struct audioformat *fp ;
  struct list_head const *__mptr ;
  unsigned int i ;

  {

  found = (struct audioformat *)((void *)0);

  cur_attr = 0;

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    if ((unsigned int )fp->format != format) {

      goto __Cont;
    } else

    if (fp->channels != channels) {

      goto __Cont;
    }

    if (rate < fp->rate_min) {

      goto __Cont;
    } else

    if (rate > fp->rate_max) {

      goto __Cont;
    }

    if (! (fp->rates & (unsigned int )(1 << 30))) {

      i = 0U;

      while (i < fp->nr_rates) {

        if (*(fp->rate_table + i) == rate) {

          break;
        }

        i ++;
      }

      if (i >= fp->nr_rates) {

        goto __Cont;
      }
    }

    attr = (int )fp->ep_attr & 12;

    if (! found) {

      found = fp;

      cur_attr = attr;

      goto __Cont;
    }

    if (attr != cur_attr) {

      if (attr == 4) {

        if (subs->direction == 0) {

          goto __Cont;
        } else {

          goto _L;
        }
      } else
      _L:

      if (attr == 8) {

        if (subs->direction == 1) {

          goto __Cont;
        }
      }

      if (cur_attr == 4) {

        if (subs->direction == 0) {

          found = fp;

          cur_attr = attr;

          goto __Cont;
        } else {

          goto _L___0;
        }
      } else
      _L___0:

      if (cur_attr == 8) {

        if (subs->direction == 1) {

          found = fp;

          cur_attr = attr;

          goto __Cont;
        }
      }
    }

    if (fp->maxpacksize > found->maxpacksize) {

      found = fp;

      cur_attr = attr;
    }
    __Cont:

    p = (struct list_head *)p->next;
  }

  return (found);
}
}

static int init_usb_pitch(struct usb_device *dev , int iface , struct usb_host_interface *alts ,
                          struct audioformat *fmt )
{
  unsigned int ep ;
  unsigned char data[1] ;
  int err ;
  unsigned int tmp ;

  {

  ep = (unsigned int )(alts->endpoint + 0)->desc.bEndpointAddress;

  if ((int )fmt->attributes & 2) {

    data[0] = (unsigned char)1;

    tmp = __create_pipe(dev, 0U);

    err = snd_usb_ctl_msg(dev, (unsigned int )(2 << 30) | tmp, (__u8 )1, (__u8 )((1 << 5) | 2),
                          (__u16 )(2 << 8), (__u16 )ep, (void *)(data), (__u16 )1,
                          1000);

    if (err < 0) {

      printk("<3>%d:%d:%d: cannot set enable PITCH\n", dev->devnum, iface, ep);

      return (err);
    }
  }

  return (0);
}
}

static int init_usb_sample_rate(struct usb_device *dev , int iface , struct usb_host_interface *alts ,
                                struct audioformat *fmt , int rate )
{
  unsigned int ep ;
  unsigned char data[3] ;
  int err ;
  int crate ;
  unsigned int tmp ;
  unsigned int tmp___0 ;

  {

  ep = (unsigned int )(alts->endpoint + 0)->desc.bEndpointAddress;

  if ((int )fmt->attributes & 1) {

    data[0] = (unsigned char )rate;

    data[1] = (unsigned char )(rate >> 8);

    data[2] = (unsigned char )(rate >> 16);

    tmp = __create_pipe(dev, 0U);

    err = snd_usb_ctl_msg(dev, (unsigned int )(2 << 30) | tmp, (__u8 )1, (__u8 )((1 << 5) | 2),
                          (__u16 )(1 << 8), (__u16 )ep, (void *)(data), (__u16 )3,
                          1000);

    if (err < 0) {

      printk("<3>%d:%d:%d: cannot set freq %d to ep 0x%x\n", dev->devnum, iface, (int )fmt->altsetting,
             rate, ep);

      return (err);
    }

    tmp___0 = __create_pipe(dev, 0U);

    err = snd_usb_ctl_msg(dev, ((unsigned int )(2 << 30) | tmp___0) | 128U, (__u8 )129,
                          (__u8 )(((1 << 5) | 2) | 128), (__u16 )(1 << 8), (__u16 )ep,
                          (void *)(data), (__u16 )3, 1000);

    if (err < 0) {

      printk("<4>%d:%d:%d: cannot get freq at ep 0x%x\n", dev->devnum, iface, (int )fmt->altsetting,
             ep);

      return (0);
    }

    crate = ((int )data[0] | ((int )data[1] << 8)) | ((int )data[2] << 16);

    if (crate != rate) {

      printk("<4>current rate %d is different from the runtime rate %d\n", crate,
             rate);
    }
  }

  return (0);
}
}

static int set_format(struct snd_usb_substream *subs , struct audioformat *fmt )
{
  struct usb_device *dev ;
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  struct usb_interface *iface ;
  unsigned int ep ;
  unsigned int attr ;
  int is_playback ;
  int err ;
  int __ret_warn_on ;
  long tmp ;
  long tmp___0 ;
  int __ret_warn_on___0 ;
  long tmp___1 ;
  long tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  unsigned int tmp___5 ;
  unsigned int tmp___6 ;
  unsigned int tmp___7 ;
  unsigned int tmp___8 ;
  unsigned int tmp___9 ;

  {

  dev = subs->dev;

  is_playback = subs->direction == 0;

  iface = usb_ifnum_to_if((struct usb_device const *)dev, (unsigned int )fmt->iface);

  __ret_warn_on = ! (! (! iface));

  tmp = __builtin_expect((long )(! (! __ret_warn_on)), 0L);

  if (tmp) {

    warn_slowpath("/scratch/sym/ipc_drivers/usb-audio/usbaudio_annotated.c", 1490,
                  (char const *)((void *)0));
  }

  tmp___0 = __builtin_expect((long )(! (! __ret_warn_on)), 0L);

  if (tmp___0) {

    return (-22);
  }

  alts = iface->altsetting + fmt->altset_idx;

  altsd = & alts->desc;

  __ret_warn_on___0 = ! (! ((int )altsd->bAlternateSetting != (int )fmt->altsetting));

  tmp___1 = __builtin_expect((long )(! (! __ret_warn_on___0)), 0L);

  if (tmp___1) {

    warn_slowpath("/scratch/sym/ipc_drivers/usb-audio/usbaudio_annotated.c", 1494,
                  (char const *)((void *)0));
  }

  tmp___2 = __builtin_expect((long )(! (! __ret_warn_on___0)), 0L);

  if (tmp___2) {

    return (-22);
  }

  if ((unsigned long )fmt == (unsigned long )subs->cur_audiofmt) {

    return (0);
  }

  if (subs->interface >= 0) {

    if (subs->interface != fmt->iface) {

      tmp___3 = usb_set_interface(subs->dev, subs->interface, 0);

      if (tmp___3 < 0) {

        printk("<3>%d:%d:%d: return to setting 0 failed\n", dev->devnum, fmt->iface,
               (int )fmt->altsetting);

        return (-5);
      }

      subs->interface = -1;

      subs->format = 0U;
    }
  }

  if (subs->interface != fmt->iface) {

    goto _L;
  } else

  if (subs->format != (unsigned int )fmt->altset_idx) {
    _L:

    tmp___4 = usb_set_interface(dev, fmt->iface, (int )fmt->altsetting);

    if (tmp___4 < 0) {

      printk("<3>%d:%d:%d: usb_set_interface failed\n", dev->devnum, fmt->iface, (int )fmt->altsetting);

      return (-5);
    }

    printk("<6>setting usb interface %d:%d\n", fmt->iface, (int )fmt->altsetting);

    subs->interface = fmt->iface;

    subs->format = (unsigned int )fmt->altset_idx;
  }

  ep = (unsigned int )((int )fmt->endpoint & 15);

  if (is_playback) {

    tmp___5 = __create_pipe(dev, ep);

    subs->datapipe = tmp___5;
  } else {

    tmp___6 = __create_pipe(dev, ep);

    subs->datapipe = tmp___6 | 128U;
  }

  if ((unsigned int )(subs->dev)->speed == 3U) {

    if ((int )(alts->endpoint + 0)->desc.bInterval >= 1) {

      if ((int )(alts->endpoint + 0)->desc.bInterval <= 4) {

        subs->datainterval = (unsigned int )((int )(alts->endpoint + 0)->desc.bInterval - 1);
      } else {

        subs->datainterval = 0U;
      }
    } else {

      subs->datainterval = 0U;
    }
  } else {

    subs->datainterval = 0U;
  }

  tmp___7 = 0U;

  subs->syncinterval = tmp___7;

  subs->syncpipe = tmp___7;

  subs->maxpacksize = fmt->maxpacksize;

  subs->fill_max = 0U;

  attr = (unsigned int )((int )fmt->ep_attr & 12);

  if (is_playback) {

    if (attr == 4U) {

      goto _L___3;
    } else {

      goto _L___4;
    }
  } else
  _L___4:

  if (! is_playback) {

    if (attr == 8U) {
      _L___3:

      if ((int )altsd->bNumEndpoints >= 2) {

        if (((int )(alts->endpoint + 1)->desc.bmAttributes & 3) != 1) {

          printk("<3>%d:%d:%d : invalid synch pipe\n", dev->devnum, fmt->iface, (int )fmt->altsetting);

          return (-22);
        } else

        if ((int )(alts->endpoint + 1)->desc.bLength >= 9) {

          if ((int )(alts->endpoint + 1)->desc.bSynchAddress != 0) {

            printk("<3>%d:%d:%d : invalid synch pipe\n", dev->devnum, fmt->iface,
                   (int )fmt->altsetting);

            return (-22);
          }
        }

        ep = (unsigned int )(alts->endpoint + 1)->desc.bEndpointAddress;

        if ((int )(alts->endpoint + 0)->desc.bLength >= 9) {

          if (is_playback) {

            if (ep != (unsigned int )((int )(alts->endpoint + 0)->desc.bSynchAddress | 128)) {

              printk("<3>%d:%d:%d : invalid synch pipe\n", dev->devnum, fmt->iface,
                     (int )fmt->altsetting);

              return (-22);
            } else {

              goto _L___0;
            }
          } else
          _L___0:

          if (! is_playback) {

            if (ep != (unsigned int )((int )(alts->endpoint + 0)->desc.bSynchAddress & -129)) {

              printk("<3>%d:%d:%d : invalid synch pipe\n", dev->devnum, fmt->iface,
                     (int )fmt->altsetting);

              return (-22);
            }
          }
        }

        ep &= 15U;

        if (is_playback) {

          tmp___8 = __create_pipe(dev, ep);

          subs->syncpipe = tmp___8 | 128U;
        } else {

          tmp___9 = __create_pipe(dev, ep);

          subs->syncpipe = tmp___9;
        }

        if ((int )(alts->endpoint + 1)->desc.bLength >= 9) {

          if ((int )(alts->endpoint + 1)->desc.bRefresh >= 1) {

            if ((int )(alts->endpoint + 1)->desc.bRefresh <= 9) {

              subs->syncinterval = (unsigned int )(alts->endpoint + 1)->desc.bRefresh;
            } else {

              goto _L___2;
            }
          } else {

            goto _L___2;
          }
        } else
        _L___2:

        if ((unsigned int )(subs->dev)->speed == 2U) {

          subs->syncinterval = 1U;
        } else

        if ((int )(alts->endpoint + 1)->desc.bInterval >= 1) {

          if ((int )(alts->endpoint + 1)->desc.bInterval <= 16) {

            subs->syncinterval = (unsigned int )((int )(alts->endpoint + 1)->desc.bInterval - 1);
          } else {

            subs->syncinterval = 3U;
          }
        } else {

          subs->syncinterval = 3U;
        }
      }
    }
  }

  if ((int )fmt->attributes & 128) {

    subs->fill_max = 1U;
  }

  err = init_usb_pitch(dev, subs->interface, alts, fmt);

  if (err < 0) {

    return (err);
  }

  subs->cur_audiofmt = fmt;

  return (0);
}
}

static int snd_usb_hw_params(struct snd_pcm_substream *substream , struct snd_pcm_hw_params *hw_params )
{
  struct snd_usb_substream *subs ;
  struct audioformat *fmt ;
  unsigned int channels ;
  unsigned int rate ;
  unsigned int format ;
  int ret ;
  int changed ;
  struct snd_interval *tmp ;
  struct snd_mask *tmp___0 ;
  struct snd_interval *tmp___1 ;
  struct snd_interval *tmp___2 ;
  struct snd_interval *tmp___3 ;
  struct snd_mask *tmp___4 ;
  unsigned int tmp___5 ;
  int tmp___6 ;
  struct snd_interval *tmp___7 ;
  int tmp___8 ;
  struct usb_host_interface *alts ;
  struct usb_interface *iface ;
  struct snd_mask *tmp___9 ;
  unsigned int tmp___10 ;
  int tmp___11 ;
  struct snd_interval *tmp___12 ;
  struct snd_interval *tmp___13 ;
  struct snd_interval *tmp___14 ;
  struct snd_mask *tmp___15 ;
  unsigned int tmp___16 ;
  int tmp___17 ;
  struct snd_interval *tmp___18 ;

  {

  subs = (struct snd_usb_substream *)(substream->runtime)->private_data;

  tmp = hw_param_interval(hw_params, 18);

  ret = snd_pcm_alloc_vmalloc_buffer(substream, (size_t )tmp->min);

  if (ret < 0) {

    return (ret);
  }

  tmp___0 = hw_param_mask(hw_params, 1);

  format = snd_mask_min((struct snd_mask const *)tmp___0);

  tmp___1 = hw_param_interval(hw_params, 11);

  rate = tmp___1->min;

  tmp___2 = hw_param_interval(hw_params, 10);

  channels = tmp___2->min;

  fmt = find_format(subs, format, rate, channels);

  if (! fmt) {

    printk("<7>cannot set format: format = 0x%x, rate = %d, channels = %d\n", format,
           rate, channels);

    return (-22);
  }

  if ((unsigned long )subs->cur_audiofmt != (unsigned long )fmt) {

    tmp___8 = 1;
  } else {

    tmp___3 = hw_param_interval(hw_params, 13);

    tmp___4 = hw_param_mask(hw_params, 1);

    tmp___5 = snd_mask_min((struct snd_mask const *)tmp___4);

    tmp___6 = snd_pcm_format_physical_width((snd_pcm_format_t )tmp___5);

    tmp___7 = hw_param_interval(hw_params, 10);

    if (subs->period_bytes != ((tmp___3->min * (unsigned int )tmp___6) * tmp___7->min) / 8U) {

      tmp___8 = 1;
    } else

    if (subs->cur_rate != rate) {

      tmp___8 = 1;
    } else {

      tmp___8 = 0;
    }
  }

  changed = tmp___8;

  ret = set_format(subs, fmt);

  if (ret < 0) {

    return (ret);
  }

  if (subs->cur_rate != rate) {

    iface = usb_ifnum_to_if((struct usb_device const *)subs->dev, (unsigned int )fmt->iface);

    alts = iface->altsetting + fmt->altset_idx;

    ret = init_usb_sample_rate(subs->dev, subs->interface, alts, fmt, (int )rate);

    if (ret < 0) {

      return (ret);
    }

    subs->cur_rate = rate;
  }

  if (changed) {

    release_substream_urbs(subs, 0);

    tmp___9 = hw_param_mask(hw_params, 1);

    tmp___10 = snd_mask_min((struct snd_mask const *)tmp___9);

    tmp___11 = snd_pcm_format_physical_width((snd_pcm_format_t )tmp___10);

    tmp___12 = hw_param_interval(hw_params, 10);

    tmp___13 = hw_param_interval(hw_params, 11);

    tmp___14 = hw_param_interval(hw_params, 13);

    tmp___15 = hw_param_mask(hw_params, 1);

    tmp___16 = snd_mask_min((struct snd_mask const *)tmp___15);

    tmp___17 = snd_pcm_format_physical_width((snd_pcm_format_t )tmp___16);

    tmp___18 = hw_param_interval(hw_params, 10);

    ret = init_substream_urbs(subs, ((tmp___14->min * (unsigned int )tmp___17) * tmp___18->min) / 8U,
                              tmp___13->min, (unsigned int )tmp___11 * tmp___12->min);
  }

  return (ret);
}
}

static int snd_usb_hw_free(struct snd_pcm_substream *substream )
{
  struct snd_usb_substream *subs ;
  int tmp ;

  {

  subs = (struct snd_usb_substream *)(substream->runtime)->private_data;

  subs->cur_audiofmt = (struct audioformat *)((void *)0);

  subs->cur_rate = 0U;

  subs->period_bytes = 0U;

  if (! ((subs->stream)->chip)->shutdown) {

    release_substream_urbs(subs, 0);
  }

  tmp = snd_pcm_free_vmalloc_buffer(substream);

  return (tmp);
}
}

static int snd_usb_pcm_prepare(struct snd_pcm_substream *substream )
{
  struct snd_pcm_runtime *runtime ;
  struct snd_usb_substream *subs ;
  snd_pcm_sframes_t tmp ;
  snd_pcm_sframes_t tmp___0 ;
  int tmp___1 ;

  {

  runtime = (struct snd_pcm_runtime *)substream->runtime;

  subs = (struct snd_usb_substream *)runtime->private_data;

  if (! subs->cur_audiofmt) {

    printk("<3>usbaudio: no format is specified!\n");

    return (-6);
  }

  tmp = bytes_to_frames(runtime, (ssize_t )subs->maxpacksize);

  subs->maxframesize = (unsigned int )tmp;

  tmp___0 = bytes_to_frames(runtime, (ssize_t )subs->curpacksize);

  subs->curframesize = (unsigned int )tmp___0;

  subs->hwptr_done = 0U;

  subs->transfer_done = 0U;

  subs->phase = 0U;

  deactivate_urbs(subs, 0, 1);

  wait_clear_urbs(subs);

  if (subs->direction == 0) {

    subs->ops.prepare = & prepare_nodata_playback_urb;

    tmp___1 = start_urbs(subs, runtime);

    return (tmp___1);
  } else {

    return (0);
  }
}
}

static struct snd_pcm_hardware __attribute__((__noderef__, __address_space__(2))) snd_usb_hardware =

     {590099U, 0ULL, 0U, 0U, 0U, 0U, 0U, (size_t )1048576, (size_t )64, (size_t )524288,
    2U, 1024U, 0UL};

static int hw_check_valid_format(struct snd_pcm_hw_params *params , struct audioformat *fp )
{
  struct snd_interval *it ;
  struct snd_interval *tmp ;
  struct snd_interval *ct ;
  struct snd_interval *tmp___0 ;
  struct snd_mask *fmts ;
  struct snd_mask *tmp___1 ;
  int tmp___2 ;

  {

  tmp = hw_param_interval(params, 11);

  it = tmp;

  tmp___0 = hw_param_interval(params, 10);

  ct = tmp___0;

  tmp___1 = hw_param_mask(params, 1);

  fmts = tmp___1;

  tmp___2 = snd_mask_test((struct snd_mask const *)fmts, (unsigned int )fp->format);

  if (! tmp___2) {

    return (0);
  }

  if (fp->channels < ct->min) {

    return (0);
  } else

  if (fp->channels > ct->max) {

    return (0);
  }

  if (fp->rate_min > it->max) {

    return (0);
  } else

  if (fp->rate_min == it->max) {

    if (it->openmax) {

      return (0);
    }
  }

  if (fp->rate_max < it->min) {

    return (0);
  } else

  if (fp->rate_max == it->min) {

    if (it->openmin) {

      return (0);
    }
  }

  return (1);
}
}

static int hw_rule_rate(struct snd_pcm_hw_params *params , struct snd_pcm_hw_rule *rule )
{
  struct snd_usb_substream *subs ;
  struct list_head *p ;
  struct snd_interval *it ;
  struct snd_interval *tmp ;
  unsigned int rmin ;
  unsigned int rmax ;
  int changed ;
  struct audioformat *fp ;
  struct list_head const *__mptr ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;

  {

  subs = (struct snd_usb_substream *)rule->private;

  tmp = hw_param_interval(params, 11);

  it = tmp;

  changed = 0;

  rmax = 0U;

  rmin = rmax;

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    tmp___0 = hw_check_valid_format(params, fp);

    if (! tmp___0) {

      goto __Cont;
    }

    tmp___1 = changed;

    changed ++;

    if (tmp___1) {

      if (rmin > fp->rate_min) {

        rmin = fp->rate_min;
      }

      if (rmax < fp->rate_max) {

        rmax = fp->rate_max;
      }
    } else {

      rmin = fp->rate_min;

      rmax = fp->rate_max;
    }
    __Cont:

    p = (struct list_head *)p->next;
  }

  if (! changed) {

    it->empty = 1U;

    return (-22);
  }

  changed = 0;

  if (it->min < rmin) {

    it->min = rmin;

    it->openmin = 0U;

    changed = 1;
  }

  if (it->max > rmax) {

    it->max = rmax;

    it->openmax = 0U;

    changed = 1;
  }

  tmp___2 = snd_interval_checkempty((struct snd_interval const *)it);

  if (tmp___2) {

    it->empty = 1U;

    return (-22);
  }

  return (changed);
}
}

static int hw_rule_channels(struct snd_pcm_hw_params *params , struct snd_pcm_hw_rule *rule )
{
  struct snd_usb_substream *subs ;
  struct list_head *p ;
  struct snd_interval *it ;
  struct snd_interval *tmp ;
  unsigned int rmin ;
  unsigned int rmax ;
  int changed ;
  struct audioformat *fp ;
  struct list_head const *__mptr ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;

  {

  subs = (struct snd_usb_substream *)rule->private;

  tmp = hw_param_interval(params, 10);

  it = tmp;

  changed = 0;

  rmax = 0U;

  rmin = rmax;

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    tmp___0 = hw_check_valid_format(params, fp);

    if (! tmp___0) {

      goto __Cont;
    }

    tmp___1 = changed;

    changed ++;

    if (tmp___1) {

      if (rmin > fp->channels) {

        rmin = fp->channels;
      }

      if (rmax < fp->channels) {

        rmax = fp->channels;
      }
    } else {

      rmin = fp->channels;

      rmax = fp->channels;
    }
    __Cont:

    p = (struct list_head *)p->next;
  }

  if (! changed) {

    it->empty = 1U;

    return (-22);
  }

  changed = 0;

  if (it->min < rmin) {

    it->min = rmin;

    it->openmin = 0U;

    changed = 1;
  }

  if (it->max > rmax) {

    it->max = rmax;

    it->openmax = 0U;

    changed = 1;
  }

  tmp___2 = snd_interval_checkempty((struct snd_interval const *)it);

  if (tmp___2) {

    it->empty = 1U;

    return (-22);
  }

  return (changed);
}
}

static int hw_rule_format(struct snd_pcm_hw_params *params , struct snd_pcm_hw_rule *rule )
{
  struct snd_usb_substream *subs ;
  struct list_head *p ;
  struct snd_mask *fmt ;
  struct snd_mask *tmp ;
  u64 fbits ;
  u32 oldbits[2] ;
  int changed ;
  struct audioformat *fp ;
  struct list_head const *__mptr ;
  int tmp___0 ;
  int tmp___1 ;

  {

  subs = (struct snd_usb_substream *)rule->private;

  tmp = hw_param_mask(params, 1);

  fmt = tmp;

  fbits = (u64 )0;

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    tmp___0 = hw_check_valid_format(params, fp);

    if (! tmp___0) {

      goto __Cont;
    }

    fbits |= 1ULL << fp->format;
    __Cont:

    p = (struct list_head *)p->next;
  }

  oldbits[0] = fmt->bits[0];

  oldbits[1] = fmt->bits[1];

  fmt->bits[0] &= (u32 )fbits;

  fmt->bits[1] &= (u32 )(fbits >> 32);

  if (! fmt->bits[0]) {

    if (! fmt->bits[1]) {

      return (-22);
    }
  }

  if (oldbits[0] != fmt->bits[0]) {

    tmp___1 = 1;
  } else

  if (oldbits[1] != fmt->bits[1]) {

    tmp___1 = 1;
  } else {

    tmp___1 = 0;
  }

  changed = tmp___1;

  return (changed);
}
}

static int check_hw_params_convention(struct snd_usb_substream *subs )
{
  int i ;
  u32 *channels ;
  u32 *rates ;
  u32 cmaster ;
  u32 rmaster ;
  u32 rate_min ;
  u32 rate_max ;
  struct list_head *p ;
  int err ;
  void *tmp ;
  void *tmp___0 ;
  struct audioformat *f ;
  struct list_head const *__mptr ;
  struct audioformat *f___0 ;
  struct list_head const *__mptr___0 ;

  {

  rate_min = (u32 )0;

  rate_max = (u32 )0;

  err = 1;

  tmp = kcalloc((size_t )64, sizeof(u32 ), 208U);

  channels = (u32 *)tmp;

  tmp___0 = kcalloc((size_t )64, sizeof(u32 ), 208U);

  rates = (u32 *)tmp___0;

  if (! channels) {

    err = -12;

    goto __out;
  } else

  if (! rates) {

    err = -12;

    goto __out;
  }

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    f = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    if (f->channels > 32U) {

      goto __out;
    }

    if (f->rates & (unsigned int )(1 << 30)) {

      if (rate_min) {

        if (f->rate_min != rate_min) {

          goto __out;
        }
      }

      if (rate_max) {

        if (f->rate_max != rate_max) {

          goto __out;
        }
      }

      rate_min = f->rate_min;

      rate_max = f->rate_max;
    }

    if (*(rates + f->format) & (unsigned int )(1 << 30)) {

      if (f->rates != *(rates + f->format)) {

        goto __out;
      }
    }

    if (f->rates & (unsigned int )(1 << 30)) {

      if (*(rates + f->format)) {

        if (*(rates + f->format) != f->rates) {

          goto __out;
        }
      }
    }

    *(channels + f->format) |= (unsigned int )(1 << f->channels);

    *(rates + f->format) |= f->rates;

    if (f->rates & (unsigned int )(1 << 31)) {

      goto __out;
    }

    p = (struct list_head *)p->next;
  }

  rmaster = (u32 )0;

  cmaster = rmaster;

  i = 0;

  while (i < 64) {

    if (cmaster != *(channels + i)) {

      if (cmaster) {

        if (*(channels + i)) {

          goto __out;
        }
      }
    }

    if (rmaster != *(rates + i)) {

      if (rmaster) {

        if (*(rates + i)) {

          goto __out;
        }
      }
    }

    if (*(channels + i)) {

      cmaster = *(channels + i);
    }

    if (*(rates + i)) {

      rmaster = *(rates + i);
    }

    i ++;
  }

  memset((void *)channels, 0, 64UL * sizeof(u32 ));

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr___0 = (struct list_head const *)p;

    f___0 = (struct audioformat *)((char *)__mptr___0 - (unsigned int )(& ((struct audioformat *)0)->list));

    if (f___0->rates & (unsigned int )(1 << 30)) {

      goto __Cont;
    }

    i = 0;

    while (i < 32) {

      if (f___0->rates & (unsigned int )(1 << i)) {

        *(channels + i) |= (unsigned int )(1 << f___0->channels);
      }

      i ++;
    }
    __Cont:

    p = (struct list_head *)p->next;
  }

  cmaster = (u32 )0;

  i = 0;

  while (i < 32) {

    if (cmaster != *(channels + i)) {

      if (cmaster) {

        if (*(channels + i)) {

          goto __out;
        }
      }
    }

    if (*(channels + i)) {

      cmaster = *(channels + i);
    }

    i ++;
  }

  err = 0;
  __out:

  kfree((void const *)channels);

  kfree((void const *)rates);

  return (err);
}
}

static int snd_usb_pcm_check_knot(struct snd_pcm_runtime *runtime , struct snd_usb_substream *subs )
{
  struct audioformat *fp ;
  int count ;
  int needs_knot ;
  int err ;
  struct list_head const *__mptr ;
  struct list_head const *__mptr___0 ;
  void *tmp ;
  struct list_head const *__mptr___1 ;
  struct list_head const *__mptr___2 ;
  int i ;
  int tmp___0 ;

  {

  count = 0;

  needs_knot = 0;

  __mptr = (struct list_head const *)subs->fmt_list.next;

  fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

  while (1) {

    __builtin_prefetch((void const *)fp->list.next);

    if (! ((unsigned long )(& fp->list) != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    if (fp->rates & (unsigned int )(1 << 30)) {

      return (0);
    }

    count = (int )((unsigned int )count + fp->nr_rates);

    if (fp->rates & (unsigned int )(1 << 31)) {

      needs_knot = 1;
    }

    __mptr___0 = (struct list_head const *)fp->list.next;

    fp = (struct audioformat *)((char *)__mptr___0 - (unsigned int )(& ((struct audioformat *)0)->list));
  }

  if (! needs_knot) {

    return (0);
  }

  subs->rate_list.count = (unsigned int )count;

  tmp = kmalloc(sizeof(int ) * (unsigned long )count, 208U);

  subs->rate_list.list = (unsigned int *)tmp;

  subs->rate_list.mask = 0U;

  count = 0;

  __mptr___1 = (struct list_head const *)subs->fmt_list.next;

  fp = (struct audioformat *)((char *)__mptr___1 - (unsigned int )(& ((struct audioformat *)0)->list));

  while (1) {

    __builtin_prefetch((void const *)fp->list.next);

    if (! ((unsigned long )(& fp->list) != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    i = 0;

    while ((unsigned int )i < fp->nr_rates) {

      tmp___0 = count;

      count ++;

      *(subs->rate_list.list + tmp___0) = *(fp->rate_table + i);

      i ++;
    }

    __mptr___2 = (struct list_head const *)fp->list.next;

    fp = (struct audioformat *)((char *)__mptr___2 - (unsigned int )(& ((struct audioformat *)0)->list));
  }

  err = snd_pcm_hw_constraint_list(runtime, 0U, 11, & subs->rate_list);

  if (err < 0) {

    return (err);
  }

  return (0);
}
}

extern int ( snd_pcm_hw_rule_add_MJR2)() ;

static int setup_hw_info(struct snd_pcm_runtime *runtime , struct snd_usb_substream *subs )
{
  struct list_head *p ;
  int err ;
  struct audioformat *fp ;
  struct list_head const *__mptr ;
  size_t tmp ;

  {

  runtime->hw.formats = subs->formats;

  runtime->hw.rate_min = 2147483647U;

  runtime->hw.rate_max = 0U;

  runtime->hw.channels_min = 256U;

  runtime->hw.channels_max = 0U;

  runtime->hw.rates = 0U;

  p = (struct list_head *)subs->fmt_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& subs->fmt_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    runtime->hw.rates |= fp->rates;

    if (runtime->hw.rate_min > fp->rate_min) {

      runtime->hw.rate_min = fp->rate_min;
    }

    if (runtime->hw.rate_max < fp->rate_max) {

      runtime->hw.rate_max = fp->rate_max;
    }

    if (runtime->hw.channels_min > fp->channels) {

      runtime->hw.channels_min = fp->channels;
    }

    if (runtime->hw.channels_max < fp->channels) {

      runtime->hw.channels_max = fp->channels;
    }

    if (fp->fmt_type == 2U) {

      if (fp->frame_size > 0U) {

        tmp = (size_t )fp->frame_size;

        runtime->hw.period_bytes_max = tmp;

        runtime->hw.period_bytes_min = tmp;
      }
    }

    p = (struct list_head *)p->next;
  }

  snd_pcm_hw_constraint_minmax(runtime, 12, 1000U, ~ 0U);

  err = check_hw_params_convention(subs);

  if (err < 0) {

    return (err);
  } else

  if (err) {

    err = snd_pcm_hw_rule_add_MJR2(runtime, 0, 11, & hw_rule_rate, subs, 1, 10, -1);

    if (err < 0) {

      return (err);
    }

    err = snd_pcm_hw_rule_add_MJR2(runtime, 0, 10, & hw_rule_channels, subs, 1, 11,
                                   -1);

    if (err < 0) {

      return (err);
    }

    err = snd_pcm_hw_rule_add_MJR2(runtime, 0, 1, & hw_rule_format, subs, 11, 10,
                                   -1);

    if (err < 0) {

      return (err);
    }

    err = snd_usb_pcm_check_knot(runtime, subs);

    if (err < 0) {

      return (err);
    }
  }

  return (0);
}
}

static int snd_usb_pcm_open(struct snd_pcm_substream *substream , int direction )
{
  struct snd_usb_stream *as ;
  struct snd_pcm_runtime *runtime ;
  struct snd_usb_substream *subs ;
  int tmp ;

  {

  as = (struct snd_usb_stream *)substream->private_data;

  runtime = (struct snd_pcm_runtime *)substream->runtime;

  subs = & as->substream[direction];

  subs->interface = -1;

  subs->format = 0U;

  runtime->hw = (struct snd_pcm_hardware )snd_usb_hardware;

  runtime->private_data = (void * __attribute__((__recursive__)) )subs;

  subs->pcm_substream = (struct snd_pcm_substream * __attribute__((__recursive__)) )substream;

  tmp = setup_hw_info(runtime, subs);

  return (tmp);
}
}

static int snd_usb_pcm_close(struct snd_pcm_substream *substream , int direction )
{
  struct snd_usb_stream *as ;
  struct snd_usb_substream *subs ;

  {

  as = (struct snd_usb_stream *)substream->private_data;

  subs = & as->substream[direction];

  if (subs->interface >= 0) {

    usb_set_interface(subs->dev, subs->interface, 0);

    subs->interface = -1;
  }

  subs->pcm_substream = (struct snd_pcm_substream * __attribute__((__recursive__)) )((void *)0);

  return (0);
}
}

static int snd_usb_playback_open(struct snd_pcm_substream *substream )
{
  int tmp ;

  {

  tmp = snd_usb_pcm_open(substream, 0);

  return (tmp);
}
}

static int snd_usb_playback_close(struct snd_pcm_substream *substream )
{
  int tmp ;

  {

  tmp = snd_usb_pcm_close(substream, 0);

  return (tmp);
}
}

static int snd_usb_capture_open(struct snd_pcm_substream *substream )
{
  int tmp ;

  {

  tmp = snd_usb_pcm_open(substream, 1);

  return (tmp);
}
}

static int snd_usb_capture_close(struct snd_pcm_substream *substream )
{
  int tmp ;

  {

  tmp = snd_usb_pcm_close(substream, 1);

  return (tmp);
}
}

static struct snd_pcm_ops __attribute__((__noderef__, __address_space__(2))) snd_usb_playback_ops =

     {& snd_usb_playback_open, & snd_usb_playback_close, & snd_pcm_lib_ioctl, & snd_usb_hw_params,
    & snd_usb_hw_free, & snd_usb_pcm_prepare, & snd_usb_pcm_playback_trigger, & snd_usb_pcm_pointer,
    (int (*)(struct snd_pcm_substream *substream , int channel , snd_pcm_uframes_t pos ,
             void *buf , snd_pcm_uframes_t count ))0, (int (*)(struct snd_pcm_substream *substream ,
                                                               int channel , snd_pcm_uframes_t pos ,
                                                               snd_pcm_uframes_t count ))0,
    & snd_pcm_get_vmalloc_page, (int (*)(struct snd_pcm_substream *substream , struct vm_area_struct *vma ))0,
    (int (*)(struct snd_pcm_substream *substream ))0};

static struct snd_pcm_ops __attribute__((__noderef__, __address_space__(2))) snd_usb_capture_ops =

     {& snd_usb_capture_open, & snd_usb_capture_close, & snd_pcm_lib_ioctl, & snd_usb_hw_params,
    & snd_usb_hw_free, & snd_usb_pcm_prepare, & snd_usb_pcm_capture_trigger, & snd_usb_pcm_pointer,
    (int (*)(struct snd_pcm_substream *substream , int channel , snd_pcm_uframes_t pos ,
             void *buf , snd_pcm_uframes_t count ))0, (int (*)(struct snd_pcm_substream *substream ,
                                                               int channel , snd_pcm_uframes_t pos ,
                                                               snd_pcm_uframes_t count ))0,
    & snd_pcm_get_vmalloc_page, (int (*)(struct snd_pcm_substream *substream , struct vm_area_struct *vma ))0,
    (int (*)(struct snd_pcm_substream *substream ))0};

unsigned int snd_usb_combine_bytes(unsigned char *bytes , int size )
{


  {

  switch (size) {
  case 1:

  return ((unsigned int )*bytes);
  case 2:

  return ((unsigned int )*bytes | ((unsigned int )*(bytes + 1) << 8));
  case 3:

  return (((unsigned int )*bytes | ((unsigned int )*(bytes + 1) << 8)) | ((unsigned int )*(bytes + 2) << 16));
  case 4:

  return ((((unsigned int )*bytes | ((unsigned int )*(bytes + 1) << 8)) | ((unsigned int )*(bytes + 2) << 16)) | ((unsigned int )*(bytes + 3) << 24));
  default:

  return (0U);
  }
}
}

void *snd_usb_find_desc(void *descstart , int desclen , void *after , u8 dtype )
{
  u8 *p ;
  u8 *end ;
  u8 *next ;

  {

  p = (u8 *)descstart;

  end = p + desclen;

  while ((unsigned long )p < (unsigned long )end) {

    if ((int )*(p + 0) < 2) {

      return ((void *)0);
    }

    next = p + (int )*(p + 0);

    if ((unsigned long )next > (unsigned long )end) {

      return ((void *)0);
    }

    if ((int )*(p + 1) == (int )dtype) {

      if (! after) {

        return ((void *)p);
      } else

      if ((unsigned long )((void *)p) > (unsigned long )after) {

        return ((void *)p);
      }
    }

    p = next;
  }

  return ((void *)0);
}
}

void *snd_usb_find_csint_desc(void *buffer , int buflen , void *after , u8 dsubtype )
{
  unsigned char *p ;
  void *tmp ;

  {

  p = (unsigned char *)after;

  while (1) {

    tmp = snd_usb_find_desc(buffer, buflen, (void *)p, (u8 )((1 << 5) | 4));

    p = (unsigned char *)tmp;

    if (! ((unsigned long )p != (unsigned long )((void *)0))) {

      break;
    }

    if ((int )*(p + 0) >= 3) {

      if ((int )*(p + 2) == (int )dsubtype) {

        return ((void *)p);
      }
    }
  }

  return ((void *)0);
}
}

int snd_usb_ctl_msg(struct usb_device *dev , unsigned int pipe , __u8 request , __u8 requesttype ,
                    __u16 value , __u16 index___0 , void *data , __u16 size , int timeout )
{
  int err ;
  void *buf ;
  size_t __len ;
  void *__ret ;

  {

  buf = (void *)0;

  if ((int )size > 0) {

    buf = kmemdup((void const *)data, (size_t )size, 208U);

    if (! buf) {

      return (-12);
    }
  }

  err = usb_control_msg(dev, pipe, request, requesttype, value, index___0, buf, size,
                        timeout);

  if ((int )size > 0) {

    __len = (size_t )size;

    __ret = __builtin_memcpy(data, (void const *)buf, __len);

    kfree((void const *)buf);
  }

  return (err);
}
}

static int usb_audio_probe(struct usb_interface * __attribute__((__extraptr__(sizeof(struct usb_device *),"Nonstub_get_usb_device"))) intf ,
                           struct usb_device_id const *id___0 ) ;

static void usb_audio_disconnect(struct usb_interface * __attribute__((__noderef__,
                                 __address_space__(2))) intf ) ;

static int usb_audio_suspend(struct usb_interface *intf , pm_message_t message ) ;

static int usb_audio_resume(struct usb_interface *intf ) ;

static struct snd_usb_audio_quirk const __constr_expr_0 = {(char const * __attribute__((__nullterm__)) )"Creative Labs", (char const * __attribute__((__recursive__)) )"Sound Blaster MP3+",
    (int16_t )-2, (unsigned short)0, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_1 = {(char const * __attribute__((__nullterm__)) )"Logitech, Inc.", (char const * __attribute__((__recursive__)) )"QuickCam Pro 9000",
    (int16_t )-2, (unsigned short)0, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_2 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UX256",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_3 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MU1000",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_4 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MU2000",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_5 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MU500",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_6 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UW500",
    (int16_t )3, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_7 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF6",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_8 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF7",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_9 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF8",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_10 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UX96",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_11 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UX16",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_12 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"EOS BX",
    (int16_t )3, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_13 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UC-MX",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_14 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UC-KX",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_15 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"S08",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_16 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CLP-150",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_17 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CLP-170",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_18 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"P-250",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_19 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"TYROS",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_20 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PF-500",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_21 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"S90",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_22 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF-R",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_23 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MDP-5",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_24 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-204",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_25 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-206",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_26 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-208",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_27 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-210",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_28 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PSR-1100",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_29 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PSR-2100",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_30 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CLP-175",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_31 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PSR-K1",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_32 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"EZ-J24",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_33 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"EZ-250i",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_34 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF ES 6",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_35 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF ES 7",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_36 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"MOTIF ES 8",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_37 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-301",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_38 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-303",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_39 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-305",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_40 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-307",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_41 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-309",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_42 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CVP-309GP",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_43 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PSR-1500",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_44 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PSR-3000",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_45 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"ELS-01/01C",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_46 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PSR-295/293",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_47 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DGX-205/203",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_48 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DGX-305",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_49 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DGX-505",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_50 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_51 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_52 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_53 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_54 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_55 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_56 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_57 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_58 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_59 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_60 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_61 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_62 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_63 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_64 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_65 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_66 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_67 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_68 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )0, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_69 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_70 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_71 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_72 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_73 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DGP-7",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_74 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DGP-5",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_75 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_76 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"CS1D",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_77 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DSP1D",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_78 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DME32",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_79 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DM2000",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_80 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"02R96",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_81 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"ACU16-C",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_82 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"NHB32-C",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_83 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DM1000",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_84 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"01V96",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_85 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"SPX2000",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_86 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"PM5D",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_87 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DME64N",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_88 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DME24N",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_89 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_90 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_91 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )((void *)0),
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_92 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"DTX",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_93 = {(char const * __attribute__((__nullterm__)) )"Yamaha", (char const * __attribute__((__recursive__)) )"UB99",
    (int16_t )-1, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct audioformat const __constr_expr_96 =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    2, 4U, 0U, 0U, 0, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)1,
    (unsigned char)9, 0U, (unsigned int )(1 << 30), 44100U, 44100U, 0U, (unsigned int *)0};

static struct audioformat const __constr_expr_97 =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    2, 2U, 0U, 0U, 1, (unsigned char)1, (unsigned char)1, (unsigned char)128, (unsigned char)129,
    (unsigned char)5, 0U, (unsigned int )(1 << 30), 44100U, 44100U, 0U, (unsigned int *)0};

static struct snd_usb_midi_endpoint_info const __constr_expr_98 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )7,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_95[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )12, (void const * __attribute__((__recursive__)) )(& __constr_expr_96)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )12,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_97)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_98)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_94 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"UA-100",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_95)};

static struct snd_usb_midi_endpoint_info const __constr_expr_101 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )15,
    (uint16_t )15};

static struct snd_usb_audio_quirk const __constr_expr_100[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_101)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_99 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-4",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_100)};

static struct snd_usb_midi_endpoint_info const __constr_expr_104 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )63,
    (uint16_t )63};

static struct snd_usb_audio_quirk const __constr_expr_103[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_104)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_102 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SC-8850",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_103)};

static struct snd_usb_midi_endpoint_info const __constr_expr_107 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )5,
    (uint16_t )5};

static struct snd_usb_audio_quirk const __constr_expr_106[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_107)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_105 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"U-8",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_106)};

static struct snd_usb_midi_endpoint_info const __constr_expr_110 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )3};

static struct snd_usb_audio_quirk const __constr_expr_109[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_110)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_108 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-2",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_109)};

static struct snd_usb_midi_endpoint_info const __constr_expr_113 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )19,
    (uint16_t )19};

static struct snd_usb_audio_quirk const __constr_expr_112[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_113)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_111 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SC-8820",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_112)};

static struct snd_usb_midi_endpoint_info const __constr_expr_116 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_115[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_116)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_114 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"PC-300",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_115)};

static struct snd_usb_midi_endpoint_info const __constr_expr_119 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_118[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_119)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_117 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-1",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_118)};

static struct snd_usb_midi_endpoint_info const __constr_expr_122 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )19,
    (uint16_t )19};

static struct snd_usb_audio_quirk const __constr_expr_121[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_122)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_120 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SK-500",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_121)};

static struct audioformat const __constr_expr_125 =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    32, 2U, 0U, 0U, 0, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)1,
    (unsigned char)1, 0U, (unsigned int )(1 << 30), 44100U, 44100U, 0U, (unsigned int *)0};

static struct audioformat const __constr_expr_126 =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    32, 2U, 0U, 0U, 1, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)129,
    (unsigned char)1, 0U, (unsigned int )(1 << 30), 44100U, 44100U, 0U, (unsigned int *)0};

static struct snd_usb_midi_endpoint_info const __constr_expr_127 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )7,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_124[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )12, (void const * __attribute__((__recursive__)) )(& __constr_expr_125)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )12,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_126)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_127)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_123 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SC-D70",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_124)};

static struct snd_usb_audio_quirk const __constr_expr_129[3] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_128 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-5",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_129)};

static struct snd_usb_midi_endpoint_info const __constr_expr_131 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_130 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"XV-5050",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_131)};

static struct snd_usb_midi_endpoint_info const __constr_expr_133 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )511,
    (uint16_t )511};

static struct snd_usb_audio_quirk const __constr_expr_132 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-880",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_133)};

static struct snd_usb_midi_endpoint_info const __constr_expr_136 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )15,
    (uint16_t )15};

static struct snd_usb_audio_quirk const __constr_expr_135[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_136)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_134 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"SD-90",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_135)};

static struct snd_usb_midi_endpoint_info const __constr_expr_139 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_138[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_139)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_137 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"MMP-2",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_138)};

static struct snd_usb_midi_endpoint_info const __constr_expr_141 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_140 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"V-SYNTH",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_141)};

static struct snd_usb_midi_endpoint_info const __constr_expr_143 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )63,
    (uint16_t )63};

static struct snd_usb_audio_quirk const __constr_expr_142 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-550",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_143)};

static struct audioformat const __constr_expr_146 =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    32, 2U, 0U, 0U, 1, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)1,
    (unsigned char)1, 0U, (unsigned int )(1 << 30), 44100U, 44100U, 0U, (unsigned int *)0};

static struct audioformat const __constr_expr_147 =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    32, 2U, 0U, 0U, 2, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)130,
    (unsigned char)1, 0U, (unsigned int )(1 << 30), 44100U, 44100U, 0U, (unsigned int *)0};

static struct snd_usb_midi_endpoint_info const __constr_expr_148 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_145[5] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )12,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_146)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )12, (void const * __attribute__((__recursive__)) )(& __constr_expr_147)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )3, (uint16_t )3,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_148)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_144 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-20",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_145)};

static struct snd_usb_midi_endpoint_info const __constr_expr_150 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_149 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"SD-20",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_150)};

static struct snd_usb_midi_endpoint_info const __constr_expr_152 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )15,
    (uint16_t )15};

static struct snd_usb_audio_quirk const __constr_expr_151 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"SD-80",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_152)};

static struct snd_usb_audio_quirk const __constr_expr_154[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )15,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )3, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_153 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-700",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_154)};

static struct snd_usb_midi_endpoint_info const __constr_expr_156 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_155 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"XV-2020",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_156)};

static struct snd_usb_midi_endpoint_info const __constr_expr_158 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )7,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_157 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"VariOS",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_158)};

static struct snd_usb_midi_endpoint_info const __constr_expr_160 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_159 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"PCR",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_160)};

static struct snd_usb_midi_endpoint_info const __constr_expr_162 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_161 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"Digital Piano",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_162)};

static struct snd_usb_audio_quirk const __constr_expr_164[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )3, (uint16_t )2, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_163 = {(char const * __attribute__((__nullterm__)) )"BOSS", (char const * __attribute__((__recursive__)) )"GS-10",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(& __constr_expr_164)};

static struct snd_usb_midi_endpoint_info const __constr_expr_166 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_165 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"GI-20",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_166)};

static struct snd_usb_midi_endpoint_info const __constr_expr_168 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_167 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"RS-70",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_168)};

static struct snd_usb_midi_endpoint_info const __constr_expr_171 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )3};

static struct snd_usb_audio_quirk const __constr_expr_170[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )13, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )13,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )3, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_171)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_169 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"UA-1000",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_170)};

static struct snd_usb_audio_quirk const __constr_expr_173[3] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_172 = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_173)};

static struct snd_usb_midi_endpoint_info const __constr_expr_175 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_174 = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_175)};

static struct snd_usb_audio_quirk const __constr_expr_177[3] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_176 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"PCR-A",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_177)};

static struct snd_usb_midi_endpoint_info const __constr_expr_179 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )7};

static struct snd_usb_audio_quirk const __constr_expr_178 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"PCR-A",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_179)};

static struct snd_usb_audio_quirk const __constr_expr_181[3] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )1, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )2, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_180 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-3FX",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_181)};

static struct snd_usb_audio_quirk const __constr_expr_182 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-1SX",
    (int16_t )0, (uint16_t )2, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_183 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"EXR Series",
    (int16_t )0, (uint16_t )2, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_midi_endpoint_info const __constr_expr_185 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )3};

static struct snd_usb_audio_quirk const __constr_expr_184 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"PCR-1",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_185)};

static struct snd_usb_midi_endpoint_info const __constr_expr_187 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_186 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SP-606",
    (int16_t )3, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_187)};

static struct snd_usb_midi_endpoint_info const __constr_expr_189 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_188 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"FANTOM-X",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_189)};

static struct snd_usb_audio_quirk const __constr_expr_191[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )15,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_190 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-25",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_191)};

static struct snd_usb_midi_endpoint_info const __constr_expr_193 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_192 = {(char const * __attribute__((__nullterm__)) )"BOSS", (char const * __attribute__((__recursive__)) )"DR-880",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_193)};

static struct snd_usb_midi_endpoint_info const __constr_expr_195 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )3};

static struct snd_usb_audio_quirk const __constr_expr_194 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )0,
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_195)};

static struct snd_usb_midi_endpoint_info const __constr_expr_198 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_197[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )14, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )14,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_198)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_196 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"UA-101",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_197)};

static struct snd_usb_midi_endpoint_info const __constr_expr_200 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_199 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"G-70",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_200)};

static struct snd_usb_midi_endpoint_info const __constr_expr_202 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_201 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"PC-50",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_202)};

static struct snd_usb_audio_quirk const __constr_expr_204[3] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_203 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-1EX",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_204)};

static struct snd_usb_midi_endpoint_info const __constr_expr_206 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )15,
    (uint16_t )15};

static struct snd_usb_audio_quirk const __constr_expr_205 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UM-3EX",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_206)};

static struct snd_usb_audio_quirk const __constr_expr_208[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )15,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_207 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-4FX",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_208)};

static struct snd_usb_midi_endpoint_info const __constr_expr_210 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_209 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"Juno-G",
    (int16_t )0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_210)};

static struct snd_usb_midi_endpoint_info const __constr_expr_213 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_212[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_213)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_211 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SH-201",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_212)};

static struct snd_usb_midi_endpoint_info const __constr_expr_216 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_215[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_216)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_214 = {(char const * __attribute__((__nullterm__)) )"Roland", (char const * __attribute__((__recursive__)) )"SonicCell",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_215)};

static struct snd_usb_midi_endpoint_info const __constr_expr_219 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_218[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_219)},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_217 = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_218)};

static struct snd_usb_audio_quirk const __constr_expr_221[4] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )15,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )15, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )-1, (unsigned short)0,
      (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_220 = {(char const * __attribute__((__nullterm__)) )"EDIROL", (char const * __attribute__((__recursive__)) )"UA-25EX",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(__constr_expr_221)};

static struct snd_usb_midi_endpoint_info const __constr_expr_223 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_222 = {(char const * __attribute__((__nullterm__)) )"Hercules", (char const * __attribute__((__recursive__)) )"DJ Console (WE)",
    (int16_t )4, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& __constr_expr_223)};

static struct snd_usb_midi_endpoint_info const __constr_expr_225 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )3};

static struct snd_usb_audio_quirk const __constr_expr_224 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"MidiSport 2x2",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_225)};

static struct snd_usb_midi_endpoint_info const __constr_expr_227 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_226 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"MidiSport 1x1",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_227)};

static struct snd_usb_midi_endpoint_info const __constr_expr_229 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_228 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"Keystation",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_229)};

static struct snd_usb_midi_endpoint_info const __constr_expr_231 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )15,
    (uint16_t )15};

static struct snd_usb_audio_quirk const __constr_expr_230 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"MidiSport 4x4",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_231)};

static struct snd_usb_midi_endpoint_info const __constr_expr_233 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )511,
    (uint16_t )511};

static struct snd_usb_audio_quirk const __constr_expr_232 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"MidiSport 8x8",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_233)};

static struct snd_usb_midi_endpoint_info const __constr_expr_235 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )511,
    (uint16_t )511};

static struct snd_usb_audio_quirk const __constr_expr_234 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"MidiSport 8x8",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_235)};

static struct snd_usb_midi_endpoint_info const __constr_expr_237 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )15,
    (uint16_t )3};

static struct snd_usb_audio_quirk const __constr_expr_236 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"MidiSport 2x4",
    (int16_t )-1, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_237)};

static struct snd_usb_midi_endpoint_info const __constr_expr_240 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_239[11] =

  { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )3, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )4, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )5, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )6, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )7, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )8, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )9, (uint16_t )5,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_240)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_238 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"Quattro",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(& __constr_expr_239)};

static struct snd_usb_midi_endpoint_info const __constr_expr_242 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_241 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"AudioPhile",
    (int16_t )6, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_242)};

static struct snd_usb_midi_endpoint_info const __constr_expr_244 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_243 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"Ozone",
    (int16_t )3, (uint16_t )5, (void const * __attribute__((__recursive__)) )(& __constr_expr_244)};

static struct snd_usb_midi_endpoint_info const __constr_expr_247 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_246[11] =

  { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )3, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )4, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )5, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )6, (uint16_t )0, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )7, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )8, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )9, (uint16_t )5,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_247)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_245 = {(char const * __attribute__((__nullterm__)) )"M-Audio", (char const * __attribute__((__recursive__)) )"OmniStudio",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(& __constr_expr_246)};

static struct snd_usb_midi_endpoint_info const __constr_expr_250 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const __constr_expr_249[5] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )11,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )2, (uint16_t )11, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )3, (uint16_t )5,
      (void const * __attribute__((__recursive__)) )(& __constr_expr_250)},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_248 = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(& __constr_expr_249)};

static struct snd_usb_audio_quirk const __constr_expr_251 = {(char const * __attribute__((__nullterm__)) )"Casio", (char const * __attribute__((__recursive__)) )"PL-40R",
    (int16_t )0, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_252 = {(char const * __attribute__((__nullterm__)) )"Casio", (char const * __attribute__((__recursive__)) )"Keyboard",
    (int16_t )0, (uint16_t )4, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_254[3] = { {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )0, (uint16_t )7, (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0,
      (char const * __attribute__((__recursive__)) )0, (int16_t )1, (uint16_t )0,
      (void const * __attribute__((__recursive__)) )0},
        {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
      (int16_t )-1, (unsigned short)0, (void const * __attribute__((__recursive__)) )0}};

static struct snd_usb_audio_quirk const __constr_expr_253 = {(char const * __attribute__((__nullterm__)) )"MOTU", (char const * __attribute__((__recursive__)) )"Fastlane",
    (int16_t )-1, (uint16_t )1, (void const * __attribute__((__recursive__)) )(& __constr_expr_254)};

static struct snd_usb_midi_endpoint_info const __constr_expr_256 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )33023,
    (uint16_t )33023};

static struct snd_usb_audio_quirk const __constr_expr_255 = {(char const * __attribute__((__nullterm__)) )"Emagic", (char const * __attribute__((__recursive__)) )0,
    (int16_t )2, (uint16_t )8, (void const * __attribute__((__recursive__)) )(& __constr_expr_256)};

static struct snd_usb_midi_endpoint_info const __constr_expr_258 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )33023,
    (uint16_t )33023};

static struct snd_usb_audio_quirk const __constr_expr_257 = {(char const * __attribute__((__nullterm__)) )"Emagic", (char const * __attribute__((__recursive__)) )0,
    (int16_t )2, (uint16_t )8, (void const * __attribute__((__recursive__)) )(& __constr_expr_258)};

static struct snd_usb_midi_endpoint_info const __constr_expr_260 = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )32783,
    (uint16_t )32771};

static struct snd_usb_audio_quirk const __constr_expr_259 = {(char const * __attribute__((__nullterm__)) )"Emagic", (char const * __attribute__((__recursive__)) )0,
    (int16_t )2, (uint16_t )8, (void const * __attribute__((__recursive__)) )(& __constr_expr_260)};

static struct snd_usb_audio_quirk const __constr_expr_261 = {(char const * __attribute__((__nullterm__)) )"TerraTec", (char const * __attribute__((__recursive__)) )"PHASE 26",
    (int16_t )3, (uint16_t )2, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_262 = {(char const * __attribute__((__nullterm__)) )"TerraTec", (char const * __attribute__((__recursive__)) )"PHASE 26",
    (int16_t )3, (uint16_t )2, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_263 = {(char const * __attribute__((__nullterm__)) )"TerraTec", (char const * __attribute__((__recursive__)) )"PHASE 26",
    (int16_t )3, (uint16_t )2, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_264 = {(char const * __attribute__((__nullterm__)) )"Miditech", (char const * __attribute__((__recursive__)) )"Play\'n Roll",
    (int16_t )0, (uint16_t )9, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_265 = {(char const * __attribute__((__nullterm__)) )"Stanton", (char const * __attribute__((__recursive__)) )"ScratchAmp",
    (int16_t )-2, (unsigned short)0, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_266 = {(char const * __attribute__((__nullterm__)) )"Stanton", (char const * __attribute__((__recursive__)) )"ScratchAmp",
    (int16_t )-2, (unsigned short)0, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_267 = {(char const * __attribute__((__nullterm__)) )"Novation", (char const * __attribute__((__recursive__)) )"ReMOTE Audio/XStation",
    (int16_t )4, (uint16_t )6, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_268 = {(char const * __attribute__((__nullterm__)) )"Novation", (char const * __attribute__((__recursive__)) )"Speedio",
    (int16_t )3, (uint16_t )6, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_269 = {(char const * __attribute__((__nullterm__)) )"Novation", (char const * __attribute__((__recursive__)) )"ReMOTE25",
    (int16_t )0, (uint16_t )6, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_270 = {(char const * __attribute__((__nullterm__)) )"Rane", (char const * __attribute__((__recursive__)) )"SL-1",
    (int16_t )-2, (unsigned short)0, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_271 = {(char const * __attribute__((__nullterm__)) )"Miditech", (char const * __attribute__((__recursive__)) )"Midistart-2",
    (int16_t )0, (uint16_t )9, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_272 = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (int16_t )0, (uint16_t )9, (void const * __attribute__((__recursive__)) )0};

static struct snd_usb_audio_quirk const __constr_expr_273 = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (int16_t )-1, (uint16_t )2, (void const * __attribute__((__recursive__)) )0};

static struct usb_device_id usb_audio_ids[190] =

  { {(__u16 )3, (__u16 )1054, (__u16 )12304, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_0))},
        {(__u16 )3,
      (__u16 )1054, (__u16 )16130, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )1, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )3, (__u16 )1054, (__u16 )16132, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)0},
        {(__u16 )3, (__u16 )1054, (__u16 )16138, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)0},
        {(__u16 )387, (__u16 )1133, (__u16 )2128, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )387, (__u16 )1133, (__u16 )2222, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )387, (__u16 )1133, (__u16 )2246, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )387, (__u16 )1133, (__u16 )2288, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )387, (__u16 )1133, (__u16 )2293, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )387, (__u16 )1133, (__u16 )2294, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )3, (__u16 )1133, (__u16 )2448, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_1))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4096, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_2))},
        {(__u16 )3, (__u16 )1177, (__u16 )4097, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_3))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4098, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_4))},
        {(__u16 )3, (__u16 )1177, (__u16 )4099, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_5))},
        {(__u16 )131,
      (__u16 )1177, (__u16 )4100, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_6))},
        {(__u16 )3, (__u16 )1177, (__u16 )4101, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_7))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4102, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_8))},
        {(__u16 )3, (__u16 )1177, (__u16 )4103, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_9))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4104, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_10))},
        {(__u16 )3, (__u16 )1177, (__u16 )4105, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_11))},
        {(__u16 )131,
      (__u16 )1177, (__u16 )4106, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_12))},
        {(__u16 )3, (__u16 )1177, (__u16 )4108, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_13))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4109, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_14))},
        {(__u16 )3, (__u16 )1177, (__u16 )4110, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_15))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4111, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_16))},
        {(__u16 )3, (__u16 )1177, (__u16 )4112, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_17))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4113, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_18))},
        {(__u16 )3, (__u16 )1177, (__u16 )4114, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_19))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4115, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_20))},
        {(__u16 )3, (__u16 )1177, (__u16 )4116, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_21))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4117, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_22))},
        {(__u16 )3, (__u16 )1177, (__u16 )4118, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_23))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4119, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_24))},
        {(__u16 )3, (__u16 )1177, (__u16 )4120, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_25))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4121, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_26))},
        {(__u16 )3, (__u16 )1177, (__u16 )4122, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_27))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4123, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_28))},
        {(__u16 )3, (__u16 )1177, (__u16 )4124, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_29))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4125, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_30))},
        {(__u16 )3, (__u16 )1177, (__u16 )4126, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_31))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4127, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_32))},
        {(__u16 )3, (__u16 )1177, (__u16 )4128, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_33))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4129, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_34))},
        {(__u16 )3, (__u16 )1177, (__u16 )4130, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_35))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4131, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_36))},
        {(__u16 )3, (__u16 )1177, (__u16 )4132, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_37))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4133, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_38))},
        {(__u16 )3, (__u16 )1177, (__u16 )4134, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_39))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4135, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_40))},
        {(__u16 )3, (__u16 )1177, (__u16 )4136, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_41))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4137, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_42))},
        {(__u16 )3, (__u16 )1177, (__u16 )4138, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_43))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4139, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_44))},
        {(__u16 )3, (__u16 )1177, (__u16 )4142, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_45))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4144, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_46))},
        {(__u16 )3, (__u16 )1177, (__u16 )4145, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_47))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4146, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_48))},
        {(__u16 )3, (__u16 )1177, (__u16 )4147, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_49))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4148, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_50))},
        {(__u16 )3, (__u16 )1177, (__u16 )4149, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_51))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4150, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_52))},
        {(__u16 )3, (__u16 )1177, (__u16 )4151, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_53))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4152, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_54))},
        {(__u16 )3, (__u16 )1177, (__u16 )4153, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_55))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4154, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_56))},
        {(__u16 )3, (__u16 )1177, (__u16 )4155, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_57))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4156, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_58))},
        {(__u16 )3, (__u16 )1177, (__u16 )4157, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_59))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4158, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_60))},
        {(__u16 )3, (__u16 )1177, (__u16 )4159, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_61))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4160, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_62))},
        {(__u16 )3, (__u16 )1177, (__u16 )4161, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_63))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4162, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_64))},
        {(__u16 )3, (__u16 )1177, (__u16 )4163, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_65))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4164, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_66))},
        {(__u16 )3, (__u16 )1177, (__u16 )4165, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_67))},
        {(__u16 )131,
      (__u16 )1177, (__u16 )4174, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_68))},
        {(__u16 )3, (__u16 )1177, (__u16 )4175, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_69))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4176, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_70))},
        {(__u16 )3, (__u16 )1177, (__u16 )4177, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_71))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )4178, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_72))},
        {(__u16 )3, (__u16 )1177, (__u16 )8192, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_73))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )8193, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_74))},
        {(__u16 )3, (__u16 )1177, (__u16 )8194, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_75))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20480, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_76))},
        {(__u16 )3, (__u16 )1177, (__u16 )20481, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_77))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20482, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_78))},
        {(__u16 )3, (__u16 )1177, (__u16 )20483, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_79))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20484, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_80))},
        {(__u16 )3, (__u16 )1177, (__u16 )20485, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_81))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20486, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_82))},
        {(__u16 )3, (__u16 )1177, (__u16 )20487, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_83))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20488, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_84))},
        {(__u16 )3, (__u16 )1177, (__u16 )20489, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_85))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20490, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_86))},
        {(__u16 )3, (__u16 )1177, (__u16 )20491, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_87))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20492, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_88))},
        {(__u16 )3, (__u16 )1177, (__u16 )20493, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_89))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )20494, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_90))},
        {(__u16 )3, (__u16 )1177, (__u16 )20495, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_91))},
        {(__u16 )3,
      (__u16 )1177, (__u16 )28672, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_92))},
        {(__u16 )3, (__u16 )1177, (__u16 )28688, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_93))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )0, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_94))},
        {(__u16 )3, (__u16 )1410, (__u16 )2, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_99))},
        {(__u16 )3, (__u16 )1410, (__u16 )3, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_102))},
        {(__u16 )3, (__u16 )1410, (__u16 )4, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_105))},
        {(__u16 )3, (__u16 )1410, (__u16 )5, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_108))},
        {(__u16 )3, (__u16 )1410, (__u16 )7, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_111))},
        {(__u16 )3, (__u16 )1410, (__u16 )8, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_114))},
        {(__u16 )3, (__u16 )1410, (__u16 )9, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_117))},
        {(__u16 )3, (__u16 )1410, (__u16 )11, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_120))},
        {(__u16 )3, (__u16 )1410, (__u16 )12, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_123))},
        {(__u16 )3, (__u16 )1410, (__u16 )16, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_128))},
        {(__u16 )3, (__u16 )1410, (__u16 )18, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_130))},
        {(__u16 )3, (__u16 )1410, (__u16 )20, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_132))},
        {(__u16 )3, (__u16 )1410, (__u16 )22, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_134))},
        {(__u16 )3, (__u16 )1410, (__u16 )27, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_137))},
        {(__u16 )3, (__u16 )1410, (__u16 )29, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_140))},
        {(__u16 )3, (__u16 )1410, (__u16 )35, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_142))},
        {(__u16 )3, (__u16 )1410, (__u16 )37, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_144))},
        {(__u16 )3, (__u16 )1410, (__u16 )39, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_149))},
        {(__u16 )3, (__u16 )1410, (__u16 )41, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_151))},
        {(__u16 )131, (__u16 )1410, (__u16 )43, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_153))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )45, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_155))},
        {(__u16 )3, (__u16 )1410, (__u16 )47, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_157))},
        {(__u16 )3, (__u16 )1410, (__u16 )51, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_159))},
        {(__u16 )3, (__u16 )1410, (__u16 )55, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_161))},
        {(__u16 )131, (__u16 )1410, (__u16 )59, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_163))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )64, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_165))},
        {(__u16 )3, (__u16 )1410, (__u16 )66, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_167))},
        {(__u16 )3, (__u16 )1410, (__u16 )68, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_169))},
        {(__u16 )3, (__u16 )1410, (__u16 )71, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_172))},
        {(__u16 )3, (__u16 )1410, (__u16 )72, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_174))},
        {(__u16 )3, (__u16 )1410, (__u16 )76, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_176))},
        {(__u16 )3, (__u16 )1410, (__u16 )77, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_178))},
        {(__u16 )3, (__u16 )1410, (__u16 )80, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_180))},
        {(__u16 )3, (__u16 )1410, (__u16 )82, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_182))},
        {(__u16 )3, (__u16 )1410, (__u16 )96, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_183))},
        {(__u16 )3, (__u16 )1410, (__u16 )101, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_184))},
        {(__u16 )131,
      (__u16 )1410, (__u16 )106, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_186))},
        {(__u16 )3, (__u16 )1410, (__u16 )109, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_188))},
        {(__u16 )131,
      (__u16 )1410, (__u16 )116, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_190))},
        {(__u16 )3, (__u16 )1410, (__u16 )117, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_192))},
        {(__u16 )131,
      (__u16 )1410, (__u16 )122, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_194))},
        {(__u16 )3, (__u16 )1410, (__u16 )125, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_196))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )128, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_199))},
        {(__u16 )3, (__u16 )1410, (__u16 )139, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_201))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )150, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_203))},
        {(__u16 )3, (__u16 )1410, (__u16 )154, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_205))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )163, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_207))},
        {(__u16 )3, (__u16 )1410, (__u16 )166, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_209))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )173, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_211))},
        {(__u16 )3, (__u16 )1410, (__u16 )194, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_214))},
        {(__u16 )3,
      (__u16 )1410, (__u16 )218, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_217))},
        {(__u16 )131, (__u16 )1410, (__u16 )230, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_220))},
        {(__u16 )131,
      (__u16 )1784, (__u16 )45056, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_222))},
        {(__u16 )131, (__u16 )1891, (__u16 )4098, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_224))},
        {(__u16 )131,
      (__u16 )1891, (__u16 )4113, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_226))},
        {(__u16 )131, (__u16 )1891, (__u16 )4117, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_228))},
        {(__u16 )131,
      (__u16 )1891, (__u16 )4129, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_230))},
        {(__u16 )15, (__u16 )1891, (__u16 )4145, (__u16 )256, (__u16 )265, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_232))},
        {(__u16 )131, (__u16 )1891, (__u16 )4147, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_234))},
        {(__u16 )131,
      (__u16 )1891, (__u16 )4161, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_236))},
        {(__u16 )131, (__u16 )1891, (__u16 )8193, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_238))},
        {(__u16 )131,
      (__u16 )1891, (__u16 )8195, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_241))},
        {(__u16 )131, (__u16 )1891, (__u16 )8200, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_243))},
        {(__u16 )131,
      (__u16 )1891, (__u16 )8205, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_245))},
        {(__u16 )3, (__u16 )1891, (__u16 )8217, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_248))},
        {(__u16 )3,
      (__u16 )1999, (__u16 )26625, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_251))},
        {(__u16 )3, (__u16 )1999, (__u16 )26626, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_252))},
        {(__u16 )35,
      (__u16 )2045, (__u16 )1, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (__u8 )2, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_253))},
        {(__u16 )3, (__u16 )2154, (__u16 )1, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_255))},
        {(__u16 )3, (__u16 )2154, (__u16 )2, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_257))},
        {(__u16 )3, (__u16 )2154, (__u16 )3, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_259))},
        {(__u16 )131, (__u16 )3277, (__u16 )18, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_261))},
        {(__u16 )131,
      (__u16 )3277, (__u16 )19, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_262))},
        {(__u16 )131, (__u16 )3277, (__u16 )20, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_263))},
        {(__u16 )3,
      (__u16 )3277, (__u16 )53, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_264))},
        {(__u16 )3, (__u16 )4157, (__u16 )256, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_265))},
        {(__u16 )3,
      (__u16 )4157, (__u16 )257, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_266))},
        {(__u16 )131, (__u16 )4661, (__u16 )1, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_267))},
        {(__u16 )131,
      (__u16 )4661, (__u16 )2, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_268))},
        {(__u16 )131, (__u16 )4661, (__u16 )18017, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )255, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_269))},
        {(__u16 )3,
      (__u16 )5093, (__u16 )1, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_270))},
        {(__u16 )3, (__u16 )18258, (__u16 )17, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (unsigned char)0, (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_271))},
        {(__u16 )3,
      (__u16 )28932, (__u16 )8706, (unsigned short)0, (unsigned short)0, (unsigned char)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_272))},
        {(__u16 )384, (unsigned short)0, (unsigned short)0, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )3, (unsigned char)0,
      (struct snd_usb_audio_quirk *)((unsigned long )(& __constr_expr_273))},
        {(__u16 )384, (unsigned short)0, (unsigned short)0, (unsigned short)0, (unsigned short)0,
      (unsigned char)0, (unsigned char)0, (unsigned char)0, (__u8 )1, (__u8 )1, (unsigned char)0,
      (struct snd_usb_audio_quirk *)0},
        {(__u16 )0, (__u16 )0, (__u16 )0, (__u16 )0, (__u16 )0, (__u8 )0, (__u8 )0, (__u8 )0,
      (__u8 )0, (__u8 )0, (__u8 )0, (struct snd_usb_audio_quirk *)0}};

extern struct usb_device_id const __mod_usb_device_table __attribute__((__unused__,
__alias__("usb_audio_ids"))) ;

static struct usb_driver usb_audio_driver =

     {"snd-usb-audio", & usb_audio_probe, & usb_audio_disconnect, (int (*)(struct usb_interface *intf ,
                                                                         unsigned int code ,
                                                                         void *buf ))0,
    & usb_audio_suspend, & usb_audio_resume, (int (*)(struct usb_interface *intf ))0,
    (int (*)(struct usb_interface *intf ))0, (int (*)(struct usb_interface *intf ))0,
    (struct usb_device_id const *)(usb_audio_ids), {{{0U}, 0U, 0U, (void *)0, {(struct lock_class_key *)0,
                                                                                 (struct lock_class *)0,
                                                                                 (char const *)0}},
                                                      {(struct list_head * __attribute__((__recursive__)) )0,
                                                       (struct list_head * __attribute__((__recursive__)) )0}},
    {{(char const *)0, (struct bus_type *)0, (struct module *)0, (char const *)0,
      (int (*)(struct device *dev ))0, (int (*)(struct device *dev ))0, (void (*)(struct device *dev ))0,
      (int (*)(struct device *dev , pm_message_t state ))0, (int (*)(struct device *dev ))0,
      (struct attribute_group **)0, (struct dev_pm_ops *)0, (struct driver_private *)0},
     0}, 0U, 0U, 0U};

__inline static void ( __attribute__((__always_inline__)) proc_pcm_format_add)(struct snd_usb_stream *stream )
{


  {

  return;
}
}

static struct lock_class_key __key___5 ;

static void init_substream(struct snd_usb_stream *as , int stream , struct audioformat *fp )
{
  struct snd_usb_substream *subs ;
  struct snd_pcm_ops __attribute__((__noderef__, __address_space__(2))) *tmp ;

  {

  subs = & as->substream[stream];

  INIT_LIST_HEAD(& subs->fmt_list);

  while (1) {

    __spin_lock_init(& subs->lock, "&subs->lock", & __key___5);

    break;
  }

  subs->stream = (struct snd_usb_stream * __attribute__((__recursive__)) )as;

  subs->direction = stream;

  subs->dev = (as->chip)->dev;

  if ((unsigned int )(subs->dev)->speed == 2U) {

    subs->ops = audio_urb_ops[stream];
  } else {

    subs->ops = audio_urb_ops_high_speed[stream];

    switch ((int )(as->chip)->usb_id) {
    case (1054 << 16) | 16130:
    case (1054 << 16) | 16132:
    case (1054 << 16) | 16138:

    subs->ops.retire_sync = & retire_playback_sync_urb_hs_emu;

    break;
    }
  }

  if (stream == 0) {

    tmp = & snd_usb_playback_ops;
  } else {

    tmp = & snd_usb_capture_ops;
  }

  snd_pcm_set_ops((struct snd_pcm *)as->pcm, stream, (struct snd_pcm_ops *)tmp);

  list_add_tail(& fp->list, & subs->fmt_list);

  subs->formats |= 1ULL << fp->format;

  subs->endpoint = (int )fp->endpoint;

  (subs->num_formats) ++;

  subs->fmt_type = fp->fmt_type;

  return;
}
}

static void free_substream(struct snd_usb_substream *subs )
{
  struct list_head *p ;
  struct list_head *n ;
  struct audioformat *fp ;
  struct list_head const *__mptr ;

  {

  if (! subs->num_formats) {

    return;
  }

  p = (struct list_head *)subs->fmt_list.next;

  n = (struct list_head *)p->next;

  while ((unsigned long )p != (unsigned long )(& subs->fmt_list)) {

    __mptr = (struct list_head const *)p;

    fp = (struct audioformat *)((char *)__mptr - (unsigned int )(& ((struct audioformat *)0)->list));

    kfree((void const *)fp->rate_table);

    kfree((void const *)fp);

    p = n;

    n = (struct list_head *)p->next;
  }

  kfree((void const *)subs->rate_list.list);

  return;
}
}

static void snd_usb_audio_stream_free(struct snd_usb_stream *stream )
{


  {

  free_substream(& stream->substream[0]);

  free_substream(& stream->substream[1]);

  list_del(& stream->list);

  kfree((void const *)stream);

  return;
}
}

static void snd_usb_audio_pcm_free(struct snd_pcm *pcm )
{
  struct snd_usb_stream *stream ;

  {

  stream = (struct snd_usb_stream *)pcm->private_data;

  if (stream) {

    stream->pcm = (struct snd_pcm * __attribute__((__recursive__)) )((void *)0);

    snd_usb_audio_stream_free(stream);
  }

  return;
}
}

static int add_audio_endpoint(struct snd_usb_audio *chip , int stream , struct audioformat *fp )
{
  struct list_head *p ;
  struct snd_usb_stream *as ;
  struct snd_usb_substream *subs ;
  struct snd_pcm *pcm ;
  int err ;
  struct list_head const *__mptr ;
  struct list_head const *__mptr___0 ;
  void *tmp ;
  int tmp___0 ;
  int tmp___1 ;

  {

  p = (struct list_head *)chip->pcm_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& chip->pcm_list))) {

      break;
    }

    __mptr = (struct list_head const *)p;

    as = (struct snd_usb_stream *)((char *)__mptr - (unsigned int )(& ((struct snd_usb_stream *)0)->list));

    if (as->fmt_type != fp->fmt_type) {

      goto __Cont;
    }

    subs = & as->substream[stream];

    if (! subs->endpoint) {

      goto __Cont;
    }

    if (subs->endpoint == (int )fp->endpoint) {

      list_add_tail(& fp->list, & subs->fmt_list);

      (subs->num_formats) ++;

      subs->formats |= 1ULL << fp->format;

      return (0);
    }
    __Cont:

    p = (struct list_head *)p->next;
  }

  p = (struct list_head *)chip->pcm_list.next;

  while (1) {

    __builtin_prefetch((void const *)p->next);

    if (! ((unsigned long )p != (unsigned long )(& chip->pcm_list))) {

      break;
    }

    __mptr___0 = (struct list_head const *)p;

    as = (struct snd_usb_stream *)((char *)__mptr___0 - (unsigned int )(& ((struct snd_usb_stream *)0)->list));

    if (as->fmt_type != fp->fmt_type) {

      goto __Cont___0;
    }

    subs = & as->substream[stream];

    if (subs->endpoint) {

      goto __Cont___0;
    }

    err = snd_pcm_new_stream((struct snd_pcm *)as->pcm, stream, 1);

    if (err < 0) {

      return (err);
    }

    init_substream(as, stream, fp);

    return (0);
    __Cont___0:

    p = (struct list_head *)p->next;
  }

  tmp = kzalloc(sizeof(*as), 208U);

  as = (struct snd_usb_stream *)tmp;

  if (! as) {

    return (-12);
  }

  as->pcm_index = chip->pcm_devs;

  as->chip = chip;

  as->fmt_type = fp->fmt_type;

  if (stream == 0) {

    tmp___0 = 0;
  } else {

    tmp___0 = 1;
  }

  if (stream == 0) {

    tmp___1 = 1;
  } else {

    tmp___1 = 0;
  }

  err = snd_pcm_new(chip->card, (char * __attribute__((__nullterm__)) )"USB Audio",
                    chip->pcm_devs, tmp___1, tmp___0, & pcm);

  if (err < 0) {

    kfree((void const *)as);

    return (err);
  }

  as->pcm = (struct snd_pcm * __attribute__((__recursive__)) )pcm;

  pcm->private_data = (void * __attribute__((__recursive__)) )as;

  pcm->private_free = & snd_usb_audio_pcm_free;

  pcm->info_flags = 0U;

  if (chip->pcm_devs > 0) {

    sprintf(pcm->name, "USB Audio #%d", chip->pcm_devs);
  } else {

    strcpy(pcm->name, "USB Audio");
  }

  init_substream(as, stream, fp);

  list_add(& as->list, & chip->pcm_list);

  (chip->pcm_devs) ++;

  proc_pcm_format_add(as);

  return (0);
}
}

static int is_big_endian_format(struct snd_usb_audio *chip , struct audioformat *fp )
{


  {

  switch ((int )chip->usb_id) {
  case (1891 << 16) | 8193:

  if ((int )fp->endpoint & 128) {

    return (1);
  }

  break;
  case (1891 << 16) | 8195:

  if (device_setup[chip->index] == 0) {

    return (1);
  } else

  if ((int )fp->altsetting == 1) {

    return (1);
  } else

  if ((int )fp->altsetting == 2) {

    return (1);
  } else

  if ((int )fp->altsetting == 3) {

    return (1);
  }
  }

  return (0);
}
}

static int parse_audio_format_i_type(struct snd_usb_audio *chip , struct audioformat *fp ,
                                     int format , unsigned char *fmt )
{
  int pcm_format ;
  int sample_width ;
  int sample_bytes ;
  int tmp ;
  int tmp___0 ;

  {

  pcm_format = -1;

  sample_width = (int )*(fmt + 6);

  sample_bytes = (int )*(fmt + 5);

  switch (format) {
  case 0:

  printk("<6>%d:%u:%d : format type 0 is detected, processed as PCM\n", (chip->dev)->devnum,
         fp->iface, (int )fp->altsetting);
  case 1:

  if (sample_width > sample_bytes * 8) {

    printk("<6>%d:%u:%d : sample bitwidth %d in over sample bytes %d\n", (chip->dev)->devnum,
           fp->iface, (int )fp->altsetting, sample_width, sample_bytes);
  }

  switch ((int )*(fmt + 5)) {
  case 1:

  pcm_format = 0;

  break;
  case 2:

  tmp = is_big_endian_format(chip, fp);

  if (tmp) {

    pcm_format = 3;
  } else {

    pcm_format = 2;
  }

  break;
  case 3:

  tmp___0 = is_big_endian_format(chip, fp);

  if (tmp___0) {

    pcm_format = 33;
  } else {

    pcm_format = 32;
  }

  break;
  case 4:

  pcm_format = 10;

  break;
  default:

  printk("<6>%d:%u:%d : unsupported sample bitwidth %d in %d bytes\n", (chip->dev)->devnum,
         fp->iface, (int )fp->altsetting, sample_width, sample_bytes);

  break;
  }

  break;
  case 2:

  pcm_format = 1;

  if (chip->usb_id == (u32 )((1274 << 16) | 16897)) {

    pcm_format = 0;
  }

  break;
  case 3:

  pcm_format = 14;

  break;
  case 4:

  pcm_format = 21;

  break;
  case 5:

  pcm_format = 20;

  break;
  default:

  printk("<6>%d:%u:%d : unsupported format type %d\n", (chip->dev)->devnum, fp->iface,
         (int )fp->altsetting, format);

  break;
  }

  return (pcm_format);
}
}

static int parse_audio_format_rates(struct snd_usb_audio *chip , struct audioformat *fp ,
                                    unsigned char *fmt , int offset )
{
  int nr_rates ;
  int tmp ;
  int r ;
  int idx ;
  void *tmp___0 ;
  unsigned int tmp___1 ;
  unsigned int rate ;
  unsigned int tmp___2 ;

  {

  nr_rates = (int )*(fmt + offset);

  if (nr_rates) {

    tmp = nr_rates;
  } else {

    tmp = 2;
  }

  if ((int )*(fmt + 0) < (offset + 1) + 3 * tmp) {

    printk("<3>%d:%u:%d : invalid FORMAT_TYPE desc\n", (chip->dev)->devnum, fp->iface,
           (int )fp->altsetting);

    return (-1);
  }

  if (nr_rates) {

    tmp___0 = kmalloc(sizeof(int ) * (unsigned long )nr_rates, 208U);

    fp->rate_table = (unsigned int *)tmp___0;

    if ((unsigned long )fp->rate_table == (unsigned long )((void *)0)) {

      printk("<3>cannot malloc\n");

      return (-1);
    }

    fp->nr_rates = 0U;

    tmp___1 = 0U;

    fp->rate_max = tmp___1;

    fp->rate_min = tmp___1;

    r = 0;

    idx = offset + 1;

    while (r < nr_rates) {

      rate = ((unsigned int )*(fmt + idx) | ((unsigned int )*((fmt + idx) + 1) << 8)) | ((unsigned int )*((fmt + idx) + 2) << 16);

      if (! rate) {

        goto __Cont;
      }

      if (rate == 48000U) {

        if (nr_rates == 1) {

          if (chip->usb_id == (u32 )((3468 << 16) | 513)) {

            goto _L;
          } else

          if (chip->usb_id == (u32 )((3468 << 16) | 258)) {
            _L:

            if ((int )fp->altsetting == 5) {

              if (fp->maxpacksize == 392U) {

                rate = 96000U;
              }
            }
          }
        }
      }

      *(fp->rate_table + fp->nr_rates) = rate;

      if (! fp->rate_min) {

        fp->rate_min = rate;
      } else

      if (rate < fp->rate_min) {

        fp->rate_min = rate;
      }

      if (! fp->rate_max) {

        fp->rate_max = rate;
      } else

      if (rate > fp->rate_max) {

        fp->rate_max = rate;
      }

      tmp___2 = snd_pcm_rate_to_rate_bit(rate);

      fp->rates |= tmp___2;

      (fp->nr_rates) ++;
      __Cont:

      r ++;

      idx += 3;
    }

    if (! fp->nr_rates) {

      return (-1);
    }
  } else {

    fp->rates = (unsigned int )(1 << 30);

    fp->rate_min = ((unsigned int )*(fmt + (offset + 1)) | ((unsigned int )*((fmt + (offset + 1)) + 1) << 8)) | ((unsigned int )*((fmt + (offset + 1)) + 2) << 16);

    fp->rate_max = ((unsigned int )*(fmt + (offset + 4)) | ((unsigned int )*((fmt + (offset + 4)) + 1) << 8)) | ((unsigned int )*((fmt + (offset + 4)) + 2) << 16);
  }

  return (0);
}
}

static int parse_audio_format_i(struct snd_usb_audio *chip , struct audioformat *fp ,
                                int format , unsigned char *fmt )
{
  int pcm_format ;
  int tmp ;

  {

  if ((int )*(fmt + 3) == 3) {

    switch ((int )chip->usb_id) {
    case (1891 << 16) | 8195:

    if (device_setup[chip->index] == 0) {

      if ((int )fp->altsetting == 6) {

        pcm_format = 3;
      } else {

        pcm_format = 2;
      }
    } else {

      pcm_format = 2;
    }

    break;
    default:

    pcm_format = 2;
    }
  } else {

    pcm_format = parse_audio_format_i_type(chip, fp, format, fmt);

    if (pcm_format < 0) {

      return (-1);
    }
  }

  fp->format = pcm_format;

  fp->channels = (unsigned int )*(fmt + 4);

  if (fp->channels < 1U) {

    printk("<3>%d:%u:%d : invalid channels %d\n", (chip->dev)->devnum, fp->iface,
           (int )fp->altsetting, fp->channels);

    return (-1);
  }

  tmp = parse_audio_format_rates(chip, fp, fmt, 7);

  return (tmp);
}
}

static int parse_audio_format_ii(struct snd_usb_audio *chip , struct audioformat *fp ,
                                 int format , unsigned char *fmt )
{
  int brate ;
  int framesize ;
  int tmp ;

  {

  switch (format) {
  case 4098:

  fp->format = 1;

  break;
  case 4097:

  fp->format = 23;

  break;
  default:

  printk("<6>%d:%u:%d : unknown format tag 0x%x is detected.  processed as MPEG.\n",
         (chip->dev)->devnum, fp->iface, (int )fp->altsetting, format);

  fp->format = 23;

  break;
  }

  fp->channels = 1U;

  brate = (int )((unsigned int )*(fmt + 4) | ((unsigned int )*((fmt + 4) + 1) << 8));

  framesize = (int )((unsigned int )*(fmt + 6) | ((unsigned int )*((fmt + 6) + 1) << 8));

  printk("<6>found format II with max.bitrate = %d, frame size=%d\n", brate, framesize);

  fp->frame_size = (unsigned int )framesize;

  tmp = parse_audio_format_rates(chip, fp, fmt, 8);

  return (tmp);
}
}

static int parse_audio_format(struct snd_usb_audio *chip , struct audioformat *fp ,
                              int format , unsigned char *fmt , int stream )
{
  int err ;

  {

  switch ((int )*(fmt + 3)) {
  case 1:
  case 3:

  err = parse_audio_format_i(chip, fp, format, fmt);

  break;
  case 2:

  err = parse_audio_format_ii(chip, fp, format, fmt);

  break;
  default:

  printk("<6>%d:%u:%d : format type %d is not supported yet\n", (chip->dev)->devnum,
         fp->iface, (int )fp->altsetting, (int )*(fmt + 3));

  return (-1);
  }

  fp->fmt_type = (unsigned int )*(fmt + 3);

  if (err < 0) {

    return (err);
  }

  if (chip->usb_id == (u32 )((1054 << 16) | 12288)) {

    goto _L;
  } else

  if (chip->usb_id == (u32 )((1054 << 16) | 12320)) {

    goto _L;
  } else

  if (chip->usb_id == (u32 )((1054 << 16) | 12385)) {
    _L:

    if ((int )*(fmt + 3) == 1) {

      if (fp->rates != (unsigned int )(1 << 7)) {

        if (fp->rates != (unsigned int )(1 << 10)) {

          return (-1);
        }
      }
    }
  }

  return (0);
}
}

static int audiophile_skip_setting_quirk(struct snd_usb_audio *chip , int iface ,
                                         int altno ) ;

static int parse_audio_endpoints(struct snd_usb_audio *chip , int iface_no )
{
  struct usb_device *dev ;
  struct usb_interface *iface ;
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  int i ;
  int altno ;
  int err ;
  int stream ;
  int format ;
  struct audioformat *fp ;
  unsigned char *fmt ;
  unsigned char *csep ;
  int num ;
  int tmp ;
  void *tmp___0 ;
  void *tmp___1 ;
  void *tmp___2 ;
  void *tmp___3 ;
  void *tmp___4 ;
  int tmp___5 ;

  {

  dev = chip->dev;

  iface = usb_ifnum_to_if((struct usb_device const *)dev, (unsigned int )iface_no);

  num = (int )iface->num_altsetting;

  if (chip->usb_id == (u32 )((1274 << 16) | 16897)) {

    num = 4;
  }

  i = 0;

  while (i < num) {

    alts = iface->altsetting + i;

    altsd = & alts->desc;

    if ((int )altsd->bInterfaceClass != 1) {

      if ((int )altsd->bInterfaceClass != 255) {

        goto __Cont;
      } else {

        goto _L___0;
      }
    } else
    _L___0:

    if ((int )altsd->bInterfaceSubClass != 2) {

      if ((int )altsd->bInterfaceSubClass != 255) {

        goto __Cont;
      } else {

        goto _L;
      }
    } else
    _L:

    if ((int )altsd->bNumEndpoints < 1) {

      goto __Cont;
    } else

    if ((int )(alts->endpoint + 0)->desc.wMaxPacketSize == 0) {

      goto __Cont;
    }

    if (((int )(alts->endpoint + 0)->desc.bmAttributes & 3) != 1) {

      goto __Cont;
    }

    if ((int )(alts->endpoint + 0)->desc.bEndpointAddress & 128) {

      stream = 1;
    } else {

      stream = 0;
    }

    altno = (int )altsd->bAlternateSetting;

    if (chip->usb_id == (u32 )((1891 << 16) | 8195)) {

      tmp = audiophile_skip_setting_quirk(chip, iface_no, altno);

      if (tmp) {

        goto __Cont;
      }
    }

    tmp___0 = snd_usb_find_csint_desc((void *)alts->extra, alts->extralen, (void *)0,
                                      (u8 )1);

    fmt = (unsigned char *)tmp___0;

    if (! fmt) {

      printk("<3>%d:%u:%d : AS_GENERAL descriptor not found\n", dev->devnum, iface_no,
             altno);

      goto __Cont;
    }

    if ((int )*(fmt + 0) < 7) {

      printk("<3>%d:%u:%d : invalid AS_GENERAL desc\n", dev->devnum, iface_no, altno);

      goto __Cont;
    }

    format = ((int )*(fmt + 6) << 8) | (int )*(fmt + 5);

    tmp___1 = snd_usb_find_csint_desc((void *)alts->extra, alts->extralen, (void *)0,
                                      (u8 )2);

    fmt = (unsigned char *)tmp___1;

    if (! fmt) {

      printk("<3>%d:%u:%d : no FORMAT_TYPE desc\n", dev->devnum, iface_no, altno);

      goto __Cont;
    }

    if ((int )*(fmt + 0) < 8) {

      printk("<3>%d:%u:%d : invalid FORMAT_TYPE desc\n", dev->devnum, iface_no, altno);

      goto __Cont;
    }

    tmp___2 = snd_usb_find_desc((void *)(alts->endpoint + 0)->extra, (alts->endpoint + 0)->extralen,
                                (void *)0, (u8 )((1 << 5) | 5));

    csep = (unsigned char *)tmp___2;

    if (! csep) {

      if ((int )altsd->bNumEndpoints >= 2) {

        tmp___3 = snd_usb_find_desc((void *)(alts->endpoint + 1)->extra, (alts->endpoint + 1)->extralen,
                                    (void *)0, (u8 )((1 << 5) | 5));

        csep = (unsigned char *)tmp___3;
      }
    }

    if (! csep) {

      printk("<4>%d:%u:%d : no or invalid class specific endpoint descriptor\n", dev->devnum,
             iface_no, altno);

      csep = (unsigned char *)((void *)0);
    } else

    if ((int )*(csep + 0) < 7) {

      printk("<4>%d:%u:%d : no or invalid class specific endpoint descriptor\n", dev->devnum,
             iface_no, altno);

      csep = (unsigned char *)((void *)0);
    } else

    if ((int )*(csep + 2) != 1) {

      printk("<4>%d:%u:%d : no or invalid class specific endpoint descriptor\n", dev->devnum,
             iface_no, altno);

      csep = (unsigned char *)((void *)0);
    }

    tmp___4 = kzalloc(sizeof(*fp), 208U);

    fp = (struct audioformat *)tmp___4;

    if (! fp) {

      printk("<3>cannot malloc\n");

      return (-12);
    }

    fp->iface = iface_no;

    fp->altsetting = (unsigned char )altno;

    fp->altset_idx = (unsigned char )i;

    fp->endpoint = (alts->endpoint + 0)->desc.bEndpointAddress;

    fp->ep_attr = (alts->endpoint + 0)->desc.bmAttributes;

    fp->maxpacksize = (unsigned int )(alts->endpoint + 0)->desc.wMaxPacketSize;

    if ((unsigned int )dev->speed == 3U) {

      fp->maxpacksize = (((fp->maxpacksize >> 11) & 3U) + 1U) * (fp->maxpacksize & 2047U);
    }

    if (csep) {

      fp->attributes = *(csep + 3);
    } else {

      fp->attributes = (unsigned char)0;
    }

    switch ((int )chip->usb_id) {
    case (2706 << 16) | 83:

    fp->attributes = (unsigned char )((int )fp->attributes & -2);

    break;
    case (1054 << 16) | 12320:
    case (1891 << 16) | 8195:

    fp->attributes = (unsigned char )((int )fp->attributes | 1);

    break;
    case (1151 << 16) | 3233:
    case (1917 << 16) | 1967:

    fp->ep_attr = (unsigned char )((int )fp->ep_attr & -13);

    if (stream == 0) {

      fp->ep_attr = (unsigned char )((int )fp->ep_attr | 8);
    } else {

      fp->ep_attr = (unsigned char )((int )fp->ep_attr | 12);
    }

    break;
    }

    tmp___5 = parse_audio_format(chip, fp, format, fmt, stream);

    if (tmp___5 < 0) {

      kfree((void const *)fp->rate_table);

      kfree((void const *)fp);

      goto __Cont;
    }

    printk("<6>%d:%u:%d: add audio endpoint 0x%x\n", dev->devnum, iface_no, altno,
           (int )fp->endpoint);

    err = add_audio_endpoint(chip, stream, fp);

    if (err < 0) {

      kfree((void const *)fp->rate_table);

      kfree((void const *)fp);

      return (err);
    }

    usb_set_interface(chip->dev, iface_no, altno);

    init_usb_pitch(chip->dev, iface_no, alts, fp);

    init_usb_sample_rate(chip->dev, iface_no, alts, fp, (int )fp->rate_max);
    __Cont:

    i ++;
  }

  return (0);
}
}

static void snd_usb_stream_disconnect(struct list_head *head )
{
  int idx ;
  struct snd_usb_stream *as ;
  struct snd_usb_substream *subs ;
  struct list_head const *__mptr ;

  {

  __mptr = (struct list_head const *)head;

  as = (struct snd_usb_stream *)((char *)__mptr - (unsigned int )(& ((struct snd_usb_stream *)0)->list));

  idx = 0;

  while (idx < 2) {

    subs = & as->substream[idx];

    if (! subs->num_formats) {

      return;
    }

    release_substream_urbs(subs, 1);

    subs->interface = -1;

    idx ++;
  }

  return;
}
}

static int snd_usb_create_streams(struct snd_usb_audio *chip , int ctrlif )
{
  struct usb_device *dev ;
  struct usb_host_interface *host_iface ;
  struct usb_interface *iface ;
  unsigned char *p1 ;
  int i ;
  int j ;
  struct usb_interface *tmp ;
  void *tmp___0 ;
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;

  {

  dev = chip->dev;

  tmp = usb_ifnum_to_if((struct usb_device const *)dev, (unsigned int )ctrlif);

  host_iface = tmp->altsetting + 0;

  tmp___0 = snd_usb_find_csint_desc((void *)host_iface->extra, host_iface->extralen,
                                    (void *)0, (u8 )1);

  p1 = (unsigned char *)tmp___0;

  if (! p1) {

    printk("<3>cannot find HEADER\n");

    return (-22);
  }

  if (! *(p1 + 7)) {

    printk("<3>invalid HEADER\n");

    return (-22);
  } else

  if ((int )*(p1 + 0) < 8 + (int )*(p1 + 7)) {

    printk("<3>invalid HEADER\n");

    return (-22);
  }

  i = 0;

  while (i < (int )*(p1 + 7)) {

    j = (int )*(p1 + (8 + i));

    iface = usb_ifnum_to_if((struct usb_device const *)dev, (unsigned int )j);

    if (! iface) {

      printk("<3>%d:%u:%d : does not exist\n", dev->devnum, ctrlif, j);

      goto __Cont;
    }

    tmp___1 = usb_interface_claimed(iface);

    if (tmp___1) {

      printk("<6>%d:%d:%d: skipping, already claimed\n", dev->devnum, ctrlif, j);

      goto __Cont;
    }

    alts = iface->altsetting + 0;

    altsd = & alts->desc;

    if ((int )altsd->bInterfaceClass == 1) {

      goto _L;
    } else

    if ((int )altsd->bInterfaceClass == 255) {
      _L:

      if ((int )altsd->bInterfaceSubClass == 3) {

        tmp___2 = snd_usb_create_midi_interface(chip, iface, (struct snd_usb_audio_quirk const *)((void *)0));

        if (tmp___2 < 0) {

          printk("<3>%d:%u:%d: cannot create sequencer device\n", dev->devnum, ctrlif,
                 j);

          goto __Cont;
        }

        usb_driver_claim_interface(& usb_audio_driver, iface, (void *)-1L);

        goto __Cont;
      }
    }

    if ((int )altsd->bInterfaceClass != 1) {

      if ((int )altsd->bInterfaceClass != 255) {

        printk("<3>%d:%u:%d: skipping non-supported interface %d\n", dev->devnum,
               ctrlif, j, (int )altsd->bInterfaceClass);

        goto __Cont;
      } else {

        goto _L___0;
      }
    } else
    _L___0:

    if ((int )altsd->bInterfaceSubClass != 2) {

      printk("<3>%d:%u:%d: skipping non-supported interface %d\n", dev->devnum, ctrlif,
             j, (int )altsd->bInterfaceClass);

      goto __Cont;
    }

    if ((unsigned int )dev->speed == 1U) {

      printk("<3>low speed audio streaming not supported\n");

      goto __Cont;
    }

    tmp___3 = parse_audio_endpoints(chip, j);

    if (! tmp___3) {

      usb_set_interface(dev, j, 0);

      usb_driver_claim_interface(& usb_audio_driver, iface, (void *)-1L);
    }
    __Cont:

    i ++;
  }

  return (0);
}
}

static int create_fixed_stream_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                     struct snd_usb_audio_quirk const *quirk )
{
  struct audioformat *fp ;
  struct usb_host_interface *alts ;
  int stream ;
  int err ;
  unsigned int *rate_table ;
  void *tmp ;
  void *tmp___0 ;
  size_t __len ;
  void *__ret ;

  {

  rate_table = (unsigned int *)((void *)0);

  tmp = kmemdup((void const *)quirk->data, sizeof(*fp), 208U);

  fp = (struct audioformat *)tmp;

  if (! fp) {

    printk("<3>cannot memdup\n");

    return (-12);
  }

  if (fp->nr_rates > 0U) {

    tmp___0 = kmalloc(sizeof(int ) * (unsigned long )fp->nr_rates, 208U);

    rate_table = (unsigned int *)tmp___0;

    if (! rate_table) {

      kfree((void const *)fp);

      return (-12);
    }

    __len = sizeof(int ) * (unsigned long )fp->nr_rates;

    __ret = __builtin_memcpy((void *)rate_table, (void const *)fp->rate_table, __len);

    fp->rate_table = rate_table;
  }

  if ((int )fp->endpoint & 128) {

    stream = 1;
  } else {

    stream = 0;
  }

  err = add_audio_endpoint(chip, stream, fp);

  if (err < 0) {

    kfree((void const *)fp);

    kfree((void const *)rate_table);

    return (err);
  }

  if (fp->iface != (int )(iface->altsetting + 0)->desc.bInterfaceNumber) {

    kfree((void const *)fp);

    kfree((void const *)rate_table);

    return (-22);
  } else

  if ((unsigned int )fp->altset_idx >= iface->num_altsetting) {

    kfree((void const *)fp);

    kfree((void const *)rate_table);

    return (-22);
  }

  alts = iface->altsetting + fp->altset_idx;

  fp->maxpacksize = (unsigned int )(alts->endpoint + 0)->desc.wMaxPacketSize;

  usb_set_interface(chip->dev, fp->iface, 0);

  init_usb_pitch(chip->dev, fp->iface, alts, fp);

  init_usb_sample_rate(chip->dev, fp->iface, alts, fp, (int )fp->rate_max);

  return (0);
}
}

static int create_standard_audio_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                       struct snd_usb_audio_quirk const *quirk )
{
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  int err ;

  {

  alts = iface->altsetting + 0;

  altsd = & alts->desc;

  err = parse_audio_endpoints(chip, (int )altsd->bInterfaceNumber);

  if (err < 0) {

    printk("<3>cannot setup if %d: error %d\n", (int )altsd->bInterfaceNumber, err);

    return (err);
  }

  usb_set_interface(chip->dev, (int )altsd->bInterfaceNumber, 0);

  return (0);
}
}

static struct audioformat const ua_format =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    32, 2U, 1U, 0U, 0, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)0,
    (unsigned char)0, 0U, (unsigned int )(1 << 30), 0U, 0U, 0U, (unsigned int *)0};

static struct snd_usb_midi_endpoint_info const ua700_ep = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )3,
    (uint16_t )3};

static struct snd_usb_audio_quirk const ua700_quirk = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (short)0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& ua700_ep)};

static struct snd_usb_midi_endpoint_info const uaxx_ep = {(signed char)0, (unsigned char)0, (signed char)0, (unsigned char)0, (uint16_t )1,
    (uint16_t )1};

static struct snd_usb_audio_quirk const uaxx_quirk = {(char const * __attribute__((__nullterm__)) )0, (char const * __attribute__((__recursive__)) )0,
    (short)0, (uint16_t )3, (void const * __attribute__((__recursive__)) )(& uaxx_ep)};

static int create_uaxx_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                             struct snd_usb_audio_quirk const *quirk )
{
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  struct audioformat *fp ;
  int stream ;
  int err ;
  int tmp ;
  int tmp___0 ;
  void *tmp___1 ;
  size_t __len ;
  void *__ret ;
  unsigned int tmp___2 ;
  unsigned int tmp___3 ;
  unsigned int tmp___4 ;

  {

  if (iface->num_altsetting < 2U) {

    return (-6);
  }

  alts = iface->altsetting + 1;

  altsd = & alts->desc;

  if ((int )altsd->bNumEndpoints == 2) {

    if (chip->usb_id == (u32 )((1410 << 16) | 43)) {

      tmp = snd_usb_create_midi_interface(chip, iface, & ua700_quirk);

      return (tmp);
    } else {

      tmp___0 = snd_usb_create_midi_interface(chip, iface, & uaxx_quirk);

      return (tmp___0);
    }
  }

  if ((int )altsd->bNumEndpoints != 1) {

    return (-6);
  }

  tmp___1 = kmalloc(sizeof(*fp), 208U);

  fp = (struct audioformat *)tmp___1;

  if (! fp) {

    return (-12);
  }

  __len = sizeof(*fp);

  if (__len >= 64UL) {

    __ret = __memcpy((void *)fp, (void const *)(& ua_format), __len);
  } else {

    __ret = __builtin_memcpy((void *)fp, (void const *)(& ua_format), __len);
  }

  fp->iface = (int )altsd->bInterfaceNumber;

  fp->endpoint = (alts->endpoint + 0)->desc.bEndpointAddress;

  fp->ep_attr = (alts->endpoint + 0)->desc.bmAttributes;

  fp->maxpacksize = (unsigned int )(alts->endpoint + 0)->desc.wMaxPacketSize;

  switch ((int )fp->maxpacksize) {
  case 288:

  tmp___2 = 44100U;

  fp->rate_min = tmp___2;

  fp->rate_max = tmp___2;

  break;
  case 312:
  case 320:

  tmp___3 = 48000U;

  fp->rate_min = tmp___3;

  fp->rate_max = tmp___3;

  break;
  case 600:
  case 608:

  tmp___4 = 96000U;

  fp->rate_min = tmp___4;

  fp->rate_max = tmp___4;

  break;
  default:

  printk("<3>unknown sample rate\n");

  kfree((void const *)fp);

  return (-6);
  }

  if ((int )fp->endpoint & 128) {

    stream = 1;
  } else {

    stream = 0;
  }

  err = add_audio_endpoint(chip, stream, fp);

  if (err < 0) {

    kfree((void const *)fp);

    return (err);
  }

  usb_set_interface(chip->dev, fp->iface, 0);

  return (0);
}
}

static struct audioformat const ua1000_format =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    10, 0U, 1U, 0U, 0, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)0,
    (unsigned char)0, 0U, (unsigned int )(1 << 30), 0U, 0U, 0U, (unsigned int *)0};

static int create_ua1000_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                               struct snd_usb_audio_quirk const *quirk )
{
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  struct audioformat *fp ;
  int stream ;
  int err ;
  void *tmp ;
  unsigned int tmp___0 ;

  {

  if (iface->num_altsetting != 2U) {

    return (-6);
  }

  alts = iface->altsetting + 1;

  altsd = & alts->desc;

  if (alts->extralen != 11) {

    return (-6);
  } else

  if ((int )*(alts->extra + 1) != ((1 << 5) | 4)) {

    return (-6);
  } else

  if ((int )altsd->bNumEndpoints != 1) {

    return (-6);
  }

  tmp = kmemdup((void const *)(& ua1000_format), sizeof(*fp), 208U);

  fp = (struct audioformat *)tmp;

  if (! fp) {

    return (-12);
  }

  fp->channels = (unsigned int )*(alts->extra + 4);

  fp->iface = (int )altsd->bInterfaceNumber;

  fp->endpoint = (alts->endpoint + 0)->desc.bEndpointAddress;

  fp->ep_attr = (alts->endpoint + 0)->desc.bmAttributes;

  fp->maxpacksize = (unsigned int )(alts->endpoint + 0)->desc.wMaxPacketSize;

  tmp___0 = ((unsigned int )*(alts->extra + 8) | ((unsigned int )*((alts->extra + 8) + 1) << 8)) | ((unsigned int )*((alts->extra + 8) + 2) << 16);

  fp->rate_min = tmp___0;

  fp->rate_max = tmp___0;

  if ((int )fp->endpoint & 128) {

    stream = 1;
  } else {

    stream = 0;
  }

  err = add_audio_endpoint(chip, stream, fp);

  if (err < 0) {

    kfree((void const *)fp);

    return (err);
  }

  usb_set_interface(chip->dev, fp->iface, 0);

  return (0);
}
}

static struct audioformat const ua101_format =

     {{(struct list_head * __attribute__((__recursive__)) )0, (struct list_head * __attribute__((__recursive__)) )0},
    10, 0U, 1U, 0U, 0, (unsigned char)1, (unsigned char)1, (unsigned char)0, (unsigned char)0,
    (unsigned char)0, 0U, (unsigned int )(1 << 30), 0U, 0U, 0U, (unsigned int *)0};

static int create_ua101_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                              struct snd_usb_audio_quirk const *quirk )
{
  struct usb_host_interface *alts ;
  struct usb_interface_descriptor *altsd ;
  struct audioformat *fp ;
  int stream ;
  int err ;
  void *tmp ;
  unsigned int tmp___0 ;

  {

  if (iface->num_altsetting != 2U) {

    return (-6);
  }

  alts = iface->altsetting + 1;

  altsd = & alts->desc;

  if (alts->extralen != 18) {

    return (-6);
  } else

  if ((int )*(alts->extra + 1) != ((1 << 5) | 4)) {

    return (-6);
  } else

  if ((int )altsd->bNumEndpoints != 1) {

    return (-6);
  }

  tmp = kmemdup((void const *)(& ua101_format), sizeof(*fp), 208U);

  fp = (struct audioformat *)tmp;

  if (! fp) {

    return (-12);
  }

  fp->channels = (unsigned int )*(alts->extra + 11);

  fp->iface = (int )altsd->bInterfaceNumber;

  fp->endpoint = (alts->endpoint + 0)->desc.bEndpointAddress;

  fp->ep_attr = (alts->endpoint + 0)->desc.bmAttributes;

  fp->maxpacksize = (unsigned int )(alts->endpoint + 0)->desc.wMaxPacketSize;

  tmp___0 = ((unsigned int )*(alts->extra + 15) | ((unsigned int )*((alts->extra + 15) + 1) << 8)) | ((unsigned int )*((alts->extra + 15) + 2) << 16);

  fp->rate_min = tmp___0;

  fp->rate_max = tmp___0;

  if ((int )fp->endpoint & 128) {

    stream = 1;
  } else {

    stream = 0;
  }

  err = add_audio_endpoint(chip, stream, fp);

  if (err < 0) {

    kfree((void const *)fp);

    return (err);
  }

  usb_set_interface(chip->dev, fp->iface, 0);

  return (0);
}
}

static int snd_usb_create_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                struct snd_usb_audio_quirk const *quirk ) ;

static int create_composite_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                  struct snd_usb_audio_quirk const *quirk )
{
  int probed_ifnum ;
  int err ;
  int tmp ;

  {

  probed_ifnum = (int )(iface->altsetting)->desc.bInterfaceNumber;

  quirk = (struct snd_usb_audio_quirk const *)quirk->data;

  while ((int const )quirk->ifnum >= 0) {

    iface = usb_ifnum_to_if((struct usb_device const *)chip->dev, (unsigned int )quirk->ifnum);

    if (! iface) {

      goto __Cont;
    }

    if ((int const )quirk->ifnum != (int const )probed_ifnum) {

      tmp = usb_interface_claimed(iface);

      if (tmp) {

        goto __Cont;
      }
    }

    err = snd_usb_create_quirk(chip, iface, quirk);

    if (err < 0) {

      return (err);
    }

    if ((int const )quirk->ifnum != (int const )probed_ifnum) {

      usb_driver_claim_interface(& usb_audio_driver, iface, (void *)-1L);
    }
    __Cont:

    quirk ++;
  }

  return (0);
}
}

static int ignore_interface_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                  struct snd_usb_audio_quirk const *quirk )
{


  {

  return (0);
}
}

static int snd_usb_extigy_boot_quirk(struct usb_device *dev , struct usb_interface *intf )
{
  struct usb_host_config *config ;
  int err ;
  unsigned int tmp ;

  {

  config = dev->actconfig;

  if ((int )config->desc.wTotalLength == 794) {

    goto _L;
  } else

  if ((int )config->desc.wTotalLength == 483) {
    _L:

    printk("sending Extigy boot sequence...\n");

    tmp = __create_pipe(dev, 0U);

    err = snd_usb_ctl_msg(dev, (unsigned int )(2 << 30) | tmp, (__u8 )16, (__u8 )67,
                          (__u16 )1, (__u16 )10, (void *)0, (__u16 )0, 1000);

    if (err < 0) {

      printk("error sending boot message: %d\n", err);
    }

    err = usb_get_descriptor(dev, (unsigned char)1, (unsigned char)0, (void *)(& dev->descriptor),
                             (int )sizeof(dev->descriptor));

    config = dev->actconfig;

    if (err < 0) {

      printk("error usb_get_descriptor: %d\n", err);
    }

    err = usb_reset_configuration(dev);

    if (err < 0) {

      printk("error usb_reset_configuration: %d\n", err);
    }

    printk("extigy_boot: new boot length = %d\n", (int )config->desc.wTotalLength);

    return (-19);
  }

  return (0);
}
}

static int snd_usb_audigy2nx_boot_quirk(struct usb_device *dev )
{
  u8 buf ;
  unsigned int tmp ;
  unsigned int tmp___0 ;

  {

  buf = (u8 )1;

  tmp = __create_pipe(dev, 0U);

  snd_usb_ctl_msg(dev, ((unsigned int )(2 << 30) | tmp) | 128U, (__u8 )42, (__u8 )((128 | (2 << 5)) | 3),
                  (__u16 )0, (__u16 )0, (void *)(& buf), (__u16 )1, 1000);

  if ((int )buf == 0) {

    tmp___0 = __create_pipe(dev, 0U);

    snd_usb_ctl_msg(dev, (unsigned int )(2 << 30) | tmp___0, (__u8 )41, (__u8 )((2 << 5) | 3),
                    (__u16 )1, (__u16 )2000, (void *)0, (__u16 )0, 1000);

    return (-19);
  }

  return (0);
}
}

static int snd_usb_cm106_write_int_reg(struct usb_device *dev , int reg , u16 value )
{
  u8 buf[4] ;
  unsigned int tmp ;
  int tmp___0 ;

  {

  buf[0] = (u8 )32;

  buf[1] = (u8 )((int )value & 255);

  buf[2] = (u8 )(((int )value >> 8) & 255);

  buf[3] = (u8 )reg;

  tmp = __create_pipe(dev, 0U);

  tmp___0 = snd_usb_ctl_msg(dev, (unsigned int )(2 << 30) | tmp, (__u8 )9, (__u8 )((1 << 5) | 2),
                            (__u16 )0, (__u16 )0, (void *)(& buf), (__u16 )4, 1000);

  return (tmp___0);
}
}

static int snd_usb_cm106_boot_quirk(struct usb_device *dev )
{
  int tmp ;

  {

  tmp = snd_usb_cm106_write_int_reg(dev, 2, (u16 )32772);

  return (tmp);
}
}

static int audiophile_skip_setting_quirk(struct snd_usb_audio *chip , int iface ,
                                         int altno )
{


  {

  usb_set_interface(chip->dev, iface, 0);

  if (device_setup[chip->index] & 1) {

    if (device_setup[chip->index] & 2) {

      if (altno != 6) {

        return (1);
      }
    }

    if (device_setup[chip->index] & 4) {

      if (altno != 1) {

        return (1);
      }
    }

    if ((device_setup[chip->index] & 31) == 25) {

      if (altno != 2) {

        return (1);
      }
    }

    if ((device_setup[chip->index] & 31) == 9) {

      if (altno != 3) {

        return (1);
      }
    }

    if ((device_setup[chip->index] & 31) == 17) {

      if (altno != 4) {

        return (1);
      }
    }

    if ((device_setup[chip->index] & 31) == 1) {

      if (altno != 5) {

        return (1);
      }
    }
  }

  return (0);
}
}

static int snd_usb_create_quirk(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                struct snd_usb_audio_quirk const *quirk )
{
  int tmp ;

  {

  if ((int const )quirk->type < 16) {

    tmp = (*(quirk_funcs[quirk->type]))(chip, iface, quirk);

    return (tmp);
  } else {

    printk("<3>invalid quirk type %d\n", (int const )quirk->type);

    return (-6);
  }
}
}

static void proc_audio_usbbus_read(struct snd_info_entry *entry , struct snd_info_buffer *buffer )
{
  struct snd_usb_audio *chip ;

  {

  chip = (struct snd_usb_audio *)entry->private_data;

  if (! chip->shutdown) {

    snd_iprintf(buffer, (char *)"%03d/%03d\n", ((chip->dev)->bus)->busnum, (chip->dev)->devnum);
  }

  return;
}
}

static void proc_audio_usbid_read(struct snd_info_entry *entry , struct snd_info_buffer *buffer )
{
  struct snd_usb_audio *chip ;

  {

  chip = (struct snd_usb_audio *)entry->private_data;

  if (! chip->shutdown) {

    snd_iprintf(buffer, (char *)"%04x:%04x\n", chip->usb_id >> 16, (int )((u16 )chip->usb_id));
  }

  return;
}
}

static void snd_usb_audio_create_proc(struct snd_usb_audio *chip )
{
  struct snd_info_entry *entry ;
  int tmp ;
  int tmp___0 ;

  {

  entry = (struct snd_info_entry *)((void *)0);

  tmp = snd_card_proc_new(chip->card, (char const * __attribute__((__nullterm__)) )"usbbus",
                          & entry);

  if (! tmp) {

    snd_info_set_text_ops(entry, (void *)chip, & proc_audio_usbbus_read);
  }

  tmp___0 = snd_card_proc_new(chip->card, (char const * __attribute__((__nullterm__)) )"usbid",
                              & entry);

  if (! tmp___0) {

    snd_info_set_text_ops(entry, (void *)chip, & proc_audio_usbid_read);
  }

  return;
}
}

static int snd_usb_audio_free(struct snd_usb_audio *chip )
{


  {

  kfree((void const *)chip);

  return (0);
}
}

static int snd_usb_audio_dev_free(struct snd_device *device )
{
  struct snd_usb_audio *chip ;
  int tmp ;

  {

  chip = (struct snd_usb_audio *)device->device_data;

  tmp = snd_usb_audio_free(chip);

  return (tmp);
}
}

static int snd_usb_audio_create(struct usb_device *dev , int idx , struct snd_usb_audio_quirk const *quirk ,
                                struct snd_usb_audio **rchip ) ;

static struct snd_device_ops ops = {& snd_usb_audio_dev_free, (int (*)(struct snd_device *dev ))0, (int (*)(struct snd_device *dev ))0};

static int snd_usb_audio_create(struct usb_device *dev , int idx , struct snd_usb_audio_quirk const *quirk ,
                                struct snd_usb_audio **rchip )
{
  struct snd_card *card ;
  struct snd_usb_audio *chip ;
  int err ;
  int len ;
  char component[14] ;
  void *tmp ;
  int tmp___0 ;
  size_t tmp___1 ;
  size_t tmp___2 ;
  char const *tmp___3 ;
  char const *tmp___4 ;

  {

  *rchip = (struct snd_usb_audio *)((void *)0);

  if ((unsigned int )dev->speed != 1U) {

    if ((unsigned int )dev->speed != 2U) {

      if ((unsigned int )dev->speed != 3U) {

        printk("<3>unknown device speed %d\n", (unsigned int )dev->speed);

        return (-6);
      }
    }
  }

  card = snd_card_new(index[idx], (char const * __attribute__((__nullterm__)) )id[idx],
                      & __this_module, 0);

  if ((unsigned long )card == (unsigned long )((void *)0)) {

    printk("<3>cannot create card instance %d\n", idx);

    return (-12);
  }

  tmp = kzalloc(sizeof(*chip), 208U);

  chip = (struct snd_usb_audio *)tmp;

  if (! chip) {

    snd_card_free(card);

    return (-12);
  }

  chip->index = idx;

  chip->dev = dev;

  chip->card = card;

  chip->usb_id = (u32 )(((int )dev->descriptor.idVendor << 16) | (int )dev->descriptor.idProduct);

  INIT_LIST_HEAD(& chip->pcm_list);

  INIT_LIST_HEAD(& chip->midi_list);

  INIT_LIST_HEAD(& chip->mixer_list);

  err = snd_device_new(card, 8192, (void *)chip, & ops);

  if (err < 0) {

    snd_usb_audio_free(chip);

    snd_card_free(card);

    return (err);
  }

  strcpy(card->driver, "USB-Audio");

  sprintf(component, "USB%04x:%04x", chip->usb_id >> 16, (int )((u16 )chip->usb_id));

  snd_component_add(card, (char const * __attribute__((__nullterm__)) )(component));

  if (quirk) {

    if (quirk->product_name) {

      strlcpy(card->shortname, (char const *)quirk->product_name, sizeof(card->shortname));
    } else {

      goto _L;
    }
  } else
  _L:

  if (! dev->descriptor.iProduct) {

    sprintf(card->shortname, "USB Device %#04x:%#04x", chip->usb_id >> 16, (int )((u16 )chip->usb_id));
  } else {

    tmp___0 = usb_string(dev, (int )dev->descriptor.iProduct, (char * __attribute__((__exp__(Nonstub_get_size("size")))) )(card->shortname),
                         sizeof(card->shortname));

    if (tmp___0 <= 0) {

      sprintf(card->shortname, "USB Device %#04x:%#04x", chip->usb_id >> 16, (int )((u16 )chip->usb_id));
    }
  }

  if (quirk) {

    if (quirk->vendor_name) {

      tmp___1 = strlcpy(card->longname, (char const *)quirk->vendor_name, sizeof(card->longname));

      len = (int )tmp___1;
    } else {

      goto _L___0;
    }
  } else
  _L___0:

  if (dev->descriptor.iManufacturer) {

    len = usb_string(dev, (int )dev->descriptor.iManufacturer, (char * __attribute__((__exp__(Nonstub_get_size("size")))) )(card->longname),
                     sizeof(card->longname));
  } else {

    len = 0;
  }

  if (len > 0) {

    strlcat(card->longname, " ", sizeof(card->longname));
  }

  strlcat(card->longname, (char const *)(card->shortname), sizeof(card->longname));

  tmp___2 = strlcat(card->longname, " at ", sizeof(card->longname));

  len = (int )tmp___2;

  if ((unsigned long )len < sizeof(card->longname)) {

    usb_make_path(dev, card->longname + len, sizeof(card->longname) - (unsigned long )len);
  }

  if ((unsigned int )dev->speed == 1U) {

    tmp___4 = ", low speed";
  } else {

    if ((unsigned int )dev->speed == 2U) {

      tmp___3 = ", full speed";
    } else {

      tmp___3 = ", high speed";
    }

    tmp___4 = tmp___3;
  }

  strlcat(card->longname, tmp___4, sizeof(card->longname));

  snd_usb_audio_create_proc(chip);

  *rchip = chip;

  return (0);
}
}

static void *snd_usb_audio_probe(struct usb_device *dev , struct usb_interface *intf ,
                                 struct usb_device_id const *usb_id )
{
  struct snd_usb_audio_quirk const *quirk ;
  int i ;
  int err ;
  struct snd_usb_audio *chip ;
  struct usb_host_interface *alts ;
  int ifnum ;
  u32 id___0 ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int error ;
  int tmp___5 ;

  {

  quirk = (struct snd_usb_audio_quirk const *)usb_id->driver_info;

  alts = intf->altsetting + 0;

  ifnum = (int )alts->desc.bInterfaceNumber;

  id___0 = (u32 )(((int )dev->descriptor.idVendor << 16) | (int )dev->descriptor.idProduct);

  if (quirk) {

    if ((int const )quirk->ifnum >= 0) {

      if (ifnum != (int )quirk->ifnum) {

        goto __err_val;
      }
    }
  }

  printk("%s Location 1\n", "snd_usb_audio_probe");

  if (id___0 == (u32 )((1054 << 16) | 12288)) {

    tmp = snd_usb_extigy_boot_quirk(dev, intf);

    if (tmp < 0) {

      goto __err_val;
    }
  }

  printk("%s Location 2\n", "snd_usb_audio_probe");

  if (id___0 == (u32 )((1054 << 16) | 12320)) {

    tmp___0 = snd_usb_audigy2nx_boot_quirk(dev);

    if (tmp___0 < 0) {

      goto __err_val;
    }
  }

  printk("%s Location 3\n", "snd_usb_audio_probe");

  if (id___0 == (u32 )((4341 << 16) | 512)) {

    tmp___1 = snd_usb_cm106_boot_quirk(dev);

    if (tmp___1 < 0) {

      goto __err_val;
    }
  }

  printk("%s Location 4\n", "snd_usb_audio_probe");

  chip = (struct snd_usb_audio *)((void *)0);

  mutex_lock_nested(& register_mutex, 0U);

  i = 0;

  while (i < 32) {

    if (usb_chip[i]) {

      if ((unsigned long )(usb_chip[i])->dev == (unsigned long )dev) {

        chip = (struct snd_usb_audio *)usb_chip[i];

        break;
      }
    }

    i ++;
  }

  printk("%s Location 5\n", "snd_usb_audio_probe");

  if (! chip) {

    i = 0;

    while (i < 32) {

      if (enable[i]) {

        if (! usb_chip[i]) {

          if (vid[i] == -1) {

            goto _L___0;
          } else

          if ((u32 )vid[i] == id___0 >> 16) {
            _L___0:

            if (pid[i] == -1) {

              goto _L;
            } else

            if (pid[i] == (int )((u16 )id___0)) {
              _L:

              tmp___2 = snd_usb_audio_create(dev, i, quirk, & chip);

              if (tmp___2 < 0) {

                goto __error;
              }

              (chip->card)->dev = & intf->dev;

              break;
            }
          }
        }
      }

      i ++;
    }

    if (! chip) {

      printk("<3>no available usb audio device\n");

      goto __error;
    }
  }

  printk("%s Location 6\n", "snd_usb_audio_probe");

  err = 1;

  if (quirk) {

    if ((int const )quirk->ifnum != -2) {

      err = snd_usb_create_quirk(chip, intf, quirk);

      if (err < 0) {

        goto __error;
      }
    }
  }

  printk("%s Location 7\n", "snd_usb_audio_probe");

  if (err > 0) {

    tmp___3 = snd_usb_create_streams(chip, ifnum);

    if (tmp___3 < 0) {

      goto __error;
    } else {

      tmp___4 = snd_usb_create_mixer(chip, ifnum, ignore_ctl_error);

      if (tmp___4 < 0) {

        goto __error;
      }
    }
  }

  printk("%s Location 8\n", "snd_usb_audio_probe");

  tmp___5 = snd_card_register(chip->card);

  error = tmp___5;

  if (error < 0) {

    printk("%s failure: %d\n", "snd_usb_audio_probe", error);

    goto __error;
  }

  printk("%s success: %d\n", "snd_usb_audio_probe", error);

  usb_chip[chip->index] = (struct snd_usb_audio * __attribute__((__noderef__, __address_space__(2))) )chip;

  (chip->num_interfaces) ++;

  mutex_unlock(& register_mutex);

  return ((void *)chip);
  __error:

  printk("%s Location ERROR\n", "snd_usb_audio_probe");

  if (chip) {

    if (! chip->num_interfaces) {

      snd_card_free(chip->card);
    }
  }

  mutex_unlock(& register_mutex);
  __err_val:

  return ((void *)0);
}
}

static void snd_usb_audio_disconnect(struct usb_device *dev , void *ptr )
{
  struct snd_usb_audio *chip ;
  struct snd_card *card ;
  struct list_head *p ;

  {

  if ((unsigned long )ptr == (unsigned long )((void *)-1L)) {

    return;
  }

  chip = (struct snd_usb_audio *)ptr;

  card = chip->card;

  mutex_lock_nested(& register_mutex, 0U);

  chip->shutdown = 1;

  (chip->num_interfaces) --;

  if (chip->num_interfaces <= 0) {

    snd_card_disconnect(card);

    p = (struct list_head *)chip->pcm_list.next;

    while (1) {

      __builtin_prefetch((void const *)p->next);

      if (! ((unsigned long )p != (unsigned long )(& chip->pcm_list))) {

        break;
      }

      snd_usb_stream_disconnect(p);

      p = (struct list_head *)p->next;
    }

    p = (struct list_head *)chip->midi_list.next;

    while (1) {

      __builtin_prefetch((void const *)p->next);

      if (! ((unsigned long )p != (unsigned long )(& chip->midi_list))) {

        break;
      }

      snd_usbmidi_disconnect(p);

      p = (struct list_head *)p->next;
    }

    p = (struct list_head *)chip->mixer_list.next;

    while (1) {

      __builtin_prefetch((void const *)p->next);

      if (! ((unsigned long )p != (unsigned long )(& chip->mixer_list))) {

        break;
      }

      snd_usb_mixer_disconnect(p);

      p = (struct list_head *)p->next;
    }

    usb_chip[chip->index] = (struct snd_usb_audio * __attribute__((__noderef__, __address_space__(2))) )((void *)0);

    mutex_unlock(& register_mutex);

    snd_card_free_when_closed(card);
  } else {

    mutex_unlock(& register_mutex);
  }

  return;
}
}

struct usb_device *Nonstub_get_usb_device(struct usb_interface *intf , int marshaling ,
                                          int gen_kern , struct usb_device *after )
{
  struct usb_device *dev ;
  struct device const __attribute__((__recursive__)) *__mptr ;

  {

  __mptr = (struct device const __attribute__((__recursive__)) *)intf->dev.parent;

  dev = (struct usb_device *)((char *)__mptr - (unsigned int )(& ((struct usb_device *)0)->dev));

  if (! gen_kern) {

    if (marshaling) {

      if (! after) {

        intf->dev.parent = (struct device * __attribute__((__recursive__)) )dev;
      } else {

        intf->dev.parent = (struct device * __attribute__((__recursive__)) )(& after->dev);

        dev = after;
      }
    } else

    if (! after) {

      intf->dev.parent = (struct device * __attribute__((__recursive__)) )dev;
    } else {

      intf->dev.parent = (struct device * __attribute__((__recursive__)) )(& after->dev);

      dev = after;
    }
  }

  return (dev);
}
}

static int usb_audio_probe(struct usb_interface * __attribute__((__extraptr__(sizeof(struct usb_device *),"Nonstub_get_usb_device"))) intf ,
                           struct usb_device_id const *id___0 )
{
  void *chip ;
  struct device const __attribute__((__recursive__)) *__mptr ;

  {

  __mptr = (struct device const __attribute__((__recursive__)) *)intf->dev.parent;

  chip = snd_usb_audio_probe((struct usb_device *)((char *)__mptr - (unsigned int )(& ((struct usb_device *)0)->dev)),
                             (struct usb_interface *)intf, id___0);

  if (chip) {

    usb_set_intfdata((struct usb_interface *)intf, chip);

    return (0);
  } else {

    return (-5);
  }
}
}

static void usb_audio_disconnect(struct usb_interface * __attribute__((__noderef__,
                                 __address_space__(2))) intf )
{
  void *tmp ;
  struct device const __attribute__((__recursive__)) *__mptr ;

  {

  tmp = usb_get_intfdata((struct usb_interface *)intf);

  __mptr = (struct device const __attribute__((__recursive__)) *)intf->dev.parent;

  snd_usb_audio_disconnect((struct usb_device *)((char *)__mptr - (unsigned int )(& ((struct usb_device *)0)->dev)),
                           tmp);

  return;
}
}

static int usb_audio_suspend(struct usb_interface *intf , pm_message_t message )
{
  struct snd_usb_audio *chip ;
  void *tmp ;
  struct list_head *p ;
  struct snd_usb_stream *as ;
  struct list_head const *__mptr ;
  int tmp___0 ;

  {

  tmp = usb_get_intfdata(intf);

  chip = (struct snd_usb_audio *)tmp;

  if ((unsigned long )chip == (unsigned long )((void *)-1L)) {

    return (0);
  }

  snd_power_change_state(chip->card, 768U);

  tmp___0 = chip->num_suspended_intf;

  (chip->num_suspended_intf) ++;

  if (! tmp___0) {

    p = (struct list_head *)chip->pcm_list.next;

    while (1) {

      __builtin_prefetch((void const *)p->next);

      if (! ((unsigned long )p != (unsigned long )(& chip->pcm_list))) {

        break;
      }

      __mptr = (struct list_head const *)p;

      as = (struct snd_usb_stream *)((char *)__mptr - (unsigned int )(& ((struct snd_usb_stream *)0)->list));

      snd_pcm_suspend_all((struct snd_pcm *)as->pcm);

      p = (struct list_head *)p->next;
    }
  }

  return (0);
}
}

static int usb_audio_resume(struct usb_interface *intf )
{
  struct snd_usb_audio *chip ;
  void *tmp ;

  {

  tmp = usb_get_intfdata(intf);

  chip = (struct snd_usb_audio *)tmp;

  if ((unsigned long )chip == (unsigned long )((void *)-1L)) {

    return (0);
  }

  (chip->num_suspended_intf) --;

  if (chip->num_suspended_intf) {

    return (0);
  }

  snd_power_change_state(chip->card, 0U);

  return (0);
}
}

static int snd_usb_audio_init(void) __attribute__((__section__(".init.text"), __no_instrument_function__)) ;

static int snd_usb_audio_init(void)
{
  int tmp ;

  {

  if (nrpacks < 1) {

    printk("<4>invalid nrpacks value.\n");

    return (-22);
  } else

  if (nrpacks > 20) {

    printk("<4>invalid nrpacks value.\n");

    return (-22);
  }

  tmp = usb_register(& usb_audio_driver);

  return (tmp);
}
}

static void snd_usb_audio_cleanup(void) __attribute__((__section__(".exit.text"))) ;

static void snd_usb_audio_cleanup(void)
{


  {

  usb_deregister(& usb_audio_driver);

  return;
}
}

int init_module(void)
{
  int tmp ;

  {

  tmp = snd_usb_audio_init();

  return (tmp);
}
}

void cleanup_module(void)
{


  {

  snd_usb_audio_cleanup();

  return;
}
}

#pragma merger(0,"./usbmidi.i","-Wall,-Wundef,-Wstrict-prototypes,-Wno-trigraphs,-fno-strict-aliasing,-fno-common,-Werror-implicit-function-declaration,-Os,-m64,-mtune=generic,-mno-red-zone,-mcmodel=kernel,-funit-at-a-time,-maccumulate-outgoing-args,-pipe,-Wno-sign-compare,-fno-asynchronous-unwind-tables,-mno-sse,-mno-mmx,-mno-sse2,-mno-3dnow,-Wframe-larger-than=2048,-fno-stack-protector,-fno-omit-frame-pointer,-fno-optimize-sibling-calls,-g,-Wdeclaration-after-statement,-Wno-pointer-sign,-fwrapv,-fno-dwarf2-cfi-asm,-g,-Wall,-Wno-attributes,-Wno-unknown-pragmas")

extern void _spin_lock_irq(spinlock_t *lock ) __attribute__((__section__(".spinlock.text"))) ;

extern void _spin_unlock_irq(spinlock_t *lock ) __attribute__((__section__(".spinlock.text"))) ;

extern void init_timer(struct timer_list *timer ) ;

extern int mod_timer(struct timer_list *timer , unsigned long expires ) ;

extern int del_timer_sync(struct timer_list *timer ) ;

extern void __tasklet_schedule(struct tasklet_struct *t ) ;

__inline static void ( __attribute__((__always_inline__)) tasklet_schedule)(struct tasklet_struct *t )
{
  int tmp ;

  {

  tmp = test_and_set_bit(0, (unsigned long volatile *)(& t->state));

  if (! tmp) {

    __tasklet_schedule(t);
  }

  return;
}
}

extern void tasklet_kill(struct tasklet_struct *t ) ;

extern void tasklet_init(struct tasklet_struct *t , void (*func)(unsigned long ) ,
                         unsigned long data ) ;

__inline static int ( __attribute__((__always_inline__)) usb_endpoint_num)(struct usb_endpoint_descriptor const *epd )
{


  {

  return ((int )((int const )epd->bEndpointAddress & 15));
}
}

__inline static int ( __attribute__((__always_inline__)) usb_endpoint_dir_in)(struct usb_endpoint_descriptor const *epd )
{


  {

  return (((int const )epd->bEndpointAddress & 128) == 128);
}
}

__inline static int ( __attribute__((__always_inline__)) usb_endpoint_dir_out)(struct usb_endpoint_descriptor const *epd )
{


  {

  return (((int const )epd->bEndpointAddress & 128) == 0);
}
}

__inline static int ( __attribute__((__always_inline__)) usb_endpoint_xfer_bulk)(struct usb_endpoint_descriptor const *epd )
{


  {

  return (((int const )epd->bmAttributes & 3) == 2);
}
}

__inline static int ( __attribute__((__always_inline__)) usb_endpoint_xfer_int)(struct usb_endpoint_descriptor const *epd )
{


  {

  return (((int const )epd->bmAttributes & 3) == 3);
}
}

__inline static void ( __attribute__((__always_inline__)) usb_fill_bulk_urb)(struct urb *urb ,
                                                                             struct usb_device *dev ,
                                                                             unsigned int pipe ,
                                                                             void *transfer_buffer ,
                                                                             int buffer_length ,
                                                                             void (*complete_fn)(struct urb * ) ,
                                                                             void *context )
{


  {

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )dev;

  urb->pipe = pipe;

  urb->transfer_buffer = (u8 *)transfer_buffer;

  urb->transfer_buffer_length = buffer_length;

  urb->complete = complete_fn;

  urb->context = (u8 * __attribute__((__recursive__, __noderef__)) )context;

  return;
}
}

__inline static void ( __attribute__((__always_inline__)) usb_fill_int_urb)(struct urb *urb ,
                                                                            struct usb_device *dev ,
                                                                            unsigned int pipe ,
                                                                            void *transfer_buffer ,
                                                                            int buffer_length ,
                                                                            void (*complete_fn)(struct urb * ) ,
                                                                            void *context ,
                                                                            int interval )
{


  {

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )dev;

  urb->pipe = pipe;

  urb->transfer_buffer = (u8 *)transfer_buffer;

  urb->transfer_buffer_length = buffer_length;

  urb->complete = complete_fn;

  urb->context = (u8 * __attribute__((__recursive__, __noderef__)) )context;

  if ((unsigned int )dev->speed == 3U) {

    urb->interval = 1 << (interval - 1);
  } else {

    urb->interval = interval;
  }

  urb->start_frame = -1;

  return;
}
}

extern void usb_kill_urb(struct urb *urb ) ;

extern int usb_bulk_msg(struct usb_device *usb_dev , unsigned int pipe , void *data ,
                        int len , int *actual_length , int timeout ) ;

__inline static __u16 ( __attribute__((__always_inline__)) usb_maxpacket)(struct usb_device *udev ,
                                                                          int pipe ,
                                                                          int is_out )
{
  struct usb_host_endpoint *ep ;
  unsigned int epnum ;
  int __ret_warn_on ;
  long tmp ;
  int __ret_warn_on___0 ;
  long tmp___0 ;

  {

  epnum = (unsigned int )((pipe >> 15) & 15);

  if (is_out) {

    __ret_warn_on = ! (! (pipe & 128));

    tmp = __builtin_expect((long )(! (! __ret_warn_on)), 0L);

    if (tmp) {

      warn_slowpath("include/linux/usb.h", 1722, (char const *)((void *)0));
    }

    __builtin_expect((long )(! (! __ret_warn_on)), 0L);

    ep = udev->ep_out[epnum];
  } else {

    __ret_warn_on___0 = ! (! (! (pipe & 128)));

    tmp___0 = __builtin_expect((long )(! (! __ret_warn_on___0)), 0L);

    if (tmp___0) {

      warn_slowpath("include/linux/usb.h", 1725, (char const *)((void *)0));
    }

    __builtin_expect((long )(! (! __ret_warn_on___0)), 0L);

    ep = udev->ep_in[epnum];
  }

  if (! ep) {

    return ((__u16 )0);
  }

  return (ep->desc.wMaxPacketSize);
}
}

extern int snd_rawmidi_new(struct snd_card *card , char * __attribute__((__nullterm__)) id ,
                           int device , int output_count , int input_count , struct snd_rawmidi **rmidi ) ;

extern void snd_rawmidi_set_ops(struct snd_rawmidi *rmidi , int stream , struct snd_rawmidi_ops *ops ) ;

extern int snd_rawmidi_receive(struct snd_rawmidi_substream *substream , unsigned char const *buffer ,
                               int count ) ;

extern int snd_rawmidi_transmit_empty(struct snd_rawmidi_substream *substream ) ;

extern int snd_rawmidi_transmit_peek(struct snd_rawmidi_substream *substream , unsigned char *buffer ,
                                     int count ) ;

extern int snd_rawmidi_transmit_ack(struct snd_rawmidi_substream *substream , int count ) ;

extern int snd_rawmidi_transmit(struct snd_rawmidi_substream *substream , unsigned char *buffer ,
                                int count ) ;

void snd_usbmidi_input_stop(struct list_head *p ) ;

void snd_usbmidi_input_start(struct list_head *p ) ;

static char const __mod_author66[44] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'a', (char const )'u', (char const )'t', (char const )'h',
        (char const )'o', (char const )'r', (char const )'=', (char const )'C',
        (char const )'l', (char const )'e', (char const )'m', (char const )'e',
        (char const )'n', (char const )'s', (char const )' ', (char const )'L',
        (char const )'a', (char const )'d', (char const )'i', (char const )'s',
        (char const )'c', (char const )'h', (char const )' ', (char const )'<',
        (char const )'c', (char const )'l', (char const )'e', (char const )'m',
        (char const )'e', (char const )'n', (char const )'s', (char const )'@',
        (char const )'l', (char const )'a', (char const )'d', (char const )'i',
        (char const )'s', (char const )'c', (char const )'h', (char const )'.',
        (char const )'d', (char const )'e', (char const )'>', (char const )'\000'};

static char const __mod_description67[41] __attribute__((__used__, __unused__,
__section__(".modinfo"))) =

  { (char const )'d', (char const )'e', (char const )'s', (char const )'c',
        (char const )'r', (char const )'i', (char const )'p', (char const )'t',
        (char const )'i', (char const )'o', (char const )'n', (char const )'=',
        (char const )'U', (char const )'S', (char const )'B', (char const )' ',
        (char const )'A', (char const )'u', (char const )'d', (char const )'i',
        (char const )'o', (char const )'/', (char const )'M', (char const )'I',
        (char const )'D', (char const )'I', (char const )' ', (char const )'h',
        (char const )'e', (char const )'l', (char const )'p', (char const )'e',
        (char const )'r', (char const )' ', (char const )'m', (char const )'o',
        (char const )'d', (char const )'u', (char const )'l', (char const )'e',
        (char const )'\000'};

static char const __mod_license68[21] __attribute__((__used__, __unused__, __section__(".modinfo"))) =

  { (char const )'l', (char const )'i', (char const )'c', (char const )'e',
        (char const )'n', (char const )'s', (char const )'e', (char const )'=',
        (char const )'D', (char const )'u', (char const )'a', (char const )'l',
        (char const )' ', (char const )'B', (char const )'S', (char const )'D',
        (char const )'/', (char const )'G', (char const )'P', (char const )'L',
        (char const )'\000'};

static void snd_usbmidi_do_output(struct snd_usb_midi_out_endpoint *ep ) ;

static uint8_t const snd_usbmidi_cin_length[16] =

  { (uint8_t const )0, (uint8_t const )0, (uint8_t const )2, (uint8_t const )3,
        (uint8_t const )3, (uint8_t const )1, (uint8_t const )2, (uint8_t const )3,
        (uint8_t const )3, (uint8_t const )3, (uint8_t const )3, (uint8_t const )3,
        (uint8_t const )2, (uint8_t const )2, (uint8_t const )3, (uint8_t const )1};

static int snd_usbmidi_submit_urb(struct urb *urb , gfp_t flags )
{
  int err ;
  int tmp ;

  {

  tmp = usb_submit_urb(urb, flags);

  err = tmp;

  if (err < 0) {

    if (err != -19) {

      printk("<3>usb_submit_urb: %d\n", err);
    }
  }

  return (err);
}
}

static int snd_usbmidi_urb_error(int status )
{


  {

  switch (status) {
  case -2:
  case -104:
  case -108:
  case -19:

  return (-19);
  case -71:
  case -62:
  case -84:

  return (-5);
  default:

  printk("<3>urb status %d\n", status);

  return (0);
  }
}
}

static void snd_usbmidi_input_data(struct snd_usb_midi_in_endpoint *ep , int portidx ,
                                   uint8_t *data , int length )
{
  struct usbmidi_in_port *port ;
  int tmp___0 ;

  {

  port = & ep->ports[portidx];

  if (! port->substream) {

    while (1) {

      break;
    }

    return;
  }

  tmp___0 = variable_test_bit((port->substream)->number, (unsigned long const volatile *)(& (ep->umidi)->input_triggered));

  if (! tmp___0) {

    return;
  }

  snd_rawmidi_receive(port->substream, (unsigned char const *)data, length);

  return;
}
}

static void snd_usbmidi_in_urb_complete(struct urb *urb )
{
  struct snd_usb_midi_in_endpoint *ep ;
  int err ;
  int tmp ;

  {

  ep = (struct snd_usb_midi_in_endpoint *)urb->context;

  if (urb->status == 0) {

    (*(((ep->umidi)->usb_protocol_ops)->input))(ep, urb->transfer_buffer, urb->actual_length);
  } else {

    tmp = snd_usbmidi_urb_error(urb->status);

    err = tmp;

    if (err < 0) {

      if (err != -19) {

        ep->error_resubmit = (u8 )1;

        mod_timer(& (ep->umidi)->error_timer, (unsigned long )(jiffies + (unsigned long volatile )100));
      }

      return;
    }
  }

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )((ep->umidi)->chip)->dev;

  snd_usbmidi_submit_urb(urb, 32U);

  return;
}
}

static void snd_usbmidi_out_urb_complete(struct urb *urb )
{
  struct snd_usb_midi_out_endpoint *ep ;
  int err ;
  int tmp ;

  {

  ep = (struct snd_usb_midi_out_endpoint *)urb->context;

  _spin_lock(& ep->buffer_lock);

  ep->urb_active = 0;

  _spin_unlock(& ep->buffer_lock);

  if (urb->status < 0) {

    tmp = snd_usbmidi_urb_error(urb->status);

    err = tmp;

    if (err < 0) {

      if (err != -19) {

        mod_timer(& (ep->umidi)->error_timer, (unsigned long )(jiffies + (unsigned long volatile )100));
      }

      return;
    }
  }

  snd_usbmidi_do_output(ep);

  return;
}
}

static void snd_usbmidi_do_output(struct snd_usb_midi_out_endpoint *ep )
{
  struct urb *urb ;
  unsigned long flags ;
  int tmp ;

  {

  urb = (struct urb *)ep->urb;

  while (1) {

    flags = _spin_lock_irqsave(& ep->buffer_lock);

    break;
  }

  if (ep->urb_active) {

    goto _L;
  } else

  if (((ep->umidi)->chip)->shutdown) {
    _L:

    while (1) {

      _spin_unlock_irqrestore(& ep->buffer_lock, flags);

      break;
    }

    return;
  }

  urb->transfer_buffer_length = 0;

  (*(((ep->umidi)->usb_protocol_ops)->output))(ep);

  if (urb->transfer_buffer_length > 0) {

    urb->dev = (struct usb_device * __attribute__((__recursive__)) )((ep->umidi)->chip)->dev;

    tmp = snd_usbmidi_submit_urb(urb, 32U);

    ep->urb_active = tmp >= 0;
  }

  while (1) {

    _spin_unlock_irqrestore(& ep->buffer_lock, flags);

    break;
  }

  return;
}
}

static void snd_usbmidi_out_tasklet(struct snd_usb_midi_out_endpoint *data )
{
  struct snd_usb_midi_out_endpoint *ep ;

  {

  ep = data;

  snd_usbmidi_do_output(ep);

  return;
}
}

static void snd_usbmidi_error_timer(struct snd_usb_midi *data )
{
  struct snd_usb_midi *umidi ;
  int i ;
  struct snd_usb_midi_in_endpoint *in ;

  {

  umidi = data;

  _spin_lock(& umidi->disc_lock);

  if (umidi->disconnected) {

    _spin_unlock(& umidi->disc_lock);

    return;
  }

  i = 0;

  while (i < 2) {

    in = umidi->endpoints[i].in;

    if (in) {

      if (in->error_resubmit) {

        in->error_resubmit = (u8 )0;

        (in->urb)->dev = (struct usb_device * __attribute__((__recursive__)) )(umidi->chip)->dev;

        snd_usbmidi_submit_urb((struct urb *)in->urb, 32U);
      }
    }

    if (umidi->endpoints[i].out) {

      snd_usbmidi_do_output(umidi->endpoints[i].out);
    }

    i ++;
  }

  _spin_unlock(& umidi->disc_lock);

  return;
}
}

static int send_bulk_static_data(struct snd_usb_midi_out_endpoint *ep , void const *data ,
                                 int len )
{
  int err ;
  void *buf ;
  void *tmp ;

  {

  tmp = kmemdup(data, (size_t )len, 208U);

  buf = tmp;

  if (! buf) {

    return (-12);
  }

  err = usb_bulk_msg(((ep->umidi)->chip)->dev, (ep->urb)->pipe, buf, len, (int *)((void *)0),
                     250);

  kfree((void const *)buf);

  return (err);
}
}

void snd_usbmidi_standard_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                                int buffer_length )
{
  int i ;
  int cable ;
  int length ;

  {

  i = 0;

  while (i + 3 < buffer_length) {

    if ((int )*(buffer + i) != 0) {

      cable = (int )*(buffer + i) >> 4;

      length = (int )snd_usbmidi_cin_length[(int )*(buffer + i) & 15];

      snd_usbmidi_input_data(ep, cable, buffer + (i + 1), length);
    }

    i += 4;
  }

  return;
}
}

void snd_usbmidi_midiman_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                               int buffer_length )
{
  int i ;
  int port ;
  int length ;

  {

  i = 0;

  while (i + 3 < buffer_length) {

    if ((int )*(buffer + (i + 3)) != 0) {

      port = (int )*(buffer + (i + 3)) >> 4;

      length = (int )*(buffer + (i + 3)) & 3;

      snd_usbmidi_input_data(ep, port, buffer + i, length);
    }

    i += 4;
  }

  return;
}
}

void snd_usbmidi_maudio_broken_running_status_input(struct snd_usb_midi_in_endpoint *ep ,
                                                    uint8_t *buffer , int buffer_length )
{
  int i ;
  int cable ;
  u8 cin ;
  struct usbmidi_in_port *port ;
  int length ;

  {

  i = 0;

  while (i + 3 < buffer_length) {

    if ((int )*(buffer + i) != 0) {

      cable = (int )*(buffer + i) >> 4;

      cin = (u8 )((int )*(buffer + i) & 15);

      port = & ep->ports[cable];

      length = (int )snd_usbmidi_cin_length[cin];

      if ((int )cin == 15) {

        if (! ((int )*(buffer + (i + 1)) >= 248)) {

          goto _L___0;
        }
      } else
      _L___0:

      if ((int )cin >= 8) {

        if ((int )cin <= 14) {

          port->running_status_length = (u8 )(length - 1);
        } else {

          goto _L;
        }
      } else
      _L:

      if ((int )cin == 4) {

        if ((int )port->running_status_length != 0) {

          if ((int )*(buffer + (i + 1)) < 128) {

            length = (int )port->running_status_length;
          } else {

            port->running_status_length = (u8 )0;
          }
        } else {

          port->running_status_length = (u8 )0;
        }
      } else {

        port->running_status_length = (u8 )0;
      }

      snd_usbmidi_input_data(ep, cable, buffer + (i + 1), length);
    }

    i += 4;
  }

  return;
}
}

void snd_usbmidi_cme_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                           int buffer_length )
{


  {

  if (buffer_length < 2) {

    snd_usbmidi_standard_input(ep, buffer, buffer_length);
  } else

  if (((int )*(buffer + 0) & 15) != 15) {

    snd_usbmidi_standard_input(ep, buffer, buffer_length);
  } else {

    snd_usbmidi_input_data(ep, (int )*(buffer + 0) >> 4, buffer + 1, buffer_length - 1);
  }

  return;
}
}

void snd_usbmidi_output_standard_packet(struct urb *urb , uint8_t p0 , uint8_t p1 ,
                                        uint8_t p2 , uint8_t p3 )
{
  uint8_t *buf ;

  {

  buf = urb->transfer_buffer + urb->transfer_buffer_length;

  *(buf + 0) = p0;

  *(buf + 1) = p1;

  *(buf + 2) = p2;

  *(buf + 3) = p3;

  urb->transfer_buffer_length += 4;

  return;
}
}

void snd_usbmidi_output_midiman_packet(struct urb *urb , uint8_t p0 , uint8_t p1 ,
                                       uint8_t p2 , uint8_t p3 )
{
  uint8_t *buf ;

  {

  buf = urb->transfer_buffer + urb->transfer_buffer_length;

  *(buf + 0) = p1;

  *(buf + 1) = p2;

  *(buf + 2) = p3;

  *(buf + 3) = (uint8_t )(((int )p0 & 240) | (int )snd_usbmidi_cin_length[(int )p0 & 15]);

  urb->transfer_buffer_length += 4;

  return;
}
}

static void snd_usbmidi_transmit_byte(struct usbmidi_out_port *port , uint8_t b ,
                                      struct urb *urb )
{
  uint8_t p0 ;
  void (*output_packet)(struct urb * , uint8_t , uint8_t , uint8_t , uint8_t ) ;

  {

  p0 = port->cable;

  output_packet = (((port->ep)->umidi)->usb_protocol_ops)->output_packet;

  if ((int )b >= 248) {

    (*output_packet)(urb, (uint8_t )((int )p0 | 15), b, (uint8_t )0, (uint8_t )0);
  } else

  if ((int )b >= 240) {

    switch ((int )b) {
    case 240:

    port->data[0] = b;

    port->state = (uint8_t )5;

    break;
    case 241:
    case 243:

    port->data[0] = b;

    port->state = (uint8_t )1;

    break;
    case 242:

    port->data[0] = b;

    port->state = (uint8_t )2;

    break;
    case 244:
    case 245:

    port->state = (uint8_t )0;

    break;
    case 246:

    (*output_packet)(urb, (uint8_t )((int )p0 | 5), (uint8_t )246, (uint8_t )0, (uint8_t )0);

    port->state = (uint8_t )0;

    break;
    case 247:

    switch ((int )port->state) {
    case 4:

    (*output_packet)(urb, (uint8_t )((int )p0 | 5), (uint8_t )247, (uint8_t )0, (uint8_t )0);

    break;
    case 5:

    (*output_packet)(urb, (uint8_t )((int )p0 | 6), port->data[0], (uint8_t )247,
                     (uint8_t )0);

    break;
    case 6:

    (*output_packet)(urb, (uint8_t )((int )p0 | 7), port->data[0], port->data[1],
                     (uint8_t )247);

    break;
    }

    port->state = (uint8_t )0;

    break;
    }
  } else

  if ((int )b >= 128) {

    port->data[0] = b;

    if ((int )b >= 192) {

      if ((int )b <= 223) {

        port->state = (uint8_t )1;
      } else {

        port->state = (uint8_t )2;
      }
    } else {

      port->state = (uint8_t )2;
    }
  } else {

    switch ((int )port->state) {
    case 1:

    if ((int )port->data[0] < 240) {

      p0 = (uint8_t )((int )p0 | ((int )port->data[0] >> 4));
    } else {

      p0 = (uint8_t )((int )p0 | 2);

      port->state = (uint8_t )0;
    }

    (*output_packet)(urb, p0, port->data[0], b, (uint8_t )0);

    break;
    case 2:

    port->data[1] = b;

    port->state = (uint8_t )3;

    break;
    case 3:

    if ((int )port->data[0] < 240) {

      p0 = (uint8_t )((int )p0 | ((int )port->data[0] >> 4));

      port->state = (uint8_t )2;
    } else {

      p0 = (uint8_t )((int )p0 | 3);

      port->state = (uint8_t )0;
    }

    (*output_packet)(urb, p0, port->data[0], port->data[1], b);

    break;
    case 4:

    port->data[0] = b;

    port->state = (uint8_t )5;

    break;
    case 5:

    port->data[1] = b;

    port->state = (uint8_t )6;

    break;
    case 6:

    (*output_packet)(urb, (uint8_t )((int )p0 | 4), port->data[0], port->data[1],
                     b);

    port->state = (uint8_t )4;

    break;
    }
  }

  return;
}
}

void snd_usbmidi_standard_output(struct snd_usb_midi_out_endpoint *ep )
{
  struct urb *urb ;
  int p ;
  struct usbmidi_out_port *port ;
  uint8_t b ;
  int tmp ;

  {

  urb = (struct urb *)ep->urb;

  p = 0;

  while (p < 16) {

    port = & ep->ports[p];

    if (! port->active) {

      goto __Cont;
    }

    while (urb->transfer_buffer_length + 3 < ep->max_transfer) {

      tmp = snd_rawmidi_transmit((struct snd_rawmidi_substream *)port->substream,
                                 & b, 1);

      if (tmp != 1) {

        port->active = 0;

        break;
      }

      snd_usbmidi_transmit_byte(port, b, urb);
    }
    __Cont:

    p ++;
  }

  return;
}
}

void snd_usbmidi_novation_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                                int buffer_length )
{


  {

  if (buffer_length < 2) {

    return;
  } else

  if (! *(buffer + 0)) {

    return;
  } else

  if (buffer_length < (int )*(buffer + 0) + 1) {

    return;
  }

  snd_usbmidi_input_data(ep, 0, buffer + 2, (int )*(buffer + 0) - 1);

  return;
}
}

void snd_usbmidi_novation_output(struct snd_usb_midi_out_endpoint *ep )
{
  uint8_t *transfer_buffer ;
  int count ;

  {

  if (! ep->ports[0].active) {

    return;
  }

  transfer_buffer = (ep->urb)->transfer_buffer;

  count = snd_rawmidi_transmit((struct snd_rawmidi_substream *)ep->ports[0].substream,
                               transfer_buffer + 2, ep->max_transfer - 2);

  if (count < 1) {

    ep->ports[0].active = 0;

    return;
  }

  *(transfer_buffer + 0) = (uint8_t )0;

  *(transfer_buffer + 1) = (uint8_t )count;

  (ep->urb)->transfer_buffer_length = 2 + count;

  return;
}
}

void snd_usbmidi_raw_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                           int buffer_length )
{


  {

  snd_usbmidi_input_data(ep, 0, buffer, buffer_length);

  return;
}
}

void snd_usbmidi_raw_output(struct snd_usb_midi_out_endpoint *ep )
{
  int count ;

  {

  if (! ep->ports[0].active) {

    return;
  }

  count = snd_rawmidi_transmit((struct snd_rawmidi_substream *)ep->ports[0].substream,
                               (ep->urb)->transfer_buffer, ep->max_transfer);

  if (count < 1) {

    ep->ports[0].active = 0;

    return;
  }

  (ep->urb)->transfer_buffer_length = count;

  return;
}
}

void snd_usbmidi_us122l_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                              int buffer_length )
{


  {

  if (buffer_length != 9) {

    return;
  }

  buffer_length = 8;

  while (1) {

    if (buffer_length) {

      if (! ((int )*(buffer + (buffer_length - 1)) == 253)) {

        break;
      }
    } else {

      break;
    }

    buffer_length --;
  }

  if (buffer_length) {

    snd_usbmidi_input_data(ep, 0, buffer, buffer_length);
  }

  return;
}
}

void snd_usbmidi_us122l_output(struct snd_usb_midi_out_endpoint *ep )
{
  int count ;

  {

  if (! ep->ports[0].active) {

    return;
  }

  if ((unsigned int )((ep->urb)->dev)->speed == 3U) {

    count = 1;
  } else {

    count = 2;
  }

  count = snd_rawmidi_transmit((struct snd_rawmidi_substream *)ep->ports[0].substream,
                               (ep->urb)->transfer_buffer, count);

  if (count < 1) {

    ep->ports[0].active = 0;

    return;
  }

  memset((void *)((ep->urb)->transfer_buffer + count), 253, (size_t )(9 - count));

  (ep->urb)->transfer_buffer_length = count;

  return;
}
}

static u8 const init_data[9] =

  { (u8 const )240, (u8 const )0, (u8 const )32, (u8 const )49,
        (u8 const )100, (u8 const )11, (u8 const )0, (u8 const )0,
        (u8 const )247};

void snd_usbmidi_emagic_init_out(struct snd_usb_midi_out_endpoint *ep )
{


  {

  send_bulk_static_data(ep, (void const *)(init_data), (int )sizeof(init_data));

  send_bulk_static_data(ep, (void const *)(init_data), (int )sizeof(init_data));

  return;
}
}

static u8 const finish_data[10] =

  { (u8 const )240, (u8 const )0, (u8 const )32, (u8 const )49,
        (u8 const )100, (u8 const )16, (u8 const )0, (u8 const )127,
        (u8 const )64, (u8 const )247};

void snd_usbmidi_emagic_finish_out(struct snd_usb_midi_out_endpoint *ep )
{


  {

  send_bulk_static_data(ep, (void const *)(finish_data), (int )sizeof(finish_data));

  return;
}
}

void snd_usbmidi_emagic_input(struct snd_usb_midi_in_endpoint *ep , uint8_t *buffer ,
                              int buffer_length )
{
  int i ;

  {

  i = 0;

  while (i < buffer_length) {

    if ((int )*(buffer + i) == 255) {

      buffer_length = i;

      break;
    }

    i ++;
  }

  if (ep->seen_f5) {

    goto switch_port;
  }

  while (buffer_length > 0) {

    i = 0;

    while (i < buffer_length) {

      if ((int )*(buffer + i) == 245) {

        break;
      }

      i ++;
    }

    snd_usbmidi_input_data(ep, ep->current_port, buffer, i);

    buffer += i;

    buffer_length -= i;

    if (buffer_length <= 0) {

      break;
    }

    ep->seen_f5 = (u8 )1;

    buffer ++;

    buffer_length --;
    switch_port:

    if (buffer_length <= 0) {

      break;
    }

    if ((int )*(buffer + 0) < 128) {

      ep->current_port = ((int )*(buffer + 0) - 1) & 15;

      buffer ++;

      buffer_length --;
    }

    ep->seen_f5 = (u8 )0;
  }

  return;
}
}

void snd_usbmidi_emagic_output(struct snd_usb_midi_out_endpoint *ep )
{
  int port0 ;
  uint8_t *buf ;
  int buf_free ;
  int length ;
  int i ;
  int portnum ;
  struct usbmidi_out_port *port ;
  int tmp ;

  {

  port0 = ep->current_port;

  buf = (ep->urb)->transfer_buffer;

  buf_free = ep->max_transfer;

  i = 0;

  while (i < 16) {

    portnum = (port0 + i) & 15;

    port = & ep->ports[portnum];

    if (! port->active) {

      goto __Cont;
    }

    tmp = snd_rawmidi_transmit_peek((struct snd_rawmidi_substream *)port->substream,
                                    buf, 1);

    if (tmp != 1) {

      port->active = 0;

      goto __Cont;
    }

    if (portnum != ep->current_port) {

      if (buf_free < 2) {

        break;
      }

      ep->current_port = portnum;

      *(buf + 0) = (uint8_t )245;

      *(buf + 1) = (uint8_t )((portnum + 1) & 15);

      buf += 2;

      buf_free -= 2;
    }

    if (buf_free < 1) {

      break;
    }

    length = snd_rawmidi_transmit((struct snd_rawmidi_substream *)port->substream,
                                  buf, buf_free);

    if (length > 0) {

      buf += length;

      buf_free -= length;

      if (buf_free < 1) {

        break;
      }
    }
    __Cont:

    i ++;
  }

  if (buf_free < ep->max_transfer) {

    if (buf_free > 0) {

      *buf = (uint8_t )255;

      buf_free --;
    }
  }

  (ep->urb)->transfer_buffer_length = ep->max_transfer - buf_free;

  return;
}
}

static int snd_usbmidi_output_open(struct snd_rawmidi_substream *substream )
{
  struct snd_usb_midi *umidi ;
  struct usbmidi_out_port *port ;
  int i ;
  int j ;

  {

  umidi = (struct snd_usb_midi *)(substream->rmidi)->private_data;

  port = (struct usbmidi_out_port *)((void *)0);

  i = 0;

  while (i < 2) {

    if (umidi->endpoints[i].out) {

      j = 0;

      while (j < 16) {

        if ((unsigned long )(umidi->endpoints[i].out)->ports[j].substream == (unsigned long )substream) {

          port = & (umidi->endpoints[i].out)->ports[j];

          break;
        }

        j ++;
      }
    }

    i ++;
  }

  if (! port) {

    while (1) {

      break;
    }

    return (-6);
  }

  (substream->runtime)->private_data = (void * __attribute__((__recursive__)) )port;

  port->state = (uint8_t )0;

  return (0);
}
}

static int snd_usbmidi_output_close(struct snd_rawmidi_substream *substream )
{


  {

  return (0);
}
}

static void snd_usbmidi_output_trigger(struct snd_rawmidi_substream *substream , int up___0 )
{
  struct usbmidi_out_port *port ;
  int tmp ;

  {

  port = (struct usbmidi_out_port *)(substream->runtime)->private_data;

  port->active = up___0;

  if (up___0) {

    if ((((port->ep)->umidi)->chip)->shutdown) {

      while (1) {

        tmp = snd_rawmidi_transmit_empty(substream);

        if (tmp) {

          break;
        }

        snd_rawmidi_transmit_ack(substream, 1);
      }

      return;
    }

    tasklet_schedule(& (port->ep)->tasklet);
  }

  return;
}
}

static int snd_usbmidi_input_open(struct snd_rawmidi_substream *substream )
{


  {

  return (0);
}
}

static int snd_usbmidi_input_close(struct snd_rawmidi_substream *substream )
{


  {

  return (0);
}
}

static void snd_usbmidi_input_trigger(struct snd_rawmidi_substream *substream , int up___0 )
{
  struct snd_usb_midi *umidi ;

  {

  umidi = (struct snd_usb_midi *)(substream->rmidi)->private_data;

  if (up___0) {

    set_bit((unsigned int )substream->number, (unsigned long volatile *)(& umidi->input_triggered));
  } else {

    clear_bit(substream->number, (unsigned long volatile *)(& umidi->input_triggered));
  }

  return;
}
}

static struct snd_rawmidi_ops snd_usbmidi_output_ops = {& snd_usbmidi_output_open, & snd_usbmidi_output_close, & snd_usbmidi_output_trigger,
    (void (*)(struct snd_rawmidi_substream *substream ))0};

static struct snd_rawmidi_ops snd_usbmidi_input_ops = {& snd_usbmidi_input_open, & snd_usbmidi_input_close, & snd_usbmidi_input_trigger,
    (void (*)(struct snd_rawmidi_substream *substream ))0};

static void snd_usbmidi_in_endpoint_delete(struct snd_usb_midi_in_endpoint *ep )
{


  {

  if (ep->urb) {

    usb_buffer_free(((ep->umidi)->chip)->dev, (size_t )(ep->urb)->transfer_buffer_length,
                    (void *)(ep->urb)->transfer_buffer, (ep->urb)->transfer_dma);

    usb_free_urb((struct urb *)ep->urb);
  }

  kfree((void const *)ep);

  return;
}
}

static int snd_usbmidi_in_endpoint_create(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *ep_info ,
                                          struct snd_usb_midi_endpoint *rep )
{
  struct snd_usb_midi_in_endpoint *ep ;
  void *buffer ;
  unsigned int pipe ;
  int length ;
  void *tmp ;
  struct urb *tmp___0 ;
  unsigned int tmp___1 ;
  unsigned int tmp___2 ;
  __u16 tmp___3 ;

  {

  rep->in = (struct snd_usb_midi_in_endpoint *)((void *)0);

  tmp = kzalloc(sizeof(*ep), 208U);

  ep = (struct snd_usb_midi_in_endpoint *)tmp;

  if (! ep) {

    return (-12);
  }

  ep->umidi = umidi;

  tmp___0 = usb_alloc_urb(0, 208U);

  ep->urb = (struct urb * __attribute__((__recursive__)) )tmp___0;

  if (! ep->urb) {

    snd_usbmidi_in_endpoint_delete(ep);

    return (-12);
  }

  if (ep_info->in_interval) {

    tmp___1 = __create_pipe((umidi->chip)->dev, (unsigned int )ep_info->in_ep);

    pipe = ((unsigned int )(1 << 30) | tmp___1) | 128U;
  } else {

    tmp___2 = __create_pipe((umidi->chip)->dev, (unsigned int )ep_info->in_ep);

    pipe = ((unsigned int )(3 << 30) | tmp___2) | 128U;
  }

  tmp___3 = usb_maxpacket((umidi->chip)->dev, (int )pipe, 0);

  length = (int )tmp___3;

  buffer = usb_buffer_alloc((umidi->chip)->dev, (size_t )length, 208U, & (ep->urb)->transfer_dma);

  if (! buffer) {

    snd_usbmidi_in_endpoint_delete(ep);

    return (-12);
  }

  if (ep_info->in_interval) {

    usb_fill_int_urb((struct urb *)ep->urb, (umidi->chip)->dev, pipe, buffer, length,
                     & snd_usbmidi_in_urb_complete, (void *)ep, (int )ep_info->in_interval);
  } else {

    usb_fill_bulk_urb((struct urb *)ep->urb, (umidi->chip)->dev, pipe, buffer, length,
                      & snd_usbmidi_in_urb_complete, (void *)ep);
  }

  (ep->urb)->transfer_flags = 4U;

  rep->in = ep;

  return (0);
}
}

static unsigned int snd_usbmidi_count_bits(unsigned int x )
{
  unsigned int bits ;

  {

  bits = 0U;

  while (x) {

    x &= x - 1U;

    bits ++;
  }

  return (bits);
}
}

static void snd_usbmidi_out_endpoint_delete(struct snd_usb_midi_out_endpoint *ep )
{


  {

  if (ep->urb) {

    usb_buffer_free(((ep->umidi)->chip)->dev, (size_t )ep->max_transfer, (void *)(ep->urb)->transfer_buffer,
                    (ep->urb)->transfer_dma);

    usb_free_urb((struct urb *)ep->urb);
  }

  kfree((void const *)ep);

  return;
}
}

static struct lock_class_key __key___4 ;

static int snd_usbmidi_out_endpoint_create(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *ep_info ,
                                           struct snd_usb_midi_endpoint *rep )
{
  struct snd_usb_midi_out_endpoint *ep ;
  int i ;
  unsigned int pipe ;
  void *buffer ;
  void *tmp ;
  struct urb *tmp___0 ;
  unsigned int tmp___1 ;
  unsigned int tmp___2 ;
  __u16 tmp___3 ;

  {

  rep->out = (struct snd_usb_midi_out_endpoint *)((void *)0);

  tmp = kzalloc(sizeof(*ep), 208U);

  ep = (struct snd_usb_midi_out_endpoint *)tmp;

  if (! ep) {

    return (-12);
  }

  ep->umidi = (struct snd_usb_midi * __attribute__((__recursive__)) )umidi;

  tmp___0 = usb_alloc_urb(0, 208U);

  ep->urb = (struct urb * __attribute__((__recursive__)) )tmp___0;

  if (! ep->urb) {

    snd_usbmidi_out_endpoint_delete(ep);

    return (-12);
  }

  if (ep_info->out_interval) {

    tmp___1 = __create_pipe((umidi->chip)->dev, (unsigned int )ep_info->out_ep);

    pipe = (unsigned int )(1 << 30) | tmp___1;
  } else {

    tmp___2 = __create_pipe((umidi->chip)->dev, (unsigned int )ep_info->out_ep);

    pipe = (unsigned int )(3 << 30) | tmp___2;
  }

  if ((umidi->chip)->usb_id == (u32 )((2706 << 16) | 4128)) {

    ep->max_transfer = 4;
  } else {

    tmp___3 = usb_maxpacket((umidi->chip)->dev, (int )pipe, 1);

    ep->max_transfer = (int )tmp___3;
  }

  buffer = usb_buffer_alloc((umidi->chip)->dev, (size_t )ep->max_transfer, 208U, & (ep->urb)->transfer_dma);

  if (! buffer) {

    snd_usbmidi_out_endpoint_delete(ep);

    return (-12);
  }

  if (ep_info->out_interval) {

    usb_fill_int_urb((struct urb *)ep->urb, (umidi->chip)->dev, pipe, buffer, ep->max_transfer,
                     & snd_usbmidi_out_urb_complete, (void *)ep, (int )ep_info->out_interval);
  } else {

    usb_fill_bulk_urb((struct urb *)ep->urb, (umidi->chip)->dev, pipe, buffer, ep->max_transfer,
                      & snd_usbmidi_out_urb_complete, (void *)ep);
  }

  (ep->urb)->transfer_flags = 4U;

  while (1) {

    __spin_lock_init(& ep->buffer_lock, "&ep->buffer_lock", & __key___4);

    break;
  }

  tasklet_init(& ep->tasklet, (void (*)(unsigned long ))(& snd_usbmidi_out_tasklet),
               (unsigned long )ep);

  i = 0;

  while (i < 16) {

    if ((int )ep_info->out_cables & (1 << i)) {

      ep->ports[i].ep = ep;

      ep->ports[i].cable = (uint8_t )(i << 4);
    }

    i ++;
  }

  if ((umidi->usb_protocol_ops)->init_out_endpoint) {

    (*((umidi->usb_protocol_ops)->init_out_endpoint))(ep);
  }

  rep->out = ep;

  return (0);
}
}

static void snd_usbmidi_free(struct snd_usb_midi *umidi )
{
  int i ;
  struct snd_usb_midi_endpoint *ep ;

  {

  i = 0;

  while (i < 2) {

    ep = & umidi->endpoints[i];

    if (ep->out) {

      snd_usbmidi_out_endpoint_delete(ep->out);
    }

    if (ep->in) {

      snd_usbmidi_in_endpoint_delete(ep->in);
    }

    i ++;
  }

  kfree((void const *)umidi);

  return;
}
}

void snd_usbmidi_disconnect(struct list_head *p )
{
  struct snd_usb_midi *umidi ;
  int i ;
  struct list_head const __attribute__((__recursive__)) *__mptr ;
  struct snd_usb_midi_endpoint *ep ;

  {

  __mptr = (struct list_head const __attribute__((__recursive__)) *)p;

  umidi = (struct snd_usb_midi *)((char *)__mptr - (unsigned int )(& ((struct snd_usb_midi *)0)->list));

  _spin_lock_irq(& umidi->disc_lock);

  umidi->disconnected = (unsigned char)1;

  _spin_unlock_irq(& umidi->disc_lock);

  i = 0;

  while (i < 2) {

    ep = & umidi->endpoints[i];

    if (ep->out) {

      tasklet_kill(& (ep->out)->tasklet);
    }

    if (ep->out) {

      if ((ep->out)->urb) {

        usb_kill_urb((struct urb *)(ep->out)->urb);

        if ((umidi->usb_protocol_ops)->finish_out_endpoint) {

          (*((umidi->usb_protocol_ops)->finish_out_endpoint))(ep->out);
        }
      }
    }

    if (ep->in) {

      usb_kill_urb((struct urb *)(ep->in)->urb);
    }

    if (ep->out) {

      snd_usbmidi_out_endpoint_delete(ep->out);

      ep->out = (struct snd_usb_midi_out_endpoint *)((void *)0);
    }

    if (ep->in) {

      snd_usbmidi_in_endpoint_delete(ep->in);

      ep->in = (struct snd_usb_midi_in_endpoint *)((void *)0);
    }

    i ++;
  }

  del_timer_sync(& umidi->error_timer);

  return;
}
}

static void snd_usbmidi_rawmidi_free(struct snd_rawmidi *rmidi )
{
  struct snd_usb_midi *umidi ;

  {

  umidi = (struct snd_usb_midi *)rmidi->private_data;

  snd_usbmidi_free(umidi);

  return;
}
}

static struct snd_rawmidi_substream *snd_usbmidi_find_substream(struct snd_usb_midi *umidi ,
                                                                int stream , int number )
{
  struct list_head *list ;
  struct snd_rawmidi_substream *substream ;
  struct list_head const __attribute__((__noderef__)) *__mptr ;

  {

  list = (struct list_head *)(umidi->rmidi)->streams[stream].substreams.next;

  while (1) {

    __builtin_prefetch((void const *)list->next);

    if (! ((unsigned long )list != (unsigned long )(& (umidi->rmidi)->streams[stream].substreams))) {

      break;
    }

    __mptr = (struct list_head const __attribute__((__noderef__)) *)list;

    substream = (struct snd_rawmidi_substream *)((char *)__mptr - (unsigned int )(& ((struct snd_rawmidi_substream *)0)->list));

    if (substream->number == number) {

      return (substream);
    }

    list = (struct list_head *)list->next;
  }

  return ((struct snd_rawmidi_substream *)((void *)0));
}
}

static struct port_info snd_usbmidi_port_info[57] =

  { {(u32 )(1410 << 16), (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 3), (short)0, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 3),
      (short)1, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 3),
      (short)2, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part C",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 3),
      (short)3, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part D",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 3),
      (short)4, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI 1",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 3), (short)5, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI 2",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 4), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 4), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 7), (short)0, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 7),
      (short)1, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 7),
      (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 11), (short)0, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 11),
      (short)1, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 11),
      (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 12), (short)0, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 12),
      (short)1, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )((((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 5)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 12),
      (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 20), (short)8, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 22), (short)0, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )(((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 22),
      (short)1, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )(((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 22),
      (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI 1",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 22), (short)3, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI 2",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 35), (short)5, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 39), (short)0, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )(((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 39),
      (short)1, (short)64, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )(((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 39),
      (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 41), (short)0, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part A",
      (unsigned int )(((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 41),
      (short)1, (short)128, (char const * __attribute__((__nullterm__)) )"%s Part B",
      (unsigned int )(((((((1 << 1) | (1 << 2)) | (1 << 6)) | (1 << 3)) | (1 << 4)) | (1 << 16)) | (1 << 18))},
        {(u32 )((1410 << 16) | 41),
      (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI 1",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 41), (short)3, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI 2",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 43), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 43), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 47), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 47), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s External MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 47), (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s Sync",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 51), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 51), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s 1",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 51), (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s 2",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 59), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 59), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 68), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 68), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1410 << 16) | 72), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 72), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s 1",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 72), (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s 2",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 77), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 77), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s 1",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 77), (short)2, (short)0, (char const * __attribute__((__nullterm__)) )"%s 2",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((1410 << 16) | 154), (short)3, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1891 << 16) | 4145), (short)8, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((1891 << 16) | 4147), (short)8, (short)0, (char const * __attribute__((__nullterm__)) )"%s Control",
      (unsigned int )((1 << 1) | (1 << 16))},
        {(u32 )((2045 << 16) | 1), (short)0, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI A",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((2045 << 16) | 1), (short)1, (short)0, (char const * __attribute__((__nullterm__)) )"%s MIDI B",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((2154 << 16) | 1), (short)8, (short)0, (char const * __attribute__((__nullterm__)) )"%s Broadcast",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((2154 << 16) | 2), (short)8, (short)0, (char const * __attribute__((__nullterm__)) )"%s Broadcast",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))},
        {(u32 )((2154 << 16) | 3), (short)4, (short)0, (char const * __attribute__((__nullterm__)) )"%s Broadcast",
      (unsigned int )(((1 << 1) | (1 << 16)) | (1 << 19))}};

static struct port_info *find_port_info(struct snd_usb_midi *umidi , int number )
{
  int i ;

  {

  i = 0;

  while ((unsigned long )i < sizeof(snd_usbmidi_port_info) / sizeof(snd_usbmidi_port_info[0]) + (sizeof(char [1]) - 1UL)) {

    if (snd_usbmidi_port_info[i].id == (umidi->chip)->usb_id) {

      if ((int )snd_usbmidi_port_info[i].port == number) {

        return (& snd_usbmidi_port_info[i]);
      }
    }

    i ++;
  }

  return ((struct port_info *)((void *)0));
}
}

static void snd_usbmidi_get_port_info(struct snd_rawmidi *rmidi , int number , struct snd_seq_port_info *seq_port_info )
{
  struct snd_usb_midi *umidi ;
  struct port_info *port_info ;

  {

  umidi = (struct snd_usb_midi *)rmidi->private_data;

  port_info = find_port_info(umidi, number);

  if (port_info) {

    seq_port_info->type = port_info->seq_flags;

    seq_port_info->midi_voices = (int )port_info->voices;
  }

  return;
}
}

static void snd_usbmidi_init_substream(struct snd_usb_midi *umidi , int stream , int number ,
                                       struct snd_rawmidi_substream **rsubstream )
{
  struct port_info *port_info ;
  char const * __attribute__((__nullterm__)) name_format ;
  struct snd_rawmidi_substream *substream ;
  struct snd_rawmidi_substream *tmp ;

  {

  tmp = snd_usbmidi_find_substream(umidi, stream, number);

  substream = tmp;

  if (! substream) {

    while (1) {

      break;
    }

    return;
  }

  port_info = find_port_info(umidi, number);

  if (port_info) {

    name_format = port_info->name;
  } else {

    name_format = (char const * __attribute__((__nullterm__)) )"%s MIDI %d";
  }

  sprintf(substream->name, (char const *)name_format, ((umidi->chip)->card)->shortname,
          number + 1);

  *rsubstream = substream;

  return;
}
}

static int snd_usbmidi_create_endpoints(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *endpoints )
{
  int i ;
  int j ;
  int err ;
  int out_ports ;
  int in_ports ;

  {

  out_ports = 0;

  in_ports = 0;

  i = 0;

  while (i < 2) {

    if ((endpoints + i)->out_cables) {

      err = snd_usbmidi_out_endpoint_create(umidi, endpoints + i, & umidi->endpoints[i]);

      if (err < 0) {

        return (err);
      }
    }

    if ((endpoints + i)->in_cables) {

      err = snd_usbmidi_in_endpoint_create(umidi, endpoints + i, & umidi->endpoints[i]);

      if (err < 0) {

        return (err);
      }
    }

    j = 0;

    while (j < 16) {

      if ((int )(endpoints + i)->out_cables & (1 << j)) {

        snd_usbmidi_init_substream(umidi, 0, out_ports, (struct snd_rawmidi_substream **)(& (umidi->endpoints[i].out)->ports[j].substream));

        out_ports ++;
      }

      if ((int )(endpoints + i)->in_cables & (1 << j)) {

        snd_usbmidi_init_substream(umidi, 1, in_ports, & (umidi->endpoints[i].in)->ports[j].substream);

        in_ports ++;
      }

      j ++;
    }

    i ++;
  }

  printk("<6>created %d output and %d input ports\n", out_ports, in_ports);

  return (0);
}
}

static int snd_usbmidi_get_ms_info(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *endpoints )
{
  struct usb_interface *intf ;
  struct usb_host_interface *hostif ;
  struct usb_interface_descriptor *intfd ;
  struct usb_ms_header_descriptor *ms_header ;
  struct usb_host_endpoint *hostep ;
  struct usb_endpoint_descriptor *ep ;
  struct usb_ms_endpoint_descriptor *ms_ep ;
  int i ;
  int epidx ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;

  {

  intf = umidi->iface;

  if (! intf) {

    return (-6);
  }

  hostif = intf->altsetting + 0;

  intfd = & hostif->desc;

  ms_header = (struct usb_ms_header_descriptor *)hostif->extra;

  if (hostif->extralen >= 7) {

    if ((int )ms_header->bLength >= 7) {

      if ((int )ms_header->bDescriptorType == ((1 << 5) | 4)) {

        if ((int )ms_header->bDescriptorSubtype == 1) {

          printk("<6>MIDIStreaming version %02x.%02x\n", (int )ms_header->bcdMSC[1],
                 (int )ms_header->bcdMSC[0]);
        } else {

          printk("<4>MIDIStreaming interface descriptor not found\n");
        }
      } else {

        printk("<4>MIDIStreaming interface descriptor not found\n");
      }
    } else {

      printk("<4>MIDIStreaming interface descriptor not found\n");
    }
  } else {

    printk("<4>MIDIStreaming interface descriptor not found\n");
  }

  epidx = 0;

  i = 0;

  while (i < (int )intfd->bNumEndpoints) {

    hostep = hostif->endpoint + i;

    ep = & hostep->desc;

    tmp = usb_endpoint_xfer_bulk((struct usb_endpoint_descriptor const *)ep);

    if (! tmp) {

      tmp___0 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)ep);

      if (! tmp___0) {

        goto __Cont;
      }
    }

    ms_ep = (struct usb_ms_endpoint_descriptor *)hostep->extra;

    if (hostep->extralen < 4) {

      goto __Cont;
    } else

    if ((int )ms_ep->bLength < 4) {

      goto __Cont;
    } else

    if ((int )ms_ep->bDescriptorType != ((1 << 5) | 5)) {

      goto __Cont;
    } else

    if ((int )ms_ep->bDescriptorSubtype != 1) {

      goto __Cont;
    }

    tmp___5 = usb_endpoint_dir_out((struct usb_endpoint_descriptor const *)ep);

    if (tmp___5) {

      if ((endpoints + epidx)->out_ep) {

        epidx ++;

        if (epidx >= 2) {

          printk("<4>too many endpoints\n");

          break;
        }
      }

      tmp___1 = usb_endpoint_num((struct usb_endpoint_descriptor const *)ep);

      (endpoints + epidx)->out_ep = (int8_t )tmp___1;

      tmp___2 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)ep);

      if (tmp___2) {

        (endpoints + epidx)->out_interval = ep->bInterval;
      } else

      if ((unsigned int )((umidi->chip)->dev)->speed == 1U) {

        (endpoints + epidx)->out_interval = (uint8_t )1;
      }

      (endpoints + epidx)->out_cables = (uint16_t )((1 << (int )ms_ep->bNumEmbMIDIJack) - 1);

      printk("<6>EP %02X: %d jack(s)\n", (int )ep->bEndpointAddress, (int )ms_ep->bNumEmbMIDIJack);
    } else {

      if ((endpoints + epidx)->in_ep) {

        epidx ++;

        if (epidx >= 2) {

          printk("<4>too many endpoints\n");

          break;
        }
      }

      tmp___3 = usb_endpoint_num((struct usb_endpoint_descriptor const *)ep);

      (endpoints + epidx)->in_ep = (int8_t )tmp___3;

      tmp___4 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)ep);

      if (tmp___4) {

        (endpoints + epidx)->in_interval = ep->bInterval;
      } else

      if ((unsigned int )((umidi->chip)->dev)->speed == 1U) {

        (endpoints + epidx)->in_interval = (uint8_t )1;
      }

      (endpoints + epidx)->in_cables = (uint16_t )((1 << (int )ms_ep->bNumEmbMIDIJack) - 1);

      printk("<6>EP %02X: %d jack(s)\n", (int )ep->bEndpointAddress, (int )ms_ep->bNumEmbMIDIJack);
    }
    __Cont:

    i ++;
  }

  return (0);
}
}

static void snd_usbmidi_switch_roland_altsetting(struct snd_usb_midi *umidi )
{
  struct usb_interface *intf ;
  struct usb_host_interface *hostif ;
  struct usb_interface_descriptor *intfd ;

  {

  intf = umidi->iface;

  if (! intf) {

    return;
  } else

  if (intf->num_altsetting != 2U) {

    return;
  }

  hostif = intf->altsetting + 1;

  intfd = & hostif->desc;

  if ((int )intfd->bNumEndpoints != 2) {

    return;
  } else

  if (((int )(hostif->endpoint + 0)->desc.bmAttributes & 3) != 2) {

    return;
  } else

  if (((int )(hostif->endpoint + 1)->desc.bmAttributes & 3) != 3) {

    return;
  }

  printk("<6>switching to altsetting %d with int ep\n", (int )intfd->bAlternateSetting);

  usb_set_interface((umidi->chip)->dev, (int )intfd->bInterfaceNumber, (int )intfd->bAlternateSetting);

  return;
}
}

static int snd_usbmidi_detect_endpoints(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *endpoint ,
                                        int max_endpoints )
{
  struct usb_interface *intf ;
  struct usb_host_interface *hostif ;
  struct usb_interface_descriptor *intfd ;
  struct usb_endpoint_descriptor *epd ;
  int i ;
  int out_eps ;
  int in_eps ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  int tmp___6 ;
  int tmp___7 ;

  {

  out_eps = 0;

  in_eps = 0;

  if ((umidi->chip)->usb_id >> 16 == 1410U) {

    snd_usbmidi_switch_roland_altsetting(umidi);
  }

  if ((endpoint + 0)->out_ep) {

    return (0);
  } else

  if ((endpoint + 0)->in_ep) {

    return (0);
  }

  intf = umidi->iface;

  if (! intf) {

    return (-2);
  } else

  if (intf->num_altsetting < 1U) {

    return (-2);
  }

  hostif = intf->cur_altsetting;

  intfd = & hostif->desc;

  i = 0;

  while (i < (int )intfd->bNumEndpoints) {

    epd = & (hostif->endpoint + i)->desc;

    tmp = usb_endpoint_xfer_bulk((struct usb_endpoint_descriptor const *)epd);

    if (! tmp) {

      tmp___0 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)epd);

      if (! tmp___0) {

        goto __Cont;
      }
    }

    if (out_eps < max_endpoints) {

      tmp___3 = usb_endpoint_dir_out((struct usb_endpoint_descriptor const *)epd);

      if (tmp___3) {

        tmp___1 = usb_endpoint_num((struct usb_endpoint_descriptor const *)epd);

        (endpoint + out_eps)->out_ep = (int8_t )tmp___1;

        tmp___2 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)epd);

        if (tmp___2) {

          (endpoint + out_eps)->out_interval = epd->bInterval;
        }

        out_eps ++;
      }
    }

    if (in_eps < max_endpoints) {

      tmp___6 = usb_endpoint_dir_in((struct usb_endpoint_descriptor const *)epd);

      if (tmp___6) {

        tmp___4 = usb_endpoint_num((struct usb_endpoint_descriptor const *)epd);

        (endpoint + in_eps)->in_ep = (int8_t )tmp___4;

        tmp___5 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)epd);

        if (tmp___5) {

          (endpoint + in_eps)->in_interval = epd->bInterval;
        }

        in_eps ++;
      }
    }
    __Cont:

    i ++;
  }

  if (out_eps) {

    tmp___7 = 0;
  } else

  if (in_eps) {

    tmp___7 = 0;
  } else {

    tmp___7 = -2;
  }

  return (tmp___7);
}
}

static int snd_usbmidi_detect_per_port_endpoints(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *endpoints )
{
  int err ;
  int i ;

  {

  err = snd_usbmidi_detect_endpoints(umidi, endpoints, 2);

  i = 0;

  while (i < 2) {

    if ((endpoints + i)->out_ep) {

      (endpoints + i)->out_cables = (uint16_t )1;
    }

    if ((endpoints + i)->in_ep) {

      (endpoints + i)->in_cables = (uint16_t )1;
    }

    i ++;
  }

  return (err);
}
}

static int snd_usbmidi_detect_yamaha(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *endpoint )
{
  struct usb_interface *intf ;
  struct usb_host_interface *hostif ;
  struct usb_interface_descriptor *intfd ;
  uint8_t *cs_desc ;
  int tmp ;

  {

  intf = umidi->iface;

  if (! intf) {

    return (-2);
  }

  hostif = intf->altsetting;

  intfd = & hostif->desc;

  if ((int )intfd->bNumEndpoints < 1) {

    return (-2);
  }

  cs_desc = (uint8_t *)hostif->extra;

  while (1) {

    if ((unsigned long )cs_desc < (unsigned long )(hostif->extra + hostif->extralen)) {

      if (! ((int )*(cs_desc + 0) >= 2)) {

        break;
      }
    } else {

      break;
    }

    if ((int )*(cs_desc + 1) == ((1 << 5) | 4)) {

      if ((int )*(cs_desc + 2) == 2) {

        endpoint->in_cables = (uint16_t )(((int )endpoint->in_cables << 1) | 1);
      } else

      if ((int )*(cs_desc + 2) == 3) {

        endpoint->out_cables = (uint16_t )(((int )endpoint->out_cables << 1) | 1);
      }
    }

    cs_desc += (int )*(cs_desc + 0);
  }

  if (! endpoint->in_cables) {

    if (! endpoint->out_cables) {

      return (-2);
    }
  }

  tmp = snd_usbmidi_detect_endpoints(umidi, endpoint, 1);

  return (tmp);
}
}

static int snd_usbmidi_create_endpoints_midiman(struct snd_usb_midi *umidi , struct snd_usb_midi_endpoint_info *endpoint )
{
  struct snd_usb_midi_endpoint_info ep_info ;
  struct usb_interface *intf ;
  struct usb_host_interface *hostif ;
  struct usb_interface_descriptor *intfd ;
  struct usb_endpoint_descriptor *epd ;
  int cable ;
  int err ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;

  {

  intf = umidi->iface;

  if (! intf) {

    return (-2);
  }

  hostif = intf->altsetting;

  intfd = & hostif->desc;

  if ((int )endpoint->out_cables > 1) {

    tmp = 5;
  } else {

    tmp = 3;
  }

  if ((int )intfd->bNumEndpoints < tmp) {

    printk("<3>not enough endpoints\n");

    return (-2);
  }

  epd = & (hostif->endpoint + 0)->desc;

  tmp___0 = usb_endpoint_dir_in((struct usb_endpoint_descriptor const *)epd);

  if (tmp___0) {

    tmp___1 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)epd);

    if (! tmp___1) {

      printk("<3>endpoint[0] isn\'t interrupt\n");

      return (-6);
    }
  } else {

    printk("<3>endpoint[0] isn\'t interrupt\n");

    return (-6);
  }

  epd = & (hostif->endpoint + 2)->desc;

  tmp___2 = usb_endpoint_dir_out((struct usb_endpoint_descriptor const *)epd);

  if (tmp___2) {

    tmp___3 = usb_endpoint_xfer_bulk((struct usb_endpoint_descriptor const *)epd);

    if (! tmp___3) {

      printk("<3>endpoint[2] isn\'t bulk output\n");

      return (-6);
    }
  } else {

    printk("<3>endpoint[2] isn\'t bulk output\n");

    return (-6);
  }

  if ((int )endpoint->out_cables > 1) {

    epd = & (hostif->endpoint + 4)->desc;

    tmp___4 = usb_endpoint_dir_out((struct usb_endpoint_descriptor const *)epd);

    if (tmp___4) {

      tmp___5 = usb_endpoint_xfer_bulk((struct usb_endpoint_descriptor const *)epd);

      if (! tmp___5) {

        printk("<3>endpoint[4] isn\'t bulk output\n");

        return (-6);
      }
    } else {

      printk("<3>endpoint[4] isn\'t bulk output\n");

      return (-6);
    }
  }

  ep_info.out_ep = (int8_t )((int )(hostif->endpoint + 2)->desc.bEndpointAddress & 15);

  ep_info.out_interval = (uint8_t )0;

  ep_info.out_cables = (uint16_t )((int )endpoint->out_cables & 21845);

  err = snd_usbmidi_out_endpoint_create(umidi, & ep_info, & umidi->endpoints[0]);

  if (err < 0) {

    return (err);
  }

  ep_info.in_ep = (int8_t )((int )(hostif->endpoint + 0)->desc.bEndpointAddress & 15);

  ep_info.in_interval = (hostif->endpoint + 0)->desc.bInterval;

  ep_info.in_cables = endpoint->in_cables;

  err = snd_usbmidi_in_endpoint_create(umidi, & ep_info, & umidi->endpoints[0]);

  if (err < 0) {

    return (err);
  }

  if ((int )endpoint->out_cables > 1) {

    ep_info.out_ep = (int8_t )((int )(hostif->endpoint + 4)->desc.bEndpointAddress & 15);

    ep_info.out_cables = (uint16_t )((int )endpoint->out_cables & 43690);

    err = snd_usbmidi_out_endpoint_create(umidi, & ep_info, & umidi->endpoints[1]);

    if (err < 0) {

      return (err);
    }
  }

  cable = 0;

  while (cable < 16) {

    if ((int )endpoint->out_cables & (1 << cable)) {

      snd_usbmidi_init_substream(umidi, 0, cable, (struct snd_rawmidi_substream **)(& (umidi->endpoints[cable & 1].out)->ports[cable].substream));
    }

    if ((int )endpoint->in_cables & (1 << cable)) {

      snd_usbmidi_init_substream(umidi, 1, cable, & (umidi->endpoints[0].in)->ports[cable].substream);
    }

    cable ++;
  }

  return (0);
}
}

static struct snd_rawmidi_global_ops snd_usbmidi_ops = {(int (*)(struct snd_rawmidi *rmidi ))0, (int (*)(struct snd_rawmidi *rmidi ))0,
    & snd_usbmidi_get_port_info};

static int snd_usbmidi_create_rawmidi(struct snd_usb_midi *umidi , int out_ports ,
                                      int in_ports )
{
  struct snd_rawmidi *rmidi ;
  int err ;
  int tmp ;

  {

  rmidi = (struct snd_rawmidi *)((void *)0);

  tmp = (umidi->chip)->next_midi_device;

  ((umidi->chip)->next_midi_device) ++;

  err = snd_rawmidi_new((umidi->chip)->card, (char * __attribute__((__nullterm__)) )"USB MIDI",
                        tmp, out_ports, in_ports, & rmidi);

  if (err < 0) {

    return (err);
  }

  strcpy(rmidi->name, (char const *)(((umidi->chip)->card)->shortname));

  rmidi->info_flags = 7U;

  rmidi->ops = & snd_usbmidi_ops;

  rmidi->private_data = (void * __attribute__((__recursive__, __noderef__)) )umidi;

  rmidi->private_free = & snd_usbmidi_rawmidi_free;

  snd_rawmidi_set_ops(rmidi, 0, & snd_usbmidi_output_ops);

  snd_rawmidi_set_ops(rmidi, 1, & snd_usbmidi_input_ops);

  umidi->rmidi = (struct snd_rawmidi * __attribute__((__recursive__)) )rmidi;

  return (0);
}
}

void snd_usbmidi_input_stop(struct list_head *p )
{
  struct snd_usb_midi *umidi ;
  int i ;
  struct list_head const __attribute__((__recursive__)) *__mptr ;
  struct snd_usb_midi_endpoint *ep ;

  {

  __mptr = (struct list_head const __attribute__((__recursive__)) *)p;

  umidi = (struct snd_usb_midi *)((char *)__mptr - (unsigned int )(& ((struct snd_usb_midi *)0)->list));

  i = 0;

  while (i < 2) {

    ep = & umidi->endpoints[i];

    if (ep->in) {

      usb_kill_urb((struct urb *)(ep->in)->urb);
    }

    i ++;
  }

  return;
}
}

static void snd_usbmidi_input_start_ep(struct snd_usb_midi_in_endpoint *ep )
{
  struct urb *urb ;

  {

  if (ep) {

    urb = usb_alloc_urb(0, 208U);

    ep->urb = (struct urb * __attribute__((__recursive__)) )urb;

    urb->dev = (struct usb_device * __attribute__((__recursive__)) )((ep->umidi)->chip)->dev;

    snd_usbmidi_submit_urb(urb, 208U);
  }

  return;
}
}

void snd_usbmidi_input_start(struct list_head *p )
{
  struct snd_usb_midi *umidi ;
  int i ;
  struct list_head const __attribute__((__recursive__)) *__mptr ;

  {

  __mptr = (struct list_head const __attribute__((__recursive__)) *)p;

  umidi = (struct snd_usb_midi *)((char *)__mptr - (unsigned int )(& ((struct snd_usb_midi *)0)->list));

  i = 0;

  while (i < 2) {

    snd_usbmidi_input_start_ep(umidi->endpoints[i].in);

    i ++;
  }

  return;
}
}

static struct lock_class_key __key___6 ;

int snd_usb_create_midi_interface(struct snd_usb_audio *chip , struct usb_interface *iface ,
                                  struct snd_usb_audio_quirk const *quirk )
{
  struct snd_usb_midi *umidi ;
  struct snd_usb_midi_endpoint_info endpoints[2] ;
  int out_ports ;
  int in_ports ;
  int i ;
  int err ;
  void *tmp ;
  int tmp___0 ;
  size_t __len ;
  void *__ret ;
  size_t __len___0 ;
  void *__ret___0 ;
  size_t __len___1 ;
  void *__ret___1 ;
  unsigned int tmp___1 ;
  unsigned int tmp___2 ;

  {

  tmp = kzalloc(sizeof(*umidi), 208U);

  umidi = (struct snd_usb_midi *)tmp;

  if (! umidi) {

    return (-12);
  }

  umidi->chip = chip;

  umidi->iface = iface;

  umidi->quirk = quirk;

  umidi->usb_protocol_ops = & snd_usbmidi_standard_ops;

  init_timer(& umidi->error_timer);

  while (1) {

    __spin_lock_init(& umidi->disc_lock, "&umidi->disc_lock", & __key___6);

    break;
  }

  umidi->error_timer.function = (void (*)(unsigned long ))(& snd_usbmidi_error_timer);

  umidi->error_timer.data = (void *)((unsigned long )umidi);

  memset((void *)(endpoints), 0, sizeof(endpoints));

  if (quirk) {

    tmp___0 = (int const )quirk->type;
  } else {

    tmp___0 = (int const )2;
  }

  switch ((int )tmp___0) {
  case 2:

  err = snd_usbmidi_get_ms_info(umidi, endpoints);

  if (chip->usb_id == (u32 )((1891 << 16) | 336)) {

    umidi->usb_protocol_ops = & snd_usbmidi_maudio_broken_running_status_ops;
  }

  break;
  case 10:

  umidi->usb_protocol_ops = & snd_usbmidi_122l_ops;
  case 3:

  __len = sizeof(struct snd_usb_midi_endpoint_info );

  if (__len >= 64UL) {

    __ret = __memcpy((void *)(& endpoints[0]), (void const *)quirk->data, __len);
  } else {

    __ret = __builtin_memcpy((void *)(& endpoints[0]), (void const *)quirk->data,
                             __len);
  }

  err = snd_usbmidi_detect_endpoints(umidi, & endpoints[0], 1);

  break;
  case 4:

  err = snd_usbmidi_detect_yamaha(umidi, & endpoints[0]);

  break;
  case 5:

  umidi->usb_protocol_ops = & snd_usbmidi_midiman_ops;

  __len___0 = sizeof(struct snd_usb_midi_endpoint_info );

  if (__len___0 >= 64UL) {

    __ret___0 = __memcpy((void *)(& endpoints[0]), (void const *)quirk->data, __len___0);
  } else {

    __ret___0 = __builtin_memcpy((void *)(& endpoints[0]), (void const *)quirk->data,
                                 __len___0);
  }

  err = 0;

  break;
  case 6:

  umidi->usb_protocol_ops = & snd_usbmidi_novation_ops;

  err = snd_usbmidi_detect_per_port_endpoints(umidi, endpoints);

  break;
  case 7:

  umidi->usb_protocol_ops = & snd_usbmidi_raw_ops;

  err = snd_usbmidi_detect_per_port_endpoints(umidi, endpoints);

  break;
  case 8:

  umidi->usb_protocol_ops = & snd_usbmidi_emagic_ops;

  __len___1 = sizeof(struct snd_usb_midi_endpoint_info );

  if (__len___1 >= 64UL) {

    __ret___1 = __memcpy((void *)(& endpoints[0]), (void const *)quirk->data, __len___1);
  } else {

    __ret___1 = __builtin_memcpy((void *)(& endpoints[0]), (void const *)quirk->data,
                                 __len___1);
  }

  err = snd_usbmidi_detect_endpoints(umidi, & endpoints[0], 1);

  break;
  case 9:

  umidi->usb_protocol_ops = & snd_usbmidi_cme_ops;

  err = snd_usbmidi_detect_per_port_endpoints(umidi, endpoints);

  break;
  default:

  while (1) {

    break;
  }

  err = -6;

  break;
  }

  if (err < 0) {

    kfree((void const *)umidi);

    return (err);
  }

  out_ports = 0;

  in_ports = 0;

  i = 0;

  while (i < 2) {

    tmp___1 = snd_usbmidi_count_bits((unsigned int )endpoints[i].out_cables);

    out_ports = (int )((unsigned int )out_ports + tmp___1);

    tmp___2 = snd_usbmidi_count_bits((unsigned int )endpoints[i].in_cables);

    in_ports = (int )((unsigned int )in_ports + tmp___2);

    i ++;
  }

  err = snd_usbmidi_create_rawmidi(umidi, out_ports, in_ports);

  if (err < 0) {

    kfree((void const *)umidi);

    return (err);
  }

  if (quirk) {

    if ((int const )quirk->type == 5) {

      err = snd_usbmidi_create_endpoints_midiman(umidi, & endpoints[0]);
    } else {

      err = snd_usbmidi_create_endpoints(umidi, endpoints);
    }
  } else {

    err = snd_usbmidi_create_endpoints(umidi, endpoints);
  }

  if (err < 0) {

    snd_usbmidi_free(umidi);

    return (err);
  }

  list_add((struct list_head *)(& umidi->list), & (umidi->chip)->midi_list);

  i = 0;

  while (i < 2) {

    snd_usbmidi_input_start_ep(umidi->endpoints[i].in);

    i ++;
  }

  return (0);
}
}

static char const __kstrtab_snd_usb_create_midi_interface[30] __attribute__((__section__("__ksymtab_strings"),
__aligned__(1))) =

  { (char const )'s', (char const )'n', (char const )'d', (char const )'_',
        (char const )'u', (char const )'s', (char const )'b', (char const )'_',
        (char const )'c', (char const )'r', (char const )'e', (char const )'a',
        (char const )'t', (char const )'e', (char const )'_', (char const )'m',
        (char const )'i', (char const )'d', (char const )'i', (char const )'_',
        (char const )'i', (char const )'n', (char const )'t', (char const )'e',
        (char const )'r', (char const )'f', (char const )'a', (char const )'c',
        (char const )'e', (char const )'\000'};

static struct kernel_symbol const __ksymtab_snd_usb_create_midi_interface __attribute__((__used__,
__unused__, __section__("__ksymtab"))) = {(unsigned long )(& snd_usb_create_midi_interface), __kstrtab_snd_usb_create_midi_interface};

static char const __kstrtab_snd_usbmidi_input_stop[23] __attribute__((__section__("__ksymtab_strings"),
__aligned__(1))) =

  { (char const )'s', (char const )'n', (char const )'d', (char const )'_',
        (char const )'u', (char const )'s', (char const )'b', (char const )'m',
        (char const )'i', (char const )'d', (char const )'i', (char const )'_',
        (char const )'i', (char const )'n', (char const )'p', (char const )'u',
        (char const )'t', (char const )'_', (char const )'s', (char const )'t',
        (char const )'o', (char const )'p', (char const )'\000'};

static struct kernel_symbol const __ksymtab_snd_usbmidi_input_stop __attribute__((__used__,
__unused__, __section__("__ksymtab"))) = {(unsigned long )(& snd_usbmidi_input_stop), __kstrtab_snd_usbmidi_input_stop};

static char const __kstrtab_snd_usbmidi_input_start[24] __attribute__((__section__("__ksymtab_strings"),
__aligned__(1))) =

  { (char const )'s', (char const )'n', (char const )'d', (char const )'_',
        (char const )'u', (char const )'s', (char const )'b', (char const )'m',
        (char const )'i', (char const )'d', (char const )'i', (char const )'_',
        (char const )'i', (char const )'n', (char const )'p', (char const )'u',
        (char const )'t', (char const )'_', (char const )'s', (char const )'t',
        (char const )'a', (char const )'r', (char const )'t', (char const )'\000'};

static struct kernel_symbol const __ksymtab_snd_usbmidi_input_start __attribute__((__used__,
__unused__, __section__("__ksymtab"))) = {(unsigned long )(& snd_usbmidi_input_start), __kstrtab_snd_usbmidi_input_start};

static char const __kstrtab_snd_usbmidi_disconnect[23] __attribute__((__section__("__ksymtab_strings"),
__aligned__(1))) =

  { (char const )'s', (char const )'n', (char const )'d', (char const )'_',
        (char const )'u', (char const )'s', (char const )'b', (char const )'m',
        (char const )'i', (char const )'d', (char const )'i', (char const )'_',
        (char const )'d', (char const )'i', (char const )'s', (char const )'c',
        (char const )'o', (char const )'n', (char const )'n', (char const )'e',
        (char const )'c', (char const )'t', (char const )'\000'};

static struct kernel_symbol const __ksymtab_snd_usbmidi_disconnect __attribute__((__used__,
__unused__, __section__("__ksymtab"))) = {(unsigned long )(& snd_usbmidi_disconnect), __kstrtab_snd_usbmidi_disconnect};

#pragma merger(0,"./usbmixer.i","-Wall,-Wundef,-Wstrict-prototypes,-Wno-trigraphs,-fno-strict-aliasing,-fno-common,-Werror-implicit-function-declaration,-Os,-m64,-mtune=generic,-mno-red-zone,-mcmodel=kernel,-funit-at-a-time,-maccumulate-outgoing-args,-pipe,-Wno-sign-compare,-fno-asynchronous-unwind-tables,-mno-sse,-mno-mmx,-mno-sse2,-mno-3dnow,-Wframe-larger-than=2048,-fno-stack-protector,-fno-omit-frame-pointer,-fno-optimize-sibling-calls,-g,-Wdeclaration-after-statement,-Wno-pointer-sign,-fwrapv,-fno-dwarf2-cfi-asm,-g,-Wall,-Wno-attributes,-Wno-unknown-pragmas")

extern int _cond_resched(void) ;

extern void __might_sleep(char *file , int line ) ;

__inline static void ( __attribute__((__always_inline__)) might_fault)(void)
{


  {

  while (1) {

    __might_sleep((char *)"include/linux/kernel.h", 155);

    _cond_resched();

    break;
  }

  return;
}
}

extern __attribute__((__noreturn__)) void __bad_pda_field(void) ;

extern struct x8664_pda _proxy_pda ;

__inline static struct task_struct *( __attribute__((__always_inline__)) get_current)(void)
{
  struct task_struct *ret__ ;

  {

  switch ((int )sizeof(_proxy_pda.pcurrent)) {
  case 2:

  __asm__ ("mov"
            "w %%gs:%c1,%0": "=r" (ret__): "i" ((unsigned int )(& ((struct x8664_pda *)0)->pcurrent)),
            "m" (_proxy_pda.pcurrent));

  break;
  case 4:

  __asm__ ("mov"
            "l %%gs:%c1,%0": "=r" (ret__): "i" ((unsigned int )(& ((struct x8664_pda *)0)->pcurrent)),
            "m" (_proxy_pda.pcurrent));

  break;
  case 8:

  __asm__ ("mov"
            "q %%gs:%c1,%0": "=r" (ret__): "i" ((unsigned int )(& ((struct x8664_pda *)0)->pcurrent)),
            "m" (_proxy_pda.pcurrent));

  break;
  default:

  __bad_pda_field();
  }

  return (ret__);
}
}

__inline static unsigned long ( __attribute__((__always_inline__)) __xchg)(unsigned long x ,
                                                                           void volatile *ptr ,
                                                                           int size )
{


  {

  switch (size) {
  case 1:

  __asm__ volatile ("xchgb %b0,%1": "=q" (x): "m" (*((long volatile *)ptr)),
                       "0" (x): "memory");

  break;
  case 2:

  __asm__ volatile ("xchgw %w0,%1": "=r" (x): "m" (*((long volatile *)ptr)),
                       "0" (x): "memory");

  break;
  case 4:

  __asm__ volatile ("xchgl %k0,%1": "=r" (x): "m" (*((long volatile *)ptr)),
                       "0" (x): "memory");

  break;
  case 8:

  __asm__ volatile ("xchgq %0,%1": "=r" (x): "m" (*((long volatile *)ptr)), "0" (x): "memory");

  break;
  }

  return (x);
}
}

extern unsigned long strlen(char const *s ) ;

extern int strcmp(char const *cs , char const *ct ) ;

__inline static int ( __attribute__((__always_inline__)) test_ti_thread_flag)(struct thread_info *ti ,
                                                                              int flag )
{
  int tmp___0 ;

  {

  tmp___0 = variable_test_bit(flag, (unsigned long const volatile *)((unsigned long *)(& ti->flags)));

  return (tmp___0);
}
}

extern void init_waitqueue_head(wait_queue_head_t *q ) ;

extern void prepare_to_wait(wait_queue_head_t *q , wait_queue_t *wait , int state ) ;

extern void finish_wait(wait_queue_head_t *q , wait_queue_t *wait ) ;

extern int autoremove_wake_function(wait_queue_t *wait , unsigned int mode , int sync ,
                                    void *key ) ;

extern void schedule(void) ;

__inline static int ( __attribute__((__always_inline__)) test_tsk_thread_flag)(struct task_struct *tsk ,
                                                                               int flag )
{
  int tmp ;

  {

  tmp = test_ti_thread_flag((struct thread_info *)tsk->stack, flag);

  return (tmp);
}
}

__inline static int ( __attribute__((__always_inline__)) signal_pending)(struct task_struct *p )
{
  int tmp ;
  int tmp___0 ;
  long tmp___1 ;

  {

  tmp = test_tsk_thread_flag(p, 2);

  if (tmp) {

    tmp___0 = 1;
  } else {

    tmp___0 = 0;
  }

  tmp___1 = __builtin_expect((long )tmp___0, 0L);

  return ((int )tmp___1);
}
}

__inline static void ( __attribute__((__always_inline__)) usb_fill_control_urb)(struct urb *urb ,
                                                                                struct usb_device *dev ,
                                                                                unsigned int pipe ,
                                                                                unsigned char *setup_packet ,
                                                                                void *transfer_buffer ,
                                                                                int buffer_length ,
                                                                                void (*complete_fn)(struct urb * ) ,
                                                                                void *context )
{


  {

  urb->dev = (struct usb_device * __attribute__((__recursive__)) )dev;

  urb->pipe = pipe;

  urb->setup_packet = setup_packet;

  urb->transfer_buffer = (u8 *)transfer_buffer;

  urb->transfer_buffer_length = buffer_length;

  urb->complete = complete_fn;

  urb->context = (u8 * __attribute__((__recursive__, __noderef__)) )context;

  return;
}
}

extern void snd_ctl_notify(struct snd_card *card , unsigned int mask , struct snd_ctl_elem_id *id ) ;

extern struct snd_kcontrol *snd_ctl_new1(struct snd_kcontrol_new const *kcontrolnew ,
                                         void *private_data ) ;

extern int snd_ctl_add(struct snd_card *card , struct snd_kcontrol *kcontrol ) ;

extern struct snd_kcontrol *snd_ctl_find_id(struct snd_card *card , struct snd_ctl_elem_id *id ) ;

extern int snd_ctl_boolean_mono_info(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_info *uinfo ) ;

extern unsigned long __attribute__((__warn_unused_result__)) copy_to_user(void *to ,
                                                                           void const *from ,
                                                                           unsigned int len ) ;

__inline static void ( __attribute__((__always_inline__)) poll_wait)(struct file *filp ,
                                                                     wait_queue_head_t *wait_address ,
                                                                     poll_table *p )
{


  {

  if (p) {

    if (wait_address) {

      (*(p->qproc))(filp, wait_address, p);
    }
  }

  return;
}
}

extern int snd_hwdep_new(struct snd_card *card , char * __attribute__((__nullterm__)) id ,
                         int device , struct snd_hwdep **rhwdep ) ;

static struct rc_config const rc_configs[3] = { {(u32 )((1054 << 16) | 12288), (u8 )0, (u8 )1, (u8 )2, (u8 )1, (u8 )18, (u32 )19},
        {(u32 )((1054 << 16) | 12320),
      (u8 )2, (u8 )1, (u8 )6, (u8 )6, (u8 )18, (u32 )19},
        {(u32 )((1054 << 16) | 12352), (u8 )2, (u8 )2, (u8 )6, (u8 )6, (u8 )2, (u32 )28305}};

static struct usbmix_name_map extigy_map[20] =

  { {2, "PCM Playback", 0},
        {5, (char const *)((void *)0), 0},
        {6, "Digital In", 0},
        {8, "Line Playback", 0},
        {10, "Mic Playback", 0},
        {11, "Capture Input Source", 0},
        {12, "Capture", 0},
        {17, (char const *)((void *)0), 1},
        {17, "Channel Routing", 2},
        {18, "Tone Control - Bass", 3},
        {18, "Tone Control - Treble", 5},
        {18, "Master Playback", 0},
        {21, (char const *)((void *)0), 0},
        {22, "Digital Out Playback", 0},
        {23, "Digital Out1 Playback", 0},
        {25, "IEC958 Optical Playback", 0},
        {26, "IEC958 Optical Playback", 0},
        {27, (char const *)((void *)0), 0},
        {29, (char const *)((void *)0), 0},
        {0, (char const *)0, 0}};

static struct usbmix_name_map mp3plus_map[7] = { {8, "Capture Input Source", 0},
        {9, "Master Playback", 0},
        {10, "Mic Boost", 7},
        {11, "Line Capture", 0},
        {12, "Digital In Playback", 0},
        {14, "Line Playback", 0},
        {0, (char const *)0, 0}};

static struct usbmix_name_map audigy2nx_map[17] =

  { {6, "Digital In Playback", 0},
        {8, "Line Playback", 0},
        {11, "What-U-Hear Capture", 0},
        {12, "Line Capture", 0},
        {13, "Digital In Capture", 0},
        {14, "Capture Source", 0},
        {17, (char const *)((void *)0), 0},
        {18, "Master Playback", 0},
        {21, (char const *)((void *)0), 0},
        {22, "Digital Out Playback", 0},
        {23, (char const *)((void *)0), 0},
        {27, (char const *)((void *)0), 0},
        {28, "Speaker Playback", 0},
        {29, "Digital Out Source", 0},
        {30, "Headphone Playback", 0},
        {31, "Headphone Source", 0},
        {0, (char const *)0, 0}};

static char const *__constr_expr_0___0[3] = { "Line", "Digital In", "What-U-Hear"};

static char const *__constr_expr_1___0[3] = { "Front", "PCM", "Digital In"};

static char const *__constr_expr_2___0[2] = { "Front", "Side"};

static struct usbmix_selector_map audigy2nx_selectors[4] = { {14, 3, __constr_expr_0___0},
        {29, 3, __constr_expr_1___0},
        {31, 2, __constr_expr_2___0},
        {0, 0, (char const **)0}};

static struct usbmix_name_map live24ext_map[2] = { {5, "Mic Capture", 0},
        {0, (char const *)0, 0}};

static struct usbmix_name_map linex_map[2] = { {3, "Master", 0},
        {0, (char const *)0, 0}};

static struct usbmix_name_map maya44_map[5] = { {2, "Line Playback", 0},
        {4, "Line Playback", 0},
        {7, "Master Playback", 0},
        {10, "Line Capture", 0},
        {0, (char const *)0, 0}};

static struct usbmix_name_map justlink_map[6] = { {3, (char const *)((void *)0), 0},
        {7, "Master Playback", 0},
        {8, (char const *)((void *)0), 0},
        {9, (char const *)((void *)0), 0},
        {12, (char const *)((void *)0), 0},
        {0, (char const *)0, 0}};

static struct usbmix_name_map aureon_51_2_map[8] =

  { {8, "Capture Source", 0},
        {9, "Master Playback", 0},
        {10, "Mic Capture", 0},
        {11, "Line Capture", 0},
        {12, "IEC958 In Capture", 0},
        {13, "Mic Playback", 0},
        {14, "Line Playback", 0},
        {0, (char const *)0, 0}};

static struct usbmix_ctl_map usbmix_ctl_maps[11] =

  { {(u32 )((1054 << 16) | 12288), (struct usbmix_name_map const *)(extigy_map),
      (struct usbmix_selector_map const *)0, 1},
        {(u32 )((1054 << 16) | 12304), (struct usbmix_name_map const *)(mp3plus_map),
      (struct usbmix_selector_map const *)0, 0},
        {(u32 )((1054 << 16) | 12320), (struct usbmix_name_map const *)(audigy2nx_map),
      (struct usbmix_selector_map const *)(audigy2nx_selectors), 0},
        {(u32 )((1054 << 16) | 12352), (struct usbmix_name_map const *)(live24ext_map),
      (struct usbmix_selector_map const *)0, 0},
        {(u32 )((1784 << 16) | 45056), (struct usbmix_name_map const *)0, (struct usbmix_selector_map const *)0,
      1},
        {(u32 )((1784 << 16) | 53250), (struct usbmix_name_map const *)0, (struct usbmix_selector_map const *)0,
      1},
        {(u32 )((2235 << 16) | 9986), (struct usbmix_name_map const *)(linex_map),
      (struct usbmix_selector_map const *)0, 1},
        {(u32 )((2706 << 16) | 145), (struct usbmix_name_map const *)(maya44_map),
      (struct usbmix_selector_map const *)0, 0},
        {(u32 )((3141 << 16) | 4440), (struct usbmix_name_map const *)(justlink_map),
      (struct usbmix_selector_map const *)0, 0},
        {(u32 )((3277 << 16) | 40), (struct usbmix_name_map const *)(aureon_51_2_map),
      (struct usbmix_selector_map const *)0, 0},
        {(u32 )0, (struct usbmix_name_map const *)0, (struct usbmix_selector_map const *)0,
      0}};

int Nonstub_return_buflen(int buflen )
{


  {

  return (buflen);
}
}

static int check_mapped_name(struct mixer_build *state , int unitid , int control ,
                             char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) buf ,
                             int buflen )
{
  struct usbmix_name_map const *p ;
  size_t tmp ;

  {

  if (! state->map) {

    return (0);
  }

  p = state->map;

  while (p->id) {

    if (p->id == (int const )unitid) {

      if (p->name) {

        if (! control) {

          buflen --;

          tmp = strlcpy((char *)buf, (char const *)p->name, (size_t )buflen);

          return ((int )tmp);
        } else

        if (! p->control) {

          buflen --;

          tmp = strlcpy((char *)buf, (char const *)p->name, (size_t )buflen);

          return ((int )tmp);
        } else

        if (control == (int )p->control) {

          buflen --;

          tmp = strlcpy((char *)buf, (char const *)p->name, (size_t )buflen);

          return ((int )tmp);
        }
      }
    }

    p ++;
  }

  return (0);
}
}

static int check_ignored_ctl(struct mixer_build *state , int unitid , int control )
{
  struct usbmix_name_map const *p ;

  {

  if (! state->map) {

    return (0);
  }

  p = state->map;

  while (p->id) {

    if (p->id == (int const )unitid) {

      if (! p->name) {

        if (! control) {

          return (1);
        } else

        if (! p->control) {

          return (1);
        } else

        if (control == (int )p->control) {

          return (1);
        }
      }
    }

    p ++;
  }

  return (0);
}
}

static int check_mapped_selector_name(struct mixer_build *state , int unitid , int index___0 ,
                                      char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) buf ,
                                      int buflen )
{
  struct usbmix_selector_map const *p ;
  size_t tmp ;

  {

  if (! state->selector_map) {

    return (0);
  }

  p = state->selector_map;

  while (p->id) {

    if (p->id == (int const )unitid) {

      if (index___0 < (int )p->count) {

        tmp = strlcpy((char *)buf, *(p->names + index___0), (size_t )buflen);

        return ((int )tmp);
      }
    }

    p ++;
  }

  return (0);
}
}

static void *find_audio_control_unit(struct mixer_build *state , unsigned char unit )
{
  unsigned char *p ;
  void *tmp ;

  {

  p = (unsigned char *)((void *)0);

  while (1) {

    tmp = snd_usb_find_desc((void *)state->buffer, (int )state->buflen, (void *)p,
                            (u8 )((1 << 5) | 4));

    p = (unsigned char *)tmp;

    if (! ((unsigned long )p != (unsigned long )((void *)0))) {

      break;
    }

    if ((int )*(p + 0) >= 4) {

      if ((int )*(p + 2) >= 2) {

        if ((int )*(p + 2) <= 8) {

          if ((int )*(p + 3) == (int )unit) {

            return ((void *)p);
          }
        }
      }
    }
  }

  return ((void *)0);
}
}

static int snd_usb_copy_string_desc(struct mixer_build *state , int index___0 , char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) buf ,
                                    int maxlen )
{
  int len ;
  int tmp ;

  {

  tmp = usb_string((state->chip)->dev, index___0, (char * __attribute__((__exp__(Nonstub_get_size("size")))) )buf,
                   (size_t )(maxlen - 1));

  len = tmp;

  *(buf + len) = (char)0;

  return (len);
}
}

static int convert_signed_value(struct usb_mixer_elem_info *cval , int val )
{


  {

  switch (cval->val_type) {
  case 0:

  return (! (! val));
  case 1:

  return (! val);
  case 3:

  val &= 255;

  break;
  case 2:

  val &= 255;

  if (val >= 128) {

    val -= 256;
  }

  break;
  case 5:

  val &= 65535;

  break;
  case 4:

  val &= 65535;

  if (val >= 32768) {

    val -= 65536;
  }

  break;
  }

  return (val);
}
}

static int convert_bytes_value(struct usb_mixer_elem_info *cval , int val )
{


  {

  switch (cval->val_type) {
  case 0:

  return (! (! val));
  case 1:

  return (! val);
  case 2:
  case 3:

  return (val & 255);
  case 4:
  case 5:

  return (val & 65535);
  }

  return (0);
}
}

static int get_relative_value(struct usb_mixer_elem_info *cval , int val )
{


  {

  if (! cval->res) {

    cval->res = 1;
  }

  if (val < cval->min) {

    return (0);
  } else

  if (val >= cval->max) {

    return ((((cval->max - cval->min) + cval->res) - 1) / cval->res);
  } else {

    return ((val - cval->min) / cval->res);
  }
}
}

static int get_abs_value(struct usb_mixer_elem_info *cval , int val )
{


  {

  if (val < 0) {

    return (cval->min);
  }

  if (! cval->res) {

    cval->res = 1;
  }

  val *= cval->res;

  val += cval->min;

  if (val > cval->max) {

    return (cval->max);
  }

  return (val);
}
}

static int get_ctl_value(struct usb_mixer_elem_info *cval , int request , int validx ,
                         int *value_ret )
{
  unsigned char buf[2] ;
  int val_len ;
  int tmp ;
  int timeout ;
  unsigned int tmp___0 ;
  unsigned int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;

  {

  if (cval->val_type >= 4) {

    tmp = 2;
  } else {

    tmp = 1;
  }

  val_len = tmp;

  timeout = 10;

  while (1) {

    tmp___3 = timeout;

    timeout --;

    if (! (tmp___3 > 0)) {

      break;
    }

    tmp___1 = __create_pipe(((cval->mixer)->chip)->dev, 0U);

    tmp___2 = snd_usb_ctl_msg(((cval->mixer)->chip)->dev, ((unsigned int )(2 << 30) | tmp___1) | 128U,
                              (__u8 )request, (__u8 )((1 | (1 << 5)) | 128), (__u16 )validx,
                              (__u16 )((cval->mixer)->ctrlif | (cval->id << 8)), (void *)(buf),
                              (__u16 )val_len, 100);

    if (tmp___2 >= val_len) {

      tmp___0 = snd_usb_combine_bytes(buf, val_len);

      *value_ret = convert_signed_value(cval, (int )tmp___0);

      return (0);
    }
  }

  return (-22);
}
}

static int get_cur_ctl_value(struct usb_mixer_elem_info *cval , int validx , int *value )
{
  int tmp ;

  {

  tmp = get_ctl_value(cval, 129, validx, value);

  return (tmp);
}
}

__inline static int ( __attribute__((__always_inline__)) get_cur_mix_value)(struct usb_mixer_elem_info *cval ,
                                                                            int channel ,
                                                                            int *value )
{
  int tmp ;

  {

  tmp = get_ctl_value(cval, 129, (int )((cval->control << 8) | (unsigned int )channel),
                      value);

  return (tmp);
}
}

static int set_ctl_value(struct usb_mixer_elem_info *cval , int request , int validx ,
                         int value_set )
{
  unsigned char buf[2] ;
  int val_len ;
  int tmp ;
  int timeout ;
  unsigned int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;

  {

  if (cval->val_type >= 4) {

    tmp = 2;
  } else {

    tmp = 1;
  }

  val_len = tmp;

  timeout = 10;

  value_set = convert_bytes_value(cval, value_set);

  buf[0] = (unsigned char )(value_set & 255);

  buf[1] = (unsigned char )((value_set >> 8) & 255);

  while (1) {

    tmp___2 = timeout;

    timeout --;

    if (! (tmp___2 > 0)) {

      break;
    }

    tmp___0 = __create_pipe(((cval->mixer)->chip)->dev, 0U);

    tmp___1 = snd_usb_ctl_msg(((cval->mixer)->chip)->dev, (unsigned int )(2 << 30) | tmp___0,
                              (__u8 )request, (__u8 )(1 | (1 << 5)), (__u16 )validx,
                              (__u16 )((cval->mixer)->ctrlif | (cval->id << 8)), (void *)(buf),
                              (__u16 )val_len, 100);

    if (tmp___1 >= 0) {

      return (0);
    }
  }

  return (-22);
}
}

static int set_cur_ctl_value(struct usb_mixer_elem_info *cval , int validx , int value )
{
  int tmp ;

  {

  tmp = set_ctl_value(cval, 1, validx, value);

  return (tmp);
}
}

__inline static int ( __attribute__((__always_inline__)) set_cur_mix_value)(struct usb_mixer_elem_info *cval ,
                                                                            int channel ,
                                                                            int value )
{
  int tmp ;

  {

  tmp = set_ctl_value(cval, 1, (int )((cval->control << 8) | (unsigned int )channel),
                      value);

  return (tmp);
}
}

static int mixer_vol_tlv(struct snd_kcontrol *kcontrol , int op_flag , unsigned int size ,
                         unsigned int *_tlv )
{
  struct usb_mixer_elem_info *cval ;
  unsigned int scale[4] ;
  int tmp ;
  int tmp___0 ;
  unsigned long tmp___1 ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  scale[0] = 1U;

  scale[1] = (unsigned int )(2UL * sizeof(unsigned int ));

  scale[2] = 0U;

  scale[3] = 0U;

  if ((unsigned long )size < sizeof(scale)) {

    return (-12);
  }

  tmp = convert_signed_value(cval, cval->min);

  scale[2] = (unsigned int )((tmp * 100) / 256);

  tmp___0 = convert_signed_value(cval, cval->res);

  scale[3] = (unsigned int )((tmp___0 * 100) / 256);

  tmp___1 = (unsigned long )copy_to_user((void *)_tlv, (void const *)(scale), (unsigned int )sizeof(scale));

  if (tmp___1) {

    return (-14);
  }

  return (0);
}
}

static int parse_audio_unit(struct mixer_build *state , int unitid ) ;

static int check_matrix_bitmap(unsigned char *bmap___0 , int ich , int och , int num_outs )
{
  int idx ;

  {

  idx = ich * num_outs + och;

  return ((int )*(bmap___0 + (idx >> 3)) & (128 >> (idx & 7)));
}
}

static int add_control_to_empty(struct mixer_build *state , struct snd_kcontrol *kctl )
{
  struct usb_mixer_elem_info *cval ;
  int err ;
  struct snd_kcontrol *tmp ;

  {

  cval = (struct usb_mixer_elem_info *)kctl->private_data;

  while (1) {

    tmp = snd_ctl_find_id((state->chip)->card, & kctl->id);

    if (! tmp) {

      break;
    }

    (kctl->id.index) ++;
  }

  err = snd_ctl_add((state->chip)->card, kctl);

  if (err < 0) {

    while (1) {

      break;
    }

    return (err);
  }

  cval->elem_id = & kctl->id;

  cval->next_id_elem = (struct usb_mixer_elem_info * __attribute__((__recursive__)) )*((state->mixer)->id_elems + cval->id);

  *((state->mixer)->id_elems + cval->id) = cval;

  return (0);
}
}

static struct iterm_name_combo iterm_names[37] =

  { {768, (char * __attribute__((__nullterm__)) )"Output"},
        {769, (char * __attribute__((__nullterm__)) )"Speaker"},
        {770, (char * __attribute__((__nullterm__)) )"Headphone"},
        {771, (char * __attribute__((__nullterm__)) )"HMD Audio"},
        {772, (char * __attribute__((__nullterm__)) )"Desktop Speaker"},
        {773, (char * __attribute__((__nullterm__)) )"Room Speaker"},
        {774, (char * __attribute__((__nullterm__)) )"Com Speaker"},
        {775, (char * __attribute__((__nullterm__)) )"LFE"},
        {1536, (char * __attribute__((__nullterm__)) )"External In"},
        {1537, (char * __attribute__((__nullterm__)) )"Analog In"},
        {1538, (char * __attribute__((__nullterm__)) )"Digital In"},
        {1539, (char * __attribute__((__nullterm__)) )"Line"},
        {1540, (char * __attribute__((__nullterm__)) )"Legacy In"},
        {1541, (char * __attribute__((__nullterm__)) )"IEC958 In"},
        {1542, (char * __attribute__((__nullterm__)) )"1394 DA Stream"},
        {1543, (char * __attribute__((__nullterm__)) )"1394 DV Stream"},
        {1792, (char * __attribute__((__nullterm__)) )"Embedded"},
        {1793, (char * __attribute__((__nullterm__)) )"Noise Source"},
        {1794, (char * __attribute__((__nullterm__)) )"Equalization Noise"},
        {1795, (char * __attribute__((__nullterm__)) )"CD"},
        {1796, (char * __attribute__((__nullterm__)) )"DAT"},
        {1797, (char * __attribute__((__nullterm__)) )"DCC"},
        {1798, (char * __attribute__((__nullterm__)) )"MiniDisk"},
        {1799, (char * __attribute__((__nullterm__)) )"Analog Tape"},
        {1800, (char * __attribute__((__nullterm__)) )"Phonograph"},
        {1801, (char * __attribute__((__nullterm__)) )"VCR Audio"},
        {1802, (char * __attribute__((__nullterm__)) )"Video Disk Audio"},
        {1803, (char * __attribute__((__nullterm__)) )"DVD Audio"},
        {1804, (char * __attribute__((__nullterm__)) )"TV Tuner Audio"},
        {1805, (char * __attribute__((__nullterm__)) )"Satellite Rec Audio"},
        {1806, (char * __attribute__((__nullterm__)) )"Cable Tuner Audio"},
        {1807, (char * __attribute__((__nullterm__)) )"DSS Audio"},
        {1808, (char * __attribute__((__nullterm__)) )"Radio Receiver"},
        {1809, (char * __attribute__((__nullterm__)) )"Radio Transmitter"},
        {1810, (char * __attribute__((__nullterm__)) )"Multi-Track Recorder"},
        {1811, (char * __attribute__((__nullterm__)) )"Synthesizer"},
        {0, (char * __attribute__((__nullterm__)) )0}};

static int get_term_name(struct mixer_build *state , struct usb_audio_term *iterm ,
                         unsigned char *name , int maxlen , int term_only )
{
  struct iterm_name_combo *names ;
  int tmp ;
  int tmp___0 ;
  unsigned long tmp___1 ;

  {

  if (iterm->name) {

    tmp = snd_usb_copy_string_desc(state, iterm->name, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )name,
                                   maxlen);

    return (tmp);
  }

  if (iterm->type >> 16) {

    if (term_only) {

      return (0);
    }

    switch (iterm->type >> 16) {
    case 5:

    strcpy((char *)name, "Selector");

    return (8);
    case 7:

    strcpy((char *)name, "Process Unit");

    return (12);
    case 8:

    strcpy((char *)name, "Ext Unit");

    return (8);
    case 4:

    strcpy((char *)name, "Mixer");

    return (5);
    default:

    tmp___0 = sprintf((char *)name, "Unit %d", iterm->id);

    return (tmp___0);
    }
  }

  switch (iterm->type & 65280) {
  case 256:

  strcpy((char *)name, "PCM");

  return (3);
  case 512:

  strcpy((char *)name, "Mic");

  return (3);
  case 1024:

  strcpy((char *)name, "Headset");

  return (7);
  case 1280:

  strcpy((char *)name, "Phone");

  return (5);
  }

  names = iterm_names;

  while (names->type) {

    if (names->type == iterm->type) {

      strcpy((char *)name, (char const *)names->name);

      tmp___1 = strlen((char const *)names->name);

      return ((int )tmp___1);
    }

    names ++;
  }

  return (0);
}
}

static int check_input_term(struct mixer_build *state , int id___0 , struct usb_audio_term *term )
{
  unsigned char *p1 ;
  int tmp ;
  void *tmp___0 ;

  {

  memset((void *)term, 0, sizeof(*term));

  while (1) {

    tmp___0 = find_audio_control_unit(state, (unsigned char )id___0);

    p1 = (unsigned char *)tmp___0;

    if (! ((unsigned long )p1 != (unsigned long )((void *)0))) {

      break;
    }

    term->id = id___0;

    switch ((int )*(p1 + 2)) {
    case 2:

    term->type = (int )((unsigned int )((int )*p1 + 4) | ((unsigned int )*((p1 + 4) + 1) << 8));

    term->channels = (int )*(p1 + 7);

    term->chconfig = (unsigned int )((int )*p1 + 8) | ((unsigned int )*((p1 + 8) + 1) << 8);

    term->name = (int )*(p1 + 11);

    return (0);
    case 6:

    id___0 = (int )*(p1 + 4);

    break;
    case 4:

    term->type = (int )*(p1 + 2) << 16;

    term->channels = (int )*(p1 + (5 + (int )*(p1 + 4)));

    term->chconfig = (unsigned int )(((int )*p1 + 6) + (int )*(p1 + 4)) | ((unsigned int )*(((p1 + 6) + (int )*(p1 + 4)) + 1) << 8);

    term->name = (int )*(p1 + ((int )*(p1 + 0) - 1));

    return (0);
    case 5:

    tmp = check_input_term(state, (int )*(p1 + 5), term);

    if (tmp < 0) {

      return (-19);
    }

    term->type = (int )*(p1 + 2) << 16;

    term->id = id___0;

    term->name = (int )*(p1 + ((9 + (int )*(p1 + 0)) - 1));

    return (0);
    case 7:
    case 8:

    if ((int )*(p1 + 6) == 1) {

      id___0 = (int )*(p1 + 7);

      break;
    }

    term->type = (int )*(p1 + 2) << 16;

    term->channels = (int )*(p1 + (7 + (int )*(p1 + 6)));

    term->chconfig = (unsigned int )(((int )*p1 + 8) + (int )*(p1 + 6)) | ((unsigned int )*(((p1 + 8) + (int )*(p1 + 6)) + 1) << 8);

    term->name = (int )*(p1 + ((12 + (int )*(p1 + 6)) + (int )*(p1 + (11 + (int )*(p1 + 6)))));

    return (0);
    default:

    return (-19);
    }
  }

  return (-19);
}
}

static struct usb_feature_control_info ( __attribute__((__noderef__, __address_space__(2))) audio_feature_info)[10] =

  { {(char const * __attribute__((__nullterm__)) )"Mute", 1U},
        {(char const * __attribute__((__nullterm__)) )"Volume", 4U},
        {(char const * __attribute__((__nullterm__)) )"Tone Control - Bass", 2U},
        {(char const * __attribute__((__nullterm__)) )"Tone Control - Mid", 2U},
        {(char const * __attribute__((__nullterm__)) )"Tone Control - Treble", 2U},
        {(char const * __attribute__((__nullterm__)) )"Graphic Equalizer",
      2U},
        {(char const * __attribute__((__nullterm__)) )"Auto Gain Control", 0U},
        {(char const * __attribute__((__nullterm__)) )"Delay Control", 5U},
        {(char const * __attribute__((__nullterm__)) )"Bass Boost", 0U},
        {(char const * __attribute__((__nullterm__)) )"Loudness", 0U}};

static void usb_mixer_elem_free(struct snd_kcontrol *kctl )
{


  {

  kfree((void const *)kctl->private_data);

  kctl->private_data = (char *)((void *)0);

  return;
}
}

static int get_min_max(struct usb_mixer_elem_info *cval , int default_min )
{
  int minchn ;
  int i ;
  int tmp ;
  int tmp___0 ;
  int last_valid_res ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int last_valid_res___0 ;
  int saved ;
  int test ;
  int check ;
  int tmp___4 ;
  int tmp___5 ;

  {

  cval->min = default_min;

  cval->max = cval->min + 1;

  cval->res = 1;

  if (cval->val_type == 0) {

    cval->initialized = (u8 )1;
  } else

  if (cval->val_type == 1) {

    cval->initialized = (u8 )1;
  } else {

    minchn = 0;

    if (cval->cmask) {

      i = 0;

      while (i < 10) {

        if (cval->cmask & (unsigned int )(1 << i)) {

          minchn = i + 1;

          break;
        }

        i ++;
      }
    }

    tmp = get_ctl_value(cval, 131, (int )((cval->control << 8) | (unsigned int )minchn),
                        & cval->max);

    if (tmp < 0) {

      goto _L;
    } else {

      tmp___0 = get_ctl_value(cval, 130, (int )((cval->control << 8) | (unsigned int )minchn),
                              & cval->min);

      if (tmp___0 < 0) {
        _L:

        while (1) {

          break;
        }

        return (-22);
      }
    }

    tmp___3 = get_ctl_value(cval, 132, (int )((cval->control << 8) | (unsigned int )minchn),
                            & cval->res);

    if (tmp___3 < 0) {

      cval->res = 1;
    } else {

      last_valid_res = cval->res;

      while (cval->res > 1) {

        tmp___1 = set_ctl_value(cval, 4, (int )((cval->control << 8) | (unsigned int )minchn),
                                cval->res / 2);

        if (tmp___1 < 0) {

          break;
        }

        cval->res /= 2;
      }

      tmp___2 = get_ctl_value(cval, 132, (int )((cval->control << 8) | (unsigned int )minchn),
                              & cval->res);

      if (tmp___2 < 0) {

        cval->res = last_valid_res;
      }
    }

    if (cval->res == 0) {

      cval->res = 1;
    }

    if (cval->min + cval->res < cval->max) {

      last_valid_res___0 = cval->res;

      get_cur_mix_value(cval, minchn, & saved);

      while (1) {

        test = saved;

        if (test < cval->max) {

          test += cval->res;
        } else {

          test -= cval->res;
        }

        if (test < cval->min) {

          cval->res = last_valid_res___0;

          break;
        } else

        if (test > cval->max) {

          cval->res = last_valid_res___0;

          break;
        } else {

          tmp___4 = set_cur_mix_value(cval, minchn, test);

          if (tmp___4) {

            cval->res = last_valid_res___0;

            break;
          } else {

            tmp___5 = get_cur_mix_value(cval, minchn, & check);

            if (tmp___5) {

              cval->res = last_valid_res___0;

              break;
            }
          }
        }

        if (test == check) {

          break;
        }

        cval->res *= 2;
      }

      set_cur_mix_value(cval, minchn, saved);
    }

    cval->initialized = (u8 )1;
  }

  return (0);
}
}

static int mixer_ctl_feature_info(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_info *uinfo )
{
  struct usb_mixer_elem_info *cval ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  if (cval->val_type == 0) {

    uinfo->type = 1;
  } else

  if (cval->val_type == 1) {

    uinfo->type = 1;
  } else {

    uinfo->type = 2;
  }

  uinfo->count = (unsigned int )cval->channels;

  if (cval->val_type == 0) {

    uinfo->value.integer.min = 0L;

    uinfo->value.integer.max = 1L;
  } else

  if (cval->val_type == 1) {

    uinfo->value.integer.min = 0L;

    uinfo->value.integer.max = 1L;
  } else {

    if (! cval->initialized) {

      get_min_max(cval, 0);
    }

    uinfo->value.integer.min = 0L;

    uinfo->value.integer.max = (long )((((cval->max - cval->min) + cval->res) - 1) / cval->res);
  }

  return (0);
}
}

static int mixer_ctl_feature_get(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_elem_info *cval ;
  int c ;
  int cnt ;
  int val ;
  int err ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  if (cval->cmask) {

    cnt = 0;

    c = 0;

    while (c < 10) {

      if (cval->cmask & (unsigned int )(1 << c)) {

        err = get_cur_mix_value(cval, c + 1, & val);

        if (err < 0) {

          if ((cval->mixer)->ignore_ctl_error) {

            ucontrol->value.integer.value[0] = (long )cval->min;

            return (0);
          }

          while (1) {

            break;
          }

          return (err);
        }

        val = get_relative_value(cval, val);

        ucontrol->value.integer.value[cnt] = (long )val;

        cnt ++;
      }

      c ++;
    }
  } else {

    err = get_cur_mix_value(cval, 0, & val);

    if (err < 0) {

      if ((cval->mixer)->ignore_ctl_error) {

        ucontrol->value.integer.value[0] = (long )cval->min;

        return (0);
      }

      while (1) {

        break;
      }

      return (err);
    }

    val = get_relative_value(cval, val);

    ucontrol->value.integer.value[0] = (long )val;
  }

  return (0);
}
}

static int mixer_ctl_feature_put(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_elem_info *cval ;
  int c ;
  int cnt ;
  int val ;
  int oval ;
  int err ;
  int changed ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  changed = 0;

  if (cval->cmask) {

    cnt = 0;

    c = 0;

    while (c < 10) {

      if (cval->cmask & (unsigned int )(1 << c)) {

        err = get_cur_mix_value(cval, c + 1, & oval);

        if (err < 0) {

          if ((cval->mixer)->ignore_ctl_error) {

            return (0);
          }

          return (err);
        }

        val = (int )ucontrol->value.integer.value[cnt];

        val = get_abs_value(cval, val);

        if (oval != val) {

          set_cur_mix_value(cval, c + 1, val);

          changed = 1;
        }

        get_cur_mix_value(cval, c + 1, & val);

        cnt ++;
      }

      c ++;
    }
  } else {

    err = get_cur_mix_value(cval, 0, & oval);

    if (err < 0) {

      if ((cval->mixer)->ignore_ctl_error) {

        return (0);
      }
    }

    if (err < 0) {

      return (err);
    }

    val = (int )ucontrol->value.integer.value[0];

    val = get_abs_value(cval, val);

    if (val != oval) {

      set_cur_mix_value(cval, 0, val);

      changed = 1;
    }
  }

  return (changed);
}
}

static struct snd_kcontrol_new __attribute__((__noderef__, __address_space__(2))) usb_feature_unit_ctl =

     {2, 0U, 0U, (unsigned char *)"", 0U, 0U, 0U, & mixer_ctl_feature_info, & mixer_ctl_feature_get,
    & mixer_ctl_feature_put, {(snd_kcontrol_tlv_rw_t *)0}, 0UL};

static void build_feature_ctl(struct mixer_build *state , unsigned char *desc , unsigned int ctl_mask ,
                              int control , struct usb_audio_term *iterm , int unitid )
{
  unsigned int len ;
  int mapped_name ;
  int nameid ;
  struct snd_kcontrol *kctl ;
  struct usb_mixer_elem_info *cval ;
  int tmp ;
  void *tmp___0 ;
  int i ;
  int c ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  size_t tmp___6 ;
  size_t tmp___7 ;
  char const *tmp___8 ;
  int tmp___9 ;

  {

  len = 0U;

  mapped_name = 0;

  nameid = (int )*(desc + ((int )*(desc + 0) - 1));

  control ++;

  if (control == 6) {

    return;
  }

  tmp = check_ignored_ctl(state, unitid, control);

  if (tmp) {

    return;
  }

  tmp___0 = kzalloc(sizeof(*cval), 208U);

  cval = (struct usb_mixer_elem_info *)tmp___0;

  if (! cval) {

    printk("<3>cannot malloc kcontrol\n");

    return;
  }

  cval->mixer = state->mixer;

  cval->id = (unsigned int )unitid;

  cval->control = (unsigned int )control;

  cval->cmask = ctl_mask;

  cval->val_type = (int )audio_feature_info[control - 1].type;

  if (ctl_mask == 0U) {

    cval->channels = 1;
  } else {

    c = 0;

    i = 0;

    while (i < 16) {

      if (ctl_mask & (unsigned int )(1 << i)) {

        c ++;
      }

      i ++;
    }

    cval->channels = c;
  }

  get_min_max(cval, 0);

  kctl = snd_ctl_new1((struct snd_kcontrol_new const *)(& usb_feature_unit_ctl),
                      (void *)cval);

  if (! kctl) {

    printk("<3>cannot malloc kcontrol\n");

    kfree((void const *)cval);

    return;
  }

  kctl->private_free = & usb_mixer_elem_free;

  tmp___1 = check_mapped_name(state, unitid, control, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                              (int )sizeof(kctl->id.name));

  len = (unsigned int )tmp___1;

  mapped_name = len != 0U;

  if (! len) {

    if (nameid) {

      tmp___2 = snd_usb_copy_string_desc(state, nameid, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                                         (int )sizeof(kctl->id.name));

      len = (unsigned int )tmp___2;
    }
  }

  switch (control) {
  case 1:
  case 2:

  if (! len) {

    tmp___3 = get_term_name(state, iterm, kctl->id.name, (int )sizeof(kctl->id.name),
                            1);

    len = (unsigned int )tmp___3;

    if (! len) {

      tmp___4 = get_term_name(state, & state->oterm, kctl->id.name, (int )sizeof(kctl->id.name),
                              1);

      len = (unsigned int )tmp___4;
    }

    if (! len) {

      tmp___5 = sprintf((char *)(kctl->id.name), "Feature %d", unitid);

      len = (unsigned int )tmp___5;
    }
  }

  if (! mapped_name) {

    if (! (state->oterm.type >> 16)) {

      if ((state->oterm.type & 65280) == 256) {

        tmp___6 = strlcat((char *)(kctl->id.name), " Capture", sizeof(kctl->id.name));

        len = (unsigned int )tmp___6;
      } else {

        tmp___7 = strlcat((char *)(kctl->id.name + len), " Playback", sizeof(kctl->id.name));

        len = (unsigned int )tmp___7;
      }
    }
  }

  if (control == 1) {

    tmp___8 = " Switch";
  } else {

    tmp___8 = " Volume";
  }

  strlcat((char *)(kctl->id.name + len), tmp___8, sizeof(kctl->id.name));

  if (control == 2) {

    kctl->tlv.c = & mixer_vol_tlv;

    kctl->vd[0].access |= (unsigned int )((1 << 4) | (1 << 28));
  }

  break;
  default:

  if (! len) {

    strlcpy((char *)(kctl->id.name), (char const *)audio_feature_info[control - 1].name,
            sizeof(kctl->id.name));
  }

  break;
  }

  switch ((int )(state->chip)->usb_id) {
  case (1137 << 16) | 257:
  case (1137 << 16) | 260:
  case (1137 << 16) | 261:
  case (1650 << 16) | 4161:

  tmp___9 = strcmp((char const *)(kctl->id.name), "PCM Playback Volume");

  if (! tmp___9) {

    if (cval->min == -15616) {

      printk("<6>using volume control quirk for the UDA1321/N101 chip\n");

      cval->max = -256;
    }
  }
  }

  add_control_to_empty(state, kctl);

  return;
}
}

static int parse_audio_feature_unit(struct mixer_build *state , int unitid , unsigned char *ftr )
{
  int channels ;
  int i ;
  int j ;
  struct usb_audio_term iterm ;
  unsigned int master_bits ;
  unsigned int first_ch_bits ;
  int err ;
  int csize ;
  int tmp ;
  unsigned int ch_bits ;
  unsigned int mask ;
  unsigned int tmp___0 ;

  {

  if ((int )*(ftr + 0) < 7) {

    printk("<3>usbaudio: unit %u: invalid FEATURE_UNIT descriptor\n", unitid);

    return (-22);
  } else {

    csize = (int )*(ftr + 5);

    if (csize) {

      if ((int )*(ftr + 0) < 7 + csize) {

        printk("<3>usbaudio: unit %u: invalid FEATURE_UNIT descriptor\n", unitid);

        return (-22);
      }
    } else {

      printk("<3>usbaudio: unit %u: invalid FEATURE_UNIT descriptor\n", unitid);

      return (-22);
    }
  }

  err = parse_audio_unit(state, (int )*(ftr + 4));

  if (err < 0) {

    return (err);
  }

  tmp = check_input_term(state, (int )*(ftr + 4), & iterm);

  if (tmp < 0) {

    return (-22);
  }

  channels = ((int )*(ftr + 0) - 7) / csize - 1;

  master_bits = snd_usb_combine_bytes(ftr + 6, csize);

  if (channels > 0) {

    first_ch_bits = snd_usb_combine_bytes((ftr + 6) + csize, csize);
  } else {

    first_ch_bits = 0U;
  }

  i = 0;

  while (i < 10) {

    ch_bits = 0U;

    j = 0;

    while (j < channels) {

      tmp___0 = snd_usb_combine_bytes((ftr + 6) + csize * (j + 1), csize);

      mask = tmp___0;

      if (mask & (unsigned int )(1 << i)) {

        ch_bits |= (unsigned int )(1 << j);
      }

      j ++;
    }

    if (ch_bits & 1U) {

      build_feature_ctl(state, ftr, ch_bits, i, & iterm, unitid);
    }

    if (master_bits & (unsigned int )(1 << i)) {

      build_feature_ctl(state, ftr, 0U, i, & iterm, unitid);
    }

    i ++;
  }

  return (0);
}
}

static void build_mixer_unit_ctl(struct mixer_build *state , unsigned char *desc ,
                                 int in_pin , int in_ch , int unitid , struct usb_audio_term *iterm )
{
  struct usb_mixer_elem_info *cval ;
  unsigned int input_pins ;
  unsigned int num_outs ;
  unsigned int i ;
  unsigned int len ;
  struct snd_kcontrol *kctl ;
  int tmp ;
  void *tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;

  {

  input_pins = (unsigned int )*(desc + 4);

  num_outs = (unsigned int )*(desc + (5U + input_pins));

  tmp = check_ignored_ctl(state, unitid, 0);

  if (tmp) {

    return;
  }

  tmp___0 = kzalloc(sizeof(*cval), 208U);

  cval = (struct usb_mixer_elem_info *)tmp___0;

  if (! cval) {

    return;
  }

  cval->mixer = state->mixer;

  cval->id = (unsigned int )unitid;

  cval->control = (unsigned int )(in_ch + 1);

  cval->val_type = 4;

  i = 0U;

  while (i < num_outs) {

    tmp___1 = check_matrix_bitmap((desc + 9) + input_pins, in_ch, (int )i, (int )num_outs);

    if (tmp___1) {

      cval->cmask |= (unsigned int )(1 << i);

      (cval->channels) ++;
    }

    i ++;
  }

  get_min_max(cval, 0);

  kctl = snd_ctl_new1((struct snd_kcontrol_new const *)(& usb_feature_unit_ctl),
                      (void *)cval);

  if (! kctl) {

    printk("<3>cannot malloc kcontrol\n");

    kfree((void const *)cval);

    return;
  }

  kctl->private_free = & usb_mixer_elem_free;

  tmp___2 = check_mapped_name(state, unitid, 0, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                              (int )sizeof(kctl->id.name));

  len = (unsigned int )tmp___2;

  if (! len) {

    tmp___3 = get_term_name(state, iterm, kctl->id.name, (int )sizeof(kctl->id.name),
                            0);

    len = (unsigned int )tmp___3;
  }

  if (! len) {

    tmp___4 = sprintf((char *)(kctl->id.name), "Mixer Source %d", in_ch + 1);

    len = (unsigned int )tmp___4;
  }

  strlcat((char *)(kctl->id.name + len), " Volume", sizeof(kctl->id.name));

  add_control_to_empty(state, kctl);

  return;
}
}

static int parse_audio_mixer_unit(struct mixer_build *state , int unitid , unsigned char *desc )
{
  struct usb_audio_term iterm ;
  int input_pins ;
  int num_ins ;
  int num_outs ;
  int pin ;
  int ich ;
  int err ;
  int och ;
  int ich_has_controls ;
  int tmp ;

  {

  if ((int )*(desc + 0) < 11) {

    printk("<3>invalid MIXER UNIT descriptor %d\n", unitid);

    return (-22);
  } else {

    input_pins = (int )*(desc + 4);

    if (input_pins) {

      num_outs = (int )*(desc + (5 + input_pins));

      if (! num_outs) {

        printk("<3>invalid MIXER UNIT descriptor %d\n", unitid);

        return (-22);
      }
    } else {

      printk("<3>invalid MIXER UNIT descriptor %d\n", unitid);

      return (-22);
    }
  }

  if ((int )*(desc + 0) <= 10 + input_pins) {

    return (0);
  }

  num_ins = 0;

  ich = 0;

  pin = 0;

  while (pin < input_pins) {

    err = parse_audio_unit(state, (int )*(desc + (5 + pin)));

    if (err < 0) {

      return (err);
    }

    err = check_input_term(state, (int )*(desc + (5 + pin)), & iterm);

    if (err < 0) {

      return (err);
    }

    num_ins += iterm.channels;

    while (ich < num_ins) {

      ich_has_controls = 0;

      och = 0;

      while (och < num_outs) {

        tmp = check_matrix_bitmap((desc + 9) + input_pins, ich, och, num_outs);

        if (tmp) {

          ich_has_controls = 1;

          break;
        }

        och ++;
      }

      if (ich_has_controls) {

        build_mixer_unit_ctl(state, desc, pin, ich, unitid, & iterm);
      }

      ich ++;
    }

    pin ++;
  }

  return (0);
}
}

static int mixer_ctl_procunit_get(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_elem_info *cval ;
  int err ;
  int val ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  err = get_cur_ctl_value(cval, (int )(cval->control << 8), & val);

  if (err < 0) {

    if ((cval->mixer)->ignore_ctl_error) {

      ucontrol->value.integer.value[0] = (long )cval->min;

      return (0);
    }
  }

  if (err < 0) {

    return (err);
  }

  val = get_relative_value(cval, val);

  ucontrol->value.integer.value[0] = (long )val;

  return (0);
}
}

static int mixer_ctl_procunit_put(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_elem_info *cval ;
  int val ;
  int oval ;
  int err ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  err = get_cur_ctl_value(cval, (int )(cval->control << 8), & oval);

  if (err < 0) {

    if ((cval->mixer)->ignore_ctl_error) {

      return (0);
    }

    return (err);
  }

  val = (int )ucontrol->value.integer.value[0];

  val = get_abs_value(cval, val);

  if (val != oval) {

    set_cur_ctl_value(cval, (int )(cval->control << 8), val);

    return (1);
  }

  return (0);
}
}

static struct snd_kcontrol_new __attribute__((__noderef__, __address_space__(2))) mixer_procunit_ctl =

     {2, 0U, 0U, (unsigned char *)"", 0U, 0U, 0U, & mixer_ctl_feature_info, & mixer_ctl_procunit_get,
    & mixer_ctl_procunit_put, {(snd_kcontrol_tlv_rw_t *)0}, 0UL};

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) updown_proc_info)[3] = { {1,
      (char *)"Switch", 0, 0},
        {2, (char *)"Mode Select", 3, 1},
        {0, (char *)0, 0, 0}};

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) prologic_proc_info)[3] = { {1,
      (char *)"Switch", 0, 0},
        {2, (char *)"Mode Select", 3, 1},
        {0, (char *)0, 0, 0}};

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) threed_enh_proc_info)[3] = { {1,
      (char *)"Switch", 0, 0},
        {2, (char *)"Spaciousness", 3, 0},
        {0, (char *)0, 0, 0}};

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) reverb_proc_info)[5] = { {1,
      (char *)"Switch", 0, 0},
        {2, (char *)"Level", 3, 0},
        {3, (char *)"Time", 5, 0},
        {4, (char *)"Delay", 3, 0},
        {0, (char *)0, 0, 0}};

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) chorus_proc_info)[5] = { {1,
      (char *)"Switch", 0, 0},
        {2, (char *)"Level", 3, 0},
        {3, (char *)"Rate", 5, 0},
        {4, (char *)"Depth", 5, 0},
        {0, (char *)0, 0, 0}};

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) dcr_proc_info)[7] = { {1,
      (char *)"Switch", 0, 0},
        {2, (char *)"Ratio", 5, 0},
        {3, (char *)"Max Amp", 4, 0},
        {4, (char *)"Threshold", 4, 0},
        {5, (char *)"Attack Time", 5, 0},
        {6, (char *)"Release Time", 5, 0},
        {0, (char *)0, 0, 0}};

static struct procunit_info ( __attribute__((__noderef__, __address_space__(2))) procunits)[7] = { {1,
      (char *)"Up Down", (struct procunit_value_info *)(updown_proc_info)},
        {2, (char *)"Dolby Prologic", (struct procunit_value_info *)(prologic_proc_info)},
        {3,
      (char *)"3D Stereo Extender", (struct procunit_value_info *)(threed_enh_proc_info)},
        {4,
      (char *)"Reverb", (struct procunit_value_info *)(reverb_proc_info)},
        {5, (char *)"Chorus", (struct procunit_value_info *)(chorus_proc_info)},
        {6, (char *)"DCR", (struct procunit_value_info *)(dcr_proc_info)},
        {0, (char *)0, (struct procunit_value_info *)0}};

static int build_audio_procunit(struct mixer_build *state , int unitid , unsigned char *dsc ,
                                struct procunit_info *list , char *name ) ;

static struct procunit_value_info ( __attribute__((__noderef__, __address_space__(2))) default_value_info)[2] = { {1,
      (char *)"Switch", 0, 0},
        {0, (char *)0, 0, 0}};

static struct procunit_info __attribute__((__noderef__, __address_space__(2))) default_info = {0,
    (char *)((void *)0), (struct procunit_value_info *)(default_value_info)};

static int build_audio_procunit(struct mixer_build *state , int unitid , unsigned char *dsc ,
                                struct procunit_info *list , char *name )
{
  int num_ins ;
  struct usb_mixer_elem_info *cval ;
  struct snd_kcontrol *kctl ;
  int i ;
  int err ;
  int nameid ;
  int type ;
  int len ;
  struct procunit_info *info ;
  struct procunit_value_info *valinfo ;
  int tmp ;
  void *tmp___0 ;
  int tmp___1 ;

  {

  num_ins = (int )*(dsc + 6);

  if ((int )*(dsc + 0) < 13) {

    printk("<3>invalid %s descriptor (id %d)\n", name, unitid);

    return (-22);
  } else

  if ((int )*(dsc + 0) < 13 + num_ins) {

    printk("<3>invalid %s descriptor (id %d)\n", name, unitid);

    return (-22);
  } else

  if ((int )*(dsc + 0) < num_ins + (int )*(dsc + (11 + num_ins))) {

    printk("<3>invalid %s descriptor (id %d)\n", name, unitid);

    return (-22);
  }

  i = 0;

  while (i < num_ins) {

    err = parse_audio_unit(state, (int )*(dsc + (7 + i)));

    if (err < 0) {

      return (err);
    }

    i ++;
  }

  type = (int )((unsigned int )*(dsc + 4) | ((unsigned int )*((dsc + 4) + 1) << 8));

  info = list;

  while (1) {

    if (info) {

      if (! info->type) {

        break;
      }
    } else {

      break;
    }

    if (info->type == type) {

      break;
    }

    info ++;
  }

  if (! info) {

    info = (struct procunit_info *)(& default_info);
  } else

  if (! info->type) {

    info = (struct procunit_info *)(& default_info);
  }

  valinfo = info->values;

  while (valinfo->control) {

    if (! ((int )*(dsc + (12 + num_ins)) & (1 << (valinfo->control - 1)))) {

      goto __Cont;
    }

    tmp = check_ignored_ctl(state, unitid, valinfo->control);

    if (tmp) {

      goto __Cont;
    }

    tmp___0 = kzalloc(sizeof(*cval), 208U);

    cval = (struct usb_mixer_elem_info *)tmp___0;

    if (! cval) {

      printk("<3>cannot malloc kcontrol\n");

      return (-12);
    }

    cval->mixer = state->mixer;

    cval->id = (unsigned int )unitid;

    cval->control = (unsigned int )valinfo->control;

    cval->val_type = valinfo->val_type;

    cval->channels = 1;

    if (type == 1) {

      if (cval->control == 2U) {

        cval->min = 1;

        cval->max = (int )*(dsc + 15);

        cval->res = 1;

        cval->initialized = (u8 )1;
      } else {

        get_min_max(cval, valinfo->min_value);
      }
    } else {

      get_min_max(cval, valinfo->min_value);
    }

    kctl = snd_ctl_new1((struct snd_kcontrol_new const *)(& mixer_procunit_ctl),
                        (void *)cval);

    if (! kctl) {

      printk("<3>cannot malloc kcontrol\n");

      kfree((void const *)cval);

      return (-12);
    }

    kctl->private_free = & usb_mixer_elem_free;

    tmp___1 = check_mapped_name(state, unitid, (int )cval->control, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                                (int )sizeof(kctl->id.name));

    if (! tmp___1) {

      if (info->name) {

        strlcpy((char *)(kctl->id.name), (char const *)info->name, sizeof(kctl->id.name));
      } else {

        nameid = (int )*(dsc + ((12 + num_ins) + (int )*(dsc + (11 + num_ins))));

        len = 0;

        if (nameid) {

          len = snd_usb_copy_string_desc(state, nameid, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                                         (int )sizeof(kctl->id.name));
        }

        if (! len) {

          strlcpy((char *)(kctl->id.name), (char const *)name, sizeof(kctl->id.name));
        }
      }
    }

    strlcat((char *)(kctl->id.name), " ", sizeof(kctl->id.name));

    strlcat((char *)(kctl->id.name), (char const *)valinfo->suffix, sizeof(kctl->id.name));

    err = add_control_to_empty(state, kctl);

    if (err < 0) {

      return (err);
    }
    __Cont:

    valinfo ++;
  }

  return (0);
}
}

static int parse_audio_processing_unit(struct mixer_build *state , int unitid , unsigned char *desc )
{
  int tmp ;

  {

  tmp = build_audio_procunit(state, unitid, desc, (struct procunit_info *)(procunits),
                             (char *)"Processing Unit");

  return (tmp);
}
}

static int parse_audio_extension_unit(struct mixer_build *state , int unitid , unsigned char *desc )
{
  int tmp ;

  {

  tmp = build_audio_procunit(state, unitid, desc, (struct procunit_info *)((void *)0),
                             (char *)"Extension Unit");

  return (tmp);
}
}

static int mixer_ctl_selector_info(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_info *uinfo )
{
  struct usb_mixer_elem_info *cval ;
  char **itemlist ;
  int tmp ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  itemlist = (char **)kcontrol->private_value;

  tmp = __snd_bug_on(0);

  if (tmp) {

    return (-22);
  }

  uinfo->type = 3;

  uinfo->count = 1U;

  uinfo->value.enumerated.items = (unsigned int )cval->max;

  if ((int )uinfo->value.enumerated.item >= cval->max) {

    uinfo->value.enumerated.item = (unsigned int )(cval->max - 1);
  }

  strcpy(uinfo->value.enumerated.name, (char const *)*(itemlist + uinfo->value.enumerated.item));

  return (0);
}
}

static int mixer_ctl_selector_get(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_elem_info *cval ;
  int val ;
  int err ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  err = get_cur_ctl_value(cval, 0, & val);

  if (err < 0) {

    if ((cval->mixer)->ignore_ctl_error) {

      ucontrol->value.enumerated.item[0] = 0U;

      return (0);
    }

    return (err);
  }

  val = get_relative_value(cval, val);

  ucontrol->value.enumerated.item[0] = (unsigned int )val;

  return (0);
}
}

static int mixer_ctl_selector_put(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_elem_info *cval ;
  int val ;
  int oval ;
  int err ;

  {

  cval = (struct usb_mixer_elem_info *)kcontrol->private_data;

  err = get_cur_ctl_value(cval, 0, & oval);

  if (err < 0) {

    if ((cval->mixer)->ignore_ctl_error) {

      return (0);
    }

    return (err);
  }

  val = (int )ucontrol->value.enumerated.item[0];

  val = get_abs_value(cval, val);

  if (val != oval) {

    set_cur_ctl_value(cval, 0, val);

    return (1);
  }

  return (0);
}
}

static struct snd_kcontrol_new __attribute__((__noderef__, __address_space__(2))) mixer_selectunit_ctl =

     {2, 0U, 0U, (unsigned char *)"", 0U, 0U, 0U, & mixer_ctl_selector_info, & mixer_ctl_selector_get,
    & mixer_ctl_selector_put, {(snd_kcontrol_tlv_rw_t *)0}, 0UL};

static void usb_mixer_selector_elem_free(struct snd_kcontrol *kctl )
{
  int i ;
  int num_ins ;
  struct usb_mixer_elem_info *cval ;
  char **itemlist ;

  {

  num_ins = 0;

  if (kctl->private_data) {

    cval = (struct usb_mixer_elem_info *)kctl->private_data;

    num_ins = cval->max;

    kfree((void const *)cval);

    kctl->private_data = (char *)((void *)0);
  }

  if (kctl->private_value) {

    itemlist = (char **)kctl->private_value;

    i = 0;

    while (i < num_ins) {

      kfree((void const *)*(itemlist + i));

      i ++;
    }

    kfree((void const *)itemlist);

    kctl->private_value = 0UL;
  }

  return;
}
}

static int parse_audio_selector_unit(struct mixer_build *state , int unitid , unsigned char *desc )
{
  unsigned int num_ins ;
  unsigned int i ;
  unsigned int nameid ;
  unsigned int len ;
  int err ;
  struct usb_mixer_elem_info *cval ;
  struct snd_kcontrol *kctl ;
  char **namelist ;
  int tmp ;
  void *tmp___0 ;
  void *tmp___1 ;
  struct usb_audio_term iterm ;
  void *tmp___2 ;
  unsigned int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  int tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;

  {

  num_ins = (unsigned int )*(desc + 4);

  if (! num_ins) {

    printk("<3>invalid SELECTOR UNIT descriptor %d\n", unitid);

    return (-22);
  } else

  if ((unsigned int )*(desc + 0) < 5U + num_ins) {

    printk("<3>invalid SELECTOR UNIT descriptor %d\n", unitid);

    return (-22);
  }

  i = 0U;

  while (i < num_ins) {

    err = parse_audio_unit(state, (int )*(desc + (5U + i)));

    if (err < 0) {

      return (err);
    }

    i ++;
  }

  if (num_ins == 1U) {

    return (0);
  }

  tmp = check_ignored_ctl(state, unitid, 0);

  if (tmp) {

    return (0);
  }

  tmp___0 = kzalloc(sizeof(*cval), 208U);

  cval = (struct usb_mixer_elem_info *)tmp___0;

  if (! cval) {

    printk("<3>cannot malloc kcontrol\n");

    return (-12);
  }

  cval->mixer = state->mixer;

  cval->id = (unsigned int )unitid;

  cval->val_type = 3;

  cval->channels = 1;

  cval->min = 1;

  cval->max = (int )num_ins;

  cval->res = 1;

  cval->initialized = (u8 )1;

  tmp___1 = kmalloc(sizeof(char *) * (unsigned long )num_ins, 208U);

  namelist = (char **)tmp___1;

  if (! namelist) {

    printk("<3>cannot malloc\n");

    kfree((void const *)cval);

    return (-12);
  }

  i = 0U;

  while (i < num_ins) {

    len = 0U;

    tmp___2 = kmalloc((size_t )64, 208U);

    *(namelist + i) = (char *)tmp___2;

    if (! *(namelist + i)) {

      printk("<3>cannot malloc\n");

      while (1) {

        tmp___3 = i;

        i --;

        if (! tmp___3) {

          break;
        }

        kfree((void const *)*(namelist + i));
      }

      kfree((void const *)namelist);

      kfree((void const *)cval);

      return (-12);
    }

    tmp___4 = check_mapped_selector_name(state, unitid, (int )i, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )*(namelist + i),
                                         64);

    len = (unsigned int )tmp___4;

    if (! len) {

      tmp___6 = check_input_term(state, (int )*(desc + (5U + i)), & iterm);

      if (tmp___6 >= 0) {

        tmp___5 = get_term_name(state, & iterm, (unsigned char *)*(namelist + i),
                                64, 0);

        len = (unsigned int )tmp___5;
      }
    }

    if (! len) {

      sprintf(*(namelist + i), "Input %d", i);
    }

    i ++;
  }

  kctl = snd_ctl_new1((struct snd_kcontrol_new const *)(& mixer_selectunit_ctl),
                      (void *)cval);

  if (! kctl) {

    printk("<3>cannot malloc kcontrol\n");

    kfree((void const *)namelist);

    kfree((void const *)cval);

    return (-12);
  }

  kctl->private_value = (unsigned long )namelist;

  kctl->private_free = & usb_mixer_selector_elem_free;

  nameid = (unsigned int )*(desc + ((int )*(desc + 0) - 1));

  tmp___7 = check_mapped_name(state, unitid, 0, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                              (int )sizeof(kctl->id.name));

  len = (unsigned int )tmp___7;

  if (! len) {

    if (nameid) {

      snd_usb_copy_string_desc(state, (int )nameid, (char * __attribute__((__exp__(Nonstub_return_buflen("buflen")))) )(kctl->id.name),
                               (int )sizeof(kctl->id.name));
    } else {

      tmp___8 = get_term_name(state, & state->oterm, kctl->id.name, (int )sizeof(kctl->id.name),
                              0);

      len = (unsigned int )tmp___8;

      if (! len) {

        strlcpy((char *)(kctl->id.name), "USB", sizeof(kctl->id.name));
      }

      if ((state->oterm.type & 65280) == 256) {

        strlcat((char *)(kctl->id.name), " Capture Source", sizeof(kctl->id.name));
      } else {

        strlcat((char *)(kctl->id.name), " Playback Source", sizeof(kctl->id.name));
      }
    }
  }

  err = add_control_to_empty(state, kctl);

  if (err < 0) {

    return (err);
  }

  return (0);
}
}

static int parse_audio_unit(struct mixer_build *state , int unitid )
{
  unsigned char *p1 ;
  int tmp ;
  void *tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;

  {

  tmp = test_and_set_bit(unitid, (unsigned long volatile *)(state->unitbitmap));

  if (tmp) {

    return (0);
  }

  tmp___0 = find_audio_control_unit(state, (unsigned char )unitid);

  p1 = (unsigned char *)tmp___0;

  if (! p1) {

    printk("<3>usbaudio: unit %d not found!\n", unitid);

    return (-22);
  }

  switch ((int )*(p1 + 2)) {
  case 2:

  return (0);
  case 4:

  tmp___1 = parse_audio_mixer_unit(state, unitid, p1);

  return (tmp___1);
  case 5:

  tmp___2 = parse_audio_selector_unit(state, unitid, p1);

  return (tmp___2);
  case 6:

  tmp___3 = parse_audio_feature_unit(state, unitid, p1);

  return (tmp___3);
  case 7:

  tmp___4 = parse_audio_processing_unit(state, unitid, p1);

  return (tmp___4);
  case 8:

  tmp___5 = parse_audio_extension_unit(state, unitid, p1);

  return (tmp___5);
  default:

  printk("<3>usbaudio: unit %u: unexpected type 0x%02x\n", unitid, (int )*(p1 + 2));

  return (-22);
  }
}
}

static void snd_usb_mixer_free(struct usb_mixer_interface *mixer )
{


  {

  kfree((void const *)mixer->id_elems);

  if (mixer->urb) {

    kfree((void const *)(mixer->urb)->transfer_buffer);

    usb_free_urb(mixer->urb);
  }

  usb_free_urb(mixer->rc_urb);

  kfree((void const *)mixer->rc_setup_packet);

  kfree((void const *)mixer);

  return;
}
}

static int snd_usb_mixer_dev_free(struct snd_device *device )
{
  struct usb_mixer_interface *mixer ;

  {

  mixer = (struct usb_mixer_interface *)device->device_data;

  snd_usb_mixer_free(mixer);

  return (0);
}
}

static int snd_usb_mixer_controls(struct usb_mixer_interface *mixer )
{
  unsigned char *desc ;
  struct mixer_build state ;
  int err ;
  struct usbmix_ctl_map const *map ;
  struct usb_host_interface *hostif ;
  struct usb_interface *tmp ;
  void *tmp___0 ;

  {

  tmp = usb_ifnum_to_if((struct usb_device const *)(mixer->chip)->dev, mixer->ctrlif);

  hostif = tmp->altsetting + 0;

  memset((void *)(& state), 0, sizeof(state));

  state.chip = mixer->chip;

  state.mixer = mixer;

  state.buffer = (unsigned char * __attribute__((__expfld__(buflen))) )hostif->extra;

  state.buflen = (unsigned int )hostif->extralen;

  map = (struct usbmix_ctl_map const *)(usbmix_ctl_maps);

  while (map->id) {

    if (map->id == (u32 const )(state.chip)->usb_id) {

      state.map = (struct usbmix_name_map const *)map->map;

      state.selector_map = (struct usbmix_selector_map const *)map->selector_map;

      mixer->ignore_ctl_error = (unsigned int )map->ignore_ctl_error;

      break;
    }

    map ++;
  }

  desc = (unsigned char *)((void *)0);

  while (1) {

    tmp___0 = snd_usb_find_csint_desc((void *)hostif->extra, hostif->extralen, (void *)desc,
                                      (u8 )3);

    desc = (unsigned char *)tmp___0;

    if (! ((unsigned long )desc != (unsigned long )((void *)0))) {

      break;
    }

    if ((int )*(desc + 0) < 9) {

      continue;
    }

    set_bit((unsigned int )*(desc + 3), (unsigned long volatile *)(state.unitbitmap));

    state.oterm.id = (int )*(desc + 3);

    state.oterm.type = (int )((unsigned int )*(desc + 4) | ((unsigned int )*((desc + 4) + 1) << 8));

    state.oterm.name = (int )*(desc + 8);

    err = parse_audio_unit(& state, (int )*(desc + 7));

    if (err < 0) {

      return (err);
    }
  }

  return (0);
}
}

static void snd_usb_mixer_notify_id(struct usb_mixer_interface *mixer , int unitid )
{
  struct usb_mixer_elem_info *info ;

  {

  info = *(mixer->id_elems + unitid);

  while (info) {

    snd_ctl_notify((mixer->chip)->card, 1U, info->elem_id);

    info = (struct usb_mixer_elem_info *)info->next_id_elem;
  }

  return;
}
}

static void snd_usb_mixer_memory_change(struct usb_mixer_interface *mixer , int unitid )
{


  {

  if (! mixer->rc_cfg) {

    return;
  }

  switch (unitid) {
  case 0:

  (mixer->rc_urb)->dev = (struct usb_device * __attribute__((__recursive__)) )(mixer->chip)->dev;

  usb_submit_urb(mixer->rc_urb, 32U);

  break;
  case 4:
  case 7:
  case 19:
  case 20:

  break;
  case 3:

  if ((mixer->chip)->usb_id == (u32 )((1054 << 16) | 12352)) {

    snd_usb_mixer_notify_id(mixer, (int )(mixer->rc_cfg)->mute_mixer_id);
  }

  break;
  default:

  while (1) {

    break;
  }

  break;
  }

  return;
}
}

static void snd_usb_mixer_status_complete(struct urb *urb )
{
  struct usb_mixer_interface *mixer ;
  u8 *buf ;
  int i ;

  {

  mixer = (struct usb_mixer_interface *)urb->context;

  if (urb->status == 0) {

    buf = urb->transfer_buffer;

    i = urb->actual_length;

    while (i >= 2) {

      while (1) {

        break;
      }

      if (((int )*(buf + 0) & 15) != 0) {

        goto __Cont;
      }

      if (! ((int )*(buf + 0) & 64)) {

        snd_usb_mixer_notify_id(mixer, (int )*(buf + 1));
      } else {

        snd_usb_mixer_memory_change(mixer, (int )*(buf + 1));
      }
      __Cont:

      buf += 2;

      i -= 2;
    }
  }

  if (urb->status != -2) {

    if (urb->status != -104) {

      urb->dev = (struct usb_device * __attribute__((__recursive__)) )(mixer->chip)->dev;

      usb_submit_urb(urb, 32U);
    }
  }

  return;
}
}

static int snd_usb_mixer_status_create(struct usb_mixer_interface *mixer )
{
  struct usb_host_interface *hostif ;
  struct usb_endpoint_descriptor *ep ;
  void *transfer_buffer ;
  int buffer_length ;
  unsigned int epnum ;
  struct usb_interface *tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  unsigned int tmp___3 ;

  {

  tmp = usb_ifnum_to_if((struct usb_device const *)(mixer->chip)->dev, mixer->ctrlif);

  hostif = tmp->altsetting + 0;

  if ((int )hostif->desc.bNumEndpoints < 1) {

    return (0);
  }

  ep = & (hostif->endpoint + 0)->desc;

  tmp___0 = usb_endpoint_dir_in((struct usb_endpoint_descriptor const *)ep);

  if (tmp___0) {

    tmp___1 = usb_endpoint_xfer_int((struct usb_endpoint_descriptor const *)ep);

    if (! tmp___1) {

      return (0);
    }
  } else {

    return (0);
  }

  tmp___2 = usb_endpoint_num((struct usb_endpoint_descriptor const *)ep);

  epnum = (unsigned int )tmp___2;

  buffer_length = (int )ep->wMaxPacketSize;

  transfer_buffer = kmalloc((size_t )buffer_length, 208U);

  if (! transfer_buffer) {

    return (-12);
  }

  mixer->urb = usb_alloc_urb(0, 208U);

  if (! mixer->urb) {

    kfree((void const *)transfer_buffer);

    return (-12);
  }

  tmp___3 = __create_pipe((mixer->chip)->dev, epnum);

  usb_fill_int_urb(mixer->urb, (mixer->chip)->dev, ((unsigned int )(1 << 30) | tmp___3) | 128U,
                   transfer_buffer, buffer_length, & snd_usb_mixer_status_complete,
                   (void *)mixer, (int )ep->bInterval);

  usb_submit_urb(mixer->urb, 208U);

  return (0);
}
}

static void snd_usb_soundblaster_remote_complete(struct urb *urb )
{
  struct usb_mixer_interface *mixer ;
  struct rc_config const *rc ;
  u32 code ;

  {

  mixer = (struct usb_mixer_interface *)urb->context;

  rc = mixer->rc_cfg;

  if (urb->status < 0) {

    return;
  } else

  if (urb->actual_length < (int )rc->min_packet_length) {

    return;
  }

  code = (u32 )mixer->rc_buffer[rc->offset];

  if ((int const )rc->length == 2) {

    code |= (unsigned int )((int )mixer->rc_buffer[(int const )rc->offset + 1] << 8);
  }

  if (code == (u32 )rc->mute_code) {

    snd_usb_mixer_notify_id(mixer, (int )rc->mute_mixer_id);
  }

  mixer->rc_code = code;

  __asm__ volatile ("sfence": : : "memory");

  __wake_up(& mixer->rc_waitq, 3U, 1, (void *)0);

  return;
}
}

static int snd_usb_sbrc_hwdep_open(struct snd_hwdep *hw , struct file *file )
{
  struct usb_mixer_interface *mixer ;
  int tmp ;

  {

  mixer = (struct usb_mixer_interface *)hw->private_data;

  tmp = test_and_set_bit(0, (unsigned long volatile *)(& mixer->rc_hwdep_open));

  if (tmp) {

    return (-16);
  }

  return (0);
}
}

static int snd_usb_sbrc_hwdep_release(struct snd_hwdep *hw , struct file *file )
{
  struct usb_mixer_interface *mixer ;

  {

  mixer = (struct usb_mixer_interface *)hw->private_data;

  clear_bit(0, (unsigned long volatile *)(& mixer->rc_hwdep_open));

  __asm__ volatile ("": : : "memory");

  return (0);
}
}

static long snd_usb_sbrc_hwdep_read(struct snd_hwdep *hw , char *buf , long count ,
                                    loff_t *offset )
{
  struct usb_mixer_interface *mixer ;
  int err ;
  u32 rc_code ;
  int __ret ;
  wait_queue_t __wait ;
  struct task_struct *tmp ;
  unsigned long tmp___0 ;
  struct task_struct *tmp___1 ;
  int tmp___2 ;
  unsigned long tmp___3 ;
  int __ret_pu ;
  char __pu_val ;
  int __ret_pu___0 ;
  u32 __pu_val___0 ;
  long tmp___4 ;

  {

  mixer = (struct usb_mixer_interface *)hw->private_data;

  if (count != 1L) {

    if (count != 4L) {

      return (-22L);
    }
  }

  __ret = 0;

  tmp___3 = __xchg(0UL, (void volatile *)(& mixer->rc_code), (int )sizeof(mixer->rc_code));

  rc_code = (u32 )tmp___3;

  if (! (rc_code != 0U)) {

    while (1) {

      tmp = get_current();

      __wait.flags = 0U;

      __wait.private = (void *)tmp;

      __wait.func = & autoremove_wake_function;

      __wait.task_list.next = (struct list_head * __attribute__((__recursive__)) )(& __wait.task_list);

      __wait.task_list.prev = (struct list_head * __attribute__((__recursive__)) )(& __wait.task_list);

      while (1) {

        prepare_to_wait(& mixer->rc_waitq, & __wait, 1);

        tmp___0 = __xchg(0UL, (void volatile *)(& mixer->rc_code), (int )sizeof(mixer->rc_code));

        rc_code = (u32 )tmp___0;

        if (rc_code != 0U) {

          break;
        }

        tmp___1 = get_current();

        tmp___2 = signal_pending(tmp___1);

        if (! tmp___2) {

          schedule();

          goto __Cont;
        }

        __ret = -512;

        break;
        __Cont: ;
      }

      finish_wait(& mixer->rc_waitq, & __wait);

      break;
    }
  }

  err = __ret;

  if (err == 0) {

    if (count == 1L) {

      might_fault();

      __pu_val = (char )rc_code;

      switch ((int )sizeof(*buf)) {
      case 1:

      __asm__ volatile ("call __put_user_"
                           "1": "=a" (__ret_pu): "0" (__pu_val), "c" (buf): "ebx");

      break;
      case 2:

      __asm__ volatile ("call __put_user_"
                           "2": "=a" (__ret_pu): "0" (__pu_val), "c" (buf): "ebx");

      break;
      case 4:

      __asm__ volatile ("call __put_user_"
                           "4": "=a" (__ret_pu): "0" (__pu_val), "c" (buf): "ebx");

      break;
      case 8:

      __asm__ volatile ("call __put_user_"
                           "8": "=a" (__ret_pu): "0" (__pu_val), "c" (buf): "ebx");

      break;
      default:

      __asm__ volatile ("call __put_user_"
                           "X": "=a" (__ret_pu): "0" (__pu_val), "c" (buf): "ebx");

      break;
      }

      err = __ret_pu;
    } else {

      might_fault();

      __pu_val___0 = rc_code;

      switch ((int )sizeof(*((u32 *)buf))) {
      case 1:

      __asm__ volatile ("call __put_user_"
                           "1": "=a" (__ret_pu___0): "0" (__pu_val___0), "c" ((u32 *)buf): "ebx");

      break;
      case 2:

      __asm__ volatile ("call __put_user_"
                           "2": "=a" (__ret_pu___0): "0" (__pu_val___0), "c" ((u32 *)buf): "ebx");

      break;
      case 4:

      __asm__ volatile ("call __put_user_"
                           "4": "=a" (__ret_pu___0): "0" (__pu_val___0), "c" ((u32 *)buf): "ebx");

      break;
      case 8:

      __asm__ volatile ("call __put_user_"
                           "8": "=a" (__ret_pu___0): "0" (__pu_val___0), "c" ((u32 *)buf): "ebx");

      break;
      default:

      __asm__ volatile ("call __put_user_"
                           "X": "=a" (__ret_pu___0): "0" (__pu_val___0), "c" ((u32 *)buf): "ebx");

      break;
      }

      err = __ret_pu___0;
    }
  }

  if (err < 0) {

    tmp___4 = (long )err;
  } else {

    tmp___4 = count;
  }

  return (tmp___4);
}
}

static unsigned int snd_usb_sbrc_hwdep_poll(struct snd_hwdep *hw , struct file *file ,
                                            poll_table *wait )
{
  struct usb_mixer_interface *mixer ;
  int tmp ;

  {

  mixer = (struct usb_mixer_interface *)hw->private_data;

  poll_wait(file, & mixer->rc_waitq, wait);

  if (mixer->rc_code) {

    tmp = 65;
  } else {

    tmp = 0;
  }

  return ((unsigned int )tmp);
}
}

static int snd_usb_soundblaster_remote_init(struct usb_mixer_interface *mixer )
{
  struct snd_hwdep *hwdep ;
  int err ;
  int len ;
  int i ;
  void *tmp ;
  unsigned int tmp___0 ;

  {

  i = 0;

  while ((unsigned long )i < sizeof(rc_configs) / sizeof(rc_configs[0]) + (sizeof(char [1]) - 1UL)) {

    if (rc_configs[i].usb_id == (u32 const )(mixer->chip)->usb_id) {

      break;
    }

    i ++;
  }

  if ((unsigned long )i >= sizeof(rc_configs) / sizeof(rc_configs[0]) + (sizeof(char [1]) - 1UL)) {

    return (0);
  }

  mixer->rc_cfg = & rc_configs[i];

  len = (int )(mixer->rc_cfg)->packet_length;

  init_waitqueue_head(& mixer->rc_waitq);

  err = snd_hwdep_new((mixer->chip)->card, (char * __attribute__((__nullterm__)) )"SB remote control",
                      0, & hwdep);

  if (err < 0) {

    return (err);
  }

  sprintf(hwdep->name, "%s remote control", ((mixer->chip)->card)->shortname);

  hwdep->iface = 15;

  hwdep->private_data = (void *)mixer;

  hwdep->ops.read = & snd_usb_sbrc_hwdep_read;

  hwdep->ops.open = & snd_usb_sbrc_hwdep_open;

  hwdep->ops.release = & snd_usb_sbrc_hwdep_release;

  hwdep->ops.poll = & snd_usb_sbrc_hwdep_poll;

  mixer->rc_urb = usb_alloc_urb(0, 208U);

  if (! mixer->rc_urb) {

    return (-12);
  }

  tmp = kmalloc(sizeof(*(mixer->rc_setup_packet)), 208U);

  mixer->rc_setup_packet = (struct usb_ctrlrequest *)tmp;

  if (! mixer->rc_setup_packet) {

    usb_free_urb(mixer->rc_urb);

    mixer->rc_urb = (struct urb *)((void *)0);

    return (-12);
  }

  (mixer->rc_setup_packet)->bRequestType = (__u8 )((128 | (1 << 5)) | 1);

  (mixer->rc_setup_packet)->bRequest = (__u8 )133;

  (mixer->rc_setup_packet)->wValue = (__u16 )0;

  (mixer->rc_setup_packet)->wIndex = (__u16 )0;

  (mixer->rc_setup_packet)->wLength = (__u16 )len;

  tmp___0 = __create_pipe((mixer->chip)->dev, 0U);

  usb_fill_control_urb(mixer->rc_urb, (mixer->chip)->dev, ((unsigned int )(2 << 30) | tmp___0) | 128U,
                       (u8 *)mixer->rc_setup_packet, (void *)(mixer->rc_buffer), len,
                       & snd_usb_soundblaster_remote_complete, (void *)mixer);

  return (0);
}
}

static int snd_audigy2nx_led_get(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_interface *mixer ;
  int index___0 ;

  {

  mixer = (struct usb_mixer_interface *)kcontrol->private_data;

  index___0 = (int )kcontrol->private_value;

  ucontrol->value.integer.value[0] = (long )mixer->audigy2nx_leds[index___0];

  return (0);
}
}

static int snd_audigy2nx_led_put(struct snd_kcontrol *kcontrol , struct snd_ctl_elem_value *ucontrol )
{
  struct usb_mixer_interface *mixer ;
  int index___0 ;
  int value ;
  int err ;
  int changed ;
  unsigned int tmp ;

  {

  mixer = (struct usb_mixer_interface *)kcontrol->private_data;

  index___0 = (int )kcontrol->private_value;

  value = (int )ucontrol->value.integer.value[0];

  if (value > 1) {

    return (-22);
  }

  changed = value != (int )mixer->audigy2nx_leds[index___0];

  tmp = __create_pipe((mixer->chip)->dev, 0U);

  err = snd_usb_ctl_msg((mixer->chip)->dev, (unsigned int )(2 << 30) | tmp, (__u8 )36,
                        (__u8 )((2 << 5) | 3), (__u16 )value, (__u16 )(index___0 + 2),
                        (void *)0, (__u16 )0, 100);

  if (err < 0) {

    return (err);
  }

  mixer->audigy2nx_leds[index___0] = (u8 )value;

  return (changed);
}
}

static struct snd_kcontrol_new ( __attribute__((__noderef__, __address_space__(2))) snd_audigy2nx_controls)[3] = { {2,
      0U, 0U, (unsigned char *)"CMSS LED Switch", 0U, 0U, 0U, & snd_ctl_boolean_mono_info,
      & snd_audigy2nx_led_get, & snd_audigy2nx_led_put, {(snd_kcontrol_tlv_rw_t *)0},
      0UL},
        {2, 0U, 0U, (unsigned char *)"Power LED Switch", 0U, 0U, 0U, & snd_ctl_boolean_mono_info,
      & snd_audigy2nx_led_get, & snd_audigy2nx_led_put, {(snd_kcontrol_tlv_rw_t *)0},
      1UL},
        {2, 0U, 0U, (unsigned char *)"Dolby Digital LED Switch", 0U, 0U, 0U, & snd_ctl_boolean_mono_info,
      & snd_audigy2nx_led_get, & snd_audigy2nx_led_put, {(snd_kcontrol_tlv_rw_t *)0},
      2UL}};

static int snd_audigy2nx_controls_create(struct usb_mixer_interface *mixer )
{
  int i ;
  int err ;
  struct snd_kcontrol *tmp ;

  {

  i = 0;

  while ((unsigned long )i < sizeof(snd_audigy2nx_controls) / sizeof(snd_audigy2nx_controls[0]) + (sizeof(char [1]) - 1UL)) {

    if (i > 1) {

      if ((mixer->chip)->usb_id == (u32 )((1054 << 16) | 12352)) {

        break;
      }
    }

    tmp = snd_ctl_new1((struct snd_kcontrol_new const *)(& snd_audigy2nx_controls[i]),
                       (void *)mixer);

    err = snd_ctl_add((mixer->chip)->card, tmp);

    if (err < 0) {

      return (err);
    }

    i ++;
  }

  mixer->audigy2nx_leds[1] = (u8 )1;

  return (0);
}
}

static void snd_audigy2nx_proc_read(struct snd_info_entry *entry , struct snd_info_buffer *buffer ) ;

static struct sb_jack const jacks_audigy2nx[5] = { {4, "dig in "},
        {7, "line in"},
        {19, "spk out"},
        {20, "hph out"},
        {-1, (char const *)((void *)0)}};

static struct sb_jack const jacks_live24ext[4] = { {4, "line in"},
        {3, "hph out"},
        {0, "RC     "},
        {-1, (char const *)((void *)0)}};

static void snd_audigy2nx_proc_read(struct snd_info_entry *entry , struct snd_info_buffer *buffer )
{
  struct sb_jack const *jacks ;
  struct usb_mixer_interface *mixer ;
  int i ;
  int err ;
  u8 buf[3] ;
  unsigned int tmp ;

  {

  mixer = (struct usb_mixer_interface *)entry->private_data;

  snd_iprintf(buffer, (char *)"%s jacks\n\n", ((mixer->chip)->card)->shortname);

  if ((mixer->chip)->usb_id == (u32 )((1054 << 16) | 12320)) {

    jacks = jacks_audigy2nx;
  } else

  if ((mixer->chip)->usb_id == (u32 )((1054 << 16) | 12352)) {

    jacks = jacks_live24ext;
  } else {

    return;
  }

  i = 0;

  while ((jacks + i)->name) {

    snd_iprintf(buffer, (char *)"%s: ", (jacks + i)->name);

    tmp = __create_pipe((mixer->chip)->dev, 0U);

    err = snd_usb_ctl_msg((mixer->chip)->dev, ((unsigned int )(2 << 30) | tmp) | 128U,
                          (__u8 )133, (__u8 )((128 | (1 << 5)) | 1), (__u16 )0, (__u16 )((jacks + i)->unitid << 8),
                          (void *)(buf), (__u16 )3, 100);

    if (err == 3) {

      if ((int )buf[0] == 3) {

        snd_iprintf(buffer, (char *)"%02x %02x\n", (int )buf[1], (int )buf[2]);
      } else

      if ((int )buf[0] == 6) {

        snd_iprintf(buffer, (char *)"%02x %02x\n", (int )buf[1], (int )buf[2]);
      } else {

        snd_iprintf(buffer, (char *)"?\n");
      }
    } else {

      snd_iprintf(buffer, (char *)"?\n");
    }

    i ++;
  }

  return;
}
}

static struct snd_device_ops __attribute__((__noderef__, __address_space__(2))) dev_ops = {& snd_usb_mixer_dev_free,
    (int (*)(struct snd_device *dev ))0, (int (*)(struct snd_device *dev ))0};

int snd_usb_create_mixer(struct snd_usb_audio *chip , int ctrlif , int ignore_error )
{
  struct usb_mixer_interface *mixer ;
  int err ;
  void *tmp ;
  void *tmp___0 ;
  struct snd_info_entry *entry ;
  int tmp___1 ;

  {

  strcpy((chip->card)->mixername, "USB Mixer");

  tmp = kzalloc(sizeof(*mixer), 208U);

  mixer = (struct usb_mixer_interface *)tmp;

  if (! mixer) {

    return (-12);
  }

  mixer->chip = chip;

  mixer->ctrlif = (unsigned int )ctrlif;

  mixer->ignore_ctl_error = (unsigned int )ignore_error;

  tmp___0 = kcalloc((size_t )256, sizeof(*(mixer->id_elems)), 208U);

  mixer->id_elems = (struct usb_mixer_elem_info **)tmp___0;

  if (! mixer->id_elems) {

    kfree((void const *)mixer);

    return (-12);
  }

  err = snd_usb_mixer_controls(mixer);

  if (err < 0) {

    goto _error;
  } else {

    err = snd_usb_mixer_status_create(mixer);

    if (err < 0) {

      goto _error;
    }
  }

  err = snd_usb_soundblaster_remote_init(mixer);

  if (err < 0) {

    goto _error;
  }

  if ((mixer->chip)->usb_id == (u32 )((1054 << 16) | 12320)) {

    goto _L;
  } else

  if ((mixer->chip)->usb_id == (u32 )((1054 << 16) | 12352)) {
    _L:

    err = snd_audigy2nx_controls_create(mixer);

    if (err < 0) {

      goto _error;
    }

    tmp___1 = snd_card_proc_new(chip->card, (char const * __attribute__((__nullterm__)) )"audigy2nx",
                                & entry);

    if (! tmp___1) {

      snd_info_set_text_ops(entry, (void *)mixer, & snd_audigy2nx_proc_read);
    }
  }

  err = snd_device_new(chip->card, 8192, (void *)mixer, (struct snd_device_ops *)(& dev_ops));

  if (err < 0) {

    goto _error;
  }

  list_add(& mixer->list, & chip->mixer_list);

  return (0);
  _error:

  snd_usb_mixer_free(mixer);

  return (err);
}
}

void snd_usb_mixer_disconnect(struct list_head *p )
{
  struct usb_mixer_interface *mixer ;
  struct list_head const *__mptr ;

  {

  __mptr = (struct list_head const *)p;

  mixer = (struct usb_mixer_interface *)((char *)__mptr - (unsigned int )(& ((struct usb_mixer_interface *)0)->list));

  usb_kill_urb(mixer->urb);

  usb_kill_urb(mixer->rc_urb);

  return;
}
}
