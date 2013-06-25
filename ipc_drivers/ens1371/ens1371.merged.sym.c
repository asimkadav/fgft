#pragma merger(0,"./ens1371_annotated.i","-Wall,-Wundef,-Wstrict-prototypes,-Wno-trigraphs,-fno-strict-aliasing,-fno-common,-Werror-implicit-function-declaration,-Os,-m64,-mtune=generic,-mno-red-zone,-mcmodel=kernel,-funit-at-a-time,-maccumulate-outgoing-args,-pipe,-Wno-sign-compare,-fno-asynchronous-unwind-tables,-mno-sse,-mno-mmx,-mno-sse2,-mno-3dnow,-Wframe-larger-than=2048,-fno-stack-protector,-fno-omit-frame-pointer,-fno-optimize-sibling-calls,-g,-Wdeclaration-after-statement,-Wno-pointer-sign,-fwrapv,-fno-dwarf2-cfi-asm,-g,-Wall,-Wno-attributes,-Wno-unknown-pragmas")
# 1 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371//"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "./include/linux/autoconf.h" 1
# 1 "<command-line>" 2
# 1 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
# 29 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h" 1





# 1 "include/linux/compiler.h" 1
# 40 "include/linux/compiler.h"
# 1 "include/linux/compiler-gcc.h" 1
# 86 "include/linux/compiler-gcc.h"
# 1 "include/linux/compiler-gcc4.h" 1
# 86 "include/linux/compiler-gcc.h" 2
# 41 "include/linux/compiler.h" 2
# 58 "include/linux/compiler.h"
struct ftrace_branch_data {
 const char *func;
 const char *file;
 unsigned line;
 union {
  struct {
   unsigned long correct;
   unsigned long incorrect;
  };
  struct {
   unsigned long miss;
   unsigned long hit;
  };
 };
};
# 7 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h" 2
# 1 "include/asm-generic/int-ll64.h" 1
# 17 "include/asm-generic/int-ll64.h"
typedef __signed__ char __s8;
typedef unsigned char __u8;

typedef __signed__ short __s16;
typedef unsigned short __u16;

typedef __signed__ int __s32;
typedef unsigned int __u32;


__extension__ typedef __signed__ long long __s64;
__extension__ typedef unsigned long long __u64;
# 40 "include/asm-generic/int-ll64.h"
typedef signed char s8;
typedef unsigned char u8;

typedef signed short s16;
typedef unsigned short u16;

typedef signed int s32;
typedef unsigned int u32;

typedef signed long long s64;
typedef unsigned long long u64;
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h" 2
# 19 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h"
static inline __attribute__((always_inline)) unsigned char readb(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned char ret; asm volatile("mov" "b" " %1,%0":"=q" (ret) :"m" (*(volatile unsigned char *)addr) :"memory"); return ret; }
static inline __attribute__((always_inline)) unsigned short readw(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned short ret; asm volatile("mov" "w" " %1,%0":"=r" (ret) :"m" (*(volatile unsigned short *)addr) :"memory"); return ret; }
static inline __attribute__((always_inline)) unsigned int readl(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned int ret; asm volatile("mov" "l" " %1,%0":"=r" (ret) :"m" (*(volatile unsigned int *)addr) :"memory"); return ret; }

static inline __attribute__((always_inline)) unsigned char __readb(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned char ret; asm volatile("mov" "b" " %1,%0":"=q" (ret) :"m" (*(volatile unsigned char *)addr) ); return ret; }
static inline __attribute__((always_inline)) unsigned short __readw(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned short ret; asm volatile("mov" "w" " %1,%0":"=r" (ret) :"m" (*(volatile unsigned short *)addr) ); return ret; }
static inline __attribute__((always_inline)) unsigned int __readl(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned int ret; asm volatile("mov" "l" " %1,%0":"=r" (ret) :"m" (*(volatile unsigned int *)addr) ); return ret; }

static inline __attribute__((always_inline)) void writeb(unsigned char val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "b" " %0,%1": :"q" (val), "m" (*(volatile unsigned char *)addr) :"memory"); }
static inline __attribute__((always_inline)) void writew(unsigned short val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "w" " %0,%1": :"r" (val), "m" (*(volatile unsigned short *)addr) :"memory"); }
static inline __attribute__((always_inline)) void writel(unsigned int val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "l" " %0,%1": :"r" (val), "m" (*(volatile unsigned int *)addr) :"memory"); }

static inline __attribute__((always_inline)) void __writeb(unsigned char val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "b" " %0,%1": :"q" (val), "m" (*(volatile unsigned char *)addr) ); }
static inline __attribute__((always_inline)) void __writew(unsigned short val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "w" " %0,%1": :"r" (val), "m" (*(volatile unsigned short *)addr) ); }
static inline __attribute__((always_inline)) void __writel(unsigned int val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "l" " %0,%1": :"r" (val), "m" (*(volatile unsigned int *)addr) ); }
# 50 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h"
static inline __attribute__((always_inline)) unsigned long readq(const volatile void __attribute__((noderef, address_space(2))) *addr) { unsigned long ret; asm volatile("mov" "q" " %1,%0":"=r" (ret) :"m" (*(volatile unsigned long *)addr) :"memory"); return ret; }
static inline __attribute__((always_inline)) void writeq(unsigned long val, volatile void __attribute__((noderef, address_space(2))) *addr) { asm volatile("mov" "q" " %0,%1": :"r" (val), "m" (*(volatile unsigned long *)addr) :"memory"); }
# 86 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_64.h" 1
# 38 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_64.h"
extern void native_io_delay(void);

extern int io_delay_type;
extern void io_delay_init(void);





static inline __attribute__((always_inline)) void slow_down_io(void)
{
 native_io_delay();





}
# 118 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_64.h"
static inline __attribute__((always_inline)) unsigned char inb(unsigned short port) { unsigned char _v; asm volatile ("in" "b" " %" "w" "1,%" "" "0" : "=a" (_v) : "Nd" (port)); return _v; } static inline __attribute__((always_inline)) unsigned char inb_p(unsigned short port) { unsigned char _v; asm volatile ("in" "b" " %" "w" "1,%" "" "0" : "=a" (_v) : "Nd" (port)); slow_down_io(); return _v; }


static inline __attribute__((always_inline)) unsigned short inw(unsigned short port) { unsigned short _v; asm volatile ("in" "w" " %" "w" "1,%" "" "0" : "=a" (_v) : "Nd" (port)); return _v; } static inline __attribute__((always_inline)) unsigned short inw_p(unsigned short port) { unsigned short _v; asm volatile ("in" "w" " %" "w" "1,%" "" "0" : "=a" (_v) : "Nd" (port)); slow_down_io(); return _v; }


static inline __attribute__((always_inline)) unsigned int inl(unsigned short port) { unsigned int _v; asm volatile ("in" "l" " %" "w" "1,%" "" "0" : "=a" (_v) : "Nd" (port)); return _v; } static inline __attribute__((always_inline)) unsigned int inl_p(unsigned short port) { unsigned int _v; asm volatile ("in" "l" " %" "w" "1,%" "" "0" : "=a" (_v) : "Nd" (port)); slow_down_io(); return _v; }


static inline __attribute__((always_inline)) void outb(unsigned char value, unsigned short port) { asm volatile ("out" "b" " %" "b" "0,%" "w" "1" : : "a" (value), "Nd" (port)); } static inline __attribute__((always_inline)) void outb_p(unsigned char value, unsigned short port) { asm volatile ("out" "b" " %" "b" "0,%" "w" "1" : : "a" (value), "Nd" (port)); slow_down_io(); }
static inline __attribute__((always_inline)) void outw(unsigned short value, unsigned short port) { asm volatile ("out" "w" " %" "w" "0,%" "w" "1" : : "a" (value), "Nd" (port)); } static inline __attribute__((always_inline)) void outw_p(unsigned short value, unsigned short port) { asm volatile ("out" "w" " %" "w" "0,%" "w" "1" : : "a" (value), "Nd" (port)); slow_down_io(); }
static inline __attribute__((always_inline)) void outl(unsigned int value, unsigned short port) { asm volatile ("out" "l" " %" "0,%" "w" "1" : : "a" (value), "Nd" (port)); } static inline __attribute__((always_inline)) void outl_p(unsigned int value, unsigned short port) { asm volatile ("out" "l" " %" "0,%" "w" "1" : : "a" (value), "Nd" (port)); slow_down_io(); }

static inline __attribute__((always_inline)) void insb(unsigned short port, void *addr, unsigned long count) { asm volatile ("rep ; ins" "b" : "=D" (addr), "=c" (count) : "d" (port), "0" (addr), "1" (count)); }
static inline __attribute__((always_inline)) void insw(unsigned short port, void *addr, unsigned long count) { asm volatile ("rep ; ins" "w" : "=D" (addr), "=c" (count) : "d" (port), "0" (addr), "1" (count)); }
static inline __attribute__((always_inline)) void insl(unsigned short port, void *addr, unsigned long count) { asm volatile ("rep ; ins" "l" : "=D" (addr), "=c" (count) : "d" (port), "0" (addr), "1" (count)); }

static inline __attribute__((always_inline)) void outsb(unsigned short port, const void *addr, unsigned long count) { asm volatile ("rep ; outs" "b" : "=S" (addr), "=c" (count) : "d" (port), "0" (addr), "1" (count)); }
static inline __attribute__((always_inline)) void outsw(unsigned short port, const void *addr, unsigned long count) { asm volatile ("rep ; outs" "w" : "=S" (addr), "=c" (count) : "d" (port), "0" (addr), "1" (count)); }
static inline __attribute__((always_inline)) void outsl(unsigned short port, const void *addr, unsigned long count) { asm volatile ("rep ; outs" "l" : "=S" (addr), "=c" (count) : "d" (port), "0" (addr), "1" (count)); }





# 1 "include/linux/vmalloc.h" 1



# 1 "include/linux/spinlock.h" 1
# 49 "include/linux/spinlock.h"
# 1 "include/linux/typecheck.h" 1
# 50 "include/linux/spinlock.h" 2
# 1 "include/linux/preempt.h" 1
# 9 "include/linux/preempt.h"
# 1 "include/linux/thread_info.h" 1
# 10 "include/linux/thread_info.h"
# 1 "include/linux/types.h" 1
# 11 "include/linux/types.h"
# 1 "include/linux/posix_types.h" 1



# 1 "include/linux/stddef.h" 1
# 15 "include/linux/stddef.h"
enum {
 false = 0,
 true = 1
};
# 5 "include/linux/posix_types.h" 2
# 36 "include/linux/posix_types.h"
typedef struct {
 unsigned long fds_bits [(1024/(8 * sizeof(unsigned long)))];
} __kernel_fd_set;


typedef void (*__kernel_sighandler_t)(int);


typedef int __kernel_key_t;
typedef int __kernel_mqd_t;

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/posix_types.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/posix_types_64.h" 1
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/posix_types_64.h"
typedef unsigned long __kernel_ino_t;
typedef unsigned int __kernel_mode_t;
typedef unsigned long __kernel_nlink_t;
typedef long __kernel_off_t;
typedef int __kernel_pid_t;
typedef int __kernel_ipc_pid_t;
typedef unsigned int __kernel_uid_t;
typedef unsigned int __kernel_gid_t;
typedef unsigned long __kernel_size_t;
typedef long __kernel_ssize_t;
typedef long __kernel_ptrdiff_t;
typedef long __kernel_time_t;
typedef long __kernel_suseconds_t;
typedef long __kernel_clock_t;
typedef int __kernel_timer_t;
typedef int __kernel_clockid_t;
typedef int __kernel_daddr_t;
typedef char * __kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;


typedef long long __kernel_loff_t;


typedef struct {
 int val[2];
} __kernel_fsid_t;

typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;
typedef __kernel_uid_t __kernel_uid32_t;
typedef __kernel_gid_t __kernel_gid32_t;

typedef unsigned long __kernel_old_dev_t;




static inline __attribute__((always_inline)) void __FD_SET(unsigned long fd, __kernel_fd_set *fdsetp)
{
 unsigned long _tmp = fd / (8 * sizeof(unsigned long));
 unsigned long _rem = fd % (8 * sizeof(unsigned long));
 fdsetp->fds_bits[_tmp] |= (1UL<<_rem);
}


static inline __attribute__((always_inline)) void __FD_CLR(unsigned long fd, __kernel_fd_set *fdsetp)
{
 unsigned long _tmp = fd / (8 * sizeof(unsigned long));
 unsigned long _rem = fd % (8 * sizeof(unsigned long));
 fdsetp->fds_bits[_tmp] &= ~(1UL<<_rem);
}


static inline __attribute__((always_inline)) int __FD_ISSET(unsigned long fd, __const__ __kernel_fd_set *p)
{
 unsigned long _tmp = fd / (8 * sizeof(unsigned long));
 unsigned long _rem = fd % (8 * sizeof(unsigned long));
 return (p->fds_bits[_tmp] & (1UL<<_rem)) != 0;
}






static inline __attribute__((always_inline)) void __FD_ZERO(__kernel_fd_set *p)
{
 unsigned long *tmp = p->fds_bits;
 int i;

 if (__builtin_constant_p((1024/(8 * sizeof(unsigned long))))) {
  switch ((1024/(8 * sizeof(unsigned long)))) {
  case 32:
   tmp[ 0] = 0; tmp[ 1] = 0; tmp[ 2] = 0; tmp[ 3] = 0;
   tmp[ 4] = 0; tmp[ 5] = 0; tmp[ 6] = 0; tmp[ 7] = 0;
   tmp[ 8] = 0; tmp[ 9] = 0; tmp[10] = 0; tmp[11] = 0;
   tmp[12] = 0; tmp[13] = 0; tmp[14] = 0; tmp[15] = 0;
   tmp[16] = 0; tmp[17] = 0; tmp[18] = 0; tmp[19] = 0;
   tmp[20] = 0; tmp[21] = 0; tmp[22] = 0; tmp[23] = 0;
   tmp[24] = 0; tmp[25] = 0; tmp[26] = 0; tmp[27] = 0;
   tmp[28] = 0; tmp[29] = 0; tmp[30] = 0; tmp[31] = 0;
   return;
  case 16:
   tmp[ 0] = 0; tmp[ 1] = 0; tmp[ 2] = 0; tmp[ 3] = 0;
   tmp[ 4] = 0; tmp[ 5] = 0; tmp[ 6] = 0; tmp[ 7] = 0;
   tmp[ 8] = 0; tmp[ 9] = 0; tmp[10] = 0; tmp[11] = 0;
   tmp[12] = 0; tmp[13] = 0; tmp[14] = 0; tmp[15] = 0;
   return;
  case 8:
   tmp[ 0] = 0; tmp[ 1] = 0; tmp[ 2] = 0; tmp[ 3] = 0;
   tmp[ 4] = 0; tmp[ 5] = 0; tmp[ 6] = 0; tmp[ 7] = 0;
   return;
  case 4:
   tmp[ 0] = 0; tmp[ 1] = 0; tmp[ 2] = 0; tmp[ 3] = 0;
   return;
  }
 }
 i = (1024/(8 * sizeof(unsigned long)));
 while (i) {
  i--;
  *tmp = 0;
  tmp++;
 }
}
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/posix_types.h" 2
# 48 "include/linux/posix_types.h" 2
# 12 "include/linux/types.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/types.h" 1







typedef unsigned short umode_t;
# 25 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/types.h"
typedef u64 dma64_addr_t;


typedef u64 dma_addr_t;
# 13 "include/linux/types.h" 2



typedef __u32 __kernel_dev_t;

typedef __kernel_fd_set fd_set;
typedef __kernel_dev_t dev_t;
typedef __kernel_ino_t ino_t;
typedef __kernel_mode_t mode_t;
typedef __kernel_nlink_t nlink_t;
typedef __kernel_off_t off_t;
typedef __kernel_pid_t pid_t;
typedef __kernel_daddr_t daddr_t;
typedef __kernel_key_t key_t;
typedef __kernel_suseconds_t suseconds_t;
typedef __kernel_timer_t timer_t;
typedef __kernel_clockid_t clockid_t;
typedef __kernel_mqd_t mqd_t;


typedef _Bool bool;

typedef __kernel_uid32_t uid_t;
typedef __kernel_gid32_t gid_t;
typedef __kernel_uid16_t uid16_t;
typedef __kernel_gid16_t gid16_t;

typedef unsigned long uintptr_t;



typedef __kernel_old_uid_t old_uid_t;
typedef __kernel_old_gid_t old_gid_t;
# 57 "include/linux/types.h"
typedef __kernel_loff_t loff_t;
# 66 "include/linux/types.h"
typedef __kernel_size_t size_t;




typedef __kernel_ssize_t ssize_t;




typedef __kernel_ptrdiff_t ptrdiff_t;




typedef __kernel_time_t time_t;




typedef __kernel_clock_t clock_t;




typedef __kernel_caddr_t caddr_t;



typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;


typedef unsigned char unchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;




typedef __u8 u_int8_t;
typedef __s8 int8_t;
typedef __u16 u_int16_t;
typedef __s16 int16_t;
typedef __u32 u_int32_t;
typedef __s32 int32_t;



typedef __u8 uint8_t;
typedef __u16 uint16_t;
typedef __u32 uint32_t;


typedef __u64 uint64_t;
typedef __u64 u_int64_t;
typedef __s64 int64_t;
# 145 "include/linux/types.h"
typedef unsigned long sector_t;
typedef unsigned long blkcnt_t;
# 175 "include/linux/types.h"
typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;
typedef __u64 __le64;
typedef __u64 __be64;

typedef __u16 __sum16;
typedef __u32 __wsum;


typedef unsigned gfp_t;
typedef unsigned fmode_t;


typedef u64 phys_addr_t;




typedef phys_addr_t resource_size_t;

typedef struct {
 volatile int counter;
} atomic_t;


typedef struct {
 volatile long counter;
} atomic64_t;


struct ustat {
 __kernel_daddr_t f_tfree;
 __kernel_ino_t f_tinode;
 char f_fname[6];
 char f_fpack[6];
};
# 11 "include/linux/thread_info.h" 2

struct timespec;
struct compat_timespec;




struct restart_block {
 long (*fn)(struct restart_block *);
 union {
  struct {
   unsigned long arg0, arg1, arg2, arg3;
  };

  struct {
   u32 *uaddr;
   u32 val;
   u32 flags;
   u32 bitset;
   u64 time;
  } futex;

  struct {
   clockid_t index;
   struct timespec __attribute__((noderef, address_space(1))) *rmtp;

   struct compat_timespec __attribute__((noderef, address_space(1))) *compat_rmtp;

   u64 expires;
  } nanosleep;

  struct {
   struct pollfd __attribute__((noderef, address_space(1))) *ufds;
   int nfds;
   int has_timeout;
   unsigned long tv_sec;
   unsigned long tv_nsec;
  } poll;
 };
};

extern long do_no_restart_syscall(struct restart_block *parm);

# 1 "include/linux/bitops.h" 1
# 17 "include/linux/bitops.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 1
# 16 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h" 1





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/asm.h" 1
# 7 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h" 2
# 42 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cpufeature.h" 1






# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/required-features.h" 1
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cpufeature.h" 2
# 167 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cpufeature.h"
# 1 "include/linux/bitops.h" 1
# 168 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cpufeature.h" 2

extern const char * const x86_cap_flags[9*32];
extern const char * const x86_power_flags[32];
# 43 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h" 2

struct alt_instr {
 u8 *instr;
 u8 *replacement;
 u8 cpuid;
 u8 instrlen;
 u8 replacementlen;
 u8 pad1;

 u32 pad2;

};

extern void alternative_instructions(void);
extern void apply_alternatives(struct alt_instr *start, struct alt_instr *end);

struct module;


extern void alternatives_smp_module_add(struct module *mod, char *name,
     void *locks, void *locks_end,
     void *text, void *text_end);
extern void alternatives_smp_module_del(struct module *mod);
extern void alternatives_smp_switch(int smp);
# 75 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h"
const unsigned char *const *find_nop_table(void);
# 148 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h"
struct paravirt_patch_site;




static inline __attribute__((always_inline)) void apply_paravirt(struct paravirt_patch_site *start,
      struct paravirt_patch_site *end)
{}




extern void add_nops(void *insns, unsigned int len);
# 180 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/alternative.h"
extern void *text_poke(void *addr, const void *opcode, size_t len);
extern void *text_poke_early(void *addr, const void *opcode, size_t len);
# 17 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 2
# 59 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) void
set_bit(unsigned int nr, volatile unsigned long *addr)
{
 if ((__builtin_constant_p(nr))) {
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "orb %1,%0"
   : "+m" (*(volatile long *) ((void *)(addr) + ((nr)>>3)))
   : "iq" ((u8)(1 << ((nr) & 7)))
   : "memory");
 } else {
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "bts %1,%0"
   : "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");
 }
}
# 82 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) void __set_bit(int nr, volatile unsigned long *addr)
{
 asm volatile("bts %1,%0" : "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");
}
# 97 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) void
clear_bit(int nr, volatile unsigned long *addr)
{
 if ((__builtin_constant_p(nr))) {
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "andb %1,%0"
   : "+m" (*(volatile long *) ((void *)(addr) + ((nr)>>3)))
   : "iq" ((u8)~(1 << ((nr) & 7))));
 } else {
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "btr %1,%0"
   : "+m" (*(volatile long *) (addr))
   : "Ir" (nr));
 }
}
# 119 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) void clear_bit_unlock(unsigned nr, volatile unsigned long *addr)
{
 __asm__ __volatile__("": : :"memory");
 clear_bit(nr, addr);
}

static inline __attribute__((always_inline)) void __clear_bit(int nr, volatile unsigned long *addr)
{
 asm volatile("btr %1,%0" : "+m" (*(volatile long *) (addr)) : "Ir" (nr));
}
# 142 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) void __clear_bit_unlock(unsigned nr, volatile unsigned long *addr)
{
 __asm__ __volatile__("": : :"memory");
 __clear_bit(nr, addr);
}
# 160 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) void __change_bit(int nr, volatile unsigned long *addr)
{
 asm volatile("btc %1,%0" : "+m" (*(volatile long *) (addr)) : "Ir" (nr));
}
# 174 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) void change_bit(int nr, volatile unsigned long *addr)
{
 if ((__builtin_constant_p(nr))) {
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "xorb %1,%0"
   : "+m" (*(volatile long *) ((void *)(addr) + ((nr)>>3)))
   : "iq" ((u8)(1 << ((nr) & 7))));
 } else {
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "btc %1,%0"
   : "+m" (*(volatile long *) (addr))
   : "Ir" (nr));
 }
}
# 195 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int test_and_set_bit(int nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "bts %2,%1\n\t"
       "sbb %0,%0" : "=r" (oldbit), "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");

 return oldbit;
}
# 212 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) int
test_and_set_bit_lock(int nr, volatile unsigned long *addr)
{
 return test_and_set_bit(nr, addr);
}
# 227 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int __test_and_set_bit(int nr, volatile unsigned long *addr)
{
 int oldbit;

 asm("bts %2,%1\n\t"
     "sbb %0,%0"
     : "=r" (oldbit), "+m" (*(volatile long *) (addr))
     : "Ir" (nr));
 return oldbit;
}
# 246 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int test_and_clear_bit(int nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "btr %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit), "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");

 return oldbit;
}
# 266 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int __test_and_clear_bit(int nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile("btr %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit), "+m" (*(volatile long *) (addr))
       : "Ir" (nr));
 return oldbit;
}


static inline __attribute__((always_inline)) int __test_and_change_bit(int nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile("btc %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit), "+m" (*(volatile long *) (addr))
       : "Ir" (nr) : "memory");

 return oldbit;
}
# 298 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int test_and_change_bit(int nr, volatile unsigned long *addr)
{
 int oldbit;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "btc %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit), "+m" (*(volatile long *) (addr)) : "Ir" (nr) : "memory");

 return oldbit;
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) int constant_test_bit(unsigned int nr, const volatile unsigned long *addr)
{
 return ((1UL << (nr % 64)) &
  (((unsigned long *)addr)[nr / 64])) != 0;
}

static inline __attribute__((always_inline)) int variable_test_bit(int nr, volatile const unsigned long *addr)
{
 int oldbit;

 asm volatile("bt %2,%1\n\t"
       "sbb %0,%0"
       : "=r" (oldbit)
       : "m" (*(unsigned long *)addr), "Ir" (nr));

 return oldbit;
}
# 347 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) unsigned long __ffs(unsigned long word)
{
 asm("bsf %1,%0"
  : "=r" (word)
  : "rm" (word));
 return word;
}







static inline __attribute__((always_inline)) unsigned long ffz(unsigned long word)
{
 asm("bsf %1,%0"
  : "=r" (word)
  : "r" (~word));
 return word;
}







static inline __attribute__((always_inline)) unsigned long __fls(unsigned long word)
{
 asm("bsr %1,%0"
     : "=r" (word)
     : "rm" (word));
 return word;
}
# 395 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int ffs(int x)
{
 int r;

 asm("bsfl %1,%0\n\t"
     "cmovzl %2,%0"
     : "=r" (r) : "rm" (x), "r" (-1));






 return r + 1;
}
# 422 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h"
static inline __attribute__((always_inline)) int fls(int x)
{
 int r;

 asm("bsrl %1,%0\n\t"
     "cmovzl %2,%0"
     : "=&r" (r) : "rm" (x), "rm" (-1));






 return r + 1;
}






# 1 "include/asm-generic/bitops/sched.h" 1
# 12 "include/asm-generic/bitops/sched.h"
static inline __attribute__((always_inline)) int sched_find_first_bit(const unsigned long *b)
{

 if (b[0])
  return __ffs(b[0]);
 return __ffs(b[1]) + 64;
# 29 "include/asm-generic/bitops/sched.h"
}
# 444 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 2



# 1 "include/asm-generic/bitops/hweight.h" 1





extern unsigned int hweight32(unsigned int w);
extern unsigned int hweight16(unsigned int w);
extern unsigned int hweight8(unsigned int w);
extern unsigned long hweight64(__u64 w);
# 448 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 2



# 1 "include/asm-generic/bitops/fls64.h" 1
# 26 "include/asm-generic/bitops/fls64.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) int fls64(__u64 x)
{
 if (x == 0)
  return 0;
 return __fls(x) + 1;
}
# 452 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 2



# 1 "include/asm-generic/bitops/ext2-non-atomic.h" 1



# 1 "include/asm-generic/bitops/le.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/byteorder.h" 1



# 1 "include/linux/byteorder/little_endian.h" 1
# 12 "include/linux/byteorder/little_endian.h"
# 1 "include/linux/swab.h" 1





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/swab.h" 1






static inline __attribute__((always_inline)) __attribute__((__const__)) __u32 __arch_swab32(__u32 val)
{
# 21 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/swab.h"
 asm("bswapl %0"
     : "=r" (val)
     : "0" (val));

 return val;
}


static inline __attribute__((always_inline)) __attribute__((__const__)) __u64 __arch_swab64(__u64 val)
{
# 53 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/swab.h"
 asm("bswapq %0"
     : "=r" (val)
     : "0" (val));
 return val;

}
# 7 "include/linux/swab.h" 2
# 46 "include/linux/swab.h"
static inline __attribute__((always_inline)) __attribute__((__const__)) __u16 __fswab16(__u16 val)
{



 return ((__u16)( (((__u16)(val) & (__u16)0x00ffU) << 8) | (((__u16)(val) & (__u16)0xff00U) >> 8)));

}

static inline __attribute__((always_inline)) __attribute__((__const__)) __u32 __fswab32(__u32 val)
{

 return __arch_swab32(val);



}

static inline __attribute__((always_inline)) __attribute__((__const__)) __u64 __fswab64(__u64 val)
{

 return __arch_swab64(val);







}

static inline __attribute__((always_inline)) __attribute__((__const__)) __u32 __fswahw32(__u32 val)
{



 return ((__u32)( (((__u32)(val) & (__u32)0x0000ffffUL) << 16) | (((__u32)(val) & (__u32)0xffff0000UL) >> 16)));

}

static inline __attribute__((always_inline)) __attribute__((__const__)) __u32 __fswahb32(__u32 val)
{



 return ((__u32)( (((__u32)(val) & (__u32)0x00ff00ffUL) << 8) | (((__u32)(val) & (__u32)0xff00ff00UL) >> 8)));

}
# 148 "include/linux/swab.h"
static inline __attribute__((always_inline)) __u16 __swab16p(const __u16 *p)
{



 return (__builtin_constant_p((__u16)(*p)) ? ((__u16)( (((__u16)(*p) & (__u16)0x00ffU) << 8) | (((__u16)(*p) & (__u16)0xff00U) >> 8))) : __fswab16(*p));

}





static inline __attribute__((always_inline)) __u32 __swab32p(const __u32 *p)
{



 return (__builtin_constant_p((__u32)(*p)) ? ((__u32)( (((__u32)(*p) & (__u32)0x000000ffUL) << 24) | (((__u32)(*p) & (__u32)0x0000ff00UL) << 8) | (((__u32)(*p) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(*p) & (__u32)0xff000000UL) >> 24))) : __fswab32(*p));

}





static inline __attribute__((always_inline)) __u64 __swab64p(const __u64 *p)
{



 return (__builtin_constant_p((__u64)(*p)) ? ((__u64)( (((__u64)(*p) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(*p) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(*p) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(*p) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(*p) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(*p) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(*p) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(*p) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(*p));

}







static inline __attribute__((always_inline)) __u32 __swahw32p(const __u32 *p)
{



 return (__builtin_constant_p((__u32)(*p)) ? ((__u32)( (((__u32)(*p) & (__u32)0x0000ffffUL) << 16) | (((__u32)(*p) & (__u32)0xffff0000UL) >> 16))) : __fswahw32(*p));

}







static inline __attribute__((always_inline)) __u32 __swahb32p(const __u32 *p)
{



 return (__builtin_constant_p((__u32)(*p)) ? ((__u32)( (((__u32)(*p) & (__u32)0x00ff00ffUL) << 8) | (((__u32)(*p) & (__u32)0xff00ff00UL) >> 8))) : __fswahb32(*p));

}





static inline __attribute__((always_inline)) void __swab16s(__u16 *p)
{



 *p = __swab16p(p);

}




static inline __attribute__((always_inline)) void __swab32s(__u32 *p)
{



 *p = __swab32p(p);

}





static inline __attribute__((always_inline)) void __swab64s(__u64 *p)
{



 *p = __swab64p(p);

}







static inline __attribute__((always_inline)) void __swahw32s(__u32 *p)
{



 *p = __swahw32p(p);

}







static inline __attribute__((always_inline)) void __swahb32s(__u32 *p)
{



 *p = __swahb32p(p);

}
# 13 "include/linux/byteorder/little_endian.h" 2
# 43 "include/linux/byteorder/little_endian.h"
static inline __attribute__((always_inline)) __le64 __cpu_to_le64p(const __u64 *p)
{
 return ( __le64)*p;
}
static inline __attribute__((always_inline)) __u64 __le64_to_cpup(const __le64 *p)
{
 return ( __u64)*p;
}
static inline __attribute__((always_inline)) __le32 __cpu_to_le32p(const __u32 *p)
{
 return ( __le32)*p;
}
static inline __attribute__((always_inline)) __u32 __le32_to_cpup(const __le32 *p)
{
 return ( __u32)*p;
}
static inline __attribute__((always_inline)) __le16 __cpu_to_le16p(const __u16 *p)
{
 return ( __le16)*p;
}
static inline __attribute__((always_inline)) __u16 __le16_to_cpup(const __le16 *p)
{
 return ( __u16)*p;
}
static inline __attribute__((always_inline)) __be64 __cpu_to_be64p(const __u64 *p)
{
 return ( __be64)__swab64p(p);
}
static inline __attribute__((always_inline)) __u64 __be64_to_cpup(const __be64 *p)
{
 return __swab64p((__u64 *)p);
}
static inline __attribute__((always_inline)) __be32 __cpu_to_be32p(const __u32 *p)
{
 return ( __be32)__swab32p(p);
}
static inline __attribute__((always_inline)) __u32 __be32_to_cpup(const __be32 *p)
{
 return __swab32p((__u32 *)p);
}
static inline __attribute__((always_inline)) __be16 __cpu_to_be16p(const __u16 *p)
{
 return ( __be16)__swab16p(p);
}
static inline __attribute__((always_inline)) __u16 __be16_to_cpup(const __be16 *p)
{
 return __swab16p((__u16 *)p);
}
# 105 "include/linux/byteorder/little_endian.h"
# 1 "include/linux/byteorder/generic.h" 1
# 143 "include/linux/byteorder/generic.h"
static inline __attribute__((always_inline)) void le16_add_cpu(__le16 *var, u16 val)
{
 *var = (( __le16)(__u16)((( __u16)(__le16)(*var)) + val));
}

static inline __attribute__((always_inline)) void le32_add_cpu(__le32 *var, u32 val)
{
 *var = (( __le32)(__u32)((( __u32)(__le32)(*var)) + val));
}

static inline __attribute__((always_inline)) void le64_add_cpu(__le64 *var, u64 val)
{
 *var = (( __le64)(__u64)((( __u64)(__le64)(*var)) + val));
}

static inline __attribute__((always_inline)) void be16_add_cpu(__be16 *var, u16 val)
{
 *var = (( __be16)(__builtin_constant_p((__u16)(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val))) ? ((__u16)( (((__u16)(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val)) & (__u16)0x00ffU) << 8) | (((__u16)(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val)) & (__u16)0xff00U) >> 8))) : __fswab16(((__builtin_constant_p((__u16)(( __u16)(__be16)(*var))) ? ((__u16)( (((__u16)(( __u16)(__be16)(*var)) & (__u16)0x00ffU) << 8) | (((__u16)(( __u16)(__be16)(*var)) & (__u16)0xff00U) >> 8))) : __fswab16(( __u16)(__be16)(*var))) + val))));
}

static inline __attribute__((always_inline)) void be32_add_cpu(__be32 *var, u32 val)
{
 *var = (( __be32)(__builtin_constant_p((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val))) ? ((__u32)( (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0x000000ffUL) << 24) | (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val)) & (__u32)0xff000000UL) >> 24))) : __fswab32(((__builtin_constant_p((__u32)(( __u32)(__be32)(*var))) ? ((__u32)( (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x000000ffUL) << 24) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x0000ff00UL) << 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0x00ff0000UL) >> 8) | (((__u32)(( __u32)(__be32)(*var)) & (__u32)0xff000000UL) >> 24))) : __fswab32(( __u32)(__be32)(*var))) + val))));
}

static inline __attribute__((always_inline)) void be64_add_cpu(__be64 *var, u64 val)
{
 *var = (( __be64)(__builtin_constant_p((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val))) ? ((__u64)( (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(((__builtin_constant_p((__u64)(( __u64)(__be64)(*var))) ? ((__u64)( (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000000000ffULL) << 56) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000000000ff00ULL) << 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000000000ff0000ULL) << 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00000000ff000000ULL) << 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x000000ff00000000ULL) >> 8) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x0000ff0000000000ULL) >> 24) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0x00ff000000000000ULL) >> 40) | (((__u64)(( __u64)(__be64)(*var)) & (__u64)0xff00000000000000ULL) >> 56))) : __fswab64(( __u64)(__be64)(*var))) + val))));
}
# 106 "include/linux/byteorder/little_endian.h" 2
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/byteorder.h" 2
# 6 "include/asm-generic/bitops/le.h" 2
# 5 "include/asm-generic/bitops/ext2-non-atomic.h" 2
# 456 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 2






# 1 "include/asm-generic/bitops/minix.h" 1
# 463 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bitops.h" 2
# 18 "include/linux/bitops.h" 2







static __inline__ __attribute__((always_inline)) int get_bitmask_order(unsigned int count)
{
 int order;

 order = fls(count);
 return order;
}

static __inline__ __attribute__((always_inline)) int get_count_order(unsigned int count)
{
 int order;

 order = fls(count) - 1;
 if (count & (count - 1))
  order++;
 return order;
}

static inline __attribute__((always_inline)) unsigned long hweight_long(unsigned long w)
{
 return sizeof(w) == 4 ? hweight32(w) : hweight64(w);
}






static inline __attribute__((always_inline)) __u32 rol32(__u32 word, unsigned int shift)
{
 return (word << shift) | (word >> (32 - shift));
}






static inline __attribute__((always_inline)) __u32 ror32(__u32 word, unsigned int shift)
{
 return (word >> shift) | (word << (32 - shift));
}






static inline __attribute__((always_inline)) __u16 rol16(__u16 word, unsigned int shift)
{
 return (word << shift) | (word >> (16 - shift));
}






static inline __attribute__((always_inline)) __u16 ror16(__u16 word, unsigned int shift)
{
 return (word >> shift) | (word << (16 - shift));
}






static inline __attribute__((always_inline)) __u8 rol8(__u8 word, unsigned int shift)
{
 return (word << shift) | (word >> (8 - shift));
}






static inline __attribute__((always_inline)) __u8 ror8(__u8 word, unsigned int shift)
{
 return (word >> shift) | (word << (8 - shift));
}

static inline __attribute__((always_inline)) unsigned fls_long(unsigned long l)
{
 if (sizeof(l) == 4)
  return fls(l);
 return fls64(l);
}
# 125 "include/linux/bitops.h"
extern unsigned long find_first_bit(const unsigned long *addr,
        unsigned long size);
# 135 "include/linux/bitops.h"
extern unsigned long find_first_zero_bit(const unsigned long *addr,
      unsigned long size);
# 147 "include/linux/bitops.h"
extern unsigned long find_last_bit(const unsigned long *addr,
       unsigned long size);
# 159 "include/linux/bitops.h"
extern unsigned long find_next_bit(const unsigned long *addr,
       unsigned long size, unsigned long offset);
# 169 "include/linux/bitops.h"
extern unsigned long find_next_zero_bit(const unsigned long *addr,
     unsigned long size,
     unsigned long offset);
# 55 "include/linux/thread_info.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h" 1
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h" 1



# 1 "include/linux/const.h" 1
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h" 2
# 42 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page_64.h" 1
# 64 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page_64.h"
void clear_page(void *page);
void copy_page(void *to, void *from);


extern unsigned long max_pfn;
extern unsigned long phys_base;

extern unsigned long __phys_addr(unsigned long);





typedef unsigned long pteval_t;
typedef unsigned long pmdval_t;
typedef unsigned long pudval_t;
typedef unsigned long pgdval_t;
typedef unsigned long pgprotval_t;

typedef struct page *pgtable_t;

typedef struct { pteval_t pte; } pte_t;



extern unsigned long init_memory_mapping(unsigned long start,
      unsigned long end);

extern void initmem_init(unsigned long start_pfn, unsigned long end_pfn);
extern void free_initmem(void);

extern void init_extra_mapping_uc(unsigned long phys, unsigned long size);
extern void init_extra_mapping_wb(unsigned long phys, unsigned long size);
# 43 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h" 2
# 56 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h"
typedef struct { pgdval_t pgd; } pgd_t;
typedef struct { pgprotval_t pgprot; } pgprot_t;

extern int page_is_ram(unsigned long pagenr);
extern int devmem_is_allowed(unsigned long pagenr);
extern void map_devmem(unsigned long pfn, unsigned long size,
         pgprot_t vma_prot);
extern void unmap_devmem(unsigned long pfn, unsigned long size,
    pgprot_t vma_prot);

extern unsigned long max_low_pfn_mapped;
extern unsigned long max_pfn_mapped;

struct page;

static inline __attribute__((always_inline)) void clear_user_page(void *page, unsigned long vaddr,
    struct page *pg)
{
 clear_page(page);
}

static inline __attribute__((always_inline)) void copy_user_page(void *to, void *from, unsigned long vaddr,
    struct page *topage)
{
 copy_page(to, from);
}





static inline __attribute__((always_inline)) pgd_t native_make_pgd(pgdval_t val)
{
 return (pgd_t) { val };
}

static inline __attribute__((always_inline)) pgdval_t native_pgd_val(pgd_t pgd)
{
 return pgd.pgd;
}



typedef struct { pudval_t pud; } pud_t;

static inline __attribute__((always_inline)) pud_t native_make_pud(pmdval_t val)
{
 return (pud_t) { val };
}

static inline __attribute__((always_inline)) pudval_t native_pud_val(pud_t pud)
{
 return pud.pud;
}
# 119 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h"
typedef struct { pmdval_t pmd; } pmd_t;

static inline __attribute__((always_inline)) pmd_t native_make_pmd(pmdval_t val)
{
 return (pmd_t) { val };
}

static inline __attribute__((always_inline)) pmdval_t native_pmd_val(pmd_t pmd)
{
 return pmd.pmd;
}
# 139 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h"
static inline __attribute__((always_inline)) pte_t native_make_pte(pteval_t val)
{
 return (pte_t) { .pte = val };
}

static inline __attribute__((always_inline)) pteval_t native_pte_val(pte_t pte)
{
 return pte.pte;
}

static inline __attribute__((always_inline)) pteval_t native_pte_flags(pte_t pte)
{
 return native_pte_val(pte) & (~((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))));
}
# 197 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h"
extern bool __virt_addr_valid(unsigned long kaddr);




# 1 "include/asm-generic/memory_model.h" 1
# 203 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h" 2
# 1 "include/asm-generic/page.h" 1
# 9 "include/asm-generic/page.h"
static __inline__ __attribute__((always_inline)) __attribute__((__const__)) int get_order(unsigned long size)
{
 int order;

 size = (size - 1) >> (12 - 1);
 order = -1;
 do {
  size >>= 1;
  order++;
 } while (size);
 return order;
}
# 204 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/page.h" 2
# 12 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h" 2
# 20 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h"
struct task_struct;
struct exec_domain;
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor-flags.h" 1
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2


struct task_struct;
struct mm_struct;

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h" 1
# 62 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h"
struct vm86_regs {



 long ebx;
 long ecx;
 long edx;
 long esi;
 long edi;
 long ebp;
 long eax;
 long __null_ds;
 long __null_es;
 long __null_fs;
 long __null_gs;
 long orig_eax;
 long eip;
 unsigned short cs, __csh;
 long eflags;
 long esp;
 unsigned short ss, __ssh;



 unsigned short es, __esh;
 unsigned short ds, __dsh;
 unsigned short fs, __fsh;
 unsigned short gs, __gsh;
};

struct revectored_struct {
 unsigned long __map[8];
};

struct vm86_struct {
 struct vm86_regs regs;
 unsigned long flags;
 unsigned long screen_bitmap;
 unsigned long cpu_type;
 struct revectored_struct int_revectored;
 struct revectored_struct int21_revectored;
};






struct vm86plus_info_struct {
 unsigned long force_return_for_pic:1;
 unsigned long vm86dbg_active:1;
 unsigned long vm86dbg_TFpendig:1;
 unsigned long unused:28;
 unsigned long is_vm86pus:1;
 unsigned char vm86dbg_intxxtab[32];
};
struct vm86plus_struct {
 struct vm86_regs regs;
 unsigned long flags;
 unsigned long screen_bitmap;
 unsigned long cpu_type;
 struct revectored_struct int_revectored;
 struct revectored_struct int21_revectored;
 struct vm86plus_info_struct vm86plus;
};



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace-abi.h" 1
# 91 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace-abi.h"
struct ptrace_bts_config {

 __u32 size;

 __u32 flags;

 __u32 signal;

 __u32 bts_size;
};
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace.h" 2



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/segment.h" 1
# 139 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/segment.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cache.h" 1
# 140 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/segment.h" 2
# 205 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/segment.h"
extern const char early_idt_handlers[32][10];
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace.h" 2
# 98 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace.h"
struct pt_regs {
 unsigned long r15;
 unsigned long r14;
 unsigned long r13;
 unsigned long r12;
 unsigned long bp;
 unsigned long bx;

 unsigned long r11;
 unsigned long r10;
 unsigned long r9;
 unsigned long r8;
 unsigned long ax;
 unsigned long cx;
 unsigned long dx;
 unsigned long si;
 unsigned long di;
 unsigned long orig_ax;


 unsigned long ip;
 unsigned long cs;
 unsigned long flags;
 unsigned long sp;
 unsigned long ss;

};







# 1 "include/linux/init.h" 1
# 139 "include/linux/init.h"
typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);

extern initcall_t __con_initcall_start[], __con_initcall_end[];
extern initcall_t __security_initcall_start[], __security_initcall_end[];


extern int do_one_initcall(initcall_t fn);
extern char __attribute__ ((__section__(".init.data"))) boot_command_line[];
extern char *saved_command_line;
extern unsigned int reset_devices;


void setup_arch(char **);
void prepare_namespace(void);

extern void (*late_time_init)(void);
# 133 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace.h" 2

struct cpuinfo_x86;
struct task_struct;

extern unsigned long profile_pc(struct pt_regs *regs);

extern unsigned long
convert_ip_to_linear(struct task_struct *child, struct pt_regs *regs);
extern void send_sigtrap(struct task_struct *tsk, struct pt_regs *regs,
    int error_code, int si_code);
void signal_fault(struct pt_regs *regs, void __attribute__((noderef, address_space(1))) *frame, char *where);

extern long syscall_trace_enter(struct pt_regs *);
extern void syscall_trace_leave(struct pt_regs *);

static inline __attribute__((always_inline)) unsigned long regs_return_value(struct pt_regs *regs)
{
 return regs->ax;
}
# 160 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ptrace.h"
static inline __attribute__((always_inline)) int user_mode(struct pt_regs *regs)
{



 return !!(regs->cs & 3);

}

static inline __attribute__((always_inline)) int user_mode_vm(struct pt_regs *regs)
{




 return user_mode(regs);

}

static inline __attribute__((always_inline)) int v8086_mode(struct pt_regs *regs)
{



 return 0;

}







static inline __attribute__((always_inline)) unsigned long kernel_trap_sp(struct pt_regs *regs)
{



 return regs->sp;

}

static inline __attribute__((always_inline)) unsigned long instruction_pointer(struct pt_regs *regs)
{
 return regs->ip;
}

static inline __attribute__((always_inline)) unsigned long frame_pointer(struct pt_regs *regs)
{
 return regs->bp;
}

static inline __attribute__((always_inline)) unsigned long user_stack_pointer(struct pt_regs *regs)
{
 return regs->sp;
}





extern void user_enable_single_step(struct task_struct *);
extern void user_disable_single_step(struct task_struct *);

extern void user_enable_block_step(struct task_struct *);






struct user_desc;
extern int do_get_thread_area(struct task_struct *p, int idx,
         struct user_desc __attribute__((noderef, address_space(1))) *info);
extern int do_set_thread_area(struct task_struct *p, int idx,
         struct user_desc __attribute__((noderef, address_space(1))) *info, int can_allocate);

extern void x86_ptrace_untrace(struct task_struct *);
extern void x86_ptrace_fork(struct task_struct *child,
       unsigned long clone_flags);
# 131 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h" 2
# 141 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h"
struct kernel_vm86_regs {



 struct pt_regs pt;



 unsigned short es, __esh;
 unsigned short ds, __dsh;
 unsigned short fs, __fsh;
 unsigned short gs, __gsh;
};

struct kernel_vm86_struct {
 struct kernel_vm86_regs regs;
# 166 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h"
 unsigned long flags;
 unsigned long screen_bitmap;
 unsigned long cpu_type;
 struct revectored_struct int_revectored;
 struct revectored_struct int21_revectored;
 struct vm86plus_info_struct vm86plus;
 struct pt_regs *regs32;
# 183 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h"
};
# 199 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vm86.h"
static inline __attribute__((always_inline)) int handle_vm86_trap(struct kernel_vm86_regs *a, long b, int c)
{
 return 0;
}
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/math_emu.h" 1
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/math_emu.h"
struct math_emu_info {
 long ___orig_eip;
 union {
  struct pt_regs *regs;
  struct kernel_vm86_regs *vm86;
 };
};
# 12 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sigcontext.h" 1
# 23 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sigcontext.h"
struct _fpx_sw_bytes {
 __u32 magic1;
 __u32 extended_size;


 __u64 xstate_bv;




 __u32 xstate_size;




 __u32 padding[7];
};
# 169 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sigcontext.h"
struct _fpstate {
 __u16 cwd;
 __u16 swd;
 __u16 twd;

 __u16 fop;
 __u64 rip;
 __u64 rdp;
 __u32 mxcsr;
 __u32 mxcsr_mask;
 __u32 st_space[32];
 __u32 xmm_space[64];
 __u32 reserved2[12];
 union {
  __u32 reserved3[12];
  struct _fpx_sw_bytes sw_reserved;

 };
};


struct sigcontext {
 unsigned long r8;
 unsigned long r9;
 unsigned long r10;
 unsigned long r11;
 unsigned long r12;
 unsigned long r13;
 unsigned long r14;
 unsigned long r15;
 unsigned long di;
 unsigned long si;
 unsigned long bp;
 unsigned long bx;
 unsigned long dx;
 unsigned long ax;
 unsigned long cx;
 unsigned long sp;
 unsigned long ip;
 unsigned long flags;
 unsigned short cs;
 unsigned short gs;
 unsigned short fs;
 unsigned short __pad0;
 unsigned long err;
 unsigned long trapno;
 unsigned long oldmask;
 unsigned long cr2;
# 225 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sigcontext.h"
 void __attribute__((noderef, address_space(1))) *fpstate;
 unsigned long reserved1[8];
};
# 266 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sigcontext.h"
struct _xsave_hdr {
 __u64 xstate_bv;
 __u64 reserved1[2];
 __u64 reserved2[5];
};







struct _xstate {
 struct _fpstate fpstate;
 struct _xsave_hdr xstate_hdr;

};
# 15 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/current.h" 1
# 19 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/current.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pda.h" 1






# 1 "include/linux/cache.h" 1



# 1 "include/linux/kernel.h" 1
# 10 "include/linux/kernel.h"
# 1 "/usr/lib/gcc/x86_64-redhat-linux/4.4.6/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-redhat-linux/4.4.6/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 102 "/usr/lib/gcc/x86_64-redhat-linux/4.4.6/include/stdarg.h" 3 4
typedef __gnuc_va_list va_list;
# 11 "include/linux/kernel.h" 2
# 1 "include/linux/linkage.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/linkage.h" 1
# 6 "include/linux/linkage.h" 2
# 12 "include/linux/kernel.h" 2




# 1 "include/linux/log2.h" 1
# 21 "include/linux/log2.h"
extern __attribute__((const, noreturn))
int ____ilog2_NaN(void);
# 31 "include/linux/log2.h"
static inline __attribute__((always_inline)) __attribute__((const))
int __ilog2_u32(u32 n)
{
 return fls(n) - 1;
}



static inline __attribute__((always_inline)) __attribute__((const))
int __ilog2_u64(u64 n)
{
 return fls64(n) - 1;
}







static inline __attribute__((always_inline)) __attribute__((const))
bool is_power_of_2(unsigned long n)
{
 return (n != 0 && ((n & (n - 1)) == 0));
}




static inline __attribute__((always_inline)) __attribute__((const))
unsigned long __roundup_pow_of_two(unsigned long n)
{
 return 1UL << fls_long(n - 1);
}




static inline __attribute__((always_inline)) __attribute__((const))
unsigned long __rounddown_pow_of_two(unsigned long n)
{
 return 1UL << (fls_long(n) - 1);
}
# 17 "include/linux/kernel.h" 2

# 1 "include/linux/ratelimit.h" 1


# 1 "include/linux/param.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/param.h" 1
# 5 "include/linux/param.h" 2
# 4 "include/linux/ratelimit.h" 2




struct ratelimit_state {
 int interval;
 int burst;
 int printed;
 int missed;
 unsigned long begin;
};




extern int __ratelimit(struct ratelimit_state *rs);
# 19 "include/linux/kernel.h" 2
# 1 "include/linux/dynamic_printk.h" 1
# 13 "include/linux/dynamic_printk.h"
extern int dynamic_enabled;





extern long long dynamic_printk_enabled;
extern long long dynamic_printk_enabled2;

struct mod_debug {
 char *modname;
 char *logical_modname;
 char *flag_names;
 int type;
 int hash;
 int hash2;
} __attribute__((aligned(8)));

int register_dynamic_debug_module(char *mod_name, int type, char *share_name,
     char *flags, int hash, int hash2);
# 78 "include/linux/dynamic_printk.h"
static inline __attribute__((always_inline)) int unregister_dynamic_debug_module(const char *mod_name)
{
 return 0;
}
static inline __attribute__((always_inline)) int __dynamic_dbg_enabled_helper(char *modname, int type,
      int value, int hash)
{
 return 0;
}
# 20 "include/linux/kernel.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bug.h" 1
# 38 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bug.h"
# 1 "include/asm-generic/bug.h" 1
# 10 "include/asm-generic/bug.h"
struct bug_entry {



 signed int bug_addr_disp;





 signed int file_disp;

 unsigned short line;

 unsigned short flags;
};
# 61 "include/asm-generic/bug.h"
extern void warn_slowpath(const char *file, const int line,
  const char *fmt, ...) __attribute__((format(printf, 3, 4)));
# 39 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/bug.h" 2
# 22 "include/linux/kernel.h" 2

extern const char linux_banner[];
extern const char linux_proc_banner[];
# 107 "include/linux/kernel.h"
extern int console_printk[];






struct completion;
struct pt_regs;
struct user;


extern int _cond_resched(void);






void __might_sleep(char *__attribute__((nullterm)) file, int line);
# 153 "include/linux/kernel.h"
static inline __attribute__((always_inline)) void might_fault(void)
{
 do { __might_sleep("include/linux/kernel.h", 155); _cond_resched(); } while (0);
}


extern struct atomic_notifier_head panic_notifier_list;
extern long (*panic_blink)(long time);
 void panic(const char * fmt, ...)
 __attribute__ ((noreturn, format (printf, 1, 2))) __attribute__((__cold__));
extern void oops_enter(void);
extern void oops_exit(void);
extern int oops_may_print(void);
 void do_exit(long error_code)
 __attribute__((noreturn));
 void complete_and_exit(struct completion *, long)
 __attribute__((noreturn));
extern unsigned long simple_strtoul(const char *,char **,unsigned int);
extern long simple_strtol(const char *,char **,unsigned int);
extern unsigned long long simple_strtoull(const char *,char **,unsigned int);
extern long long simple_strtoll(const char *,char **,unsigned int);
extern int strict_strtoul(const char *, unsigned int, unsigned long *);
extern int strict_strtol(const char *, unsigned int, long *);
extern int strict_strtoull(const char *, unsigned int, unsigned long long *);
extern int strict_strtoll(const char *, unsigned int, long long *);
extern int sprintf(char * buf, const char * fmt, ...)
 __attribute__ ((format (printf, 2, 3)));
extern int vsprintf(char *buf, const char *, va_list)
 __attribute__ ((format (printf, 2, 0)));
extern int snprintf(char * buf, size_t size, const char * fmt, ...)
 __attribute__ ((format (printf, 3, 4)));
extern int vsnprintf(char *buf, size_t size, const char *fmt, va_list args)
 __attribute__ ((format (printf, 3, 0)));
extern int scnprintf(char * buf, size_t size, const char * fmt, ...)
 __attribute__ ((format (printf, 3, 4)));
extern int vscnprintf(char *buf, size_t size, const char *fmt, va_list args)
 __attribute__ ((format (printf, 3, 0)));
extern char *kasprintf(gfp_t gfp, const char *fmt, ...)
 __attribute__ ((format (printf, 2, 3)));
extern char *kvasprintf(gfp_t gfp, const char *fmt, va_list args);

extern int sscanf(const char *, const char *, ...)
 __attribute__ ((format (scanf, 2, 3)));
extern int vsscanf(const char *, const char *, va_list)
 __attribute__ ((format (scanf, 2, 0)));

extern int get_option(char **str, int *pint);
extern char *get_options(const char *str, int nints, int *ints);
extern unsigned long long memparse(const char *ptr, char **retptr);

extern int core_kernel_text(unsigned long addr);
extern int __kernel_text_address(unsigned long addr);
extern int kernel_text_address(unsigned long addr);
extern int func_ptr_is_kernel_text(void *ptr);

struct pid;
extern struct pid *session_of_pgrp(struct pid *pgrp);
# 236 "include/linux/kernel.h"
 int vprintk(const char *fmt, va_list args)
 __attribute__ ((format (printf, 1, 0)));
 int printk(const char * fmt, ...)
 __attribute__ ((format (printf, 1, 2))) __attribute__((__cold__));

extern struct ratelimit_state printk_ratelimit_state;
extern int printk_ratelimit(void);
extern bool printk_timed_ratelimit(unsigned long *caller_jiffies,
       unsigned int interval_msec);
# 258 "include/linux/kernel.h"
extern int printk_needs_cpu(int cpu);
extern void printk_tick(void);

extern void __attribute__((format(printf, 1, 2)))
 early_printk(const char *fmt, ...);

unsigned long int_sqrt(unsigned long);

static inline __attribute__((always_inline)) void console_silent(void)
{
 (console_printk[0]) = 0;
}

static inline __attribute__((always_inline)) void console_verbose(void)
{
 if ((console_printk[0]))
  (console_printk[0]) = 15;
}

extern void bust_spinlocks(int yes);
extern void wake_up_klogd(void);
extern int oops_in_progress;
extern int panic_timeout;
extern int panic_on_oops;
extern int panic_on_unrecovered_nmi;
extern const char *print_tainted(void);
extern void add_taint(unsigned flag);
extern int test_taint(unsigned flag);
extern unsigned long get_taint(void);
extern int root_mountflags;


extern enum system_states {
 SYSTEM_BOOTING,
 SYSTEM_RUNNING,
 SYSTEM_HALT,
 SYSTEM_POWER_OFF,
 SYSTEM_RESTART,
 SYSTEM_SUSPEND_DISK,
} system_state;
# 311 "include/linux/kernel.h"
extern void dump_stack(void) __attribute__((__cold__));

enum {
 DUMP_PREFIX_NONE,
 DUMP_PREFIX_ADDRESS,
 DUMP_PREFIX_OFFSET
};
extern void hex_dump_to_buffer(const void *buf, size_t len,
    int rowsize, int groupsize,
    char *linebuf, size_t linebuflen, bool ascii);
extern void print_hex_dump(const char *level, const char *prefix_str,
    int prefix_type, int rowsize, int groupsize,
    const void *buf, size_t len, bool ascii);
extern void print_hex_dump_bytes(const char *prefix_str, int prefix_type,
   const void *buf, size_t len);

extern const char hex_asc[];



static inline __attribute__((always_inline)) char *pack_hex_byte(char *buf, u8 byte)
{
 *buf++ = hex_asc[((byte) & 0xf0) >> 4];
 *buf++ = hex_asc[((byte) & 0x0f)];
 return buf;
}
# 497 "include/linux/kernel.h"
struct sysinfo;
extern int do_sysinfo(struct sysinfo *info);




struct sysinfo {
 long uptime;
 unsigned long loads[3];
 unsigned long totalram;
 unsigned long freeram;
 unsigned long sharedram;
 unsigned long bufferram;
 unsigned long totalswap;
 unsigned long freeswap;
 unsigned short procs;
 unsigned short pad;
 unsigned long totalhigh;
 unsigned long freehigh;
 unsigned int mem_unit;
 char _f[20-2*sizeof(long)-sizeof(int)];
};
# 5 "include/linux/cache.h" 2
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pda.h" 2



struct x8664_pda {
 struct task_struct *pcurrent;
 unsigned long data_offset;

 unsigned long kernelstack;
 unsigned long oldrsp;
 int irqcount;
 unsigned int cpunumber;





 char *irqstackptr;
 short nodenumber;
 short in_bootmem;
 unsigned int __softirq_pending;
 unsigned int __nmi_count;
 short mmu_state;
 short isidle;
 struct mm_struct *active_mm;
 unsigned apic_timer_irqs;
 unsigned irq0_irqs;
 unsigned irq_resched_count;
 unsigned irq_call_count;
 unsigned irq_tlb_count;
 unsigned irq_thermal_count;
 unsigned irq_threshold_count;
 unsigned irq_spurious_count;
} __attribute__((__aligned__((1 << (7)))));

extern struct x8664_pda **_cpu_pda;
extern void pda_init(int);







extern void __bad_pda_field(void) __attribute__((noreturn));





extern struct x8664_pda _proxy_pda;
# 20 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/current.h" 2

struct task_struct;

static inline __attribute__((always_inline)) __attribute__((always_inline)) struct task_struct *get_current(void)
{
 return ({ typeof(_proxy_pda.pcurrent) ret__; switch (sizeof(_proxy_pda.pcurrent)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,pcurrent)), "m" (_proxy_pda.pcurrent)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,pcurrent)), "m" (_proxy_pda.pcurrent)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,pcurrent)), "m" (_proxy_pda.pcurrent)); break; default: __bad_pda_field(); } ret__; });
}
# 16 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h" 1






# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg_64.h" 1
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg_64.h"
static inline __attribute__((always_inline)) void set_64bit(volatile unsigned long *ptr, unsigned long val)
{
 *ptr = val;
}
# 23 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg_64.h"
static inline __attribute__((always_inline)) unsigned long __xchg(unsigned long x, volatile void *ptr,
       int size)
{
 switch (size) {
 case 1:
  asm volatile("xchgb %b0,%1"
        : "=q" (x)
        : "m" (*((volatile long *)(ptr))), "0" (x)
        : "memory");
  break;
 case 2:
  asm volatile("xchgw %w0,%1"
        : "=r" (x)
        : "m" (*((volatile long *)(ptr))), "0" (x)
        : "memory");
  break;
 case 4:
  asm volatile("xchgl %k0,%1"
        : "=r" (x)
        : "m" (*((volatile long *)(ptr))), "0" (x)
        : "memory");
  break;
 case 8:
  asm volatile("xchgq %0,%1"
        : "=r" (x)
        : "m" (*((volatile long *)(ptr))), "0" (x)
        : "memory");
  break;
 }
 return x;
}
# 63 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg_64.h"
static inline __attribute__((always_inline)) unsigned long __cmpxchg(volatile void *ptr, unsigned long old,
          unsigned long new, int size)
{
 unsigned long prev;
 switch (size) {
 case 1:
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "cmpxchgb %b1,%2"
        : "=a"(prev)
        : "q"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 2:
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "cmpxchgw %w1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 4:
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "cmpxchgl %k1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 8:
  asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "cmpxchgq %1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 }
 return old;
}






static inline __attribute__((always_inline)) unsigned long __sync_cmpxchg(volatile void *ptr,
        unsigned long old,
        unsigned long new, int size)
{
 unsigned long prev;
 switch (size) {
 case 1:
  asm volatile("lock; cmpxchgb %b1,%2"
        : "=a"(prev)
        : "q"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 2:
  asm volatile("lock; cmpxchgw %w1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 4:
  asm volatile("lock; cmpxchgl %1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 }
 return old;
}

static inline __attribute__((always_inline)) unsigned long __cmpxchg_local(volatile void *ptr,
         unsigned long old,
         unsigned long new, int size)
{
 unsigned long prev;
 switch (size) {
 case 1:
  asm volatile("cmpxchgb %b1,%2"
        : "=a"(prev)
        : "q"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 2:
  asm volatile("cmpxchgw %w1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 4:
  asm volatile("cmpxchgl %k1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 case 8:
  asm volatile("cmpxchgq %1,%2"
        : "=a"(prev)
        : "r"(new), "m"(*((volatile long *)(ptr))), "0"(old)
        : "memory");
  return prev;
 }
 return old;
}
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg.h" 2
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/nops.h" 1
# 9 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h" 2


# 1 "include/linux/irqflags.h" 1
# 57 "include/linux/irqflags.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irqflags.h" 1
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irqflags.h"
static inline __attribute__((always_inline)) unsigned long native_save_fl(void)
{
 unsigned long flags;

 asm volatile("# __raw_save_flags\n\t"
       "pushf ; pop %0"
       : "=g" (flags)
       :
       : "memory");

 return flags;
}

static inline __attribute__((always_inline)) void native_restore_fl(unsigned long flags)
{
 asm volatile("push %0 ; popf"
       :
       :"g" (flags)
       :"memory", "cc");
}

static inline __attribute__((always_inline)) void native_irq_disable(void)
{
 asm volatile("cli": : :"memory");
}

static inline __attribute__((always_inline)) void native_irq_enable(void)
{
 asm volatile("sti": : :"memory");
}

static inline __attribute__((always_inline)) void native_safe_halt(void)
{
 asm volatile("sti; hlt": : :"memory");
}

static inline __attribute__((always_inline)) void native_halt(void)
{
 asm volatile("hlt": : :"memory");
}
# 59 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irqflags.h"
static inline __attribute__((always_inline)) unsigned long __raw_local_save_flags(void)
{
 return native_save_fl();
}

static inline __attribute__((always_inline)) void raw_local_irq_restore(unsigned long flags)
{
 native_restore_fl(flags);
}

static inline __attribute__((always_inline)) void raw_local_irq_disable(void)
{
 native_irq_disable();
}

static inline __attribute__((always_inline)) void raw_local_irq_enable(void)
{
 native_irq_enable();
}





static inline __attribute__((always_inline)) void raw_safe_halt(void)
{
 native_safe_halt();
}





static inline __attribute__((always_inline)) void halt(void)
{
 native_halt();
}




static inline __attribute__((always_inline)) unsigned long __raw_local_irq_save(void)
{
 unsigned long flags = __raw_local_save_flags();

 raw_local_irq_disable();

 return flags;
}
# 157 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irqflags.h"
static inline __attribute__((always_inline)) int raw_irqs_disabled_flags(unsigned long flags)
{
 return !(flags & 0x00000200);
}

static inline __attribute__((always_inline)) int raw_irqs_disabled(void)
{
 unsigned long flags = __raw_local_save_flags();

 return raw_irqs_disabled_flags(flags);
}
# 58 "include/linux/irqflags.h" 2
# 12 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h" 2
# 20 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
struct task_struct;
struct task_struct *__switch_to(struct task_struct *prev,
    struct task_struct *next);
# 142 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
extern void native_load_gs_index(unsigned);
# 168 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
static inline __attribute__((always_inline)) unsigned long get_limit(unsigned long segment)
{
 unsigned long __limit;
 asm("lsll %1,%0" : "=r" (__limit) : "r" (segment));
 return __limit + 1;
}

static inline __attribute__((always_inline)) void native_clts(void)
{
 asm volatile("clts");
}
# 187 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
static unsigned long __force_order;

static inline __attribute__((always_inline)) unsigned long native_read_cr0(void)
{
 unsigned long val;
 asm volatile("mov %%cr0,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((always_inline)) void native_write_cr0(unsigned long val)
{
 asm volatile("mov %0,%%cr0": : "r" (val), "m" (__force_order));
}

static inline __attribute__((always_inline)) unsigned long native_read_cr2(void)
{
 unsigned long val;
 asm volatile("mov %%cr2,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((always_inline)) void native_write_cr2(unsigned long val)
{
 asm volatile("mov %0,%%cr2": : "r" (val), "m" (__force_order));
}

static inline __attribute__((always_inline)) unsigned long native_read_cr3(void)
{
 unsigned long val;
 asm volatile("mov %%cr3,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((always_inline)) void native_write_cr3(unsigned long val)
{
 asm volatile("mov %0,%%cr3": : "r" (val), "m" (__force_order));
}

static inline __attribute__((always_inline)) unsigned long native_read_cr4(void)
{
 unsigned long val;
 asm volatile("mov %%cr4,%0\n\t" : "=r" (val), "=m" (__force_order));
 return val;
}

static inline __attribute__((always_inline)) unsigned long native_read_cr4_safe(void)
{
 unsigned long val;
# 243 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
 val = native_read_cr4();

 return val;
}

static inline __attribute__((always_inline)) void native_write_cr4(unsigned long val)
{
 asm volatile("mov %0,%%cr4": : "r" (val), "m" (__force_order));
}


static inline __attribute__((always_inline)) unsigned long native_read_cr8(void)
{
 unsigned long cr8;
 asm volatile("movq %%cr8,%0" : "=r" (cr8));
 return cr8;
}

static inline __attribute__((always_inline)) void native_write_cr8(unsigned long val)
{
 asm volatile("movq %0,%%cr8" :: "r" (val) : "memory");
}


static inline __attribute__((always_inline)) void native_wbinvd(void)
{
 asm volatile("wbinvd": : :"memory");
}
# 300 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
static inline __attribute__((always_inline)) void clflush(volatile void *__p)
{
 asm volatile("clflush %0" : "+m" (*(volatile char *)__p));
}



void disable_hlt(void);
void enable_hlt(void);

void cpu_idle_wait(void);

extern unsigned long arch_align_stack(unsigned long sp);
extern void free_init_pages(char *what, unsigned long begin, unsigned long end);

void default_idle(void);

void stop_this_cpu(void *dummy);
# 421 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/system.h"
static inline __attribute__((always_inline)) void rdtsc_barrier(void)
{
 asm volatile ("661:\n\t" ".byte 0x66,0x66,0x90\n" "\n662:\n" ".section .altinstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661b\n" " " ".quad" " " "663f\n" "	 .byte %c0\n" "	 .byte 662b-661b\n" "	 .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "mfence" "\n664:\n" ".previous" :: "i" ((3*32+17)) : "memory");
 asm volatile ("661:\n\t" ".byte 0x66,0x66,0x90\n" "\n662:\n" ".section .altinstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661b\n" " " ".quad" " " "663f\n" "	 .byte %c0\n" "	 .byte 662b-661b\n" "	 .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((3*32+18)) : "memory");
}
# 18 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/percpu.h" 1
# 21 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/percpu.h"
# 1 "include/asm-generic/percpu.h" 1



# 1 "include/linux/threads.h" 1
# 5 "include/asm-generic/percpu.h" 2
# 65 "include/asm-generic/percpu.h"
extern void setup_per_cpu_areas(void);
# 22 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/percpu.h" 2

extern __typeof__(struct x8664_pda) per_cpu__pda;
# 20 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr-index.h" 1
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h" 2
# 14 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/errno.h" 1
# 1 "include/asm-generic/errno.h" 1



# 1 "include/asm-generic/errno-base.h" 1
# 5 "include/asm-generic/errno.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/errno.h" 2
# 15 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h" 2

static inline __attribute__((always_inline)) unsigned long long native_read_tscp(unsigned int *aux)
{
 unsigned long low, high;
 asm volatile(".byte 0x0f,0x01,0xf9"
       : "=a" (low), "=d" (high), "=c" (*aux));
 return low | ((u64)high << 32);
}
# 42 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h"
static inline __attribute__((always_inline)) unsigned long long native_read_msr(unsigned int msr)
{
 unsigned low, high;

 asm volatile("rdmsr" : "=a" (low), "=d" (high) : "c" (msr));
 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((always_inline)) unsigned long long native_read_msr_safe(unsigned int msr,
            int *err)
{
 unsigned low, high;

 asm volatile("2: rdmsr ; xor %[err],%[err]\n"
       "1:\n\t"
       ".section .fixup,\"ax\"\n\t"
       "3:  mov %[fault],%[err] ; jmp 1b\n\t"
       ".previous\n\t"
       " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "2b" "," "3b" "\n" " .previous\n"
       : [err] "=r" (*err), "=a" (low), "=d" (high)
       : "c" (msr), [fault] "i" (-14));
 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((always_inline)) unsigned long long native_read_msr_amd_safe(unsigned int msr,
            int *err)
{
 unsigned low, high;

 asm volatile("2: rdmsr ; xor %0,%0\n"
       "1:\n\t"
       ".section .fixup,\"ax\"\n\t"
       "3:  mov %3,%0 ; jmp 1b\n\t"
       ".previous\n\t"
       " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "2b" "," "3b" "\n" " .previous\n"
       : "=r" (*err), "=a" (low), "=d" (high)
       : "c" (msr), "D" (0x9c5a203a), "i" (-14));
 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((always_inline)) void native_write_msr(unsigned int msr,
        unsigned low, unsigned high)
{
 asm volatile("wrmsr" : : "c" (msr), "a"(low), "d" (high) : "memory");
}


__attribute__((no_instrument_function)) static inline __attribute__((always_inline)) int native_write_msr_safe(unsigned int msr,
     unsigned low, unsigned high)
{
 int err;
 asm volatile("2: wrmsr ; xor %[err],%[err]\n"
       "1:\n\t"
       ".section .fixup,\"ax\"\n\t"
       "3:  mov %[fault],%[err] ; jmp 1b\n\t"
       ".previous\n\t"
       " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "2b" "," "3b" "\n" " .previous\n"
       : [err] "=a" (err)
       : "c" (msr), "0" (low), "d" (high),
         [fault] "i" (-14)
       : "memory");
 return err;
}

extern unsigned long long native_read_tsc(void);

static inline __attribute__((always_inline)) __attribute__((always_inline)) unsigned long long __native_read_tsc(void)
{
 unsigned low, high;

 asm volatile("rdtsc" : "=a" (low), "=d" (high));

 return ((low) | ((u64)(high) << 32));
}

static inline __attribute__((always_inline)) unsigned long long native_read_pmc(int counter)
{
 unsigned low, high;

 asm volatile("rdpmc" : "=a" (low), "=d" (high) : "c" (counter));
 return ((low) | ((u64)(high) << 32));
}




# 1 "include/linux/errno.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/errno.h" 1
# 5 "include/linux/errno.h" 2
# 129 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h" 2
# 142 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h"
static inline __attribute__((always_inline)) void wrmsr(unsigned msr, unsigned low, unsigned high)
{
 native_write_msr(msr, low, high);
}
# 154 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h"
static inline __attribute__((always_inline)) int wrmsr_safe(unsigned msr, unsigned low, unsigned high)
{
 return native_write_msr_safe(msr, low, high);
}
# 169 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h"
static inline __attribute__((always_inline)) int rdmsrl_safe(unsigned msr, unsigned long long *p)
{
 int err;

 *p = native_read_msr_safe(msr, &err);
 return err;
}
static inline __attribute__((always_inline)) int rdmsrl_amd_safe(unsigned msr, unsigned long long *p)
{
 int err;

 *p = native_read_msr_amd_safe(msr, &err);
 return err;
}
# 217 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/msr.h"
int rdmsr_on_cpu(unsigned int cpu, u32 msr_no, u32 *l, u32 *h);
int wrmsr_on_cpu(unsigned int cpu, u32 msr_no, u32 l, u32 h);
int rdmsr_safe_on_cpu(unsigned int cpu, u32 msr_no, u32 *l, u32 *h);
int wrmsr_safe_on_cpu(unsigned int cpu, u32 msr_no, u32 l, u32 h);
# 21 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/desc_defs.h" 1
# 22 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/desc_defs.h"
struct desc_struct {
 union {
  struct {
   unsigned int a;
   unsigned int b;
  };
  struct {
   u16 limit0;
   u16 base0;
   unsigned base1: 8, type: 4, s: 1, dpl: 2, p: 1;
   unsigned limit: 4, avl: 1, l: 1, d: 1, g: 1, base2: 8;
  };
 };
} __attribute__((packed));

enum {
 GATE_INTERRUPT = 0xE,
 GATE_TRAP = 0xF,
 GATE_CALL = 0xC,
 GATE_TASK = 0x5,
};


struct gate_struct64 {
 u16 offset_low;
 u16 segment;
 unsigned ist : 3, zero0 : 5, type : 5, dpl : 2, p : 1;
 u16 offset_middle;
 u32 offset_high;
 u32 zero1;
} __attribute__((packed));





enum {
 DESC_TSS = 0x9,
 DESC_LDT = 0x2,
 DESCTYPE_S = 0x10,
};


struct ldttss_desc64 {
 u16 limit0;
 u16 base0;
 unsigned base1 : 8, type : 5, dpl : 2, p : 1;
 unsigned limit1 : 4, zero0 : 3, g : 1, base2 : 8;
 u32 base3;
 u32 zero1;
} __attribute__((packed));


typedef struct gate_struct64 gate_desc;
typedef struct ldttss_desc64 ldt_desc;
typedef struct ldttss_desc64 tss_desc;
# 88 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/desc_defs.h"
struct desc_ptr {
 unsigned short size;
 unsigned long address;
} __attribute__((packed)) ;
# 22 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h" 1
# 28 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
# 1 "include/linux/err.h" 1





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/errno.h" 1
# 7 "include/linux/err.h" 2
# 22 "include/linux/err.h"
static inline __attribute__((always_inline)) void *ERR_PTR(long error)
{
 return (void *) error;
}

static inline __attribute__((always_inline)) long PTR_ERR(const void *ptr)
{
 return (long) ptr;
}

static inline __attribute__((always_inline)) long IS_ERR(const void *ptr)
{
 return __builtin_expect(!!(((unsigned long)ptr) >= (unsigned long)-4095), 0);
}
# 44 "include/linux/err.h"
static inline __attribute__((always_inline)) void *ERR_CAST(const void *ptr)
{

 return (void *) ptr;
}
# 29 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h" 2




struct task_struct;
struct ds_context;
struct ds_tracer;
struct bts_tracer;
struct pebs_tracer;

typedef void (*bts_ovfl_callback_t)(struct bts_tracer *);
typedef void (*pebs_ovfl_callback_t)(struct pebs_tracer *);
# 50 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
enum ds_feature {
 dsf_bts = 0,
 dsf_bts_kernel,



 dsf_bts_user,



 dsf_bts_overflow,
 dsf_bts_max,
 dsf_pebs = dsf_bts_max,

 dsf_pebs_max,
 dsf_ctl_max = dsf_pebs_max,
 dsf_bts_timestamps = dsf_ctl_max,




};
# 96 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
extern struct bts_tracer *ds_request_bts(struct task_struct *task,
      void *base, size_t size,
      bts_ovfl_callback_t ovfl,
      size_t th, unsigned int flags);
extern struct pebs_tracer *ds_request_pebs(struct task_struct *task,
        void *base, size_t size,
        pebs_ovfl_callback_t ovfl,
        size_t th, unsigned int flags);







extern void ds_release_bts(struct bts_tracer *tracer);
extern void ds_suspend_bts(struct bts_tracer *tracer);
extern void ds_resume_bts(struct bts_tracer *tracer);
extern void ds_release_pebs(struct pebs_tracer *tracer);
extern void ds_suspend_pebs(struct pebs_tracer *tracer);
extern void ds_resume_pebs(struct pebs_tracer *tracer);
# 125 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
struct ds_trace {

 size_t n;

 size_t size;


 void *begin;

 void *end;

 void *top;

 void *ith;

 unsigned int flags;
};




enum bts_qualifier {
 bts_invalid,


 bts_branch,


 bts_task_arrives,


 bts_task_departs,


 bts_qual_bit_size = 4,
 bts_qual_max = (1 << bts_qual_bit_size),
};

struct bts_struct {
 __u64 qualifier;
 union {

  struct {
   __u64 from;
   __u64 to;
  } lbr;

  struct {
   __u64 jiffies;
   pid_t pid;
  } timestamp;
 } variant;
};
# 186 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
struct bts_trace {
 struct ds_trace ds;

 int (*read)(struct bts_tracer *tracer, const void *at,
      struct bts_struct *out);
 int (*write)(struct bts_tracer *tracer, const struct bts_struct *in);
};
# 201 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
struct pebs_trace {
 struct ds_trace ds;


 unsigned long long reset_value;
};
# 220 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
extern const struct bts_trace *ds_read_bts(struct bts_tracer *tracer);
extern const struct pebs_trace *ds_read_pebs(struct pebs_tracer *tracer);
# 231 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
extern int ds_reset_bts(struct bts_tracer *tracer);
extern int ds_reset_pebs(struct pebs_tracer *tracer);
# 242 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ds.h"
extern int ds_set_pebs_reset(struct pebs_tracer *tracer, u64 value);




struct cpuinfo_x86;
extern void __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) ds_init_intel(struct cpuinfo_x86 *);




extern void ds_switch_to(struct task_struct *prev, struct task_struct *next);




extern void ds_copy_thread(struct task_struct *tsk, struct task_struct *father);
extern void ds_exit_thread(struct task_struct *tsk);
# 24 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2

# 1 "include/linux/personality.h" 1
# 10 "include/linux/personality.h"
struct exec_domain;
struct pt_regs;

extern int register_exec_domain(struct exec_domain *);
extern int unregister_exec_domain(struct exec_domain *);
extern int __set_personality(unsigned long);
# 24 "include/linux/personality.h"
enum {
 ADDR_NO_RANDOMIZE = 0x0040000,
 FDPIC_FUNCPTRS = 0x0080000,


 MMAP_PAGE_ZERO = 0x0100000,
 ADDR_COMPAT_LAYOUT = 0x0200000,
 READ_IMPLIES_EXEC = 0x0400000,
 ADDR_LIMIT_32BIT = 0x0800000,
 SHORT_INODE = 0x1000000,
 WHOLE_SECONDS = 0x2000000,
 STICKY_TIMEOUTS = 0x4000000,
 ADDR_LIMIT_3GB = 0x8000000,
};
# 51 "include/linux/personality.h"
enum {
 PER_LINUX = 0x0000,
 PER_LINUX_32BIT = 0x0000 | ADDR_LIMIT_32BIT,
 PER_LINUX_FDPIC = 0x0000 | FDPIC_FUNCPTRS,
 PER_SVR4 = 0x0001 | STICKY_TIMEOUTS | MMAP_PAGE_ZERO,
 PER_SVR3 = 0x0002 | STICKY_TIMEOUTS | SHORT_INODE,
 PER_SCOSVR3 = 0x0003 | STICKY_TIMEOUTS |
      WHOLE_SECONDS | SHORT_INODE,
 PER_OSR5 = 0x0003 | STICKY_TIMEOUTS | WHOLE_SECONDS,
 PER_WYSEV386 = 0x0004 | STICKY_TIMEOUTS | SHORT_INODE,
 PER_ISCR4 = 0x0005 | STICKY_TIMEOUTS,
 PER_BSD = 0x0006,
 PER_SUNOS = 0x0006 | STICKY_TIMEOUTS,
 PER_XENIX = 0x0007 | STICKY_TIMEOUTS | SHORT_INODE,
 PER_LINUX32 = 0x0008,
 PER_LINUX32_3GB = 0x0008 | ADDR_LIMIT_3GB,
 PER_IRIX32 = 0x0009 | STICKY_TIMEOUTS,
 PER_IRIXN32 = 0x000a | STICKY_TIMEOUTS,
 PER_IRIX64 = 0x000b | STICKY_TIMEOUTS,
 PER_RISCOS = 0x000c,
 PER_SOLARIS = 0x000d | STICKY_TIMEOUTS,
 PER_UW7 = 0x000e | STICKY_TIMEOUTS | MMAP_PAGE_ZERO,
 PER_OSF4 = 0x000f,
 PER_HPUX = 0x0010,
 PER_MASK = 0x00ff,
};
# 86 "include/linux/personality.h"
typedef void (*handler_t)(int, struct pt_regs *);

struct exec_domain {
 const char *name;
 handler_t handler;
 unsigned char pers_low;
 unsigned char pers_high;
 unsigned long *signal_map;
 unsigned long *signal_invmap;
 struct map_segment *err_map;
 struct map_segment *socktype_map;
 struct map_segment *sockopt_map;
 struct map_segment *af_map;
 struct module *module;
 struct exec_domain *next;
};
# 26 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2
# 1 "include/linux/cpumask.h" 1
# 142 "include/linux/cpumask.h"
# 1 "include/linux/bitmap.h" 1







# 1 "include/linux/string.h" 1
# 14 "include/linux/string.h"
extern char *strndup_user(const char __attribute__((noderef, address_space(1))) *, long);




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/string.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/string_64.h" 1
# 9 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/string_64.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) void *__inline_memcpy(void *to, const void *from, size_t n)
{
 unsigned long d0, d1, d2;
 asm volatile("rep ; movsl\n\t"
       "testb $2,%b4\n\t"
       "je 1f\n\t"
       "movsw\n"
       "1:\ttestb $1,%b4\n\t"
       "je 2f\n\t"
       "movsb\n"
       "2:"
       : "=&c" (d0), "=&D" (d1), "=&S" (d2)
       : "0" (n / 4), "q" (n), "1" ((long)to), "2" ((long)from)
       : "memory");
 return to;
}






extern void *memcpy(void *to, const void *from, size_t len);
# 47 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/string_64.h"
void *memset(void *s, int c, size_t n);


void *memmove(void *dest, const void *src, size_t count);

int memcmp(const void *cs, const void *ct, size_t count);
size_t strlen(const char *s);
char *strcpy(char *dest, const char *src);
char *strcat(char *dest, const char *src);
int strcmp(const char *cs, const char *ct);
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/string.h" 2
# 20 "include/linux/string.h" 2


extern char * strcpy(char *,const char *);


extern char * strncpy(char *,const char *, __kernel_size_t);


size_t strlcpy(char *, const char *, size_t);


extern char * strcat(char *, const char *);


extern char * strncat(char *, const char *, __kernel_size_t);


extern size_t strlcat(char *, const char *, __kernel_size_t);


extern int strcmp(const char *,const char *);


extern int strncmp(const char *,const char *,__kernel_size_t);


extern int strnicmp(const char *, const char *, __kernel_size_t);


extern int strcasecmp(const char *s1, const char *s2);


extern int strncasecmp(const char *s1, const char *s2, size_t n);


extern char * strchr(const char *,int);


extern char * strnchr(const char *, size_t, int);


extern char * strrchr(const char *,int);

extern char * strstrip(char *);

extern char * strstr(const char *,const char *);


extern __kernel_size_t strlen(const char *);


extern __kernel_size_t strnlen(const char *,__kernel_size_t);


extern char * strpbrk(const char *,const char *);


extern char * strsep(char **,const char *);


extern __kernel_size_t strspn(const char *,const char *);


extern __kernel_size_t strcspn(const char *,const char *);
# 96 "include/linux/string.h"
extern void * memscan(void *,int,__kernel_size_t);


extern int memcmp(const void *,const void *,__kernel_size_t);


extern void * memchr(const void *,int,__kernel_size_t);


extern char *kstrdup(const char *s, gfp_t gfp);
extern char *kstrndup(const char *s, size_t len, gfp_t gfp);
extern void *kmemdup(const void *src, size_t len, gfp_t gfp);

extern char **argv_split(gfp_t gfp, const char *str, int *argcp);
extern void argv_free(char **argv);

extern bool sysfs_streq(const char *s1, const char *s2);

extern ssize_t memory_read_from_buffer(void *to, size_t count, loff_t *ppos,
   const void *from, size_t available);
# 9 "include/linux/bitmap.h" 2
# 87 "include/linux/bitmap.h"
extern int __bitmap_empty(const unsigned long *bitmap, int bits);
extern int __bitmap_full(const unsigned long *bitmap, int bits);
extern int __bitmap_equal(const unsigned long *bitmap1,
                 const unsigned long *bitmap2, int bits);
extern void __bitmap_complement(unsigned long *dst, const unsigned long *src,
   int bits);
extern void __bitmap_shift_right(unsigned long *dst,
                        const unsigned long *src, int shift, int bits);
extern void __bitmap_shift_left(unsigned long *dst,
                        const unsigned long *src, int shift, int bits);
extern void __bitmap_and(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, int bits);
extern void __bitmap_or(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, int bits);
extern void __bitmap_xor(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, int bits);
extern void __bitmap_andnot(unsigned long *dst, const unsigned long *bitmap1,
   const unsigned long *bitmap2, int bits);
extern int __bitmap_intersects(const unsigned long *bitmap1,
   const unsigned long *bitmap2, int bits);
extern int __bitmap_subset(const unsigned long *bitmap1,
   const unsigned long *bitmap2, int bits);
extern int __bitmap_weight(const unsigned long *bitmap, int bits);

extern int bitmap_scnprintf(char *buf, unsigned int len,
   const unsigned long *src, int nbits);
extern int __bitmap_parse(const char *buf, unsigned int buflen, int is_user,
   unsigned long *dst, int nbits);
extern int bitmap_parse_user(const char __attribute__((noderef, address_space(1))) *ubuf, unsigned int ulen,
   unsigned long *dst, int nbits);
extern int bitmap_scnlistprintf(char *buf, unsigned int len,
   const unsigned long *src, int nbits);
extern int bitmap_parselist(const char *buf, unsigned long *maskp,
   int nmaskbits);
extern void bitmap_remap(unsigned long *dst, const unsigned long *src,
  const unsigned long *old, const unsigned long *new, int bits);
extern int bitmap_bitremap(int oldbit,
  const unsigned long *old, const unsigned long *new, int bits);
extern void bitmap_onto(unsigned long *dst, const unsigned long *orig,
  const unsigned long *relmap, int bits);
extern void bitmap_fold(unsigned long *dst, const unsigned long *orig,
  int sz, int bits);
extern int bitmap_find_free_region(unsigned long *bitmap, int bits, int order);
extern void bitmap_release_region(unsigned long *bitmap, int pos, int order);
extern int bitmap_allocate_region(unsigned long *bitmap, int pos, int order);
extern void bitmap_copy_le(void *dst, const unsigned long *src, int nbits);
# 143 "include/linux/bitmap.h"
static inline __attribute__((always_inline)) void bitmap_zero(unsigned long *dst, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = 0UL;
 else {
  int len = (((nbits) + (8 * sizeof(long)) - 1) / (8 * sizeof(long))) * sizeof(unsigned long);
  memset(dst, 0, len);
 }
}

static inline __attribute__((always_inline)) void bitmap_fill(unsigned long *dst, int nbits)
{
 size_t nlongs = (((nbits) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)));
 if (!(__builtin_constant_p(nbits) && (nbits) <= 64)) {
  int len = (nlongs - 1) * sizeof(unsigned long);
  memset(dst, 0xff, len);
 }
 dst[nlongs - 1] = ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL );
}

static inline __attribute__((always_inline)) void bitmap_copy(unsigned long *dst, const unsigned long *src,
   int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src;
 else {
  int len = (((nbits) + (8 * sizeof(long)) - 1) / (8 * sizeof(long))) * sizeof(unsigned long);
  memcpy(dst, src, len);
 }
}

static inline __attribute__((always_inline)) void bitmap_and(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src1 & *src2;
 else
  __bitmap_and(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_or(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src1 | *src2;
 else
  __bitmap_or(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_xor(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src1 ^ *src2;
 else
  __bitmap_xor(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_andnot(unsigned long *dst, const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src1 & ~(*src2);
 else
  __bitmap_andnot(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_complement(unsigned long *dst, const unsigned long *src,
   int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = ~(*src) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL );
 else
  __bitmap_complement(dst, src, nbits);
}

static inline __attribute__((always_inline)) int bitmap_equal(const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! ((*src1 ^ *src2) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_equal(src1, src2, nbits);
}

static inline __attribute__((always_inline)) int bitmap_intersects(const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ((*src1 & *src2) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL )) != 0;
 else
  return __bitmap_intersects(src1, src2, nbits);
}

static inline __attribute__((always_inline)) int bitmap_subset(const unsigned long *src1,
   const unsigned long *src2, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! ((*src1 & ~(*src2)) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_subset(src1, src2, nbits);
}

static inline __attribute__((always_inline)) int bitmap_empty(const unsigned long *src, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! (*src & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_empty(src, nbits);
}

static inline __attribute__((always_inline)) int bitmap_full(const unsigned long *src, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return ! (~(*src) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 else
  return __bitmap_full(src, nbits);
}

static inline __attribute__((always_inline)) int bitmap_weight(const unsigned long *src, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  return hweight_long(*src & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL ));
 return __bitmap_weight(src, nbits);
}

static inline __attribute__((always_inline)) void bitmap_shift_right(unsigned long *dst,
   const unsigned long *src, int n, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = *src >> n;
 else
  __bitmap_shift_right(dst, src, n, nbits);
}

static inline __attribute__((always_inline)) void bitmap_shift_left(unsigned long *dst,
   const unsigned long *src, int n, int nbits)
{
 if ((__builtin_constant_p(nbits) && (nbits) <= 64))
  *dst = (*src << n) & ( ((nbits) % 64) ? (1UL<<((nbits) % 64))-1 : ~0UL );
 else
  __bitmap_shift_left(dst, src, n, nbits);
}

static inline __attribute__((always_inline)) int bitmap_parse(const char *buf, unsigned int buflen,
   unsigned long *maskp, int nmaskbits)
{
 return __bitmap_parse(buf, buflen, 0, maskp, nmaskbits);
}
# 143 "include/linux/cpumask.h" 2

typedef struct cpumask { unsigned long bits[(((255) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; } cpumask_t;
extern cpumask_t _unused_cpumask_arg_;



static inline __attribute__((always_inline)) void __cpu_set(int cpu, volatile cpumask_t *dstp)
{
 set_bit(cpu, dstp->bits);
}


static inline __attribute__((always_inline)) void __cpu_clear(int cpu, volatile cpumask_t *dstp)
{
 clear_bit(cpu, dstp->bits);
}


static inline __attribute__((always_inline)) void __cpus_setall(cpumask_t *dstp, int nbits)
{
 bitmap_fill(dstp->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_clear(cpumask_t *dstp, int nbits)
{
 bitmap_zero(dstp->bits, nbits);
}





static inline __attribute__((always_inline)) int __cpu_test_and_set(int cpu, cpumask_t *addr)
{
 return test_and_set_bit(cpu, addr->bits);
}


static inline __attribute__((always_inline)) void __cpus_and(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_or(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_xor(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_andnot(cpumask_t *dstp, const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_complement(cpumask_t *dstp,
     const cpumask_t *srcp, int nbits)
{
 bitmap_complement(dstp->bits, srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_equal(const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_equal(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_intersects(const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_subset(const cpumask_t *src1p,
     const cpumask_t *src2p, int nbits)
{
 return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_empty(const cpumask_t *srcp, int nbits)
{
 return bitmap_empty(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_full(const cpumask_t *srcp, int nbits)
{
 return bitmap_full(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_weight(const cpumask_t *srcp, int nbits)
{
 return bitmap_weight(srcp->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_shift_right(cpumask_t *dstp,
     const cpumask_t *srcp, int n, int nbits)
{
 bitmap_shift_right(dstp->bits, srcp->bits, n, nbits);
}



static inline __attribute__((always_inline)) void __cpus_shift_left(cpumask_t *dstp,
     const cpumask_t *srcp, int n, int nbits)
{
 bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}
# 287 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int __check_is_bitmap(const unsigned long *bitmap)
{
 return 1;
}
# 299 "include/linux/cpumask.h"
extern const unsigned long
 cpu_bit_bitmap[64 +1][(((255) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];

static inline __attribute__((always_inline)) const struct cpumask *get_cpu_mask(unsigned int cpu)
{
 const unsigned long *p = cpu_bit_bitmap[1 + cpu % 64];
 p -= cpu / 64;
 return ((struct cpumask *)(1 ? (p) : (void *)sizeof(__check_is_bitmap(p))));
}
# 338 "include/linux/cpumask.h"
extern cpumask_t cpu_mask_all;
# 366 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int __cpu_remap(int oldbit,
  const cpumask_t *oldp, const cpumask_t *newp, int nbits)
{
 return bitmap_bitremap(oldbit, oldp->bits, newp->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_remap(cpumask_t *dstp, const cpumask_t *srcp,
  const cpumask_t *oldp, const cpumask_t *newp, int nbits)
{
 bitmap_remap(dstp->bits, srcp->bits, oldp->bits, newp->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_onto(cpumask_t *dstp, const cpumask_t *origp,
  const cpumask_t *relmapp, int nbits)
{
 bitmap_onto(dstp->bits, origp->bits, relmapp->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_fold(cpumask_t *dstp, const cpumask_t *origp,
  int sz, int nbits)
{
 bitmap_fold(dstp->bits, origp->bits, sz, nbits);
}
# 409 "include/linux/cpumask.h"
extern int nr_cpu_ids;

int __first_cpu(const cpumask_t *srcp);
int __next_cpu(int n, const cpumask_t *srcp);
int __any_online_cpu(const cpumask_t *mask);
# 434 "include/linux/cpumask.h"
int __next_cpu_nr(int n, const cpumask_t *srcp);
# 485 "include/linux/cpumask.h"
extern const struct cpumask *const cpu_possible_mask;
extern const struct cpumask *const cpu_online_mask;
extern const struct cpumask *const cpu_present_mask;
extern const struct cpumask *const cpu_active_mask;
# 544 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) unsigned int cpumask_check(unsigned int cpu)
{



 return cpu;
}
# 595 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) unsigned int cpumask_first(const struct cpumask *srcp)
{
 return find_first_bit(((srcp)->bits), 255);
}
# 607 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) unsigned int cpumask_next(int n, const struct cpumask *srcp)
{

 if (n != -1)
  cpumask_check(n);
 return find_next_bit(((srcp)->bits), 255, n+1);
}
# 622 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) unsigned int cpumask_next_zero(int n, const struct cpumask *srcp)
{

 if (n != -1)
  cpumask_check(n);
 return find_next_zero_bit(((srcp)->bits), 255, n+1);
}

int cpumask_next_and(int n, const struct cpumask *, const struct cpumask *);
int cpumask_any_but(const struct cpumask *mask, unsigned int cpu);
# 680 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) void cpumask_set_cpu(unsigned int cpu, struct cpumask *dstp)
{
 set_bit(cpumask_check(cpu), ((dstp)->bits));
}






static inline __attribute__((always_inline)) void cpumask_clear_cpu(int cpu, struct cpumask *dstp)
{
 clear_bit(cpumask_check(cpu), ((dstp)->bits));
}
# 712 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int cpumask_test_and_set_cpu(int cpu, struct cpumask *cpumask)
{
 return test_and_set_bit(cpumask_check(cpu), ((cpumask)->bits));
}





static inline __attribute__((always_inline)) void cpumask_setall(struct cpumask *dstp)
{
 bitmap_fill(((dstp)->bits), 255);
}





static inline __attribute__((always_inline)) void cpumask_clear(struct cpumask *dstp)
{
 bitmap_zero(((dstp)->bits), 255);
}







static inline __attribute__((always_inline)) void cpumask_and(struct cpumask *dstp,
          const struct cpumask *src1p,
          const struct cpumask *src2p)
{
 bitmap_and(((dstp)->bits), ((src1p)->bits),
           ((src2p)->bits), 255);
}







static inline __attribute__((always_inline)) void cpumask_or(struct cpumask *dstp, const struct cpumask *src1p,
         const struct cpumask *src2p)
{
 bitmap_or(((dstp)->bits), ((src1p)->bits),
          ((src2p)->bits), 255);
}







static inline __attribute__((always_inline)) void cpumask_xor(struct cpumask *dstp,
          const struct cpumask *src1p,
          const struct cpumask *src2p)
{
 bitmap_xor(((dstp)->bits), ((src1p)->bits),
           ((src2p)->bits), 255);
}







static inline __attribute__((always_inline)) void cpumask_andnot(struct cpumask *dstp,
      const struct cpumask *src1p,
      const struct cpumask *src2p)
{
 bitmap_andnot(((dstp)->bits), ((src1p)->bits),
       ((src2p)->bits), 255);
}






static inline __attribute__((always_inline)) void cpumask_complement(struct cpumask *dstp,
          const struct cpumask *srcp)
{
 bitmap_complement(((dstp)->bits), ((srcp)->bits),
           255);
}






static inline __attribute__((always_inline)) bool cpumask_equal(const struct cpumask *src1p,
    const struct cpumask *src2p)
{
 return bitmap_equal(((src1p)->bits), ((src2p)->bits),
       255);
}






static inline __attribute__((always_inline)) bool cpumask_intersects(const struct cpumask *src1p,
         const struct cpumask *src2p)
{
 return bitmap_intersects(((src1p)->bits), ((src2p)->bits),
            255);
}






static inline __attribute__((always_inline)) int cpumask_subset(const struct cpumask *src1p,
     const struct cpumask *src2p)
{
 return bitmap_subset(((src1p)->bits), ((src2p)->bits),
        255);
}





static inline __attribute__((always_inline)) bool cpumask_empty(const struct cpumask *srcp)
{
 return bitmap_empty(((srcp)->bits), 255);
}





static inline __attribute__((always_inline)) bool cpumask_full(const struct cpumask *srcp)
{
 return bitmap_full(((srcp)->bits), 255);
}





static inline __attribute__((always_inline)) unsigned int cpumask_weight(const struct cpumask *srcp)
{
 return bitmap_weight(((srcp)->bits), 255);
}







static inline __attribute__((always_inline)) void cpumask_shift_right(struct cpumask *dstp,
           const struct cpumask *srcp, int n)
{
 bitmap_shift_right(((dstp)->bits), ((srcp)->bits), n,
            255);
}







static inline __attribute__((always_inline)) void cpumask_shift_left(struct cpumask *dstp,
          const struct cpumask *srcp, int n)
{
 bitmap_shift_left(((dstp)->bits), ((srcp)->bits), n,
           255);
}






static inline __attribute__((always_inline)) void cpumask_copy(struct cpumask *dstp,
    const struct cpumask *srcp)
{
 bitmap_copy(((dstp)->bits), ((srcp)->bits), 255);
}
# 943 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int cpumask_scnprintf(char *buf, int len,
        const struct cpumask *srcp)
{
 return bitmap_scnprintf(buf, len, ((srcp)->bits), 255);
}
# 957 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int cpumask_parse_user(const char __attribute__((noderef, address_space(1))) *buf, int len,
         struct cpumask *dstp)
{
 return bitmap_parse_user(buf, len, ((dstp)->bits), 255);
}
# 972 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int cpulist_scnprintf(char *buf, int len,
        const struct cpumask *srcp)
{
 return bitmap_scnlistprintf(buf, len, ((srcp)->bits),
        255);
}
# 987 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int cpulist_parse(const char *buf, struct cpumask *dstp)
{
 return bitmap_parselist(buf, ((dstp)->bits), 255);
}






static inline __attribute__((always_inline)) size_t cpumask_size(void)
{


 return (((255) + (8 * sizeof(long)) - 1) / (8 * sizeof(long))) * sizeof(long);
}
# 1030 "include/linux/cpumask.h"
typedef struct cpumask cpumask_var_t[1];

static inline __attribute__((always_inline)) bool alloc_cpumask_var(cpumask_var_t *mask, gfp_t flags)
{
 return true;
}

static inline __attribute__((always_inline)) bool alloc_cpumask_var_node(cpumask_var_t *mask, gfp_t flags,
       int node)
{
 return true;
}

static inline __attribute__((always_inline)) void alloc_bootmem_cpumask_var(cpumask_var_t *mask)
{
}

static inline __attribute__((always_inline)) void free_cpumask_var(cpumask_var_t mask)
{
}

static inline __attribute__((always_inline)) void free_bootmem_cpumask_var(cpumask_var_t mask)
{
}




extern const unsigned long cpu_all_bits[(((255) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];
# 1069 "include/linux/cpumask.h"
void set_cpu_possible(unsigned int cpu, bool possible);
void set_cpu_present(unsigned int cpu, bool present);
void set_cpu_online(unsigned int cpu, bool online);
void set_cpu_active(unsigned int cpu, bool active);
void init_cpu_present(const struct cpumask *src);
void init_cpu_possible(const struct cpumask *src);
void init_cpu_online(const struct cpumask *src);
# 27 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h" 2
# 35 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
static inline __attribute__((always_inline)) void *current_text_addr(void)
{
 void *pc;

 asm volatile("mov $1f, %0; 1:":"=r" (pc));

 return pc;
}
# 58 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
struct cpuinfo_x86 {
 __u8 x86;
 __u8 x86_vendor;
 __u8 x86_model;
 __u8 x86_mask;
# 76 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
 int x86_tlbsize;
 __u8 x86_virt_bits;
 __u8 x86_phys_bits;


 __u8 x86_coreid_bits;

 __u32 extended_cpuid_level;

 int cpuid_level;
 __u32 x86_capability[9];
 char x86_vendor_id[16];
 char x86_model_id[64];

 int x86_cache_size;
 int x86_cache_alignment;
 int x86_power;
 unsigned long loops_per_jiffy;


 cpumask_t llc_shared_map;


 u16 x86_max_cores;
 u16 apicid;
 u16 initial_apicid;
 u16 x86_clflush_size;


 u16 booted_cores;

 u16 phys_proc_id;

 u16 cpu_core_id;

 u16 cpu_index;

 unsigned int x86_hyper_vendor;
} __attribute__((__aligned__((1 << (7)))));
# 133 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
extern struct cpuinfo_x86 boot_cpu_data;
extern struct cpuinfo_x86 new_cpu_data;

extern struct tss_struct doublefault_tss;
extern __u32 cleared_cpu_caps[9];


extern __typeof__(struct cpuinfo_x86) per_cpu__cpu_info;







extern const struct seq_operations cpuinfo_op;

static inline __attribute__((always_inline)) int hlt_works(int cpu)
{



 return 1;

}



extern void cpu_detect(struct cpuinfo_x86 *c);

extern struct pt_regs *idle_regs(struct pt_regs *);

extern void early_cpu_init(void);
extern void identify_boot_cpu(void);
extern void identify_secondary_cpu(struct cpuinfo_x86 *);
extern void print_cpu_info(struct cpuinfo_x86 *);
extern void init_scattered_cpuid_features(struct cpuinfo_x86 *c);
extern unsigned int init_intel_cacheinfo(struct cpuinfo_x86 *c);
extern unsigned short num_cache_leaves;

extern void detect_extended_topology(struct cpuinfo_x86 *c);
extern void detect_ht(struct cpuinfo_x86 *c);

static inline __attribute__((always_inline)) void native_cpuid(unsigned int *eax, unsigned int *ebx,
    unsigned int *ecx, unsigned int *edx)
{

 asm("cpuid"
     : "=a" (*eax),
       "=b" (*ebx),
       "=c" (*ecx),
       "=d" (*edx)
     : "0" (*eax), "2" (*ecx));
}

static inline __attribute__((always_inline)) void load_cr3(pgd_t *pgdir)
{
 (native_write_cr3(__phys_addr((unsigned long)(pgdir))));
}
# 227 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
struct x86_hw_tss {
 u32 reserved1;
 u64 sp0;
 u64 sp1;
 u64 sp2;
 u64 reserved2;
 u64 ist[7];
 u32 reserved3;
 u32 reserved4;
 u16 reserved5;
 u16 io_bitmap_base;

} __attribute__((packed)) __attribute__((__aligned__((1 << (7)))));
# 252 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
struct tss_struct {



 struct x86_hw_tss x86_tss;







 unsigned long io_bitmap[((65536/8)/sizeof(long)) + 1];



 unsigned long io_bitmap_max;
 struct thread_struct *io_bitmap_owner;




 unsigned long stack[64];

} __attribute__((__aligned__((1 << (7)))));

extern __typeof__(struct tss_struct) per_cpu__init_tss;




struct orig_ist {
 unsigned long ist[7];
};



struct i387_fsave_struct {
 u32 cwd;
 u32 swd;
 u32 twd;
 u32 fip;
 u32 fcs;
 u32 foo;
 u32 fos;


 u32 st_space[20];


 u32 status;
};

struct i387_fxsave_struct {
 u16 cwd;
 u16 swd;
 u16 twd;
 u16 fop;
 union {
  struct {
   u64 rip;
   u64 rdp;
  };
  struct {
   u32 fip;
   u32 fcs;
   u32 foo;
   u32 fos;
  };
 };
 u32 mxcsr;
 u32 mxcsr_mask;


 u32 st_space[32];


 u32 xmm_space[64];

 u32 padding[12];

 union {
  u32 padding1[12];
  u32 sw_reserved[12];
 };

} __attribute__((aligned(16)));

struct i387_soft_struct {
 u32 cwd;
 u32 swd;
 u32 twd;
 u32 fip;
 u32 fcs;
 u32 foo;
 u32 fos;

 u32 st_space[20];
 u8 ftop;
 u8 changed;
 u8 lookahead;
 u8 no_update;
 u8 rm;
 u8 alimit;
 struct math_emu_info *info;
 u32 entry_eip;
};

struct xsave_hdr_struct {
 u64 xstate_bv;
 u64 reserved1[2];
 u64 reserved2[5];
} __attribute__((packed));

struct xsave_struct {
 struct i387_fxsave_struct i387;
 struct xsave_hdr_struct xsave_hdr;

} __attribute__ ((packed, aligned (64)));

union thread_xstate {
 struct i387_fsave_struct fsave;
 struct i387_fxsave_struct fxsave;
 struct i387_soft_struct soft;
 struct xsave_struct xsave;
};


extern __typeof__(struct orig_ist) per_cpu__orig_ist;


extern void print_cpu_info(struct cpuinfo_x86 *);
extern unsigned int xstate_size;
extern void free_thread_xstate(struct task_struct *);
extern struct kmem_cache *task_xstate_cachep;
extern void init_scattered_cpuid_features(struct cpuinfo_x86 *c);
extern unsigned int init_intel_cacheinfo(struct cpuinfo_x86 *c);
extern unsigned short num_cache_leaves;

struct thread_struct {

 struct desc_struct tls_array[3];
 unsigned long sp0;
 unsigned long sp;



 unsigned long usersp;
 unsigned short es;
 unsigned short ds;
 unsigned short fsindex;
 unsigned short gsindex;

 unsigned long ip;
 unsigned long fs;
 unsigned long gs;

 unsigned long debugreg0;
 unsigned long debugreg1;
 unsigned long debugreg2;
 unsigned long debugreg3;
 unsigned long debugreg6;
 unsigned long debugreg7;

 unsigned long cr2;
 unsigned long trap_no;
 unsigned long error_code;

 union thread_xstate *xstate;
# 432 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
 unsigned long *io_bitmap_ptr;
 unsigned long iopl;

 unsigned io_bitmap_max;

 unsigned long debugctlmsr;


 struct ds_context *ds_ctx;



 unsigned int bts_ovfl_signal;

};

static inline __attribute__((always_inline)) unsigned long native_get_debugreg(int regno)
{
 unsigned long val = 0;

 switch (regno) {
 case 0:
  asm("mov %%db0, %0" :"=r" (val));
  break;
 case 1:
  asm("mov %%db1, %0" :"=r" (val));
  break;
 case 2:
  asm("mov %%db2, %0" :"=r" (val));
  break;
 case 3:
  asm("mov %%db3, %0" :"=r" (val));
  break;
 case 6:
  asm("mov %%db6, %0" :"=r" (val));
  break;
 case 7:
  asm("mov %%db7, %0" :"=r" (val));
  break;
 default:
  do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"), "i" (472), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0);
 }
 return val;
}

static inline __attribute__((always_inline)) void native_set_debugreg(int regno, unsigned long value)
{
 switch (regno) {
 case 0:
  asm("mov %0, %%db0" ::"r" (value));
  break;
 case 1:
  asm("mov %0, %%db1" ::"r" (value));
  break;
 case 2:
  asm("mov %0, %%db2" ::"r" (value));
  break;
 case 3:
  asm("mov %0, %%db3" ::"r" (value));
  break;
 case 6:
  asm("mov %0, %%db6" ::"r" (value));
  break;
 case 7:
  asm("mov %0, %%db7" ::"r" (value));
  break;
 default:
  do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"), "i" (499), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0);
 }
}




static inline __attribute__((always_inline)) void native_set_iopl_mask(unsigned mask)
{
# 520 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
}

static inline __attribute__((always_inline)) void
native_load_sp0(struct tss_struct *tss, struct thread_struct *thread)
{
 tss->x86_tss.sp0 = thread->sp0;







}

static inline __attribute__((always_inline)) void native_swapgs(void)
{

 asm volatile("swapgs" ::: "memory");

}
# 556 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
static inline __attribute__((always_inline)) void load_sp0(struct tss_struct *tss,
       struct thread_struct *thread)
{
 native_load_sp0(tss, thread);
}
# 571 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
extern unsigned long mmu_cr4_features;

static inline __attribute__((always_inline)) void set_in_cr4(unsigned long mask)
{
 unsigned cr4;

 mmu_cr4_features |= mask;
 cr4 = (native_read_cr4());
 cr4 |= mask;
 (native_write_cr4(cr4));
}

static inline __attribute__((always_inline)) void clear_in_cr4(unsigned long mask)
{
 unsigned cr4;

 mmu_cr4_features &= ~mask;
 cr4 = (native_read_cr4());
 cr4 &= ~mask;
 (native_write_cr4(cr4));
}

typedef struct {
 unsigned long seg;
} mm_segment_t;





extern int kernel_thread(int (*fn)(void *), void *arg, unsigned long flags);


extern void release_thread(struct task_struct *);


extern void prepare_to_copy(struct task_struct *tsk);

unsigned long get_wchan(struct task_struct *p);






static inline __attribute__((always_inline)) void cpuid(unsigned int op,
    unsigned int *eax, unsigned int *ebx,
    unsigned int *ecx, unsigned int *edx)
{
 *eax = op;
 *ecx = 0;
 native_cpuid(eax, ebx, ecx, edx);
}


static inline __attribute__((always_inline)) void cpuid_count(unsigned int op, int count,
          unsigned int *eax, unsigned int *ebx,
          unsigned int *ecx, unsigned int *edx)
{
 *eax = op;
 *ecx = count;
 native_cpuid(eax, ebx, ecx, edx);
}




static inline __attribute__((always_inline)) unsigned int cpuid_eax(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return eax;
}

static inline __attribute__((always_inline)) unsigned int cpuid_ebx(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return ebx;
}

static inline __attribute__((always_inline)) unsigned int cpuid_ecx(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return ecx;
}

static inline __attribute__((always_inline)) unsigned int cpuid_edx(unsigned int op)
{
 unsigned int eax, ebx, ecx, edx;

 cpuid(op, &eax, &ebx, &ecx, &edx);

 return edx;
}


static inline __attribute__((always_inline)) void rep_nop(void)
{
 asm volatile("rep; nop" ::: "memory");
}

static inline __attribute__((always_inline)) void cpu_relax(void)
{
 rep_nop();
}


static inline __attribute__((always_inline)) void sync_core(void)
{
 int tmp;

 asm volatile("cpuid" : "=a" (tmp) : "0" (1)
       : "ebx", "ecx", "edx", "memory");
}

static inline __attribute__((always_inline)) void __monitor(const void *eax, unsigned long ecx,
        unsigned long edx)
{

 asm volatile(".byte 0x0f, 0x01, 0xc8;"
       :: "a" (eax), "c" (ecx), "d"(edx));
}

static inline __attribute__((always_inline)) void __mwait(unsigned long eax, unsigned long ecx)
{

 asm volatile(".byte 0x0f, 0x01, 0xc9;"
       :: "a" (eax), "c" (ecx));
}

static inline __attribute__((always_inline)) void __sti_mwait(unsigned long eax, unsigned long ecx)
{
 do { } while (0);

 asm volatile("sti; .byte 0x0f, 0x01, 0xc9;"
       :: "a" (eax), "c" (ecx));
}

extern void mwait_idle_with_hints(unsigned long eax, unsigned long ecx);

extern void select_idle_routine(const struct cpuinfo_x86 *c);

extern unsigned long boot_option_idle_override;
extern unsigned long idle_halt;
extern unsigned long idle_nomwait;
# 737 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
static inline __attribute__((always_inline)) void wbinvd_halt(void)
{
 asm volatile("mfence":::"memory");

 if ((__builtin_constant_p((0*32+19)) && ( ((((0*32+19))>>5)==0 && (1UL<<(((0*32+19))&31) & ((1<<((0*32+ 0) & 31))|0|(1<<((0*32+ 5) & 31))|(1<<((0*32+ 6) & 31))| (1<<((0*32+ 8) & 31))|(1<<((0*32+13) & 31))|(1<<((0*32+24) & 31))|(1<<((0*32+15) & 31))| (1<<((0*32+25) & 31))|(1<<((0*32+26) & 31))))) || ((((0*32+19))>>5)==1 && (1UL<<(((0*32+19))&31) & ((1<<((1*32+29) & 31))|0))) || ((((0*32+19))>>5)==2 && (1UL<<(((0*32+19))&31) & 0)) || ((((0*32+19))>>5)==3 && (1UL<<(((0*32+19))&31) & ((1<<((3*32+20) & 31))))) || ((((0*32+19))>>5)==4 && (1UL<<(((0*32+19))&31) & 0)) || ((((0*32+19))>>5)==5 && (1UL<<(((0*32+19))&31) & 0)) || ((((0*32+19))>>5)==6 && (1UL<<(((0*32+19))&31) & 0)) || ((((0*32+19))>>5)==7 && (1UL<<(((0*32+19))&31) & 0)) ) ? 1 : (__builtin_constant_p(((0*32+19))) ? constant_test_bit(((0*32+19)), ((unsigned long *)((&boot_cpu_data)->x86_capability))) : variable_test_bit(((0*32+19)), ((unsigned long *)((&boot_cpu_data)->x86_capability))))))
  asm volatile("cli; wbinvd; 1: hlt; jmp 1b" : : : "memory");
 else
  while (1)
   halt();
}

extern void enable_sep_cpu(void);
extern int sysenter_setup(void);


extern struct desc_ptr early_gdt_descr;

extern void cpu_set_gdt(int);
extern void switch_to_new_gdt(void);
extern void cpu_init(void);
extern void init_gdt(int cpu);

static inline __attribute__((always_inline)) unsigned long get_debugctlmsr(void)
{
    unsigned long debugctlmsr = 0;





 ((debugctlmsr) = native_read_msr((0x000001d9)));

    return debugctlmsr;
}

static inline __attribute__((always_inline)) void update_debugctlmsr(unsigned long debugctlmsr)
{




 native_write_msr((0x000001d9), (u32)((u64)(debugctlmsr)), (u32)((u64)(debugctlmsr) >> 32));
}





extern unsigned int machine_id;
extern unsigned int machine_submodel_id;
extern unsigned int BIOS_revision;


extern int bootloader_type;

extern char ignore_fpu_irq;
# 811 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
static inline __attribute__((always_inline)) void prefetch(const void *x)
{
 asm volatile ("661:\n\t" "prefetcht0 (%1)" "\n662:\n" ".section .altinstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661b\n" " " ".quad" " " "663f\n" "	 .byte %c0\n" "	 .byte 662b-661b\n" "	 .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchnta (%1)" "\n664:\n" ".previous" :: "i" ((0*32+25)), "r" (x))


             ;
}






static inline __attribute__((always_inline)) void prefetchw(const void *x)
{
 asm volatile ("661:\n\t" "prefetcht0 (%1)" "\n662:\n" ".section .altinstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661b\n" " " ".quad" " " "663f\n" "	 .byte %c0\n" "	 .byte 662b-661b\n" "	 .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchw (%1)" "\n664:\n" ".previous" :: "i" ((1*32+31)), "r" (x))


             ;
}

static inline __attribute__((always_inline)) void spin_lock_prefetch(const void *x)
{
 prefetchw(x);
}
# 935 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
extern void start_thread(struct pt_regs *regs, unsigned long new_ip,
            unsigned long new_sp);
# 950 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/processor.h"
extern int get_tsc_mode(unsigned long adr);
extern int set_tsc_mode(unsigned int val);
# 23 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ftrace.h" 1
# 24 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h" 1





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cmpxchg.h" 1
# 7 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h" 2
# 39 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) void atomic_add(int i, atomic_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "addl %1,%0"
       : "=m" (v->counter)
       : "ir" (i), "m" (v->counter));
}
# 53 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) void atomic_sub(int i, atomic_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "subl %1,%0"
       : "=m" (v->counter)
       : "ir" (i), "m" (v->counter));
}
# 69 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic_sub_and_test(int i, atomic_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "subl %2,%0; sete %1"
       : "=m" (v->counter), "=qm" (c)
       : "ir" (i), "m" (v->counter) : "memory");
 return c;
}







static inline __attribute__((always_inline)) void atomic_inc(atomic_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "incl %0"
       : "=m" (v->counter)
       : "m" (v->counter));
}







static inline __attribute__((always_inline)) void atomic_dec(atomic_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "decl %0"
       : "=m" (v->counter)
       : "m" (v->counter));
}
# 113 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic_dec_and_test(atomic_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "decl %0; sete %1"
       : "=m" (v->counter), "=qm" (c)
       : "m" (v->counter) : "memory");
 return c != 0;
}
# 131 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic_inc_and_test(atomic_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "incl %0; sete %1"
       : "=m" (v->counter), "=qm" (c)
       : "m" (v->counter) : "memory");
 return c != 0;
}
# 150 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic_add_negative(int i, atomic_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "addl %2,%0; sets %1"
       : "=m" (v->counter), "=qm" (c)
       : "ir" (i), "m" (v->counter) : "memory");
 return c;
}
# 167 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic_add_return(int i, atomic_t *v)
{
 int __i = i;
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "xaddl %0, %1"
       : "+r" (i), "+m" (v->counter)
       : : "memory");
 return i + __i;
}

static inline __attribute__((always_inline)) int atomic_sub_return(int i, atomic_t *v)
{
 return atomic_add_return(-i, v);
}
# 213 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) void atomic64_add(long i, atomic64_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "addq %1,%0"
       : "=m" (v->counter)
       : "er" (i), "m" (v->counter));
}
# 227 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) void atomic64_sub(long i, atomic64_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "subq %1,%0"
       : "=m" (v->counter)
       : "er" (i), "m" (v->counter));
}
# 243 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic64_sub_and_test(long i, atomic64_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "subq %2,%0; sete %1"
       : "=m" (v->counter), "=qm" (c)
       : "er" (i), "m" (v->counter) : "memory");
 return c;
}







static inline __attribute__((always_inline)) void atomic64_inc(atomic64_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "incq %0"
       : "=m" (v->counter)
       : "m" (v->counter));
}







static inline __attribute__((always_inline)) void atomic64_dec(atomic64_t *v)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "decq %0"
       : "=m" (v->counter)
       : "m" (v->counter));
}
# 287 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic64_dec_and_test(atomic64_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "decq %0; sete %1"
       : "=m" (v->counter), "=qm" (c)
       : "m" (v->counter) : "memory");
 return c != 0;
}
# 305 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic64_inc_and_test(atomic64_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "incq %0; sete %1"
       : "=m" (v->counter), "=qm" (c)
       : "m" (v->counter) : "memory");
 return c != 0;
}
# 324 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic64_add_negative(long i, atomic64_t *v)
{
 unsigned char c;

 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "addq %2,%0; sets %1"
       : "=m" (v->counter), "=qm" (c)
       : "er" (i), "m" (v->counter) : "memory");
 return c;
}
# 341 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) long atomic64_add_return(long i, atomic64_t *v)
{
 long __i = i;
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "xaddq %0, %1;"
       : "+r" (i), "+m" (v->counter)
       : : "memory");
 return i + __i;
}

static inline __attribute__((always_inline)) long atomic64_sub_return(long i, atomic64_t *v)
{
 return atomic64_add_return(-i, v);
}
# 373 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic_add_unless(atomic_t *v, int a, int u)
{
 int c, old;
 c = ((v)->counter);
 for (;;) {
  if (__builtin_expect(!!(c == (u)), 0))
   break;
  old = (((__typeof__(*(&(((v))->counter))))__cmpxchg((&(((v))->counter)), (unsigned long)((c)), (unsigned long)((c + (a))), sizeof(*(&(((v))->counter))))));
  if (__builtin_expect(!!(old == c), 1))
   break;
  c = old;
 }
 return c != (u);
}
# 399 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) int atomic64_add_unless(atomic64_t *v, long a, long u)
{
 long c, old;
 c = ((v)->counter);
 for (;;) {
  if (__builtin_expect(!!(c == (u)), 0))
   break;
  old = (((__typeof__(*(&(((v))->counter))))__cmpxchg((&(((v))->counter)), (unsigned long)((c)), (unsigned long)((c + (a))), sizeof(*(&(((v))->counter))))));
  if (__builtin_expect(!!(old == c), 1))
   break;
  c = old;
 }
 return c != (u);
}
# 421 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) short int atomic_inc_short(short int *v)
{
 asm(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "addw $1, %0" : "+m" (*v));
 return *v;
}
# 435 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
static inline __attribute__((always_inline)) void atomic_or_long(unsigned long *v1, unsigned long v2)
{
 asm(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "orq %1, %0" : "+m" (*v1) : "r" (v2));
}
# 458 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h"
# 1 "include/asm-generic/atomic.h" 1
# 23 "include/asm-generic/atomic.h"
typedef atomic64_t atomic_long_t;



static inline __attribute__((always_inline)) long atomic_long_read(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)((v)->counter);
}

static inline __attribute__((always_inline)) void atomic_long_set(atomic_long_t *l, long i)
{
 atomic64_t *v = (atomic64_t *)l;

 (((v)->counter) = (i));
}

static inline __attribute__((always_inline)) void atomic_long_inc(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_inc(v);
}

static inline __attribute__((always_inline)) void atomic_long_dec(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_dec(v);
}

static inline __attribute__((always_inline)) void atomic_long_add(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_add(i, v);
}

static inline __attribute__((always_inline)) void atomic_long_sub(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 atomic64_sub(i, v);
}

static inline __attribute__((always_inline)) int atomic_long_sub_and_test(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_sub_and_test(i, v);
}

static inline __attribute__((always_inline)) int atomic_long_dec_and_test(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_dec_and_test(v);
}

static inline __attribute__((always_inline)) int atomic_long_inc_and_test(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_inc_and_test(v);
}

static inline __attribute__((always_inline)) int atomic_long_add_negative(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return atomic64_add_negative(i, v);
}

static inline __attribute__((always_inline)) long atomic_long_add_return(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_add_return(i, v);
}

static inline __attribute__((always_inline)) long atomic_long_sub_return(long i, atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_sub_return(i, v);
}

static inline __attribute__((always_inline)) long atomic_long_inc_return(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)(atomic64_add_return(1, (v)));
}

static inline __attribute__((always_inline)) long atomic_long_dec_return(atomic_long_t *l)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)(atomic64_sub_return(1, (v)));
}

static inline __attribute__((always_inline)) long atomic_long_add_unless(atomic_long_t *l, long a, long u)
{
 atomic64_t *v = (atomic64_t *)l;

 return (long)atomic64_add_unless(v, a, u);
}
# 459 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic_64.h" 2
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 2
# 25 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h" 2

struct thread_info {
 struct task_struct *task;
 struct exec_domain *exec_domain;
 __u32 flags;
 __u32 status;
 __u32 cpu;
 int preempt_count;

 mm_segment_t addr_limit;
 struct restart_block restart_block;
 void __attribute__((noderef, address_space(1))) *sysenter_return;






};
# 204 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h"
static inline __attribute__((always_inline)) struct thread_info *current_thread_info(void)
{
 struct thread_info *ti;
 ti = (void *)(({ typeof(_proxy_pda.kernelstack) ret__; switch (sizeof(_proxy_pda.kernelstack)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,kernelstack)), "m" (_proxy_pda.kernelstack)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,kernelstack)), "m" (_proxy_pda.kernelstack)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,kernelstack)), "m" (_proxy_pda.kernelstack)); break; default: __bad_pda_field(); } ret__; }) + (5*8) - (((1UL) << 12) << 1));
 return ti;
}


static inline __attribute__((always_inline)) struct thread_info *stack_thread_info(void)
{
 struct thread_info *ti;
 asm("andq %%rsp,%0; " : "=r" (ti) : "0" (~((((1UL) << 12) << 1) - 1)));
 return ti;
}
# 249 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/thread_info.h"
static inline __attribute__((always_inline)) void set_restore_sigmask(void)
{
 struct thread_info *ti = current_thread_info();
 ti->status |= 0x0008;
 set_bit(2, (unsigned long *)&ti->flags);
}



extern void arch_task_cache_init(void);
extern void free_thread_info(struct thread_info *ti);
extern int arch_dup_task_struct(struct task_struct *dst, struct task_struct *src);
# 56 "include/linux/thread_info.h" 2
# 64 "include/linux/thread_info.h"
static inline __attribute__((always_inline)) void set_ti_thread_flag(struct thread_info *ti, int flag)
{
 set_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((always_inline)) void clear_ti_thread_flag(struct thread_info *ti, int flag)
{
 clear_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((always_inline)) int test_and_set_ti_thread_flag(struct thread_info *ti, int flag)
{
 return test_and_set_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((always_inline)) int test_and_clear_ti_thread_flag(struct thread_info *ti, int flag)
{
 return test_and_clear_bit(flag, (unsigned long *)&ti->flags);
}

static inline __attribute__((always_inline)) int test_ti_thread_flag(struct thread_info *ti, int flag)
{
 return (__builtin_constant_p((flag)) ? constant_test_bit((flag), ((unsigned long *)&ti->flags)) : variable_test_bit((flag), ((unsigned long *)&ti->flags)));
}
# 10 "include/linux/preempt.h" 2

# 1 "include/linux/list.h" 1




# 1 "include/linux/poison.h" 1
# 6 "include/linux/list.h" 2
# 1 "include/linux/prefetch.h" 1
# 53 "include/linux/prefetch.h"
static inline __attribute__((always_inline)) void prefetch_range(void *addr, size_t len)
{







}
# 7 "include/linux/list.h" 2
# 19 "include/linux/list.h"
struct list_head {
    struct list_head *__attribute__((recursive)) next, *__attribute__((recursive)) prev;
};






static inline __attribute__((always_inline)) void INIT_LIST_HEAD(struct list_head *list)
{
 list->next = list;
 list->prev = list;
}
# 51 "include/linux/list.h"
extern void __list_add(struct list_head *new,
         struct list_head *prev,
         struct list_head *next);
# 64 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_add(struct list_head *new, struct list_head *head)
{
 __list_add(new, head, head->next);
}
# 78 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_add_tail(struct list_head *new, struct list_head *head)
{
 __list_add(new, head->prev, head);
}
# 90 "include/linux/list.h"
static inline __attribute__((always_inline)) void __list_del(struct list_head * prev, struct list_head * next)
{
 next->prev = prev;
 prev->next = next;
}
# 110 "include/linux/list.h"
extern void list_del(struct list_head *entry);
# 120 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_replace(struct list_head *old,
    struct list_head *new)
{
 new->next = old->next;
 new->next->prev = new;
 new->prev = old->prev;
 new->prev->next = new;
}

static inline __attribute__((always_inline)) void list_replace_init(struct list_head *old,
     struct list_head *new)
{
 list_replace(old, new);
 INIT_LIST_HEAD(old);
}





static inline __attribute__((always_inline)) void list_del_init(struct list_head *entry)
{
 __list_del(entry->prev, entry->next);
 INIT_LIST_HEAD(entry);
}






static inline __attribute__((always_inline)) void list_move(struct list_head *list, struct list_head *head)
{
 __list_del(list->prev, list->next);
 list_add(list, head);
}






static inline __attribute__((always_inline)) void list_move_tail(struct list_head *list,
      struct list_head *head)
{
 __list_del(list->prev, list->next);
 list_add_tail(list, head);
}






static inline __attribute__((always_inline)) int list_is_last(const struct list_head *list,
    const struct list_head *head)
{
 return list->next == head;
}





static inline __attribute__((always_inline)) int list_empty(const struct list_head *head)
{
 return head->next == head;
}
# 202 "include/linux/list.h"
static inline __attribute__((always_inline)) int list_empty_careful(const struct list_head *head)
{
 struct list_head *next = head->next;
 return (next == head) && (next == head->prev);
}





static inline __attribute__((always_inline)) int list_is_singular(const struct list_head *head)
{
 return !list_empty(head) && (head->next == head->prev);
}

static inline __attribute__((always_inline)) void __list_cut_position(struct list_head *list,
  struct list_head *head, struct list_head *entry)
{
 struct list_head *new_first = entry->next;
 list->next = head->next;
 list->next->prev = list;
 list->prev = entry;
 entry->next = list;
 head->next = new_first;
 new_first->prev = head;
}
# 243 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_cut_position(struct list_head *list,
  struct list_head *head, struct list_head *entry)
{
 if (list_empty(head))
  return;
 if (list_is_singular(head) &&
  (head->next != entry && head != entry))
  return;
 if (entry == head)
  INIT_LIST_HEAD(list);
 else
  __list_cut_position(list, head, entry);
}

static inline __attribute__((always_inline)) void __list_splice(const struct list_head *list,
     struct list_head *prev,
     struct list_head *next)
{
 struct list_head *first = list->next;
 struct list_head *last = list->prev;

 first->prev = prev;
 prev->next = first;

 last->next = next;
 next->prev = last;
}






static inline __attribute__((always_inline)) void list_splice(const struct list_head *list,
    struct list_head *head)
{
 if (!list_empty(list))
  __list_splice(list, head, head->next);
}






static inline __attribute__((always_inline)) void list_splice_tail(struct list_head *list,
    struct list_head *head)
{
 if (!list_empty(list))
  __list_splice(list, head->prev, head);
}
# 302 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_splice_init(struct list_head *list,
        struct list_head *head)
{
 if (!list_empty(list)) {
  __list_splice(list, head, head->next);
  INIT_LIST_HEAD(list);
 }
}
# 319 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_splice_tail_init(struct list_head *list,
      struct list_head *head)
{
 if (!list_empty(list)) {
  __list_splice(list, head->prev, head);
  INIT_LIST_HEAD(list);
 }
}
# 540 "include/linux/list.h"
struct hlist_head {
 struct hlist_node *first;
};

struct hlist_node {
 struct hlist_node *next, **pprev;
};




static inline __attribute__((always_inline)) void INIT_HLIST_NODE(struct hlist_node *h)
{
 h->next = ((void *)0);
 h->pprev = ((void *)0);
}

static inline __attribute__((always_inline)) int hlist_unhashed(const struct hlist_node *h)
{
 return !h->pprev;
}

static inline __attribute__((always_inline)) int hlist_empty(const struct hlist_head *h)
{
 return !h->first;
}

static inline __attribute__((always_inline)) void __hlist_del(struct hlist_node *n)
{
 struct hlist_node *next = n->next;
 struct hlist_node **pprev = n->pprev;
 *pprev = next;
 if (next)
  next->pprev = pprev;
}

static inline __attribute__((always_inline)) void hlist_del(struct hlist_node *n)
{
 __hlist_del(n);
 n->next = ((void *) 0x00100100);
 n->pprev = ((void *) 0x00200200);
}

static inline __attribute__((always_inline)) void hlist_del_init(struct hlist_node *n)
{
 if (!hlist_unhashed(n)) {
  __hlist_del(n);
  INIT_HLIST_NODE(n);
 }
}

static inline __attribute__((always_inline)) void hlist_add_head(struct hlist_node *n, struct hlist_head *h)
{
 struct hlist_node *first = h->first;
 n->next = first;
 if (first)
  first->pprev = &n->next;
 h->first = n;
 n->pprev = &h->first;
}


static inline __attribute__((always_inline)) void hlist_add_before(struct hlist_node *n,
     struct hlist_node *next)
{
 n->pprev = next->pprev;
 n->next = next;
 next->pprev = &n->next;
 *(n->pprev) = n;
}

static inline __attribute__((always_inline)) void hlist_add_after(struct hlist_node *n,
     struct hlist_node *next)
{
 next->next = n->next;
 n->next = next;
 next->pprev = &n->next;

 if(next->next)
  next->next->pprev = &next->next;
}





static inline __attribute__((always_inline)) void hlist_move_list(struct hlist_head *old,
       struct hlist_head *new)
{
 new->first = old->first;
 if (new->first)
  new->first->pprev = &new->first;
 old->first = ((void *)0);
}
# 12 "include/linux/preempt.h" 2
# 51 "include/linux/spinlock.h" 2




# 1 "include/linux/stringify.h" 1
# 56 "include/linux/spinlock.h" 2
# 1 "include/linux/bottom_half.h" 1



extern void local_bh_disable(void);
extern void _local_bh_enable(void);
extern void local_bh_enable(void);
extern void local_bh_enable_ip(unsigned long ip);
# 57 "include/linux/spinlock.h" 2
# 80 "include/linux/spinlock.h"
# 1 "include/linux/spinlock_types.h" 1
# 13 "include/linux/spinlock_types.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock_types.h" 1







typedef struct raw_spinlock {
 unsigned int slock;
} raw_spinlock_t;



typedef struct {
 unsigned int lock;
} raw_rwlock_t;
# 14 "include/linux/spinlock_types.h" 2




# 1 "include/linux/lockdep.h" 1
# 12 "include/linux/lockdep.h"
struct task_struct;
struct lockdep_map;





# 1 "include/linux/debug_locks.h" 1





struct task_struct;

extern int debug_locks;
extern int debug_locks_silent;




extern int debug_locks_off(void);
# 40 "include/linux/debug_locks.h"
struct task_struct;


extern void debug_show_all_locks(void);
extern void __debug_show_held_locks(struct task_struct *task);
extern void debug_show_held_locks(struct task_struct *task);
extern void debug_check_no_locks_freed(const void *from, unsigned long len);
extern void debug_check_no_locks_held(struct task_struct *task);
# 20 "include/linux/lockdep.h" 2
# 1 "include/linux/stacktrace.h" 1



struct task_struct;


struct stack_trace {
 unsigned int nr_entries, max_entries;
 unsigned long *entries;
 int skip;
};

extern void save_stack_trace(struct stack_trace *trace);
extern void save_stack_trace_tsk(struct task_struct *tsk,
    struct stack_trace *trace);

extern void print_stack_trace(struct stack_trace *trace, int spaces);


extern void save_stack_trace_user(struct stack_trace *trace);
# 21 "include/linux/lockdep.h" 2




enum lock_usage_bit
{
 LOCK_USED = 0,
 LOCK_USED_IN_HARDIRQ,
 LOCK_USED_IN_SOFTIRQ,
 LOCK_ENABLED_SOFTIRQS,
 LOCK_ENABLED_HARDIRQS,
 LOCK_USED_IN_HARDIRQ_READ,
 LOCK_USED_IN_SOFTIRQ_READ,
 LOCK_ENABLED_SOFTIRQS_READ,
 LOCK_ENABLED_HARDIRQS_READ,
 LOCK_USAGE_STATES
};
# 68 "include/linux/lockdep.h"
struct lockdep_subclass_key {
 char __one_byte;
} __attribute__ ((__packed__));

struct lock_class_key {
 struct lockdep_subclass_key subkeys[8UL];
};






struct lock_class {



 struct list_head hash_entry;




 struct list_head lock_entry;

 struct lockdep_subclass_key *key;
 unsigned int subclass;
 unsigned int dep_gen_id;




 unsigned long usage_mask;
 struct stack_trace usage_traces[LOCK_USAGE_STATES];






 struct list_head locks_after, locks_before;





 unsigned int version;




 unsigned long ops;

 const char *name;
 int name_version;





};
# 166 "include/linux/lockdep.h"
struct lockdep_map {
 struct lock_class_key *key;
 struct lock_class *class_cache;
 const char *name;




};





struct lock_list {
 struct list_head entry;
 struct lock_class *class;
 struct stack_trace trace;
 int distance;
};




struct lock_chain {
 u8 irq_context;
 u8 depth;
 u16 base;
 struct list_head entry;
 u64 chain_key;
};
# 206 "include/linux/lockdep.h"
struct held_lock {
# 221 "include/linux/lockdep.h"
 u64 prev_chain_key;
 unsigned long acquire_ip;
 struct lockdep_map *instance;
 struct lockdep_map *nest_lock;




 unsigned int class_idx:13;
# 243 "include/linux/lockdep.h"
 unsigned int irq_context:2;
 unsigned int trylock:1;
 unsigned int read:2;
 unsigned int check:2;
 unsigned int hardirqs_off:1;
};




extern void lockdep_init(void);
extern void lockdep_info(void);
extern void lockdep_reset(void);
extern void lockdep_reset_lock(struct lockdep_map *lock);
extern void lockdep_free_key_range(void *start, unsigned long size);
extern void lockdep_sys_exit(void);

extern void lockdep_off(void);
extern void lockdep_on(void);







extern void lockdep_init_map(struct lockdep_map *lock, const char *name,
        struct lock_class_key *key, int subclass);
# 310 "include/linux/lockdep.h"
extern void lock_acquire(struct lockdep_map *lock, unsigned int subclass,
    int trylock, int read, int check,
    struct lockdep_map *nest_lock, unsigned long ip);

extern void lock_release(struct lockdep_map *lock, int nested,
    unsigned long ip);

extern void lock_set_class(struct lockdep_map *lock, const char *name,
      struct lock_class_key *key, unsigned int subclass,
      unsigned long ip);

static inline __attribute__((always_inline)) void lock_set_subclass(struct lockdep_map *lock,
  unsigned int subclass, unsigned long ip)
{
 lock_set_class(lock, lock->name, lock->key, subclass, ip);
}
# 394 "include/linux/lockdep.h"
extern void early_init_irq_lock_class(void);
# 406 "include/linux/lockdep.h"
static inline __attribute__((always_inline)) void early_boot_irqs_off(void)
{
}
static inline __attribute__((always_inline)) void early_boot_irqs_on(void)
{
}
static inline __attribute__((always_inline)) void print_irqtrace_events(struct task_struct *curr)
{
}
# 19 "include/linux/spinlock_types.h" 2

typedef struct {
 raw_spinlock_t raw_lock;




 unsigned int magic, owner_cpu;
 void *owner;


 struct lockdep_map dep_map;

} spinlock_t;



typedef struct {
 raw_rwlock_t raw_lock;




 unsigned int magic, owner_cpu;
 void *owner;


 struct lockdep_map dep_map;

} rwlock_t;
# 81 "include/linux/spinlock.h" 2

extern int __attribute__((section(".spinlock.text"))) generic__raw_read_trylock(raw_rwlock_t *lock);





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/rwlock.h" 1
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h" 2



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/paravirt.h" 1
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h" 2
# 61 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) void __ticket_spin_lock(raw_spinlock_t *lock)
{
 short inc = 0x0100;

 asm volatile (
  ".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "xaddw %w0, %1\n"
  "1:\t"
  "cmpb %h0, %b0\n\t"
  "je 2f\n\t"
  "rep ; nop\n\t"
  "movb %1, %b0\n\t"

  "jmp 1b\n"
  "2:"
  : "+Q" (inc), "+m" (lock->slock)
  :
  : "memory", "cc");
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) int __ticket_spin_trylock(raw_spinlock_t *lock)
{
 int tmp, new;

 asm volatile("movzwl %2, %0\n\t"
       "cmpb %h0,%b0\n\t"
       "leal 0x100(%" "q" "0), %1\n\t"
       "jne 1f\n\t"
       ".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "cmpxchgw %w1,%2\n\t"
       "1:"
       "sete %b1\n\t"
       "movzbl %b1,%0\n\t"
       : "=&a" (tmp), "=&q" (new), "+m" (lock->slock)
       :
       : "memory", "cc");

 return tmp;
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) void __ticket_spin_unlock(raw_spinlock_t *lock)
{
 asm volatile( "incb %0"
       : "+m" (lock->slock)
       :
       : "memory", "cc");
}
# 161 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h"
static inline __attribute__((always_inline)) int __ticket_spin_is_locked(raw_spinlock_t *lock)
{
 int tmp = (*(volatile typeof(lock->slock) *)&(lock->slock));

 return !!(((tmp >> 8) ^ tmp) & ((1 << 8) - 1));
}

static inline __attribute__((always_inline)) int __ticket_spin_is_contended(raw_spinlock_t *lock)
{
 int tmp = (*(volatile typeof(lock->slock) *)&(lock->slock));

 return (((tmp >> 8) - tmp) & ((1 << 8) - 1)) > 1;
}
# 239 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h"
static inline __attribute__((always_inline)) int __raw_spin_is_locked(raw_spinlock_t *lock)
{
 return __ticket_spin_is_locked(lock);
}

static inline __attribute__((always_inline)) int __raw_spin_is_contended(raw_spinlock_t *lock)
{
 return __ticket_spin_is_contended(lock);
}


static inline __attribute__((always_inline)) __attribute__((always_inline)) void __raw_spin_lock(raw_spinlock_t *lock)
{
 __ticket_spin_lock(lock);
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) int __raw_spin_trylock(raw_spinlock_t *lock)
{
 return __ticket_spin_trylock(lock);
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) void __raw_spin_unlock(raw_spinlock_t *lock)
{
 __ticket_spin_unlock(lock);
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) void __raw_spin_lock_flags(raw_spinlock_t *lock,
        unsigned long flags)
{
 __raw_spin_lock(lock);
}



static inline __attribute__((always_inline)) void __raw_spin_unlock_wait(raw_spinlock_t *lock)
{
 while (__raw_spin_is_locked(lock))
  cpu_relax();
}
# 297 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/spinlock.h"
static inline __attribute__((always_inline)) int __raw_read_can_lock(raw_rwlock_t *lock)
{
 return (int)(lock)->lock > 0;
}





static inline __attribute__((always_inline)) int __raw_write_can_lock(raw_rwlock_t *lock)
{
 return (lock)->lock == 0x01000000;
}

static inline __attribute__((always_inline)) void __raw_read_lock(raw_rwlock_t *rw)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " " subl $1,(%0)\n\t"
       "jns 1f\n"
       "call __read_lock_failed\n\t"
       "1:\n"
       ::"D" (rw) : "memory");
}

static inline __attribute__((always_inline)) void __raw_write_lock(raw_rwlock_t *rw)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " " subl %1,(%0)\n\t"
       "jz 1f\n"
       "call __write_lock_failed\n\t"
       "1:\n"
       ::"D" (rw), "i" (0x01000000) : "memory");
}

static inline __attribute__((always_inline)) int __raw_read_trylock(raw_rwlock_t *lock)
{
 atomic_t *count = (atomic_t *)lock;

 atomic_dec(count);
 if (((count)->counter) >= 0)
  return 1;
 atomic_inc(count);
 return 0;
}

static inline __attribute__((always_inline)) int __raw_write_trylock(raw_rwlock_t *lock)
{
 atomic_t *count = (atomic_t *)lock;

 if (atomic_sub_and_test(0x01000000, count))
  return 1;
 atomic_add(0x01000000, count);
 return 0;
}

static inline __attribute__((always_inline)) void __raw_read_unlock(raw_rwlock_t *rw)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "incl %0" :"+m" (rw->lock) : : "memory");
}

static inline __attribute__((always_inline)) void __raw_write_unlock(raw_rwlock_t *rw)
{
 asm volatile(".section .smp_locks,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661f\n" ".previous\n" "661:\n\tlock; " "addl %1, %0"
       : "+m" (rw->lock) : "i" (0x01000000) : "memory");
}
# 89 "include/linux/spinlock.h" 2





  extern void __spin_lock_init(spinlock_t *lock, const char *name,
          struct lock_class_key *key);
# 109 "include/linux/spinlock.h"
  extern void __rwlock_init(rwlock_t *lock, const char *name,
       struct lock_class_key *key);
# 145 "include/linux/spinlock.h"
# 1 "include/linux/spinlock_api_smp.h" 1
# 18 "include/linux/spinlock_api_smp.h"
int in_lock_functions(unsigned long addr);



void __attribute__((section(".spinlock.text"))) _spin_lock(spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _spin_lock_nested(spinlock_t *lock, int subclass)
       ;
void __attribute__((section(".spinlock.text"))) _spin_lock_nest_lock(spinlock_t *lock, struct lockdep_map *map)
       ;
void __attribute__((section(".spinlock.text"))) _read_lock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _write_lock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _spin_lock_bh(spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _read_lock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _write_lock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _spin_lock_irq(spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _read_lock_irq(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _write_lock_irq(rwlock_t *lock) ;
unsigned long __attribute__((section(".spinlock.text"))) _spin_lock_irqsave(spinlock_t *lock)
       ;
unsigned long __attribute__((section(".spinlock.text"))) _spin_lock_irqsave_nested(spinlock_t *lock, int subclass)
       ;
unsigned long __attribute__((section(".spinlock.text"))) _read_lock_irqsave(rwlock_t *lock)
       ;
unsigned long __attribute__((section(".spinlock.text"))) _write_lock_irqsave(rwlock_t *lock)
       ;
int __attribute__((section(".spinlock.text"))) _spin_trylock(spinlock_t *lock);
int __attribute__((section(".spinlock.text"))) _read_trylock(rwlock_t *lock);
int __attribute__((section(".spinlock.text"))) _write_trylock(rwlock_t *lock);
int __attribute__((section(".spinlock.text"))) _spin_trylock_bh(spinlock_t *lock);
void __attribute__((section(".spinlock.text"))) _spin_unlock(spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _read_unlock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _write_unlock(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _spin_unlock_bh(spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _read_unlock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _write_unlock_bh(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _spin_unlock_irq(spinlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _read_unlock_irq(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _write_unlock_irq(rwlock_t *lock) ;
void __attribute__((section(".spinlock.text"))) _spin_unlock_irqrestore(spinlock_t *lock, unsigned long flags)
       ;
void __attribute__((section(".spinlock.text"))) _read_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
       ;
void __attribute__((section(".spinlock.text"))) _write_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
       ;
# 146 "include/linux/spinlock.h" 2





 extern void _raw_spin_lock(spinlock_t *lock);

 extern int _raw_spin_trylock(spinlock_t *lock);
 extern void _raw_spin_unlock(spinlock_t *lock);
 extern void _raw_read_lock(rwlock_t *lock);
 extern int _raw_read_trylock(rwlock_t *lock);
 extern void _raw_read_unlock(rwlock_t *lock);
 extern void _raw_write_lock(rwlock_t *lock);
 extern int _raw_write_trylock(rwlock_t *lock);
 extern void _raw_write_unlock(rwlock_t *lock);
# 353 "include/linux/spinlock.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 354 "include/linux/spinlock.h" 2
# 362 "include/linux/spinlock.h"
extern int _atomic_dec_and_lock(atomic_t *atomic, spinlock_t *lock);
# 5 "include/linux/vmalloc.h" 2



struct vm_area_struct;
# 26 "include/linux/vmalloc.h"
struct vm_struct {
 struct vm_struct *next;
 void *addr;
 unsigned long size;
 unsigned long flags;
 struct page **pages;
 unsigned int nr_pages;
 unsigned long phys_addr;
 void *caller;
};




extern void vm_unmap_ram(const void *mem, unsigned int count);
extern void *vm_map_ram(struct page **pages, unsigned int count,
    int node, pgprot_t prot);
extern void vm_unmap_aliases(void);


extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) vmalloc_init(void);






extern void *vmalloc(unsigned long size);
extern void *vmalloc_user(unsigned long size);
extern void *vmalloc_node(unsigned long size, int node);
extern void *vmalloc_exec(unsigned long size);
extern void *vmalloc_32(unsigned long size);
extern void *vmalloc_32_user(unsigned long size);
extern void *__vmalloc(unsigned long size, gfp_t gfp_mask, pgprot_t prot);
extern void *__vmalloc_area(struct vm_struct *area, gfp_t gfp_mask,
    pgprot_t prot);
extern void vfree(const void *addr);

extern void *vmap(struct page **pages, unsigned int count,
   unsigned long flags, pgprot_t prot);
extern void vunmap(const void *addr);

extern int remap_vmalloc_range(struct vm_area_struct *vma, void *addr,
       unsigned long pgoff);
void vmalloc_sync_all(void);





static inline __attribute__((always_inline)) size_t get_vm_area_size(const struct vm_struct *area)
{

 return area->size - ((1UL) << 12);
}

extern struct vm_struct *get_vm_area(unsigned long size, unsigned long flags);
extern struct vm_struct *get_vm_area_caller(unsigned long size,
     unsigned long flags, void *caller);
extern struct vm_struct *__get_vm_area(unsigned long size, unsigned long flags,
     unsigned long start, unsigned long end);
extern struct vm_struct *__get_vm_area_caller(unsigned long size,
     unsigned long flags,
     unsigned long start, unsigned long end,
     void *caller);
extern struct vm_struct *get_vm_area_node(unsigned long size,
       unsigned long flags, int node,
       gfp_t gfp_mask);
extern struct vm_struct *remove_vm_area(const void *addr);

extern int map_vm_area(struct vm_struct *area, pgprot_t prot,
   struct page ***pages);
extern void unmap_kernel_range(unsigned long addr, unsigned long size);


extern struct vm_struct *alloc_vm_area(size_t size);
extern void free_vm_area(struct vm_struct *area);


extern long vread(char *buf, char *addr, unsigned long count);
extern long vwrite(char *buf, char *addr, unsigned long count);




extern rwlock_t vmlist_lock;
extern struct vm_struct *vmlist;
# 144 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_64.h" 2






static inline __attribute__((always_inline)) unsigned long virt_to_phys(volatile void *address)
{
 return __phys_addr((unsigned long)(address));
}

static inline __attribute__((always_inline)) void *phys_to_virt(unsigned long address)
{
 return ((void *)((unsigned long)(address)+((unsigned long)(0xffff880000000000UL))));
}







# 1 "include/asm-generic/iomap.h" 1
# 28 "include/asm-generic/iomap.h"
extern unsigned int ioread8(void __attribute__((noderef, address_space(2))) *);
extern unsigned int ioread16(void __attribute__((noderef, address_space(2))) *);
extern unsigned int ioread16be(void __attribute__((noderef, address_space(2))) *);
extern unsigned int ioread32(void __attribute__((noderef, address_space(2))) *);
extern unsigned int ioread32be(void __attribute__((noderef, address_space(2))) *);

extern void iowrite8(u8, void __attribute__((noderef, address_space(2))) *);
extern void iowrite16(u16, void __attribute__((noderef, address_space(2))) *);
extern void iowrite16be(u16, void __attribute__((noderef, address_space(2))) *);
extern void iowrite32(u32, void __attribute__((noderef, address_space(2))) *);
extern void iowrite32be(u32, void __attribute__((noderef, address_space(2))) *);
# 51 "include/asm-generic/iomap.h"
extern void ioread8_rep(void __attribute__((noderef, address_space(2))) *port, void *buf, unsigned long count);
extern void ioread16_rep(void __attribute__((noderef, address_space(2))) *port, void *buf, unsigned long count);
extern void ioread32_rep(void __attribute__((noderef, address_space(2))) *port, void *buf, unsigned long count);

extern void iowrite8_rep(void __attribute__((noderef, address_space(2))) *port, const void *buf, unsigned long count);
extern void iowrite16_rep(void __attribute__((noderef, address_space(2))) *port, const void *buf, unsigned long count);
extern void iowrite32_rep(void __attribute__((noderef, address_space(2))) *port, const void *buf, unsigned long count);


extern void __attribute__((noderef, address_space(2))) *ioport_map(unsigned long port, unsigned int nr);
extern void ioport_unmap(void __attribute__((noderef, address_space(2))) *);






struct pci_dev;
extern void __attribute__((noderef, address_space(2))) *pci_iomap(struct pci_dev *dev, int bar, unsigned long max);
extern void pci_iounmap(struct pci_dev *dev, void __attribute__((noderef, address_space(2))) *);
# 167 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_64.h" 2






extern void __attribute__((noderef, address_space(2))) *ioremap_nocache(resource_size_t offset, unsigned long size);
extern void __attribute__((noderef, address_space(2))) *ioremap_cache(resource_size_t offset, unsigned long size);
extern void __attribute__((noderef, address_space(2))) *ioremap_prot(resource_size_t offset, unsigned long size,
    unsigned long prot_val);




static inline __attribute__((always_inline)) void __attribute__((noderef, address_space(2))) *ioremap(resource_size_t offset, unsigned long size)
{
 return ioremap_nocache(offset, size);
}

extern void iounmap(volatile void __attribute__((noderef, address_space(2))) *addr);

extern void __attribute__((noderef, address_space(2))) *fix_ioremap(unsigned idx, unsigned long phys);
# 206 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_64.h"
void __memcpy_fromio(void *, unsigned long, unsigned);
void __memcpy_toio(unsigned long, const void *, unsigned);

static inline __attribute__((always_inline)) void memcpy_fromio(void *to, const volatile void __attribute__((noderef, address_space(2))) *from,
     unsigned len)
{
 __memcpy_fromio(to, (unsigned long)from, len);
}

static inline __attribute__((always_inline)) void memcpy_toio(volatile void __attribute__((noderef, address_space(2))) *to, const void *from,
          unsigned len)
{
 __memcpy_toio((unsigned long)to, from, len);
}

void memset_io(volatile void __attribute__((noderef, address_space(2))) *a, int b, size_t c);
# 87 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io.h" 2


extern void *xlate_dev_mem_ptr(unsigned long phys);
extern void unxlate_dev_mem_ptr(unsigned long phys, void *addr);

extern int ioremap_change_attr(unsigned long vaddr, unsigned long size,
    unsigned long prot_val);
extern void __attribute__((noderef, address_space(2))) *ioremap_wc(unsigned long offset, unsigned long size);






extern void early_ioremap_init(void);
extern void early_ioremap_reset(void);
extern void __attribute__((noderef, address_space(2))) *early_ioremap(unsigned long offset, unsigned long size);
extern void __attribute__((noderef, address_space(2))) *early_memremap(unsigned long offset, unsigned long size);
extern void early_iounmap(void __attribute__((noderef, address_space(2))) *addr, unsigned long size);
extern void __attribute__((noderef, address_space(2))) *fix_ioremap(unsigned idx, unsigned long phys);
# 30 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 1 "include/linux/delay.h" 1
# 12 "include/linux/delay.h"
extern unsigned long loops_per_jiffy;

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/delay.h" 1
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/delay.h"
extern void __bad_udelay(void);
extern void __bad_ndelay(void);

extern void __udelay(unsigned long usecs);
extern void __ndelay(unsigned long nsecs);
extern void __const_udelay(unsigned long xloops);
extern void __delay(unsigned long loops);
# 29 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/delay.h"
void use_tsc_delay(void);
# 15 "include/linux/delay.h" 2
# 44 "include/linux/delay.h"
extern unsigned long lpj_fine;
void calibrate_delay(void);
void msleep(unsigned int msecs);
unsigned long msleep_interruptible(unsigned int msecs);

static inline __attribute__((always_inline)) void ssleep(unsigned int seconds)
{
 msleep(seconds * 1000);
}
# 31 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 1 "include/linux/interrupt.h" 1
# 10 "include/linux/interrupt.h"
# 1 "include/linux/irqreturn.h" 1
# 19 "include/linux/irqreturn.h"
typedef int irqreturn_t;
# 11 "include/linux/interrupt.h" 2
# 1 "include/linux/irqnr.h" 1
# 25 "include/linux/irqnr.h"
extern int nr_irqs;
extern struct irq_desc *irq_to_desc(unsigned int irq);
# 12 "include/linux/interrupt.h" 2
# 1 "include/linux/hardirq.h" 1




# 1 "include/linux/smp_lock.h" 1




# 1 "include/linux/sched.h" 1
# 44 "include/linux/sched.h"
struct sched_param {
 int sched_priority;
};



# 1 "include/linux/capability.h" 1
# 18 "include/linux/capability.h"
struct task_struct;
# 40 "include/linux/capability.h"
typedef struct __user_cap_header_struct {
 __u32 version;
 int pid;
} __attribute__((noderef, address_space(1))) *cap_user_header_t;

typedef struct __user_cap_data_struct {
        __u32 effective;
        __u32 permitted;
        __u32 inheritable;
} __attribute__((noderef, address_space(1))) *cap_user_data_t;
# 72 "include/linux/capability.h"
struct vfs_cap_data {
 __le32 magic_etc;
 struct {
  __le32 permitted;
  __le32 inheritable;
 } data[2];
};
# 99 "include/linux/capability.h"
typedef struct kernel_cap_struct {
 __u32 cap[2];
} kernel_cap_t;


struct cpu_vfs_cap_data {
 __u32 magic_etc;
 kernel_cap_t permitted;
 kernel_cap_t inheritable;
};
# 429 "include/linux/capability.h"
static inline __attribute__((always_inline)) kernel_cap_t cap_combine(const kernel_cap_t a,
           const kernel_cap_t b)
{
 kernel_cap_t dest;
 do { unsigned __capi; for (__capi = 0; __capi < 2; ++__capi) { dest.cap[__capi] = a.cap[__capi] | b.cap[__capi]; } } while (0);
 return dest;
}

static inline __attribute__((always_inline)) kernel_cap_t cap_intersect(const kernel_cap_t a,
      const kernel_cap_t b)
{
 kernel_cap_t dest;
 do { unsigned __capi; for (__capi = 0; __capi < 2; ++__capi) { dest.cap[__capi] = a.cap[__capi] & b.cap[__capi]; } } while (0);
 return dest;
}

static inline __attribute__((always_inline)) kernel_cap_t cap_drop(const kernel_cap_t a,
        const kernel_cap_t drop)
{
 kernel_cap_t dest;
 do { unsigned __capi; for (__capi = 0; __capi < 2; ++__capi) { dest.cap[__capi] = a.cap[__capi] &~ drop.cap[__capi]; } } while (0);
 return dest;
}

static inline __attribute__((always_inline)) kernel_cap_t cap_invert(const kernel_cap_t c)
{
 kernel_cap_t dest;
 do { unsigned __capi; for (__capi = 0; __capi < 2; ++__capi) { dest.cap[__capi] = ~ c.cap[__capi]; } } while (0);
 return dest;
}

static inline __attribute__((always_inline)) int cap_isclear(const kernel_cap_t a)
{
 unsigned __capi;
 for (__capi = 0; __capi < 2; ++__capi) {
  if (a.cap[__capi] != 0)
   return 0;
 }
 return 1;
}
# 477 "include/linux/capability.h"
static inline __attribute__((always_inline)) int cap_issubset(const kernel_cap_t a, const kernel_cap_t set)
{
 kernel_cap_t dest;
 dest = cap_drop(a, set);
 return cap_isclear(dest);
}



static inline __attribute__((always_inline)) int cap_is_fs_cap(int cap)
{
 const kernel_cap_t __cap_fs_set = ((kernel_cap_t){{ ((1 << ((0) & 31)) | (1 << ((1) & 31)) | (1 << ((2) & 31)) | (1 << ((3) & 31)) | (1 << ((4) & 31))), ((1 << ((32) & 31))) } });
 return !!((1 << ((cap) & 31)) & __cap_fs_set.cap[((cap) >> 5)]);
}

static inline __attribute__((always_inline)) kernel_cap_t cap_drop_fs_set(const kernel_cap_t a)
{
 const kernel_cap_t __cap_fs_set = ((kernel_cap_t){{ ((1 << ((0) & 31)) | (1 << ((1) & 31)) | (1 << ((2) & 31)) | (1 << ((3) & 31)) | (1 << ((4) & 31))), ((1 << ((32) & 31))) } });
 return cap_drop(a, __cap_fs_set);
}

static inline __attribute__((always_inline)) kernel_cap_t cap_raise_fs_set(const kernel_cap_t a,
         const kernel_cap_t permitted)
{
 const kernel_cap_t __cap_fs_set = ((kernel_cap_t){{ ((1 << ((0) & 31)) | (1 << ((1) & 31)) | (1 << ((2) & 31)) | (1 << ((3) & 31)) | (1 << ((4) & 31))), ((1 << ((32) & 31))) } });
 return cap_combine(a,
      cap_intersect(permitted, __cap_fs_set));
}

static inline __attribute__((always_inline)) kernel_cap_t cap_drop_nfsd_set(const kernel_cap_t a)
{
 const kernel_cap_t __cap_fs_set = ((kernel_cap_t){{ ((1 << ((0) & 31)) | (1 << ((1) & 31)) | (1 << ((2) & 31)) | (1 << ((3) & 31)) | (1 << ((4) & 31))) | (1 << ((24) & 31)) | (1 << ((27) & 31)), ((1 << ((32) & 31))) } });
 return cap_drop(a, __cap_fs_set);
}

static inline __attribute__((always_inline)) kernel_cap_t cap_raise_nfsd_set(const kernel_cap_t a,
           const kernel_cap_t permitted)
{
 const kernel_cap_t __cap_nfsd_set = ((kernel_cap_t){{ ((1 << ((0) & 31)) | (1 << ((1) & 31)) | (1 << ((2) & 31)) | (1 << ((3) & 31)) | (1 << ((4) & 31))) | (1 << ((24) & 31)) | (1 << ((27) & 31)), ((1 << ((32) & 31))) } });
 return cap_combine(a,
      cap_intersect(permitted, __cap_nfsd_set));
}

extern const kernel_cap_t __cap_empty_set;
extern const kernel_cap_t __cap_full_set;
extern const kernel_cap_t __cap_init_eff_set;
# 550 "include/linux/capability.h"
extern int capable(int cap);


struct dentry;
extern int get_vfs_caps_from_disk(const struct dentry *dentry, struct cpu_vfs_cap_data *cpu_caps);
# 51 "include/linux/sched.h" 2



# 1 "include/linux/timex.h" 1
# 56 "include/linux/timex.h"
# 1 "include/linux/time.h" 1







# 1 "include/linux/seqlock.h" 1
# 32 "include/linux/seqlock.h"
typedef struct {
 unsigned sequence;
 spinlock_t lock;
} seqlock_t;
# 60 "include/linux/seqlock.h"
static inline __attribute__((always_inline)) void write_seqlock(seqlock_t *sl)
{
 _spin_lock(&sl->lock);
 ++sl->sequence;
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((always_inline)) void write_sequnlock(seqlock_t *sl)
{
 __asm__ __volatile__("": : :"memory");
 sl->sequence++;
 _spin_unlock(&sl->lock);
}

static inline __attribute__((always_inline)) int write_tryseqlock(seqlock_t *sl)
{
 int ret = (_spin_trylock(&sl->lock));

 if (ret) {
  ++sl->sequence;
  __asm__ __volatile__("": : :"memory");
 }
 return ret;
}


static inline __attribute__((always_inline)) __attribute__((always_inline)) unsigned read_seqbegin(const seqlock_t *sl)
{
 unsigned ret;

repeat:
 ret = sl->sequence;
 __asm__ __volatile__("": : :"memory");
 if (__builtin_expect(!!(ret & 1), 0)) {
  cpu_relax();
  goto repeat;
 }

 return ret;
}






static inline __attribute__((always_inline)) __attribute__((always_inline)) int read_seqretry(const seqlock_t *sl, unsigned start)
{
 __asm__ __volatile__("": : :"memory");

 return (sl->sequence != start);
}
# 121 "include/linux/seqlock.h"
typedef struct seqcount {
 unsigned sequence;
} seqcount_t;





static inline __attribute__((always_inline)) unsigned read_seqcount_begin(const seqcount_t *s)
{
 unsigned ret;

repeat:
 ret = s->sequence;
 __asm__ __volatile__("": : :"memory");
 if (__builtin_expect(!!(ret & 1), 0)) {
  cpu_relax();
  goto repeat;
 }
 return ret;
}




static inline __attribute__((always_inline)) int read_seqcount_retry(const seqcount_t *s, unsigned start)
{
 __asm__ __volatile__("": : :"memory");

 return s->sequence != start;
}






static inline __attribute__((always_inline)) void write_seqcount_begin(seqcount_t *s)
{
 s->sequence++;
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((always_inline)) void write_seqcount_end(seqcount_t *s)
{
 __asm__ __volatile__("": : :"memory");
 s->sequence++;
}
# 9 "include/linux/time.h" 2
# 1 "include/linux/math64.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/div64.h" 1
# 57 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/div64.h"
# 1 "include/asm-generic/div64.h" 1
# 58 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/div64.h" 2
# 6 "include/linux/math64.h" 2
# 15 "include/linux/math64.h"
static inline __attribute__((always_inline)) u64 div_u64_rem(u64 dividend, u32 divisor, u32 *remainder)
{
 *remainder = dividend % divisor;
 return dividend / divisor;
}




static inline __attribute__((always_inline)) s64 div_s64_rem(s64 dividend, s32 divisor, s32 *remainder)
{
 *remainder = dividend % divisor;
 return dividend / divisor;
}




static inline __attribute__((always_inline)) u64 div64_u64(u64 dividend, u64 divisor)
{
 return dividend / divisor;
}
# 66 "include/linux/math64.h"
static inline __attribute__((always_inline)) u64 div_u64(u64 dividend, u32 divisor)
{
 u32 remainder;
 return div_u64_rem(dividend, divisor, &remainder);
}






static inline __attribute__((always_inline)) s64 div_s64(s64 dividend, s32 divisor)
{
 s32 remainder;
 return div_s64_rem(dividend, divisor, &remainder);
}


u32 iter_div_u64_rem(u64 dividend, u32 divisor, u64 *remainder);

static inline __attribute__((always_inline)) __attribute__((always_inline)) u32
__iter_div_u64_rem(u64 dividend, u32 divisor, u64 *remainder)
{
 u32 ret = 0;

 while (dividend >= divisor) {


  asm("" : "+rm"(dividend));

  dividend -= divisor;
  ret++;
 }

 *remainder = dividend;

 return ret;
}
# 10 "include/linux/time.h" 2




struct timespec {
 time_t tv_sec;
 long tv_nsec;
};


struct timeval {
 time_t tv_sec;
 suseconds_t tv_usec;
};

struct timezone {
 int tz_minuteswest;
 int tz_dsttime;
};



extern struct timezone sys_tz;
# 45 "include/linux/time.h"
static inline __attribute__((always_inline)) int timespec_equal(const struct timespec *a,
                                 const struct timespec *b)
{
 return (a->tv_sec == b->tv_sec) && (a->tv_nsec == b->tv_nsec);
}






static inline __attribute__((always_inline)) int timespec_compare(const struct timespec *lhs, const struct timespec *rhs)
{
 if (lhs->tv_sec < rhs->tv_sec)
  return -1;
 if (lhs->tv_sec > rhs->tv_sec)
  return 1;
 return lhs->tv_nsec - rhs->tv_nsec;
}

static inline __attribute__((always_inline)) int timeval_compare(const struct timeval *lhs, const struct timeval *rhs)
{
 if (lhs->tv_sec < rhs->tv_sec)
  return -1;
 if (lhs->tv_sec > rhs->tv_sec)
  return 1;
 return lhs->tv_usec - rhs->tv_usec;
}

extern unsigned long mktime(const unsigned int year, const unsigned int mon,
       const unsigned int day, const unsigned int hour,
       const unsigned int min, const unsigned int sec);

extern void set_normalized_timespec(struct timespec *ts, time_t sec, long nsec);
extern struct timespec timespec_add_safe(const struct timespec lhs,
      const struct timespec rhs);




static inline __attribute__((always_inline)) struct timespec timespec_sub(struct timespec lhs,
      struct timespec rhs)
{
 struct timespec ts_delta;
 set_normalized_timespec(&ts_delta, lhs.tv_sec - rhs.tv_sec,
    lhs.tv_nsec - rhs.tv_nsec);
 return ts_delta;
}







extern struct timespec xtime;
extern struct timespec wall_to_monotonic;
extern seqlock_t xtime_lock;

extern unsigned long read_persistent_clock(void);
extern int update_persistent_clock(struct timespec now);
extern int no_sync_cmos_clock __attribute__((__section__(".data.read_mostly")));
void timekeeping_init(void);
extern int timekeeping_suspended;

unsigned long get_seconds(void);
struct timespec current_kernel_time(void);




extern void do_gettimeofday(struct timeval *tv);
extern int do_settimeofday(struct timespec *tv);
extern int do_sys_settimeofday(struct timespec *tv, struct timezone *tz);

extern long do_utimes(int dfd, char __attribute__((noderef, address_space(1))) *filename, struct timespec *times, int flags);
struct itimerval;
extern int do_setitimer(int which, struct itimerval *value,
   struct itimerval *ovalue);
extern unsigned int alarm_setitimer(unsigned int seconds);
extern int do_getitimer(int which, struct itimerval *value);
extern void getnstimeofday(struct timespec *tv);
extern void getrawmonotonic(struct timespec *ts);
extern void getboottime(struct timespec *ts);
extern void monotonic_to_bootbased(struct timespec *ts);

extern struct timespec timespec_trunc(struct timespec t, unsigned gran);
extern int timekeeping_valid_for_hres(void);
extern void update_wall_time(void);
extern void update_xtime_cache(u64 nsec);

struct tms;
extern void do_sys_times(struct tms *);
# 146 "include/linux/time.h"
static inline __attribute__((always_inline)) s64 timespec_to_ns(const struct timespec *ts)
{
 return ((s64) ts->tv_sec * 1000000000L) + ts->tv_nsec;
}
# 158 "include/linux/time.h"
static inline __attribute__((always_inline)) s64 timeval_to_ns(const struct timeval *tv)
{
 return ((s64) tv->tv_sec * 1000000000L) +
  tv->tv_usec * 1000L;
}







extern struct timespec ns_to_timespec(const s64 nsec);







extern struct timeval ns_to_timeval(const s64 nsec);
# 188 "include/linux/time.h"
static inline __attribute__((always_inline)) __attribute__((always_inline)) void timespec_add_ns(struct timespec *a, u64 ns)
{
 a->tv_sec += __iter_div_u64_rem(a->tv_nsec + ns, 1000000000L, &ns);
 a->tv_nsec = ns;
}
# 211 "include/linux/time.h"
struct itimerspec {
 struct timespec it_interval;
 struct timespec it_value;
};

struct itimerval {
 struct timeval it_interval;
 struct timeval it_value;
};
# 57 "include/linux/timex.h" 2







struct timex {
 unsigned int modes;
 long offset;
 long freq;
 long maxerror;
 long esterror;
 int status;
 long constant;
 long precision;
 long tolerance;


 struct timeval time;
 long tick;

 long ppsfreq;
 long jitter;
 int shift;
 long stabil;
 long jitcnt;
 long calcnt;
 long errcnt;
 long stbcnt;

 int tai;

 int :32; int :32; int :32; int :32;
 int :32; int :32; int :32; int :32;
 int :32; int :32; int :32;
};
# 170 "include/linux/timex.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/timex.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/tsc.h" 1
# 15 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/tsc.h"
typedef unsigned long long cycles_t;

extern unsigned int cpu_khz;
extern unsigned int tsc_khz;

extern void disable_TSC(void);

static inline __attribute__((always_inline)) cycles_t get_cycles(void)
{
 unsigned long long ret = 0;





 ((ret) = __native_read_tsc());

 return ret;
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) cycles_t vget_cycles(void)
{
# 45 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/tsc.h"
 return (cycles_t)__native_read_tsc();
}

extern void tsc_init(void);
extern void mark_tsc_unstable(char *reason);
extern int unsynchronized_tsc(void);
int check_tsc_unstable(void);





extern void check_tsc_sync_source(int cpu);
extern void check_tsc_sync_target(void);

extern int notsc_setup(char *);
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/timex.h" 2
# 171 "include/linux/timex.h" 2
# 210 "include/linux/timex.h"
extern unsigned long tick_usec;
extern unsigned long tick_nsec;
extern int tickadj;




extern int time_status;
extern long time_maxerror;
extern long time_esterror;

extern long time_adjust;

extern void ntp_init(void);
extern void ntp_clear(void);





static inline __attribute__((always_inline)) int ntp_synced(void)
{
 return !(time_status & 0x0040);
}
# 252 "include/linux/timex.h"
extern u64 tick_length;

extern void second_overflow(void);
extern void update_ntp_one_tick(void);
extern int do_adjtimex(struct timex *);




int read_current_timer(unsigned long *timer_val);
# 55 "include/linux/sched.h" 2
# 1 "include/linux/jiffies.h" 1
# 81 "include/linux/jiffies.h"
extern u64 __attribute__((section(".data"))) jiffies_64;
extern unsigned long volatile __attribute__((section(".data"))) jiffies;




static inline __attribute__((always_inline)) u64 get_jiffies_64(void)
{
 return (u64)jiffies;
}
# 183 "include/linux/jiffies.h"
extern unsigned long preset_lpj;
# 296 "include/linux/jiffies.h"
extern unsigned int jiffies_to_msecs(const unsigned long j);
extern unsigned int jiffies_to_usecs(const unsigned long j);
extern unsigned long msecs_to_jiffies(const unsigned int m);
extern unsigned long usecs_to_jiffies(const unsigned int u);
extern unsigned long timespec_to_jiffies(const struct timespec *value);
extern void jiffies_to_timespec(const unsigned long jiffies,
    struct timespec *value);
extern unsigned long timeval_to_jiffies(const struct timeval *value);
extern void jiffies_to_timeval(const unsigned long jiffies,
          struct timeval *value);
extern clock_t jiffies_to_clock_t(long x);
extern unsigned long clock_t_to_jiffies(unsigned long x);
extern u64 jiffies_64_to_clock_t(u64 x);
extern u64 nsec_to_clock_t(u64 x);
# 56 "include/linux/sched.h" 2
# 1 "include/linux/rbtree.h" 1
# 100 "include/linux/rbtree.h"
struct rb_node
{
 unsigned long rb_parent_color;


 struct rb_node *rb_right;
 struct rb_node *rb_left;
} __attribute__((aligned(sizeof(long))));


struct rb_root
{
 struct rb_node *rb_node;
};
# 123 "include/linux/rbtree.h"
static inline __attribute__((always_inline)) void rb_set_parent(struct rb_node *rb, struct rb_node *p)
{
 rb->rb_parent_color = (rb->rb_parent_color & 3) | (unsigned long)p;
}
static inline __attribute__((always_inline)) void rb_set_color(struct rb_node *rb, int color)
{
 rb->rb_parent_color = (rb->rb_parent_color & ~1) | color;
}
# 139 "include/linux/rbtree.h"
extern void rb_insert_color(struct rb_node *, struct rb_root *);
extern void rb_erase(struct rb_node *, struct rb_root *);


extern struct rb_node *rb_next(const struct rb_node *);
extern struct rb_node *rb_prev(const struct rb_node *);
extern struct rb_node *rb_first(const struct rb_root *);
extern struct rb_node *rb_last(const struct rb_root *);


extern void rb_replace_node(struct rb_node *victim, struct rb_node *new,
       struct rb_root *root);

static inline __attribute__((always_inline)) void rb_link_node(struct rb_node * node, struct rb_node * parent,
    struct rb_node ** rb_link)
{
 node->rb_parent_color = (unsigned long )parent;
 node->rb_left = node->rb_right = ((void *)0);

 *rb_link = node;
}
# 57 "include/linux/sched.h" 2



# 1 "include/linux/nodemask.h" 1
# 90 "include/linux/nodemask.h"
# 1 "include/linux/numa.h" 1
# 91 "include/linux/nodemask.h" 2

typedef struct { unsigned long bits[((((1 << 6)) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; } nodemask_t;
extern nodemask_t _unused_nodemask_arg_;


static inline __attribute__((always_inline)) void __node_set(int node, volatile nodemask_t *dstp)
{
 set_bit(node, dstp->bits);
}


static inline __attribute__((always_inline)) void __node_clear(int node, volatile nodemask_t *dstp)
{
 clear_bit(node, dstp->bits);
}


static inline __attribute__((always_inline)) void __nodes_setall(nodemask_t *dstp, int nbits)
{
 bitmap_fill(dstp->bits, nbits);
}


static inline __attribute__((always_inline)) void __nodes_clear(nodemask_t *dstp, int nbits)
{
 bitmap_zero(dstp->bits, nbits);
}






static inline __attribute__((always_inline)) int __node_test_and_set(int node, nodemask_t *addr)
{
 return test_and_set_bit(node, addr->bits);
}



static inline __attribute__((always_inline)) void __nodes_and(nodemask_t *dstp, const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_or(nodemask_t *dstp, const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_xor(nodemask_t *dstp, const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_andnot(nodemask_t *dstp, const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_complement(nodemask_t *dstp,
     const nodemask_t *srcp, int nbits)
{
 bitmap_complement(dstp->bits, srcp->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodes_equal(const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 return bitmap_equal(src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodes_intersects(const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodes_subset(const nodemask_t *src1p,
     const nodemask_t *src2p, int nbits)
{
 return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodes_empty(const nodemask_t *srcp, int nbits)
{
 return bitmap_empty(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodes_full(const nodemask_t *srcp, int nbits)
{
 return bitmap_full(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodes_weight(const nodemask_t *srcp, int nbits)
{
 return bitmap_weight(srcp->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_shift_right(nodemask_t *dstp,
     const nodemask_t *srcp, int n, int nbits)
{
 bitmap_shift_right(dstp->bits, srcp->bits, n, nbits);
}



static inline __attribute__((always_inline)) void __nodes_shift_left(nodemask_t *dstp,
     const nodemask_t *srcp, int n, int nbits)
{
 bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}





static inline __attribute__((always_inline)) int __first_node(const nodemask_t *srcp)
{
 return ({ int __min1 = ((1 << 6)); int __min2 = (find_first_bit(srcp->bits, (1 << 6))); __min1 < __min2 ? __min1: __min2; });
}


static inline __attribute__((always_inline)) int __next_node(int n, const nodemask_t *srcp)
{
 return ({ int __min1 = ((1 << 6)); int __min2 = (find_next_bit(srcp->bits, (1 << 6), n+1)); __min1 < __min2 ? __min1: __min2; });
}
# 255 "include/linux/nodemask.h"
static inline __attribute__((always_inline)) int __first_unset_node(const nodemask_t *maskp)
{
 return ({ int __min1 = ((1 << 6)); int __min2 = (find_first_zero_bit(maskp->bits, (1 << 6))); __min1 < __min2 ? __min1: __min2; })
                                                  ;
}
# 289 "include/linux/nodemask.h"
static inline __attribute__((always_inline)) int __nodemask_scnprintf(char *buf, int len,
     const nodemask_t *srcp, int nbits)
{
 return bitmap_scnprintf(buf, len, srcp->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodemask_parse_user(const char __attribute__((noderef, address_space(1))) *buf, int len,
     nodemask_t *dstp, int nbits)
{
 return bitmap_parse_user(buf, len, dstp->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodelist_scnprintf(char *buf, int len,
     const nodemask_t *srcp, int nbits)
{
 return bitmap_scnlistprintf(buf, len, srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodelist_parse(const char *buf, nodemask_t *dstp, int nbits)
{
 return bitmap_parselist(buf, dstp->bits, nbits);
}



static inline __attribute__((always_inline)) int __node_remap(int oldbit,
  const nodemask_t *oldp, const nodemask_t *newp, int nbits)
{
 return bitmap_bitremap(oldbit, oldp->bits, newp->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_remap(nodemask_t *dstp, const nodemask_t *srcp,
  const nodemask_t *oldp, const nodemask_t *newp, int nbits)
{
 bitmap_remap(dstp->bits, srcp->bits, oldp->bits, newp->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_onto(nodemask_t *dstp, const nodemask_t *origp,
  const nodemask_t *relmapp, int nbits)
{
 bitmap_onto(dstp->bits, origp->bits, relmapp->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_fold(nodemask_t *dstp, const nodemask_t *origp,
  int sz, int nbits)
{
 bitmap_fold(dstp->bits, origp->bits, sz, nbits);
}
# 363 "include/linux/nodemask.h"
enum node_states {
 N_POSSIBLE,
 N_ONLINE,
 N_NORMAL_MEMORY,



 N_HIGH_MEMORY = N_NORMAL_MEMORY,

 N_CPU,
 NR_NODE_STATES
};






extern nodemask_t node_states[NR_NODE_STATES];


static inline __attribute__((always_inline)) int node_state(int node, enum node_states state)
{
 return (__builtin_constant_p(((node))) ? constant_test_bit(((node)), ((node_states[state]).bits)) : variable_test_bit(((node)), ((node_states[state]).bits)));
}

static inline __attribute__((always_inline)) void node_set_state(int node, enum node_states state)
{
 __node_set(node, &node_states[state]);
}

static inline __attribute__((always_inline)) void node_clear_state(int node, enum node_states state)
{
 __node_clear(node, &node_states[state]);
}

static inline __attribute__((always_inline)) int num_node_state(enum node_states state)
{
 return __nodes_weight(&(node_states[state]), (1 << 6));
}







extern int nr_node_ids;
# 61 "include/linux/sched.h" 2
# 1 "include/linux/mm_types.h" 1



# 1 "include/linux/auxvec.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/auxvec.h" 1
# 5 "include/linux/auxvec.h" 2
# 5 "include/linux/mm_types.h" 2




# 1 "include/linux/prio_tree.h" 1
# 14 "include/linux/prio_tree.h"
struct raw_prio_tree_node {
 struct prio_tree_node *left;
 struct prio_tree_node *right;
 struct prio_tree_node *parent;
};

struct prio_tree_node {
 struct prio_tree_node *left;
 struct prio_tree_node *right;
 struct prio_tree_node *parent;
 unsigned long start;
 unsigned long last;
};

struct prio_tree_root {
 struct prio_tree_node *prio_tree_node;
 unsigned short index_bits;
 unsigned short raw;




};

struct prio_tree_iter {
 struct prio_tree_node *cur;
 unsigned long mask;
 unsigned long value;
 int size_level;

 struct prio_tree_root *root;
 unsigned long r_index;
 unsigned long h_index;
};

static inline __attribute__((always_inline)) void prio_tree_iter_init(struct prio_tree_iter *iter,
  struct prio_tree_root *root, unsigned long r_index, unsigned long h_index)
{
 iter->root = root;
 iter->r_index = r_index;
 iter->h_index = h_index;
 iter->cur = ((void *)0);
}
# 84 "include/linux/prio_tree.h"
static inline __attribute__((always_inline)) int prio_tree_empty(const struct prio_tree_root *root)
{
 return root->prio_tree_node == ((void *)0);
}

static inline __attribute__((always_inline)) int prio_tree_root(const struct prio_tree_node *node)
{
 return node->parent == node;
}

static inline __attribute__((always_inline)) int prio_tree_left_empty(const struct prio_tree_node *node)
{
 return node->left == node;
}

static inline __attribute__((always_inline)) int prio_tree_right_empty(const struct prio_tree_node *node)
{
 return node->right == node;
}


struct prio_tree_node *prio_tree_replace(struct prio_tree_root *root,
                struct prio_tree_node *old, struct prio_tree_node *node);
struct prio_tree_node *prio_tree_insert(struct prio_tree_root *root,
                struct prio_tree_node *node);
void prio_tree_remove(struct prio_tree_root *root, struct prio_tree_node *node);
struct prio_tree_node *prio_tree_next(struct prio_tree_iter *iter);
# 10 "include/linux/mm_types.h" 2

# 1 "include/linux/rwsem.h" 1
# 15 "include/linux/rwsem.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 16 "include/linux/rwsem.h" 2

struct rw_semaphore;


# 1 "include/linux/rwsem-spinlock.h" 1
# 22 "include/linux/rwsem-spinlock.h"
struct rwsem_waiter;
# 31 "include/linux/rwsem-spinlock.h"
struct rw_semaphore {
 __s32 activity;
 spinlock_t wait_lock;
 struct list_head wait_list;

 struct lockdep_map dep_map;

};
# 53 "include/linux/rwsem-spinlock.h"
extern void __init_rwsem(struct rw_semaphore *sem, const char *name,
    struct lock_class_key *key);
# 63 "include/linux/rwsem-spinlock.h"
extern void __down_read(struct rw_semaphore *sem);
extern int __down_read_trylock(struct rw_semaphore *sem);
extern void __down_write(struct rw_semaphore *sem);
extern void __down_write_nested(struct rw_semaphore *sem, int subclass);
extern int __down_write_trylock(struct rw_semaphore *sem);
extern void __up_read(struct rw_semaphore *sem);
extern void __up_write(struct rw_semaphore *sem);
extern void __downgrade_write(struct rw_semaphore *sem);

static inline __attribute__((always_inline)) int rwsem_is_locked(struct rw_semaphore *sem)
{
 return (sem->activity != 0);
}
# 21 "include/linux/rwsem.h" 2







extern void down_read(struct rw_semaphore *sem);




extern int down_read_trylock(struct rw_semaphore *sem);




extern void down_write(struct rw_semaphore *sem);




extern int down_write_trylock(struct rw_semaphore *sem);




extern void up_read(struct rw_semaphore *sem);




extern void up_write(struct rw_semaphore *sem);




extern void downgrade_write(struct rw_semaphore *sem);
# 74 "include/linux/rwsem.h"
extern void down_read_nested(struct rw_semaphore *sem, int subclass);
extern void down_write_nested(struct rw_semaphore *sem, int subclass);






extern void down_read_non_owner(struct rw_semaphore *sem);
extern void up_read_non_owner(struct rw_semaphore *sem);
# 12 "include/linux/mm_types.h" 2
# 1 "include/linux/completion.h" 1
# 11 "include/linux/completion.h"
# 1 "include/linux/wait.h" 1
# 28 "include/linux/wait.h"
typedef struct __wait_queue wait_queue_t;
typedef int (*wait_queue_func_t)(wait_queue_t *wait, unsigned mode, int sync, void *key);
int default_wake_function(wait_queue_t *wait, unsigned mode, int sync, void *key);

struct __wait_queue {
 unsigned int flags;

 void *private;
 wait_queue_func_t func;
 struct list_head task_list;
};

struct wait_bit_key {
 void *flags;
 int bit_nr;
};

struct wait_bit_queue {
 struct wait_bit_key key;
 wait_queue_t wait;
};

struct __wait_queue_head {
 spinlock_t lock;
 struct list_head task_list;
};
typedef struct __wait_queue_head wait_queue_head_t;

struct task_struct;
# 80 "include/linux/wait.h"
extern void init_waitqueue_head(wait_queue_head_t *q);
# 91 "include/linux/wait.h"
static inline __attribute__((always_inline)) void init_waitqueue_entry(wait_queue_t *q, struct task_struct *p)
{
 q->flags = 0;
 q->private = p;
 q->func = default_wake_function;
}

static inline __attribute__((always_inline)) void init_waitqueue_func_entry(wait_queue_t *q,
     wait_queue_func_t func)
{
 q->flags = 0;
 q->private = ((void *)0);
 q->func = func;
}

static inline __attribute__((always_inline)) int waitqueue_active(wait_queue_head_t *q)
{
 return !list_empty(&q->task_list);
}

extern void add_wait_queue(wait_queue_head_t *q, wait_queue_t *wait);
extern void add_wait_queue_exclusive(wait_queue_head_t *q, wait_queue_t *wait);
extern void remove_wait_queue(wait_queue_head_t *q, wait_queue_t *wait);

static inline __attribute__((always_inline)) void __add_wait_queue(wait_queue_head_t *head, wait_queue_t *new)
{
 list_add(&new->task_list, &head->task_list);
}




static inline __attribute__((always_inline)) void __add_wait_queue_tail(wait_queue_head_t *head,
      wait_queue_t *new)
{
 list_add_tail(&new->task_list, &head->task_list);
}

static inline __attribute__((always_inline)) void __remove_wait_queue(wait_queue_head_t *head,
       wait_queue_t *old)
{
 list_del(&old->task_list);
}

void __wake_up_common(wait_queue_head_t *q, unsigned int mode,
   int nr_exclusive, int sync, void *key);
void __wake_up(wait_queue_head_t *q, unsigned int mode, int nr, void *key);
extern void __wake_up_locked(wait_queue_head_t *q, unsigned int mode);
extern void __wake_up_sync(wait_queue_head_t *q, unsigned int mode, int nr);
void __wake_up_bit(wait_queue_head_t *, void *, int);
int __wait_on_bit(wait_queue_head_t *, struct wait_bit_queue *, int (*)(void *), unsigned);
int __wait_on_bit_lock(wait_queue_head_t *, struct wait_bit_queue *, int (*)(void *), unsigned);
void wake_up_bit(void *, int);
int out_of_line_wait_on_bit(void *, int, int (*)(void *), unsigned);
int out_of_line_wait_on_bit_lock(void *, int, int (*)(void *), unsigned);
wait_queue_head_t *bit_waitqueue(void *, int);
# 405 "include/linux/wait.h"
static inline __attribute__((always_inline)) void add_wait_queue_exclusive_locked(wait_queue_head_t *q,
         wait_queue_t * wait)
{
 wait->flags |= 0x01;
 __add_wait_queue_tail(q, wait);
}




static inline __attribute__((always_inline)) void remove_wait_queue_locked(wait_queue_head_t *q,
         wait_queue_t * wait)
{
 __remove_wait_queue(q, wait);
}






extern void sleep_on(wait_queue_head_t *q);
extern long sleep_on_timeout(wait_queue_head_t *q,
          signed long timeout);
extern void interruptible_sleep_on(wait_queue_head_t *q);
extern long interruptible_sleep_on_timeout(wait_queue_head_t *q,
        signed long timeout);




void prepare_to_wait(wait_queue_head_t *q, wait_queue_t *wait, int state);
void prepare_to_wait_exclusive(wait_queue_head_t *q, wait_queue_t *wait, int state);
void finish_wait(wait_queue_head_t *q, wait_queue_t *wait);
void abort_exclusive_wait(wait_queue_head_t *q, wait_queue_t *wait,
   unsigned int mode, void *key);
int autoremove_wake_function(wait_queue_t *wait, unsigned mode, int sync, void *key);
int wake_bit_function(wait_queue_t *wait, unsigned mode, int sync, void *key);
# 483 "include/linux/wait.h"
static inline __attribute__((always_inline)) int wait_on_bit(void *word, int bit,
    int (*action)(void *), unsigned mode)
{
 if (!(__builtin_constant_p((bit)) ? constant_test_bit((bit), (word)) : variable_test_bit((bit), (word))))
  return 0;
 return out_of_line_wait_on_bit(word, bit, action, mode);
}
# 507 "include/linux/wait.h"
static inline __attribute__((always_inline)) int wait_on_bit_lock(void *word, int bit,
    int (*action)(void *), unsigned mode)
{
 if (!test_and_set_bit(bit, word))
  return 0;
 return out_of_line_wait_on_bit_lock(word, bit, action, mode);
}
# 12 "include/linux/completion.h" 2
# 25 "include/linux/completion.h"
struct completion {
 unsigned int done;
 wait_queue_head_t wait;
};
# 73 "include/linux/completion.h"
static inline __attribute__((always_inline)) void init_completion(struct completion *x)
{
 x->done = 0;
 init_waitqueue_head(&x->wait);
}

extern void wait_for_completion(struct completion *);
extern int wait_for_completion_interruptible(struct completion *x);
extern int wait_for_completion_killable(struct completion *x);
extern unsigned long wait_for_completion_timeout(struct completion *x,
         unsigned long timeout);
extern unsigned long wait_for_completion_interruptible_timeout(
   struct completion *x, unsigned long timeout);
extern bool try_wait_for_completion(struct completion *x);
extern bool completion_done(struct completion *x);

extern void complete(struct completion *);
extern void complete_all(struct completion *);
# 13 "include/linux/mm_types.h" 2


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmu.h" 1




# 1 "include/linux/mutex.h" 1
# 18 "include/linux/mutex.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 19 "include/linux/mutex.h" 2
# 48 "include/linux/mutex.h"
struct mutex {

 atomic_t count;
 spinlock_t wait_lock;
 struct list_head wait_list;

 struct thread_info *owner;
 const char *name;
 void *magic;


 struct lockdep_map dep_map;

};





struct mutex_waiter {
 struct list_head list;
 struct task_struct *task;

 struct mutex *lock;
 void *magic;

};


# 1 "include/linux/mutex-debug.h" 1
# 21 "include/linux/mutex-debug.h"
extern void mutex_destroy(struct mutex *lock);
# 78 "include/linux/mutex.h" 2
# 106 "include/linux/mutex.h"
extern void __mutex_init(struct mutex *lock, const char *name,
    struct lock_class_key *key);







static inline __attribute__((always_inline)) int mutex_is_locked(struct mutex *lock)
{
 return ((&lock->count)->counter) != 1;
}






extern void mutex_lock_nested(struct mutex *lock, unsigned int subclass);
extern int __attribute__((warn_unused_result)) mutex_lock_interruptible_nested(struct mutex *lock,
     unsigned int subclass);
extern int __attribute__((warn_unused_result)) mutex_lock_killable_nested(struct mutex *lock,
     unsigned int subclass);
# 150 "include/linux/mutex.h"
extern int mutex_trylock(struct mutex *lock);
extern void mutex_unlock(struct mutex *lock);
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmu.h" 2





typedef struct {
 void *ldt;
 int size;
 struct mutex lock;
 void *vdso;
} mm_context_t;


void leave_mm(int cpu);
# 16 "include/linux/mm_types.h" 2






struct address_space;




typedef atomic_long_t mm_counter_t;
# 39 "include/linux/mm_types.h"
struct page {
 unsigned long flags;

 atomic_t _count;
 union {
  atomic_t _mapcount;



  struct {
   u16 inuse;
   u16 objects;
  };
 };
 union {
     struct {
  unsigned long private;






  struct address_space *mapping;






     };

     spinlock_t ptl;

     struct kmem_cache *slab;
     struct page * __attribute__((recursive)) first_page;
 } __attribute__((recursive));
 union {
  unsigned long index;
  void *freelist;
 };
 struct list_head lru;
# 97 "include/linux/mm_types.h"
};






struct vm_region {
 struct rb_node vm_rb;
 unsigned long vm_flags;
 unsigned long vm_start;
 unsigned long vm_end;
 unsigned long vm_top;
 unsigned long vm_pgoff;
 struct file *vm_file;

 atomic_t vm_usage;
};







struct vm_area_struct {
 struct mm_struct * vm_mm;
 unsigned long vm_start;
 unsigned long vm_end;



 struct vm_area_struct *vm_next;

 pgprot_t vm_page_prot;
 unsigned long vm_flags;

 struct rb_node vm_rb;







 union {
  struct {
   struct list_head list;
   void *parent;
   struct vm_area_struct *head;
  } vm_set;

  struct raw_prio_tree_node prio_tree_node;
 } shared;







 struct list_head anon_vma_node;
 struct anon_vma *anon_vma;


 struct vm_operations_struct * vm_ops;


 unsigned long vm_pgoff;

 struct file * vm_file;
    void * __attribute__((noderef, address_space(2))) vm_private_data;
 unsigned long vm_truncate_count;





 struct mempolicy *vm_policy;

};

struct core_thread {
 struct task_struct *task;
 struct core_thread *next;
};

struct core_state {
 atomic_t nr_threads;
 struct core_thread dumper;
 struct completion startup;
};

struct mm_struct {
 struct vm_area_struct * mmap;
 struct rb_root mm_rb;
 struct vm_area_struct * mmap_cache;
 unsigned long (*get_unmapped_area) (struct file *filp,
    unsigned long addr, unsigned long len,
    unsigned long pgoff, unsigned long flags);
 void (*unmap_area) (struct mm_struct *mm, unsigned long addr);
 unsigned long mmap_base;
 unsigned long task_size;
 unsigned long cached_hole_size;
 unsigned long free_area_cache;
 pgd_t * pgd;
 atomic_t mm_users;
 atomic_t mm_count;
 int map_count;
 struct rw_semaphore mmap_sem;
 spinlock_t page_table_lock;

 struct list_head mmlist;







 mm_counter_t _file_rss;
 mm_counter_t _anon_rss;

 unsigned long hiwater_rss;
 unsigned long hiwater_vm;

 unsigned long total_vm, locked_vm, shared_vm, exec_vm;
 unsigned long stack_vm, reserved_vm, def_flags, nr_ptes;
 unsigned long start_code, end_code, start_data, end_data;
 unsigned long start_brk, brk, start_stack;
 unsigned long arg_start, arg_end, env_start, env_end;

 unsigned long saved_auxv[(2*(2 + 19 + 1))];

 cpumask_t cpu_vm_mask;


 mm_context_t context;
# 243 "include/linux/mm_types.h"
 unsigned int faultstamp;
 unsigned int token_priority;
 unsigned int last_interval;

 unsigned long flags;

 struct core_state *core_state;


 spinlock_t ioctx_lock;
 struct hlist_head ioctx_list;
# 271 "include/linux/mm_types.h"
 struct file *exe_file;
 unsigned long num_exe_file_vmas;




};
# 62 "include/linux/sched.h" 2




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cputime.h" 1
# 1 "include/asm-generic/cputime.h" 1






typedef unsigned long cputime_t;
# 24 "include/asm-generic/cputime.h"
typedef u64 cputime64_t;
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/cputime.h" 2
# 67 "include/linux/sched.h" 2

# 1 "include/linux/smp.h" 1
# 14 "include/linux/smp.h"
extern void cpu_idle(void);

struct call_single_data {
 struct list_head list;
 void (*func) (void *info);
 void *info;
 u16 flags;
 u16 priv;
};


extern unsigned int total_cpus;

int smp_call_function_single(int cpuid, void (*func) (void *info), void *info,
    int wait);







# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h" 1
# 12 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec.h" 1





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec_def.h" 1
# 27 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec_def.h"
struct intel_mp_floating {
 char mpf_signature[4];
 unsigned int mpf_physptr;
 unsigned char mpf_length;
 unsigned char mpf_specification;
 unsigned char mpf_checksum;
 unsigned char mpf_feature1;
 unsigned char mpf_feature2;
 unsigned char mpf_feature3;
 unsigned char mpf_feature4;
 unsigned char mpf_feature5;
};



struct mpc_table {
 char signature[4];
 unsigned short length;
 char spec;
 char checksum;
 char oem[8];
 char productid[12];
 unsigned int oemptr;
 unsigned short oemsize;
 unsigned short oemcount;
 unsigned int lapic;
 unsigned int reserved;
};
# 73 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec_def.h"
struct mpc_cpu {
 unsigned char type;
 unsigned char apicid;
 unsigned char apicver;
 unsigned char cpuflag;
 unsigned int cpufeature;
 unsigned int featureflag;
 unsigned int reserved[2];
};

struct mpc_bus {
 unsigned char type;
 unsigned char busid;
 unsigned char bustype[6];
};
# 111 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec_def.h"
struct mpc_ioapic {
 unsigned char type;
 unsigned char apicid;
 unsigned char apicver;
 unsigned char flags;
 unsigned int apicaddr;
};

struct mpc_intsrc {
 unsigned char type;
 unsigned char irqtype;
 unsigned short irqflag;
 unsigned char srcbus;
 unsigned char srcbusirq;
 unsigned char dstapic;
 unsigned char dstirq;
};

enum mp_irq_source_types {
 mp_INT = 0,
 mp_NMI = 1,
 mp_SMI = 2,
 mp_ExtINT = 3
};







struct mpc_lintsrc {
 unsigned char type;
 unsigned char irqtype;
 unsigned short irqflag;
 unsigned char srcbusid;
 unsigned char srcbusirq;
 unsigned char destapic;
 unsigned char destapiclint;
};



struct mpc_oemtable {
 char signature[4];
 unsigned short length;
 char rev;
 char checksum;
 char mpc[8];
};
# 174 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec_def.h"
enum mp_bustype {
 MP_BUS_ISA = 1,
 MP_BUS_EISA,
 MP_BUS_PCI,
 MP_BUS_MCA,
};
# 7 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec.h" 2

extern int apic_version[255];
extern int pic_mode;
# 33 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec.h"
extern void early_find_smp_config(void);
extern void early_get_smp_config(void);





extern unsigned long mp_bus_not_pci[(((256) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];

extern unsigned int boot_cpu_physical_apicid;
extern unsigned int max_physical_apicid;
extern int smp_found_config;
extern int mpc_default_type;
extern unsigned long mp_lapic_addr;

extern void find_smp_config(void);
extern void get_smp_config(void);

extern void early_reserve_e820_mpc_new(void);




void __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) generic_processor_info(int apicid, int version);

extern void mp_register_ioapic(int id, u32 address, u32 gsi_base);
extern void mp_override_legacy_irq(u8 bus_irq, u8 polarity, u8 trigger,
       u32 gsi);
extern void mp_config_acpi_legacy_irqs(void);
extern int mp_register_gsi(u32 gsi, int edge_level, int active_high_low);
extern int acpi_probe_gsi(void);

extern int mp_config_acpi_gsi(unsigned char number, unsigned int devfn, u8 pin,
    u32 gsi, int triggering, int polarity);
# 84 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec.h"
struct physid_mask {
 unsigned long mask[(((255) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];
};

typedef struct physid_mask physid_mask_t;
# 140 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mpspec.h"
static inline __attribute__((always_inline)) void physid_set_mask_of_physid(int physid, physid_mask_t *map)
{
 bitmap_zero((*map).mask, 255);
 set_bit(physid, (*map).mask);
}




extern physid_mask_t phys_cpu_present_map;
# 13 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apic.h" 1



# 1 "include/linux/pm.h" 1
# 29 "include/linux/pm.h"
extern void (*pm_idle)(void);
extern void (*pm_power_off)(void);
extern void (*pm_power_off_prepare)(void);





struct device;

typedef struct pm_message {
 int event;
} pm_message_t;
# 170 "include/linux/pm.h"
struct dev_pm_ops {
 int (*prepare)(struct device *dev);
 void (*complete)(struct device *dev);
 int (*suspend)(struct device *dev);
 int (*resume)(struct device *dev);
 int (*freeze)(struct device *dev);
 int (*thaw)(struct device *dev);
 int (*poweroff)(struct device *dev);
 int (*restore)(struct device *dev);
 int (*suspend_noirq)(struct device *dev);
 int (*resume_noirq)(struct device *dev);
 int (*freeze_noirq)(struct device *dev);
 int (*thaw_noirq)(struct device *dev);
 int (*poweroff_noirq)(struct device *dev);
 int (*restore_noirq)(struct device *dev);
};
# 308 "include/linux/pm.h"
enum dpm_state {
 DPM_INVALID,
 DPM_ON,
 DPM_PREPARING,
 DPM_RESUMING,
 DPM_SUSPENDING,
 DPM_OFF,
 DPM_OFF_IRQ,
};

struct dev_pm_info {
 pm_message_t power_state;
 unsigned can_wakeup:1;
 unsigned should_wakeup:1;
 enum dpm_state status;

 struct list_head entry;

};
# 383 "include/linux/pm.h"
extern void device_pm_lock(void);
extern int sysdev_resume(void);
extern void device_power_up(pm_message_t state);
extern void device_resume(pm_message_t state);

extern void device_pm_unlock(void);
extern int sysdev_suspend(pm_message_t state);
extern int device_power_down(pm_message_t state);
extern int device_suspend(pm_message_t state);
extern int device_prepare_suspend(pm_message_t state);

extern void __suspend_report_result(const char *function, void *fn, int ret);
# 416 "include/linux/pm.h"
extern unsigned int pm_flags;
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apic.h" 2



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap.h" 1






# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap_64.h" 1
# 15 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap_64.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h" 1
# 26 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h"
# 1 "include/acpi/pdc_intel.h" 1
# 27 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/numa.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/numa_64.h" 1




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apicdef.h" 1
# 162 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apicdef.h"
struct local_apic {

        struct { unsigned int __reserved[4]; } __reserved_01;

        struct { unsigned int __reserved[4]; } __reserved_02;

        struct {
  unsigned int __reserved_1 : 24,
   phys_apic_id : 4,
   __reserved_2 : 4;
  unsigned int __reserved[3];
 } id;

        const
 struct {
  unsigned int version : 8,
   __reserved_1 : 8,
   max_lvt : 8,
   __reserved_2 : 8;
  unsigned int __reserved[3];
 } version;

        struct { unsigned int __reserved[4]; } __reserved_03;

        struct { unsigned int __reserved[4]; } __reserved_04;

        struct { unsigned int __reserved[4]; } __reserved_05;

        struct { unsigned int __reserved[4]; } __reserved_06;

        struct {
  unsigned int priority : 8,
   __reserved_1 : 24;
  unsigned int __reserved_2[3];
 } tpr;

        const
 struct {
  unsigned int priority : 8,
   __reserved_1 : 24;
  unsigned int __reserved_2[3];
 } apr;

        const
 struct {
  unsigned int priority : 8,
   __reserved_1 : 24;
  unsigned int __reserved_2[3];
 } ppr;

        struct {
  unsigned int eoi;
  unsigned int __reserved[3];
 } eoi;

        struct { unsigned int __reserved[4]; } __reserved_07;

        struct {
  unsigned int __reserved_1 : 24,
   logical_dest : 8;
  unsigned int __reserved_2[3];
 } ldr;

        struct {
  unsigned int __reserved_1 : 28,
   model : 4;
  unsigned int __reserved_2[3];
 } dfr;

        struct {
  unsigned int spurious_vector : 8,
   apic_enabled : 1,
   focus_cpu : 1,
   __reserved_2 : 22;
  unsigned int __reserved_3[3];
 } svr;

        struct {
         unsigned int bitfield;
  unsigned int __reserved[3];
 } isr [8];

        struct {
         unsigned int bitfield;
  unsigned int __reserved[3];
 } tmr [8];

        struct {
         unsigned int bitfield;
  unsigned int __reserved[3];
 } irr [8];

        union {
  struct {
   unsigned int send_cs_error : 1,
    receive_cs_error : 1,
    send_accept_error : 1,
    receive_accept_error : 1,
    __reserved_1 : 1,
    send_illegal_vector : 1,
    receive_illegal_vector : 1,
    illegal_register_address : 1,
    __reserved_2 : 24;
   unsigned int __reserved_3[3];
  } error_bits;
  struct {
   unsigned int errors;
   unsigned int __reserved_3[3];
  } all_errors;
 } esr;

        struct { unsigned int __reserved[4]; } __reserved_08;

        struct { unsigned int __reserved[4]; } __reserved_09;

        struct { unsigned int __reserved[4]; } __reserved_10;

        struct { unsigned int __reserved[4]; } __reserved_11;

        struct { unsigned int __reserved[4]; } __reserved_12;

        struct { unsigned int __reserved[4]; } __reserved_13;

        struct { unsigned int __reserved[4]; } __reserved_14;

        struct {
  unsigned int vector : 8,
   delivery_mode : 3,
   destination_mode : 1,
   delivery_status : 1,
   __reserved_1 : 1,
   level : 1,
   trigger : 1,
   __reserved_2 : 2,
   shorthand : 2,
   __reserved_3 : 12;
  unsigned int __reserved_4[3];
 } icr1;

        struct {
  union {
   unsigned int __reserved_1 : 24,
    phys_dest : 4,
    __reserved_2 : 4;
   unsigned int __reserved_3 : 24,
    logical_dest : 8;
  } dest;
  unsigned int __reserved_4[3];
 } icr2;

        struct {
  unsigned int vector : 8,
   __reserved_1 : 4,
   delivery_status : 1,
   __reserved_2 : 3,
   mask : 1,
   timer_mode : 1,
   __reserved_3 : 14;
  unsigned int __reserved_4[3];
 } lvt_timer;

        struct {
  unsigned int vector : 8,
   delivery_mode : 3,
   __reserved_1 : 1,
   delivery_status : 1,
   __reserved_2 : 3,
   mask : 1,
   __reserved_3 : 15;
  unsigned int __reserved_4[3];
 } lvt_thermal;

        struct {
  unsigned int vector : 8,
   delivery_mode : 3,
   __reserved_1 : 1,
   delivery_status : 1,
   __reserved_2 : 3,
   mask : 1,
   __reserved_3 : 15;
  unsigned int __reserved_4[3];
 } lvt_pc;

        struct {
  unsigned int vector : 8,
   delivery_mode : 3,
   __reserved_1 : 1,
   delivery_status : 1,
   polarity : 1,
   remote_irr : 1,
   trigger : 1,
   mask : 1,
   __reserved_2 : 15;
  unsigned int __reserved_3[3];
 } lvt_lint0;

        struct {
  unsigned int vector : 8,
   delivery_mode : 3,
   __reserved_1 : 1,
   delivery_status : 1,
   polarity : 1,
   remote_irr : 1,
   trigger : 1,
   mask : 1,
   __reserved_2 : 15;
  unsigned int __reserved_3[3];
 } lvt_lint1;

        struct {
  unsigned int vector : 8,
   __reserved_1 : 4,
   delivery_status : 1,
   __reserved_2 : 3,
   mask : 1,
   __reserved_3 : 15;
  unsigned int __reserved_4[3];
 } lvt_error;

        struct {
  unsigned int initial_count;
  unsigned int __reserved_2[3];
 } timer_icr;

        const
 struct {
  unsigned int curr_count;
  unsigned int __reserved_2[3];
 } timer_ccr;

        struct { unsigned int __reserved[4]; } __reserved_16;

        struct { unsigned int __reserved[4]; } __reserved_17;

        struct { unsigned int __reserved[4]; } __reserved_18;

        struct { unsigned int __reserved[4]; } __reserved_19;

        struct {
  unsigned int divisor : 4,
   __reserved_1 : 28;
  unsigned int __reserved_2[3];
 } timer_dcr;

        struct { unsigned int __reserved[4]; } __reserved_20;

} __attribute__ ((packed));
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/numa_64.h" 2

struct bootnode {
 u64 start;
 u64 end;
};

extern int compute_hash_shift(struct bootnode *nodes, int numblks,
         int *nodeids);



extern void numa_init_array(void);
extern int numa_off;

extern void srat_reserve_add_area(int nodeid);
extern int hotadd_percent;

extern s16 apicid_to_node[32768];

extern unsigned long numa_free_all_bootmem(void);
extern void setup_node_bootmem(int nodeid, unsigned long start,
          unsigned long end);


extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) init_cpu_to_node(void);
extern void __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) numa_set_node(int cpu, int node);
extern void __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) numa_clear_node(int cpu);
extern void __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) numa_add_cpu(int cpu);
extern void __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) numa_remove_cpu(int cpu);
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/numa.h" 2
# 29 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h" 2
# 57 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h"
int __acpi_acquire_global_lock(unsigned int *lock);
int __acpi_release_global_lock(unsigned int *lock);
# 83 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h"
extern int acpi_lapic;
extern int acpi_ioapic;
extern int acpi_noirq;
extern int acpi_strict;
extern int acpi_disabled;
extern int acpi_ht;
extern int acpi_pci_disabled;
extern int acpi_skip_timer_override;
extern int acpi_use_timer_override;

extern u8 acpi_sci_flags;
extern int acpi_sci_override_gsi;
void acpi_pic_sci_set_trigger(unsigned int, u16);

static inline __attribute__((always_inline)) void disable_acpi(void)
{
 acpi_disabled = 1;
 acpi_ht = 0;
 acpi_pci_disabled = 1;
 acpi_noirq = 1;
}




extern int acpi_gsi_to_irq(u32 gsi, unsigned int *irq);

static inline __attribute__((always_inline)) void acpi_noirq_set(void) { acpi_noirq = 1; }
static inline __attribute__((always_inline)) void acpi_disable_pci(void)
{
 acpi_pci_disabled = 1;
 acpi_noirq_set();
}


extern int acpi_save_state_mem(void);
extern void acpi_restore_state_mem(void);

extern unsigned long acpi_wakeup_address;


extern void acpi_reserve_bootmem(void);




static inline __attribute__((always_inline)) unsigned int acpi_processor_cstate_check(unsigned int max_cstate)
{






 if (boot_cpu_data.x86 == 0x0F &&
     boot_cpu_data.x86_vendor == 2 &&
     boot_cpu_data.x86_model <= 0x05 &&
     boot_cpu_data.x86_mask < 0x0A)
  return 1;
 else if ((__builtin_constant_p((3*32+21)) && ( ((((3*32+21))>>5)==0 && (1UL<<(((3*32+21))&31) & ((1<<((0*32+ 0) & 31))|0|(1<<((0*32+ 5) & 31))|(1<<((0*32+ 6) & 31))| (1<<((0*32+ 8) & 31))|(1<<((0*32+13) & 31))|(1<<((0*32+24) & 31))|(1<<((0*32+15) & 31))| (1<<((0*32+25) & 31))|(1<<((0*32+26) & 31))))) || ((((3*32+21))>>5)==1 && (1UL<<(((3*32+21))&31) & ((1<<((1*32+29) & 31))|0))) || ((((3*32+21))>>5)==2 && (1UL<<(((3*32+21))&31) & 0)) || ((((3*32+21))>>5)==3 && (1UL<<(((3*32+21))&31) & ((1<<((3*32+20) & 31))))) || ((((3*32+21))>>5)==4 && (1UL<<(((3*32+21))&31) & 0)) || ((((3*32+21))>>5)==5 && (1UL<<(((3*32+21))&31) & 0)) || ((((3*32+21))>>5)==6 && (1UL<<(((3*32+21))&31) & 0)) || ((((3*32+21))>>5)==7 && (1UL<<(((3*32+21))&31) & 0)) ) ? 1 : (__builtin_constant_p(((3*32+21))) ? constant_test_bit(((3*32+21)), ((unsigned long *)((&boot_cpu_data)->x86_capability))) : variable_test_bit(((3*32+21)), ((unsigned long *)((&boot_cpu_data)->x86_capability))))))
  return 1;
 else
  return max_cstate;
}
# 160 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/acpi.h"
struct bootnode;


extern int acpi_numa;
extern int acpi_scan_nodes(unsigned long start, unsigned long end);

extern void acpi_fake_nodes(const struct bootnode *fake_nodes,
       int num_nodes);
# 16 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap_64.h" 2


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vsyscall.h" 1



enum vsyscall_num {
 __NR_vgettimeofday,
 __NR_vtime,
 __NR_vgetcpu,
};
# 33 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vsyscall.h"
extern int __vgetcpu_mode;
extern volatile unsigned long __jiffies;


extern int vgetcpu_mode;
extern struct timezone sys_tz;

extern void map_vsyscall(void);
# 19 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap_64.h" 2
# 35 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap_64.h"
enum fixed_addresses {
 VSYSCALL_LAST_PAGE,
 VSYSCALL_FIRST_PAGE = VSYSCALL_LAST_PAGE
       + (((-2UL << 20)-(-10UL << 20)) >> 12) - 1,
 VSYSCALL_HPET,
 FIX_DBGP_BASE,
 FIX_EARLYCON_MEM_BASE,
 FIX_APIC_BASE,
 FIX_IO_APIC_BASE_0,
 FIX_IO_APIC_BASE_END = FIX_IO_APIC_BASE_0 + 128 - 1,



 __end_of_permanent_fixed_addresses,

 FIX_ACPI_BEGIN,
 FIX_ACPI_END = FIX_ACPI_BEGIN + 4 - 1,
# 65 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap_64.h"
 FIX_BTMAP_END = __end_of_permanent_fixed_addresses + 256 -
   (__end_of_permanent_fixed_addresses & 255),
 FIX_BTMAP_BEGIN = FIX_BTMAP_END + 64*4 - 1,
 __end_of_fixed_addresses
};
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap.h" 2


extern int fixmaps_set;

extern pte_t *kmap_pte;
extern pgprot_t kmap_prot;
extern pte_t *pkmap_page_table;

void __native_set_fixmap(enum fixed_addresses idx, pte_t pte);
void native_set_fixmap(enum fixed_addresses idx,
         unsigned long phys, pgprot_t flags);


static inline __attribute__((always_inline)) void __set_fixmap(enum fixed_addresses idx,
    unsigned long phys, pgprot_t flags)
{
 native_set_fixmap(idx, phys, flags);
}
# 43 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap.h"
extern void __this_fixmap_does_not_exist(void);






static inline __attribute__((always_inline)) __attribute__((always_inline)) unsigned long fix_to_virt(const unsigned int idx)
{
# 61 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap.h"
 if (idx >= __end_of_fixed_addresses)
  __this_fixmap_does_not_exist();

 return (((-2UL << 20)-((1UL) << 12)) - ((idx) << 12));
}

static inline __attribute__((always_inline)) unsigned long virt_to_fix(const unsigned long vaddr)
{
 do { if (__builtin_expect(!!(vaddr >= ((-2UL << 20)-((1UL) << 12)) || vaddr < (((-2UL << 20)-((1UL) << 12)) - (__end_of_fixed_addresses << 12))), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fixmap.h"), "i" (69), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 return ((((-2UL << 20)-((1UL) << 12)) - ((vaddr)&(~(((1UL) << 12)-1)))) >> 12);
}
# 9 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apic.h" 2
# 36 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apic.h"
extern void generic_apic_probe(void);



extern unsigned int apic_verbosity;
extern int local_apic_timer_c2_ok;

extern int disable_apic;
# 54 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apic.h"
extern int is_vsmp_box(void);
extern void xapic_wait_icr_idle(void);
extern u32 safe_xapic_wait_icr_idle(void);
extern void xapic_icr_write(u32, u32);
extern int setup_profiling_timer(unsigned int);

static inline __attribute__((always_inline)) void native_apic_mem_write(u32 reg, u32 v)
{
 volatile u32 *addr = (volatile u32 *)((fix_to_virt(FIX_APIC_BASE)) + reg);

 asm volatile ("661:\n\t" "movl %0, %1" "\n662:\n" ".section .altinstructions,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "661b\n" " " ".quad" " " "663f\n" "	 .byte %c[feat]\n" "	 .byte 662b-661b\n" "	 .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "xchgl %0, %1" "\n664:\n" ".previous" : "=r" (v), "=m" (*addr) : [feat] "i" ((3*32+19)), "0" (v), "m" (*addr))

                                           ;
}

static inline __attribute__((always_inline)) u32 native_apic_mem_read(u32 reg)
{
 return *((volatile u32 *)((fix_to_virt(FIX_APIC_BASE)) + reg));
}

static inline __attribute__((always_inline)) void native_apic_msr_write(u32 reg, u32 v)
{
 if (reg == 0xE0 || reg == 0x20 || reg == 0xD0 ||
     reg == 0x30)
  return;

 wrmsr(0x800 + (reg >> 4), v, 0);
}

static inline __attribute__((always_inline)) u32 native_apic_msr_read(u32 reg)
{
 u32 low, high;

 if (reg == 0xE0)
  return -1;

 do { u64 __val = native_read_msr((0x800 + (reg >> 4))); (low) = (u32)__val; (high) = (u32)(__val >> 32); } while (0);
 return low;
}


extern int x2apic;
extern void check_x2apic(void);
extern void enable_x2apic(void);
extern void enable_IR_x2apic(void);
extern void x2apic_icr_write(u32 low, u32 id);
static inline __attribute__((always_inline)) int x2apic_enabled(void)
{
 int msr, msr2;

 if (!(__builtin_constant_p((4*32+21)) && ( ((((4*32+21))>>5)==0 && (1UL<<(((4*32+21))&31) & ((1<<((0*32+ 0) & 31))|0|(1<<((0*32+ 5) & 31))|(1<<((0*32+ 6) & 31))| (1<<((0*32+ 8) & 31))|(1<<((0*32+13) & 31))|(1<<((0*32+24) & 31))|(1<<((0*32+15) & 31))| (1<<((0*32+25) & 31))|(1<<((0*32+26) & 31))))) || ((((4*32+21))>>5)==1 && (1UL<<(((4*32+21))&31) & ((1<<((1*32+29) & 31))|0))) || ((((4*32+21))>>5)==2 && (1UL<<(((4*32+21))&31) & 0)) || ((((4*32+21))>>5)==3 && (1UL<<(((4*32+21))&31) & ((1<<((3*32+20) & 31))))) || ((((4*32+21))>>5)==4 && (1UL<<(((4*32+21))&31) & 0)) || ((((4*32+21))>>5)==5 && (1UL<<(((4*32+21))&31) & 0)) || ((((4*32+21))>>5)==6 && (1UL<<(((4*32+21))&31) & 0)) || ((((4*32+21))>>5)==7 && (1UL<<(((4*32+21))&31) & 0)) ) ? 1 : (__builtin_constant_p(((4*32+21))) ? constant_test_bit(((4*32+21)), ((unsigned long *)((&boot_cpu_data)->x86_capability))) : variable_test_bit(((4*32+21)), ((unsigned long *)((&boot_cpu_data)->x86_capability))))))
  return 0;

 do { u64 __val = native_read_msr((0x0000001b)); (msr) = (u32)__val; (msr2) = (u32)(__val >> 32); } while (0);
 if (msr & (1UL << 10))
  return 1;
 return 0;
}




struct apic_ops {
 u32 (*read)(u32 reg);
 void (*write)(u32 reg, u32 v);
 u64 (*icr_read)(void);
 void (*icr_write)(u32 low, u32 high);
 void (*wait_icr_idle)(void);
 u32 (*safe_wait_icr_idle)(void);
};

extern struct apic_ops *apic_ops;
# 134 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/apic.h"
extern int get_physical_broadcast(void);


static inline __attribute__((always_inline)) void ack_x2APIC_irq(void)
{

 native_apic_msr_write(0xB0, 0);
}



static inline __attribute__((always_inline)) void ack_APIC_irq(void)
{






 (apic_ops->write)(0xB0, 0);
}

extern int lapic_get_maxlvt(void);
extern void clear_local_APIC(void);
extern void connect_bsp_APIC(void);
extern void disconnect_bsp_APIC(int virt_wire_setup);
extern void disable_local_APIC(void);
extern void lapic_shutdown(void);
extern int verify_local_APIC(void);
extern void cache_APIC_registers(void);
extern void sync_Arb_IDs(void);
extern void init_bsp_APIC(void);
extern void setup_local_APIC(void);
extern void end_local_APIC_setup(void);
extern void init_apic_mappings(void);
extern void setup_boot_APIC_clock(void);
extern void setup_secondary_APIC_clock(void);
extern int APIC_init_uniprocessor(void);
extern void enable_NMI_through_LVT0(void);





extern void early_init_lapic_mapping(void);
extern int apic_is_clustered_box(void);







extern u8 setup_APIC_eilvt_mce(u8 vector, u8 msg_type, u8 mask);
extern u8 setup_APIC_eilvt_ibs(u8 vector, u8 msg_type, u8 mask);
# 14 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_apic.h" 1






# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq_vectors.h" 1
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_apic.h" 2
# 27 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_apic.h"
union IO_APIC_reg_00 {
 u32 raw;
 struct {
  u32 __reserved_2 : 14,
   LTS : 1,
   delivery_type : 1,
   __reserved_1 : 8,
   ID : 8;
 } __attribute__ ((packed)) bits;
};

union IO_APIC_reg_01 {
 u32 raw;
 struct {
  u32 version : 8,
   __reserved_2 : 7,
   PRQ : 1,
   entries : 8,
   __reserved_1 : 8;
 } __attribute__ ((packed)) bits;
};

union IO_APIC_reg_02 {
 u32 raw;
 struct {
  u32 __reserved_2 : 24,
   arbitration : 4,
   __reserved_1 : 4;
 } __attribute__ ((packed)) bits;
};

union IO_APIC_reg_03 {
 u32 raw;
 struct {
  u32 boot_DT : 1,
   __reserved_1 : 31;
 } __attribute__ ((packed)) bits;
};

enum ioapic_irq_destination_types {
 dest_Fixed = 0,
 dest_LowestPrio = 1,
 dest_SMI = 2,
 dest__reserved_1 = 3,
 dest_NMI = 4,
 dest_INIT = 5,
 dest__reserved_2 = 6,
 dest_ExtINT = 7
};

struct IO_APIC_route_entry {
 __u32 vector : 8,
  delivery_mode : 3,



  dest_mode : 1,
  delivery_status : 1,
  polarity : 1,
  irr : 1,
  trigger : 1,
  mask : 1,
  __reserved_2 : 15;

 __u32 __reserved_3 : 24,
  dest : 8;
} __attribute__ ((packed));

struct IR_IO_APIC_route_entry {
 __u64 vector : 8,
  zero : 3,
  index2 : 1,
  delivery_status : 1,
  polarity : 1,
  irr : 1,
  trigger : 1,
  mask : 1,
  reserved : 31,
  format : 1,
  index : 15;
} __attribute__ ((packed));






extern int nr_ioapics;
extern int nr_ioapic_registers[128];







struct mp_config_ioapic {
 unsigned long mp_apicaddr;
 unsigned int mp_apicid;
 unsigned char mp_type;
 unsigned char mp_apicver;
 unsigned char mp_flags;
};

struct mp_config_intsrc {
 unsigned int mp_dstapic;
 unsigned char mp_type;
 unsigned char mp_irqtype;
 unsigned short mp_irqflag;
 unsigned char mp_srcbus;
 unsigned char mp_srcbusirq;
 unsigned char mp_dstirq;
};


extern struct mp_config_ioapic mp_ioapics[128];


extern int mp_irq_entries;


extern struct mp_config_intsrc mp_irqs[(256 * 4)];


extern int mpc_default_type;


extern int sis_apic_bug;


extern int skip_ioapic_setup;


extern int noioapicquirk;


extern int noioapicreroute;


extern int timer_through_8259;

static inline __attribute__((always_inline)) void disable_ioapic_setup(void)
{

 noioapicquirk = 1;
 noioapicreroute = -1;

 skip_ioapic_setup = 1;
}
# 185 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/io_apic.h"
extern int io_apic_get_unique_id(int ioapic, int apic_id);
extern int io_apic_get_version(int ioapic);
extern int io_apic_get_redir_entries(int ioapic);
extern int io_apic_set_pci_routing(int ioapic, int pin, int irq,
       int edge_level, int active_high_low);


extern int (*ioapic_renumber_irq)(int ioapic, int irq);
extern void ioapic_init_mappings(void);


extern int save_mask_IO_APIC_setup(void);
extern void restore_IO_APIC_setup(void);
extern void reinit_intr_remapped_IO_APIC(int);


extern void probe_nr_irqs_gsi(void);
# 16 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h" 2







extern cpumask_var_t cpu_callin_mask;
extern cpumask_var_t cpu_callout_mask;
extern cpumask_var_t cpu_initialized_mask;
extern cpumask_var_t cpu_sibling_setup_mask;
# 42 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h"
extern void (*mtrr_hook)(void);
extern void zap_low_mappings(void);

extern int __attribute__ ((__section__(".cpuinit.text"))) __attribute__((__cold__)) get_local_pda(int cpu);

extern int smp_num_siblings;
extern unsigned int num_processors;

extern __typeof__(cpumask_t) per_cpu__cpu_sibling_map;
extern __typeof__(cpumask_t) per_cpu__cpu_core_map;
extern __typeof__(u16) per_cpu__cpu_llc_id;




static inline __attribute__((always_inline)) struct cpumask *cpu_sibling_mask(int cpu)
{
 return &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__cpu_sibling_map))); (typeof((&per_cpu__cpu_sibling_map))) (__ptr + (((((_cpu_pda[cpu])->data_offset))))); }));
}

static inline __attribute__((always_inline)) struct cpumask *cpu_core_mask(int cpu)
{
 return &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__cpu_core_map))); (typeof((&per_cpu__cpu_core_map))) (__ptr + (((((_cpu_pda[cpu])->data_offset))))); }));
}

extern __typeof__(u16) per_cpu__x86_cpu_to_apicid; extern __typeof__(u16) *x86_cpu_to_apicid_early_ptr; extern __typeof__(u16) x86_cpu_to_apicid_early_map[];
extern __typeof__(u16) per_cpu__x86_bios_cpu_apicid; extern __typeof__(u16) *x86_bios_cpu_apicid_early_ptr; extern __typeof__(u16) x86_bios_cpu_apicid_early_map[];


extern struct {
 void *sp;
 unsigned short ss;
} stack_start;

struct smp_ops {
 void (*smp_prepare_boot_cpu)(void);
 void (*smp_prepare_cpus)(unsigned max_cpus);
 void (*smp_cpus_done)(unsigned max_cpus);

 void (*smp_send_stop)(void);
 void (*smp_send_reschedule)(int cpu);

 int (*cpu_up)(unsigned cpu);
 int (*cpu_disable)(void);
 void (*cpu_die)(unsigned int cpu);
 void (*play_dead)(void);

 void (*send_call_func_ipi)(const struct cpumask *mask);
 void (*send_call_func_single_ipi)(int cpu);
};


extern void set_cpu_sibling_map(int cpu);





extern struct smp_ops smp_ops;

static inline __attribute__((always_inline)) void smp_send_stop(void)
{
 smp_ops.smp_send_stop();
}

static inline __attribute__((always_inline)) void smp_prepare_boot_cpu(void)
{
 smp_ops.smp_prepare_boot_cpu();
}

static inline __attribute__((always_inline)) void smp_prepare_cpus(unsigned int max_cpus)
{
 smp_ops.smp_prepare_cpus(max_cpus);
}

static inline __attribute__((always_inline)) void smp_cpus_done(unsigned int max_cpus)
{
 smp_ops.smp_cpus_done(max_cpus);
}

static inline __attribute__((always_inline)) int __cpu_up(unsigned int cpu)
{
 return smp_ops.cpu_up(cpu);
}

static inline __attribute__((always_inline)) int __cpu_disable(void)
{
 return smp_ops.cpu_disable();
}

static inline __attribute__((always_inline)) void __cpu_die(unsigned int cpu)
{
 smp_ops.cpu_die(cpu);
}

static inline __attribute__((always_inline)) void play_dead(void)
{
 smp_ops.play_dead();
}

static inline __attribute__((always_inline)) void smp_send_reschedule(int cpu)
{
 smp_ops.smp_send_reschedule(cpu);
}

static inline __attribute__((always_inline)) void arch_send_call_function_single_ipi(int cpu)
{
 smp_ops.send_call_func_single_ipi(cpu);
}

static inline __attribute__((always_inline)) void arch_send_call_function_ipi(cpumask_t mask)
{
 smp_ops.send_call_func_ipi(&mask);
}

void cpu_disable_common(void);
void native_smp_prepare_boot_cpu(void);
void native_smp_prepare_cpus(unsigned int max_cpus);
void native_smp_cpus_done(unsigned int max_cpus);
int native_cpu_up(unsigned int cpunum);
int native_cpu_disable(void);
void native_cpu_die(unsigned int cpu);
void native_play_dead(void);
void play_dead_common(void);

void native_send_call_func_ipi(const struct cpumask *mask);
void native_send_call_func_single_ipi(int cpu);

extern void prefill_possible_map(void);

void smp_store_cpu_info(int id);



static inline __attribute__((always_inline)) int num_booting_cpus(void)
{
 return cpumask_weight(cpu_callout_mask);
}






extern unsigned disabled_cpus __attribute__ ((__section__(".cpuinit.data")));
# 236 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/smp.h"
extern int hard_smp_processor_id(void);
# 37 "include/linux/smp.h" 2
# 46 "include/linux/smp.h"
extern void smp_send_stop(void);




extern void smp_send_reschedule(int cpu);





extern void smp_prepare_cpus(unsigned int max_cpus);




extern int __cpu_up(unsigned int cpunum);




extern void smp_cpus_done(unsigned int max_cpus);




int smp_call_function(void(*func)(void *info), void *info, int wait);
void smp_call_function_many(const struct cpumask *mask,
       void (*func)(void *info), void *info, bool wait);


static inline __attribute__((always_inline)) int
smp_call_function_mask(cpumask_t mask, void(*func)(void *info), void *info,
         int wait)
{
 smp_call_function_many(&mask, func, info, wait);
 return 0;
}

void __smp_call_function_single(int cpuid, struct call_single_data *data);





void generic_smp_call_function_single_interrupt(void);
void generic_smp_call_function_interrupt(void);
void ipi_call_lock(void);
void ipi_call_unlock(void);
void ipi_call_lock_irq(void);
void ipi_call_unlock_irq(void);





int on_each_cpu(void (*func) (void *info), void *info, int wait);
# 118 "include/linux/smp.h"
void smp_prepare_boot_cpu(void);

extern unsigned int setup_max_cpus;
# 179 "include/linux/smp.h"
void smp_setup_processor_id(void);
# 69 "include/linux/sched.h" 2
# 1 "include/linux/sem.h" 1



# 1 "include/linux/ipc.h" 1
# 9 "include/linux/ipc.h"
struct ipc_perm
{
 __kernel_key_t key;
 __kernel_uid_t uid;
 __kernel_gid_t gid;
 __kernel_uid_t cuid;
 __kernel_gid_t cgid;
 __kernel_mode_t mode;
 unsigned short seq;
};


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ipcbuf.h" 1
# 14 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ipcbuf.h"
struct ipc64_perm {
 __kernel_key_t key;
 __kernel_uid32_t uid;
 __kernel_gid32_t gid;
 __kernel_uid32_t cuid;
 __kernel_gid32_t cgid;
 __kernel_mode_t mode;
 unsigned short __pad1;
 unsigned short seq;
 unsigned short __pad2;
 unsigned long __unused1;
 unsigned long __unused2;
};
# 22 "include/linux/ipc.h" 2
# 57 "include/linux/ipc.h"
struct ipc_kludge {
 struct msgbuf __attribute__((noderef, address_space(1))) *msgp;
 long msgtyp;
};
# 82 "include/linux/ipc.h"
# 1 "include/linux/kref.h" 1
# 19 "include/linux/kref.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 20 "include/linux/kref.h" 2

struct kref {
 atomic_t refcount;
};

void kref_set(struct kref *kref, int num);
void kref_init(struct kref *kref);
void kref_get(struct kref *kref);
int kref_put(struct kref *kref, void (*release) (struct kref *kref));
# 83 "include/linux/ipc.h" 2





struct kern_ipc_perm
{
 spinlock_t lock;
 int deleted;
 int id;
 key_t key;
 uid_t uid;
 gid_t gid;
 uid_t cuid;
 gid_t cgid;
 mode_t mode;
 unsigned long seq;
 void *security;
};
# 5 "include/linux/sem.h" 2
# 23 "include/linux/sem.h"
struct semid_ds {
 struct ipc_perm sem_perm;
 __kernel_time_t sem_otime;
 __kernel_time_t sem_ctime;
 struct sem *sem_base;
 struct sem_queue *sem_pending;
 struct sem_queue **sem_pending_last;
 struct sem_undo *undo;
 unsigned short sem_nsems;
};


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sembuf.h" 1
# 13 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sembuf.h"
struct semid64_ds {
 struct ipc64_perm sem_perm;
 __kernel_time_t sem_otime;
 unsigned long __unused1;
 __kernel_time_t sem_ctime;
 unsigned long __unused2;
 unsigned long sem_nsems;
 unsigned long __unused3;
 unsigned long __unused4;
};
# 36 "include/linux/sem.h" 2


struct sembuf {
 unsigned short sem_num;
 short sem_op;
 short sem_flg;
};


union semun {
 int val;
 struct semid_ds __attribute__((noderef, address_space(1))) *buf;
 unsigned short __attribute__((noderef, address_space(1))) *array;
 struct seminfo __attribute__((noderef, address_space(1))) *__buf;
 void __attribute__((noderef, address_space(1))) *__pad;
};

struct seminfo {
 int semmap;
 int semmni;
 int semmns;
 int semmnu;
 int semmsl;
 int semopm;
 int semume;
 int semusz;
 int semvmx;
 int semaem;
};
# 80 "include/linux/sem.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 81 "include/linux/sem.h" 2
# 1 "include/linux/rcupdate.h" 1
# 39 "include/linux/rcupdate.h"
# 1 "include/linux/percpu.h" 1




# 1 "include/linux/slab.h" 1
# 12 "include/linux/slab.h"
# 1 "include/linux/gfp.h" 1



# 1 "include/linux/mmzone.h" 1
# 17 "include/linux/mmzone.h"
# 1 "include/linux/pageblock-flags.h" 1
# 29 "include/linux/pageblock-flags.h"
enum pageblock_bits {
 PB_migrate,
 PB_migrate_end = PB_migrate + 3 - 1,

 NR_PAGEBLOCK_BITS
};
# 60 "include/linux/pageblock-flags.h"
struct page;


unsigned long get_pageblock_flags_group(struct page *page,
     int start_bitidx, int end_bitidx);
void set_pageblock_flags_group(struct page *page, unsigned long flags,
     int start_bitidx, int end_bitidx);
# 18 "include/linux/mmzone.h" 2
# 1 "include/linux/bounds.h" 1
# 19 "include/linux/mmzone.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 20 "include/linux/mmzone.h" 2
# 49 "include/linux/mmzone.h"
extern int page_group_by_mobility_disabled;

static inline __attribute__((always_inline)) int get_pageblock_migratetype(struct page *page)
{
 if (__builtin_expect(!!(page_group_by_mobility_disabled), 0))
  return 0;

 return get_pageblock_flags_group(page, PB_migrate, PB_migrate_end);
}

struct free_area {
 struct list_head free_list[5];
 unsigned long nr_free;
};

struct pglist_data;
# 73 "include/linux/mmzone.h"
struct zone_padding {
 char x[0];
} __attribute__((__aligned__(1 << ((7)))));





enum zone_stat_item {

 NR_FREE_PAGES,
 NR_LRU_BASE,
 NR_INACTIVE_ANON = NR_LRU_BASE,
 NR_ACTIVE_ANON,
 NR_INACTIVE_FILE,
 NR_ACTIVE_FILE,

 NR_UNEVICTABLE,
 NR_MLOCK,




 NR_ANON_PAGES,
 NR_FILE_MAPPED,

 NR_FILE_PAGES,
 NR_FILE_DIRTY,
 NR_WRITEBACK,
 NR_SLAB_RECLAIMABLE,
 NR_SLAB_UNRECLAIMABLE,
 NR_PAGETABLE,
 NR_UNSTABLE_NFS,
 NR_BOUNCE,
 NR_VMSCAN_WRITE,

 NR_WRITEBACK_TEMP,

 NUMA_HIT,
 NUMA_MISS,
 NUMA_FOREIGN,
 NUMA_INTERLEAVE_HIT,
 NUMA_LOCAL,
 NUMA_OTHER,

 NR_VM_ZONE_STAT_ITEMS };
# 133 "include/linux/mmzone.h"
enum lru_list {
 LRU_INACTIVE_ANON = 0,
 LRU_ACTIVE_ANON = 0 + 1,
 LRU_INACTIVE_FILE = 0 + 2,
 LRU_ACTIVE_FILE = 0 + 2 + 1,

 LRU_UNEVICTABLE,



 NR_LRU_LISTS
};





static inline __attribute__((always_inline)) int is_file_lru(enum lru_list l)
{
 return (l == LRU_INACTIVE_FILE || l == LRU_ACTIVE_FILE);
}

static inline __attribute__((always_inline)) int is_active_lru(enum lru_list l)
{
 return (l == LRU_ACTIVE_ANON || l == LRU_ACTIVE_FILE);
}

static inline __attribute__((always_inline)) int is_unevictable_lru(enum lru_list l)
{

 return (l == LRU_UNEVICTABLE);



}

struct per_cpu_pages {
 int count;
 int high;
 int batch;
 struct list_head list;
};

struct per_cpu_pageset {
 struct per_cpu_pages pcp;

 s8 expire;


 s8 stat_threshold;
 s8 vm_stat_diff[NR_VM_ZONE_STAT_ITEMS];

} __attribute__((__aligned__((1 << (7)))));
# 195 "include/linux/mmzone.h"
enum zone_type {
# 215 "include/linux/mmzone.h"
 ZONE_DMA,







 ZONE_DMA32,






 ZONE_NORMAL,
# 242 "include/linux/mmzone.h"
 ZONE_MOVABLE,
 __MAX_NR_ZONES
};
# 266 "include/linux/mmzone.h"
struct zone_reclaim_stat {
# 275 "include/linux/mmzone.h"
 unsigned long recent_rotated[2];
 unsigned long recent_scanned[2];
};

struct zone {

 unsigned long pages_min, pages_low, pages_high;
# 290 "include/linux/mmzone.h"
 unsigned long lowmem_reserve[4];


 int node;



 unsigned long min_unmapped_pages;
 unsigned long min_slab_pages;
 struct per_cpu_pageset *pageset[255];






 spinlock_t lock;


 seqlock_t span_seqlock;

 struct free_area free_area[11];
# 322 "include/linux/mmzone.h"
 struct zone_padding _pad1_;


 spinlock_t lru_lock;
 struct {
  struct list_head list;
  unsigned long nr_scan;
 } lru[NR_LRU_LISTS];

 struct zone_reclaim_stat reclaim_stat;

 unsigned long pages_scanned;
 unsigned long flags;


 atomic_long_t vm_stat[NR_VM_ZONE_STAT_ITEMS];
# 352 "include/linux/mmzone.h"
 int prev_priority;





 unsigned int inactive_ratio;


 struct zone_padding _pad2_;
# 388 "include/linux/mmzone.h"
 wait_queue_head_t * wait_table;
 unsigned long wait_table_hash_nr_entries;
 unsigned long wait_table_bits;




 struct pglist_data *zone_pgdat;

 unsigned long zone_start_pfn;
# 409 "include/linux/mmzone.h"
 unsigned long spanned_pages;
 unsigned long present_pages;




 const char *name;
} __attribute__((__aligned__(1 << ((7)))));

typedef enum {
 ZONE_ALL_UNRECLAIMABLE,
 ZONE_RECLAIM_LOCKED,
 ZONE_OOM_LOCKED,
} zone_flags_t;

static inline __attribute__((always_inline)) void zone_set_flag(struct zone *zone, zone_flags_t flag)
{
 set_bit(flag, &zone->flags);
}

static inline __attribute__((always_inline)) int zone_test_and_set_flag(struct zone *zone, zone_flags_t flag)
{
 return test_and_set_bit(flag, &zone->flags);
}

static inline __attribute__((always_inline)) void zone_clear_flag(struct zone *zone, zone_flags_t flag)
{
 clear_bit(flag, &zone->flags);
}

static inline __attribute__((always_inline)) int zone_is_all_unreclaimable(const struct zone *zone)
{
 return (__builtin_constant_p((ZONE_ALL_UNRECLAIMABLE)) ? constant_test_bit((ZONE_ALL_UNRECLAIMABLE), (&zone->flags)) : variable_test_bit((ZONE_ALL_UNRECLAIMABLE), (&zone->flags)));
}

static inline __attribute__((always_inline)) int zone_is_reclaim_locked(const struct zone *zone)
{
 return (__builtin_constant_p((ZONE_RECLAIM_LOCKED)) ? constant_test_bit((ZONE_RECLAIM_LOCKED), (&zone->flags)) : variable_test_bit((ZONE_RECLAIM_LOCKED), (&zone->flags)));
}

static inline __attribute__((always_inline)) int zone_is_oom_locked(const struct zone *zone)
{
 return (__builtin_constant_p((ZONE_OOM_LOCKED)) ? constant_test_bit((ZONE_OOM_LOCKED), (&zone->flags)) : variable_test_bit((ZONE_OOM_LOCKED), (&zone->flags)));
}
# 535 "include/linux/mmzone.h"
struct zonelist_cache {
 unsigned short z_to_n[((1 << 6) * 4)];
 unsigned long fullzones[(((((1 << 6) * 4)) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];
 unsigned long last_full_zap;
};
# 549 "include/linux/mmzone.h"
struct zoneref {
 struct zone *zone;
 int zone_idx;
};
# 571 "include/linux/mmzone.h"
struct zonelist {
 struct zonelist_cache *zlcache_ptr;
 struct zoneref _zonerefs[((1 << 6) * 4) + 1];

 struct zonelist_cache zlcache;

};


struct node_active_region {
 unsigned long start_pfn;
 unsigned long end_pfn;
 int nid;
};




extern struct page *mem_map;
# 603 "include/linux/mmzone.h"
struct bootmem_data;
typedef struct pglist_data {
 struct zone node_zones[4];
 struct zonelist node_zonelists[2];
 int nr_zones;






 struct bootmem_data *bdata;
# 623 "include/linux/mmzone.h"
 spinlock_t node_size_lock;

 unsigned long node_start_pfn;
 unsigned long node_present_pages;
 unsigned long node_spanned_pages;

 int node_id;
 wait_queue_head_t kswapd_wait;
 struct task_struct *kswapd;
 int kswapd_max_order;
} pg_data_t;
# 644 "include/linux/mmzone.h"
# 1 "include/linux/memory_hotplug.h" 1



# 1 "include/linux/mmzone.h" 1
# 5 "include/linux/memory_hotplug.h" 2

# 1 "include/linux/notifier.h" 1
# 15 "include/linux/notifier.h"
# 1 "include/linux/srcu.h" 1
# 30 "include/linux/srcu.h"
struct srcu_struct_array {
 int c[2];
};

struct srcu_struct {
 int completed;
 struct srcu_struct_array *per_cpu_ref;
 struct mutex mutex;
};







int init_srcu_struct(struct srcu_struct *sp);
void cleanup_srcu_struct(struct srcu_struct *sp);
int srcu_read_lock(struct srcu_struct *sp) ;
void srcu_read_unlock(struct srcu_struct *sp, int idx) ;
void synchronize_srcu(struct srcu_struct *sp);
long srcu_batches_completed(struct srcu_struct *sp);
# 16 "include/linux/notifier.h" 2
# 50 "include/linux/notifier.h"
struct notifier_block {
 int (*notifier_call)(struct notifier_block *, unsigned long, void *);
 struct notifier_block *next;
 int priority;
};

struct atomic_notifier_head {
 spinlock_t lock;
 struct notifier_block *head;
};

struct blocking_notifier_head {
 struct rw_semaphore rwsem;
 struct notifier_block *head;
};

struct raw_notifier_head {
 struct notifier_block *head;
};

struct srcu_notifier_head {
 struct mutex mutex;
 struct srcu_struct srcu;
 struct notifier_block *head;
};
# 89 "include/linux/notifier.h"
extern void srcu_init_notifier_head(struct srcu_notifier_head *nh);
# 115 "include/linux/notifier.h"
extern int atomic_notifier_chain_register(struct atomic_notifier_head *nh,
  struct notifier_block *nb);
extern int blocking_notifier_chain_register(struct blocking_notifier_head *nh,
  struct notifier_block *nb);
extern int raw_notifier_chain_register(struct raw_notifier_head *nh,
  struct notifier_block *nb);
extern int srcu_notifier_chain_register(struct srcu_notifier_head *nh,
  struct notifier_block *nb);

extern int blocking_notifier_chain_cond_register(
  struct blocking_notifier_head *nh,
  struct notifier_block *nb);

extern int atomic_notifier_chain_unregister(struct atomic_notifier_head *nh,
  struct notifier_block *nb);
extern int blocking_notifier_chain_unregister(struct blocking_notifier_head *nh,
  struct notifier_block *nb);
extern int raw_notifier_chain_unregister(struct raw_notifier_head *nh,
  struct notifier_block *nb);
extern int srcu_notifier_chain_unregister(struct srcu_notifier_head *nh,
  struct notifier_block *nb);

extern int atomic_notifier_call_chain(struct atomic_notifier_head *nh,
  unsigned long val, void *v);
extern int __atomic_notifier_call_chain(struct atomic_notifier_head *nh,
 unsigned long val, void *v, int nr_to_call, int *nr_calls);
extern int blocking_notifier_call_chain(struct blocking_notifier_head *nh,
  unsigned long val, void *v);
extern int __blocking_notifier_call_chain(struct blocking_notifier_head *nh,
 unsigned long val, void *v, int nr_to_call, int *nr_calls);
extern int raw_notifier_call_chain(struct raw_notifier_head *nh,
  unsigned long val, void *v);
extern int __raw_notifier_call_chain(struct raw_notifier_head *nh,
 unsigned long val, void *v, int nr_to_call, int *nr_calls);
extern int srcu_notifier_call_chain(struct srcu_notifier_head *nh,
  unsigned long val, void *v);
extern int __srcu_notifier_call_chain(struct srcu_notifier_head *nh,
 unsigned long val, void *v, int nr_to_call, int *nr_calls);
# 165 "include/linux/notifier.h"
static inline __attribute__((always_inline)) int notifier_from_errno(int err)
{
 return 0x8000 | (0x0001 - err);
}


static inline __attribute__((always_inline)) int notifier_to_errno(int ret)
{
 ret &= ~0x8000;
 return ret > 0x0001 ? 0x0001 - ret : 0;
}
# 258 "include/linux/notifier.h"
extern struct blocking_notifier_head reboot_notifier_list;
# 7 "include/linux/memory_hotplug.h" 2

struct page;
struct zone;
struct pglist_data;
struct mem_section;
# 26 "include/linux/memory_hotplug.h"
static inline __attribute__((always_inline))
void pgdat_resize_lock(struct pglist_data *pgdat, unsigned long *flags)
{
 do { ({ unsigned long __dummy; typeof(*flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); *flags = _spin_lock_irqsave(&pgdat->node_size_lock); } while (0);
}
static inline __attribute__((always_inline))
void pgdat_resize_unlock(struct pglist_data *pgdat, unsigned long *flags)
{
 do { ({ unsigned long __dummy; typeof(*flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); _spin_unlock_irqrestore(&pgdat->node_size_lock, *flags); } while (0);
}
static inline __attribute__((always_inline))
void pgdat_resize_init(struct pglist_data *pgdat)
{
 do { static struct lock_class_key __key; __spin_lock_init((&pgdat->node_size_lock), "&pgdat->node_size_lock", &__key); } while (0);
}



static inline __attribute__((always_inline)) unsigned zone_span_seqbegin(struct zone *zone)
{
 return read_seqbegin(&zone->span_seqlock);
}
static inline __attribute__((always_inline)) int zone_span_seqretry(struct zone *zone, unsigned iv)
{
 return read_seqretry(&zone->span_seqlock, iv);
}
static inline __attribute__((always_inline)) void zone_span_writelock(struct zone *zone)
{
 write_seqlock(&zone->span_seqlock);
}
static inline __attribute__((always_inline)) void zone_span_writeunlock(struct zone *zone)
{
 write_sequnlock(&zone->span_seqlock);
}
static inline __attribute__((always_inline)) void zone_seqlock_init(struct zone *zone)
{
 do { (&zone->span_seqlock)->sequence = 0; do { static struct lock_class_key __key; __spin_lock_init((&(&zone->span_seqlock)->lock), "&(&zone->span_seqlock)->lock", &__key); } while (0); } while (0);
}
extern int zone_grow_free_lists(struct zone *zone, unsigned long new_nr_pages);
extern int zone_grow_waitqueues(struct zone *zone, unsigned long nr_pages);
extern int add_one_highpage(struct page *page, int pfn, int bad_ppro);

extern void online_page(struct page *page);

extern int online_pages(unsigned long, unsigned long);
extern void __offline_isolated_pages(unsigned long, unsigned long);
extern int offline_pages(unsigned long, unsigned long, unsigned long);


extern int __add_pages(int nid, struct zone *zone, unsigned long start_pfn,
 unsigned long nr_pages);
extern int __remove_pages(struct zone *zone, unsigned long start_pfn,
 unsigned long nr_pages);


extern int memory_add_physaddr_to_nid(u64 start);
# 126 "include/linux/memory_hotplug.h"
extern pg_data_t *node_data[];
static inline __attribute__((always_inline)) void arch_refresh_nodedata(int nid, pg_data_t *pgdat)
{
 node_data[nid] = pgdat;
}
# 150 "include/linux/memory_hotplug.h"
static inline __attribute__((always_inline)) void register_page_bootmem_info_node(struct pglist_data *pgdat)
{
}
static inline __attribute__((always_inline)) void put_page_bootmem(struct page *page)
{
}
# 198 "include/linux/memory_hotplug.h"
extern int walk_memory_resource(unsigned long start_pfn,
   unsigned long nr_pages, void *arg,
   int (*func)(unsigned long, unsigned long, void *));






static inline __attribute__((always_inline)) int is_mem_section_removable(unsigned long pfn,
     unsigned long nr_pages)
{
 return 0;
}


extern int add_memory(int nid, u64 start, u64 size);
extern int arch_add_memory(int nid, u64 start, u64 size);
extern int remove_memory(u64 start, u64 size);
extern int sparse_add_one_section(struct zone *zone, unsigned long start_pfn,
        int nr_pages);
extern void sparse_remove_one_section(struct zone *zone, struct mem_section *ms);
extern struct page *sparse_decode_mem_map(unsigned long coded_mem_map,
       unsigned long pnum);
# 645 "include/linux/mmzone.h" 2

void get_zone_counts(unsigned long *active, unsigned long *inactive,
   unsigned long *free);
void build_all_zonelists(void);
void wakeup_kswapd(struct zone *zone, int order);
int zone_watermark_ok(struct zone *z, int order, unsigned long mark,
  int classzone_idx, int alloc_flags);
enum memmap_context {
 MEMMAP_EARLY,
 MEMMAP_HOTPLUG,
};
extern int init_currently_empty_zone(struct zone *zone, unsigned long start_pfn,
         unsigned long size,
         enum memmap_context context);


void memory_present(int nid, unsigned long start, unsigned long end);
# 675 "include/linux/mmzone.h"
static inline __attribute__((always_inline)) int populated_zone(struct zone *zone)
{
 return (!!zone->present_pages);
}

extern int movable_zone;

static inline __attribute__((always_inline)) int zone_movable_is_highmem(void)
{



 return 0;

}

static inline __attribute__((always_inline)) int is_highmem_idx(enum zone_type idx)
{




 return 0;

}

static inline __attribute__((always_inline)) int is_normal_idx(enum zone_type idx)
{
 return (idx == ZONE_NORMAL);
}







static inline __attribute__((always_inline)) int is_highmem(struct zone *zone)
{






 return 0;

}

static inline __attribute__((always_inline)) int is_normal(struct zone *zone)
{
 return zone == zone->zone_pgdat->node_zones + ZONE_NORMAL;
}

static inline __attribute__((always_inline)) int is_dma32(struct zone *zone)
{

 return zone == zone->zone_pgdat->node_zones + ZONE_DMA32;



}

static inline __attribute__((always_inline)) int is_dma(struct zone *zone)
{

 return zone == zone->zone_pgdat->node_zones + ZONE_DMA;



}


struct ctl_table;
struct file;
int min_free_kbytes_sysctl_handler(struct ctl_table *, int, struct file *,
     void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int sysctl_lowmem_reserve_ratio[4 -1];
int lowmem_reserve_ratio_sysctl_handler(struct ctl_table *, int, struct file *,
     void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
int percpu_pagelist_fraction_sysctl_handler(struct ctl_table *, int, struct file *,
     void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
int sysctl_min_unmapped_ratio_sysctl_handler(struct ctl_table *, int,
   struct file *, void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
int sysctl_min_slab_ratio_sysctl_handler(struct ctl_table *, int,
   struct file *, void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);

extern int numa_zonelist_order_handler(struct ctl_table *, int,
   struct file *, void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern char numa_zonelist_order[];


# 1 "include/linux/topology.h" 1
# 34 "include/linux/topology.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h" 1
# 80 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h"
extern cpumask_t *node_to_cpumask_map;


extern __typeof__(int) per_cpu__x86_cpu_to_node_map; extern __typeof__(int) *x86_cpu_to_node_map_early_ptr; extern __typeof__(int) x86_cpu_to_node_map_early_map[];
# 97 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h"
static inline __attribute__((always_inline)) int cpu_to_node(int cpu)
{
 return (*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__x86_cpu_to_node_map))); (typeof((&per_cpu__x86_cpu_to_node_map))) (__ptr + (((((_cpu_pda[cpu])->data_offset))))); }));
}


static inline __attribute__((always_inline)) int early_cpu_to_node(int cpu)
{
 if ((x86_cpu_to_node_map_early_ptr))
  return (x86_cpu_to_node_map_early_ptr)[cpu];

 return (*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__x86_cpu_to_node_map))); (typeof((&per_cpu__x86_cpu_to_node_map))) (__ptr + (((((_cpu_pda[cpu])->data_offset))))); }));
}


static inline __attribute__((always_inline)) const cpumask_t *cpumask_of_node(int node)
{
 return &node_to_cpumask_map[node];
}


static inline __attribute__((always_inline)) cpumask_t node_to_cpumask(int node)
{
 return node_to_cpumask_map[node];
}
# 189 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h"
extern int __node_distance(int, int);
# 223 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h"
# 1 "include/asm-generic/topology.h" 1
# 224 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h" 2



static inline __attribute__((always_inline)) int node_to_first_cpu(int node)
{
 return cpumask_first(cpumask_of_node(node));
}


extern cpumask_t cpu_coregroup_map(int cpu);
extern const struct cpumask *cpu_coregroup_mask(int cpu);
# 248 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/topology.h"
static inline __attribute__((always_inline)) void arch_fix_phys_package_id(int num, u32 slot)
{
}

struct pci_bus;
void set_pci_bus_resources_arch_default(struct pci_bus *b);







extern int get_mp_bus_to_node(int busnum);
extern void set_mp_bus_to_node(int busnum, int node);
# 35 "include/linux/topology.h" 2
# 52 "include/linux/topology.h"
int arch_update_cpu_topology(void);
# 768 "include/linux/mmzone.h" 2
# 781 "include/linux/mmzone.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone_64.h" 1
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone_64.h"
# 1 "include/linux/mmdebug.h" 1



# 1 "include/linux/autoconf.h" 1
# 5 "include/linux/mmdebug.h" 2
# 11 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone_64.h" 2




struct memnode {
 int shift;
 unsigned int mapsize;
 s16 *map;
 s16 embedded_map[64 - 8];
} __attribute__((__aligned__((1 << (7)))));
extern struct memnode memnode;




extern struct pglist_data *node_data[];

static inline __attribute__((always_inline)) __attribute__((pure)) int phys_to_nid(unsigned long addr)
{
 unsigned nid;
 do { if (__builtin_expect(!!(!memnode.map), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone_64.h"), "i" (31), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 nid = memnode.map[addr >> memnode.shift];
 do { if (__builtin_expect(!!(nid >= (1 << 6) || !node_data[nid]), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone_64.h"), "i" (33), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 return nid;
}
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/mmzone.h" 2
# 782 "include/linux/mmzone.h" 2



extern struct pglist_data *first_online_pgdat(void);
extern struct pglist_data *next_online_pgdat(struct pglist_data *pgdat);
extern struct zone *next_zone(struct zone *zone);
# 809 "include/linux/mmzone.h"
static inline __attribute__((always_inline)) struct zone *zonelist_zone(struct zoneref *zoneref)
{
 return zoneref->zone;
}

static inline __attribute__((always_inline)) int zonelist_zone_idx(struct zoneref *zoneref)
{
 return zoneref->zone_idx;
}

static inline __attribute__((always_inline)) int zonelist_node_idx(struct zoneref *zoneref)
{


 return zoneref->zone->node;



}
# 842 "include/linux/mmzone.h"
struct zoneref *next_zones_zonelist(struct zoneref *z,
     enum zone_type highest_zoneidx,
     nodemask_t *nodes,
     struct zone **zone);
# 859 "include/linux/mmzone.h"
static inline __attribute__((always_inline)) struct zoneref *first_zones_zonelist(struct zonelist *zonelist,
     enum zone_type highest_zoneidx,
     nodemask_t *nodes,
     struct zone **zone)
{
 return next_zones_zonelist(zonelist->_zonerefs, highest_zoneidx, nodes,
        zone);
}
# 897 "include/linux/mmzone.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sparsemem.h" 1
# 898 "include/linux/mmzone.h" 2
# 940 "include/linux/mmzone.h"
struct page;
struct page_cgroup;
struct mem_section {
# 955 "include/linux/mmzone.h"
 unsigned long section_mem_map;


 unsigned long *pageblock_flags;
# 967 "include/linux/mmzone.h"
};
# 980 "include/linux/mmzone.h"
extern struct mem_section *mem_section[((1UL << (44 - 27)) / (((1UL) << 12) / sizeof (struct mem_section)))];




static inline __attribute__((always_inline)) struct mem_section *__nr_to_section(unsigned long nr)
{
 if (!mem_section[((nr) / (((1UL) << 12) / sizeof (struct mem_section)))])
  return ((void *)0);
 return &mem_section[((nr) / (((1UL) << 12) / sizeof (struct mem_section)))][nr & ((((1UL) << 12) / sizeof (struct mem_section)) - 1)];
}
extern int __section_nr(struct mem_section* ms);
extern unsigned long usemap_size(void);
# 1005 "include/linux/mmzone.h"
static inline __attribute__((always_inline)) struct page *__section_mem_map_addr(struct mem_section *section)
{
 unsigned long map = section->section_mem_map;
 map &= (~((1UL<<2)-1));
 return (struct page *)map;
}

static inline __attribute__((always_inline)) int present_section(struct mem_section *section)
{
 return (section && (section->section_mem_map & (1UL<<0)));
}

static inline __attribute__((always_inline)) int present_section_nr(unsigned long nr)
{
 return present_section(__nr_to_section(nr));
}

static inline __attribute__((always_inline)) int valid_section(struct mem_section *section)
{
 return (section && (section->section_mem_map & (1UL<<1)));
}

static inline __attribute__((always_inline)) int valid_section_nr(unsigned long nr)
{
 return valid_section(__nr_to_section(nr));
}

static inline __attribute__((always_inline)) struct mem_section *__pfn_to_section(unsigned long pfn)
{
 return __nr_to_section(((pfn) >> (27 - 12)));
}

static inline __attribute__((always_inline)) int pfn_valid(unsigned long pfn)
{
 if (((pfn) >> (27 - 12)) >= (1UL << (44 - 27)))
  return 0;
 return valid_section(__nr_to_section(((pfn) >> (27 - 12))));
}

static inline __attribute__((always_inline)) int pfn_present(unsigned long pfn)
{
 if (((pfn) >> (27 - 12)) >= (1UL << (44 - 27)))
  return 0;
 return present_section(__nr_to_section(((pfn) >> (27 - 12))));
}
# 1067 "include/linux/mmzone.h"
void sparse_init(void);






bool early_pfn_in_nid(unsigned long pfn, int nid);
# 1083 "include/linux/mmzone.h"
void memory_present(int nid, unsigned long start, unsigned long end);
unsigned long __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) node_memmap_size_bytes(int, unsigned long, unsigned long);
# 5 "include/linux/gfp.h" 2



struct vm_area_struct;
# 102 "include/linux/gfp.h"
static inline __attribute__((always_inline)) int allocflags_to_migratetype(gfp_t gfp_flags)
{
 ({ int __ret_warn_on = !!((gfp_flags & ((( gfp_t)0x80000u)|(( gfp_t)0x100000u))) == ((( gfp_t)0x80000u)|(( gfp_t)0x100000u))); if (__builtin_expect(!!(__ret_warn_on), 0)) warn_slowpath("include/linux/gfp.h", 104, ((void *)0)); __builtin_expect(!!(__ret_warn_on), 0); });

 if (__builtin_expect(!!(page_group_by_mobility_disabled), 0))
  return 0;


 return (((gfp_flags & (( gfp_t)0x100000u)) != 0) << 1) |
  ((gfp_flags & (( gfp_t)0x80000u)) != 0);
}

static inline __attribute__((always_inline)) enum zone_type gfp_zone(gfp_t flags)
{

 if (flags & (( gfp_t)0x01u))
  return ZONE_DMA;


 if (flags & (( gfp_t)0x04u))
  return ZONE_DMA32;

 if ((flags & ((( gfp_t)0x02u) | (( gfp_t)0x100000u))) ==
   ((( gfp_t)0x02u) | (( gfp_t)0x100000u)))
  return ZONE_MOVABLE;




 return ZONE_NORMAL;
}
# 141 "include/linux/gfp.h"
static inline __attribute__((always_inline)) int gfp_zonelist(gfp_t flags)
{
 if (1 && __builtin_expect(!!(flags & (( gfp_t)0x40000u)), 0))
  return 1;

 return 0;
}
# 158 "include/linux/gfp.h"
static inline __attribute__((always_inline)) struct zonelist *node_zonelist(int nid, gfp_t flags)
{
 return (node_data[nid])->node_zonelists + gfp_zonelist(flags);
}


static inline __attribute__((always_inline)) void arch_free_page(struct page *page, int order) { }


static inline __attribute__((always_inline)) void arch_alloc_page(struct page *page, int order) { }


struct page *
__alloc_pages_internal(gfp_t gfp_mask, unsigned int order,
         struct zonelist *zonelist, nodemask_t *nodemask);

static inline __attribute__((always_inline)) struct page *
__alloc_pages(gfp_t gfp_mask, unsigned int order,
  struct zonelist *zonelist)
{
 return __alloc_pages_internal(gfp_mask, order, zonelist, ((void *)0));
}

static inline __attribute__((always_inline)) struct page *
__alloc_pages_nodemask(gfp_t gfp_mask, unsigned int order,
  struct zonelist *zonelist, nodemask_t *nodemask)
{
 return __alloc_pages_internal(gfp_mask, order, zonelist, nodemask);
}


static inline __attribute__((always_inline)) struct page *alloc_pages_node(int nid, gfp_t gfp_mask,
      unsigned int order)
{
 if (__builtin_expect(!!(order >= 11), 0))
  return ((void *)0);


 if (nid < 0)
  nid = ({ typeof(_proxy_pda.nodenumber) ret__; switch (sizeof(_proxy_pda.nodenumber)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,nodenumber)), "m" (_proxy_pda.nodenumber)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,nodenumber)), "m" (_proxy_pda.nodenumber)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,nodenumber)), "m" (_proxy_pda.nodenumber)); break; default: __bad_pda_field(); } ret__; });

 return __alloc_pages(gfp_mask, order, node_zonelist(nid, gfp_mask));
}


extern struct page *alloc_pages_current(gfp_t gfp_mask, unsigned order);

static inline __attribute__((always_inline)) struct page *
alloc_pages(gfp_t gfp_mask, unsigned int order)
{
 if (__builtin_expect(!!(order >= 11), 0))
  return ((void *)0);

 return alloc_pages_current(gfp_mask, order);
}
extern struct page *alloc_page_vma(gfp_t gfp_mask,
   struct vm_area_struct *vma, unsigned long addr);







extern unsigned long __get_free_pages(gfp_t gfp_mask, unsigned int order);
extern unsigned long get_zeroed_page(gfp_t gfp_mask);

void *alloc_pages_exact(size_t size, gfp_t gfp_mask);
void free_pages_exact(void *virt, size_t size);







extern void __free_pages(struct page *page, unsigned int order);
extern void free_pages(unsigned long addr, unsigned int order);
extern void free_hot_page(struct page *page);
extern void free_cold_page(struct page *page);




void page_alloc_init(void);
void drain_zone_pages(struct zone *zone, struct per_cpu_pages *pcp);
void drain_all_pages(void);
void drain_local_pages(void *dummy);
# 13 "include/linux/slab.h" 2
# 84 "include/linux/slab.h"
void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) kmem_cache_init(void);
int slab_is_available(void);

struct kmem_cache *kmem_cache_create(const char *, size_t, size_t,
   unsigned long,
   void (*)(void *));
void kmem_cache_destroy(struct kmem_cache *);
int kmem_cache_shrink(struct kmem_cache *);
void kmem_cache_free(struct kmem_cache *, void *);
unsigned int kmem_cache_size(struct kmem_cache *);
const char *kmem_cache_name(struct kmem_cache *);
int kmem_ptr_validate(struct kmem_cache *cachep, const void *ptr);
# 127 "include/linux/slab.h"
void * __attribute__((warn_unused_result)) __krealloc(const void *, size_t, gfp_t);
void * __attribute__((warn_unused_result)) krealloc(const void *, size_t, gfp_t);
void kfree(const void *);
void kzfree(const void *);
size_t ksize(const void *);
# 153 "include/linux/slab.h"
# 1 "include/linux/slub_def.h" 1
# 11 "include/linux/slub_def.h"
# 1 "include/linux/workqueue.h" 1







# 1 "include/linux/timer.h" 1




# 1 "include/linux/ktime.h" 1
# 46 "include/linux/ktime.h"
union ktime {
 s64 tv64;
# 57 "include/linux/ktime.h"
};

typedef union ktime ktime_t;
# 81 "include/linux/ktime.h"
static inline __attribute__((always_inline)) ktime_t ktime_set(const long secs, const unsigned long nsecs)
{

 if (__builtin_expect(!!(secs >= (((s64)~((u64)1 << 63)) / 1000000000L)), 0))
  return (ktime_t){ .tv64 = ((s64)~((u64)1 << 63)) };

 return (ktime_t) { .tv64 = (s64)secs * 1000000000L + (s64)nsecs };
}
# 113 "include/linux/ktime.h"
static inline __attribute__((always_inline)) ktime_t timespec_to_ktime(struct timespec ts)
{
 return ktime_set(ts.tv_sec, ts.tv_nsec);
}


static inline __attribute__((always_inline)) ktime_t timeval_to_ktime(struct timeval tv)
{
 return ktime_set(tv.tv_sec, tv.tv_usec * 1000L);
}
# 287 "include/linux/ktime.h"
static inline __attribute__((always_inline)) int ktime_equal(const ktime_t cmp1, const ktime_t cmp2)
{
 return cmp1.tv64 == cmp2.tv64;
}

static inline __attribute__((always_inline)) s64 ktime_to_us(const ktime_t kt)
{
 struct timeval tv = ns_to_timeval((kt).tv64);
 return (s64) tv.tv_sec * 1000000L + tv.tv_usec;
}

static inline __attribute__((always_inline)) s64 ktime_us_delta(const ktime_t later, const ktime_t earlier)
{
       return ktime_to_us(({ (ktime_t){ .tv64 = (later).tv64 - (earlier).tv64 }; }));
}

static inline __attribute__((always_inline)) ktime_t ktime_add_us(const ktime_t kt, const u64 usec)
{
 return ({ (ktime_t){ .tv64 = (kt).tv64 + (usec * 1000) }; });
}

static inline __attribute__((always_inline)) ktime_t ktime_sub_us(const ktime_t kt, const u64 usec)
{
 return ({ (ktime_t){ .tv64 = (kt).tv64 - (usec * 1000) }; });
}

extern ktime_t ktime_add_safe(const ktime_t lhs, const ktime_t rhs);
# 325 "include/linux/ktime.h"
extern void ktime_get_ts(struct timespec *ts);




static inline __attribute__((always_inline)) ktime_t ns_to_ktime(u64 ns)
{
 static const ktime_t ktime_zero = { .tv64 = 0 };
 return ({ (ktime_t){ .tv64 = (ktime_zero).tv64 + (ns) }; });
}
# 6 "include/linux/timer.h" 2

# 1 "include/linux/debugobjects.h" 1






enum debug_obj_state {
 ODEBUG_STATE_NONE,
 ODEBUG_STATE_INIT,
 ODEBUG_STATE_INACTIVE,
 ODEBUG_STATE_ACTIVE,
 ODEBUG_STATE_DESTROYED,
 ODEBUG_STATE_NOTAVAILABLE,
 ODEBUG_STATE_MAX,
};

struct debug_obj_descr;
# 26 "include/linux/debugobjects.h"
struct debug_obj {
 struct hlist_node node;
 enum debug_obj_state state;
 void *object;
 struct debug_obj_descr *descr;
};
# 45 "include/linux/debugobjects.h"
struct debug_obj_descr {
 const char *name;

 int (*fixup_init) (void *addr, enum debug_obj_state state);
 int (*fixup_activate) (void *addr, enum debug_obj_state state);
 int (*fixup_destroy) (void *addr, enum debug_obj_state state);
 int (*fixup_free) (void *addr, enum debug_obj_state state);
};


extern void debug_object_init (void *addr, struct debug_obj_descr *descr);
extern void
debug_object_init_on_stack(void *addr, struct debug_obj_descr *descr);
extern void debug_object_activate (void *addr, struct debug_obj_descr *descr);
extern void debug_object_deactivate(void *addr, struct debug_obj_descr *descr);
extern void debug_object_destroy (void *addr, struct debug_obj_descr *descr);
extern void debug_object_free (void *addr, struct debug_obj_descr *descr);

extern void debug_objects_early_init(void);
extern void debug_objects_mem_init(void);
# 84 "include/linux/debugobjects.h"
extern void debug_check_no_obj_freed(const void *address, unsigned long size);
# 8 "include/linux/timer.h" 2

struct tvec_base;

struct timer_list {
 struct list_head entry;
 unsigned long expires;

 void (*function)(unsigned long);
 unsigned long data;

 struct tvec_base *base;





};

extern struct tvec_base boot_tvec_bases;
# 40 "include/linux/timer.h"
void init_timer(struct timer_list *timer);
void init_timer_deferrable(struct timer_list *timer);


extern void init_timer_on_stack(struct timer_list *timer);
extern void destroy_timer_on_stack(struct timer_list *timer);
# 54 "include/linux/timer.h"
static inline __attribute__((always_inline)) void setup_timer(struct timer_list * timer,
    void (*function)(unsigned long),
    unsigned long data)
{
 timer->function = function;
 timer->data = data;
 init_timer(timer);
}

static inline __attribute__((always_inline)) void setup_timer_on_stack(struct timer_list *timer,
     void (*function)(unsigned long),
     unsigned long data)
{
 timer->function = function;
 timer->data = data;
 init_timer_on_stack(timer);
}
# 82 "include/linux/timer.h"
static inline __attribute__((always_inline)) int timer_pending(const struct timer_list * timer)
{
 return timer->entry.next != ((void *)0);
}

extern void add_timer_on(struct timer_list *timer, int cpu);
extern int del_timer(struct timer_list * timer);
extern int __mod_timer(struct timer_list *timer, unsigned long expires);
extern int mod_timer(struct timer_list *timer, unsigned long expires);
# 102 "include/linux/timer.h"
extern unsigned long next_timer_interrupt(void);





extern unsigned long get_next_timer_interrupt(unsigned long now);
# 136 "include/linux/timer.h"
static inline __attribute__((always_inline)) void init_timer_stats(void)
{
}

static inline __attribute__((always_inline)) void timer_stats_timer_set_start_info(struct timer_list *timer)
{
}

static inline __attribute__((always_inline)) void timer_stats_timer_clear_start_info(struct timer_list *timer)
{
}
# 163 "include/linux/timer.h"
static inline __attribute__((always_inline)) void add_timer(struct timer_list *timer)
{
 do { if (__builtin_expect(!!(timer_pending(timer)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/timer.h"), "i" (165), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 __mod_timer(timer, timer->expires);
}


  extern int try_to_del_timer_sync(struct timer_list *timer);
  extern int del_timer_sync(struct timer_list *timer);







extern void init_timers(void);
extern void run_local_timers(void);
struct hrtimer;
extern enum hrtimer_restart it_real_fn(struct hrtimer *);

unsigned long __round_jiffies(unsigned long j, int cpu);
unsigned long __round_jiffies_relative(unsigned long j, int cpu);
unsigned long round_jiffies(unsigned long j);
unsigned long round_jiffies_relative(unsigned long j);

unsigned long __round_jiffies_up(unsigned long j, int cpu);
unsigned long __round_jiffies_up_relative(unsigned long j, int cpu);
unsigned long round_jiffies_up(unsigned long j);
unsigned long round_jiffies_up_relative(unsigned long j);
# 9 "include/linux/workqueue.h" 2



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 13 "include/linux/workqueue.h" 2

struct workqueue_struct;

struct work_struct;
typedef void (*work_func_t)(struct work_struct *work);







struct work_struct {
 atomic_long_t data;



    struct list_head __attribute__((noderef, address_space(2))) entry;
 work_func_t func;

 struct lockdep_map lockdep_map;

};



struct delayed_work {
 struct work_struct work;
 struct timer_list timer;
};

struct execute_work {
 struct work_struct work;
};
# 173 "include/linux/workqueue.h"
extern struct workqueue_struct * __attribute__((noderef, address_space(2)))
__create_workqueue_key(const char *name, int singlethread,
         int freezeable, int rt, struct lock_class_key *key,
         const char *lock_name);
# 204 "include/linux/workqueue.h"
extern void destroy_workqueue(struct workqueue_struct *wq);

extern int queue_work(struct workqueue_struct * __attribute__((noderef, address_space(2))) wq,
                      struct work_struct * __attribute__((noderef, address_space(2))) work);
extern int queue_work_on(int cpu, struct workqueue_struct * __attribute__((noderef, address_space(2))) wq,
                         struct work_struct * __attribute__((noderef, address_space(2)))work);
extern int queue_delayed_work(struct workqueue_struct * __attribute__((noderef, address_space(2))) wq,
                              struct delayed_work * __attribute__((noderef, address_space(2))) work, unsigned long delay);
extern int queue_delayed_work_on(int cpu, struct workqueue_struct * __attribute__((noderef, address_space(2))) wq,
                                 struct delayed_work * __attribute__((noderef, address_space(2))) work, unsigned long delay);

extern void flush_workqueue(struct workqueue_struct *wq);
extern void flush_scheduled_work(void);

extern int schedule_work(struct work_struct *__attribute__((noderef, address_space(2))) work);
extern int schedule_work_on(int cpu, struct work_struct *__attribute__((noderef, address_space(2))) work);
extern int schedule_delayed_work(struct delayed_work *__attribute__((noderef, address_space(2))) work, unsigned long delay);
extern int schedule_delayed_work_on(int cpu,
                                    struct delayed_work *__attribute__((noderef, address_space(2))) work,
                                    unsigned long delay);
extern int schedule_on_each_cpu(work_func_t func);
extern int current_is_keventd(void);
extern int keventd_up(void);

extern void init_workqueues(void);
int execute_in_process_context(work_func_t fn, struct execute_work *);

extern int flush_work(struct work_struct *work);

extern int cancel_work_sync(struct work_struct *__attribute__((noderef, address_space(2))) work);







static inline __attribute__((always_inline)) int cancel_delayed_work(struct delayed_work *work)
{
 int ret;

 ret = del_timer_sync(&work->timer);
 if (ret)
  clear_bit(0, ((unsigned long *)(&(&work->work)->data)));
 return ret;
}

extern int cancel_delayed_work_sync(struct delayed_work *work);


static inline __attribute__((always_inline))
void cancel_rearming_delayed_workqueue(struct workqueue_struct *wq,
     struct delayed_work *work)
{
 cancel_delayed_work_sync(work);
}


static inline __attribute__((always_inline))
void cancel_rearming_delayed_work(struct delayed_work *work)
{
 cancel_delayed_work_sync(work);
}







long work_on_cpu(unsigned int cpu, long (*fn)(void *), void *arg);
# 12 "include/linux/slub_def.h" 2
# 1 "include/linux/kobject.h" 1
# 21 "include/linux/kobject.h"
# 1 "include/linux/sysfs.h" 1
# 18 "include/linux/sysfs.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 19 "include/linux/sysfs.h" 2

struct kobject;
struct module;






struct attribute {
    const char *__attribute__((nullterm)) name;
 struct module *owner;
 mode_t mode;
};

struct attribute_group {
 const char *name;
 mode_t (*is_visible)(struct kobject *,
           struct attribute *, int);
 struct attribute **attrs;
};
# 63 "include/linux/sysfs.h"
struct vm_area_struct;

struct bin_attribute {
 struct attribute attr;
 size_t size;
 void *private;
 ssize_t (*read)(struct kobject *, struct bin_attribute *,
   char *, loff_t, size_t);
 ssize_t (*write)(struct kobject *, struct bin_attribute *,
    char *, loff_t, size_t);
 int (*mmap)(struct kobject *, struct bin_attribute *attr,
      struct vm_area_struct *vma);
};

struct sysfs_ops {
 ssize_t (*show)(struct kobject *, struct attribute *,char *);
 ssize_t (*store)(struct kobject *,struct attribute *,const char *, size_t);
};

struct sysfs_dirent;



int sysfs_schedule_callback(struct kobject *kobj, void (*func)(void *),
       void *data, struct module *owner);

int __attribute__((warn_unused_result)) sysfs_create_dir(struct kobject *kobj);
void sysfs_remove_dir(struct kobject *kobj);
int __attribute__((warn_unused_result)) sysfs_rename_dir(struct kobject *kobj, const char *new_name);
int __attribute__((warn_unused_result)) sysfs_move_dir(struct kobject *kobj,
    struct kobject *new_parent_kobj);

int __attribute__((warn_unused_result)) sysfs_create_file(struct kobject *kobj,
       const struct attribute *attr);
int __attribute__((warn_unused_result)) sysfs_chmod_file(struct kobject *kobj, struct attribute *attr,
      mode_t mode);
void sysfs_remove_file(struct kobject *kobj, const struct attribute *attr);

int __attribute__((warn_unused_result)) sysfs_create_bin_file(struct kobject *kobj,
           struct bin_attribute *attr);
void sysfs_remove_bin_file(struct kobject *kobj, struct bin_attribute *attr);

int __attribute__((warn_unused_result)) sysfs_create_link(struct kobject *kobj, struct kobject *target,
       const char *name);
int __attribute__((warn_unused_result)) sysfs_create_link_nowarn(struct kobject *kobj,
       struct kobject *target,
       const char *name);
void sysfs_remove_link(struct kobject *kobj, const char *name);

int __attribute__((warn_unused_result)) sysfs_create_group(struct kobject *kobj,
        const struct attribute_group *grp);
int sysfs_update_group(struct kobject *kobj,
         const struct attribute_group *grp);
void sysfs_remove_group(struct kobject *kobj,
   const struct attribute_group *grp);
int sysfs_add_file_to_group(struct kobject *kobj,
   const struct attribute *attr, const char *group);
void sysfs_remove_file_from_group(struct kobject *kobj,
   const struct attribute *attr, const char *group);

void sysfs_notify(struct kobject *kobj, const char *dir, const char *attr);
void sysfs_notify_dirent(struct sysfs_dirent *sd);
struct sysfs_dirent *sysfs_get_dirent(struct sysfs_dirent *parent_sd,
          const unsigned char *name);
struct sysfs_dirent *sysfs_get(struct sysfs_dirent *sd);
void sysfs_put(struct sysfs_dirent *sd);
void sysfs_printk_last_file(void);
int __attribute__((warn_unused_result)) sysfs_init(void);
# 22 "include/linux/kobject.h" 2





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 28 "include/linux/kobject.h" 2






extern char uevent_helper[];


extern u64 uevent_seqnum;
# 49 "include/linux/kobject.h"
enum kobject_action {
 KOBJ_ADD,
 KOBJ_REMOVE,
 KOBJ_CHANGE,
 KOBJ_MOVE,
 KOBJ_ONLINE,
 KOBJ_OFFLINE,
 KOBJ_MAX
};

struct kobject {
 const char *name;
 struct list_head entry;
 struct kobject *parent;
 struct kset *kset;
 struct kobj_type *ktype;
 struct sysfs_dirent *sd;
 struct kref kref;
 unsigned int state_initialized:1;
 unsigned int state_in_sysfs:1;
 unsigned int state_add_uevent_sent:1;
 unsigned int state_remove_uevent_sent:1;
};

extern int kobject_set_name(struct kobject *kobj, const char *name, ...)
       __attribute__((format(printf, 2, 3)));

static inline __attribute__((always_inline)) const char *kobject_name(const struct kobject *kobj)
{
 return kobj->name;
}

extern void kobject_init(struct kobject *kobj, struct kobj_type *ktype);
extern int __attribute__((warn_unused_result)) kobject_add(struct kobject *kobj,
        struct kobject *parent,
        const char *fmt, ...);
extern int __attribute__((warn_unused_result)) kobject_init_and_add(struct kobject *kobj,
          struct kobj_type *ktype,
          struct kobject *parent,
          const char *fmt, ...);

extern void kobject_del(struct kobject *kobj);

extern struct kobject * __attribute__((warn_unused_result)) kobject_create(void);
extern struct kobject * __attribute__((warn_unused_result)) kobject_create_and_add(const char *name,
      struct kobject *parent);

extern int __attribute__((warn_unused_result)) kobject_rename(struct kobject *, const char *new_name);
extern int __attribute__((warn_unused_result)) kobject_move(struct kobject *, struct kobject *);

extern struct kobject *kobject_get(struct kobject *kobj);
extern void kobject_put(struct kobject *kobj);

extern char *kobject_get_path(struct kobject *kobj, gfp_t flag);

struct kobj_type {
 void (*release)(struct kobject *kobj);
 struct sysfs_ops *sysfs_ops;
 struct attribute **default_attrs;
};

struct kobj_uevent_env {
 char *envp[32];
 int envp_idx;
 char buf[2048];
 int buflen;
};

struct kset_uevent_ops {
 int (*filter)(struct kset *kset, struct kobject *kobj);
 const char *(*name)(struct kset *kset, struct kobject *kobj);
 int (*uevent)(struct kset *kset, struct kobject *kobj,
        struct kobj_uevent_env *env);
};

struct kobj_attribute {
 struct attribute attr;
 ssize_t (*show)(struct kobject *kobj, struct kobj_attribute *attr,
   char *buf);
 ssize_t (*store)(struct kobject *kobj, struct kobj_attribute *attr,
    const char *buf, size_t count);
};

extern struct sysfs_ops kobj_sysfs_ops;
# 151 "include/linux/kobject.h"
struct kset {
 struct list_head list;
 spinlock_t list_lock;
 struct kobject kobj;
 struct kset_uevent_ops *uevent_ops;
};

extern void kset_init(struct kset *kset);
extern int __attribute__((warn_unused_result)) kset_register(struct kset *kset);
extern void kset_unregister(struct kset *kset);
extern struct kset * __attribute__((warn_unused_result)) kset_create_and_add(const char *name,
      struct kset_uevent_ops *u,
      struct kobject *parent_kobj);

static inline __attribute__((always_inline)) struct kset *to_kset(struct kobject *kobj)
{
 return kobj ? ({ const typeof( ((struct kset *)0)->kobj ) *__mptr = (kobj); (struct kset *)( (char *)__mptr - __builtin_offsetof(struct kset,kobj) );}) : ((void *)0);
}

static inline __attribute__((always_inline)) struct kset *kset_get(struct kset *k)
{
 return k ? to_kset(kobject_get(&k->kobj)) : ((void *)0);
}

static inline __attribute__((always_inline)) void kset_put(struct kset *k)
{
 kobject_put(&k->kobj);
}

static inline __attribute__((always_inline)) struct kobj_type *get_ktype(struct kobject *kobj)
{
 return kobj->ktype;
}

extern struct kobject *kset_find_obj(struct kset *, const char *);


extern struct kobject *kernel_kobj;

extern struct kobject *mm_kobj;

extern struct kobject *hypervisor_kobj;

extern struct kobject *power_kobj;

extern struct kobject *firmware_kobj;


int kobject_uevent(struct kobject *kobj, enum kobject_action action);
int kobject_uevent_env(struct kobject *kobj, enum kobject_action action,
   char *envp[]);

int add_uevent_var(struct kobj_uevent_env *env, const char *format, ...)
 __attribute__((format (printf, 2, 3)));

int kobject_action_type(const char *buf, size_t count,
   enum kobject_action *type);
# 13 "include/linux/slub_def.h" 2

enum stat_item {
 ALLOC_FASTPATH,
 ALLOC_SLOWPATH,
 FREE_FASTPATH,
 FREE_SLOWPATH,
 FREE_FROZEN,
 FREE_ADD_PARTIAL,
 FREE_REMOVE_PARTIAL,
 ALLOC_FROM_PARTIAL,
 ALLOC_SLAB,
 ALLOC_REFILL,
 FREE_SLAB,
 CPUSLAB_FLUSH,
 DEACTIVATE_FULL,
 DEACTIVATE_EMPTY,
 DEACTIVATE_TO_HEAD,
 DEACTIVATE_TO_TAIL,
 DEACTIVATE_REMOTE_FREES,
 ORDER_FALLBACK,
 NR_SLUB_STAT_ITEMS };

struct kmem_cache_cpu {
 void **freelist;
 struct page *page;
 int node;
 unsigned int offset;
 unsigned int objsize;



};

struct kmem_cache_node {
 spinlock_t list_lock;
 unsigned long nr_partial;
 unsigned long min_partial;
 struct list_head partial;

 atomic_long_t nr_slabs;
 atomic_long_t total_objects;
 struct list_head full;

};






struct kmem_cache_order_objects {
 unsigned long x;
};




struct kmem_cache {

 unsigned long flags;
 int size;
 int objsize;
 int offset;
 struct kmem_cache_order_objects oo;





 struct kmem_cache_node local_node;


 struct kmem_cache_order_objects max;
 struct kmem_cache_order_objects min;
 gfp_t allocflags;
 int refcount;
 void (*ctor)(void *);
 int inuse;
 int align;
 const char *name;
 struct list_head list;

 struct kobject kobj;






 int remote_node_defrag_ratio;
 struct kmem_cache_node *node[(1 << 6)];


 struct kmem_cache_cpu *cpu_slab[255];



};
# 127 "include/linux/slub_def.h"
extern struct kmem_cache kmalloc_caches[12 + 1];





static inline __attribute__((always_inline)) __attribute__((always_inline)) int kmalloc_index(size_t size)
{
 if (!size)
  return 0;

 if (size <= 8)
  return ( __builtin_constant_p(8) ? ( (8) < 1 ? ____ilog2_NaN() : (8) & (1ULL << 63) ? 63 : (8) & (1ULL << 62) ? 62 : (8) & (1ULL << 61) ? 61 : (8) & (1ULL << 60) ? 60 : (8) & (1ULL << 59) ? 59 : (8) & (1ULL << 58) ? 58 : (8) & (1ULL << 57) ? 57 : (8) & (1ULL << 56) ? 56 : (8) & (1ULL << 55) ? 55 : (8) & (1ULL << 54) ? 54 : (8) & (1ULL << 53) ? 53 : (8) & (1ULL << 52) ? 52 : (8) & (1ULL << 51) ? 51 : (8) & (1ULL << 50) ? 50 : (8) & (1ULL << 49) ? 49 : (8) & (1ULL << 48) ? 48 : (8) & (1ULL << 47) ? 47 : (8) & (1ULL << 46) ? 46 : (8) & (1ULL << 45) ? 45 : (8) & (1ULL << 44) ? 44 : (8) & (1ULL << 43) ? 43 : (8) & (1ULL << 42) ? 42 : (8) & (1ULL << 41) ? 41 : (8) & (1ULL << 40) ? 40 : (8) & (1ULL << 39) ? 39 : (8) & (1ULL << 38) ? 38 : (8) & (1ULL << 37) ? 37 : (8) & (1ULL << 36) ? 36 : (8) & (1ULL << 35) ? 35 : (8) & (1ULL << 34) ? 34 : (8) & (1ULL << 33) ? 33 : (8) & (1ULL << 32) ? 32 : (8) & (1ULL << 31) ? 31 : (8) & (1ULL << 30) ? 30 : (8) & (1ULL << 29) ? 29 : (8) & (1ULL << 28) ? 28 : (8) & (1ULL << 27) ? 27 : (8) & (1ULL << 26) ? 26 : (8) & (1ULL << 25) ? 25 : (8) & (1ULL << 24) ? 24 : (8) & (1ULL << 23) ? 23 : (8) & (1ULL << 22) ? 22 : (8) & (1ULL << 21) ? 21 : (8) & (1ULL << 20) ? 20 : (8) & (1ULL << 19) ? 19 : (8) & (1ULL << 18) ? 18 : (8) & (1ULL << 17) ? 17 : (8) & (1ULL << 16) ? 16 : (8) & (1ULL << 15) ? 15 : (8) & (1ULL << 14) ? 14 : (8) & (1ULL << 13) ? 13 : (8) & (1ULL << 12) ? 12 : (8) & (1ULL << 11) ? 11 : (8) & (1ULL << 10) ? 10 : (8) & (1ULL << 9) ? 9 : (8) & (1ULL << 8) ? 8 : (8) & (1ULL << 7) ? 7 : (8) & (1ULL << 6) ? 6 : (8) & (1ULL << 5) ? 5 : (8) & (1ULL << 4) ? 4 : (8) & (1ULL << 3) ? 3 : (8) & (1ULL << 2) ? 2 : (8) & (1ULL << 1) ? 1 : (8) & (1ULL << 0) ? 0 : ____ilog2_NaN() ) : (sizeof(8) <= 4) ? __ilog2_u32(8) : __ilog2_u64(8) );


 if (size > 64 && size <= 96)
  return 1;
 if (size > 128 && size <= 192)
  return 2;

 if (size <= 8) return 3;
 if (size <= 16) return 4;
 if (size <= 32) return 5;
 if (size <= 64) return 6;
 if (size <= 128) return 7;
 if (size <= 256) return 8;
 if (size <= 512) return 9;
 if (size <= 1024) return 10;
 if (size <= 2 * 1024) return 11;
 if (size <= 4 * 1024) return 12;




 if (size <= 8 * 1024) return 13;
 if (size <= 16 * 1024) return 14;
 if (size <= 32 * 1024) return 15;
 if (size <= 64 * 1024) return 16;
 if (size <= 128 * 1024) return 17;
 if (size <= 256 * 1024) return 18;
 if (size <= 512 * 1024) return 19;
 if (size <= 1024 * 1024) return 20;
 if (size <= 2 * 1024 * 1024) return 21;
 return -1;
# 179 "include/linux/slub_def.h"
}







static inline __attribute__((always_inline)) __attribute__((always_inline)) struct kmem_cache *kmalloc_slab(size_t size)
{
 int index = kmalloc_index(size);

 if (index == 0)
  return ((void *)0);

 return &kmalloc_caches[index];
}
# 204 "include/linux/slub_def.h"
void *kmem_cache_alloc(struct kmem_cache *, gfp_t);
void *__kmalloc(size_t size, gfp_t flags);

static inline __attribute__((always_inline)) __attribute__((always_inline)) void *kmalloc_large(size_t size, gfp_t flags)
{
 return (void *)__get_free_pages(flags | (( gfp_t)0x4000u), get_order(size));
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) void *kmalloc(size_t size, gfp_t flags)
{
 if (__builtin_constant_p(size)) {
  if (size > ((1UL) << 12))
   return kmalloc_large(size, flags);

  if (!(flags & (( gfp_t)0x01u))) {
   struct kmem_cache *s = kmalloc_slab(size);

   if (!s)
    return ((void *)16);

   return kmem_cache_alloc(s, flags);
  }
 }
 return __kmalloc(size, flags);
}


void *__kmalloc_node(size_t size, gfp_t flags, int node);
void *kmem_cache_alloc_node(struct kmem_cache *, gfp_t flags, int node);

static inline __attribute__((always_inline)) __attribute__((always_inline)) void *kmalloc_node(size_t size, gfp_t flags, int node)
{
 if (__builtin_constant_p(size) &&
  size <= ((1UL) << 12) && !(flags & (( gfp_t)0x01u))) {
   struct kmem_cache *s = kmalloc_slab(size);

  if (!s)
   return ((void *)16);

  return kmem_cache_alloc_node(s, flags, node);
 }
 return __kmalloc_node(size, flags, node);
}
# 154 "include/linux/slab.h" 2
# 211 "include/linux/slab.h"
static inline __attribute__((always_inline)) void *kcalloc(size_t n, size_t size, gfp_t flags)
{
 if (size != 0 && n > (~0UL) / size)
  return ((void *)0);
 return __kmalloc(n * size, flags | (( gfp_t)0x8000u));
}
# 257 "include/linux/slab.h"
extern void *__kmalloc_track_caller(size_t, gfp_t, unsigned long);
# 275 "include/linux/slab.h"
extern void *__kmalloc_node_track_caller(size_t, gfp_t, int, unsigned long);
# 294 "include/linux/slab.h"
static inline __attribute__((always_inline)) void *kmem_cache_zalloc(struct kmem_cache *k, gfp_t flags)
{
 return kmem_cache_alloc(k, flags | (( gfp_t)0x8000u));
}






static inline __attribute__((always_inline)) void *kzalloc(size_t size, gfp_t flags)
{
 return kmalloc(size, flags | (( gfp_t)0x8000u));
}







static inline __attribute__((always_inline)) void *kzalloc_node(size_t size, gfp_t flags, int node)
{
 return kmalloc_node(size, flags | (( gfp_t)0x8000u), node);
}
# 6 "include/linux/percpu.h" 2
# 68 "include/linux/percpu.h"
struct percpu_data {
 void *ptrs[1];
};
# 84 "include/linux/percpu.h"
extern void *__percpu_alloc_mask(size_t size, gfp_t gfp, cpumask_t *mask);
extern void percpu_free(void *__pdata);
# 40 "include/linux/rcupdate.h" 2
# 50 "include/linux/rcupdate.h"
struct rcu_head {
 struct rcu_head *next;
 void (*func)(struct rcu_head *head);
};


extern int rcu_scheduler_active;


# 1 "include/linux/rcuclassic.h" 1
# 49 "include/linux/rcuclassic.h"
struct rcu_ctrlblk {
 long cur;
 long completed;
 long pending;






 int signaled;

 spinlock_t lock __attribute__((__aligned__(1 << ((7)))));
 unsigned long cpumask[(((255) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];

} __attribute__((__aligned__(1 << ((7)))));


static inline __attribute__((always_inline)) int rcu_batch_before(long a, long b)
{
 return (a - b) < 0;
}


static inline __attribute__((always_inline)) int rcu_batch_after(long a, long b)
{
 return (a - b) > 0;
}


struct rcu_data {

 long quiescbatch;
 int passed_quiesc;
 int qs_pending;
# 100 "include/linux/rcuclassic.h"
 long batch;
 struct rcu_head *nxtlist;
 struct rcu_head **nxttail[3];
 long qlen;
 struct rcu_head *donelist;
 struct rcu_head **donetail;
 long blimit;
 int cpu;
 struct rcu_head barrier;
};

extern __typeof__(struct rcu_data) per_cpu__rcu_data;
extern __typeof__(struct rcu_data) per_cpu__rcu_bh_data;







static inline __attribute__((always_inline)) void rcu_qsctr_inc(int cpu)
{
 struct rcu_data *rdp = &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__rcu_data))); (typeof((&per_cpu__rcu_data))) (__ptr + (((((_cpu_pda[cpu])->data_offset))))); }));
 rdp->passed_quiesc = 1;
}
static inline __attribute__((always_inline)) void rcu_bh_qsctr_inc(int cpu)
{
 struct rcu_data *rdp = &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__rcu_bh_data))); (typeof((&per_cpu__rcu_bh_data))) (__ptr + (((((_cpu_pda[cpu])->data_offset))))); }));
 rdp->passed_quiesc = 1;
}

extern int rcu_pending(int cpu);
extern int rcu_needs_cpu(int cpu);


extern struct lockdep_map rcu_lock_map;
# 173 "include/linux/rcuclassic.h"
extern void __rcu_init(void);

extern void rcu_check_callbacks(int cpu, int user);
extern void rcu_restart_cpu(int cpu);

extern long rcu_batches_completed(void);
extern long rcu_batches_completed_bh(void);





static inline __attribute__((always_inline)) int rcu_blocking_is_gp(void)
{
 return cpumask_weight(cpu_online_mask) == 1;
}
# 60 "include/linux/rcupdate.h" 2
# 203 "include/linux/rcupdate.h"
struct rcu_synchronize {
 struct rcu_head head;
 struct completion completion;
};

extern void wakeme_after_rcu(struct rcu_head *head);
# 239 "include/linux/rcupdate.h"
extern void call_rcu(struct rcu_head *head,
         void (*func)(struct rcu_head *head));
# 260 "include/linux/rcupdate.h"
extern void call_rcu_bh(struct rcu_head *head,
   void (*func)(struct rcu_head *head));


extern void synchronize_rcu(void);
extern void rcu_barrier(void);
extern void rcu_barrier_bh(void);
extern void rcu_barrier_sched(void);


extern void rcu_init(void);
extern void rcu_scheduler_starting(void);
extern int rcu_needs_cpu(int cpu);
# 82 "include/linux/sem.h" 2

struct task_struct;


struct sem {
 int semval;
 int sempid;
};


struct sem_array {
 struct kern_ipc_perm sem_perm;
 time_t sem_otime;
 time_t sem_ctime;
 struct sem *sem_base;
 struct list_head sem_pending;
 struct list_head list_id;
 unsigned long sem_nsems;
};


struct sem_queue {
 struct list_head list;
 struct task_struct *sleeper;
 struct sem_undo *undo;
 int pid;
 int status;
 struct sembuf *sops;
 int nsops;
 int alter;
};




struct sem_undo {
 struct list_head list_proc;

 struct rcu_head rcu;
 struct sem_undo_list *ulp;
 struct list_head list_id;
 int semid;
 short * semadj;
};




struct sem_undo_list {
 atomic_t refcnt;
 spinlock_t lock;
 struct list_head list_proc;
};

struct sysv_sem {
 struct sem_undo_list *undo_list;
};



extern int copy_semundo(unsigned long clone_flags, struct task_struct *tsk);
extern void exit_sem(struct task_struct *tsk);
# 70 "include/linux/sched.h" 2
# 1 "include/linux/signal.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/signal.h" 1
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/signal.h"
struct siginfo;
# 28 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/signal.h"
typedef unsigned long old_sigset_t;

typedef struct {
 unsigned long sig[(64 / 64)];
} sigset_t;
# 120 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/signal.h"
# 1 "include/asm-generic/signal.h" 1
# 17 "include/asm-generic/signal.h"
typedef void __signalfn_t(int);
typedef __signalfn_t __attribute__((noderef, address_space(1))) *__sighandler_t;

typedef void __restorefn_t(void);
typedef __restorefn_t __attribute__((noderef, address_space(1))) *__sigrestore_t;
# 121 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/signal.h" 2




extern void do_notify_resume(struct pt_regs *, void *, __u32);
# 167 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/signal.h"
struct sigaction {
 __sighandler_t sa_handler;
 unsigned long sa_flags;
 __sigrestore_t sa_restorer;
 sigset_t sa_mask;
};

struct k_sigaction {
 struct sigaction sa;
};



typedef struct sigaltstack {
 void __attribute__((noderef, address_space(1))) *ss_sp;
 int ss_flags;
 size_t ss_size;
} stack_t;
# 5 "include/linux/signal.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/siginfo.h" 1







# 1 "include/asm-generic/siginfo.h" 1






typedef union sigval {
 int sival_int;
 void __attribute__((noderef, address_space(1))) *sival_ptr;
} sigval_t;
# 40 "include/asm-generic/siginfo.h"
typedef struct siginfo {
 int si_signo;
 int si_errno;
 int si_code;

 union {
  int _pad[((128 - (4 * sizeof(int))) / sizeof(int))];


  struct {
   pid_t _pid;
   uid_t _uid;
  } _kill;


  struct {
   timer_t _tid;
   int _overrun;
   char _pad[sizeof( uid_t) - sizeof(int)];
   sigval_t _sigval;
   int _sys_private;
  } _timer;


  struct {
   pid_t _pid;
   uid_t _uid;
   sigval_t _sigval;
  } _rt;


  struct {
   pid_t _pid;
   uid_t _uid;
   int _status;
   clock_t _utime;
   clock_t _stime;
  } _sigchld;


  struct {
   void __attribute__((noderef, address_space(1))) *_addr;



  } _sigfault;


  struct {
   long _band;
   int _fd;
  } _sigpoll;
 } _sifields;
} siginfo_t;
# 253 "include/asm-generic/siginfo.h"
typedef struct sigevent {
 sigval_t sigev_value;
 int sigev_signo;
 int sigev_notify;
 union {
  int _pad[((64 - (sizeof(int) * 2 + sizeof(sigval_t))) / sizeof(int))];
   int _tid;

  struct {
   void (*_function)(sigval_t);
   void *_attribute;
  } _sigev_thread;
 } _sigev_un;
} sigevent_t;







struct siginfo;
void do_schedule_next_timer(struct siginfo *info);





static inline __attribute__((always_inline)) void copy_siginfo(struct siginfo *to, struct siginfo *from)
{
 if (from->si_code < 0)
  memcpy(to, from, sizeof(*to));
 else

  memcpy(to, from, (4 * sizeof(int)) + sizeof(from->_sifields._sigchld));
}



extern int copy_siginfo_to_user(struct siginfo __attribute__((noderef, address_space(1))) *to, struct siginfo *from);
# 9 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/siginfo.h" 2
# 6 "include/linux/signal.h" 2
# 14 "include/linux/signal.h"
struct sigqueue {
 struct list_head list;
 int flags;
 siginfo_t info;
 struct user_struct *user;
};




struct sigpending {
 struct list_head list;
 sigset_t signal;
};
# 38 "include/linux/signal.h"
static inline __attribute__((always_inline)) void sigaddset(sigset_t *set, int _sig)
{
 unsigned long sig = _sig - 1;
 if ((64 / 64) == 1)
  set->sig[0] |= 1UL << sig;
 else
  set->sig[sig / 64] |= 1UL << (sig % 64);
}

static inline __attribute__((always_inline)) void sigdelset(sigset_t *set, int _sig)
{
 unsigned long sig = _sig - 1;
 if ((64 / 64) == 1)
  set->sig[0] &= ~(1UL << sig);
 else
  set->sig[sig / 64] &= ~(1UL << (sig % 64));
}

static inline __attribute__((always_inline)) int sigismember(sigset_t *set, int _sig)
{
 unsigned long sig = _sig - 1;
 if ((64 / 64) == 1)
  return 1 & (set->sig[0] >> sig);
 else
  return 1 & (set->sig[sig / 64] >> (sig % 64));
}

static inline __attribute__((always_inline)) int sigfindinword(unsigned long word)
{
 return ffz(~word);
}



static inline __attribute__((always_inline)) int sigisemptyset(sigset_t *set)
{
 extern void _NSIG_WORDS_is_unsupported_size(void);
 switch ((64 / 64)) {
 case 4:
  return (set->sig[3] | set->sig[2] |
   set->sig[1] | set->sig[0]) == 0;
 case 2:
  return (set->sig[1] | set->sig[0]) == 0;
 case 1:
  return set->sig[0] == 0;
 default:
  _NSIG_WORDS_is_unsupported_size();
  return 0;
 }
}
# 119 "include/linux/signal.h"
static inline __attribute__((always_inline)) void sigorsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 64)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) | (b3)); r->sig[2] = ((a2) | (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) | (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) | (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }


static inline __attribute__((always_inline)) void sigandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 64)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & (b3)); r->sig[2] = ((a2) & (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }


static inline __attribute__((always_inline)) void signandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 64)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & ~(b3)); r->sig[2] = ((a2) & ~(b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & ~(b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & ~(b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }
# 149 "include/linux/signal.h"
static inline __attribute__((always_inline)) void signotset(sigset_t *set) { extern void _NSIG_WORDS_is_unsupported_size(void); switch ((64 / 64)) { case 4: set->sig[3] = (~(set->sig[3])); set->sig[2] = (~(set->sig[2])); case 2: set->sig[1] = (~(set->sig[1])); case 1: set->sig[0] = (~(set->sig[0])); break; default: _NSIG_WORDS_is_unsupported_size(); } }




static inline __attribute__((always_inline)) void sigemptyset(sigset_t *set)
{
 switch ((64 / 64)) {
 default:
  memset(set, 0, sizeof(sigset_t));
  break;
 case 2: set->sig[1] = 0;
 case 1: set->sig[0] = 0;
  break;
 }
}

static inline __attribute__((always_inline)) void sigfillset(sigset_t *set)
{
 switch ((64 / 64)) {
 default:
  memset(set, -1, sizeof(sigset_t));
  break;
 case 2: set->sig[1] = -1;
 case 1: set->sig[0] = -1;
  break;
 }
}



static inline __attribute__((always_inline)) void sigaddsetmask(sigset_t *set, unsigned long mask)
{
 set->sig[0] |= mask;
}

static inline __attribute__((always_inline)) void sigdelsetmask(sigset_t *set, unsigned long mask)
{
 set->sig[0] &= ~mask;
}

static inline __attribute__((always_inline)) int sigtestsetmask(sigset_t *set, unsigned long mask)
{
 return (set->sig[0] & mask) != 0;
}

static inline __attribute__((always_inline)) void siginitset(sigset_t *set, unsigned long mask)
{
 set->sig[0] = mask;
 switch ((64 / 64)) {
 default:
  memset(&set->sig[1], 0, sizeof(long)*((64 / 64)-1));
  break;
 case 2: set->sig[1] = 0;
 case 1: ;
 }
}

static inline __attribute__((always_inline)) void siginitsetinv(sigset_t *set, unsigned long mask)
{
 set->sig[0] = ~mask;
 switch ((64 / 64)) {
 default:
  memset(&set->sig[1], -1, sizeof(long)*((64 / 64)-1));
  break;
 case 2: set->sig[1] = -1;
 case 1: ;
 }
}



static inline __attribute__((always_inline)) void init_sigpending(struct sigpending *sig)
{
 sigemptyset(&sig->signal);
 INIT_LIST_HEAD(&sig->list);
}

extern void flush_sigqueue(struct sigpending *queue);


static inline __attribute__((always_inline)) int valid_signal(unsigned long sig)
{
 return sig <= 64 ? 1 : 0;
}

extern int next_signal(struct sigpending *pending, sigset_t *mask);
extern int group_send_sig_info(int sig, struct siginfo *info, struct task_struct *p);
extern int __group_send_sig_info(int, struct siginfo *, struct task_struct *);
extern long do_sigpending(void __attribute__((noderef, address_space(1))) *, unsigned long);
extern int sigprocmask(int, sigset_t *, sigset_t *);
extern int show_unhandled_signals;

struct pt_regs;
extern int get_signal_to_deliver(siginfo_t *info, struct k_sigaction *return_ka, struct pt_regs *regs, void *cookie);
extern void exit_signals(struct task_struct *tsk);

extern struct kmem_cache *sighand_cachep;

int unhandled_signal(struct task_struct *tsk, int sig);
# 373 "include/linux/signal.h"
void signals_init(void);
# 71 "include/linux/sched.h" 2
# 1 "include/linux/fs_struct.h" 1



# 1 "include/linux/path.h" 1



struct dentry;
struct vfsmount;

struct path {
 struct vfsmount *mnt;
 struct dentry *dentry;
};

extern void path_get(struct path *);
extern void path_put(struct path *);
# 5 "include/linux/fs_struct.h" 2

struct fs_struct {
 atomic_t count;
 rwlock_t lock;
 int umask;
 struct path root, pwd;
};

extern struct kmem_cache *fs_cachep;

extern void exit_fs(struct task_struct *);
extern void set_fs_root(struct fs_struct *, struct path *);
extern void set_fs_pwd(struct fs_struct *, struct path *);
extern struct fs_struct *copy_fs_struct(struct fs_struct *);
extern void put_fs_struct(struct fs_struct *);
# 72 "include/linux/sched.h" 2


# 1 "include/linux/pid.h" 1





enum pid_type
{
 PIDTYPE_PID,
 PIDTYPE_PGID,
 PIDTYPE_SID,
 PIDTYPE_MAX
};
# 50 "include/linux/pid.h"
struct upid {

 int nr;
 struct pid_namespace *ns;
 struct hlist_node pid_chain;
};

struct pid
{
 atomic_t count;
 unsigned int level;

 struct hlist_head tasks[PIDTYPE_MAX];
 struct rcu_head rcu;
 struct upid numbers[1];
};

extern struct pid init_struct_pid;

struct pid_link
{
 struct hlist_node node;
 struct pid *pid;
};

static inline __attribute__((always_inline)) struct pid *get_pid(struct pid *pid)
{
 if (pid)
  atomic_inc(&pid->count);
 return pid;
}

extern void put_pid(struct pid *pid);
extern struct task_struct *pid_task(struct pid *pid, enum pid_type);
extern struct task_struct *get_pid_task(struct pid *pid, enum pid_type);

extern struct pid *get_task_pid(struct task_struct *task, enum pid_type type);





extern void attach_pid(struct task_struct *task, enum pid_type type,
   struct pid *pid);
extern void detach_pid(struct task_struct *task, enum pid_type);
extern void change_pid(struct task_struct *task, enum pid_type,
   struct pid *pid);
extern void transfer_pid(struct task_struct *old, struct task_struct *new,
    enum pid_type);

struct pid_namespace;
extern struct pid_namespace init_pid_ns;
# 112 "include/linux/pid.h"
extern struct pid *find_pid_ns(int nr, struct pid_namespace *ns);
extern struct pid *find_vpid(int nr);




extern struct pid *find_get_pid(int nr);
extern struct pid *find_ge_pid(int nr, struct pid_namespace *);
int next_pidmap(struct pid_namespace *pid_ns, int last);

extern struct pid *alloc_pid(struct pid_namespace *ns);
extern void free_pid(struct pid *pid);
# 135 "include/linux/pid.h"
static inline __attribute__((always_inline)) struct pid_namespace *ns_of_pid(struct pid *pid)
{
 struct pid_namespace *ns = ((void *)0);
 if (pid)
  ns = pid->numbers[pid->level].ns;
 return ns;
}
# 154 "include/linux/pid.h"
static inline __attribute__((always_inline)) pid_t pid_nr(struct pid *pid)
{
 pid_t nr = 0;
 if (pid)
  nr = pid->numbers[0].nr;
 return nr;
}

pid_t pid_nr_ns(struct pid *pid, struct pid_namespace *ns);
pid_t pid_vnr(struct pid *pid);
# 75 "include/linux/sched.h" 2


# 1 "include/linux/proportions.h" 1
# 12 "include/linux/proportions.h"
# 1 "include/linux/percpu_counter.h" 1
# 18 "include/linux/percpu_counter.h"
struct percpu_counter {
 spinlock_t lock;
 s64 count;

 struct list_head list;

 s32 *counters;
};

extern int percpu_counter_batch;

int __percpu_counter_init(struct percpu_counter *fbc, s64 amount,
     struct lock_class_key *key);
# 39 "include/linux/percpu_counter.h"
void percpu_counter_destroy(struct percpu_counter *fbc);
void percpu_counter_set(struct percpu_counter *fbc, s64 amount);
void __percpu_counter_add(struct percpu_counter *fbc, s64 amount, s32 batch);
s64 __percpu_counter_sum(struct percpu_counter *fbc);

static inline __attribute__((always_inline)) void percpu_counter_add(struct percpu_counter *fbc, s64 amount)
{
 __percpu_counter_add(fbc, amount, percpu_counter_batch);
}

static inline __attribute__((always_inline)) s64 percpu_counter_sum_positive(struct percpu_counter *fbc)
{
 s64 ret = __percpu_counter_sum(fbc);
 return ret < 0 ? 0 : ret;
}

static inline __attribute__((always_inline)) s64 percpu_counter_sum(struct percpu_counter *fbc)
{
 return __percpu_counter_sum(fbc);
}

static inline __attribute__((always_inline)) s64 percpu_counter_read(struct percpu_counter *fbc)
{
 return fbc->count;
}






static inline __attribute__((always_inline)) s64 percpu_counter_read_positive(struct percpu_counter *fbc)
{
 s64 ret = fbc->count;

 __asm__ __volatile__("": : :"memory");
 if (ret >= 0)
  return ret;
 return 1;
}
# 134 "include/linux/percpu_counter.h"
static inline __attribute__((always_inline)) void percpu_counter_inc(struct percpu_counter *fbc)
{
 percpu_counter_add(fbc, 1);
}

static inline __attribute__((always_inline)) void percpu_counter_dec(struct percpu_counter *fbc)
{
 percpu_counter_add(fbc, -1);
}

static inline __attribute__((always_inline)) void percpu_counter_sub(struct percpu_counter *fbc, s64 amount)
{
 percpu_counter_add(fbc, -amount);
}
# 13 "include/linux/proportions.h" 2



struct prop_global {





 int shift;






 struct percpu_counter events;
};






struct prop_descriptor {
 int index;
 struct prop_global pg[2];
 struct mutex mutex;
};

int prop_descriptor_init(struct prop_descriptor *pd, int shift);
void prop_change_shift(struct prop_descriptor *pd, int new_shift);





struct prop_local_percpu {



 struct percpu_counter events;




 int shift;
 unsigned long period;
 spinlock_t lock;
};

int prop_local_init_percpu(struct prop_local_percpu *pl);
void prop_local_destroy_percpu(struct prop_local_percpu *pl);
void __prop_inc_percpu(struct prop_descriptor *pd, struct prop_local_percpu *pl);
void prop_fraction_percpu(struct prop_descriptor *pd, struct prop_local_percpu *pl,
  long *numerator, long *denominator);

static inline __attribute__((always_inline))
void prop_inc_percpu(struct prop_descriptor *pd, struct prop_local_percpu *pl)
{
 unsigned long flags;

 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); do { (flags) = __raw_local_irq_save(); } while (0); do { } while (0); } while (0);
 __prop_inc_percpu(pd, pl);
 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); if (raw_irqs_disabled_flags(flags)) { raw_local_irq_restore(flags); do { } while (0); } else { do { } while (0); raw_local_irq_restore(flags); } } while (0);
}
# 89 "include/linux/proportions.h"
void __prop_inc_percpu_max(struct prop_descriptor *pd,
      struct prop_local_percpu *pl, long frac);






struct prop_local_single {



 unsigned long events;





 unsigned long period;
 int shift;
 spinlock_t lock;
};





int prop_local_init_single(struct prop_local_single *pl);
void prop_local_destroy_single(struct prop_local_single *pl);
void __prop_inc_single(struct prop_descriptor *pd, struct prop_local_single *pl);
void prop_fraction_single(struct prop_descriptor *pd, struct prop_local_single *pl,
  long *numerator, long *denominator);

static inline __attribute__((always_inline))
void prop_inc_single(struct prop_descriptor *pd, struct prop_local_single *pl)
{
 unsigned long flags;

 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); do { (flags) = __raw_local_irq_save(); } while (0); do { } while (0); } while (0);
 __prop_inc_single(pd, pl);
 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); if (raw_irqs_disabled_flags(flags)) { raw_local_irq_restore(flags); do { } while (0); } else { do { } while (0); raw_local_irq_restore(flags); } } while (0);
}
# 78 "include/linux/sched.h" 2
# 1 "include/linux/seccomp.h" 1
# 24 "include/linux/seccomp.h"
typedef struct { } seccomp_t;



static inline __attribute__((always_inline)) long prctl_get_seccomp(void)
{
 return -22;
}

static inline __attribute__((always_inline)) long prctl_set_seccomp(unsigned long arg2)
{
 return -22;
}
# 79 "include/linux/sched.h" 2

# 1 "include/linux/rtmutex.h" 1
# 16 "include/linux/rtmutex.h"
# 1 "include/linux/plist.h" 1
# 80 "include/linux/plist.h"
struct plist_head {
 struct list_head prio_list;
 struct list_head node_list;

 spinlock_t *lock;

};

struct plist_node {
 int prio;
 struct plist_head plist;
};
# 127 "include/linux/plist.h"
static inline __attribute__((always_inline)) void
plist_head_init(struct plist_head *head, spinlock_t *lock)
{
 INIT_LIST_HEAD(&head->prio_list);
 INIT_LIST_HEAD(&head->node_list);

 head->lock = lock;

}






static inline __attribute__((always_inline)) void plist_node_init(struct plist_node *node, int prio)
{
 node->prio = prio;
 plist_head_init(&node->plist, ((void *)0));
}

extern void plist_add(struct plist_node *node, struct plist_head *head);
extern void plist_del(struct plist_node *node, struct plist_head *head);
# 195 "include/linux/plist.h"
static inline __attribute__((always_inline)) int plist_head_empty(const struct plist_head *head)
{
 return list_empty(&head->node_list);
}





static inline __attribute__((always_inline)) int plist_node_empty(const struct plist_node *node)
{
 return plist_head_empty(&node->plist);
}
# 234 "include/linux/plist.h"
static inline __attribute__((always_inline)) struct plist_node* plist_first(const struct plist_head *head)
{
 return ({ const typeof( ((struct plist_node *)0)->plist.node_list ) *__mptr = (head->node_list.next); (struct plist_node *)( (char *)__mptr - __builtin_offsetof(struct plist_node,plist.node_list) );})
                                        ;
}
# 17 "include/linux/rtmutex.h" 2
# 26 "include/linux/rtmutex.h"
struct rt_mutex {
 spinlock_t wait_lock;
 struct plist_head wait_list;
 struct task_struct *owner;

 int save_state;
 const char *name, *file;
 int line;
 void *magic;

};

struct rt_mutex_waiter;
struct hrtimer_sleeper;


 extern int rt_mutex_debug_check_no_locks_freed(const void *from,
      unsigned long len);
 extern void rt_mutex_debug_check_no_locks_held(struct task_struct *task);
# 58 "include/linux/rtmutex.h"
 extern void rt_mutex_debug_task_free(struct task_struct *tsk);
# 80 "include/linux/rtmutex.h"
static inline __attribute__((always_inline)) int rt_mutex_is_locked(struct rt_mutex *lock)
{
 return lock->owner != ((void *)0);
}

extern void __rt_mutex_init(struct rt_mutex *lock, const char *name);
extern void rt_mutex_destroy(struct rt_mutex *lock);

extern void rt_mutex_lock(struct rt_mutex *lock);
extern int rt_mutex_lock_interruptible(struct rt_mutex *lock,
      int detect_deadlock);
extern int rt_mutex_timed_lock(struct rt_mutex *lock,
     struct hrtimer_sleeper *timeout,
     int detect_deadlock);

extern int rt_mutex_trylock(struct rt_mutex *lock);

extern void rt_mutex_unlock(struct rt_mutex *lock);
# 81 "include/linux/sched.h" 2



# 1 "include/linux/resource.h" 1





struct task_struct;
# 24 "include/linux/resource.h"
struct rusage {
 struct timeval ru_utime;
 struct timeval ru_stime;
 long ru_maxrss;
 long ru_ixrss;
 long ru_idrss;
 long ru_isrss;
 long ru_minflt;
 long ru_majflt;
 long ru_nswap;
 long ru_inblock;
 long ru_oublock;
 long ru_msgsnd;
 long ru_msgrcv;
 long ru_nsignals;
 long ru_nvcsw;
 long ru_nivcsw;
};

struct rlimit {
 unsigned long rlim_cur;
 unsigned long rlim_max;
};
# 71 "include/linux/resource.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/resource.h" 1
# 1 "include/asm-generic/resource.h" 1
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/resource.h" 2
# 72 "include/linux/resource.h" 2

int getrusage(struct task_struct *p, int who, struct rusage __attribute__((noderef, address_space(1))) *ru);
# 85 "include/linux/sched.h" 2

# 1 "include/linux/hrtimer.h" 1
# 26 "include/linux/hrtimer.h"
struct hrtimer_clock_base;
struct hrtimer_cpu_base;




enum hrtimer_mode {
 HRTIMER_MODE_ABS,
 HRTIMER_MODE_REL,
};




enum hrtimer_restart {
 HRTIMER_NORESTART,
 HRTIMER_RESTART,
};
# 100 "include/linux/hrtimer.h"
struct hrtimer {
 struct rb_node node;
 ktime_t _expires;
 ktime_t _softexpires;
 enum hrtimer_restart (*function)(struct hrtimer *);
 struct hrtimer_clock_base *base;
 unsigned long state;
 struct list_head cb_entry;





};
# 122 "include/linux/hrtimer.h"
struct hrtimer_sleeper {
 struct hrtimer timer;
 struct task_struct *task;
};
# 139 "include/linux/hrtimer.h"
struct hrtimer_clock_base {
 struct hrtimer_cpu_base *cpu_base;
 clockid_t index;
 struct rb_root active;
 struct rb_node *first;
 ktime_t resolution;
 ktime_t (*get_time)(void);
 ktime_t softirq_time;



};
# 168 "include/linux/hrtimer.h"
struct hrtimer_cpu_base {
 spinlock_t lock;
 struct hrtimer_clock_base clock_base[2];





};

static inline __attribute__((always_inline)) void hrtimer_set_expires(struct hrtimer *timer, ktime_t time)
{
 timer->_expires = time;
 timer->_softexpires = time;
}

static inline __attribute__((always_inline)) void hrtimer_set_expires_range(struct hrtimer *timer, ktime_t time, ktime_t delta)
{
 timer->_softexpires = time;
 timer->_expires = ktime_add_safe(time, delta);
}

static inline __attribute__((always_inline)) void hrtimer_set_expires_range_ns(struct hrtimer *timer, ktime_t time, unsigned long delta)
{
 timer->_softexpires = time;
 timer->_expires = ktime_add_safe(time, ns_to_ktime(delta));
}

static inline __attribute__((always_inline)) void hrtimer_set_expires_tv64(struct hrtimer *timer, s64 tv64)
{
 timer->_expires.tv64 = tv64;
 timer->_softexpires.tv64 = tv64;
}

static inline __attribute__((always_inline)) void hrtimer_add_expires(struct hrtimer *timer, ktime_t time)
{
 timer->_expires = ktime_add_safe(timer->_expires, time);
 timer->_softexpires = ktime_add_safe(timer->_softexpires, time);
}

static inline __attribute__((always_inline)) void hrtimer_add_expires_ns(struct hrtimer *timer, u64 ns)
{
 timer->_expires = ({ (ktime_t){ .tv64 = (timer->_expires).tv64 + (ns) }; });
 timer->_softexpires = ({ (ktime_t){ .tv64 = (timer->_softexpires).tv64 + (ns) }; });
}

static inline __attribute__((always_inline)) ktime_t hrtimer_get_expires(const struct hrtimer *timer)
{
 return timer->_expires;
}

static inline __attribute__((always_inline)) ktime_t hrtimer_get_softexpires(const struct hrtimer *timer)
{
 return timer->_softexpires;
}

static inline __attribute__((always_inline)) s64 hrtimer_get_expires_tv64(const struct hrtimer *timer)
{
 return timer->_expires.tv64;
}
static inline __attribute__((always_inline)) s64 hrtimer_get_softexpires_tv64(const struct hrtimer *timer)
{
 return timer->_softexpires.tv64;
}

static inline __attribute__((always_inline)) s64 hrtimer_get_expires_ns(const struct hrtimer *timer)
{
 return ((timer->_expires).tv64);
}

static inline __attribute__((always_inline)) ktime_t hrtimer_expires_remaining(const struct hrtimer *timer)
{
    return ({ (ktime_t){ .tv64 = (timer->_expires).tv64 - (timer->base->get_time()).tv64 }; });
}
# 286 "include/linux/hrtimer.h"
static inline __attribute__((always_inline)) void clock_was_set(void) { }
static inline __attribute__((always_inline)) void hrtimer_peek_ahead_timers(void) { }

static inline __attribute__((always_inline)) void hres_timers_resume(void) { }





static inline __attribute__((always_inline)) ktime_t hrtimer_cb_get_time(struct hrtimer *timer)
{
 return timer->base->softirq_time;
}

static inline __attribute__((always_inline)) int hrtimer_is_hres_active(struct hrtimer *timer)
{
 return 0;
}


extern ktime_t ktime_get(void);
extern ktime_t ktime_get_real(void);


extern __typeof__(struct tick_device) per_cpu__tick_cpu_device;





extern void hrtimer_init(struct hrtimer *timer, clockid_t which_clock,
    enum hrtimer_mode mode);


extern void hrtimer_init_on_stack(struct hrtimer *timer, clockid_t which_clock,
      enum hrtimer_mode mode);

extern void destroy_hrtimer_on_stack(struct hrtimer *timer);
# 335 "include/linux/hrtimer.h"
extern int hrtimer_start(struct hrtimer *timer, ktime_t tim,
    const enum hrtimer_mode mode);
extern int hrtimer_start_range_ns(struct hrtimer *timer, ktime_t tim,
   unsigned long range_ns, const enum hrtimer_mode mode);
extern int hrtimer_cancel(struct hrtimer *timer);
extern int hrtimer_try_to_cancel(struct hrtimer *timer);

static inline __attribute__((always_inline)) int hrtimer_start_expires(struct hrtimer *timer,
      enum hrtimer_mode mode)
{
 unsigned long delta;
 ktime_t soft, hard;
 soft = hrtimer_get_softexpires(timer);
 hard = hrtimer_get_expires(timer);
 delta = ((({ (ktime_t){ .tv64 = (hard).tv64 - (soft).tv64 }; })).tv64);
 return hrtimer_start_range_ns(timer, soft, delta, mode);
}

static inline __attribute__((always_inline)) int hrtimer_restart(struct hrtimer *timer)
{
 return hrtimer_start_expires(timer, HRTIMER_MODE_ABS);
}


extern ktime_t hrtimer_get_remaining(const struct hrtimer *timer);
extern int hrtimer_get_res(const clockid_t which_clock, struct timespec *tp);

extern ktime_t hrtimer_get_next_event(void);





static inline __attribute__((always_inline)) int hrtimer_active(const struct hrtimer *timer)
{
 return timer->state != 0x00;
}




static inline __attribute__((always_inline)) int hrtimer_is_queued(struct hrtimer *timer)
{
 return timer->state & 0x01;
}





static inline __attribute__((always_inline)) int hrtimer_callback_running(struct hrtimer *timer)
{
 return timer->state & 0x02;
}


extern u64
hrtimer_forward(struct hrtimer *timer, ktime_t now, ktime_t interval);


static inline __attribute__((always_inline)) u64 hrtimer_forward_now(struct hrtimer *timer,
          ktime_t interval)
{
 return hrtimer_forward(timer, timer->base->get_time(), interval);
}


extern long hrtimer_nanosleep(struct timespec *rqtp,
         struct timespec __attribute__((noderef, address_space(1))) *rmtp,
         const enum hrtimer_mode mode,
         const clockid_t clockid);
extern long hrtimer_nanosleep_restart(struct restart_block *restart_block);

extern void hrtimer_init_sleeper(struct hrtimer_sleeper *sl,
     struct task_struct *tsk);

extern int schedule_hrtimeout_range(ktime_t *expires, unsigned long delta,
      const enum hrtimer_mode mode);
extern int schedule_hrtimeout(ktime_t *expires, const enum hrtimer_mode mode);


extern void hrtimer_run_queues(void);
extern void hrtimer_run_pending(void);


extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) hrtimers_init(void);
# 429 "include/linux/hrtimer.h"
extern void sysrq_timer_list_show(void);
# 459 "include/linux/hrtimer.h"
static inline __attribute__((always_inline)) void timer_stats_account_hrtimer(struct hrtimer *timer)
{
}

static inline __attribute__((always_inline)) void timer_stats_hrtimer_set_start_info(struct hrtimer *timer)
{
}

static inline __attribute__((always_inline)) void timer_stats_hrtimer_clear_start_info(struct hrtimer *timer)
{
}
# 87 "include/linux/sched.h" 2
# 1 "include/linux/task_io_accounting.h" 1
# 11 "include/linux/task_io_accounting.h"
struct task_io_accounting {
# 45 "include/linux/task_io_accounting.h"
};
# 88 "include/linux/sched.h" 2

# 1 "include/linux/latencytop.h" 1
# 17 "include/linux/latencytop.h"
struct latency_record {
 unsigned long backtrace[12];
 unsigned int count;
 unsigned long time;
 unsigned long max;
};


struct task_struct;

void account_scheduler_latency(struct task_struct *task, int usecs, int inter);

void clear_all_latency_tracing(struct task_struct *p);
# 90 "include/linux/sched.h" 2
# 1 "include/linux/cred.h" 1
# 16 "include/linux/cred.h"
# 1 "include/linux/key.h" 1
# 22 "include/linux/key.h"
# 1 "include/linux/sysctl.h" 1
# 32 "include/linux/sysctl.h"
struct file;
struct completion;






struct __sysctl_args {
 int __attribute__((noderef, address_space(1))) *name;
 int nlen;
 void __attribute__((noderef, address_space(1))) *oldval;
 size_t __attribute__((noderef, address_space(1))) *oldlenp;
 void __attribute__((noderef, address_space(1))) *newval;
 size_t newlen;
 unsigned long __unused[4];
};
# 60 "include/linux/sysctl.h"
enum
{
 CTL_KERN=1,
 CTL_VM=2,
 CTL_NET=3,
 CTL_PROC=4,
 CTL_FS=5,
 CTL_DEBUG=6,
 CTL_DEV=7,
 CTL_BUS=8,
 CTL_ABI=9,
 CTL_CPU=10,
 CTL_ARLAN=254,
 CTL_S390DBF=5677,
 CTL_SUNRPC=7249,
 CTL_PM=9899,
 CTL_FRV=9898,
};


enum
{
 CTL_BUS_ISA=1
};


enum
{
 INOTIFY_MAX_USER_INSTANCES=1,
 INOTIFY_MAX_USER_WATCHES=2,
 INOTIFY_MAX_QUEUED_EVENTS=3
};


enum
{
 KERN_OSTYPE=1,
 KERN_OSRELEASE=2,
 KERN_OSREV=3,
 KERN_VERSION=4,
 KERN_SECUREMASK=5,
 KERN_PROF=6,
 KERN_NODENAME=7,
 KERN_DOMAINNAME=8,

 KERN_PANIC=15,
 KERN_REALROOTDEV=16,

 KERN_SPARC_REBOOT=21,
 KERN_CTLALTDEL=22,
 KERN_PRINTK=23,
 KERN_NAMETRANS=24,
 KERN_PPC_HTABRECLAIM=25,
 KERN_PPC_ZEROPAGED=26,
 KERN_PPC_POWERSAVE_NAP=27,
 KERN_MODPROBE=28,
 KERN_SG_BIG_BUFF=29,
 KERN_ACCT=30,
 KERN_PPC_L2CR=31,

 KERN_RTSIGNR=32,
 KERN_RTSIGMAX=33,

 KERN_SHMMAX=34,
 KERN_MSGMAX=35,
 KERN_MSGMNB=36,
 KERN_MSGPOOL=37,
 KERN_SYSRQ=38,
 KERN_MAX_THREADS=39,
  KERN_RANDOM=40,
  KERN_SHMALL=41,
  KERN_MSGMNI=42,
  KERN_SEM=43,
  KERN_SPARC_STOP_A=44,
  KERN_SHMMNI=45,
 KERN_OVERFLOWUID=46,
 KERN_OVERFLOWGID=47,
 KERN_SHMPATH=48,
 KERN_HOTPLUG=49,
 KERN_IEEE_EMULATION_WARNINGS=50,
 KERN_S390_USER_DEBUG_LOGGING=51,
 KERN_CORE_USES_PID=52,
 KERN_TAINTED=53,
 KERN_CADPID=54,
 KERN_PIDMAX=55,
   KERN_CORE_PATTERN=56,
 KERN_PANIC_ON_OOPS=57,
 KERN_HPPA_PWRSW=58,
 KERN_HPPA_UNALIGNED=59,
 KERN_PRINTK_RATELIMIT=60,
 KERN_PRINTK_RATELIMIT_BURST=61,
 KERN_PTY=62,
 KERN_NGROUPS_MAX=63,
 KERN_SPARC_SCONS_PWROFF=64,
 KERN_HZ_TIMER=65,
 KERN_UNKNOWN_NMI_PANIC=66,
 KERN_BOOTLOADER_TYPE=67,
 KERN_RANDOMIZE=68,
 KERN_SETUID_DUMPABLE=69,
 KERN_SPIN_RETRY=70,
 KERN_ACPI_VIDEO_FLAGS=71,
 KERN_IA64_UNALIGNED=72,
 KERN_COMPAT_LOG=73,
 KERN_MAX_LOCK_DEPTH=74,
 KERN_NMI_WATCHDOG=75,
 KERN_PANIC_ON_NMI=76,
};




enum
{
 VM_UNUSED1=1,
 VM_UNUSED2=2,
 VM_UNUSED3=3,
 VM_UNUSED4=4,
 VM_OVERCOMMIT_MEMORY=5,
 VM_UNUSED5=6,
 VM_UNUSED7=7,
 VM_UNUSED8=8,
 VM_UNUSED9=9,
 VM_PAGE_CLUSTER=10,
 VM_DIRTY_BACKGROUND=11,
 VM_DIRTY_RATIO=12,
 VM_DIRTY_WB_CS=13,
 VM_DIRTY_EXPIRE_CS=14,
 VM_NR_PDFLUSH_THREADS=15,
 VM_OVERCOMMIT_RATIO=16,
 VM_PAGEBUF=17,
 VM_HUGETLB_PAGES=18,
 VM_SWAPPINESS=19,
 VM_LOWMEM_RESERVE_RATIO=20,
 VM_MIN_FREE_KBYTES=21,
 VM_MAX_MAP_COUNT=22,
 VM_LAPTOP_MODE=23,
 VM_BLOCK_DUMP=24,
 VM_HUGETLB_GROUP=25,
 VM_VFS_CACHE_PRESSURE=26,
 VM_LEGACY_VA_LAYOUT=27,
 VM_SWAP_TOKEN_TIMEOUT=28,
 VM_DROP_PAGECACHE=29,
 VM_PERCPU_PAGELIST_FRACTION=30,
 VM_ZONE_RECLAIM_MODE=31,
 VM_MIN_UNMAPPED=32,
 VM_PANIC_ON_OOM=33,
 VM_VDSO_ENABLED=34,
 VM_MIN_SLAB=35,
};



enum
{
 NET_CORE=1,
 NET_ETHER=2,
 NET_802=3,
 NET_UNIX=4,
 NET_IPV4=5,
 NET_IPX=6,
 NET_ATALK=7,
 NET_NETROM=8,
 NET_AX25=9,
 NET_BRIDGE=10,
 NET_ROSE=11,
 NET_IPV6=12,
 NET_X25=13,
 NET_TR=14,
 NET_DECNET=15,
 NET_ECONET=16,
 NET_SCTP=17,
 NET_LLC=18,
 NET_NETFILTER=19,
 NET_DCCP=20,
 NET_IRDA=412,
};


enum
{
 RANDOM_POOLSIZE=1,
 RANDOM_ENTROPY_COUNT=2,
 RANDOM_READ_THRESH=3,
 RANDOM_WRITE_THRESH=4,
 RANDOM_BOOT_ID=5,
 RANDOM_UUID=6
};


enum
{
 PTY_MAX=1,
 PTY_NR=2
};


enum
{
 BUS_ISA_MEM_BASE=1,
 BUS_ISA_PORT_BASE=2,
 BUS_ISA_PORT_SHIFT=3
};


enum
{
 NET_CORE_WMEM_MAX=1,
 NET_CORE_RMEM_MAX=2,
 NET_CORE_WMEM_DEFAULT=3,
 NET_CORE_RMEM_DEFAULT=4,

 NET_CORE_MAX_BACKLOG=6,
 NET_CORE_FASTROUTE=7,
 NET_CORE_MSG_COST=8,
 NET_CORE_MSG_BURST=9,
 NET_CORE_OPTMEM_MAX=10,
 NET_CORE_HOT_LIST_LENGTH=11,
 NET_CORE_DIVERT_VERSION=12,
 NET_CORE_NO_CONG_THRESH=13,
 NET_CORE_NO_CONG=14,
 NET_CORE_LO_CONG=15,
 NET_CORE_MOD_CONG=16,
 NET_CORE_DEV_WEIGHT=17,
 NET_CORE_SOMAXCONN=18,
 NET_CORE_BUDGET=19,
 NET_CORE_AEVENT_ETIME=20,
 NET_CORE_AEVENT_RSEQTH=21,
 NET_CORE_WARNINGS=22,
};







enum
{
 NET_UNIX_DESTROY_DELAY=1,
 NET_UNIX_DELETE_DELAY=2,
 NET_UNIX_MAX_DGRAM_QLEN=3,
};


enum
{
 NET_NF_CONNTRACK_MAX=1,
 NET_NF_CONNTRACK_TCP_TIMEOUT_SYN_SENT=2,
 NET_NF_CONNTRACK_TCP_TIMEOUT_SYN_RECV=3,
 NET_NF_CONNTRACK_TCP_TIMEOUT_ESTABLISHED=4,
 NET_NF_CONNTRACK_TCP_TIMEOUT_FIN_WAIT=5,
 NET_NF_CONNTRACK_TCP_TIMEOUT_CLOSE_WAIT=6,
 NET_NF_CONNTRACK_TCP_TIMEOUT_LAST_ACK=7,
 NET_NF_CONNTRACK_TCP_TIMEOUT_TIME_WAIT=8,
 NET_NF_CONNTRACK_TCP_TIMEOUT_CLOSE=9,
 NET_NF_CONNTRACK_UDP_TIMEOUT=10,
 NET_NF_CONNTRACK_UDP_TIMEOUT_STREAM=11,
 NET_NF_CONNTRACK_ICMP_TIMEOUT=12,
 NET_NF_CONNTRACK_GENERIC_TIMEOUT=13,
 NET_NF_CONNTRACK_BUCKETS=14,
 NET_NF_CONNTRACK_LOG_INVALID=15,
 NET_NF_CONNTRACK_TCP_TIMEOUT_MAX_RETRANS=16,
 NET_NF_CONNTRACK_TCP_LOOSE=17,
 NET_NF_CONNTRACK_TCP_BE_LIBERAL=18,
 NET_NF_CONNTRACK_TCP_MAX_RETRANS=19,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_CLOSED=20,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_COOKIE_WAIT=21,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_COOKIE_ECHOED=22,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_ESTABLISHED=23,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_SENT=24,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_RECD=25,
 NET_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_ACK_SENT=26,
 NET_NF_CONNTRACK_COUNT=27,
 NET_NF_CONNTRACK_ICMPV6_TIMEOUT=28,
 NET_NF_CONNTRACK_FRAG6_TIMEOUT=29,
 NET_NF_CONNTRACK_FRAG6_LOW_THRESH=30,
 NET_NF_CONNTRACK_FRAG6_HIGH_THRESH=31,
 NET_NF_CONNTRACK_CHECKSUM=32,
};


enum
{

 NET_IPV4_FORWARD=8,
 NET_IPV4_DYNADDR=9,

 NET_IPV4_CONF=16,
 NET_IPV4_NEIGH=17,
 NET_IPV4_ROUTE=18,
 NET_IPV4_FIB_HASH=19,
 NET_IPV4_NETFILTER=20,

 NET_IPV4_TCP_TIMESTAMPS=33,
 NET_IPV4_TCP_WINDOW_SCALING=34,
 NET_IPV4_TCP_SACK=35,
 NET_IPV4_TCP_RETRANS_COLLAPSE=36,
 NET_IPV4_DEFAULT_TTL=37,
 NET_IPV4_AUTOCONFIG=38,
 NET_IPV4_NO_PMTU_DISC=39,
 NET_IPV4_TCP_SYN_RETRIES=40,
 NET_IPV4_IPFRAG_HIGH_THRESH=41,
 NET_IPV4_IPFRAG_LOW_THRESH=42,
 NET_IPV4_IPFRAG_TIME=43,
 NET_IPV4_TCP_MAX_KA_PROBES=44,
 NET_IPV4_TCP_KEEPALIVE_TIME=45,
 NET_IPV4_TCP_KEEPALIVE_PROBES=46,
 NET_IPV4_TCP_RETRIES1=47,
 NET_IPV4_TCP_RETRIES2=48,
 NET_IPV4_TCP_FIN_TIMEOUT=49,
 NET_IPV4_IP_MASQ_DEBUG=50,
 NET_TCP_SYNCOOKIES=51,
 NET_TCP_STDURG=52,
 NET_TCP_RFC1337=53,
 NET_TCP_SYN_TAILDROP=54,
 NET_TCP_MAX_SYN_BACKLOG=55,
 NET_IPV4_LOCAL_PORT_RANGE=56,
 NET_IPV4_ICMP_ECHO_IGNORE_ALL=57,
 NET_IPV4_ICMP_ECHO_IGNORE_BROADCASTS=58,
 NET_IPV4_ICMP_SOURCEQUENCH_RATE=59,
 NET_IPV4_ICMP_DESTUNREACH_RATE=60,
 NET_IPV4_ICMP_TIMEEXCEED_RATE=61,
 NET_IPV4_ICMP_PARAMPROB_RATE=62,
 NET_IPV4_ICMP_ECHOREPLY_RATE=63,
 NET_IPV4_ICMP_IGNORE_BOGUS_ERROR_RESPONSES=64,
 NET_IPV4_IGMP_MAX_MEMBERSHIPS=65,
 NET_TCP_TW_RECYCLE=66,
 NET_IPV4_ALWAYS_DEFRAG=67,
 NET_IPV4_TCP_KEEPALIVE_INTVL=68,
 NET_IPV4_INET_PEER_THRESHOLD=69,
 NET_IPV4_INET_PEER_MINTTL=70,
 NET_IPV4_INET_PEER_MAXTTL=71,
 NET_IPV4_INET_PEER_GC_MINTIME=72,
 NET_IPV4_INET_PEER_GC_MAXTIME=73,
 NET_TCP_ORPHAN_RETRIES=74,
 NET_TCP_ABORT_ON_OVERFLOW=75,
 NET_TCP_SYNACK_RETRIES=76,
 NET_TCP_MAX_ORPHANS=77,
 NET_TCP_MAX_TW_BUCKETS=78,
 NET_TCP_FACK=79,
 NET_TCP_REORDERING=80,
 NET_TCP_ECN=81,
 NET_TCP_DSACK=82,
 NET_TCP_MEM=83,
 NET_TCP_WMEM=84,
 NET_TCP_RMEM=85,
 NET_TCP_APP_WIN=86,
 NET_TCP_ADV_WIN_SCALE=87,
 NET_IPV4_NONLOCAL_BIND=88,
 NET_IPV4_ICMP_RATELIMIT=89,
 NET_IPV4_ICMP_RATEMASK=90,
 NET_TCP_TW_REUSE=91,
 NET_TCP_FRTO=92,
 NET_TCP_LOW_LATENCY=93,
 NET_IPV4_IPFRAG_SECRET_INTERVAL=94,
 NET_IPV4_IGMP_MAX_MSF=96,
 NET_TCP_NO_METRICS_SAVE=97,
 NET_TCP_DEFAULT_WIN_SCALE=105,
 NET_TCP_MODERATE_RCVBUF=106,
 NET_TCP_TSO_WIN_DIVISOR=107,
 NET_TCP_BIC_BETA=108,
 NET_IPV4_ICMP_ERRORS_USE_INBOUND_IFADDR=109,
 NET_TCP_CONG_CONTROL=110,
 NET_TCP_ABC=111,
 NET_IPV4_IPFRAG_MAX_DIST=112,
  NET_TCP_MTU_PROBING=113,
 NET_TCP_BASE_MSS=114,
 NET_IPV4_TCP_WORKAROUND_SIGNED_WINDOWS=115,
 NET_TCP_DMA_COPYBREAK=116,
 NET_TCP_SLOW_START_AFTER_IDLE=117,
 NET_CIPSOV4_CACHE_ENABLE=118,
 NET_CIPSOV4_CACHE_BUCKET_SIZE=119,
 NET_CIPSOV4_RBM_OPTFMT=120,
 NET_CIPSOV4_RBM_STRICTVALID=121,
 NET_TCP_AVAIL_CONG_CONTROL=122,
 NET_TCP_ALLOWED_CONG_CONTROL=123,
 NET_TCP_MAX_SSTHRESH=124,
 NET_TCP_FRTO_RESPONSE=125,
};

enum {
 NET_IPV4_ROUTE_FLUSH=1,
 NET_IPV4_ROUTE_MIN_DELAY=2,
 NET_IPV4_ROUTE_MAX_DELAY=3,
 NET_IPV4_ROUTE_GC_THRESH=4,
 NET_IPV4_ROUTE_MAX_SIZE=5,
 NET_IPV4_ROUTE_GC_MIN_INTERVAL=6,
 NET_IPV4_ROUTE_GC_TIMEOUT=7,
 NET_IPV4_ROUTE_GC_INTERVAL=8,
 NET_IPV4_ROUTE_REDIRECT_LOAD=9,
 NET_IPV4_ROUTE_REDIRECT_NUMBER=10,
 NET_IPV4_ROUTE_REDIRECT_SILENCE=11,
 NET_IPV4_ROUTE_ERROR_COST=12,
 NET_IPV4_ROUTE_ERROR_BURST=13,
 NET_IPV4_ROUTE_GC_ELASTICITY=14,
 NET_IPV4_ROUTE_MTU_EXPIRES=15,
 NET_IPV4_ROUTE_MIN_PMTU=16,
 NET_IPV4_ROUTE_MIN_ADVMSS=17,
 NET_IPV4_ROUTE_SECRET_INTERVAL=18,
 NET_IPV4_ROUTE_GC_MIN_INTERVAL_MS=19,
};

enum
{
 NET_PROTO_CONF_ALL=-2,
 NET_PROTO_CONF_DEFAULT=-3


};

enum
{
 NET_IPV4_CONF_FORWARDING=1,
 NET_IPV4_CONF_MC_FORWARDING=2,
 NET_IPV4_CONF_PROXY_ARP=3,
 NET_IPV4_CONF_ACCEPT_REDIRECTS=4,
 NET_IPV4_CONF_SECURE_REDIRECTS=5,
 NET_IPV4_CONF_SEND_REDIRECTS=6,
 NET_IPV4_CONF_SHARED_MEDIA=7,
 NET_IPV4_CONF_RP_FILTER=8,
 NET_IPV4_CONF_ACCEPT_SOURCE_ROUTE=9,
 NET_IPV4_CONF_BOOTP_RELAY=10,
 NET_IPV4_CONF_LOG_MARTIANS=11,
 NET_IPV4_CONF_TAG=12,
 NET_IPV4_CONF_ARPFILTER=13,
 NET_IPV4_CONF_MEDIUM_ID=14,
 NET_IPV4_CONF_NOXFRM=15,
 NET_IPV4_CONF_NOPOLICY=16,
 NET_IPV4_CONF_FORCE_IGMP_VERSION=17,
 NET_IPV4_CONF_ARP_ANNOUNCE=18,
 NET_IPV4_CONF_ARP_IGNORE=19,
 NET_IPV4_CONF_PROMOTE_SECONDARIES=20,
 NET_IPV4_CONF_ARP_ACCEPT=21,
 __NET_IPV4_CONF_MAX
};


enum
{
 NET_IPV4_NF_CONNTRACK_MAX=1,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_SYN_SENT=2,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_SYN_RECV=3,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_ESTABLISHED=4,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_FIN_WAIT=5,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_CLOSE_WAIT=6,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_LAST_ACK=7,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_TIME_WAIT=8,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_CLOSE=9,
 NET_IPV4_NF_CONNTRACK_UDP_TIMEOUT=10,
 NET_IPV4_NF_CONNTRACK_UDP_TIMEOUT_STREAM=11,
 NET_IPV4_NF_CONNTRACK_ICMP_TIMEOUT=12,
 NET_IPV4_NF_CONNTRACK_GENERIC_TIMEOUT=13,
 NET_IPV4_NF_CONNTRACK_BUCKETS=14,
 NET_IPV4_NF_CONNTRACK_LOG_INVALID=15,
 NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_MAX_RETRANS=16,
 NET_IPV4_NF_CONNTRACK_TCP_LOOSE=17,
 NET_IPV4_NF_CONNTRACK_TCP_BE_LIBERAL=18,
 NET_IPV4_NF_CONNTRACK_TCP_MAX_RETRANS=19,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_CLOSED=20,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_COOKIE_WAIT=21,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_COOKIE_ECHOED=22,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_ESTABLISHED=23,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_SENT=24,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_RECD=25,
  NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_ACK_SENT=26,
 NET_IPV4_NF_CONNTRACK_COUNT=27,
 NET_IPV4_NF_CONNTRACK_CHECKSUM=28,
};


enum {
 NET_IPV6_CONF=16,
 NET_IPV6_NEIGH=17,
 NET_IPV6_ROUTE=18,
 NET_IPV6_ICMP=19,
 NET_IPV6_BINDV6ONLY=20,
 NET_IPV6_IP6FRAG_HIGH_THRESH=21,
 NET_IPV6_IP6FRAG_LOW_THRESH=22,
 NET_IPV6_IP6FRAG_TIME=23,
 NET_IPV6_IP6FRAG_SECRET_INTERVAL=24,
 NET_IPV6_MLD_MAX_MSF=25,
};

enum {
 NET_IPV6_ROUTE_FLUSH=1,
 NET_IPV6_ROUTE_GC_THRESH=2,
 NET_IPV6_ROUTE_MAX_SIZE=3,
 NET_IPV6_ROUTE_GC_MIN_INTERVAL=4,
 NET_IPV6_ROUTE_GC_TIMEOUT=5,
 NET_IPV6_ROUTE_GC_INTERVAL=6,
 NET_IPV6_ROUTE_GC_ELASTICITY=7,
 NET_IPV6_ROUTE_MTU_EXPIRES=8,
 NET_IPV6_ROUTE_MIN_ADVMSS=9,
 NET_IPV6_ROUTE_GC_MIN_INTERVAL_MS=10
};

enum {
 NET_IPV6_FORWARDING=1,
 NET_IPV6_HOP_LIMIT=2,
 NET_IPV6_MTU=3,
 NET_IPV6_ACCEPT_RA=4,
 NET_IPV6_ACCEPT_REDIRECTS=5,
 NET_IPV6_AUTOCONF=6,
 NET_IPV6_DAD_TRANSMITS=7,
 NET_IPV6_RTR_SOLICITS=8,
 NET_IPV6_RTR_SOLICIT_INTERVAL=9,
 NET_IPV6_RTR_SOLICIT_DELAY=10,
 NET_IPV6_USE_TEMPADDR=11,
 NET_IPV6_TEMP_VALID_LFT=12,
 NET_IPV6_TEMP_PREFERED_LFT=13,
 NET_IPV6_REGEN_MAX_RETRY=14,
 NET_IPV6_MAX_DESYNC_FACTOR=15,
 NET_IPV6_MAX_ADDRESSES=16,
 NET_IPV6_FORCE_MLD_VERSION=17,
 NET_IPV6_ACCEPT_RA_DEFRTR=18,
 NET_IPV6_ACCEPT_RA_PINFO=19,
 NET_IPV6_ACCEPT_RA_RTR_PREF=20,
 NET_IPV6_RTR_PROBE_INTERVAL=21,
 NET_IPV6_ACCEPT_RA_RT_INFO_MAX_PLEN=22,
 NET_IPV6_PROXY_NDP=23,
 NET_IPV6_ACCEPT_SOURCE_ROUTE=25,
 __NET_IPV6_MAX
};


enum {
 NET_IPV6_ICMP_RATELIMIT=1
};


enum {
 NET_NEIGH_MCAST_SOLICIT=1,
 NET_NEIGH_UCAST_SOLICIT=2,
 NET_NEIGH_APP_SOLICIT=3,
 NET_NEIGH_RETRANS_TIME=4,
 NET_NEIGH_REACHABLE_TIME=5,
 NET_NEIGH_DELAY_PROBE_TIME=6,
 NET_NEIGH_GC_STALE_TIME=7,
 NET_NEIGH_UNRES_QLEN=8,
 NET_NEIGH_PROXY_QLEN=9,
 NET_NEIGH_ANYCAST_DELAY=10,
 NET_NEIGH_PROXY_DELAY=11,
 NET_NEIGH_LOCKTIME=12,
 NET_NEIGH_GC_INTERVAL=13,
 NET_NEIGH_GC_THRESH1=14,
 NET_NEIGH_GC_THRESH2=15,
 NET_NEIGH_GC_THRESH3=16,
 NET_NEIGH_RETRANS_TIME_MS=17,
 NET_NEIGH_REACHABLE_TIME_MS=18,
 __NET_NEIGH_MAX
};


enum {
 NET_DCCP_DEFAULT=1,
};


enum {
 NET_IPX_PPROP_BROADCASTING=1,
 NET_IPX_FORWARDING=2
};


enum {
 NET_LLC2=1,
 NET_LLC_STATION=2,
};


enum {
 NET_LLC2_TIMEOUT=1,
};


enum {
 NET_LLC_STATION_ACK_TIMEOUT=1,
};


enum {
 NET_LLC2_ACK_TIMEOUT=1,
 NET_LLC2_P_TIMEOUT=2,
 NET_LLC2_REJ_TIMEOUT=3,
 NET_LLC2_BUSY_TIMEOUT=4,
};


enum {
 NET_ATALK_AARP_EXPIRY_TIME=1,
 NET_ATALK_AARP_TICK_TIME=2,
 NET_ATALK_AARP_RETRANSMIT_LIMIT=3,
 NET_ATALK_AARP_RESOLVE_TIME=4
};



enum {
 NET_NETROM_DEFAULT_PATH_QUALITY=1,
 NET_NETROM_OBSOLESCENCE_COUNT_INITIALISER=2,
 NET_NETROM_NETWORK_TTL_INITIALISER=3,
 NET_NETROM_TRANSPORT_TIMEOUT=4,
 NET_NETROM_TRANSPORT_MAXIMUM_TRIES=5,
 NET_NETROM_TRANSPORT_ACKNOWLEDGE_DELAY=6,
 NET_NETROM_TRANSPORT_BUSY_DELAY=7,
 NET_NETROM_TRANSPORT_REQUESTED_WINDOW_SIZE=8,
 NET_NETROM_TRANSPORT_NO_ACTIVITY_TIMEOUT=9,
 NET_NETROM_ROUTING_CONTROL=10,
 NET_NETROM_LINK_FAILS_COUNT=11,
 NET_NETROM_RESET=12
};


enum {
 NET_AX25_IP_DEFAULT_MODE=1,
 NET_AX25_DEFAULT_MODE=2,
 NET_AX25_BACKOFF_TYPE=3,
 NET_AX25_CONNECT_MODE=4,
 NET_AX25_STANDARD_WINDOW=5,
 NET_AX25_EXTENDED_WINDOW=6,
 NET_AX25_T1_TIMEOUT=7,
 NET_AX25_T2_TIMEOUT=8,
 NET_AX25_T3_TIMEOUT=9,
 NET_AX25_IDLE_TIMEOUT=10,
 NET_AX25_N2=11,
 NET_AX25_PACLEN=12,
 NET_AX25_PROTOCOL=13,
 NET_AX25_DAMA_SLAVE_TIMEOUT=14
};


enum {
 NET_ROSE_RESTART_REQUEST_TIMEOUT=1,
 NET_ROSE_CALL_REQUEST_TIMEOUT=2,
 NET_ROSE_RESET_REQUEST_TIMEOUT=3,
 NET_ROSE_CLEAR_REQUEST_TIMEOUT=4,
 NET_ROSE_ACK_HOLD_BACK_TIMEOUT=5,
 NET_ROSE_ROUTING_CONTROL=6,
 NET_ROSE_LINK_FAIL_TIMEOUT=7,
 NET_ROSE_MAX_VCS=8,
 NET_ROSE_WINDOW_SIZE=9,
 NET_ROSE_NO_ACTIVITY_TIMEOUT=10
};


enum {
 NET_X25_RESTART_REQUEST_TIMEOUT=1,
 NET_X25_CALL_REQUEST_TIMEOUT=2,
 NET_X25_RESET_REQUEST_TIMEOUT=3,
 NET_X25_CLEAR_REQUEST_TIMEOUT=4,
 NET_X25_ACK_HOLD_BACK_TIMEOUT=5,
 NET_X25_FORWARD=6
};


enum
{
 NET_TR_RIF_TIMEOUT=1
};


enum {
 NET_DECNET_NODE_TYPE = 1,
 NET_DECNET_NODE_ADDRESS = 2,
 NET_DECNET_NODE_NAME = 3,
 NET_DECNET_DEFAULT_DEVICE = 4,
 NET_DECNET_TIME_WAIT = 5,
 NET_DECNET_DN_COUNT = 6,
 NET_DECNET_DI_COUNT = 7,
 NET_DECNET_DR_COUNT = 8,
 NET_DECNET_DST_GC_INTERVAL = 9,
 NET_DECNET_CONF = 10,
 NET_DECNET_NO_FC_MAX_CWND = 11,
 NET_DECNET_MEM = 12,
 NET_DECNET_RMEM = 13,
 NET_DECNET_WMEM = 14,
 NET_DECNET_DEBUG_LEVEL = 255
};


enum {
 NET_DECNET_CONF_LOOPBACK = -2,
 NET_DECNET_CONF_DDCMP = -3,
 NET_DECNET_CONF_PPP = -4,
 NET_DECNET_CONF_X25 = -5,
 NET_DECNET_CONF_GRE = -6,
 NET_DECNET_CONF_ETHER = -7


};


enum {
 NET_DECNET_CONF_DEV_PRIORITY = 1,
 NET_DECNET_CONF_DEV_T1 = 2,
 NET_DECNET_CONF_DEV_T2 = 3,
 NET_DECNET_CONF_DEV_T3 = 4,
 NET_DECNET_CONF_DEV_FORWARDING = 5,
 NET_DECNET_CONF_DEV_BLKSIZE = 6,
 NET_DECNET_CONF_DEV_STATE = 7
};


enum {
 NET_SCTP_RTO_INITIAL = 1,
 NET_SCTP_RTO_MIN = 2,
 NET_SCTP_RTO_MAX = 3,
 NET_SCTP_RTO_ALPHA = 4,
 NET_SCTP_RTO_BETA = 5,
 NET_SCTP_VALID_COOKIE_LIFE = 6,
 NET_SCTP_ASSOCIATION_MAX_RETRANS = 7,
 NET_SCTP_PATH_MAX_RETRANS = 8,
 NET_SCTP_MAX_INIT_RETRANSMITS = 9,
 NET_SCTP_HB_INTERVAL = 10,
 NET_SCTP_PRESERVE_ENABLE = 11,
 NET_SCTP_MAX_BURST = 12,
 NET_SCTP_ADDIP_ENABLE = 13,
 NET_SCTP_PRSCTP_ENABLE = 14,
 NET_SCTP_SNDBUF_POLICY = 15,
 NET_SCTP_SACK_TIMEOUT = 16,
 NET_SCTP_RCVBUF_POLICY = 17,
};


enum {
 NET_BRIDGE_NF_CALL_ARPTABLES = 1,
 NET_BRIDGE_NF_CALL_IPTABLES = 2,
 NET_BRIDGE_NF_CALL_IP6TABLES = 3,
 NET_BRIDGE_NF_FILTER_VLAN_TAGGED = 4,
 NET_BRIDGE_NF_FILTER_PPPOE_TAGGED = 5,
};


enum {
 NET_IRDA_DISCOVERY=1,
 NET_IRDA_DEVNAME=2,
 NET_IRDA_DEBUG=3,
 NET_IRDA_FAST_POLL=4,
 NET_IRDA_DISCOVERY_SLOTS=5,
 NET_IRDA_DISCOVERY_TIMEOUT=6,
 NET_IRDA_SLOT_TIMEOUT=7,
 NET_IRDA_MAX_BAUD_RATE=8,
 NET_IRDA_MIN_TX_TURN_TIME=9,
 NET_IRDA_MAX_TX_DATA_SIZE=10,
 NET_IRDA_MAX_TX_WINDOW=11,
 NET_IRDA_MAX_NOREPLY_TIME=12,
 NET_IRDA_WARN_NOREPLY_TIME=13,
 NET_IRDA_LAP_KEEPALIVE_TIME=14,
};



enum
{
 FS_NRINODE=1,
 FS_STATINODE=2,
 FS_MAXINODE=3,
 FS_NRDQUOT=4,
 FS_MAXDQUOT=5,
 FS_NRFILE=6,
 FS_MAXFILE=7,
 FS_DENTRY=8,
 FS_NRSUPER=9,
 FS_MAXSUPER=10,
 FS_OVERFLOWUID=11,
 FS_OVERFLOWGID=12,
 FS_LEASES=13,
 FS_DIR_NOTIFY=14,
 FS_LEASE_TIME=15,
 FS_DQSTATS=16,
 FS_XFS=17,
 FS_AIO_NR=18,
 FS_AIO_MAX_NR=19,
 FS_INOTIFY=20,
 FS_OCFS2=988,
};


enum {
 FS_DQ_LOOKUPS = 1,
 FS_DQ_DROPS = 2,
 FS_DQ_READS = 3,
 FS_DQ_WRITES = 4,
 FS_DQ_CACHE_HITS = 5,
 FS_DQ_ALLOCATED = 6,
 FS_DQ_FREE = 7,
 FS_DQ_SYNCS = 8,
 FS_DQ_WARNINGS = 9,
};




enum {
 DEV_CDROM=1,
 DEV_HWMON=2,
 DEV_PARPORT=3,
 DEV_RAID=4,
 DEV_MAC_HID=5,
 DEV_SCSI=6,
 DEV_IPMI=7,
};


enum {
 DEV_CDROM_INFO=1,
 DEV_CDROM_AUTOCLOSE=2,
 DEV_CDROM_AUTOEJECT=3,
 DEV_CDROM_DEBUG=4,
 DEV_CDROM_LOCK=5,
 DEV_CDROM_CHECK_MEDIA=6
};


enum {
 DEV_PARPORT_DEFAULT=-3
};


enum {
 DEV_RAID_SPEED_LIMIT_MIN=1,
 DEV_RAID_SPEED_LIMIT_MAX=2
};


enum {
 DEV_PARPORT_DEFAULT_TIMESLICE=1,
 DEV_PARPORT_DEFAULT_SPINTIME=2
};


enum {
 DEV_PARPORT_SPINTIME=1,
 DEV_PARPORT_BASE_ADDR=2,
 DEV_PARPORT_IRQ=3,
 DEV_PARPORT_DMA=4,
 DEV_PARPORT_MODES=5,
 DEV_PARPORT_DEVICES=6,
 DEV_PARPORT_AUTOPROBE=16
};


enum {
 DEV_PARPORT_DEVICES_ACTIVE=-3,
};


enum {
 DEV_PARPORT_DEVICE_TIMESLICE=1,
};


enum {
 DEV_MAC_HID_KEYBOARD_SENDS_LINUX_KEYCODES=1,
 DEV_MAC_HID_KEYBOARD_LOCK_KEYCODES=2,
 DEV_MAC_HID_MOUSE_BUTTON_EMULATION=3,
 DEV_MAC_HID_MOUSE_BUTTON2_KEYCODE=4,
 DEV_MAC_HID_MOUSE_BUTTON3_KEYCODE=5,
 DEV_MAC_HID_ADB_MOUSE_SENDS_KEYCODES=6
};


enum {
 DEV_SCSI_LOGGING_LEVEL=1,
};


enum {
 DEV_IPMI_POWEROFF_POWERCYCLE=1,
};


enum
{
 ABI_DEFHANDLER_COFF=1,
 ABI_DEFHANDLER_ELF=2,
 ABI_DEFHANDLER_LCALL7=3,
 ABI_DEFHANDLER_LIBCSO=4,
 ABI_TRACE=5,
 ABI_FAKE_UTSNAME=6,
};





struct ctl_table;
struct nsproxy;
struct ctl_table_root;

struct ctl_table_set {
 struct list_head list;
 struct ctl_table_set *parent;
 int (*is_seen)(struct ctl_table_set *);
};

extern void setup_sysctl_set(struct ctl_table_set *p,
 struct ctl_table_set *parent,
 int (*is_seen)(struct ctl_table_set *));

struct ctl_table_header;

extern void sysctl_head_get(struct ctl_table_header *);
extern void sysctl_head_put(struct ctl_table_header *);
extern int sysctl_is_seen(struct ctl_table_header *);
extern struct ctl_table_header *sysctl_head_grab(struct ctl_table_header *);
extern struct ctl_table_header *sysctl_head_next(struct ctl_table_header *prev);
extern struct ctl_table_header *__sysctl_head_next(struct nsproxy *namespaces,
      struct ctl_table_header *prev);
extern void sysctl_head_finish(struct ctl_table_header *prev);
extern int sysctl_perm(struct ctl_table_root *root,
  struct ctl_table *table, int op);

typedef struct ctl_table ctl_table;

typedef int ctl_handler (struct ctl_table *table,
    void __attribute__((noderef, address_space(1))) *oldval, size_t __attribute__((noderef, address_space(1))) *oldlenp,
    void __attribute__((noderef, address_space(1))) *newval, size_t newlen);

typedef int proc_handler (struct ctl_table *ctl, int write, struct file * filp,
     void __attribute__((noderef, address_space(1))) *buffer, size_t *lenp, loff_t *ppos);

extern int proc_dostring(struct ctl_table *, int, struct file *,
    void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_dointvec(struct ctl_table *, int, struct file *,
    void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_dointvec_minmax(struct ctl_table *, int, struct file *,
    void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_dointvec_jiffies(struct ctl_table *, int, struct file *,
     void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_dointvec_userhz_jiffies(struct ctl_table *, int, struct file *,
     void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_dointvec_ms_jiffies(struct ctl_table *, int, struct file *,
        void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_doulongvec_minmax(struct ctl_table *, int, struct file *,
      void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
extern int proc_doulongvec_ms_jiffies_minmax(struct ctl_table *table, int,
          struct file *, void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);

extern int do_sysctl (int __attribute__((noderef, address_space(1))) *name, int nlen,
        void __attribute__((noderef, address_space(1))) *oldval, size_t __attribute__((noderef, address_space(1))) *oldlenp,
        void __attribute__((noderef, address_space(1))) *newval, size_t newlen);

extern ctl_handler sysctl_data;
extern ctl_handler sysctl_string;
extern ctl_handler sysctl_intvec;
extern ctl_handler sysctl_jiffies;
extern ctl_handler sysctl_ms_jiffies;
# 1051 "include/linux/sysctl.h"
struct ctl_table
{
 int ctl_name;
 const char *procname;
 void *data;
 int maxlen;
 mode_t mode;
 struct ctl_table *child;
 struct ctl_table *parent;
 proc_handler *proc_handler;
 ctl_handler *strategy;
 void *extra1;
 void *extra2;
};

struct ctl_table_root {
 struct list_head root_list;
 struct ctl_table_set default_set;
 struct ctl_table_set *(*lookup)(struct ctl_table_root *root,
        struct nsproxy *namespaces);
 int (*permissions)(struct ctl_table_root *root,
   struct nsproxy *namespaces, struct ctl_table *table);
};



struct ctl_table_header
{
 struct ctl_table *ctl_table;
 struct list_head ctl_entry;
 int used;
 int count;
 struct completion *unregistering;
 struct ctl_table *ctl_table_arg;
 struct ctl_table_root *root;
 struct ctl_table_set *set;
 struct ctl_table *attached_by;
 struct ctl_table *attached_to;
 struct ctl_table_header *parent;
};


struct ctl_path {
 const char *procname;
 int ctl_name;
};

void register_sysctl_root(struct ctl_table_root *root);
struct ctl_table_header *__register_sysctl_paths(
 struct ctl_table_root *root, struct nsproxy *namespaces,
 const struct ctl_path *path, struct ctl_table *table);
struct ctl_table_header *register_sysctl_table(struct ctl_table * table);
struct ctl_table_header *register_sysctl_paths(const struct ctl_path *path,
      struct ctl_table *table);

void unregister_sysctl_table(struct ctl_table_header * table);
int sysctl_check_table(struct nsproxy *namespaces, struct ctl_table *table);
# 23 "include/linux/key.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 24 "include/linux/key.h" 2




typedef int32_t key_serial_t;


typedef uint32_t key_perm_t;

struct key;
# 73 "include/linux/key.h"
struct seq_file;
struct user_struct;
struct signal_struct;
struct cred;

struct key_type;
struct key_owner;
struct keyring_list;
struct keyring_name;
# 97 "include/linux/key.h"
typedef struct __key_reference_with_attributes *key_ref_t;

static inline __attribute__((always_inline)) key_ref_t make_key_ref(const struct key *key,
         unsigned long possession)
{
 return (key_ref_t) ((unsigned long) key | possession);
}

static inline __attribute__((always_inline)) struct key *key_ref_to_ptr(const key_ref_t key_ref)
{
 return (struct key *) ((unsigned long) key_ref & ~1UL);
}

static inline __attribute__((always_inline)) unsigned long is_key_possessed(const key_ref_t key_ref)
{
 return (unsigned long) key_ref & 1UL;
}
# 123 "include/linux/key.h"
struct key {
 atomic_t usage;
 key_serial_t serial;
 struct rb_node serial_node;
 struct key_type *type;
 struct rw_semaphore sem;
 struct key_user *user;
 void *security;
 time_t expiry;
 uid_t uid;
 gid_t gid;
 key_perm_t perm;
 unsigned short quotalen;
 unsigned short datalen;
# 147 "include/linux/key.h"
 unsigned long flags;
# 160 "include/linux/key.h"
 char *description;




 union {
  struct list_head link;
  unsigned long x[2];
  void *p[2];
 } type_data;





 union {
  unsigned long value;
  void *data;
  struct keyring_list *subscriptions;
 } payload;
};

extern struct key *key_alloc(struct key_type *type,
        const char *desc,
        uid_t uid, gid_t gid,
        const struct cred *cred,
        key_perm_t perm,
        unsigned long flags);






extern void key_revoke(struct key *key);
extern void key_put(struct key *key);

static inline __attribute__((always_inline)) struct key *key_get(struct key *key)
{
 if (key)
  atomic_inc(&key->usage);
 return key;
}

static inline __attribute__((always_inline)) void key_ref_put(key_ref_t key_ref)
{
 key_put(key_ref_to_ptr(key_ref));
}

extern struct key *request_key(struct key_type *type,
          const char *description,
          const char *callout_info);

extern struct key *request_key_with_auxdata(struct key_type *type,
         const char *description,
         const void *callout_info,
         size_t callout_len,
         void *aux);

extern struct key *request_key_async(struct key_type *type,
         const char *description,
         const void *callout_info,
         size_t callout_len);

extern struct key *request_key_async_with_auxdata(struct key_type *type,
        const char *description,
        const void *callout_info,
        size_t callout_len,
        void *aux);

extern int wait_for_key_construction(struct key *key, bool intr);

extern int key_validate(struct key *key);

extern key_ref_t key_create_or_update(key_ref_t keyring,
          const char *type,
          const char *description,
          const void *payload,
          size_t plen,
          key_perm_t perm,
          unsigned long flags);

extern int key_update(key_ref_t key,
        const void *payload,
        size_t plen);

extern int key_link(struct key *keyring,
      struct key *key);

extern int key_unlink(struct key *keyring,
        struct key *key);

extern struct key *keyring_alloc(const char *description, uid_t uid, gid_t gid,
     const struct cred *cred,
     unsigned long flags,
     struct key *dest);

extern int keyring_clear(struct key *keyring);

extern key_ref_t keyring_search(key_ref_t keyring,
    struct key_type *type,
    const char *description);

extern int keyring_add_key(struct key *keyring,
      struct key *key);

extern struct key *key_lookup(key_serial_t id);

static inline __attribute__((always_inline)) key_serial_t key_serial(struct key *key)
{
 return key ? key->serial : 0;
}


extern ctl_table key_sysctls[];





extern int install_thread_keyring_to_cred(struct cred *cred);
extern void key_fsuid_changed(struct task_struct *tsk);
extern void key_fsgid_changed(struct task_struct *tsk);
extern void key_init(void);
# 17 "include/linux/cred.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 18 "include/linux/cred.h" 2

struct user_struct;
struct cred;
struct inode;







struct group_info {
 atomic_t usage;
 int ngroups;
 int nblocks;
 gid_t small_block[32];
 gid_t *blocks[0];
};
# 46 "include/linux/cred.h"
static inline __attribute__((always_inline)) struct group_info *get_group_info(struct group_info *gi)
{
 atomic_inc(&gi->usage);
 return gi;
}
# 62 "include/linux/cred.h"
extern struct group_info *groups_alloc(int);
extern struct group_info init_groups;
extern void groups_free(struct group_info *);
extern int set_current_groups(struct group_info *);
extern int set_groups(struct cred *, struct group_info *);
extern int groups_search(const struct group_info *, gid_t);





extern int in_group_p(gid_t);
extern int in_egroup_p(gid_t);






struct thread_group_cred {
 atomic_t usage;
 pid_t tgid;
 spinlock_t lock;
 struct key *session_keyring;
 struct key *process_keyring;
 struct rcu_head rcu;
};
# 114 "include/linux/cred.h"
struct cred {
 atomic_t usage;
 uid_t uid;
 gid_t gid;
 uid_t suid;
 gid_t sgid;
 uid_t euid;
 gid_t egid;
 uid_t fsuid;
 gid_t fsgid;
 unsigned securebits;
 kernel_cap_t cap_inheritable;
 kernel_cap_t cap_permitted;
 kernel_cap_t cap_effective;
 kernel_cap_t cap_bset;

 unsigned char jit_keyring;

 struct key *thread_keyring;
 struct key *request_key_auth;
 struct thread_group_cred *tgcred;


 void *security;

 struct user_struct *user;
 struct group_info *group_info;
 struct rcu_head rcu;
};

extern void __put_cred(struct cred *);
extern int copy_creds(struct task_struct *, unsigned long);
extern struct cred *prepare_creds(void);
extern struct cred *prepare_exec_creds(void);
extern struct cred *prepare_usermodehelper_creds(void);
extern int commit_creds(struct cred *);
extern void abort_creds(struct cred *);
extern const struct cred *override_creds(const struct cred *);
extern void revert_creds(const struct cred *);
extern struct cred *prepare_kernel_cred(struct task_struct *);
extern int change_create_files_as(struct cred *, struct inode *);
extern int set_security_override(struct cred *, u32);
extern int set_security_override_from_ctx(struct cred *, const char *);
extern int set_create_files_as(struct cred *, struct inode *);
extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) cred_init(void);
# 167 "include/linux/cred.h"
static inline __attribute__((always_inline)) struct cred *get_new_cred(struct cred *cred)
{
 atomic_inc(&cred->usage);
 return cred;
}
# 186 "include/linux/cred.h"
static inline __attribute__((always_inline)) const struct cred *get_cred(const struct cred *cred)
{
 return get_new_cred((struct cred *) cred);
}
# 202 "include/linux/cred.h"
static inline __attribute__((always_inline)) void put_cred(const struct cred *_cred)
{
 struct cred *cred = (struct cred *) _cred;

 do { if (__builtin_expect(!!(((&(cred)->usage)->counter) <= 0), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/cred.h"), "i" (206), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (atomic_dec_and_test(&(cred)->usage))
  __put_cred(cred);
}
# 91 "include/linux/sched.h" 2



struct mem_cgroup;
struct exec_domain;
struct futex_pi_state;
struct robust_list_head;
struct bio;
struct bts_tracer;
# 117 "include/linux/sched.h"
extern unsigned long avenrun[];
# 131 "include/linux/sched.h"
extern unsigned long total_forks;
extern int nr_threads;
extern __typeof__(unsigned long) per_cpu__process_counts;
extern int nr_processes(void);
extern unsigned long nr_running(void);
extern unsigned long nr_uninterruptible(void);
extern unsigned long nr_active(void);
extern unsigned long nr_iowait(void);

struct seq_file;
struct cfs_rq;
struct task_group;

extern void proc_sched_show_task(struct task_struct *p, struct seq_file *m);
extern void proc_sched_set_task(struct task_struct *p);
extern void
print_cfs_rq(struct seq_file *m, int cpu, struct cfs_rq *cfs_rq);
# 162 "include/linux/sched.h"
extern unsigned long long time_sync_thresh;
# 239 "include/linux/sched.h"
extern rwlock_t tasklist_lock;
extern spinlock_t mmlist_lock;

struct task_struct;

extern void sched_init(void);
extern void sched_init_smp(void);
extern void schedule_tail(struct task_struct *prev);
extern void init_idle(struct task_struct *idle, int cpu);
extern void init_idle_bootup_task(struct task_struct *idle);

extern int runqueue_is_locked(void);
extern void task_rq_unlock_wait(struct task_struct *p);

extern cpumask_var_t nohz_cpu_mask;



static inline __attribute__((always_inline)) int select_nohz_load_balancer(int cpu)
{
 return 0;
}





extern void show_state_filter(unsigned long state_filter);

static inline __attribute__((always_inline)) void show_state(void)
{
 show_state_filter(0);
}

extern void show_regs(struct pt_regs *);






extern void show_stack(struct task_struct *task, unsigned long *sp);

void io_schedule(void);
long io_schedule_timeout(long timeout);

extern void cpu_init (void);
extern void trap_init(void);
extern void update_process_times(int user);
extern void scheduler_tick(void);

extern void sched_show_task(struct task_struct *p);


extern void softlockup_tick(void);
extern void touch_softlockup_watchdog(void);
extern void touch_all_softlockup_watchdogs(void);
extern int proc_dosoftlockup_thresh(struct ctl_table *table, int write,
        struct file *filp, void __attribute__((noderef, address_space(1))) *buffer,
        size_t *lenp, loff_t *ppos);
extern unsigned int softlockup_panic;
extern unsigned long sysctl_hung_task_check_count;
extern unsigned long sysctl_hung_task_timeout_secs;
extern unsigned long sysctl_hung_task_warnings;
extern int softlockup_thresh;
# 324 "include/linux/sched.h"
extern char __sched_text_start[], __sched_text_end[];


extern int in_sched_functions(unsigned long addr);


extern signed long schedule_timeout(signed long timeout);
extern signed long schedule_timeout_interruptible(signed long timeout);
extern signed long schedule_timeout_killable(signed long timeout);
extern signed long schedule_timeout_uninterruptible(signed long timeout);
 void schedule(void);

struct nsproxy;
struct user_namespace;




extern int sysctl_max_map_count;

# 1 "include/linux/aio.h" 1





# 1 "include/linux/aio_abi.h" 1
# 33 "include/linux/aio_abi.h"
typedef unsigned long aio_context_t;

enum {
 IOCB_CMD_PREAD = 0,
 IOCB_CMD_PWRITE = 1,
 IOCB_CMD_FSYNC = 2,
 IOCB_CMD_FDSYNC = 3,




 IOCB_CMD_NOOP = 6,
 IOCB_CMD_PREADV = 7,
 IOCB_CMD_PWRITEV = 8,
};
# 58 "include/linux/aio_abi.h"
struct io_event {
 __u64 data;
 __u64 obj;
 __s64 res;
 __s64 res2;
};
# 79 "include/linux/aio_abi.h"
struct iocb {

 __u64 aio_data;
 __u32 aio_key, aio_reserved1;



 __u16 aio_lio_opcode;
 __s16 aio_reqprio;
 __u32 aio_fildes;

 __u64 aio_buf;
 __u64 aio_nbytes;
 __s64 aio_offset;


 __u64 aio_reserved2;


 __u32 aio_flags;





 __u32 aio_resfd;
};
# 7 "include/linux/aio.h" 2
# 1 "include/linux/uio.h" 1
# 16 "include/linux/uio.h"
struct iovec
{
 void __attribute__((noderef, address_space(1))) *iov_base;
 __kernel_size_t iov_len;
};



struct kvec {
 void *iov_base;
 size_t iov_len;
};
# 45 "include/linux/uio.h"
static inline __attribute__((always_inline)) size_t iov_length(const struct iovec *iov, unsigned long nr_segs)
{
 unsigned long seg;
 size_t ret = 0;

 for (seg = 0; seg < nr_segs; seg++)
  ret += iov[seg].iov_len;
 return ret;
}

unsigned long iov_shorten(struct iovec *iov, unsigned long nr_segs, size_t to);
# 8 "include/linux/aio.h" 2


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 11 "include/linux/aio.h" 2




struct kioctx;
# 87 "include/linux/aio.h"
struct kiocb {
 struct list_head ki_run_list;
 unsigned long ki_flags;
 int ki_users;
 unsigned ki_key;

 struct file *ki_filp;
 struct kioctx *ki_ctx;
 int (*ki_cancel)(struct kiocb *, struct io_event *);
 ssize_t (*ki_retry)(struct kiocb *);
 void (*ki_dtor)(struct kiocb *);

 union {
  void __attribute__((noderef, address_space(1))) *user;
  struct task_struct *tsk;
 } ki_obj;

 __u64 ki_user_data;
 wait_queue_t ki_wait;
 loff_t ki_pos;

 void *private;

 unsigned short ki_opcode;
 size_t ki_nbytes;
 char __attribute__((noderef, address_space(1))) *ki_buf;
 size_t ki_left;
 struct iovec ki_inline_vec;
  struct iovec *ki_iovec;
  unsigned long ki_nr_segs;
  unsigned long ki_cur_seg;

 struct list_head ki_list;






 struct file *ki_eventfd;
};
# 149 "include/linux/aio.h"
struct aio_ring {
 unsigned id;
 unsigned nr;
 unsigned head;
 unsigned tail;

 unsigned magic;
 unsigned compat_features;
 unsigned incompat_features;
 unsigned header_length;


 struct io_event io_events[0];
};




struct aio_ring_info {
 unsigned long mmap_base;
 unsigned long mmap_size;

 struct page **ring_pages;
 spinlock_t ring_lock;
 long nr_pages;

 unsigned nr, tail;

 struct page *internal_pages[8];
};

struct kioctx {
 atomic_t users;
 int dead;
 struct mm_struct *mm;


 unsigned long user_id;
 struct hlist_node list;

 wait_queue_head_t wait;

 spinlock_t ctx_lock;

 int reqs_active;
 struct list_head active_reqs;
 struct list_head run_list;


 unsigned max_reqs;

 struct aio_ring_info ring_info;

 struct delayed_work wq;

 struct rcu_head rcu_head;
};


extern unsigned aio_max_size;


extern ssize_t wait_on_sync_kiocb(struct kiocb *iocb);
extern int aio_put_req(struct kiocb *iocb);
extern void kick_iocb(struct kiocb *iocb);
extern int aio_complete(struct kiocb *iocb, long res, long res2);
struct mm_struct;
extern void exit_aio(struct mm_struct *mm);
# 230 "include/linux/aio.h"
static inline __attribute__((always_inline)) struct kiocb *list_kiocb(struct list_head *h)
{
 return ({ const typeof( ((struct kiocb *)0)->ki_list ) *__mptr = (h); (struct kiocb *)( (char *)__mptr - __builtin_offsetof(struct kiocb,ki_list) );});
}


extern unsigned long aio_nr;
extern unsigned long aio_max_nr;
# 345 "include/linux/sched.h" 2

extern unsigned long
arch_get_unmapped_area(struct file *, unsigned long, unsigned long,
         unsigned long, unsigned long);
extern unsigned long
arch_get_unmapped_area_topdown(struct file *filp, unsigned long addr,
     unsigned long len, unsigned long pgoff,
     unsigned long flags);
extern void arch_unmap_area(struct mm_struct *, unsigned long);
extern void arch_unmap_area_topdown(struct mm_struct *, unsigned long);
# 395 "include/linux/sched.h"
extern void set_dumpable(struct mm_struct *mm, int value);
extern int get_dumpable(struct mm_struct *mm);
# 426 "include/linux/sched.h"
struct sighand_struct {
 atomic_t count;
 struct k_sigaction action[64];
 spinlock_t siglock;
 wait_queue_head_t signalfd_wqh;
};

struct pacct_struct {
 int ac_flag;
 long ac_exitcode;
 unsigned long ac_mem;
 cputime_t ac_utime, ac_stime;
 unsigned long ac_minflt, ac_majflt;
};
# 452 "include/linux/sched.h"
struct task_cputime {
 cputime_t utime;
 cputime_t stime;
 unsigned long long sum_exec_runtime;
};
# 479 "include/linux/sched.h"
struct thread_group_cputimer {
 struct task_cputime cputime;
 int running;
 spinlock_t lock;
};
# 492 "include/linux/sched.h"
struct signal_struct {
 atomic_t count;
 atomic_t live;

 wait_queue_head_t wait_chldexit;


 struct task_struct *curr_target;


 struct sigpending shared_pending;


 int group_exit_code;





 int notify_count;
 struct task_struct *group_exit_task;


 int group_stop_count;
 unsigned int flags;


 struct list_head posix_timers;


 struct hrtimer real_timer;
 struct pid *leader_pid;
 ktime_t it_real_incr;


 cputime_t it_prof_expires, it_virt_expires;
 cputime_t it_prof_incr, it_virt_incr;





 struct thread_group_cputimer cputimer;


 struct task_cputime cputime_expires;

 struct list_head cpu_timers[3];
# 548 "include/linux/sched.h"
 union {
  pid_t pgrp __attribute__((deprecated));
  pid_t __pgrp;
 };

 struct pid *tty_old_pgrp;

 union {
  pid_t session __attribute__((deprecated));
  pid_t __session;
 };


 int leader;

 struct tty_struct *tty;







 cputime_t utime, stime, cutime, cstime;
 cputime_t gtime;
 cputime_t cgtime;
 unsigned long nvcsw, nivcsw, cnvcsw, cnivcsw;
 unsigned long min_flt, maj_flt, cmin_flt, cmaj_flt;
 unsigned long inblock, oublock, cinblock, coublock;
 struct task_io_accounting ioac;







 unsigned long long sum_sched_runtime;
# 596 "include/linux/sched.h"
 struct rlimit rlim[16];


 struct pacct_struct pacct;


 struct taskstats *stats;


 unsigned audit_tty;
 struct tty_audit_buf *tty_audit_buf;

};
# 632 "include/linux/sched.h"
static inline __attribute__((always_inline)) int signal_group_exit(const struct signal_struct *sig)
{
 return (sig->flags & 0x00000008) ||
  (sig->group_exit_task != ((void *)0));
}




struct user_struct {
 atomic_t __count;
 atomic_t processes;
 atomic_t files;
 atomic_t sigpending;

 atomic_t inotify_watches;
 atomic_t inotify_devs;


 atomic_t epoll_watches;



 unsigned long mq_bytes;

 unsigned long locked_shm;


 struct key *uid_keyring;
 struct key *session_keyring;



 struct hlist_node uidhash_node;
 uid_t uid;
 struct user_namespace *user_ns;
# 676 "include/linux/sched.h"
};

extern int uids_sysfs_init(void);

extern struct user_struct *find_user(uid_t);

extern struct user_struct root_user;



struct backing_dev_info;
struct reclaim_state;


struct sched_info {

 unsigned long pcount;
 unsigned long long run_delay;


 unsigned long long last_arrival,
      last_queued;


 unsigned int bkl_count;

};



struct task_delay_info {
 spinlock_t lock;
 unsigned int flags;
# 725 "include/linux/sched.h"
 struct timespec blkio_start, blkio_end;
 u64 blkio_delay;
 u64 swapin_delay;
 u32 blkio_count;

 u32 swapin_count;


 struct timespec freepages_start, freepages_end;
 u64 freepages_delay;
 u32 freepages_count;
};


static inline __attribute__((always_inline)) int sched_info_on(void)
{

 return 1;






}

enum cpu_idle_type {
 CPU_IDLE,
 CPU_NOT_IDLE,
 CPU_NEWLY_IDLE,
 CPU_MAX_IDLE_TYPES
};
# 784 "include/linux/sched.h"
enum powersavings_balance_level {
 POWERSAVINGS_BALANCE_NONE = 0,
 POWERSAVINGS_BALANCE_BASIC,


 POWERSAVINGS_BALANCE_WAKEUP,


 MAX_POWERSAVINGS_BALANCE_LEVELS
};

extern int sched_mc_power_savings, sched_smt_power_savings;

static inline __attribute__((always_inline)) int sd_balance_for_mc_power(void)
{
 if (sched_smt_power_savings)
  return 256;

 return 0;
}

static inline __attribute__((always_inline)) int sd_balance_for_package_power(void)
{
 if (sched_mc_power_savings | sched_smt_power_savings)
  return 256;

 return 0;
}







static inline __attribute__((always_inline)) int sd_power_saving_flags(void)
{
 if (sched_mc_power_savings | sched_smt_power_savings)
  return 2;

 return 0;
}

struct sched_group {
 struct sched_group *next;






 unsigned int __cpu_power;




 u32 reciprocal_cpu_power;

 unsigned long cpumask[];
};

static inline __attribute__((always_inline)) struct cpumask *sched_group_cpus(struct sched_group *sg)
{
 return ((struct cpumask *)(1 ? (sg->cpumask) : (void *)sizeof(__check_is_bitmap(sg->cpumask))));
}

enum sched_domain_level {
 SD_LV_NONE = 0,
 SD_LV_SIBLING,
 SD_LV_MC,
 SD_LV_CPU,
 SD_LV_NODE,
 SD_LV_ALLNODES,
 SD_LV_MAX
};

struct sched_domain_attr {
 int relax_domain_level;
};





struct sched_domain {

 struct sched_domain *parent;
 struct sched_domain *child;
 struct sched_group *groups;
 unsigned long min_interval;
 unsigned long max_interval;
 unsigned int busy_factor;
 unsigned int imbalance_pct;
 unsigned int cache_nice_tries;
 unsigned int busy_idx;
 unsigned int idle_idx;
 unsigned int newidle_idx;
 unsigned int wake_idx;
 unsigned int forkexec_idx;
 int flags;
 enum sched_domain_level level;


 unsigned long last_balance;
 unsigned int balance_interval;
 unsigned int nr_balance_failed;

 u64 last_update;



 unsigned int lb_count[CPU_MAX_IDLE_TYPES];
 unsigned int lb_failed[CPU_MAX_IDLE_TYPES];
 unsigned int lb_balanced[CPU_MAX_IDLE_TYPES];
 unsigned int lb_imbalance[CPU_MAX_IDLE_TYPES];
 unsigned int lb_gained[CPU_MAX_IDLE_TYPES];
 unsigned int lb_hot_gained[CPU_MAX_IDLE_TYPES];
 unsigned int lb_nobusyg[CPU_MAX_IDLE_TYPES];
 unsigned int lb_nobusyq[CPU_MAX_IDLE_TYPES];


 unsigned int alb_count;
 unsigned int alb_failed;
 unsigned int alb_pushed;


 unsigned int sbe_count;
 unsigned int sbe_balanced;
 unsigned int sbe_pushed;


 unsigned int sbf_count;
 unsigned int sbf_balanced;
 unsigned int sbf_pushed;


 unsigned int ttwu_wake_remote;
 unsigned int ttwu_move_affine;
 unsigned int ttwu_move_balance;


 char *name;



 unsigned long span[];
};

static inline __attribute__((always_inline)) struct cpumask *sched_domain_span(struct sched_domain *sd)
{
 return ((struct cpumask *)(1 ? (sd->span) : (void *)sizeof(__check_is_bitmap(sd->span))));
}

extern void partition_sched_domains(int ndoms_new, struct cpumask *doms_new,
        struct sched_domain_attr *dattr_new);


static inline __attribute__((always_inline)) int test_sd_parent(struct sched_domain *sd, int flag)
{
 if (sd->parent && (sd->parent->flags & flag))
  return 1;

 return 0;
}
# 960 "include/linux/sched.h"
struct io_context;





static inline __attribute__((always_inline)) void prefetch_stack(struct task_struct *t) { }


struct audit_context;
struct mempolicy;
struct pipe_inode_info;
struct uts_namespace;

struct rq;
struct sched_domain;

struct sched_class {
 const struct sched_class *next;

 void (*enqueue_task) (struct rq *rq, struct task_struct *p, int wakeup);
 void (*dequeue_task) (struct rq *rq, struct task_struct *p, int sleep);
 void (*yield_task) (struct rq *rq);

 void (*check_preempt_curr) (struct rq *rq, struct task_struct *p, int sync);

 struct task_struct * (*pick_next_task) (struct rq *rq);
 void (*put_prev_task) (struct rq *rq, struct task_struct *p);


 int (*select_task_rq)(struct task_struct *p, int sync);

 unsigned long (*load_balance) (struct rq *this_rq, int this_cpu,
   struct rq *busiest, unsigned long max_load_move,
   struct sched_domain *sd, enum cpu_idle_type idle,
   int *all_pinned, int *this_best_prio);

 int (*move_one_task) (struct rq *this_rq, int this_cpu,
         struct rq *busiest, struct sched_domain *sd,
         enum cpu_idle_type idle);
 void (*pre_schedule) (struct rq *this_rq, struct task_struct *task);
 void (*post_schedule) (struct rq *this_rq);
 void (*task_wake_up) (struct rq *this_rq, struct task_struct *task);

 void (*set_cpus_allowed)(struct task_struct *p,
     const struct cpumask *newmask);

 void (*rq_online)(struct rq *rq);
 void (*rq_offline)(struct rq *rq);


 void (*set_curr_task) (struct rq *rq);
 void (*task_tick) (struct rq *rq, struct task_struct *p, int queued);
 void (*task_new) (struct rq *rq, struct task_struct *p);

 void (*switched_from) (struct rq *this_rq, struct task_struct *task,
          int running);
 void (*switched_to) (struct rq *this_rq, struct task_struct *task,
        int running);
 void (*prio_changed) (struct rq *this_rq, struct task_struct *task,
        int oldprio, int running);




};

struct load_weight {
 unsigned long weight, inv_weight;
};
# 1041 "include/linux/sched.h"
struct sched_entity {
 struct load_weight load;
 struct rb_node run_node;
 struct list_head group_node;
 unsigned int on_rq;

 u64 exec_start;
 u64 sum_exec_runtime;
 u64 vruntime;
 u64 prev_sum_exec_runtime;

 u64 last_wakeup;
 u64 avg_overlap;


 u64 wait_start;
 u64 wait_max;
 u64 wait_count;
 u64 wait_sum;

 u64 sleep_start;
 u64 sleep_max;
 s64 sum_sleep_runtime;

 u64 block_start;
 u64 block_max;
 u64 exec_max;
 u64 slice_max;

 u64 nr_migrations;
 u64 nr_migrations_cold;
 u64 nr_failed_migrations_affine;
 u64 nr_failed_migrations_running;
 u64 nr_failed_migrations_hot;
 u64 nr_forced_migrations;
 u64 nr_forced2_migrations;

 u64 nr_wakeups;
 u64 nr_wakeups_sync;
 u64 nr_wakeups_migrate;
 u64 nr_wakeups_local;
 u64 nr_wakeups_remote;
 u64 nr_wakeups_affine;
 u64 nr_wakeups_affine_attempts;
 u64 nr_wakeups_passive;
 u64 nr_wakeups_idle;
# 1096 "include/linux/sched.h"
};

struct sched_rt_entity {
 struct list_head run_list;
 unsigned long timeout;
 unsigned int time_slice;
 int nr_cpus_allowed;

 struct sched_rt_entity *back;







};

struct task_struct {
 volatile long state;
    void *__attribute__((recursive)) stack;
 atomic_t usage;
 unsigned int flags;
 unsigned int ptrace;

 int lock_depth;







 int prio, static_prio, normal_prio;
 unsigned int rt_priority;
 const struct sched_class *sched_class;
 struct sched_entity se;
 struct sched_rt_entity rt;
# 1148 "include/linux/sched.h"
 unsigned char fpu_counter;
 s8 oomkilladj;

 unsigned int btrace_seq;


 unsigned int policy;
 cpumask_t cpus_allowed;







 struct sched_info sched_info;


 struct list_head tasks;

 struct mm_struct *mm, *active_mm;


 struct linux_binfmt *binfmt;
 int exit_state;
 int exit_code, exit_signal;
 int pdeath_signal;

 unsigned int personality;
 unsigned did_exec:1;
 pid_t pid;
 pid_t tgid;
# 1190 "include/linux/sched.h"
 struct task_struct *real_parent;
 struct task_struct *parent;



 struct list_head children;
 struct list_head sibling;
 struct task_struct *group_leader;






 struct list_head ptraced;
 struct list_head ptrace_entry;






 struct bts_tracer *bts;



 void *bts_buffer;
 size_t bts_size;



 struct pid_link pids[PIDTYPE_MAX];
 struct list_head thread_group;

 struct completion *vfork_done;
 int __attribute__((noderef, address_space(1))) *set_child_tid;
 int __attribute__((noderef, address_space(1))) *clear_child_tid;

 cputime_t utime, stime, utimescaled, stimescaled;
 cputime_t gtime;
 cputime_t prev_utime, prev_stime;
 unsigned long nvcsw, nivcsw;
 struct timespec start_time;
 struct timespec real_start_time;

 unsigned long min_flt, maj_flt;

 struct task_cputime cputime_expires;
 struct list_head cpu_timers[3];


 const struct cred *real_cred;

 const struct cred *cred;

 struct mutex cred_exec_mutex;

 char comm[16];




 int link_count, total_link_count;


 struct sysv_sem sysvsem;



 unsigned long last_switch_timestamp;
 unsigned long last_switch_count;


 struct thread_struct thread;

 struct fs_struct *fs;

 struct files_struct *files;

 struct nsproxy *nsproxy;

 struct signal_struct *signal;
 struct sighand_struct *sighand;

 sigset_t blocked, real_blocked;
 sigset_t saved_sigmask;
 struct sigpending pending;

 unsigned long sas_ss_sp;
 size_t sas_ss_size;
 int (*notifier)(void *priv);
 void *notifier_data;
 sigset_t *notifier_mask;
 struct audit_context *audit_context;

 uid_t loginuid;
 unsigned int sessionid;

 seccomp_t seccomp;


    u32 parent_exec_id;
    u32 self_exec_id;

 spinlock_t alloc_lock;


 spinlock_t pi_lock;



 struct plist_head pi_waiters;

 struct rt_mutex_waiter *pi_blocked_on;




 struct mutex_waiter *blocked_on;
# 1327 "include/linux/sched.h"
 u64 curr_chain_key;
 int lockdep_depth;
 unsigned int lockdep_recursion;
 struct held_lock held_locks[48UL];



 void *journal_info;


 struct bio *bio_list, **bio_tail;


 struct reclaim_state *reclaim_state;

 struct backing_dev_info *backing_dev_info;

 struct io_context *io_context;

 unsigned long ptrace_message;
 siginfo_t *last_siginfo;
 struct task_io_accounting ioac;
# 1366 "include/linux/sched.h"
 struct robust_list_head __attribute__((noderef, address_space(1))) *robust_list;

 struct compat_robust_list_head __attribute__((noderef, address_space(1))) *compat_robust_list;

 struct list_head pi_state_list;
 struct futex_pi_state *pi_state_cache;


 struct mempolicy *mempolicy;
 short il_next;

 atomic_t fs_excl;
 struct rcu_head rcu;




 struct pipe_inode_info *splice_pipe;

 struct task_delay_info *delays;


 int make_it_fail;

 struct prop_local_single dirties;

 int latency_record_count;
 struct latency_record latency_record[32];





 unsigned long timer_slack_ns;
 unsigned long default_timer_slack_ns;

 struct list_head *scm_work_list;
# 1418 "include/linux/sched.h"
 unsigned long trace;

};
# 1444 "include/linux/sched.h"
static inline __attribute__((always_inline)) int rt_prio(int prio)
{
 if (__builtin_expect(!!(prio < 100), 0))
  return 1;
 return 0;
}

static inline __attribute__((always_inline)) int rt_task(struct task_struct *p)
{
 return rt_prio(p->prio);
}

static inline __attribute__((always_inline)) void set_task_session(struct task_struct *tsk, pid_t session)
{
 tsk->signal->__session = session;
}

static inline __attribute__((always_inline)) void set_task_pgrp(struct task_struct *tsk, pid_t pgrp)
{
 tsk->signal->__pgrp = pgrp;
}

static inline __attribute__((always_inline)) struct pid *task_pid(struct task_struct *task)
{
 return task->pids[PIDTYPE_PID].pid;
}

static inline __attribute__((always_inline)) struct pid *task_tgid(struct task_struct *task)
{
 return task->group_leader->pids[PIDTYPE_PID].pid;
}

static inline __attribute__((always_inline)) struct pid *task_pgrp(struct task_struct *task)
{
 return task->group_leader->pids[PIDTYPE_PGID].pid;
}

static inline __attribute__((always_inline)) struct pid *task_session(struct task_struct *task)
{
 return task->group_leader->pids[PIDTYPE_SID].pid;
}

struct pid_namespace;
# 1502 "include/linux/sched.h"
static inline __attribute__((always_inline)) pid_t task_pid_nr(struct task_struct *tsk)
{
 return tsk->pid;
}

pid_t task_pid_nr_ns(struct task_struct *tsk, struct pid_namespace *ns);

static inline __attribute__((always_inline)) pid_t task_pid_vnr(struct task_struct *tsk)
{
 return pid_vnr(task_pid(tsk));
}


static inline __attribute__((always_inline)) pid_t task_tgid_nr(struct task_struct *tsk)
{
 return tsk->tgid;
}

pid_t task_tgid_nr_ns(struct task_struct *tsk, struct pid_namespace *ns);

static inline __attribute__((always_inline)) pid_t task_tgid_vnr(struct task_struct *tsk)
{
 return pid_vnr(task_tgid(tsk));
}


static inline __attribute__((always_inline)) pid_t task_pgrp_nr(struct task_struct *tsk)
{
 return tsk->signal->__pgrp;
}

pid_t task_pgrp_nr_ns(struct task_struct *tsk, struct pid_namespace *ns);

static inline __attribute__((always_inline)) pid_t task_pgrp_vnr(struct task_struct *tsk)
{
 return pid_vnr(task_pgrp(tsk));
}


static inline __attribute__((always_inline)) pid_t task_session_nr(struct task_struct *tsk)
{
 return tsk->signal->__session;
}

pid_t task_session_nr_ns(struct task_struct *tsk, struct pid_namespace *ns);

static inline __attribute__((always_inline)) pid_t task_session_vnr(struct task_struct *tsk)
{
 return pid_vnr(task_session(tsk));
}
# 1562 "include/linux/sched.h"
static inline __attribute__((always_inline)) int pid_alive(struct task_struct *p)
{
 return p->pids[PIDTYPE_PID].pid != ((void *)0);
}







static inline __attribute__((always_inline)) int is_global_init(struct task_struct *tsk)
{
 return tsk->pid == 1;
}





extern int is_container_init(struct task_struct *tsk);

extern struct pid *cad_pid;

extern void free_task(struct task_struct *tsk);


extern void __put_task_struct(struct task_struct *t);

static inline __attribute__((always_inline)) void put_task_struct(struct task_struct *t)
{
 if (atomic_dec_and_test(&t->usage))
  __put_task_struct(t);
}

extern cputime_t task_utime(struct task_struct *p);
extern cputime_t task_stime(struct task_struct *p);
extern cputime_t task_gtime(struct task_struct *p);
# 1660 "include/linux/sched.h"
extern int set_cpus_allowed_ptr(struct task_struct *p,
    const struct cpumask *new_mask);
# 1671 "include/linux/sched.h"
static inline __attribute__((always_inline)) int set_cpus_allowed(struct task_struct *p, cpumask_t new_mask)
{
 return set_cpus_allowed_ptr(p, &new_mask);
}

extern unsigned long long sched_clock(void);

extern void sched_clock_init(void);
extern u64 sched_clock_cpu(int cpu);
# 1694 "include/linux/sched.h"
extern void sched_clock_tick(void);
extern void sched_clock_idle_sleep_event(void);
extern void sched_clock_idle_wakeup_event(u64 delta_ns);






extern unsigned long long cpu_clock(int cpu);

extern unsigned long long
task_sched_runtime(struct task_struct *task);
extern unsigned long long thread_group_sched_runtime(struct task_struct *task);



extern void sched_exec(void);




extern void sched_clock_idle_sleep_event(void);
extern void sched_clock_idle_wakeup_event(u64 delta_ns);


extern void idle_task_exit(void);




extern void sched_idle_next(void);




static inline __attribute__((always_inline)) void wake_up_idle_cpu(int cpu) { }


extern unsigned int sysctl_sched_latency;
extern unsigned int sysctl_sched_min_granularity;
extern unsigned int sysctl_sched_wakeup_granularity;
extern unsigned int sysctl_sched_shares_ratelimit;
extern unsigned int sysctl_sched_shares_thresh;

extern unsigned int sysctl_sched_child_runs_first;
extern unsigned int sysctl_sched_features;
extern unsigned int sysctl_sched_migration_cost;
extern unsigned int sysctl_sched_nr_migrate;

int sched_nr_latency_handler(struct ctl_table *table, int write,
  struct file *file, void __attribute__((noderef, address_space(1))) *buffer, size_t *length,
  loff_t *ppos);

extern unsigned int sysctl_sched_rt_period;
extern int sysctl_sched_rt_runtime;

int sched_rt_handler(struct ctl_table *table, int write,
  struct file *filp, void __attribute__((noderef, address_space(1))) *buffer, size_t *lenp,
  loff_t *ppos);

extern unsigned int sysctl_sched_compat_yield;


extern int rt_mutex_getprio(struct task_struct *p);
extern void rt_mutex_setprio(struct task_struct *p, int prio);
extern void rt_mutex_adjust_pi(struct task_struct *p);
# 1769 "include/linux/sched.h"
extern void set_user_nice(struct task_struct *p, long nice);
extern int task_prio(const struct task_struct *p);
extern int task_nice(const struct task_struct *p);
extern int can_nice(const struct task_struct *p, const int nice);
extern int task_curr(const struct task_struct *p);
extern int idle_cpu(int cpu);
extern int sched_setscheduler(struct task_struct *, int, struct sched_param *);
extern int sched_setscheduler_nocheck(struct task_struct *, int,
          struct sched_param *);
extern struct task_struct *idle_task(int cpu);
extern struct task_struct *curr_task(int cpu);
extern void set_curr_task(int cpu, struct task_struct *p);

void yield(void);




extern struct exec_domain default_exec_domain;

union thread_union {
 struct thread_info thread_info;
 unsigned long stack[(((1UL) << 12) << 1)/sizeof(long)];
};


static inline __attribute__((always_inline)) int kstack_end(void *addr)
{



 return !(((unsigned long)addr+sizeof(void*)-1) & ((((1UL) << 12) << 1)-sizeof(void*)));
}


extern union thread_union init_thread_union;
extern struct task_struct init_task;

extern struct mm_struct init_mm;

extern struct pid_namespace init_pid_ns;
# 1825 "include/linux/sched.h"
extern struct task_struct *find_task_by_pid_type_ns(int type, int pid,
  struct pid_namespace *ns);

extern struct task_struct *find_task_by_vpid(pid_t nr);
extern struct task_struct *find_task_by_pid_ns(pid_t nr,
  struct pid_namespace *ns);

extern void __set_special_pids(struct pid *pid);


extern struct user_struct * alloc_uid(struct user_namespace *, uid_t);
static inline __attribute__((always_inline)) struct user_struct *get_uid(struct user_struct *u)
{
 atomic_inc(&u->__count);
 return u;
}
extern void free_uid(struct user_struct *);
extern void release_uids(struct user_namespace *ns);



extern void do_timer(unsigned long ticks);

extern int wake_up_state(struct task_struct *tsk, unsigned int state);
extern int wake_up_process(struct task_struct *tsk);
extern void wake_up_new_task(struct task_struct *tsk,
    unsigned long clone_flags);

 extern void kick_process(struct task_struct *tsk);



extern void sched_fork(struct task_struct *p, int clone_flags);
extern void sched_dead(struct task_struct *p);

extern void proc_caches_init(void);
extern void flush_signals(struct task_struct *);
extern void ignore_signals(struct task_struct *);
extern void flush_signal_handlers(struct task_struct *, int force_default);
extern int dequeue_signal(struct task_struct *tsk, sigset_t *mask, siginfo_t *info);

static inline __attribute__((always_inline)) int dequeue_signal_lock(struct task_struct *tsk, sigset_t *mask, siginfo_t *info)
{
 unsigned long flags;
 int ret;

 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = _spin_lock_irqsave(&tsk->sighand->siglock); } while (0);
 ret = dequeue_signal(tsk, mask, info);
 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); _spin_unlock_irqrestore(&tsk->sighand->siglock, flags); } while (0);

 return ret;
}

extern void block_all_signals(int (*notifier)(void *priv), void *priv,
         sigset_t *mask);
extern void unblock_all_signals(void);
extern void release_task(struct task_struct * p);
extern int send_sig_info(int, struct siginfo *, struct task_struct *);
extern int force_sigsegv(int, struct task_struct *);
extern int force_sig_info(int, struct siginfo *, struct task_struct *);
extern int __kill_pgrp_info(int sig, struct siginfo *info, struct pid *pgrp);
extern int kill_pid_info(int sig, struct siginfo *info, struct pid *pid);
extern int kill_pid_info_as_uid(int, struct siginfo *, struct pid *, uid_t, uid_t, u32);
extern int kill_pgrp(struct pid *pid, int sig, int priv);
extern int kill_pid(struct pid *pid, int sig, int priv);
extern int kill_proc_info(int, struct siginfo *, pid_t);
extern int do_notify_parent(struct task_struct *, int);
extern void force_sig(int, struct task_struct *);
extern void force_sig_specific(int, struct task_struct *);
extern int send_sig(int, struct task_struct *, int);
extern void zap_other_threads(struct task_struct *p);
extern struct sigqueue *sigqueue_alloc(void);
extern void sigqueue_free(struct sigqueue *);
extern int send_sigqueue(struct sigqueue *, struct task_struct *, int group);
extern int do_sigaction(int, struct k_sigaction *, struct k_sigaction *);
extern int do_sigaltstack(const stack_t __attribute__((noderef, address_space(1))) *, stack_t __attribute__((noderef, address_space(1))) *, unsigned long);

static inline __attribute__((always_inline)) int kill_cad_pid(int sig, int priv)
{
 return kill_pid(cad_pid, sig, priv);
}






static inline __attribute__((always_inline)) int is_si_special(const struct siginfo *info)
{
 return info <= ((struct siginfo *) 2);
}



static inline __attribute__((always_inline)) int on_sig_stack(unsigned long sp)
{
 return (sp - get_current()->sas_ss_sp < get_current()->sas_ss_size);
}

static inline __attribute__((always_inline)) int sas_ss_flags(unsigned long sp)
{
 return (get_current()->sas_ss_size == 0 ? 2
  : on_sig_stack(sp) ? 1 : 0);
}




extern struct mm_struct * mm_alloc(void);


extern void __mmdrop(struct mm_struct *);
static inline __attribute__((always_inline)) void mmdrop(struct mm_struct * mm)
{
 if (__builtin_expect(!!(atomic_dec_and_test(&mm->mm_count)), 0))
  __mmdrop(mm);
}


extern void mmput(struct mm_struct *);

extern struct mm_struct *get_task_mm(struct task_struct *task);

extern void mm_release(struct task_struct *, struct mm_struct *);

extern struct mm_struct *dup_mm(struct task_struct *tsk);

extern int copy_thread(int, unsigned long, unsigned long, unsigned long, struct task_struct *, struct pt_regs *);
extern void flush_thread(void);
extern void exit_thread(void);

extern void exit_files(struct task_struct *);
extern void __cleanup_signal(struct signal_struct *);
extern void __cleanup_sighand(struct sighand_struct *);

extern void exit_itimers(struct signal_struct *);
extern void flush_itimer_signals(void);

extern void do_group_exit(int);

extern void daemonize(const char *, ...);
extern int allow_signal(int);
extern int disallow_signal(int);

extern int do_execve(char *, char __attribute__((noderef, address_space(1))) * __attribute__((noderef, address_space(1))) *, char __attribute__((noderef, address_space(1))) * __attribute__((noderef, address_space(1))) *, struct pt_regs *);
extern long do_fork(unsigned long, unsigned long, struct pt_regs *, unsigned long, int __attribute__((noderef, address_space(1))) *, int __attribute__((noderef, address_space(1))) *);
struct task_struct *fork_idle(int);

extern void set_task_comm(struct task_struct *tsk, char *from);
extern char *get_task_comm(char *to, struct task_struct *tsk);


extern unsigned long wait_task_inactive(struct task_struct *, long match_state);
# 1991 "include/linux/sched.h"
extern bool is_single_threaded(struct task_struct *);
# 2012 "include/linux/sched.h"
static inline __attribute__((always_inline)) int has_group_leader_pid(struct task_struct *p)
{
 return p->pid == p->tgid;
}

static inline __attribute__((always_inline))
int same_thread_group(struct task_struct *p1, struct task_struct *p2)
{
 return p1->tgid == p2->tgid;
}

static inline __attribute__((always_inline)) struct task_struct *next_thread(const struct task_struct *p)
{
 return ({ const typeof( ((struct task_struct *)0)->thread_group ) *__mptr = (({ typeof(p->thread_group.next) _________p1 = (*(volatile typeof(p->thread_group.next) *)&(p->thread_group.next)); do { } while (0); (_________p1); })); (struct task_struct *)( (char *)__mptr - __builtin_offsetof(struct task_struct,thread_group) );})
                                      ;
}

static inline __attribute__((always_inline)) int thread_group_empty(struct task_struct *p)
{
 return list_empty(&p->thread_group);
}
# 2047 "include/linux/sched.h"
static inline __attribute__((always_inline)) void task_lock(struct task_struct *p)
{
 _spin_lock(&p->alloc_lock);
}

static inline __attribute__((always_inline)) void task_unlock(struct task_struct *p)
{
 _spin_unlock(&p->alloc_lock);
}

extern struct sighand_struct *lock_task_sighand(struct task_struct *tsk,
       unsigned long *flags);

static inline __attribute__((always_inline)) void unlock_task_sighand(struct task_struct *tsk,
      unsigned long *flags)
{
 do { ({ unsigned long __dummy; typeof(*flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); _spin_unlock_irqrestore(&tsk->sighand->siglock, *flags); } while (0);
}






static inline __attribute__((always_inline)) void setup_thread_stack(struct task_struct *p, struct task_struct *org)
{
 *((struct thread_info *)(p)->stack) = *((struct thread_info *)(org)->stack);
 ((struct thread_info *)(p)->stack)->task = p;
}

static inline __attribute__((always_inline)) unsigned long *end_of_stack(struct task_struct *p)
{
 return (unsigned long *)(((struct thread_info *)(p)->stack) + 1);
}



static inline __attribute__((always_inline)) int object_is_on_stack(void *obj)
{
 void *stack = ((get_current())->stack);

 return (obj >= stack) && (obj < (stack + (((1UL) << 12) << 1)));
}

extern void thread_info_cache_init(void);




static inline __attribute__((always_inline)) void set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
 set_ti_thread_flag(((struct thread_info *)(tsk)->stack), flag);
}

static inline __attribute__((always_inline)) void clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
 clear_ti_thread_flag(((struct thread_info *)(tsk)->stack), flag);
}

static inline __attribute__((always_inline)) int test_and_set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
 return test_and_set_ti_thread_flag(((struct thread_info *)(tsk)->stack), flag);
}

static inline __attribute__((always_inline)) int test_and_clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
 return test_and_clear_ti_thread_flag(((struct thread_info *)(tsk)->stack), flag);
}

static inline __attribute__((always_inline)) int test_tsk_thread_flag(struct task_struct *tsk, int flag)
{
 return test_ti_thread_flag(((struct thread_info *)(tsk)->stack), flag);
}

static inline __attribute__((always_inline)) void set_tsk_need_resched(struct task_struct *tsk)
{
 set_tsk_thread_flag(tsk,3);
}

static inline __attribute__((always_inline)) void clear_tsk_need_resched(struct task_struct *tsk)
{
 clear_tsk_thread_flag(tsk,3);
}

static inline __attribute__((always_inline)) int test_tsk_need_resched(struct task_struct *tsk)
{
 return __builtin_expect(!!(test_tsk_thread_flag(tsk,3)), 0);
}

static inline __attribute__((always_inline)) int signal_pending(struct task_struct *p)
{
 return __builtin_expect(!!(test_tsk_thread_flag(p,2)), 0);
}

extern int __fatal_signal_pending(struct task_struct *p);

static inline __attribute__((always_inline)) int fatal_signal_pending(struct task_struct *p)
{
 return signal_pending(p) && __fatal_signal_pending(p);
}

static inline __attribute__((always_inline)) int signal_pending_state(long state, struct task_struct *p)
{
 if (!(state & (1 | 128)))
  return 0;
 if (!signal_pending(p))
  return 0;

 return (state & 1) || __fatal_signal_pending(p);
}

static inline __attribute__((always_inline)) int need_resched(void)
{
 return __builtin_expect(!!(test_ti_thread_flag(current_thread_info(), 3)), 0);
}
# 2170 "include/linux/sched.h"
extern int _cond_resched(void);






static inline __attribute__((always_inline)) int cond_resched(void)
{
 return _cond_resched();
}

extern int cond_resched_lock(spinlock_t * lock);
extern int cond_resched_softirq(void);
static inline __attribute__((always_inline)) int cond_resched_bkl(void)
{
 return _cond_resched();
}






static inline __attribute__((always_inline)) int spin_needbreak(spinlock_t *lock)
{



 return 0;

}




void thread_group_cputime(struct task_struct *tsk, struct task_cputime *times);
void thread_group_cputimer(struct task_struct *tsk, struct task_cputime *times);

static inline __attribute__((always_inline)) void thread_group_cputime_init(struct signal_struct *sig)
{
 sig->cputimer.cputime = (struct task_cputime) { .utime = (0UL), .stime = (0UL), .sum_exec_runtime = 0, };
 do { static struct lock_class_key __key; __spin_lock_init((&sig->cputimer.lock), "&sig->cputimer.lock", &__key); } while (0);
 sig->cputimer.running = 0;
}

static inline __attribute__((always_inline)) void thread_group_cputime_free(struct signal_struct *sig)
{
}







extern void recalc_sigpending_and_wake(struct task_struct *t);
extern void recalc_sigpending(void);

extern void signal_wake_up(struct task_struct *t, int resume_stopped);






static inline __attribute__((always_inline)) unsigned int task_cpu(const struct task_struct *p)
{
 return ((struct thread_info *)(p)->stack)->cpu;
}

extern void set_task_cpu(struct task_struct *p, unsigned int cpu);
# 2256 "include/linux/sched.h"
extern void arch_pick_mmap_layout(struct mm_struct *mm);


extern void
__trace_special(void *__tr, void *__data,
  unsigned long arg1, unsigned long arg2, unsigned long arg3);
# 2270 "include/linux/sched.h"
extern long sched_setaffinity(pid_t pid, const struct cpumask *new_mask);
extern long sched_getaffinity(pid_t pid, struct cpumask *mask);

extern void normalize_rt_tasks(void);
# 2301 "include/linux/sched.h"
extern int task_can_switch_user(struct user_struct *up,
     struct task_struct *tsk);
# 2325 "include/linux/sched.h"
static inline __attribute__((always_inline)) void add_rchar(struct task_struct *tsk, ssize_t amt)
{
}

static inline __attribute__((always_inline)) void add_wchar(struct task_struct *tsk, ssize_t amt)
{
}

static inline __attribute__((always_inline)) void inc_syscr(struct task_struct *tsk)
{
}

static inline __attribute__((always_inline)) void inc_syscw(struct task_struct *tsk)
{
}
# 2350 "include/linux/sched.h"
static inline __attribute__((always_inline)) void mm_update_next_owner(struct mm_struct *mm)
{
}

static inline __attribute__((always_inline)) void mm_init_owner(struct mm_struct *mm, struct task_struct *p)
{
}
# 6 "include/linux/smp_lock.h" 2



extern int __attribute__((section(".spinlock.text"))) __reacquire_kernel_lock(void);
extern void __attribute__((section(".spinlock.text"))) __release_kernel_lock(void);
# 20 "include/linux/smp_lock.h"
static inline __attribute__((always_inline)) int reacquire_kernel_lock(struct task_struct *task)
{
 if (__builtin_expect(!!(task->lock_depth >= 0), 0))
  return __reacquire_kernel_lock();
 return 0;
}

extern void __attribute__((section(".spinlock.text"))) lock_kernel(void) ;
extern void __attribute__((section(".spinlock.text"))) unlock_kernel(void) ;







static inline __attribute__((always_inline)) void cycle_kernel_lock(void)
{
 lock_kernel();
 unlock_kernel();
}
# 6 "include/linux/hardirq.h" 2

# 1 "include/linux/ftrace_irq.h" 1
# 9 "include/linux/ftrace_irq.h"
static inline __attribute__((always_inline)) void ftrace_nmi_enter(void) { }
static inline __attribute__((always_inline)) void ftrace_nmi_exit(void) { }
# 8 "include/linux/hardirq.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hardirq.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hardirq_64.h" 1




# 1 "include/linux/irq.h" 1
# 24 "include/linux/irq.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq.h" 1
# 13 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq.h"
static inline __attribute__((always_inline)) int irq_canonicalize(int irq)
{
 return ((irq == 2) ? 9 : irq);
}
# 36 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq.h"
extern void fixup_irqs(void);


extern unsigned int do_IRQ(struct pt_regs *regs);
extern void init_IRQ(void);
extern void native_init_IRQ(void);


extern unsigned long used_vectors[(((256) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];
extern int vector_used_by_percpu_irq(unsigned int vector);
# 25 "include/linux/irq.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq_regs.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq_regs_64.h" 1
# 1 "include/asm-generic/irq_regs.h" 1
# 21 "include/asm-generic/irq_regs.h"
extern __typeof__(struct pt_regs *) per_cpu____irq_regs;

static inline __attribute__((always_inline)) struct pt_regs *get_irq_regs(void)
{
 return (*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu____irq_regs))); (typeof((&per_cpu____irq_regs))) (__ptr + ((({ typeof(_proxy_pda.data_offset) ret__; switch (sizeof(_proxy_pda.data_offset)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; default: __bad_pda_field(); } ret__; })))); }));
}

static inline __attribute__((always_inline)) struct pt_regs *set_irq_regs(struct pt_regs *new_regs)
{
 struct pt_regs *old_regs, **pp_regs = &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu____irq_regs))); (typeof((&per_cpu____irq_regs))) (__ptr + ((({ typeof(_proxy_pda.data_offset) ret__; switch (sizeof(_proxy_pda.data_offset)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; default: __bad_pda_field(); } ret__; })))); }));

 old_regs = *pp_regs;
 *pp_regs = new_regs;
 return old_regs;
}
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq_regs_64.h" 2
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/irq_regs.h" 2
# 27 "include/linux/irq.h" 2

struct irq_desc;
typedef void (*irq_flow_handler_t)(unsigned int irq,
         struct irq_desc *desc);
# 77 "include/linux/irq.h"
struct proc_dir_entry;
struct msi_desc;
# 102 "include/linux/irq.h"
struct irq_chip {
 const char *name;
 unsigned int (*startup)(unsigned int irq);
 void (*shutdown)(unsigned int irq);
 void (*enable)(unsigned int irq);
 void (*disable)(unsigned int irq);

 void (*ack)(unsigned int irq);
 void (*mask)(unsigned int irq);
 void (*mask_ack)(unsigned int irq);
 void (*unmask)(unsigned int irq);
 void (*eoi)(unsigned int irq);

 void (*end)(unsigned int irq);
 void (*set_affinity)(unsigned int irq,
     const struct cpumask *dest);
 int (*retrigger)(unsigned int irq);
 int (*set_type)(unsigned int irq, unsigned int flow_type);
 int (*set_wake)(unsigned int irq, unsigned int on);
# 130 "include/linux/irq.h"
 const char *typename;
};

struct timer_rand_state;
struct irq_2_iommu;
# 161 "include/linux/irq.h"
struct irq_desc {
 unsigned int irq;







 irq_flow_handler_t handle_irq;
 struct irq_chip *chip;
 struct msi_desc *msi_desc;
 void *handler_data;
 void *chip_data;
 struct irqaction *action;
 unsigned int status;

 unsigned int depth;
 unsigned int wake_depth;
 unsigned int irq_count;
 unsigned long last_unhandled;
 unsigned int irqs_unhandled;
 spinlock_t lock;

 cpumask_t affinity;
 unsigned int cpu;


 cpumask_t pending_mask;


 struct proc_dir_entry *dir;

 const char *name;
} __attribute__((__aligned__(1 << ((7)))));

extern void arch_init_copy_chip_data(struct irq_desc *old_desc,
     struct irq_desc *desc, int cpu);
extern void arch_free_chip_data(struct irq_desc *old_desc, struct irq_desc *desc);


extern struct irq_desc irq_desc[(256 + (32 * 128))];
# 213 "include/linux/irq.h"
extern struct irq_desc *irq_to_desc_alloc_cpu(unsigned int irq, int cpu);

static inline __attribute__((always_inline)) struct irq_desc *
irq_remap_to_desc(unsigned int irq, struct irq_desc *desc)
{



 return desc;

}





typedef struct irq_chip hw_irq_controller;

typedef struct irq_desc irq_desc_t;




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hw_irq.h" 1
# 21 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hw_irq.h"
# 1 "include/linux/profile.h" 1
# 9 "include/linux/profile.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/errno.h" 1
# 10 "include/linux/profile.h" 2






struct proc_dir_entry;
struct pt_regs;
struct notifier_block;


void create_prof_cpu_mask(struct proc_dir_entry *de);
int create_proc_profile(void);
# 34 "include/linux/profile.h"
enum profile_type {
 PROFILE_TASK_EXIT,
 PROFILE_MUNMAP
};



extern int prof_on __attribute__((__section__(".data.read_mostly")));


int profile_init(void);
int profile_setup(char *str);
void profile_tick(int type);




void profile_hits(int type, void *ip, unsigned int nr_hits);




static inline __attribute__((always_inline)) void profile_hit(int type, void *ip)
{



 if (__builtin_expect(!!(prof_on == type), 0))
  profile_hits(type, ip, 1);
}

struct task_struct;
struct mm_struct;


void profile_task_exit(struct task_struct * task);




int profile_handoff_task(struct task_struct * task);


void profile_munmap(unsigned long addr);

int task_handoff_register(struct notifier_block * n);
int task_handoff_unregister(struct notifier_block * n);

int profile_event_register(enum profile_type, struct notifier_block * n);
int profile_event_unregister(enum profile_type, struct notifier_block * n);

int register_timer_hook(int (*hook)(struct pt_regs *));
void unregister_timer_hook(int (*hook)(struct pt_regs *));

struct pt_regs;
# 22 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hw_irq.h" 2


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 25 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hw_irq.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sections.h" 1
# 1 "include/asm-generic/sections.h" 1





extern char _text[], _stext[], _etext[];
extern char _data[], _sdata[], _edata[];
extern char __bss_start[], __bss_stop[];
extern char __init_begin[], __init_end[];
extern char _sinittext[], _einittext[];
extern char _end[];
extern char __per_cpu_start[], __per_cpu_end[];
extern char __kprobes_text_start[], __kprobes_text_end[];
extern char __initdata_begin[], __initdata_end[];
extern char __start_rodata[], __end_rodata[];
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/sections.h" 2
# 27 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hw_irq.h" 2




extern void apic_timer_interrupt(void);
extern void error_interrupt(void);
extern void spurious_interrupt(void);
extern void thermal_interrupt(void);
extern void reschedule_interrupt(void);

extern void invalidate_interrupt(void);
extern void invalidate_interrupt0(void);
extern void invalidate_interrupt1(void);
extern void invalidate_interrupt2(void);
extern void invalidate_interrupt3(void);
extern void invalidate_interrupt4(void);
extern void invalidate_interrupt5(void);
extern void invalidate_interrupt6(void);
extern void invalidate_interrupt7(void);

extern void irq_move_cleanup_interrupt(void);
extern void threshold_interrupt(void);

extern void call_function_interrupt(void);
extern void call_function_single_interrupt(void);


extern void disable_8259A_irq(unsigned int irq);
extern void enable_8259A_irq(unsigned int irq);
extern int i8259A_irq_pending(unsigned int irq);
extern void make_8259A_irq(unsigned int irq);
extern void init_8259A(int aeoi);



extern unsigned long io_apic_irqs;

extern void init_VISWS_APIC_irqs(void);
extern void setup_IO_APIC(void);
extern void disable_IO_APIC(void);
extern int IO_APIC_get_PCI_irq_vector(int bus, int slot, int fn);
extern void setup_ioapic_dest(void);


extern void enable_IO_APIC(void);






extern void send_IPI(int dest, int vector);


extern atomic_t irq_err_count;
extern atomic_t irq_mis_count;


extern void eisa_set_level_irq(unsigned int irq);


extern void vic_cpi_interrupt(void);
extern void vic_sys_interrupt(void);
extern void vic_cmn_interrupt(void);
extern void qic_timer_interrupt(void);
extern void qic_invalidate_interrupt(void);
extern void qic_reschedule_interrupt(void);
extern void qic_enable_irq_interrupt(void);
extern void qic_call_function_interrupt(void);


extern void smp_apic_timer_interrupt(struct pt_regs *);
extern void smp_spurious_interrupt(struct pt_regs *);
extern void smp_error_interrupt(struct pt_regs *);

extern void smp_reschedule_interrupt(struct pt_regs *);
extern void smp_call_function_interrupt(struct pt_regs *);
extern void smp_call_function_single_interrupt(struct pt_regs *);



extern void smp_invalidate_interrupt(struct pt_regs *);



extern void (*__attribute__ ((__section__(".init.rodata"))) interrupt[256 -0x20])(void);

typedef int vector_irq_t[256];
extern __typeof__(vector_irq_t) per_cpu__vector_irq;


extern void lock_vector_lock(void);
extern void unlock_vector_lock(void);
extern void __setup_vector_irq(int cpu);
# 237 "include/linux/irq.h" 2

extern int setup_irq(unsigned int irq, struct irqaction *new);







void move_native_irq(int irq);
void move_masked_irq(int irq);
# 272 "include/linux/irq.h"
extern int no_irq_affinity;

static inline __attribute__((always_inline)) int irq_balancing_disabled(unsigned int irq)
{
 struct irq_desc *desc;

 desc = irq_to_desc(irq);
 return desc->status & 0x00400000;
}


extern int handle_IRQ_event(unsigned int irq, struct irqaction *action);





extern void handle_level_irq(unsigned int irq, struct irq_desc *desc);
extern void handle_fasteoi_irq(unsigned int irq, struct irq_desc *desc);
extern void handle_edge_irq(unsigned int irq, struct irq_desc *desc);
extern void handle_simple_irq(unsigned int irq, struct irq_desc *desc);
extern void handle_percpu_irq(unsigned int irq, struct irq_desc *desc);
extern void handle_bad_irq(unsigned int irq, struct irq_desc *desc);





extern unsigned int __do_IRQ(unsigned int irq);
# 309 "include/linux/irq.h"
static inline __attribute__((always_inline)) void generic_handle_irq_desc(unsigned int irq, struct irq_desc *desc)
{



 if (__builtin_expect(!!(desc->handle_irq), 1))
  desc->handle_irq(irq, desc);
 else
  __do_IRQ(irq);

}

static inline __attribute__((always_inline)) void generic_handle_irq(unsigned int irq)
{
 generic_handle_irq_desc(irq, irq_to_desc(irq));
}


extern void note_interrupt(unsigned int irq, struct irq_desc *desc,
      int action_ret);


void check_irq_resend(struct irq_desc *desc, unsigned int irq);


extern int noirqdebug_setup(char *str);


extern int can_request_irq(unsigned int irq, unsigned long irqflags);


extern struct irq_chip no_irq_chip;
extern struct irq_chip dummy_irq_chip;

extern void
set_irq_chip_and_handler(unsigned int irq, struct irq_chip *chip,
    irq_flow_handler_t handle);
extern void
set_irq_chip_and_handler_name(unsigned int irq, struct irq_chip *chip,
         irq_flow_handler_t handle, const char *name);

extern void
__set_irq_handler(unsigned int irq, irq_flow_handler_t handle, int is_chained,
    const char *name);


static inline __attribute__((always_inline)) void __set_irq_handler_unlocked(int irq,
           irq_flow_handler_t handler)
{
 struct irq_desc *desc;

 desc = irq_to_desc(irq);
 desc->handle_irq = handler;
}




static inline __attribute__((always_inline)) void
set_irq_handler(unsigned int irq, irq_flow_handler_t handle)
{
 __set_irq_handler(irq, handle, 0, ((void *)0));
}






static inline __attribute__((always_inline)) void
set_irq_chained_handler(unsigned int irq,
   irq_flow_handler_t handle)
{
 __set_irq_handler(irq, handle, 1, ((void *)0));
}

extern void set_irq_noprobe(unsigned int irq);
extern void set_irq_probe(unsigned int irq);


extern unsigned int create_irq_nr(unsigned int irq_want);
extern int create_irq(void);
extern void destroy_irq(unsigned int irq);


static inline __attribute__((always_inline)) int irq_has_action(unsigned int irq)
{
 struct irq_desc *desc = irq_to_desc(irq);
 return desc->action != ((void *)0);
}


extern void dynamic_irq_init(unsigned int irq);
extern void dynamic_irq_cleanup(unsigned int irq);


extern int set_irq_chip(unsigned int irq, struct irq_chip *chip);
extern int set_irq_data(unsigned int irq, void *data);
extern int set_irq_chip_data(unsigned int irq, void *data);
extern int set_irq_type(unsigned int irq, unsigned int type);
extern int set_irq_msi(unsigned int irq, struct msi_desc *entry);
# 6 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hardirq_64.h" 2
# 23 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hardirq_64.h"
extern void ack_bad_irq(unsigned int irq);
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/hardirq.h" 2


extern u64 arch_irq_stat_cpu(unsigned int cpu);


extern u64 arch_irq_stat(void);
# 9 "include/linux/hardirq.h" 2
# 111 "include/linux/hardirq.h"
extern void synchronize_irq(unsigned int irq);
# 120 "include/linux/hardirq.h"
struct task_struct;


static inline __attribute__((always_inline)) void account_system_vtime(struct task_struct *tsk)
{
}
# 156 "include/linux/hardirq.h"
extern void irq_enter(void);
# 171 "include/linux/hardirq.h"
extern void irq_exit(void);
# 13 "include/linux/interrupt.h" 2





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 19 "include/linux/interrupt.h" 2
# 62 "include/linux/interrupt.h"
typedef irqreturn_t (*irq_handler_t)(int, void *);

struct irqaction {
 irq_handler_t handler;
 unsigned long flags;
 cpumask_t mask;
 const char *name;
 void *dev_id;
 struct irqaction *next;
 int irq;
 struct proc_dir_entry *dir;
};

extern irqreturn_t no_action(int cpl, void *dev_id);
extern int __attribute__((warn_unused_result)) request_irq(unsigned int, irq_handler_t handler,
         unsigned long, const char *, void *);
extern void free_irq(unsigned int, void *);

struct device;

extern int __attribute__((warn_unused_result)) devm_request_irq(struct device *dev, unsigned int irq,
       irq_handler_t handler, unsigned long irqflags,
       const char *devname, void *dev_id);
extern void devm_free_irq(struct device *dev, unsigned int irq, void *dev_id);
# 105 "include/linux/interrupt.h"
extern void disable_irq_nosync(unsigned int irq);
extern void disable_irq(unsigned int irq);
extern void enable_irq(unsigned int irq);



extern cpumask_var_t irq_default_affinity;

extern int irq_set_affinity(unsigned int irq, const struct cpumask *cpumask);
extern int irq_can_set_affinity(unsigned int irq);
extern int irq_select_affinity(unsigned int irq);
# 145 "include/linux/interrupt.h"
static inline __attribute__((always_inline)) void disable_irq_nosync_lockdep(unsigned int irq)
{
 disable_irq_nosync(irq);

 do { raw_local_irq_disable(); do { } while (0); } while (0);

}

static inline __attribute__((always_inline)) void disable_irq_nosync_lockdep_irqsave(unsigned int irq, unsigned long *flags)
{
 disable_irq_nosync(irq);

 do { ({ unsigned long __dummy; typeof(*flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); do { (*flags) = __raw_local_irq_save(); } while (0); do { } while (0); } while (0);

}

static inline __attribute__((always_inline)) void disable_irq_lockdep(unsigned int irq)
{
 disable_irq(irq);

 do { raw_local_irq_disable(); do { } while (0); } while (0);

}

static inline __attribute__((always_inline)) void enable_irq_lockdep(unsigned int irq)
{

 do { do { } while (0); raw_local_irq_enable(); } while (0);

 enable_irq(irq);
}

static inline __attribute__((always_inline)) void enable_irq_lockdep_irqrestore(unsigned int irq, unsigned long *flags)
{

 do { ({ unsigned long __dummy; typeof(*flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); if (raw_irqs_disabled_flags(*flags)) { raw_local_irq_restore(*flags); do { } while (0); } else { do { } while (0); raw_local_irq_restore(*flags); } } while (0);

 enable_irq(irq);
}


extern int set_irq_wake(unsigned int irq, unsigned int on);

static inline __attribute__((always_inline)) int enable_irq_wake(unsigned int irq)
{
 return set_irq_wake(irq, 1);
}

static inline __attribute__((always_inline)) int disable_irq_wake(unsigned int irq)
{
 return set_irq_wake(irq, 0);
}
# 246 "include/linux/interrupt.h"
enum
{
 HI_SOFTIRQ=0,
 TIMER_SOFTIRQ,
 NET_TX_SOFTIRQ,
 NET_RX_SOFTIRQ,
 BLOCK_SOFTIRQ,
 TASKLET_SOFTIRQ,
 SCHED_SOFTIRQ,
 HRTIMER_SOFTIRQ,
 RCU_SOFTIRQ,

 NR_SOFTIRQS
};





struct softirq_action
{
 void (*action)(struct softirq_action *);
};

 void do_softirq(void);
 void __do_softirq(void);
extern void open_softirq(int nr, void (*action)(struct softirq_action *));
extern void softirq_init(void);

extern void raise_softirq_irqoff(unsigned int nr);
extern void raise_softirq(unsigned int nr);
# 285 "include/linux/interrupt.h"
extern __typeof__(struct list_head [NR_SOFTIRQS]) per_cpu__softirq_work_list;




extern void send_remote_softirq(struct call_single_data *cp, int cpu, int softirq);




extern void __send_remote_softirq(struct call_single_data *cp, int cpu,
      int this_cpu, int softirq);
# 318 "include/linux/interrupt.h"
struct tasklet_struct
{
 struct tasklet_struct *next;
 unsigned long state;
 atomic_t count;
 void (*func)(unsigned long);
 unsigned long data;
};
# 334 "include/linux/interrupt.h"
enum
{
 TASKLET_STATE_SCHED,
 TASKLET_STATE_RUN
};


static inline __attribute__((always_inline)) int tasklet_trylock(struct tasklet_struct *t)
{
 return !test_and_set_bit(TASKLET_STATE_RUN, &(t)->state);
}

static inline __attribute__((always_inline)) void tasklet_unlock(struct tasklet_struct *t)
{
 __asm__ __volatile__("": : :"memory");
 clear_bit(TASKLET_STATE_RUN, &(t)->state);
}

static inline __attribute__((always_inline)) void tasklet_unlock_wait(struct tasklet_struct *t)
{
 while ((__builtin_constant_p((TASKLET_STATE_RUN)) ? constant_test_bit((TASKLET_STATE_RUN), (&(t)->state)) : variable_test_bit((TASKLET_STATE_RUN), (&(t)->state)))) { __asm__ __volatile__("": : :"memory"); }
}






extern void __tasklet_schedule(struct tasklet_struct *t);

static inline __attribute__((always_inline)) void tasklet_schedule(struct tasklet_struct *t)
{
 if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
  __tasklet_schedule(t);
}

extern void __tasklet_hi_schedule(struct tasklet_struct *t);

static inline __attribute__((always_inline)) void tasklet_hi_schedule(struct tasklet_struct *t)
{
 if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
  __tasklet_hi_schedule(t);
}


static inline __attribute__((always_inline)) void tasklet_disable_nosync(struct tasklet_struct *t)
{
 atomic_inc(&t->count);
 __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((always_inline)) void tasklet_disable(struct tasklet_struct *t)
{
 tasklet_disable_nosync(t);
 tasklet_unlock_wait(t);
 asm volatile("mfence":::"memory");
}

static inline __attribute__((always_inline)) void tasklet_enable(struct tasklet_struct *t)
{
 __asm__ __volatile__("": : :"memory");
 atomic_dec(&t->count);
}

static inline __attribute__((always_inline)) void tasklet_hi_enable(struct tasklet_struct *t)
{
 __asm__ __volatile__("": : :"memory");
 atomic_dec(&t->count);
}

extern void tasklet_kill(struct tasklet_struct *t);
extern void tasklet_kill_immediate(struct tasklet_struct *t, unsigned int cpu);
extern void tasklet_init(struct tasklet_struct *t,
    void (*func)(unsigned long), unsigned long data);
# 451 "include/linux/interrupt.h"
extern unsigned long probe_irq_on(void);
extern int probe_irq_off(unsigned long);
extern unsigned int probe_irq_mask(unsigned long);




extern void init_irq_proc(void);






int show_interrupts(struct seq_file *p, void *v);

struct irq_desc;

extern int early_irq_init(void);
extern int arch_early_irq_init(void);
extern int arch_init_chip_data(struct irq_desc *desc, int cpu);
# 32 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2

# 1 "include/linux/pci.h" 1
# 20 "include/linux/pci.h"
# 1 "include/linux/pci_regs.h" 1
# 21 "include/linux/pci.h" 2
# 43 "include/linux/pci.h"
# 1 "include/linux/mod_devicetable.h" 1
# 12 "include/linux/mod_devicetable.h"
typedef unsigned long kernel_ulong_t;




struct pci_device_id {
 __u32 vendor, device;
 __u32 subvendor, subdevice;
 __u32 class, class_mask;
 kernel_ulong_t driver_data;
};







struct ieee1394_device_id {
 __u32 match_flags;
 __u32 vendor_id;
 __u32 model_id;
 __u32 specifier_id;
 __u32 version;
 kernel_ulong_t driver_data
  __attribute__((aligned(sizeof(kernel_ulong_t))));
};
# 98 "include/linux/mod_devicetable.h"
struct usb_device_id {

 __u16 match_flags;


 __u16 idVendor;
 __u16 idProduct;
 __u16 bcdDevice_lo;
 __u16 bcdDevice_hi;


 __u8 bDeviceClass;
 __u8 bDeviceSubClass;
 __u8 bDeviceProtocol;


 __u8 bInterfaceClass;
 __u8 bInterfaceSubClass;
 __u8 bInterfaceProtocol;


 kernel_ulong_t driver_info;
};
# 136 "include/linux/mod_devicetable.h"
struct hid_device_id {
 __u16 bus;
 __u16 pad1;
 __u32 vendor;
 __u32 product;
 kernel_ulong_t driver_data
  __attribute__((aligned(sizeof(kernel_ulong_t))));
};


struct ccw_device_id {
 __u16 match_flags;

 __u16 cu_type;
 __u16 dev_type;
 __u8 cu_model;
 __u8 dev_model;

 kernel_ulong_t driver_info;
};







struct ap_device_id {
 __u16 match_flags;
 __u8 dev_type;
 __u8 pad1;
 __u32 pad2;
 kernel_ulong_t driver_info;
};




struct css_device_id {
 __u8 match_flags;
 __u8 type;
 __u16 pad2;
 __u32 pad3;
 kernel_ulong_t driver_data;
};




struct acpi_device_id {
 __u8 id[16];
 kernel_ulong_t driver_data;
};




struct pnp_device_id {
 __u8 id[8];
 kernel_ulong_t driver_data;
};

struct pnp_card_device_id {
 __u8 id[8];
 kernel_ulong_t driver_data;
 struct {
  __u8 id[8];
 } devs[8];
};




struct serio_device_id {
 __u8 type;
 __u8 extra;
 __u8 id;
 __u8 proto;
};




struct of_device_id
{
 char name[32];
 char type[32];
 char compatible[128];

 void *data;



};


struct vio_device_id {
 char type[32];
 char compat[32];
};



struct pcmcia_device_id {
 __u16 match_flags;

 __u16 manf_id;
 __u16 card_id;

 __u8 func_id;


 __u8 function;


 __u8 device_no;

 __u32 prod_id_hash[4]
  __attribute__((aligned(sizeof(__u32))));



 const char * prod_id[4];






 kernel_ulong_t driver_info;

 char * cisfile;



};
# 312 "include/linux/mod_devicetable.h"
struct input_device_id {

 kernel_ulong_t flags;

 __u16 bustype;
 __u16 vendor;
 __u16 product;
 __u16 version;

 kernel_ulong_t evbit[0x1f / 64 + 1];
 kernel_ulong_t keybit[0x2ff / 64 + 1];
 kernel_ulong_t relbit[0x0f / 64 + 1];
 kernel_ulong_t absbit[0x3f / 64 + 1];
 kernel_ulong_t mscbit[0x07 / 64 + 1];
 kernel_ulong_t ledbit[0x0f / 64 + 1];
 kernel_ulong_t sndbit[0x07 / 64 + 1];
 kernel_ulong_t ffbit[0x7f / 64 + 1];
 kernel_ulong_t swbit[0x0f / 64 + 1];

 kernel_ulong_t driver_info;
};






struct eisa_device_id {
 char sig[8];
 kernel_ulong_t driver_data;
};



struct parisc_device_id {
 __u8 hw_type;
 __u8 hversion_rev;
 __u16 hversion;
 __u32 sversion;
};
# 362 "include/linux/mod_devicetable.h"
struct sdio_device_id {
 __u8 class;
 __u16 vendor;
 __u16 device;
 kernel_ulong_t driver_data
  __attribute__((aligned(sizeof(kernel_ulong_t))));
};


struct ssb_device_id {
 __u16 vendor;
 __u16 coreid;
 __u8 revision;
};
# 385 "include/linux/mod_devicetable.h"
struct virtio_device_id {
 __u32 device;
 __u32 vendor;
};







struct i2c_device_id {
 char name[20];
 kernel_ulong_t driver_data
   __attribute__((aligned(sizeof(kernel_ulong_t))));
};


enum dmi_field {
 DMI_NONE,
 DMI_BIOS_VENDOR,
 DMI_BIOS_VERSION,
 DMI_BIOS_DATE,
 DMI_SYS_VENDOR,
 DMI_PRODUCT_NAME,
 DMI_PRODUCT_VERSION,
 DMI_PRODUCT_SERIAL,
 DMI_PRODUCT_UUID,
 DMI_BOARD_VENDOR,
 DMI_BOARD_NAME,
 DMI_BOARD_VERSION,
 DMI_BOARD_SERIAL,
 DMI_BOARD_ASSET_TAG,
 DMI_CHASSIS_VENDOR,
 DMI_CHASSIS_TYPE,
 DMI_CHASSIS_VERSION,
 DMI_CHASSIS_SERIAL,
 DMI_CHASSIS_ASSET_TAG,
 DMI_STRING_MAX,
};

struct dmi_strmatch {
 unsigned char slot;
 char substr[79];
};
# 440 "include/linux/mod_devicetable.h"
struct dmi_system_id {
 int (*callback)(const struct dmi_system_id *);
 const char *ident;
 struct dmi_strmatch matches[4];
 void *driver_data;
};
# 44 "include/linux/pci.h" 2



# 1 "include/linux/ioport.h" 1
# 18 "include/linux/ioport.h"
struct resource {
 resource_size_t start;
 resource_size_t end;
 const char *name;
 unsigned long flags;
 struct resource *parent, *sibling, *child;
};

struct resource_list {
 struct resource_list *next;
 struct resource *res;
 struct pci_dev *dev;
};
# 108 "include/linux/ioport.h"
extern struct resource ioport_resource;
extern struct resource iomem_resource;

extern int request_resource(struct resource *root, struct resource *new);
extern int release_resource(struct resource *new);
extern void reserve_region_with_split(struct resource *root,
        resource_size_t start, resource_size_t end,
        const char *name);
extern int insert_resource(struct resource *parent, struct resource *new);
extern void insert_resource_expand_to_fit(struct resource *root, struct resource *new);
extern int allocate_resource(struct resource *root, struct resource *new,
        resource_size_t size, resource_size_t min,
        resource_size_t max, resource_size_t align,
        void (*alignf)(void *, struct resource *,
         resource_size_t, resource_size_t),
        void *alignf_data);
int adjust_resource(struct resource *res, resource_size_t start,
      resource_size_t size);
resource_size_t resource_alignment(struct resource *res);
static inline __attribute__((always_inline)) resource_size_t resource_size(struct resource *res)
{
 return res->end - res->start + 1;
}
static inline __attribute__((always_inline)) unsigned long resource_type(struct resource *res)
{
 return res->flags & 0x00000f00;
}
# 144 "include/linux/ioport.h"
extern struct resource * __request_region(struct resource *,
     resource_size_t start,
     resource_size_t n,
     const char *name, int flags);






extern int __check_region(struct resource *, resource_size_t, resource_size_t);
extern void __release_region(struct resource *, resource_size_t,
    resource_size_t);

static inline __attribute__((always_inline)) int __attribute__((deprecated)) check_region(resource_size_t s,
      resource_size_t n)
{
 return __check_region(&ioport_resource, s, n);
}


struct device;





extern struct resource * __devm_request_region(struct device *dev,
    struct resource *parent, resource_size_t start,
    resource_size_t n, const char *name);






extern void __devm_release_region(struct device *dev, struct resource *parent,
      resource_size_t start, resource_size_t n);
extern int iomem_map_sanity_check(resource_size_t addr, unsigned long size);
extern int iomem_is_exclusive(u64 addr);
# 48 "include/linux/pci.h" 2




# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 53 "include/linux/pci.h" 2
# 1 "include/linux/device.h" 1
# 17 "include/linux/device.h"
# 1 "include/linux/klist.h" 1
# 19 "include/linux/klist.h"
struct klist_node;
struct klist {
 spinlock_t k_lock;
 struct list_head k_list;
 void (*get)(struct klist_node *);
 void (*put)(struct klist_node *);
} __attribute__ ((aligned (4)));
# 36 "include/linux/klist.h"
extern void klist_init(struct klist *k, void (*get)(struct klist_node *),
         void (*put)(struct klist_node *));

struct klist_node {
 void *n_klist;
 struct list_head n_node;
 struct kref n_ref;
};

extern void klist_add_tail(struct klist_node *n, struct klist *k);
extern void klist_add_head(struct klist_node *n, struct klist *k);
extern void klist_add_after(struct klist_node *n, struct klist_node *pos);
extern void klist_add_before(struct klist_node *n, struct klist_node *pos);

extern void klist_del(struct klist_node *n);
extern void klist_remove(struct klist_node *n);

extern int klist_node_attached(struct klist_node *n);


struct klist_iter {
 struct klist *i_klist;
 struct klist_node *i_cur;
};


extern void klist_iter_init(struct klist *k, struct klist_iter *i);
extern void klist_iter_init_node(struct klist *k, struct klist_iter *i,
     struct klist_node *n);
extern void klist_iter_exit(struct klist_iter *i);
extern struct klist_node *klist_next(struct klist_iter *i);
# 18 "include/linux/device.h" 2




# 1 "include/linux/module.h" 1
# 10 "include/linux/module.h"
# 1 "include/linux/stat.h" 1





# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/stat.h" 1
# 68 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/stat.h"
struct stat {
 unsigned long st_dev;
 unsigned long st_ino;
 unsigned long st_nlink;

 unsigned int st_mode;
 unsigned int st_uid;
 unsigned int st_gid;
 unsigned int __pad0;
 unsigned long st_rdev;
 long st_size;
 long st_blksize;
 long st_blocks;

 unsigned long st_atime;
 unsigned long st_atime_nsec;
 unsigned long st_mtime;
 unsigned long st_mtime_nsec;
 unsigned long st_ctime;
 unsigned long st_ctime_nsec;
 long __unused[3];
};



struct __old_kernel_stat {
 unsigned short st_dev;
 unsigned short st_ino;
 unsigned short st_mode;
 unsigned short st_nlink;
 unsigned short st_uid;
 unsigned short st_gid;
 unsigned short st_rdev;






 unsigned int st_size;
 unsigned int st_atime;
 unsigned int st_mtime;
 unsigned int st_ctime;

};
# 7 "include/linux/stat.h" 2
# 62 "include/linux/stat.h"
struct kstat {
 u64 ino;
 dev_t dev;
 umode_t mode;
 unsigned int nlink;
 uid_t uid;
 gid_t gid;
 dev_t rdev;
 loff_t size;
 struct timespec atime;
 struct timespec mtime;
 struct timespec ctime;
 unsigned long blksize;
 unsigned long long blocks;
};
# 11 "include/linux/module.h" 2


# 1 "include/linux/kmod.h" 1
# 32 "include/linux/kmod.h"
extern int request_module(const char * name, ...) __attribute__ ((format (printf, 1, 2)));







struct key;
struct file;
struct subprocess_info;


struct subprocess_info *call_usermodehelper_setup(char *path, char **argv,
        char **envp, gfp_t gfp_mask);


void call_usermodehelper_setkeys(struct subprocess_info *info,
     struct key *session_keyring);
int call_usermodehelper_stdinpipe(struct subprocess_info *sub_info,
      struct file **filp);
void call_usermodehelper_setcleanup(struct subprocess_info *info,
        void (*cleanup)(char **argv, char **envp));

enum umh_wait {
 UMH_NO_WAIT = -1,
 UMH_WAIT_EXEC = 0,
 UMH_WAIT_PROC = 1,
};


int call_usermodehelper_exec(struct subprocess_info *info, enum umh_wait wait);



void call_usermodehelper_freeinfo(struct subprocess_info *info);

static inline __attribute__((always_inline)) int
call_usermodehelper(char *path, char **argv, char **envp, enum umh_wait wait)
{
 struct subprocess_info *info;
 gfp_t gfp_mask = (wait == UMH_NO_WAIT) ? ((( gfp_t)0x20u)) : ((( gfp_t)0x10u) | (( gfp_t)0x40u) | (( gfp_t)0x80u));

 info = call_usermodehelper_setup(path, argv, envp, gfp_mask);
 if (info == ((void *)0))
  return -12;
 return call_usermodehelper_exec(info, wait);
}

static inline __attribute__((always_inline)) int
call_usermodehelper_keys(char *path, char **argv, char **envp,
    struct key *session_keyring, enum umh_wait wait)
{
 struct subprocess_info *info;
 gfp_t gfp_mask = (wait == UMH_NO_WAIT) ? ((( gfp_t)0x20u)) : ((( gfp_t)0x10u) | (( gfp_t)0x40u) | (( gfp_t)0x80u));

 info = call_usermodehelper_setup(path, argv, envp, gfp_mask);
 if (info == ((void *)0))
  return -12;

 call_usermodehelper_setkeys(info, session_keyring);
 return call_usermodehelper_exec(info, wait);
}

extern void usermodehelper_init(void);

struct file;
extern int call_usermodehelper_pipe(char *path, char *argv[], char *envp[],
        struct file **filp);

extern int usermodehelper_disable(void);
extern void usermodehelper_enable(void);
# 14 "include/linux/module.h" 2
# 1 "include/linux/elf.h" 1




# 1 "include/linux/elf-em.h" 1
# 6 "include/linux/elf.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h" 1
# 9 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/user.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/user_64.h" 1
# 50 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/user_64.h"
struct user_i387_struct {
 unsigned short cwd;
 unsigned short swd;
 unsigned short twd;

 unsigned short fop;
 __u64 rip;
 __u64 rdp;
 __u32 mxcsr;
 __u32 mxcsr_mask;
 __u32 st_space[32];
 __u32 xmm_space[64];
 __u32 padding[24];
};




struct user_regs_struct {
 unsigned long r15;
 unsigned long r14;
 unsigned long r13;
 unsigned long r12;
 unsigned long bp;
 unsigned long bx;
 unsigned long r11;
 unsigned long r10;
 unsigned long r9;
 unsigned long r8;
 unsigned long ax;
 unsigned long cx;
 unsigned long dx;
 unsigned long si;
 unsigned long di;
 unsigned long orig_ax;
 unsigned long ip;
 unsigned long cs;
 unsigned long flags;
 unsigned long sp;
 unsigned long ss;
 unsigned long fs_base;
 unsigned long gs_base;
 unsigned long ds;
 unsigned long es;
 unsigned long fs;
 unsigned long gs;
};





struct user {


  struct user_regs_struct regs;

  int u_fpvalid;

  int pad0;
  struct user_i387_struct i387;

  unsigned long int u_tsize;
  unsigned long int u_dsize;
  unsigned long int u_ssize;
  unsigned long start_code;
  unsigned long start_stack;



  long int signal;
  int reserved;
  int pad1;
  unsigned long u_ar0;

  struct user_i387_struct *u_fpstate;
  unsigned long magic;
  char u_comm[32];
  unsigned long u_debugreg[8];
  unsigned long error_code;
  unsigned long fault_address;
};
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/user.h" 2
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h" 2


typedef unsigned long elf_greg_t;


typedef elf_greg_t elf_gregset_t[(sizeof(struct user_regs_struct) / sizeof(elf_greg_t))];

typedef struct user_i387_struct elf_fpregset_t;
# 75 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vdso.h" 1




extern const char VDSO64_PRELINK[];
# 19 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vdso.h"
extern const char VDSO32_PRELINK[];
# 36 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/vdso.h"
extern void __attribute__((noderef, address_space(1))) __kernel_sigreturn;
extern void __attribute__((noderef, address_space(1))) __kernel_rt_sigreturn;





extern const char vdso32_int80_start, vdso32_int80_end;
extern const char vdso32_syscall_start, vdso32_syscall_end;
extern const char vdso32_sysenter_start, vdso32_sysenter_end;
# 76 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h" 2

extern unsigned int vdso_enabled;
# 149 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h"
static inline __attribute__((always_inline)) void start_ia32_thread(struct pt_regs *regs, u32 ip, u32 sp)
{
 asm volatile("\n" "1:\t" "movl %k0,%%" "fs" "\n" "2:\n" ".section .fixup,\"ax\"\n" "3:\t" "movl %k1, %%" "fs" "\n\t" "jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : :"r" (0), "r" (0) : "memory");
 asm volatile("\n" "1:\t" "movl %k0,%%" "ds" "\n" "2:\n" ".section .fixup,\"ax\"\n" "3:\t" "movl %k1, %%" "ds" "\n\t" "jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : :"r" ((5* 8 + 3)), "r" (0) : "memory");
 asm volatile("\n" "1:\t" "movl %k0,%%" "es" "\n" "2:\n" ".section .fixup,\"ax\"\n" "3:\t" "movl %k1, %%" "es" "\n\t" "jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : :"r" ((5* 8 + 3)), "r" (0) : "memory");
 native_load_gs_index(0);
 regs->ip = ip;
 regs->sp = sp;
 regs->flags = 0x00000200;
 regs->cs = (4 * 8 + 3);
 regs->ss = (5* 8 + 3);
}

static inline __attribute__((always_inline)) void elf_common_init(struct thread_struct *t,
       struct pt_regs *regs, const u16 ds)
{
 regs->ax = regs->bx = regs->cx = regs->dx = 0;
 regs->si = regs->di = regs->bp = 0;
 regs->r8 = regs->r9 = regs->r10 = regs->r11 = 0;
 regs->r12 = regs->r13 = regs->r14 = regs->r15 = 0;
 t->fs = t->gs = 0;
 t->fsindex = t->gsindex = 0;
 t->ds = t->es = ds;
}
# 240 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h"
extern void set_personality_64bit(void);
extern unsigned int sysctl_vsyscall32;
extern int force_personality32;
# 279 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h"
struct task_struct;
# 324 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/elf.h"
struct linux_binprm;


extern int arch_setup_additional_pages(struct linux_binprm *bprm,
           int uses_interp);

extern int syscall32_setup_pages(struct linux_binprm *, int exstack);


extern unsigned long arch_randomize_brk(struct mm_struct *mm);
# 8 "include/linux/elf.h" 2


struct file;
# 20 "include/linux/elf.h"
typedef __u32 Elf32_Addr;
typedef __u16 Elf32_Half;
typedef __u32 Elf32_Off;
typedef __s32 Elf32_Sword;
typedef __u32 Elf32_Word;


typedef __u64 Elf64_Addr;
typedef __u16 Elf64_Half;
typedef __s16 Elf64_SHalf;
typedef __u64 Elf64_Off;
typedef __s32 Elf64_Sword;
typedef __u32 Elf64_Word;
typedef __u64 Elf64_Xword;
typedef __s64 Elf64_Sxword;
# 127 "include/linux/elf.h"
typedef struct dynamic{
  Elf32_Sword d_tag;
  union{
    Elf32_Sword d_val;
    Elf32_Addr d_ptr;
  } d_un;
} Elf32_Dyn;

typedef struct {
  Elf64_Sxword d_tag;
  union {
    Elf64_Xword d_val;
    Elf64_Addr d_ptr;
  } d_un;
} Elf64_Dyn;
# 150 "include/linux/elf.h"
typedef struct elf32_rel {
  Elf32_Addr r_offset;
  Elf32_Word r_info;
} Elf32_Rel;

typedef struct elf64_rel {
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
} Elf64_Rel;

typedef struct elf32_rela{
  Elf32_Addr r_offset;
  Elf32_Word r_info;
  Elf32_Sword r_addend;
} Elf32_Rela;

typedef struct elf64_rela {
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
  Elf64_Sxword r_addend;
} Elf64_Rela;

typedef struct elf32_sym{
  Elf32_Word st_name;
  Elf32_Addr st_value;
  Elf32_Word st_size;
  unsigned char st_info;
  unsigned char st_other;
  Elf32_Half st_shndx;
} Elf32_Sym;

typedef struct elf64_sym {
  Elf64_Word st_name;
  unsigned char st_info;
  unsigned char st_other;
  Elf64_Half st_shndx;
  Elf64_Addr st_value;
  Elf64_Xword st_size;
} Elf64_Sym;




typedef struct elf32_hdr{
  unsigned char e_ident[16];
  Elf32_Half e_type;
  Elf32_Half e_machine;
  Elf32_Word e_version;
  Elf32_Addr e_entry;
  Elf32_Off e_phoff;
  Elf32_Off e_shoff;
  Elf32_Word e_flags;
  Elf32_Half e_ehsize;
  Elf32_Half e_phentsize;
  Elf32_Half e_phnum;
  Elf32_Half e_shentsize;
  Elf32_Half e_shnum;
  Elf32_Half e_shstrndx;
} Elf32_Ehdr;

typedef struct elf64_hdr {
  unsigned char e_ident[16];
  Elf64_Half e_type;
  Elf64_Half e_machine;
  Elf64_Word e_version;
  Elf64_Addr e_entry;
  Elf64_Off e_phoff;
  Elf64_Off e_shoff;
  Elf64_Word e_flags;
  Elf64_Half e_ehsize;
  Elf64_Half e_phentsize;
  Elf64_Half e_phnum;
  Elf64_Half e_shentsize;
  Elf64_Half e_shnum;
  Elf64_Half e_shstrndx;
} Elf64_Ehdr;







typedef struct elf32_phdr{
  Elf32_Word p_type;
  Elf32_Off p_offset;
  Elf32_Addr p_vaddr;
  Elf32_Addr p_paddr;
  Elf32_Word p_filesz;
  Elf32_Word p_memsz;
  Elf32_Word p_flags;
  Elf32_Word p_align;
} Elf32_Phdr;

typedef struct elf64_phdr {
  Elf64_Word p_type;
  Elf64_Word p_flags;
  Elf64_Off p_offset;
  Elf64_Addr p_vaddr;
  Elf64_Addr p_paddr;
  Elf64_Xword p_filesz;
  Elf64_Xword p_memsz;
  Elf64_Xword p_align;
} Elf64_Phdr;
# 289 "include/linux/elf.h"
typedef struct {
  Elf32_Word sh_name;
  Elf32_Word sh_type;
  Elf32_Word sh_flags;
  Elf32_Addr sh_addr;
  Elf32_Off sh_offset;
  Elf32_Word sh_size;
  Elf32_Word sh_link;
  Elf32_Word sh_info;
  Elf32_Word sh_addralign;
  Elf32_Word sh_entsize;
} Elf32_Shdr;

typedef struct elf64_shdr {
  Elf64_Word sh_name;
  Elf64_Word sh_type;
  Elf64_Xword sh_flags;
  Elf64_Addr sh_addr;
  Elf64_Off sh_offset;
  Elf64_Xword sh_size;
  Elf64_Word sh_link;
  Elf64_Word sh_info;
  Elf64_Xword sh_addralign;
  Elf64_Xword sh_entsize;
} Elf64_Shdr;
# 367 "include/linux/elf.h"
typedef struct elf32_note {
  Elf32_Word n_namesz;
  Elf32_Word n_descsz;
  Elf32_Word n_type;
} Elf32_Nhdr;


typedef struct elf64_note {
  Elf64_Word n_namesz;
  Elf64_Word n_descsz;
  Elf64_Word n_type;
} Elf64_Nhdr;
# 391 "include/linux/elf.h"
extern Elf64_Dyn _DYNAMIC [];
# 401 "include/linux/elf.h"
static inline __attribute__((always_inline)) int elf_coredump_extra_notes_size(void) { return 0; }
static inline __attribute__((always_inline)) int elf_coredump_extra_notes_write(struct file *file,
   loff_t *foffset) { return 0; }
# 15 "include/linux/module.h" 2


# 1 "include/linux/moduleparam.h" 1
# 32 "include/linux/moduleparam.h"
struct kernel_param;


typedef int (*param_set_fn)(const char *val, struct kernel_param *kp);

typedef int (*param_get_fn)(char *buffer, struct kernel_param *kp);

struct kernel_param {
 const char *name;
 unsigned int perm;
 param_set_fn set;
 param_get_fn get;
 union {
  void *arg;
  const struct kparam_string *str;
  const struct kparam_array *arr;
 };
};


struct kparam_string {
 unsigned int maxlen;
 char *string;
};


struct kparam_array
{
 unsigned int max;
 unsigned int *num;
 param_set_fn set;
 param_get_fn get;
 unsigned int elemsize;
 void *elem;
};
# 135 "include/linux/moduleparam.h"
extern int parse_args(const char *name,
        char *args,
        struct kernel_param *params,
        unsigned num,
        int (*unknown)(char *param, char *val));







extern int param_set_byte(const char *val, struct kernel_param *kp);
extern int param_get_byte(char *buffer, struct kernel_param *kp);


extern int param_set_short(const char *val, struct kernel_param *kp);
extern int param_get_short(char *buffer, struct kernel_param *kp);


extern int param_set_ushort(const char *val, struct kernel_param *kp);
extern int param_get_ushort(char *buffer, struct kernel_param *kp);


extern int param_set_int(const char *val, struct kernel_param *kp);
extern int param_get_int(char *buffer, struct kernel_param *kp);


extern int param_set_uint(const char *val, struct kernel_param *kp);
extern int param_get_uint(char *buffer, struct kernel_param *kp);


extern int param_set_long(const char *val, struct kernel_param *kp);
extern int param_get_long(char *buffer, struct kernel_param *kp);


extern int param_set_ulong(const char *val, struct kernel_param *kp);
extern int param_get_ulong(char *buffer, struct kernel_param *kp);


extern int param_set_charp(const char *val, struct kernel_param *kp);
extern int param_get_charp(char *buffer, struct kernel_param *kp);


extern int param_set_bool(const char *val, struct kernel_param *kp);
extern int param_get_bool(char *buffer, struct kernel_param *kp);


extern int param_set_invbool(const char *val, struct kernel_param *kp);
extern int param_get_invbool(char *buffer, struct kernel_param *kp);
# 199 "include/linux/moduleparam.h"
extern int param_array_set(const char *val, struct kernel_param *kp);
extern int param_array_get(char *buffer, struct kernel_param *kp);

extern int param_set_copystring(const char *val, struct kernel_param *kp);
extern int param_get_string(char *buffer, struct kernel_param *kp);



struct module;


extern int module_param_sysfs_setup(struct module *mod,
        struct kernel_param *kparam,
        unsigned int num_params);

extern void module_param_sysfs_remove(struct module *mod);
# 18 "include/linux/module.h" 2
# 1 "include/linux/marker.h" 1
# 18 "include/linux/marker.h"
struct module;
struct marker;
# 33 "include/linux/marker.h"
typedef void marker_probe_func(void *probe_private, void *call_private,
  const char *fmt, va_list *args);

struct marker_probe_closure {
 marker_probe_func *func;
 void *probe_private;
};

struct marker {
 const char *name;
 const char *format;


 char state;
 char ptype;

 void (*call)(const struct marker *mdata, void *call_private, ...);
 struct marker_probe_closure single;
 struct marker_probe_closure *multi;
 const char *tp_name;
 void *tp_cb;
} __attribute__((aligned(8)));
# 107 "include/linux/marker.h"
extern void marker_update_probe_range(struct marker *begin,
 struct marker *end);
# 176 "include/linux/marker.h"
static inline __attribute__((always_inline)) void __attribute__((format(printf,1,2))) ___mark_check_format(const char *fmt, ...)
{
}







extern marker_probe_func __mark_empty_function;

extern void marker_probe_cb(const struct marker *mdata,
 void *call_private, ...);





extern int marker_probe_register(const char *name, const char *format,
    marker_probe_func *probe, void *probe_private);




extern int marker_probe_unregister(const char *name,
 marker_probe_func *probe, void *probe_private);



extern int marker_probe_unregister_private_data(marker_probe_func *probe,
 void *probe_private);

extern void *marker_get_private_data(const char *name, marker_probe_func *probe,
 int num);
# 19 "include/linux/module.h" 2
# 1 "include/linux/tracepoint.h" 1
# 20 "include/linux/tracepoint.h"
struct module;
struct tracepoint;

struct tracepoint {
 const char *name;
 int state;
 void **funcs;
} __attribute__((aligned(32)));
# 91 "include/linux/tracepoint.h"
extern void tracepoint_update_probe_range(struct tracepoint *begin,
 struct tracepoint *end);
# 122 "include/linux/tracepoint.h"
extern int tracepoint_probe_register(const char *name, void *probe);





extern int tracepoint_probe_unregister(const char *name, void *probe);

extern int tracepoint_probe_register_noupdate(const char *name, void *probe);
extern int tracepoint_probe_unregister_noupdate(const char *name, void *probe);
extern void tracepoint_probe_update_all(void);

struct tracepoint_iter {
 struct module *module;
 struct tracepoint *tracepoint;
};

extern void tracepoint_iter_start(struct tracepoint_iter *iter);
extern void tracepoint_iter_next(struct tracepoint_iter *iter);
extern void tracepoint_iter_stop(struct tracepoint_iter *iter);
extern void tracepoint_iter_reset(struct tracepoint_iter *iter);
extern int tracepoint_get_iter_range(struct tracepoint **tracepoint,
 struct tracepoint *begin, struct tracepoint *end);






static inline __attribute__((always_inline)) void tracepoint_synchronize_unregister(void)
{
 synchronize_rcu();
}
# 20 "include/linux/module.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h" 1






# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 8 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h" 2


typedef struct {
 atomic_long_t a;
} local_t;






static inline __attribute__((always_inline)) void local_inc(local_t *l)
{
 asm volatile(" " "incq" " " "%0"
       : "+m" (l->a.counter));
}

static inline __attribute__((always_inline)) void local_dec(local_t *l)
{
 asm volatile(" " "decq" " " "%0"
       : "+m" (l->a.counter));
}

static inline __attribute__((always_inline)) void local_add(long i, local_t *l)
{
 asm volatile(" " "addq" " " "%1,%0"
       : "+m" (l->a.counter)
       : "ir" (i));
}

static inline __attribute__((always_inline)) void local_sub(long i, local_t *l)
{
 asm volatile(" " "subq" " " "%1,%0"
       : "+m" (l->a.counter)
       : "ir" (i));
}
# 54 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h"
static inline __attribute__((always_inline)) int local_sub_and_test(long i, local_t *l)
{
 unsigned char c;

 asm volatile(" " "subq" " " "%2,%0; sete %1"
       : "+m" (l->a.counter), "=qm" (c)
       : "ir" (i) : "memory");
 return c;
}
# 72 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h"
static inline __attribute__((always_inline)) int local_dec_and_test(local_t *l)
{
 unsigned char c;

 asm volatile(" " "decq" " " "%0; sete %1"
       : "+m" (l->a.counter), "=qm" (c)
       : : "memory");
 return c != 0;
}
# 90 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h"
static inline __attribute__((always_inline)) int local_inc_and_test(local_t *l)
{
 unsigned char c;

 asm volatile(" " "incq" " " "%0; sete %1"
       : "+m" (l->a.counter), "=qm" (c)
       : : "memory");
 return c != 0;
}
# 109 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h"
static inline __attribute__((always_inline)) int local_add_negative(long i, local_t *l)
{
 unsigned char c;

 asm volatile(" " "addq" " " "%2,%0; sets %1"
       : "+m" (l->a.counter), "=qm" (c)
       : "ir" (i) : "memory");
 return c;
}
# 126 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h"
static inline __attribute__((always_inline)) long local_add_return(long i, local_t *l)
{
 long __i;






 __i = i;
 asm volatile(" " "xaddq" " " "%0, %1;"
       : "+r" (i), "+m" (l->a.counter)
       : : "memory");
 return i + __i;
# 149 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/local.h"
}

static inline __attribute__((always_inline)) long local_sub_return(long i, local_t *l)
{
 return local_add_return(-i, l);
}
# 21 "include/linux/module.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/module.h" 1




struct mod_arch_specific {};
# 23 "include/linux/module.h" 2
# 34 "include/linux/module.h"
struct kernel_symbol
{
 unsigned long value;
 const char *name;
};

struct modversion_info
{
 unsigned long crc;
 char name[(64 - sizeof(unsigned long))];
};

struct module;

struct module_attribute {
        struct attribute attr;
        ssize_t (*show)(struct module_attribute *, struct module *, char *);
        ssize_t (*store)(struct module_attribute *, struct module *,
    const char *, size_t count);
 void (*setup)(struct module *, const char *);
 int (*test)(struct module *);
 void (*free)(struct module *);
};

struct module_kobject
{
 struct kobject kobj;
 struct module *mod;
 struct kobject *drivers_dir;
 struct module_param_attrs *mp;
};


extern int init_module(void);
extern void cleanup_module(void);


struct exception_table_entry;

const struct exception_table_entry *
search_extable(const struct exception_table_entry *first,
        const struct exception_table_entry *last,
        unsigned long value);
void sort_extable(struct exception_table_entry *start,
    struct exception_table_entry *finish);
void sort_main_extable(void);






extern struct module __this_module;
# 165 "include/linux/module.h"
const struct exception_table_entry *search_exception_tables(unsigned long add);

struct notifier_block;




void *__symbol_get(const char *symbol);
void *__symbol_get_gpl(const char *symbol);
# 222 "include/linux/module.h"
enum module_state
{
 MODULE_STATE_LIVE,
 MODULE_STATE_COMING,
 MODULE_STATE_GOING,
};

struct module
{
 enum module_state state;


 struct list_head list;


 char name[(64 - sizeof(unsigned long))];


 struct module_kobject mkobj;
 struct module_attribute *modinfo_attrs;
 const char *version;
 const char *srcversion;
 struct kobject *holders_dir;


 const struct kernel_symbol *syms;
 const unsigned long *crcs;
 unsigned int num_syms;


 unsigned int num_gpl_syms;
 const struct kernel_symbol *gpl_syms;
 const unsigned long *gpl_crcs;
# 269 "include/linux/module.h"
 const struct kernel_symbol *gpl_future_syms;
 const unsigned long *gpl_future_crcs;
 unsigned int num_gpl_future_syms;


 unsigned int num_exentries;
 struct exception_table_entry *extable;


 int (*init)(void);


 void *module_init;


 void *module_core;


 unsigned int init_size, core_size;


 unsigned int init_text_size, core_text_size;


 struct mod_arch_specific arch;

 unsigned int taints;



 unsigned num_bugs;
 struct list_head bug_list;
 struct bug_entry *bug_table;




 Elf64_Sym *symtab;
 unsigned int num_symtab;
 char *strtab;


 struct module_sect_attrs *sect_attrs;


 struct module_notes_attrs *notes_attrs;



 void *percpu;



 char *args;

 struct marker *markers;
 unsigned int num_markers;


 struct tracepoint *tracepoints;
 unsigned int num_tracepoints;




 struct list_head modules_which_use_me;


 struct task_struct *waiter;


 void (*exit)(void);


 char *refptr;




};







static inline __attribute__((always_inline)) int module_is_live(struct module *mod)
{
 return mod->state != MODULE_STATE_GOING;
}


struct module *module_text_address(unsigned long addr);
struct module *__module_text_address(unsigned long addr);
int is_module_address(unsigned long addr);

static inline __attribute__((always_inline)) int within_module_core(unsigned long addr, struct module *mod)
{
 return (unsigned long)mod->module_core <= addr &&
        addr < (unsigned long)mod->module_core + mod->core_size;
}

static inline __attribute__((always_inline)) int within_module_init(unsigned long addr, struct module *mod)
{
 return (unsigned long)mod->module_init <= addr &&
        addr < (unsigned long)mod->module_init + mod->init_size;
}



int module_get_kallsym(unsigned int symnum, unsigned long *value, char *type,
   char *name, char *module_name, int *exported);


unsigned long module_kallsyms_lookup_name(const char *name);

extern void __module_put_and_exit(struct module *mod, long code)
 __attribute__((noreturn));



unsigned int module_refcount(struct module *mod);
void __symbol_put(const char *symbol);

void symbol_put_addr(void *addr);

static inline __attribute__((always_inline)) local_t *__module_ref_addr(struct module *mod, int cpu)
{

 return (local_t *) (mod->refptr + (((_cpu_pda[cpu])->data_offset)));



}



static inline __attribute__((always_inline)) void __module_get(struct module *module)
{
 if (module) {
  local_inc(__module_ref_addr(module, ({ do { } while (0); ({ typeof(_proxy_pda.cpunumber) ret__; switch (sizeof(_proxy_pda.cpunumber)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,cpunumber)), "m" (_proxy_pda.cpunumber)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,cpunumber)), "m" (_proxy_pda.cpunumber)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,cpunumber)), "m" (_proxy_pda.cpunumber)); break; default: __bad_pda_field(); } ret__; }); })));
  do { } while (0);
 }
}

static inline __attribute__((always_inline)) int try_module_get(struct module *module)
{
 int ret = 1;

 if (module) {
  unsigned int cpu = ({ do { } while (0); ({ typeof(_proxy_pda.cpunumber) ret__; switch (sizeof(_proxy_pda.cpunumber)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,cpunumber)), "m" (_proxy_pda.cpunumber)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,cpunumber)), "m" (_proxy_pda.cpunumber)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,cpunumber)), "m" (_proxy_pda.cpunumber)); break; default: __bad_pda_field(); } ret__; }); });
  if (__builtin_expect(!!(module_is_live(module)), 1))
   local_inc(__module_ref_addr(module, cpu));
  else
   ret = 0;
  do { } while (0);
 }
 return ret;
}

extern void module_put(struct module *module);
# 458 "include/linux/module.h"
const char *module_address_lookup(unsigned long addr,
       unsigned long *symbolsize,
       unsigned long *offset,
       char **modname,
       char *namebuf);
int lookup_module_symbol_name(unsigned long addr, char *symname);
int lookup_module_symbol_attrs(unsigned long addr, unsigned long *size, unsigned long *offset, char *modname, char *name);


const struct exception_table_entry *search_module_extables(unsigned long addr);

int register_module_notifier(struct notifier_block * nb);
int unregister_module_notifier(struct notifier_block * nb);

extern void print_modules(void);

extern void module_update_markers(void);

extern void module_update_tracepoints(void);
extern int module_get_iter_tracepoints(struct tracepoint_iter *iter);
# 594 "include/linux/module.h"
struct device_driver;

struct module;

extern struct kset *module_kset;
extern struct kobj_type module_ktype;
extern int module_sysfs_initialized;

int mod_sysfs_init(struct module *mod);
int mod_sysfs_setup(struct module *mod,
      struct kernel_param *kparam,
      unsigned int num_params);
int module_add_modinfo_attrs(struct module *mod);
void module_remove_modinfo_attrs(struct module *mod);
# 23 "include/linux/device.h" 2

# 1 "include/linux/semaphore.h" 1
# 16 "include/linux/semaphore.h"
struct semaphore {
 spinlock_t lock;
 unsigned int count;
 struct list_head wait_list;
};
# 32 "include/linux/semaphore.h"
static inline __attribute__((always_inline)) void sema_init(struct semaphore *sem, int val)
{
 static struct lock_class_key __key;
 *sem = (struct semaphore) { .lock = (spinlock_t) { .raw_lock = { 0 }, .magic = 0xdead4ead, .owner = ((void *)-1L), .owner_cpu = -1, .dep_map = { .name = "(*sem).lock" } }, .count = val, .wait_list = { &((*sem).wait_list), &((*sem).wait_list) }, };
 lockdep_init_map(&sem->lock.dep_map, "semaphore->lock", &__key, 0);
}




extern void down(struct semaphore *sem);
extern int __attribute__((warn_unused_result)) down_interruptible(struct semaphore *sem);
extern int __attribute__((warn_unused_result)) down_killable(struct semaphore *sem);
extern int __attribute__((warn_unused_result)) down_trylock(struct semaphore *sem);
extern int __attribute__((warn_unused_result)) down_timeout(struct semaphore *sem, long jiffies);
extern void up(struct semaphore *sem);
# 25 "include/linux/device.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 26 "include/linux/device.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/device.h" 1



struct dev_archdata {

 void *acpi_handle;


struct dma_mapping_ops *dma_ops;


 void *iommu;

};
# 27 "include/linux/device.h" 2



struct device;
struct device_driver;
struct driver_private;
struct class;
struct class_private;
struct bus_type;
struct bus_type_private;

struct bus_attribute {
 struct attribute attr;
 ssize_t (*show)(struct bus_type *bus, char *buf);
 ssize_t (*store)(struct bus_type *bus, const char *buf, size_t count);
};




extern int __attribute__((warn_unused_result)) bus_create_file(struct bus_type *,
     struct bus_attribute *);
extern void bus_remove_file(struct bus_type *, struct bus_attribute *);

struct bus_type {
 const char *name;
 struct bus_attribute *bus_attrs;
 struct device_attribute *dev_attrs;
 struct driver_attribute *drv_attrs;

 int (*match)(struct device *dev, struct device_driver *drv);
 int (*uevent)(struct device *dev, struct kobj_uevent_env *env);
 int (*probe)(struct device *dev);
 int (*remove)(struct device *dev);
 void (*shutdown)(struct device *dev);

 int (*suspend)(struct device *dev, pm_message_t state);
 int (*suspend_late)(struct device *dev, pm_message_t state);
 int (*resume_early)(struct device *dev);
 int (*resume)(struct device *dev);

 struct dev_pm_ops *pm;

 struct bus_type_private *p;
};

extern int __attribute__((warn_unused_result)) bus_register(struct bus_type *bus);
extern void bus_unregister(struct bus_type *bus);

extern int __attribute__((warn_unused_result)) bus_rescan_devices(struct bus_type *bus);



int bus_for_each_dev(struct bus_type *bus, struct device *start, void *data,
       int (*fn)(struct device *dev, void *data));
struct device *bus_find_device(struct bus_type *bus, struct device *start,
          void *data,
          int (*match)(struct device *dev, void *data));
struct device *bus_find_device_by_name(struct bus_type *bus,
           struct device *start,
           const char *name);

int __attribute__((warn_unused_result)) bus_for_each_drv(struct bus_type *bus,
      struct device_driver *start, void *data,
      int (*fn)(struct device_driver *, void *));

void bus_sort_breadthfirst(struct bus_type *bus,
      int (*compare)(const struct device *a,
       const struct device *b));






struct notifier_block;

extern int bus_register_notifier(struct bus_type *bus,
     struct notifier_block *nb);
extern int bus_unregister_notifier(struct bus_type *bus,
       struct notifier_block *nb);
# 119 "include/linux/device.h"
extern struct kset *bus_get_kset(struct bus_type *bus);
extern struct klist *bus_get_device_klist(struct bus_type *bus);

struct device_driver {
    const char *__attribute__((nullterm)) name;
 struct bus_type *bus;

 struct module *owner;
 const char *mod_name;

 int (*probe) (struct device *dev);
 int (*remove) (struct device *dev);
 void (*shutdown) (struct device *dev);
 int (*suspend) (struct device *dev, pm_message_t state);
 int (*resume) (struct device *dev);
 struct attribute_group **groups;

 struct dev_pm_ops *pm;

 struct driver_private *p;
};


extern int __attribute__((warn_unused_result)) driver_register(struct device_driver *drv);
extern void driver_unregister(struct device_driver *drv);

extern struct device_driver *get_driver(struct device_driver *drv);
extern void put_driver(struct device_driver *drv);
extern struct device_driver *driver_find(const char *name,
      struct bus_type *bus);
extern int driver_probe_done(void);
extern int wait_for_device_probe(void);




struct driver_attribute {
 struct attribute attr;
 ssize_t (*show)(struct device_driver *driver, char *buf);
 ssize_t (*store)(struct device_driver *driver, const char *buf,
    size_t count);
};





extern int __attribute__((warn_unused_result)) driver_create_file(struct device_driver *driver,
        struct driver_attribute *attr);
extern void driver_remove_file(struct device_driver *driver,
          struct driver_attribute *attr);

extern int __attribute__((warn_unused_result)) driver_add_kobj(struct device_driver *drv,
     struct kobject *kobj,
     const char *fmt, ...);

extern int __attribute__((warn_unused_result)) driver_for_each_device(struct device_driver *drv,
            struct device *start,
            void *data,
            int (*fn)(struct device *dev,
        void *));
struct device *driver_find_device(struct device_driver *drv,
      struct device *start, void *data,
      int (*match)(struct device *dev, void *data));




struct class {
    const char *__attribute__((nullterm)) name;
 struct module *owner;

 struct class_attribute *class_attrs;
 struct device_attribute *dev_attrs;
 struct kobject *dev_kobj;

 int (*dev_uevent)(struct device *dev, struct kobj_uevent_env *env);

 void (*class_release)(struct class *class);
 void (*dev_release)(struct device *dev);

 int (*suspend)(struct device *dev, pm_message_t state);
 int (*resume)(struct device *dev);

 struct dev_pm_ops *pm;
 struct class_private *p;
};

struct class_dev_iter {
 struct klist_iter ki;
 const struct device_type *type;
};

extern struct kobject *sysfs_dev_block_kobj;
extern struct kobject *sysfs_dev_char_kobj;
extern int __attribute__((warn_unused_result)) __class_register(struct class *class,
      struct lock_class_key *key);
extern void class_unregister(struct class *class);
# 226 "include/linux/device.h"
extern void class_dev_iter_init(struct class_dev_iter *iter,
    struct class *class,
    struct device *start,
    const struct device_type *type);
extern struct device *class_dev_iter_next(struct class_dev_iter *iter);
extern void class_dev_iter_exit(struct class_dev_iter *iter);

extern int class_for_each_device(struct class *class, struct device *start,
     void *data,
     int (*fn)(struct device *dev, void *data));
extern struct device *class_find_device(struct class *class,
     struct device *start, void *data,
     int (*match)(struct device *, void *));

struct class_attribute {
 struct attribute attr;
 ssize_t (*show)(struct class *class, char *buf);
 ssize_t (*store)(struct class *class, const char *buf, size_t count);
};




extern int __attribute__((warn_unused_result)) class_create_file(struct class *class,
       const struct class_attribute *attr);
extern void class_remove_file(struct class *class,
         const struct class_attribute *attr);

struct class_interface {
 struct list_head node;
 struct class *class;

 int (*add_dev) (struct device *, struct class_interface *);
 void (*remove_dev) (struct device *, struct class_interface *);
};

extern int __attribute__((warn_unused_result)) class_interface_register(struct class_interface *);
extern void class_interface_unregister(struct class_interface *);

extern struct class * __attribute__((warn_unused_result)) __class_create(struct module *owner,
        const char *__attribute__((nullterm)) name,
        struct lock_class_key *key);
extern void class_destroy(struct class *cls);
# 287 "include/linux/device.h"
struct device_type {
 const char *name;
 struct attribute_group **groups;
 int (*uevent)(struct device *dev, struct kobj_uevent_env *env);
 void (*release)(struct device *dev);

 int (*suspend)(struct device *dev, pm_message_t state);
 int (*resume)(struct device *dev);

 struct dev_pm_ops *pm;
};


struct device_attribute {
 struct attribute attr;
 ssize_t (*show)(struct device *dev, struct device_attribute *attr,
   char *buf);
 ssize_t (*store)(struct device *dev, struct device_attribute *attr,
    const char *buf, size_t count);
};




extern int __attribute__((warn_unused_result)) device_create_file(struct device *device,
        struct device_attribute *entry);
extern void device_remove_file(struct device *dev,
          struct device_attribute *attr);
extern int __attribute__((warn_unused_result)) device_create_bin_file(struct device *dev,
            struct bin_attribute *attr);
extern void device_remove_bin_file(struct device *dev,
       struct bin_attribute *attr);
extern int device_schedule_callback_owner(struct device *dev,
  void (*func)(struct device *dev), struct module *owner);






typedef void (*dr_release_t)(struct device *dev, void *res);
typedef int (*dr_match_t)(struct device *dev, void *res, void *match_data);







extern void *devres_alloc(dr_release_t release, size_t size, gfp_t gfp);

extern void devres_free(void *res);
extern void devres_add(struct device *dev, void *res);
extern void *devres_find(struct device *dev, dr_release_t release,
    dr_match_t match, void *match_data);
extern void *devres_get(struct device *dev, void *new_res,
   dr_match_t match, void *match_data);
extern void *devres_remove(struct device *dev, dr_release_t release,
      dr_match_t match, void *match_data);
extern int devres_destroy(struct device *dev, dr_release_t release,
     dr_match_t match, void *match_data);


extern void * __attribute__((warn_unused_result)) devres_open_group(struct device *dev, void *id,
          gfp_t gfp);
extern void devres_close_group(struct device *dev, void *id);
extern void devres_remove_group(struct device *dev, void *id);
extern int devres_release_group(struct device *dev, void *id);


extern void *devm_kzalloc(struct device *dev, size_t size, gfp_t gfp);
extern void devm_kfree(struct device *dev, void *p);

struct device_dma_parameters {




 unsigned int max_segment_size;
 unsigned long segment_boundary_mask;
};

struct device {
 struct klist klist_children;
 struct klist_node knode_parent;
 struct klist_node knode_driver;
 struct klist_node knode_bus;
    struct device *__attribute__((recursive)) parent;

 struct kobject kobj;
 char bus_id[20];
 unsigned uevent_suppress:1;
 const char *init_name;
 struct device_type *type;

 struct semaphore sem;



 struct bus_type *bus;
 struct device_driver *driver;

    char * __attribute__((noderef, address_space(2))) __attribute__((recursive)) driver_data;
 void *platform_data;

 struct dev_pm_info power;


 int numa_node;

 u64 *dma_mask;
 u64 coherent_dma_mask;





 struct device_dma_parameters *dma_parms;

 struct list_head dma_pools;

 struct dma_coherent_mem *dma_mem;


 struct dev_archdata archdata;

 dev_t devt;

 spinlock_t devres_lock;
 struct list_head devres_head;

 struct klist_node knode_class;
 struct class *class;
 struct attribute_group **groups;

 void (*release)(struct device *dev);
};


# 1 "include/linux/pm_wakeup.h" 1
# 33 "include/linux/pm_wakeup.h"
static inline __attribute__((always_inline)) void device_init_wakeup(struct device *dev, int val)
{
 dev->power.can_wakeup = dev->power.should_wakeup = !!val;
}

static inline __attribute__((always_inline)) void device_set_wakeup_capable(struct device *dev, int val)
{
 dev->power.can_wakeup = !!val;
}

static inline __attribute__((always_inline)) int device_can_wakeup(struct device *dev)
{
 return dev->power.can_wakeup;
}

static inline __attribute__((always_inline)) void device_set_wakeup_enable(struct device *dev, int val)
{
 dev->power.should_wakeup = !!val;
}

static inline __attribute__((always_inline)) int device_may_wakeup(struct device *dev)
{
 return dev->power.can_wakeup && dev->power.should_wakeup;
}
# 427 "include/linux/device.h" 2

static inline __attribute__((always_inline)) const char *dev_name(const struct device *dev)
{

 return dev->bus_id;
}

extern int dev_set_name(struct device *dev, const char *__attribute__((nullterm)) name, ...)
    __attribute__((format(printf, 2, 3)));


static inline __attribute__((always_inline)) int dev_to_node(struct device *dev)
{
 return dev->numa_node;
}
static inline __attribute__((always_inline)) void set_dev_node(struct device *dev, int node)
{
 dev->numa_node = node;
}
# 456 "include/linux/device.h"
static inline __attribute__((always_inline)) void *dev_get_drvdata(const struct device *dev)
{
 return dev->driver_data;
}

static inline __attribute__((always_inline)) void dev_set_drvdata(struct device *dev, void *data)
{
 dev->driver_data = data;
}

static inline __attribute__((always_inline)) int device_is_registered(struct device *dev)
{
 return dev->kobj.state_in_sysfs;
}

void driver_init(void);




extern int __attribute__((warn_unused_result)) device_register(struct device *dev);
extern void device_unregister(struct device *dev);
extern void device_initialize(struct device *dev);
extern int __attribute__((warn_unused_result)) device_add(struct device *dev);
extern void device_del(struct device *dev);
extern int device_for_each_child(struct device *dev, void *data,
       int (*fn)(struct device *dev, void *data));
extern struct device *device_find_child(struct device *dev, void *data,
    int (*match)(struct device *dev, void *data));
extern int device_rename(struct device *dev, char *new_name);
extern int device_move(struct device *dev, struct device *new_parent);




extern struct device *__root_device_register(const char *name,
          struct module *owner);
static inline __attribute__((always_inline)) struct device *root_device_register(const char *name)
{
 return __root_device_register(name, (&__this_module));
}
extern void root_device_unregister(struct device *root);





extern int __attribute__((warn_unused_result)) device_bind_driver(struct device *dev);
extern void device_release_driver(struct device *dev);
extern int __attribute__((warn_unused_result)) device_attach(struct device *dev);
extern int __attribute__((warn_unused_result)) driver_attach(struct device_driver *drv);
extern int __attribute__((warn_unused_result)) device_reprobe(struct device *dev);




extern struct device *device_create_vargs(struct class *cls,
       struct device *parent,
       dev_t devt,
       void *drvdata,
       const char *fmt,
       va_list vargs);
extern struct device *device_create(struct class *cls, struct device *parent,
        dev_t devt, void *drvdata,
        const char *__attribute__((nullterm)) fmt, ...)
    __attribute__((format(printf, 5, 6)));
extern void device_destroy(struct class *cls, dev_t devt);







extern int (*platform_notify)(struct device *dev);

extern int (*platform_notify_remove)(struct device *dev);






extern struct device *get_device(struct device *dev);
extern void put_device(struct device *dev);



extern void device_shutdown(void);


extern void sysdev_shutdown(void);


extern const char *dev_driver_string(const struct device *dev);
# 54 "include/linux/pci.h" 2
# 1 "include/linux/io.h" 1
# 25 "include/linux/io.h"
struct device;

void __iowrite32_copy(void __attribute__((noderef, address_space(2))) *to, const void *from, size_t count);
void __iowrite64_copy(void __attribute__((noderef, address_space(2))) *to, const void *from, size_t count);


int ioremap_page_range(unsigned long addr, unsigned long end,
         unsigned long phys_addr, pgprot_t prot);
# 45 "include/linux/io.h"
void __attribute__((noderef, address_space(2))) * devm_ioport_map(struct device *dev, unsigned long port,
          unsigned int nr);
void devm_ioport_unmap(struct device *dev, void __attribute__((noderef, address_space(2))) *addr);
# 61 "include/linux/io.h"
void __attribute__((noderef, address_space(2))) *devm_ioremap(struct device *dev, resource_size_t offset,
       unsigned long size);
void __attribute__((noderef, address_space(2))) *devm_ioremap_nocache(struct device *dev, resource_size_t offset,
        unsigned long size);
void devm_iounmap(struct device *dev, void __attribute__((noderef, address_space(2))) *addr);
int check_signature(const volatile void __attribute__((noderef, address_space(2))) *io_addr,
   const unsigned char *signature, int length);
void devm_ioremap_release(struct device *dev, void *res);
# 55 "include/linux/pci.h" 2


# 1 "include/linux/pci_ids.h" 1
# 58 "include/linux/pci.h" 2


struct pci_slot {
 struct pci_bus *bus;
 struct list_head list;
 struct hotplug_slot *hotplug;
 unsigned char number;
 struct kobject kobj;
};

static inline __attribute__((always_inline)) const char *pci_slot_name(const struct pci_slot *slot)
{
 return kobject_name(&slot->kobj);
}


enum pci_mmap_state {
 pci_mmap_io,
 pci_mmap_mem
};
# 88 "include/linux/pci.h"
enum {

 PCI_STD_RESOURCES,
 PCI_STD_RESOURCE_END = 5,


 PCI_ROM_RESOURCE,




 PCI_BRIDGE_RESOURCES,
 PCI_BRIDGE_RESOURCE_END = PCI_BRIDGE_RESOURCES +
      4 - 1,


 PCI_NUM_RESOURCES,


 DEVICE_COUNT_RESOURCE
};

typedef int pci_power_t;
# 128 "include/linux/pci.h"
typedef unsigned int pci_channel_state_t;

enum pci_channel_state {

 pci_channel_io_normal = ( pci_channel_state_t) 1,


 pci_channel_io_frozen = ( pci_channel_state_t) 2,


 pci_channel_io_perm_failure = ( pci_channel_state_t) 3,
};

typedef unsigned int pcie_reset_state_t;

enum pcie_reset_state {

 pcie_deassert_reset = ( pcie_reset_state_t) 1,


 pcie_warm_reset = ( pcie_reset_state_t) 2,


 pcie_hot_reset = ( pcie_reset_state_t) 3
};

typedef unsigned short pci_dev_flags_t;
enum pci_dev_flags {



 PCI_DEV_FLAGS_MSI_INTX_DISABLE_BUG = ( pci_dev_flags_t) 1,

 PCI_DEV_FLAGS_NO_D3 = ( pci_dev_flags_t) 2,
};

enum pci_irq_reroute_variant {
 INTEL_IRQ_REROUTE_VARIANT = 1,
 MAX_IRQ_REROUTE_VARIANTS = 3
};

typedef unsigned short pci_bus_flags_t;
enum pci_bus_flags {
 PCI_BUS_FLAGS_NO_MSI = ( pci_bus_flags_t) 1,
 PCI_BUS_FLAGS_NO_MMRBC = ( pci_bus_flags_t) 2,
};

struct pci_cap_saved_state {
 struct hlist_node next;
 char cap_nr;
 u32 data[0];
};

struct pcie_link_state;
struct pci_vpd;




struct pci_dev {
 struct list_head bus_list;
 struct pci_bus *bus;
 struct pci_bus *subordinate;

 void *sysdata;
 struct proc_dir_entry *procent;
 struct pci_slot *slot;

 unsigned int devfn;
 unsigned short vendor;
 unsigned short device;
 unsigned short subsystem_vendor;
 unsigned short subsystem_device;
 unsigned int class;
 u8 revision;
 u8 hdr_type;
 u8 pcie_type;
 u8 rom_base_reg;
 u8 pin;

 struct pci_driver *driver;
 u64 dma_mask;





 struct device_dma_parameters dma_parms;

 pci_power_t current_state;


 int pm_cap;

 unsigned int pme_support:5;

 unsigned int d1_support:1;
 unsigned int d2_support:1;
 unsigned int no_d1d2:1;





 pci_channel_state_t error_state;
 struct device dev;

 int cfg_size;





 unsigned int irq;
 struct resource resource[DEVICE_COUNT_RESOURCE];


 unsigned int transparent:1;
 unsigned int multifunction:1;

 unsigned int is_added:1;
 unsigned int is_busmaster:1;
 unsigned int no_msi:1;
 unsigned int block_ucfg_access:1;
 unsigned int broken_parity_status:1;
 unsigned int irq_reroute_variant:2;
 unsigned int msi_enabled:1;
 unsigned int msix_enabled:1;
 unsigned int ari_enabled:1;
 unsigned int is_managed:1;
 unsigned int is_pcie:1;
 unsigned int state_saved:1;
 pci_dev_flags_t dev_flags;
 atomic_t enable_cnt;

 u32 saved_config_space[16];
 struct hlist_head saved_cap_space;
 struct bin_attribute *rom_attr;
 int rom_attr_enabled;
 struct bin_attribute *res_attr[DEVICE_COUNT_RESOURCE];
 struct bin_attribute *res_attr_wc[DEVICE_COUNT_RESOURCE];

 struct list_head msi_list;

 struct pci_vpd *vpd;
};

extern struct pci_dev *alloc_pci_dev(void);





static inline __attribute__((always_inline)) int pci_channel_offline(struct pci_dev *pdev)
{
 return (pdev->error_state != pci_channel_io_normal);
}

static inline __attribute__((always_inline)) struct pci_cap_saved_state *pci_find_saved_cap(
 struct pci_dev *pci_dev, char cap)
{
 struct pci_cap_saved_state *tmp;
 struct hlist_node *pos;

 for (pos = (&pci_dev->saved_cap_space)->first; pos && ({ __builtin_prefetch(pos->next); 1;}) && ({ tmp = ({ const typeof( ((typeof(*tmp) *)0)->next ) *__mptr = (pos); (typeof(*tmp) *)( (char *)__mptr - __builtin_offsetof(typeof(*tmp),next) );}); 1;}); pos = pos->next) {
  if (tmp->cap_nr == cap)
   return tmp;
 }
 return ((void *)0);
}

static inline __attribute__((always_inline)) void pci_add_saved_cap(struct pci_dev *pci_dev,
 struct pci_cap_saved_state *new_cap)
{
 hlist_add_head(&new_cap->next, &pci_dev->saved_cap_space);
}







struct pci_bus {
 struct list_head node;
 struct pci_bus *parent;
 struct list_head children;
 struct list_head devices;
 struct pci_dev *self;
 struct list_head slots;
 struct resource *resource[16];


 struct pci_ops *ops;
 void *sysdata;
 struct proc_dir_entry *procdir;

 unsigned char number;
 unsigned char primary;
 unsigned char secondary;
 unsigned char subordinate;

 char name[48];

 unsigned short bridge_ctl;
 pci_bus_flags_t bus_flags;
 struct device *bridge;
 struct device dev;
 struct bin_attribute *legacy_io;
 struct bin_attribute *legacy_mem;
 unsigned int is_added:1;
};





static inline __attribute__((always_inline)) bool pci_dev_msi_enabled(struct pci_dev *pci_dev)
{
 return pci_dev->msi_enabled || pci_dev->msix_enabled;
}
# 366 "include/linux/pci.h"
struct pci_ops {
 int (*read)(struct pci_bus *bus, unsigned int devfn, int where, int size, u32 *val);
 int (*write)(struct pci_bus *bus, unsigned int devfn, int where, int size, u32 val);
};





extern int raw_pci_read(unsigned int domain, unsigned int bus,
   unsigned int devfn, int reg, int len, u32 *val);
extern int raw_pci_write(unsigned int domain, unsigned int bus,
   unsigned int devfn, int reg, int len, u32 val);

struct pci_bus_region {
 resource_size_t start;
 resource_size_t end;
};

struct pci_dynids {
 spinlock_t lock;
 struct list_head list;
};
# 397 "include/linux/pci.h"
typedef unsigned int pci_ers_result_t;

enum pci_ers_result {

 PCI_ERS_RESULT_NONE = ( pci_ers_result_t) 1,


 PCI_ERS_RESULT_CAN_RECOVER = ( pci_ers_result_t) 2,


 PCI_ERS_RESULT_NEED_RESET = ( pci_ers_result_t) 3,


 PCI_ERS_RESULT_DISCONNECT = ( pci_ers_result_t) 4,


 PCI_ERS_RESULT_RECOVERED = ( pci_ers_result_t) 5,
};


struct pci_error_handlers {

 pci_ers_result_t (*error_detected)(struct pci_dev *dev,
        enum pci_channel_state error);


 pci_ers_result_t (*mmio_enabled)(struct pci_dev *dev);


 pci_ers_result_t (*link_reset)(struct pci_dev *dev);


 pci_ers_result_t (*slot_reset)(struct pci_dev *dev);


 void (*resume)(struct pci_dev *dev);
};



struct module;
struct pci_driver {
 struct list_head node;
 char *name;
 const struct pci_device_id *id_table;
 int (*probe) (struct pci_dev *dev, const struct pci_device_id *id);
 void (*remove) (struct pci_dev *dev);
 int (*suspend) (struct pci_dev *dev, pm_message_t state);
 int (*suspend_late) (struct pci_dev *dev, pm_message_t state);
 int (*resume_early) (struct pci_dev *dev);
 int (*resume) (struct pci_dev *dev);
 void (*shutdown) (struct pci_dev *dev);
 struct pci_error_handlers *err_handler;
 struct device_driver driver;
 struct pci_dynids dynids;
};
# 511 "include/linux/pci.h"
extern struct bus_type pci_bus_type;



extern struct list_head pci_root_buses;

extern int no_pci_devices(void);

void pcibios_fixup_bus(struct pci_bus *);
int __attribute__((warn_unused_result)) pcibios_enable_device(struct pci_dev *, int mask);
char *pcibios_setup(char *str);


void pcibios_align_resource(void *, struct resource *, resource_size_t,
    resource_size_t);
void pcibios_update_irq(struct pci_dev *, int irq);



extern struct pci_bus *pci_find_bus(int domain, int busnr);
void pci_bus_add_devices(struct pci_bus *bus);
struct pci_bus *pci_scan_bus_parented(struct device *parent, int bus,
          struct pci_ops *ops, void *sysdata);
static inline __attribute__((always_inline)) struct pci_bus * __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) pci_scan_bus(int bus, struct pci_ops *ops,
        void *sysdata)
{
 struct pci_bus *root_bus;
 root_bus = pci_scan_bus_parented(((void *)0), bus, ops, sysdata);
 if (root_bus)
  pci_bus_add_devices(root_bus);
 return root_bus;
}
struct pci_bus *pci_create_bus(struct device *parent, int bus,
          struct pci_ops *ops, void *sysdata);
struct pci_bus *pci_add_new_bus(struct pci_bus *parent, struct pci_dev *dev,
    int busnr);
struct pci_slot *pci_create_slot(struct pci_bus *parent, int slot_nr,
     const char *name,
     struct hotplug_slot *hotplug);
void pci_destroy_slot(struct pci_slot *slot);
void pci_renumber_slot(struct pci_slot *slot, int slot_nr);
int pci_scan_slot(struct pci_bus *bus, int devfn);
struct pci_dev *pci_scan_single_device(struct pci_bus *bus, int devfn);
void pci_device_add(struct pci_dev *dev, struct pci_bus *bus);
unsigned int pci_scan_child_bus(struct pci_bus *bus);
int __attribute__((warn_unused_result)) pci_bus_add_device(struct pci_dev *dev);
void pci_read_bridge_bases(struct pci_bus *child);
struct resource *pci_find_parent_resource(const struct pci_dev *dev,
       struct resource *res);
u8 pci_swizzle_interrupt_pin(struct pci_dev *dev, u8 pin);
int pci_get_interrupt_pin(struct pci_dev *dev, struct pci_dev **bridge);
u8 pci_common_swizzle(struct pci_dev *dev, u8 *pinp);
extern struct pci_dev *pci_dev_get(struct pci_dev *dev);
extern void pci_dev_put(struct pci_dev *dev);
extern void pci_remove_bus(struct pci_bus *b);
extern void pci_remove_bus_device(struct pci_dev *dev);
extern void pci_stop_bus_device(struct pci_dev *dev);
void pci_setup_cardbus(struct pci_bus *bus);
extern void pci_sort_breadthfirst(void);




struct pci_dev __attribute__((deprecated)) *pci_find_device(unsigned int vendor,
          unsigned int device,
          struct pci_dev *from);
struct pci_dev __attribute__((deprecated)) *pci_find_slot(unsigned int bus,
        unsigned int devfn);


enum pci_lost_interrupt_reason {
 PCI_LOST_IRQ_NO_INFORMATION = 0,
 PCI_LOST_IRQ_DISABLE_MSI,
 PCI_LOST_IRQ_DISABLE_MSIX,
 PCI_LOST_IRQ_DISABLE_ACPI,
};
enum pci_lost_interrupt_reason pci_lost_interrupt(struct pci_dev *dev);
int pci_find_capability(struct pci_dev *dev, int cap);
int pci_find_next_capability(struct pci_dev *dev, u8 pos, int cap);
int pci_find_ext_capability(struct pci_dev *dev, int cap);
int pci_find_ht_capability(struct pci_dev *dev, int ht_cap);
int pci_find_next_ht_capability(struct pci_dev *dev, int pos, int ht_cap);
struct pci_bus *pci_find_next_bus(const struct pci_bus *from);

struct pci_dev *pci_get_device(unsigned int vendor, unsigned int device,
    struct pci_dev *from);
struct pci_dev *pci_get_subsys(unsigned int vendor, unsigned int device,
    unsigned int ss_vendor, unsigned int ss_device,
    struct pci_dev *from);
struct pci_dev *pci_get_slot(struct pci_bus *bus, unsigned int devfn);
struct pci_dev *pci_get_bus_and_slot(unsigned int bus, unsigned int devfn);
struct pci_dev *pci_get_class(unsigned int class, struct pci_dev *from);
int pci_dev_present(const struct pci_device_id *ids);

int pci_bus_read_config_byte(struct pci_bus *bus, unsigned int devfn,
        int where, u8 *val);
int pci_bus_read_config_word(struct pci_bus *bus, unsigned int devfn,
        int where, u16 *val);
int pci_bus_read_config_dword(struct pci_bus *bus, unsigned int devfn,
         int where, u32 *val);
int pci_bus_write_config_byte(struct pci_bus *bus, unsigned int devfn,
         int where, u8 val);
int pci_bus_write_config_word(struct pci_bus *bus, unsigned int devfn,
         int where, u16 val);
int pci_bus_write_config_dword(struct pci_bus *bus, unsigned int devfn,
          int where, u32 val);

static inline __attribute__((always_inline)) int pci_read_config_byte(struct pci_dev *dev, int where, u8 *val)
{
 return pci_bus_read_config_byte(dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_read_config_word(struct pci_dev *dev, int where, u16 *val)
{
 return pci_bus_read_config_word(dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_read_config_dword(struct pci_dev *dev, int where,
     u32 *val)
{
 return pci_bus_read_config_dword(dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_write_config_byte(struct pci_dev *dev, int where, u8 val)
{
 return pci_bus_write_config_byte(dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_write_config_word(struct pci_dev *dev, int where, u16 val)
{
 return pci_bus_write_config_word(dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_write_config_dword(struct pci_dev *dev, int where,
      u32 val)
{
 return pci_bus_write_config_dword(dev->bus, dev->devfn, where, val);
}

int __attribute__((warn_unused_result)) pci_enable_device(struct pci_dev *dev);
int __attribute__((warn_unused_result)) pci_enable_device_io(struct pci_dev *dev);
int __attribute__((warn_unused_result)) pci_enable_device_mem(struct pci_dev *dev);
int __attribute__((warn_unused_result)) pci_reenable_device(struct pci_dev *);
int __attribute__((warn_unused_result)) pcim_enable_device(struct pci_dev *pdev);
void pcim_pin_device(struct pci_dev *pdev);

static inline __attribute__((always_inline)) int pci_is_managed(struct pci_dev *pdev)
{
 return pdev->is_managed;
}

void pci_disable_device(struct pci_dev *dev);
void pci_set_master(struct pci_dev *dev);
void pci_clear_master(struct pci_dev *dev);
int pci_set_pcie_reset_state(struct pci_dev *dev, enum pcie_reset_state state);

int __attribute__((warn_unused_result)) pci_set_mwi(struct pci_dev *dev);
int pci_try_set_mwi(struct pci_dev *dev);
void pci_clear_mwi(struct pci_dev *dev);
void pci_intx(struct pci_dev *dev, int enable);
void pci_msi_off(struct pci_dev *dev);
int pci_set_dma_mask(struct pci_dev *dev, u64 mask);
int pci_set_consistent_dma_mask(struct pci_dev *dev, u64 mask);
int pci_set_dma_max_seg_size(struct pci_dev *dev, unsigned int size);
int pci_set_dma_seg_boundary(struct pci_dev *dev, unsigned long mask);
int pcix_get_max_mmrbc(struct pci_dev *dev);
int pcix_get_mmrbc(struct pci_dev *dev);
int pcix_set_mmrbc(struct pci_dev *dev, int mmrbc);
int pcie_get_readrq(struct pci_dev *dev);
int pcie_set_readrq(struct pci_dev *dev, int rq);
int pci_reset_function(struct pci_dev *dev);
int pci_execute_reset_function(struct pci_dev *dev);
void pci_update_resource(struct pci_dev *dev, int resno);
int __attribute__((warn_unused_result)) pci_assign_resource(struct pci_dev *dev, int i);
int pci_select_bars(struct pci_dev *dev, unsigned long flags);


int pci_enable_rom(struct pci_dev *pdev);
void pci_disable_rom(struct pci_dev *pdev);
void __attribute__((noderef, address_space(2))) __attribute__((warn_unused_result)) *pci_map_rom(struct pci_dev *pdev, size_t *size);
void pci_unmap_rom(struct pci_dev *pdev, void __attribute__((noderef, address_space(2))) *rom);
size_t pci_get_rom_size(struct pci_dev *pdev, void __attribute__((noderef, address_space(2))) *rom, size_t size);


int pci_save_state(struct pci_dev *dev);
int pci_restore_state(struct pci_dev *dev);
int pci_set_power_state(struct pci_dev *dev, pci_power_t state);
pci_power_t pci_choose_state(struct pci_dev *dev, pm_message_t state);
bool pci_pme_capable(struct pci_dev *dev, pci_power_t state);
void pci_pme_active(struct pci_dev *dev, bool enable);
int pci_enable_wake(struct pci_dev *dev, pci_power_t state, int enable);
int pci_wake_from_d3(struct pci_dev *dev, bool enable);
pci_power_t pci_target_state(struct pci_dev *dev);
int pci_prepare_to_sleep(struct pci_dev *dev);
int pci_back_from_sleep(struct pci_dev *dev);


int pci_bus_find_capability(struct pci_bus *bus, unsigned int devfn, int cap);


ssize_t pci_read_vpd(struct pci_dev *dev, loff_t pos, size_t count, void *buf);
ssize_t pci_write_vpd(struct pci_dev *dev, loff_t pos, size_t count, const void *buf);
int pci_vpd_truncate(struct pci_dev *dev, size_t size);


void pci_bus_assign_resources(struct pci_bus *bus);
void pci_bus_size_bridges(struct pci_bus *bus);
int pci_claim_resource(struct pci_dev *, int);
void pci_assign_unassigned_resources(void);
void pdev_enable_device(struct pci_dev *);
void pdev_sort_resources(struct pci_dev *, struct resource_list *);
int pci_enable_resources(struct pci_dev *, int mask);
void pci_fixup_irqs(u8 (*)(struct pci_dev *, u8 *),
      int (*)(struct pci_dev *, u8, u8));

int __attribute__((warn_unused_result)) pci_request_regions(struct pci_dev *, const char *);
int __attribute__((warn_unused_result)) pci_request_regions_exclusive(struct pci_dev *, const char *);
void pci_release_regions(struct pci_dev *);
int __attribute__((warn_unused_result)) pci_request_region(struct pci_dev *, int, const char *);
int __attribute__((warn_unused_result)) pci_request_region_exclusive(struct pci_dev *, int, const char *);
void pci_release_region(struct pci_dev *, int);
int pci_request_selected_regions(struct pci_dev *, int, const char *);
int pci_request_selected_regions_exclusive(struct pci_dev *, int, const char *);
void pci_release_selected_regions(struct pci_dev *, int);


int __attribute__((warn_unused_result)) pci_bus_alloc_resource(struct pci_bus *bus,
   struct resource *res, resource_size_t size,
   resource_size_t align, resource_size_t min,
   unsigned int type_mask,
   void (*alignf)(void *, struct resource *,
    resource_size_t, resource_size_t),
   void *alignf_data);
void pci_enable_bridges(struct pci_bus *bus);


int __attribute__((warn_unused_result)) __pci_register_driver(struct pci_driver *, struct module *,
           const char *mod_name);







void pci_unregister_driver(struct pci_driver *dev);
void pci_remove_behind_bridge(struct pci_dev *dev);
struct pci_driver *pci_dev_driver(const struct pci_dev *dev);
const struct pci_device_id *pci_match_id(const struct pci_device_id *ids,
      struct pci_dev *dev);
int pci_scan_bridge(struct pci_bus *bus, struct pci_dev *dev, int max,
      int pass);

void pci_walk_bus(struct pci_bus *top, void (*cb)(struct pci_dev *, void *),
    void *userdata);
int pci_cfg_space_size_ext(struct pci_dev *dev);
int pci_cfg_space_size(struct pci_dev *dev);
unsigned char pci_bus_max_busnr(struct pci_bus *bus);



# 1 "include/linux/dmapool.h" 1
# 15 "include/linux/dmapool.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/scatterlist.h" 1





struct scatterlist {



 unsigned long page_link;
 unsigned int offset;
 unsigned int length;
 dma_addr_t dma_address;
 unsigned int dma_length;
};
# 16 "include/linux/dmapool.h" 2

struct dma_pool *dma_pool_create(const char *name, struct device *dev,
   size_t size, size_t align, size_t allocation);

void dma_pool_destroy(struct dma_pool *pool);

void *dma_pool_alloc(struct dma_pool *pool, gfp_t mem_flags,
       dma_addr_t *handle);

void dma_pool_free(struct dma_pool *pool, void *vaddr, dma_addr_t addr);




struct dma_pool *dmam_pool_create(const char *name, struct device *dev,
      size_t size, size_t align, size_t allocation);
void dmam_pool_destroy(struct dma_pool *pool);
# 768 "include/linux/pci.h" 2
# 776 "include/linux/pci.h"
enum pci_dma_burst_strategy {
 PCI_DMA_BURST_INFINITY,

 PCI_DMA_BURST_BOUNDARY,

 PCI_DMA_BURST_MULTIPLE,

};

struct msix_entry {
 u32 vector;
 u16 entry;
};
# 823 "include/linux/pci.h"
extern int pci_enable_msi(struct pci_dev *dev);
extern void pci_msi_shutdown(struct pci_dev *dev);
extern void pci_disable_msi(struct pci_dev *dev);
extern int pci_enable_msix(struct pci_dev *dev,
 struct msix_entry *entries, int nvec);
extern void pci_msix_shutdown(struct pci_dev *dev);
extern void pci_disable_msix(struct pci_dev *dev);
extern void msi_remove_pci_irq_vectors(struct pci_dev *dev);
extern void pci_restore_msi_state(struct pci_dev *dev);
extern int pci_msi_enabled(void);



static inline __attribute__((always_inline)) int pcie_aspm_enabled(void)
{
 return 0;
}






int ht_create_irq(struct pci_dev *dev, int idx);
void ht_destroy_irq(unsigned int irq);


extern void pci_block_user_cfg_access(struct pci_dev *dev);
extern void pci_unblock_user_cfg_access(struct pci_dev *dev);







extern int pci_domains_supported;
# 1057 "include/linux/pci.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci.h" 1



# 1 "include/linux/mm.h" 1
# 17 "include/linux/mm.h"
struct mempolicy;
struct anon_vma;
struct file_ra_state;
struct user_struct;
struct writeback_control;


extern unsigned long max_mapnr;


extern unsigned long num_physpages;
extern void * high_memory;
extern int page_cluster;


extern int sysctl_legacy_va_layout;




extern unsigned long mmap_min_addr;


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h" 1
# 174 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h"
extern pgprot_t pgprot_writecombine(pgprot_t prot);





extern unsigned long empty_zero_page[((1UL) << 12) / sizeof(unsigned long)];


extern spinlock_t pgd_lock;
extern struct list_head pgd_list;





static inline __attribute__((always_inline)) int pte_dirty(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 6);
}

static inline __attribute__((always_inline)) int pte_young(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 5);
}

static inline __attribute__((always_inline)) int pte_write(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 1);
}

static inline __attribute__((always_inline)) int pte_file(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 6);
}

static inline __attribute__((always_inline)) int pte_huge(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 7);
}

static inline __attribute__((always_inline)) int pte_global(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 8);
}

static inline __attribute__((always_inline)) int pte_exec(pte_t pte)
{
 return !(native_pte_flags(pte) & (((pteval_t)(1)) << 63));
}

static inline __attribute__((always_inline)) int pte_special(pte_t pte)
{
 return native_pte_flags(pte) & (((pteval_t)(1)) << 9);
}

static inline __attribute__((always_inline)) unsigned long pte_pfn(pte_t pte)
{
 return (native_pte_val(pte) & ((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1)))) >> 12;
}



static inline __attribute__((always_inline)) int pmd_large(pmd_t pte)
{
 return (native_pmd_val(pte) & ((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 0))) ==
  ((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 0));
}

static inline __attribute__((always_inline)) pte_t pte_mkclean(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) & ~(((pteval_t)(1)) << 6));
}

static inline __attribute__((always_inline)) pte_t pte_mkold(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) & ~(((pteval_t)(1)) << 5));
}

static inline __attribute__((always_inline)) pte_t pte_wrprotect(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) & ~(((pteval_t)(1)) << 1));
}

static inline __attribute__((always_inline)) pte_t pte_mkexec(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) & ~(((pteval_t)(1)) << 63));
}

static inline __attribute__((always_inline)) pte_t pte_mkdirty(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) | (((pteval_t)(1)) << 6));
}

static inline __attribute__((always_inline)) pte_t pte_mkyoung(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) | (((pteval_t)(1)) << 5));
}

static inline __attribute__((always_inline)) pte_t pte_mkwrite(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) | (((pteval_t)(1)) << 1));
}

static inline __attribute__((always_inline)) pte_t pte_mkhuge(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) | (((pteval_t)(1)) << 7));
}

static inline __attribute__((always_inline)) pte_t pte_clrhuge(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) & ~(((pteval_t)(1)) << 7));
}

static inline __attribute__((always_inline)) pte_t pte_mkglobal(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) | (((pteval_t)(1)) << 8));
}

static inline __attribute__((always_inline)) pte_t pte_clrglobal(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) & ~(((pteval_t)(1)) << 8));
}

static inline __attribute__((always_inline)) pte_t pte_mkspecial(pte_t pte)
{
 return native_make_pte(native_pte_val(pte) | (((pteval_t)(1)) << 9));
}

extern pteval_t __supported_pte_mask;





static inline __attribute__((always_inline)) pgprotval_t massage_pgprot(pgprot_t pgprot)
{
 pgprotval_t protval = ((pgprot).pgprot);

 if (protval & (((pteval_t)(1)) << 0))
  protval &= __supported_pte_mask;

 return protval;
}

static inline __attribute__((always_inline)) pte_t pfn_pte(unsigned long page_nr, pgprot_t pgprot)
{
 return native_make_pte(((phys_addr_t)page_nr << 12) | massage_pgprot(pgprot))
                              ;
}

static inline __attribute__((always_inline)) pmd_t pfn_pmd(unsigned long page_nr, pgprot_t pgprot)
{
 return native_make_pmd(((phys_addr_t)page_nr << 12) | massage_pgprot(pgprot))
                              ;
}

static inline __attribute__((always_inline)) pte_t pte_modify(pte_t pte, pgprot_t newprot)
{
 pteval_t val = native_pte_val(pte);





 val &= (((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))) | (((pteval_t)(1)) << 4) | (((pteval_t)(1)) << 3) | (((pteval_t)(1)) << 9) | (((pteval_t)(1)) << 5) | (((pteval_t)(1)) << 6));
 val |= massage_pgprot(newprot) & ~(((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))) | (((pteval_t)(1)) << 4) | (((pteval_t)(1)) << 3) | (((pteval_t)(1)) << 9) | (((pteval_t)(1)) << 5) | (((pteval_t)(1)) << 6));

 return native_make_pte(val);
}



static inline __attribute__((always_inline)) pgprot_t pgprot_modify(pgprot_t oldprot, pgprot_t newprot)
{
 pgprotval_t preservebits = ((oldprot).pgprot) & (((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))) | (((pteval_t)(1)) << 4) | (((pteval_t)(1)) << 3) | (((pteval_t)(1)) << 9) | (((pteval_t)(1)) << 5) | (((pteval_t)(1)) << 6));
 pgprotval_t addbits = ((newprot).pgprot);
 return ((pgprot_t) { (preservebits | addbits) } );
}





static inline __attribute__((always_inline)) int is_new_memtype_allowed(unsigned long flags,
      unsigned long new_flags)
{






 if ((flags == ((((pteval_t)(1)) << 4)) &&
      new_flags == (0)) ||
     (flags == ((((pteval_t)(1)) << 3)) &&
      new_flags == (0))) {
  return 0;
 }

 return 1;
}






struct file;
pgprot_t phys_mem_access_prot(struct file *file, unsigned long pfn,
                              unsigned long size, pgprot_t vma_prot);
int phys_mem_access_prot_allowed(struct file *file, unsigned long pfn,
                              unsigned long size, pgprot_t *vma_prot);



void set_pte_vaddr(unsigned long vaddr, pte_t pte);





static inline __attribute__((always_inline)) void native_pagetable_setup_start(pgd_t *base) {}
static inline __attribute__((always_inline)) void native_pagetable_setup_done(pgd_t *base) {}


struct seq_file;
extern void arch_report_meminfo(struct seq_file *m);
# 435 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h"
static inline __attribute__((always_inline)) void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) paravirt_pagetable_setup_start(pgd_t *base)
{
 native_pagetable_setup_start(base);
}

static inline __attribute__((always_inline)) void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) paravirt_pagetable_setup_done(pgd_t *base)
{
 native_pagetable_setup_done(base);
}







# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h" 1
# 16 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
extern pud_t level3_kernel_pgt[512];
extern pud_t level3_ident_pgt[512];
extern pmd_t level2_kernel_pgt[512];
extern pmd_t level2_fixmap_pgt[512];
extern pmd_t level2_ident_pgt[512];
extern pgd_t init_level4_pgt[];



extern void paging_init(void);
# 73 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
struct mm_struct;

void set_pte_vaddr_pud(pud_t *pud_page, unsigned long vaddr, pte_t new_pte);


static inline __attribute__((always_inline)) void native_pte_clear(struct mm_struct *mm, unsigned long addr,
        pte_t *ptep)
{
 *ptep = native_make_pte(0);
}

static inline __attribute__((always_inline)) void native_set_pte(pte_t *ptep, pte_t pte)
{
 *ptep = pte;
}

static inline __attribute__((always_inline)) void native_set_pte_atomic(pte_t *ptep, pte_t pte)
{
 native_set_pte(ptep, pte);
}

static inline __attribute__((always_inline)) pte_t native_ptep_get_and_clear(pte_t *xp)
{

 return native_make_pte(((__typeof__(*(&xp->pte)))__xchg((unsigned long)(0), (&xp->pte), sizeof(*(&xp->pte)))));







}

static inline __attribute__((always_inline)) void native_set_pmd(pmd_t *pmdp, pmd_t pmd)
{
 *pmdp = pmd;
}

static inline __attribute__((always_inline)) void native_pmd_clear(pmd_t *pmd)
{
 native_set_pmd(pmd, native_make_pmd(0));
}

static inline __attribute__((always_inline)) void native_set_pud(pud_t *pudp, pud_t pud)
{
 *pudp = pud;
}

static inline __attribute__((always_inline)) void native_pud_clear(pud_t *pud)
{
 native_set_pud(pud, native_make_pud(0));
}

static inline __attribute__((always_inline)) void native_set_pgd(pgd_t *pgdp, pgd_t pgd)
{
 *pgdp = pgd;
}

static inline __attribute__((always_inline)) void native_pgd_clear(pgd_t *pgd)
{
 native_set_pgd(pgd, native_make_pgd(0));
}
# 159 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
static inline __attribute__((always_inline)) int pgd_bad(pgd_t pgd)
{
 return (native_pgd_val(pgd) & ~(((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))) | (((pteval_t)(1)) << 2))) != ((((pteval_t)(1)) << 0) | (((pteval_t)(1)) << 1) | (((pteval_t)(1)) << 5) | (((pteval_t)(1)) << 6));
}

static inline __attribute__((always_inline)) int pud_bad(pud_t pud)
{
 return (native_pud_val(pud) & ~(((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))) | (((pteval_t)(1)) << 2))) != ((((pteval_t)(1)) << 0) | (((pteval_t)(1)) << 1) | (((pteval_t)(1)) << 5) | (((pteval_t)(1)) << 6));
}

static inline __attribute__((always_inline)) int pmd_bad(pmd_t pmd)
{
 return (native_pmd_val(pmd) & ~(((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))) | (((pteval_t)(1)) << 2))) != ((((pteval_t)(1)) << 0) | (((pteval_t)(1)) << 1) | (((pteval_t)(1)) << 5) | (((pteval_t)(1)) << 6));
}
# 191 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
static inline __attribute__((always_inline)) int pgd_large(pgd_t pgd) { return 0; }
# 204 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
static inline __attribute__((always_inline)) int pud_large(pud_t pte)
{
 return (native_pud_val(pte) & ((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 0))) ==
  ((((pteval_t)(1)) << 7) | (((pteval_t)(1)) << 0));
}
# 244 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
extern int direct_gbpages;
# 266 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable_64.h"
extern int kern_addr_valid(unsigned long addr);
extern void cleanup_highmap(void);
# 452 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h" 2
# 479 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h"
enum {
 PG_LEVEL_NONE,
 PG_LEVEL_4K,
 PG_LEVEL_2M,
 PG_LEVEL_1G,
 PG_LEVEL_NUM
};


extern void update_page_count(int level, unsigned long pages);
# 499 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h"
extern pte_t *lookup_address(unsigned long address, unsigned int *level);


static inline __attribute__((always_inline)) pte_t native_local_ptep_get_and_clear(pte_t *ptep)
{
 pte_t res = *ptep;


 native_pte_clear(((void *)0), 0, ptep);
 return res;
}

static inline __attribute__((always_inline)) void native_set_pte_at(struct mm_struct *mm, unsigned long addr,
         pte_t *ptep , pte_t pte)
{
 native_set_pte(ptep, pte);
}
# 542 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h"
struct vm_area_struct;


extern int ptep_set_access_flags(struct vm_area_struct *vma,
     unsigned long address, pte_t *ptep,
     pte_t entry, int dirty);


extern int ptep_test_and_clear_young(struct vm_area_struct *vma,
         unsigned long addr, pte_t *ptep);


extern int ptep_clear_flush_young(struct vm_area_struct *vma,
      unsigned long address, pte_t *ptep);


static inline __attribute__((always_inline)) pte_t ptep_get_and_clear(struct mm_struct *mm, unsigned long addr,
           pte_t *ptep)
{
 pte_t pte = native_ptep_get_and_clear(ptep);
 do { } while (0);
 return pte;
}


static inline __attribute__((always_inline)) pte_t ptep_get_and_clear_full(struct mm_struct *mm,
         unsigned long addr, pte_t *ptep,
         int full)
{
 pte_t pte;
 if (full) {




  pte = native_local_ptep_get_and_clear(ptep);
 } else {
  pte = ptep_get_and_clear(mm, addr, ptep);
 }
 return pte;
}


static inline __attribute__((always_inline)) void ptep_set_wrprotect(struct mm_struct *mm,
          unsigned long addr, pte_t *ptep)
{
 clear_bit(1, (unsigned long *)&ptep->pte);
 do { } while (0);
}
# 602 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h"
static inline __attribute__((always_inline)) void clone_pgd_range(pgd_t *dst, pgd_t *src, int count)
{
       memcpy(dst, src, count * sizeof(pgd_t));
}


# 1 "include/asm-generic/pgtable.h" 1
# 166 "include/asm-generic/pgtable.h"
void pgd_clear_bad(pgd_t *);
void pud_clear_bad(pud_t *);
void pmd_clear_bad(pmd_t *);

static inline __attribute__((always_inline)) int pgd_none_or_clear_bad(pgd_t *pgd)
{
 if ((!native_pgd_val(*pgd)))
  return 1;
 if (__builtin_expect(!!(pgd_bad(*pgd)), 0)) {
  pgd_clear_bad(pgd);
  return 1;
 }
 return 0;
}

static inline __attribute__((always_inline)) int pud_none_or_clear_bad(pud_t *pud)
{
 if ((!native_pud_val(*pud)))
  return 1;
 if (__builtin_expect(!!(pud_bad(*pud)), 0)) {
  pud_clear_bad(pud);
  return 1;
 }
 return 0;
}

static inline __attribute__((always_inline)) int pmd_none_or_clear_bad(pmd_t *pmd)
{
 if ((!native_pmd_val((*pmd))))
  return 1;
 if (__builtin_expect(!!(pmd_bad(*pmd)), 0)) {
  pmd_clear_bad(pmd);
  return 1;
 }
 return 0;
}

static inline __attribute__((always_inline)) pte_t __ptep_modify_prot_start(struct mm_struct *mm,
          unsigned long addr,
          pte_t *ptep)
{





 return ptep_get_and_clear(mm, addr, ptep);
}

static inline __attribute__((always_inline)) void __ptep_modify_prot_commit(struct mm_struct *mm,
          unsigned long addr,
          pte_t *ptep, pte_t pte)
{




 native_set_pte_at(mm, addr, ptep, pte);
}
# 241 "include/asm-generic/pgtable.h"
static inline __attribute__((always_inline)) pte_t ptep_modify_prot_start(struct mm_struct *mm,
        unsigned long addr,
        pte_t *ptep)
{
 return __ptep_modify_prot_start(mm, addr, ptep);
}





static inline __attribute__((always_inline)) void ptep_modify_prot_commit(struct mm_struct *mm,
        unsigned long addr,
        pte_t *ptep, pte_t pte)
{
 __ptep_modify_prot_commit(mm, addr, ptep, pte);
}
# 335 "include/asm-generic/pgtable.h"
extern int track_pfn_vma_new(struct vm_area_struct *vma, pgprot_t *prot,
    unsigned long pfn, unsigned long size);
extern int track_pfn_vma_copy(struct vm_area_struct *vma);
extern void untrack_pfn_vma(struct vm_area_struct *vma, unsigned long pfn,
    unsigned long size);
# 609 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pgtable.h" 2
# 41 "include/linux/mm.h" 2
# 57 "include/linux/mm.h"
extern struct kmem_cache *vm_area_cachep;
# 133 "include/linux/mm.h"
extern pgprot_t protection_map[16];
# 146 "include/linux/mm.h"
static inline __attribute__((always_inline)) int is_linear_pfn_mapping(struct vm_area_struct *vma)
{
 return ((vma->vm_flags & 0x00000400) && vma->vm_pgoff);
}

static inline __attribute__((always_inline)) int is_pfn_mapping(struct vm_area_struct *vma)
{
 return (vma->vm_flags & 0x00000400);
}
# 165 "include/linux/mm.h"
struct vm_fault {
 unsigned int flags;
 unsigned long pgoff;
 void __attribute__((noderef, address_space(1))) *virtual_address;

 struct page *page;




};






struct vm_operations_struct {
 void (*open)(struct vm_area_struct * area);
 void (*close)(struct vm_area_struct * area);
 int (*fault)(struct vm_area_struct *vma, struct vm_fault *vmf);



 int (*page_mkwrite)(struct vm_area_struct *vma, struct page *page);




 int (*access)(struct vm_area_struct *vma, unsigned long addr,
        void *buf, int len, int write);
# 204 "include/linux/mm.h"
 int (*set_policy)(struct vm_area_struct *vma, struct mempolicy *new);
# 216 "include/linux/mm.h"
 struct mempolicy *(*get_policy)(struct vm_area_struct *vma,
     unsigned long addr);
 int (*migrate)(struct vm_area_struct *vma, const nodemask_t *from,
  const nodemask_t *to, unsigned long flags);

};

struct mmu_gather;
struct inode;
# 233 "include/linux/mm.h"
# 1 "include/linux/page-flags.h" 1
# 72 "include/linux/page-flags.h"
enum pageflags {
 PG_locked,
 PG_error,
 PG_referenced,
 PG_uptodate,
 PG_dirty,
 PG_lru,
 PG_active,
 PG_slab,
 PG_owner_priv_1,
 PG_arch_1,
 PG_reserved,
 PG_private,
 PG_writeback,

 PG_head,
 PG_tail,



 PG_swapcache,
 PG_mappedtodisk,
 PG_reclaim,
 PG_buddy,
 PG_swapbacked,

 PG_unevictable,
 PG_mlocked,




 __NR_PAGEFLAGS,


 PG_checked = PG_owner_priv_1,


 PG_pinned = PG_owner_priv_1,
 PG_savepinned = PG_dirty,


 PG_slob_page = PG_active,
 PG_slob_free = PG_private,


 PG_slub_frozen = PG_active,
 PG_slub_debug = PG_error,
};
# 181 "include/linux/page-flags.h"
struct page;

static inline __attribute__((always_inline)) int PageLocked(struct page *page) { return (__builtin_constant_p((PG_locked)) ? constant_test_bit((PG_locked), (&page->flags)) : variable_test_bit((PG_locked), (&page->flags))); }
static inline __attribute__((always_inline)) int PageError(struct page *page) { return (__builtin_constant_p((PG_error)) ? constant_test_bit((PG_error), (&page->flags)) : variable_test_bit((PG_error), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageError(struct page *page) { set_bit(PG_error, &page->flags); } static inline __attribute__((always_inline)) void ClearPageError(struct page *page) { clear_bit(PG_error, &page->flags); }
static inline __attribute__((always_inline)) int PageReferenced(struct page *page) { return (__builtin_constant_p((PG_referenced)) ? constant_test_bit((PG_referenced), (&page->flags)) : variable_test_bit((PG_referenced), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageReferenced(struct page *page) { set_bit(PG_referenced, &page->flags); } static inline __attribute__((always_inline)) void ClearPageReferenced(struct page *page) { clear_bit(PG_referenced, &page->flags); } static inline __attribute__((always_inline)) int TestClearPageReferenced(struct page *page) { return test_and_clear_bit(PG_referenced, &page->flags); }
static inline __attribute__((always_inline)) int PageDirty(struct page *page) { return (__builtin_constant_p((PG_dirty)) ? constant_test_bit((PG_dirty), (&page->flags)) : variable_test_bit((PG_dirty), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageDirty(struct page *page) { set_bit(PG_dirty, &page->flags); } static inline __attribute__((always_inline)) void ClearPageDirty(struct page *page) { clear_bit(PG_dirty, &page->flags); } static inline __attribute__((always_inline)) int TestSetPageDirty(struct page *page) { return test_and_set_bit(PG_dirty, &page->flags); } static inline __attribute__((always_inline)) int TestClearPageDirty(struct page *page) { return test_and_clear_bit(PG_dirty, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageDirty(struct page *page) { __clear_bit(PG_dirty, &page->flags); }
static inline __attribute__((always_inline)) int PageLRU(struct page *page) { return (__builtin_constant_p((PG_lru)) ? constant_test_bit((PG_lru), (&page->flags)) : variable_test_bit((PG_lru), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageLRU(struct page *page) { set_bit(PG_lru, &page->flags); } static inline __attribute__((always_inline)) void ClearPageLRU(struct page *page) { clear_bit(PG_lru, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageLRU(struct page *page) { __clear_bit(PG_lru, &page->flags); }
static inline __attribute__((always_inline)) int PageActive(struct page *page) { return (__builtin_constant_p((PG_active)) ? constant_test_bit((PG_active), (&page->flags)) : variable_test_bit((PG_active), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageActive(struct page *page) { set_bit(PG_active, &page->flags); } static inline __attribute__((always_inline)) void ClearPageActive(struct page *page) { clear_bit(PG_active, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageActive(struct page *page) { __clear_bit(PG_active, &page->flags); }
 static inline __attribute__((always_inline)) int TestClearPageActive(struct page *page) { return test_and_clear_bit(PG_active, &page->flags); }
static inline __attribute__((always_inline)) int PageSlab(struct page *page) { return (__builtin_constant_p((PG_slab)) ? constant_test_bit((PG_slab), (&page->flags)) : variable_test_bit((PG_slab), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageSlab(struct page *page) { __set_bit(PG_slab, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageSlab(struct page *page) { __clear_bit(PG_slab, &page->flags); }
static inline __attribute__((always_inline)) int PageChecked(struct page *page) { return (__builtin_constant_p((PG_checked)) ? constant_test_bit((PG_checked), (&page->flags)) : variable_test_bit((PG_checked), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageChecked(struct page *page) { set_bit(PG_checked, &page->flags); } static inline __attribute__((always_inline)) void ClearPageChecked(struct page *page) { clear_bit(PG_checked, &page->flags); }
static inline __attribute__((always_inline)) int PagePinned(struct page *page) { return (__builtin_constant_p((PG_pinned)) ? constant_test_bit((PG_pinned), (&page->flags)) : variable_test_bit((PG_pinned), (&page->flags))); } static inline __attribute__((always_inline)) void SetPagePinned(struct page *page) { set_bit(PG_pinned, &page->flags); } static inline __attribute__((always_inline)) void ClearPagePinned(struct page *page) { clear_bit(PG_pinned, &page->flags); } static inline __attribute__((always_inline)) int TestSetPagePinned(struct page *page) { return test_and_set_bit(PG_pinned, &page->flags); } static inline __attribute__((always_inline)) int TestClearPagePinned(struct page *page) { return test_and_clear_bit(PG_pinned, &page->flags); }
static inline __attribute__((always_inline)) int PageSavePinned(struct page *page) { return (__builtin_constant_p((PG_savepinned)) ? constant_test_bit((PG_savepinned), (&page->flags)) : variable_test_bit((PG_savepinned), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageSavePinned(struct page *page) { set_bit(PG_savepinned, &page->flags); } static inline __attribute__((always_inline)) void ClearPageSavePinned(struct page *page) { clear_bit(PG_savepinned, &page->flags); };
static inline __attribute__((always_inline)) int PageReserved(struct page *page) { return (__builtin_constant_p((PG_reserved)) ? constant_test_bit((PG_reserved), (&page->flags)) : variable_test_bit((PG_reserved), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageReserved(struct page *page) { set_bit(PG_reserved, &page->flags); } static inline __attribute__((always_inline)) void ClearPageReserved(struct page *page) { clear_bit(PG_reserved, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageReserved(struct page *page) { __clear_bit(PG_reserved, &page->flags); }
static inline __attribute__((always_inline)) int PagePrivate(struct page *page) { return (__builtin_constant_p((PG_private)) ? constant_test_bit((PG_private), (&page->flags)) : variable_test_bit((PG_private), (&page->flags))); } static inline __attribute__((always_inline)) void SetPagePrivate(struct page *page) { set_bit(PG_private, &page->flags); } static inline __attribute__((always_inline)) void ClearPagePrivate(struct page *page) { clear_bit(PG_private, &page->flags); } static inline __attribute__((always_inline)) void __ClearPagePrivate(struct page *page) { __clear_bit(PG_private, &page->flags); }
 static inline __attribute__((always_inline)) void __SetPagePrivate(struct page *page) { __set_bit(PG_private, &page->flags); }
static inline __attribute__((always_inline)) int PageSwapBacked(struct page *page) { return (__builtin_constant_p((PG_swapbacked)) ? constant_test_bit((PG_swapbacked), (&page->flags)) : variable_test_bit((PG_swapbacked), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageSwapBacked(struct page *page) { set_bit(PG_swapbacked, &page->flags); } static inline __attribute__((always_inline)) void ClearPageSwapBacked(struct page *page) { clear_bit(PG_swapbacked, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageSwapBacked(struct page *page) { __clear_bit(PG_swapbacked, &page->flags); }

static inline __attribute__((always_inline)) int PageSlobPage(struct page *page) { return (__builtin_constant_p((PG_slob_page)) ? constant_test_bit((PG_slob_page), (&page->flags)) : variable_test_bit((PG_slob_page), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageSlobPage(struct page *page) { __set_bit(PG_slob_page, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageSlobPage(struct page *page) { __clear_bit(PG_slob_page, &page->flags); }
static inline __attribute__((always_inline)) int PageSlobFree(struct page *page) { return (__builtin_constant_p((PG_slob_free)) ? constant_test_bit((PG_slob_free), (&page->flags)) : variable_test_bit((PG_slob_free), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageSlobFree(struct page *page) { __set_bit(PG_slob_free, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageSlobFree(struct page *page) { __clear_bit(PG_slob_free, &page->flags); }

static inline __attribute__((always_inline)) int PageSlubFrozen(struct page *page) { return (__builtin_constant_p((PG_slub_frozen)) ? constant_test_bit((PG_slub_frozen), (&page->flags)) : variable_test_bit((PG_slub_frozen), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageSlubFrozen(struct page *page) { __set_bit(PG_slub_frozen, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageSlubFrozen(struct page *page) { __clear_bit(PG_slub_frozen, &page->flags); }
static inline __attribute__((always_inline)) int PageSlubDebug(struct page *page) { return (__builtin_constant_p((PG_slub_debug)) ? constant_test_bit((PG_slub_debug), (&page->flags)) : variable_test_bit((PG_slub_debug), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageSlubDebug(struct page *page) { __set_bit(PG_slub_debug, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageSlubDebug(struct page *page) { __clear_bit(PG_slub_debug, &page->flags); }





static inline __attribute__((always_inline)) int PageWriteback(struct page *page) { return (__builtin_constant_p((PG_writeback)) ? constant_test_bit((PG_writeback), (&page->flags)) : variable_test_bit((PG_writeback), (&page->flags))); } static inline __attribute__((always_inline)) int TestSetPageWriteback(struct page *page) { return test_and_set_bit(PG_writeback, &page->flags); } static inline __attribute__((always_inline)) int TestClearPageWriteback(struct page *page) { return test_and_clear_bit(PG_writeback, &page->flags); }
static inline __attribute__((always_inline)) int PageBuddy(struct page *page) { return (__builtin_constant_p((PG_buddy)) ? constant_test_bit((PG_buddy), (&page->flags)) : variable_test_bit((PG_buddy), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageBuddy(struct page *page) { __set_bit(PG_buddy, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageBuddy(struct page *page) { __clear_bit(PG_buddy, &page->flags); }
static inline __attribute__((always_inline)) int PageMappedToDisk(struct page *page) { return (__builtin_constant_p((PG_mappedtodisk)) ? constant_test_bit((PG_mappedtodisk), (&page->flags)) : variable_test_bit((PG_mappedtodisk), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageMappedToDisk(struct page *page) { set_bit(PG_mappedtodisk, &page->flags); } static inline __attribute__((always_inline)) void ClearPageMappedToDisk(struct page *page) { clear_bit(PG_mappedtodisk, &page->flags); }


static inline __attribute__((always_inline)) int PageReclaim(struct page *page) { return (__builtin_constant_p((PG_reclaim)) ? constant_test_bit((PG_reclaim), (&page->flags)) : variable_test_bit((PG_reclaim), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageReclaim(struct page *page) { set_bit(PG_reclaim, &page->flags); } static inline __attribute__((always_inline)) void ClearPageReclaim(struct page *page) { clear_bit(PG_reclaim, &page->flags); } static inline __attribute__((always_inline)) int TestClearPageReclaim(struct page *page) { return test_and_clear_bit(PG_reclaim, &page->flags); }
static inline __attribute__((always_inline)) int PageReadahead(struct page *page) { return (__builtin_constant_p((PG_reclaim)) ? constant_test_bit((PG_reclaim), (&page->flags)) : variable_test_bit((PG_reclaim), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageReadahead(struct page *page) { set_bit(PG_reclaim, &page->flags); } static inline __attribute__((always_inline)) void ClearPageReadahead(struct page *page) { clear_bit(PG_reclaim, &page->flags); }
# 224 "include/linux/page-flags.h"
static inline __attribute__((always_inline)) int PageHighMem(struct page *page) { return 0; }



static inline __attribute__((always_inline)) int PageSwapCache(struct page *page) { return (__builtin_constant_p((PG_swapcache)) ? constant_test_bit((PG_swapcache), (&page->flags)) : variable_test_bit((PG_swapcache), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageSwapCache(struct page *page) { set_bit(PG_swapcache, &page->flags); } static inline __attribute__((always_inline)) void ClearPageSwapCache(struct page *page) { clear_bit(PG_swapcache, &page->flags); }






static inline __attribute__((always_inline)) int PageUnevictable(struct page *page) { return (__builtin_constant_p((PG_unevictable)) ? constant_test_bit((PG_unevictable), (&page->flags)) : variable_test_bit((PG_unevictable), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageUnevictable(struct page *page) { set_bit(PG_unevictable, &page->flags); } static inline __attribute__((always_inline)) void ClearPageUnevictable(struct page *page) { clear_bit(PG_unevictable, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageUnevictable(struct page *page) { __clear_bit(PG_unevictable, &page->flags); }
 static inline __attribute__((always_inline)) int TestClearPageUnevictable(struct page *page) { return test_and_clear_bit(PG_unevictable, &page->flags); }


static inline __attribute__((always_inline)) int PageMlocked(struct page *page) { return (__builtin_constant_p((PG_mlocked)) ? constant_test_bit((PG_mlocked), (&page->flags)) : variable_test_bit((PG_mlocked), (&page->flags))); } static inline __attribute__((always_inline)) void SetPageMlocked(struct page *page) { set_bit(PG_mlocked, &page->flags); } static inline __attribute__((always_inline)) void ClearPageMlocked(struct page *page) { clear_bit(PG_mlocked, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageMlocked(struct page *page) { __clear_bit(PG_mlocked, &page->flags); }
 static inline __attribute__((always_inline)) int TestSetPageMlocked(struct page *page) { return test_and_set_bit(PG_mlocked, &page->flags); } static inline __attribute__((always_inline)) int TestClearPageMlocked(struct page *page) { return test_and_clear_bit(PG_mlocked, &page->flags); }
# 256 "include/linux/page-flags.h"
static inline __attribute__((always_inline)) int PageUncached(struct page *page) { return 0; }


static inline __attribute__((always_inline)) int PageUptodate(struct page *page)
{
 int ret = (__builtin_constant_p((PG_uptodate)) ? constant_test_bit((PG_uptodate), (&(page)->flags)) : variable_test_bit((PG_uptodate), (&(page)->flags)));
# 271 "include/linux/page-flags.h"
 if (ret)
  __asm__ __volatile__("": : :"memory");

 return ret;
}

static inline __attribute__((always_inline)) void __SetPageUptodate(struct page *page)
{
 __asm__ __volatile__("": : :"memory");
 __set_bit(PG_uptodate, &(page)->flags);
}

static inline __attribute__((always_inline)) void SetPageUptodate(struct page *page)
{
# 297 "include/linux/page-flags.h"
 __asm__ __volatile__("": : :"memory");
 set_bit(PG_uptodate, &(page)->flags);

}

static inline __attribute__((always_inline)) void ClearPageUptodate(struct page *page) { clear_bit(PG_uptodate, &page->flags); }

extern void cancel_dirty_page(struct page *page, unsigned int account_size);

int test_clear_page_writeback(struct page *page);
int test_set_page_writeback(struct page *page);

static inline __attribute__((always_inline)) void set_page_writeback(struct page *page)
{
 test_set_page_writeback(page);
}
# 321 "include/linux/page-flags.h"
static inline __attribute__((always_inline)) int PageHead(struct page *page) { return (__builtin_constant_p((PG_head)) ? constant_test_bit((PG_head), (&page->flags)) : variable_test_bit((PG_head), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageHead(struct page *page) { __set_bit(PG_head, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageHead(struct page *page) { __clear_bit(PG_head, &page->flags); }
static inline __attribute__((always_inline)) int PageTail(struct page *page) { return (__builtin_constant_p((PG_tail)) ? constant_test_bit((PG_tail), (&page->flags)) : variable_test_bit((PG_tail), (&page->flags))); } static inline __attribute__((always_inline)) void __SetPageTail(struct page *page) { __set_bit(PG_tail, &page->flags); } static inline __attribute__((always_inline)) void __ClearPageTail(struct page *page) { __clear_bit(PG_tail, &page->flags); }

static inline __attribute__((always_inline)) int PageCompound(struct page *page)
{
 return page->flags & ((1L << PG_head) | (1L << PG_tail));

}
# 234 "include/linux/mm.h" 2
# 251 "include/linux/mm.h"
static inline __attribute__((always_inline)) int put_page_testzero(struct page *page)
{
 do { if (__builtin_expect(!!(((&page->_count)->counter) == 0), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/mm.h"), "i" (253), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 return atomic_dec_and_test(&page->_count);
}





static inline __attribute__((always_inline)) int get_page_unless_zero(struct page *page)
{
 return atomic_add_unless((&page->_count), 1, 0);
}


struct page *vmalloc_to_page(const void *addr);
unsigned long vmalloc_to_pfn(const void *addr);







static inline __attribute__((always_inline)) int is_vmalloc_addr(const void *x)
{

 unsigned long addr = (unsigned long)x;

 return addr >= (0xffffc20000000000UL) && addr < (0xffffe1ffffffffffUL);



}

static inline __attribute__((always_inline)) struct page *compound_head(struct page *page)
{
 if (__builtin_expect(!!(PageTail(page)), 0))
  return page->first_page;
 return page;
}

static inline __attribute__((always_inline)) int page_count(struct page *page)
{
 return ((&compound_head(page)->_count)->counter);
}

static inline __attribute__((always_inline)) void get_page(struct page *page)
{
 page = compound_head(page);
 do { if (__builtin_expect(!!(((&page->_count)->counter) == 0), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/mm.h"), "i" (302), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 atomic_inc(&page->_count);
}

static inline __attribute__((always_inline)) struct page *virt_to_head_page(const void *x)
{
 struct page *page = (((struct page *)(0xffffe20000000000UL)) + (__phys_addr((unsigned long)(x)) >> 12));
 return compound_head(page);
}





static inline __attribute__((always_inline)) void init_page_count(struct page *page)
{
 (((&page->_count)->counter) = (1));
}

void put_page(struct page *page);
void put_pages_list(struct list_head *pages);

void split_page(struct page *page, unsigned int order);






typedef void compound_page_dtor(struct page *);

static inline __attribute__((always_inline)) void set_compound_page_dtor(struct page *page,
      compound_page_dtor *dtor)
{
 page[1].lru.next = (void *)dtor;
}

static inline __attribute__((always_inline)) compound_page_dtor *get_compound_page_dtor(struct page *page)
{
 return (compound_page_dtor *)page[1].lru.next;
}

static inline __attribute__((always_inline)) int compound_order(struct page *page)
{
 if (!PageHead(page))
  return 0;
 return (unsigned long)page[1].lru.prev;
}

static inline __attribute__((always_inline)) void set_compound_order(struct page *page, unsigned long order)
{
 page[1].lru.prev = (void *)order;
}
# 501 "include/linux/mm.h"
static inline __attribute__((always_inline)) enum zone_type page_zonenum(struct page *page)
{
 return (page->flags >> (((((sizeof(unsigned long)*8) - 0) - 6) - 2) * (2 != 0))) & ((1UL << 2) - 1);
}
# 514 "include/linux/mm.h"
static inline __attribute__((always_inline)) int page_zone_id(struct page *page)
{
 return (page->flags >> ((((((sizeof(unsigned long)*8) - 0) - 6) < ((((sizeof(unsigned long)*8) - 0) - 6) - 2))? (((sizeof(unsigned long)*8) - 0) - 6) : ((((sizeof(unsigned long)*8) - 0) - 6) - 2)) * ((6 + 2) != 0))) & ((1UL << (6 + 2)) - 1);
}

static inline __attribute__((always_inline)) int zone_to_nid(struct zone *zone)
{

 return zone->node;



}




static inline __attribute__((always_inline)) int page_to_nid(struct page *page)
{
 return (page->flags >> ((((sizeof(unsigned long)*8) - 0) - 6) * (6 != 0))) & ((1UL << 6) - 1);
}


static inline __attribute__((always_inline)) struct zone *page_zone(struct page *page)
{
 return &(node_data[page_to_nid(page)])->node_zones[page_zonenum(page)];
}
# 549 "include/linux/mm.h"
static inline __attribute__((always_inline)) void set_page_zone(struct page *page, enum zone_type zone)
{
 page->flags &= ~(((1UL << 2) - 1) << (((((sizeof(unsigned long)*8) - 0) - 6) - 2) * (2 != 0)));
 page->flags |= (zone & ((1UL << 2) - 1)) << (((((sizeof(unsigned long)*8) - 0) - 6) - 2) * (2 != 0));
}

static inline __attribute__((always_inline)) void set_page_node(struct page *page, unsigned long node)
{
 page->flags &= ~(((1UL << 6) - 1) << ((((sizeof(unsigned long)*8) - 0) - 6) * (6 != 0)));
 page->flags |= (node & ((1UL << 6) - 1)) << ((((sizeof(unsigned long)*8) - 0) - 6) * (6 != 0));
}

static inline __attribute__((always_inline)) void set_page_section(struct page *page, unsigned long section)
{
 page->flags &= ~(((1UL << 0) - 1) << (((sizeof(unsigned long)*8) - 0) * (0 != 0)));
 page->flags |= (section & ((1UL << 0) - 1)) << (((sizeof(unsigned long)*8) - 0) * (0 != 0));
}

static inline __attribute__((always_inline)) void set_page_links(struct page *page, enum zone_type zone,
 unsigned long node, unsigned long pfn)
{
 set_page_zone(page, zone);
 set_page_node(page, node);
 set_page_section(page, ((pfn) >> (27 - 12)));
}





static inline __attribute__((always_inline)) unsigned long round_hint_to_min(unsigned long hint)
{

 hint &= (~(((1UL) << 12)-1));
 if (((void *)hint != ((void *)0)) &&
     (hint < mmap_min_addr))
  return (((mmap_min_addr)+((typeof(mmap_min_addr))(((1UL) << 12))-1))&~((typeof(mmap_min_addr))(((1UL) << 12))-1));

 return hint;
}




# 1 "include/linux/vmstat.h" 1





# 1 "include/linux/mm.h" 1
# 7 "include/linux/vmstat.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 9 "include/linux/vmstat.h" 2
# 31 "include/linux/vmstat.h"
enum vm_event_item { PGPGIN, PGPGOUT, PSWPIN, PSWPOUT,
  PGALLOC_DMA, PGALLOC_DMA32, PGALLOC_NORMAL , PGALLOC_MOVABLE,
  PGFREE, PGACTIVATE, PGDEACTIVATE,
  PGFAULT, PGMAJFAULT,
  PGREFILL_DMA, PGREFILL_DMA32, PGREFILL_NORMAL , PGREFILL_MOVABLE,
  PGSTEAL_DMA, PGSTEAL_DMA32, PGSTEAL_NORMAL , PGSTEAL_MOVABLE,
  PGSCAN_KSWAPD_DMA, PGSCAN_KSWAPD_DMA32, PGSCAN_KSWAPD_NORMAL , PGSCAN_KSWAPD_MOVABLE,
  PGSCAN_DIRECT_DMA, PGSCAN_DIRECT_DMA32, PGSCAN_DIRECT_NORMAL , PGSCAN_DIRECT_MOVABLE,
  PGINODESTEAL, SLABS_SCANNED, KSWAPD_STEAL, KSWAPD_INODESTEAL,
  PAGEOUTRUN, ALLOCSTALL, PGROTATED,

  HTLB_BUDDY_PGALLOC, HTLB_BUDDY_PGALLOC_FAIL,


  UNEVICTABLE_PGCULLED,
  UNEVICTABLE_PGSCANNED,
  UNEVICTABLE_PGRESCUED,
  UNEVICTABLE_PGMLOCKED,
  UNEVICTABLE_PGMUNLOCKED,
  UNEVICTABLE_PGCLEARED,
  UNEVICTABLE_PGSTRANDED,
  UNEVICTABLE_MLOCKFREED,

  NR_VM_EVENT_ITEMS
};

extern int sysctl_stat_interval;
# 70 "include/linux/vmstat.h"
struct vm_event_state {
 unsigned long event[NR_VM_EVENT_ITEMS];
};

extern __typeof__(struct vm_event_state) per_cpu__vm_event_states;

static inline __attribute__((always_inline)) void __count_vm_event(enum vm_event_item item)
{
 (*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__vm_event_states))); (typeof((&per_cpu__vm_event_states))) (__ptr + ((({ typeof(_proxy_pda.data_offset) ret__; switch (sizeof(_proxy_pda.data_offset)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; default: __bad_pda_field(); } ret__; })))); })).event[item]++;
}

static inline __attribute__((always_inline)) void count_vm_event(enum vm_event_item item)
{
 (*({ extern int simple_identifier_vm_event_states(void); do { } while (0); &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__vm_event_states))); (typeof((&per_cpu__vm_event_states))) (__ptr + ((({ typeof(_proxy_pda.data_offset) ret__; switch (sizeof(_proxy_pda.data_offset)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; default: __bad_pda_field(); } ret__; })))); })); })).event[item]++;
 do { } while (0);
}

static inline __attribute__((always_inline)) void __count_vm_events(enum vm_event_item item, long delta)
{
 (*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__vm_event_states))); (typeof((&per_cpu__vm_event_states))) (__ptr + ((({ typeof(_proxy_pda.data_offset) ret__; switch (sizeof(_proxy_pda.data_offset)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; default: __bad_pda_field(); } ret__; })))); })).event[item] += delta;
}

static inline __attribute__((always_inline)) void count_vm_events(enum vm_event_item item, long delta)
{
 (*({ extern int simple_identifier_vm_event_states(void); do { } while (0); &(*({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((&per_cpu__vm_event_states))); (typeof((&per_cpu__vm_event_states))) (__ptr + ((({ typeof(_proxy_pda.data_offset) ret__; switch (sizeof(_proxy_pda.data_offset)) { case 2: asm("mov" "w %%gs:%c1,%0" : "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 4: asm("mov" "l %%gs:%c1,%0": "=r" (ret__): "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; case 8: asm("mov" "q %%gs:%c1,%0": "=r" (ret__) : "i" (__builtin_offsetof(struct x8664_pda,data_offset)), "m" (_proxy_pda.data_offset)); break; default: __bad_pda_field(); } ret__; })))); })); })).event[item] += delta;
 do { } while (0);
}

extern void all_vm_events(unsigned long *);

extern void vm_events_fold_cpu(int cpu);
# 138 "include/linux/vmstat.h"
extern atomic_long_t vm_stat[NR_VM_ZONE_STAT_ITEMS];

static inline __attribute__((always_inline)) void zone_page_state_add(long x, struct zone *zone,
     enum zone_stat_item item)
{
 atomic_long_add(x, &zone->vm_stat[item]);
 atomic_long_add(x, &vm_stat[item]);
}

static inline __attribute__((always_inline)) unsigned long global_page_state(enum zone_stat_item item)
{
 long x = atomic_long_read(&vm_stat[item]);

 if (x < 0)
  x = 0;

 return x;
}

static inline __attribute__((always_inline)) unsigned long zone_page_state(struct zone *zone,
     enum zone_stat_item item)
{
 long x = atomic_long_read(&zone->vm_stat[item]);

 if (x < 0)
  x = 0;

 return x;
}

extern unsigned long global_lru_pages(void);

static inline __attribute__((always_inline)) unsigned long zone_lru_pages(struct zone *zone)
{
 return (zone_page_state(zone, NR_ACTIVE_ANON)
  + zone_page_state(zone, NR_ACTIVE_FILE)
  + zone_page_state(zone, NR_INACTIVE_ANON)
  + zone_page_state(zone, NR_INACTIVE_FILE));
}







static inline __attribute__((always_inline)) unsigned long node_page_state(int node,
     enum zone_stat_item item)
{
 struct zone *zones = (node_data[node])->node_zones;

 return

  zone_page_state(&zones[ZONE_DMA], item) +


  zone_page_state(&zones[ZONE_DMA32], item) +




  zone_page_state(&zones[ZONE_NORMAL], item) +
  zone_page_state(&zones[ZONE_MOVABLE], item);
}

extern void zone_statistics(struct zone *, struct zone *);
# 220 "include/linux/vmstat.h"
static inline __attribute__((always_inline)) void zap_zone_vm_stats(struct zone *zone)
{
 memset(zone->vm_stat, 0, sizeof(zone->vm_stat));
}

extern void inc_zone_state(struct zone *, enum zone_stat_item);


void __mod_zone_page_state(struct zone *, enum zone_stat_item item, int);
void __inc_zone_page_state(struct page *, enum zone_stat_item);
void __dec_zone_page_state(struct page *, enum zone_stat_item);

void mod_zone_page_state(struct zone *, enum zone_stat_item, int);
void inc_zone_page_state(struct page *, enum zone_stat_item);
void dec_zone_page_state(struct page *, enum zone_stat_item);

extern void inc_zone_state(struct zone *, enum zone_stat_item);
extern void __inc_zone_state(struct zone *, enum zone_stat_item);
extern void dec_zone_state(struct zone *, enum zone_stat_item);
extern void __dec_zone_state(struct zone *, enum zone_stat_item);

void refresh_cpu_vm_stats(int);
# 594 "include/linux/mm.h" 2

static inline __attribute__((always_inline)) __attribute__((always_inline)) void *lowmem_page_address(struct page *page)
{
 return ((void *)((unsigned long)((unsigned long)((page) - ((struct page *)(0xffffe20000000000UL))) << 12)+((unsigned long)(0xffff880000000000UL))));
}
# 636 "include/linux/mm.h"
extern struct address_space swapper_space;
static inline __attribute__((always_inline)) struct address_space *page_mapping(struct page *page)
{
 struct address_space *mapping = page->mapping;

 do { if (__builtin_expect(!!(PageSlab(page)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/mm.h"), "i" (641), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);

 if (__builtin_expect(!!(PageSwapCache(page)), 0))
  mapping = &swapper_space;
 else

 if (__builtin_expect(!!((unsigned long)mapping & 1), 0))
  mapping = ((void *)0);
 return mapping;
}

static inline __attribute__((always_inline)) int PageAnon(struct page *page)
{
 return ((unsigned long)page->mapping & 1) != 0;
}





static inline __attribute__((always_inline)) unsigned long page_index(struct page *page)
{
 if (__builtin_expect(!!(PageSwapCache(page)), 0))
  return ((page)->private);
 return page->index;
}






static inline __attribute__((always_inline)) void reset_page_mapcount(struct page *page)
{
 (((&(page)->_mapcount)->counter) = (-1));
}

static inline __attribute__((always_inline)) int page_mapcount(struct page *page)
{
 return ((&(page)->_mapcount)->counter) + 1;
}




static inline __attribute__((always_inline)) int page_mapped(struct page *page)
{
 return ((&(page)->_mapcount)->counter) >= 0;
}
# 712 "include/linux/mm.h"
extern void pagefault_out_of_memory(void);



extern void show_free_areas(void);


extern int shmem_lock(struct file *file, int lock, struct user_struct *user);







struct file *shmem_file_setup(char *name, loff_t size, unsigned long flags);

int shmem_zero_setup(struct vm_area_struct *);
# 739 "include/linux/mm.h"
extern int can_do_mlock(void);
extern int user_shm_lock(size_t, struct user_struct *);
extern void user_shm_unlock(size_t, struct user_struct *);




struct zap_details {
 struct vm_area_struct *nonlinear_vma;
 struct address_space *check_mapping;
 unsigned long first_index;
 unsigned long last_index;
 spinlock_t *i_mmap_lock;
 unsigned long truncate_count;
};

struct page *vm_normal_page(struct vm_area_struct *vma, unsigned long addr,
  pte_t pte);

int zap_vma_ptes(struct vm_area_struct *vma, unsigned long address,
  unsigned long size);
unsigned long zap_page_range(struct vm_area_struct *vma, unsigned long address,
  unsigned long size, struct zap_details *);
unsigned long unmap_vmas(struct mmu_gather **tlb,
  struct vm_area_struct *start_vma, unsigned long start_addr,
  unsigned long end_addr, unsigned long *nr_accounted,
  struct zap_details *);
# 777 "include/linux/mm.h"
struct mm_walk {
 int (*pgd_entry)(pgd_t *, unsigned long, unsigned long, struct mm_walk *);
 int (*pud_entry)(pud_t *, unsigned long, unsigned long, struct mm_walk *);
 int (*pmd_entry)(pmd_t *, unsigned long, unsigned long, struct mm_walk *);
 int (*pte_entry)(pte_t *, unsigned long, unsigned long, struct mm_walk *);
 int (*pte_hole)(unsigned long, unsigned long, struct mm_walk *);
 struct mm_struct *mm;
 void *private;
};

int walk_page_range(unsigned long addr, unsigned long end,
  struct mm_walk *walk);
void free_pgd_range(struct mmu_gather *tlb, unsigned long addr,
  unsigned long end, unsigned long floor, unsigned long ceiling);
int copy_page_range(struct mm_struct *dst, struct mm_struct *src,
   struct vm_area_struct *vma);
void unmap_mapping_range(struct address_space *mapping,
  loff_t const holebegin, loff_t const holelen, int even_cows);
int follow_phys(struct vm_area_struct *vma, unsigned long address,
  unsigned int flags, unsigned long *prot, resource_size_t *phys);
int generic_access_phys(struct vm_area_struct *vma, unsigned long addr,
   void *buf, int len, int write);

static inline __attribute__((always_inline)) void unmap_shared_mapping_range(struct address_space *mapping,
  loff_t const holebegin, loff_t const holelen)
{
 unmap_mapping_range(mapping, holebegin, holelen, 0);
}

extern int vmtruncate(struct inode * inode, loff_t offset);
extern int vmtruncate_range(struct inode * inode, loff_t offset, loff_t end);


extern int handle_mm_fault(struct mm_struct *mm, struct vm_area_struct *vma,
   unsigned long address, int write_access);
# 823 "include/linux/mm.h"
extern int make_pages_present(unsigned long addr, unsigned long end);
extern int access_process_vm(struct task_struct *tsk, unsigned long addr, void *buf, int len, int write);

int get_user_pages(struct task_struct *tsk, struct mm_struct *mm, unsigned long start,
  int len, int write, int force, struct page **pages, struct vm_area_struct **vmas);

extern int try_to_release_page(struct page * page, gfp_t gfp_mask);
extern void do_invalidatepage(struct page *page, unsigned long offset);

int __set_page_dirty_nobuffers(struct page *page);
int __set_page_dirty_no_writeback(struct page *page);
int redirty_page_for_writepage(struct writeback_control *wbc,
    struct page *page);
int set_page_dirty(struct page *page);
int set_page_dirty_lock(struct page *page);
int clear_page_dirty_for_io(struct page *page);

extern unsigned long move_page_tables(struct vm_area_struct *vma,
  unsigned long old_addr, struct vm_area_struct *new_vma,
  unsigned long new_addr, unsigned long len);
extern unsigned long do_mremap(unsigned long addr,
          unsigned long old_len, unsigned long new_len,
          unsigned long flags, unsigned long new_addr);
extern int mprotect_fixup(struct vm_area_struct *vma,
     struct vm_area_struct **pprev, unsigned long start,
     unsigned long end, unsigned long newflags);
# 860 "include/linux/mm.h"
int get_user_pages_fast(unsigned long start, int nr_pages, int write,
   struct page **pages);
# 878 "include/linux/mm.h"
struct shrinker {
 int (*shrink)(int nr_to_scan, gfp_t gfp_mask);
 int seeks;


 struct list_head list;
 long nr;
};

extern void register_shrinker(struct shrinker *);
extern void unregister_shrinker(struct shrinker *);

int vma_wants_writenotify(struct vm_area_struct *vma);

extern pte_t *get_locked_pte(struct mm_struct *mm, unsigned long addr, spinlock_t **ptl);
# 901 "include/linux/mm.h"
int __pud_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address);
# 911 "include/linux/mm.h"
int __pmd_alloc(struct mm_struct *mm, pud_t *pud, unsigned long address);


int __pte_alloc(struct mm_struct *mm, pmd_t *pmd, unsigned long address);
int __pte_alloc_kernel(pmd_t *pmd, unsigned long address);






static inline __attribute__((always_inline)) pud_t *pud_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address)
{
 return (__builtin_expect(!!((!native_pgd_val(*pgd))), 0) && __pud_alloc(mm, pgd, address))?
  ((void *)0): ((pud_t *)((unsigned long)((void *)((unsigned long)((unsigned long)native_pgd_val((*(pgd))) & ((pteval_t)(((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1))))+((unsigned long)(0xffff880000000000UL))))) + ((((address)) >> 30) & (512 - 1)));
}

static inline __attribute__((always_inline)) pmd_t *pmd_alloc(struct mm_struct *mm, pud_t *pud, unsigned long address)
{
 return (__builtin_expect(!!((!native_pud_val(*pud))), 0) && __pmd_alloc(mm, pud, address))?
  ((void *)0): ((pmd_t *)((unsigned long)((void *)((unsigned long)(native_pud_val((*(pud))) & (((signed long)(~(((1UL) << 12)-1))) & ((phys_addr_t)(1ULL << 46) - 1)))+((unsigned long)(0xffff880000000000UL))))) + (((address) >> 21) & (512 - 1)));
}
# 957 "include/linux/mm.h"
static inline __attribute__((always_inline)) void pgtable_page_ctor(struct page *page)
{
 do { do { static struct lock_class_key __key; __spin_lock_init((&((page)->ptl)), "__pte_lockptr(page)", &__key); } while (0); } while (0);
 inc_zone_page_state(page, NR_PAGETABLE);
}

static inline __attribute__((always_inline)) void pgtable_page_dtor(struct page *page)
{
 ((page)->mapping = ((void *)0));
 dec_zone_page_state(page, NR_PAGETABLE);
}
# 995 "include/linux/mm.h"
extern void free_area_init(unsigned long * zones_size);
extern void free_area_init_node(int nid, unsigned long * zones_size,
  unsigned long zone_start_pfn, unsigned long *zholes_size);
# 1026 "include/linux/mm.h"
extern void free_area_init_nodes(unsigned long *max_zone_pfn);
extern void add_active_range(unsigned int nid, unsigned long start_pfn,
     unsigned long end_pfn);
extern void remove_active_range(unsigned int nid, unsigned long start_pfn,
     unsigned long end_pfn);
extern void push_node_boundaries(unsigned int nid, unsigned long start_pfn,
     unsigned long end_pfn);
extern void remove_all_active_ranges(void);
extern unsigned long absent_pages_in_range(unsigned long start_pfn,
      unsigned long end_pfn);
extern void get_pfn_range_for_nid(unsigned int nid,
   unsigned long *start_pfn, unsigned long *end_pfn);
extern unsigned long find_min_pfn_with_active_regions(void);
extern void free_bootmem_with_active_regions(int nid,
      unsigned long max_low_pfn);
typedef int (*work_fn_t)(unsigned long, unsigned long, void *);
extern void work_with_active_regions(int nid, work_fn_t work_fn, void *data);
extern void sparse_memory_present_with_active_regions(int nid);
# 1054 "include/linux/mm.h"
extern int __attribute__ ((__section__(".meminit.text"))) __attribute__((__cold__)) early_pfn_to_nid(unsigned long pfn);


extern int __attribute__ ((__section__(".meminit.text"))) __attribute__((__cold__)) __early_pfn_to_nid(unsigned long pfn);



extern void set_dma_reserve(unsigned long new_dma_reserve);
extern void memmap_init_zone(unsigned long, int, unsigned long,
    unsigned long, enum memmap_context);
extern void setup_per_zone_pages_min(void);
extern void mem_init(void);
extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) mmap_init(void);
extern void show_mem(void);
extern void si_meminfo(struct sysinfo * val);
extern void si_meminfo_node(struct sysinfo *val, int nid);
extern int after_bootmem;


extern void setup_per_cpu_pageset(void);





extern atomic_t mmap_pages_allocated;


void vma_prio_tree_add(struct vm_area_struct *, struct vm_area_struct *old);
void vma_prio_tree_insert(struct vm_area_struct *, struct prio_tree_root *);
void vma_prio_tree_remove(struct vm_area_struct *, struct prio_tree_root *);
struct vm_area_struct *vma_prio_tree_next(struct vm_area_struct *vma,
 struct prio_tree_iter *iter);





static inline __attribute__((always_inline)) void vma_nonlinear_insert(struct vm_area_struct *vma,
     struct list_head *list)
{
 vma->shared.vm_set.parent = ((void *)0);
 list_add_tail(&vma->shared.vm_set.list, list);
}


extern int __vm_enough_memory(struct mm_struct *mm, long pages, int cap_sys_admin);
extern void vma_adjust(struct vm_area_struct *vma, unsigned long start,
 unsigned long end, unsigned long pgoff, struct vm_area_struct *insert);
extern struct vm_area_struct *vma_merge(struct mm_struct *,
 struct vm_area_struct *prev, unsigned long addr, unsigned long end,
 unsigned long vm_flags, struct anon_vma *, struct file *, unsigned long,
 struct mempolicy *);
extern struct anon_vma *find_mergeable_anon_vma(struct vm_area_struct *);
extern int split_vma(struct mm_struct *,
 struct vm_area_struct *, unsigned long addr, int new_below);
extern int insert_vm_struct(struct mm_struct *, struct vm_area_struct *);
extern void __vma_link_rb(struct mm_struct *, struct vm_area_struct *,
 struct rb_node **, struct rb_node *);
extern void unlink_file_vma(struct vm_area_struct *);
extern struct vm_area_struct *copy_vma(struct vm_area_struct **,
 unsigned long addr, unsigned long len, unsigned long pgoff);
extern void exit_mmap(struct mm_struct *);

extern int mm_take_all_locks(struct mm_struct *mm);
extern void mm_drop_all_locks(struct mm_struct *mm);



extern void added_exe_file_vma(struct mm_struct *mm);
extern void removed_exe_file_vma(struct mm_struct *mm);
# 1133 "include/linux/mm.h"
extern int may_expand_vm(struct mm_struct *mm, unsigned long npages);
extern int install_special_mapping(struct mm_struct *mm,
       unsigned long addr, unsigned long len,
       unsigned long flags, struct page **pages);

extern unsigned long get_unmapped_area(struct file *, unsigned long, unsigned long, unsigned long, unsigned long);

extern unsigned long do_mmap_pgoff(struct file *file, unsigned long addr,
 unsigned long len, unsigned long prot,
 unsigned long flag, unsigned long pgoff);
extern unsigned long mmap_region(struct file *file, unsigned long addr,
 unsigned long len, unsigned long flags,
 unsigned int vm_flags, unsigned long pgoff);

static inline __attribute__((always_inline)) unsigned long do_mmap(struct file *file, unsigned long addr,
 unsigned long len, unsigned long prot,
 unsigned long flag, unsigned long offset)
{
 unsigned long ret = -22;
 if ((offset + (((len)+((typeof(len))(((1UL) << 12))-1))&~((typeof(len))(((1UL) << 12))-1))) < offset)
  goto out;
 if (!(offset & ~(~(((1UL) << 12)-1))))
  ret = do_mmap_pgoff(file, addr, len, prot, flag, offset >> 12);
out:
 return ret;
}

extern int do_munmap(struct mm_struct *, unsigned long, size_t);

extern unsigned long do_brk(unsigned long, unsigned long);


extern unsigned long page_unuse(struct page *);
extern void truncate_inode_pages(struct address_space *, loff_t);
extern void truncate_inode_pages_range(struct address_space *,
           loff_t lstart, loff_t lend);


extern int filemap_fault(struct vm_area_struct *, struct vm_fault *);


int write_one_page(struct page *page, int wait);
void task_dirty_inc(struct task_struct *tsk);





int do_page_cache_readahead(struct address_space *mapping, struct file *filp,
   unsigned long offset, unsigned long nr_to_read);
int force_page_cache_readahead(struct address_space *mapping, struct file *filp,
   unsigned long offset, unsigned long nr_to_read);

void page_cache_sync_readahead(struct address_space *mapping,
          struct file_ra_state *ra,
          struct file *filp,
          unsigned long offset,
          unsigned long size);

void page_cache_async_readahead(struct address_space *mapping,
    struct file_ra_state *ra,
    struct file *filp,
    struct page *pg,
    unsigned long offset,
    unsigned long size);

unsigned long max_sane_readahead(unsigned long nr);


extern int expand_stack(struct vm_area_struct *vma, unsigned long address);



extern int expand_stack_downwards(struct vm_area_struct *vma,
      unsigned long address);


extern struct vm_area_struct * find_vma(struct mm_struct * mm, unsigned long addr);
extern struct vm_area_struct * find_vma_prev(struct mm_struct * mm, unsigned long addr,
          struct vm_area_struct **pprev);



static inline __attribute__((always_inline)) struct vm_area_struct * find_vma_intersection(struct mm_struct * mm, unsigned long start_addr, unsigned long end_addr)
{
 struct vm_area_struct * vma = find_vma(mm,start_addr);

 if (vma && end_addr <= vma->vm_start)
  vma = ((void *)0);
 return vma;
}

static inline __attribute__((always_inline)) unsigned long vma_pages(struct vm_area_struct *vma)
{
 return (vma->vm_end - vma->vm_start) >> 12;
}

pgprot_t vm_get_page_prot(unsigned long vm_flags);
struct vm_area_struct *find_extend_vma(struct mm_struct *, unsigned long addr);
int remap_pfn_range(struct vm_area_struct *, unsigned long addr,
   unsigned long pfn, unsigned long size, pgprot_t);
int vm_insert_page(struct vm_area_struct *, unsigned long addr, struct page *);
int vm_insert_pfn(struct vm_area_struct *vma, unsigned long addr,
   unsigned long pfn);
int vm_insert_mixed(struct vm_area_struct *vma, unsigned long addr,
   unsigned long pfn);

struct page *follow_page(struct vm_area_struct *, unsigned long address,
   unsigned int foll_flags);





typedef int (*pte_fn_t)(pte_t *pte, pgtable_t token, unsigned long addr,
   void *data);
extern int apply_to_page_range(struct mm_struct *mm, unsigned long address,
          unsigned long size, pte_fn_t fn, void *data);


void vm_stat_account(struct mm_struct *, unsigned long, struct file *, long);
# 1262 "include/linux/mm.h"
extern int debug_pagealloc_enabled;

extern void kernel_map_pages(struct page *page, int numpages, int enable);

static inline __attribute__((always_inline)) void enable_debug_pagealloc(void)
{
 debug_pagealloc_enabled = 1;
}
# 1284 "include/linux/mm.h"
extern struct vm_area_struct *get_gate_vma(struct task_struct *tsk);

int in_gate_area_no_task(unsigned long addr);
int in_gate_area(struct task_struct *task, unsigned long addr);





int drop_caches_sysctl_handler(struct ctl_table *, int, struct file *,
     void __attribute__((noderef, address_space(1))) *, size_t *, loff_t *);
unsigned long shrink_slab(unsigned long scanned, gfp_t gfp_mask,
   unsigned long lru_pages);




extern int randomize_va_space;


const char * arch_vma_name(struct vm_area_struct *vma);
void print_vma_addr(char *prefix, unsigned long rip);

struct page *sparse_mem_map_populate(unsigned long pnum, int nid);
pgd_t *vmemmap_pgd_populate(unsigned long addr, int node);
pud_t *vmemmap_pud_populate(pgd_t *pgd, unsigned long addr, int node);
pmd_t *vmemmap_pmd_populate(pud_t *pud, unsigned long addr, int node);
pte_t *vmemmap_pte_populate(pmd_t *pmd, unsigned long addr, int node);
void *vmemmap_alloc_block(unsigned long size, int node);
void vmemmap_verify(pte_t *, int, unsigned long, unsigned long);
int vmemmap_populate_basepages(struct page *start_page,
      unsigned long pages, int node);
int vmemmap_populate(struct page *start_page, unsigned long pages, int node);
void vmemmap_populate_print_last(void);

extern void *alloc_locked_buffer(size_t size);
extern void free_locked_buffer(void *buffer, size_t size);
extern void release_locked_buffer(void *buffer, size_t size);
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci.h" 2
# 13 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci.h"
struct pci_sysdata {
 int domain;
 int node;

 void *iommu;

};

extern int pci_routeirq;
extern int noioapicquirk;
extern int noioapicreroute;


extern struct pci_bus *pci_scan_bus_on_node(int busno, struct pci_ops *ops,
         int node);
extern struct pci_bus *pci_scan_bus_with_sysdata(int busno);

static inline __attribute__((always_inline)) int pci_domain_nr(struct pci_bus *bus)
{
 struct pci_sysdata *sd = bus->sysdata;
 return sd->domain;
}

static inline __attribute__((always_inline)) int pci_proc_domain(struct pci_bus *bus)
{
 return pci_domain_nr(bus);
}







extern unsigned int pcibios_assign_all_busses(void);





extern unsigned long pci_mem_start;





void pcibios_config_init(void);
struct pci_bus *pcibios_scan_root(int bus);

void pcibios_set_master(struct pci_dev *dev);
void pcibios_penalize_isa_irq(int irq, int active);
struct irq_routing_table *pcibios_get_irq_routing_table(void);
int pcibios_set_irq_routing(struct pci_dev *dev, int pin, int irq);



extern int pci_mmap_page_range(struct pci_dev *dev, struct vm_area_struct *vma,
          enum pci_mmap_state mmap_state,
          int write_combine);



extern void early_quirks(void);
static inline __attribute__((always_inline)) void pci_dma_burst_advice(struct pci_dev *pdev,
     enum pci_dma_burst_strategy *strat,
     unsigned long *strategy_parameter)
{
 *strat = PCI_DMA_BURST_INFINITY;
 *strategy_parameter = ~0UL;
}




extern void pci_iommu_alloc(void);






# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci_64.h" 1






static inline __attribute__((always_inline)) void *pci_iommu(struct pci_bus *bus)
{
 struct pci_sysdata *sd = bus->sysdata;
 return sd->iommu;
}

static inline __attribute__((always_inline)) void set_pci_iommu(struct pci_bus *bus, void *val)
{
 struct pci_sysdata *sd = bus->sysdata;
 sd->iommu = val;
}


extern int (*pci_config_read)(int seg, int bus, int dev, int fn,
         int reg, int len, u32 *value);
extern int (*pci_config_write)(int seg, int bus, int dev, int fn,
          int reg, int len, u32 value);

extern void dma32_reserve_bootmem(void);
# 95 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci.h" 2



# 1 "include/asm-generic/pci-dma-compat.h" 1






# 1 "include/linux/dma-mapping.h" 1
# 9 "include/linux/dma-mapping.h"
enum dma_data_direction {
 DMA_BIDIRECTIONAL = 0,
 DMA_TO_DEVICE = 1,
 DMA_FROM_DEVICE = 2,
 DMA_NONE = 3,
};
# 39 "include/linux/dma-mapping.h"
static inline __attribute__((always_inline)) int valid_dma_direction(int dma_direction)
{
 return ((dma_direction == DMA_BIDIRECTIONAL) ||
  (dma_direction == DMA_TO_DEVICE) ||
  (dma_direction == DMA_FROM_DEVICE));
}

static inline __attribute__((always_inline)) int is_device_dma_capable(struct device *dev)
{
 return dev->dma_mask != ((void *)0) && *dev->dma_mask != 0x0ULL;
}

static inline __attribute__((always_inline)) int is_buffer_dma_capable(u64 mask, dma_addr_t addr, size_t size)
{
 return addr + size <= mask;
}


# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h" 1
# 9 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"
# 1 "include/linux/scatterlist.h" 1
# 10 "include/linux/scatterlist.h"
struct sg_table {
 struct scatterlist *sgl;
 unsigned int nents;
 unsigned int orig_nents;
};
# 55 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) void sg_assign_page(struct scatterlist *sg, struct page *page)
{
 unsigned long page_link = sg->page_link & 0x3;





 do { if (__builtin_expect(!!((unsigned long) page & 0x03), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/scatterlist.h"), "i" (63), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);




 sg->page_link = page_link | (unsigned long) page;
}
# 85 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) void sg_set_page(struct scatterlist *sg, struct page *page,
          unsigned int len, unsigned int offset)
{
 sg_assign_page(sg, page);
 sg->offset = offset;
 sg->length = len;
}

static inline __attribute__((always_inline)) struct page *sg_page(struct scatterlist *sg)
{




 return (struct page *)((sg)->page_link & ~0x3);
}
# 109 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) void sg_set_buf(struct scatterlist *sg, const void *buf,
         unsigned int buflen)
{
 sg_set_page(sg, (((struct page *)(0xffffe20000000000UL)) + (__phys_addr((unsigned long)(buf)) >> 12)), buflen, ((unsigned long)(buf) & ~(~(((1UL) << 12)-1))));
}
# 131 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) void sg_chain(struct scatterlist *prv, unsigned int prv_nents,
       struct scatterlist *sgl)
{







 prv[prv_nents - 1].offset = 0;
 prv[prv_nents - 1].length = 0;





 prv[prv_nents - 1].page_link = ((unsigned long) sgl | 0x01) & ~0x02;
}
# 160 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) void sg_mark_end(struct scatterlist *sg)
{






 sg->page_link |= 0x02;
 sg->page_link &= ~0x01;
}
# 182 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) dma_addr_t sg_phys(struct scatterlist *sg)
{
 return ((dma_addr_t)(unsigned long)((sg_page(sg)) - ((struct page *)(0xffffe20000000000UL))) << 12) + sg->offset;
}
# 197 "include/linux/scatterlist.h"
static inline __attribute__((always_inline)) void *sg_virt(struct scatterlist *sg)
{
 return lowmem_page_address(sg_page(sg)) + sg->offset;
}

struct scatterlist *sg_next(struct scatterlist *);
struct scatterlist *sg_last(struct scatterlist *s, unsigned int);
void sg_init_table(struct scatterlist *, unsigned int);
void sg_init_one(struct scatterlist *, const void *, unsigned int);

typedef struct scatterlist *(sg_alloc_fn)(unsigned int, gfp_t);
typedef void (sg_free_fn)(struct scatterlist *, unsigned int);

void __sg_free_table(struct sg_table *, unsigned int, sg_free_fn *);
void sg_free_table(struct sg_table *);
int __sg_alloc_table(struct sg_table *, unsigned int, unsigned int, gfp_t,
       sg_alloc_fn *);
int sg_alloc_table(struct sg_table *, unsigned int, gfp_t);

size_t sg_copy_from_buffer(struct scatterlist *sgl, unsigned int nents,
      void *buf, size_t buflen);
size_t sg_copy_to_buffer(struct scatterlist *sgl, unsigned int nents,
    void *buf, size_t buflen);
# 246 "include/linux/scatterlist.h"
struct sg_mapping_iter {

 struct page *page;
 void *addr;
 size_t length;
 size_t consumed;


 struct scatterlist *__sg;
 unsigned int __nents;
 unsigned int __offset;
 unsigned int __flags;
};

void sg_miter_start(struct sg_mapping_iter *miter, struct scatterlist *sgl,
      unsigned int nents, unsigned int flags);
bool sg_miter_next(struct sg_mapping_iter *miter);
void sg_miter_stop(struct sg_mapping_iter *miter);
# 10 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/swiotlb.h" 1



# 1 "include/linux/swiotlb.h" 1





struct device;
struct dma_attrs;
struct scatterlist;
# 24 "include/linux/swiotlb.h"
extern void
swiotlb_init(void);

extern void *swiotlb_alloc_boot(size_t bytes, unsigned long nslabs);
extern void *swiotlb_alloc(unsigned order, unsigned long nslabs);

extern dma_addr_t swiotlb_phys_to_bus(struct device *hwdev,
          phys_addr_t address);
extern phys_addr_t swiotlb_bus_to_phys(dma_addr_t address);

extern int swiotlb_arch_range_needs_mapping(void *ptr, size_t size);

extern void
*swiotlb_alloc_coherent(struct device *hwdev, size_t size,
   dma_addr_t *dma_handle, gfp_t flags);

extern void
swiotlb_free_coherent(struct device *hwdev, size_t size,
        void *vaddr, dma_addr_t dma_handle);

extern dma_addr_t
swiotlb_map_single(struct device *hwdev, void *ptr, size_t size, int dir);

extern void
swiotlb_unmap_single(struct device *hwdev, dma_addr_t dev_addr,
       size_t size, int dir);

extern dma_addr_t
swiotlb_map_single_attrs(struct device *hwdev, void *ptr, size_t size,
    int dir, struct dma_attrs *attrs);

extern void
swiotlb_unmap_single_attrs(struct device *hwdev, dma_addr_t dev_addr,
      size_t size, int dir, struct dma_attrs *attrs);

extern int
swiotlb_map_sg(struct device *hwdev, struct scatterlist *sg, int nents,
        int direction);

extern void
swiotlb_unmap_sg(struct device *hwdev, struct scatterlist *sg, int nents,
   int direction);

extern int
swiotlb_map_sg_attrs(struct device *hwdev, struct scatterlist *sgl, int nelems,
       int dir, struct dma_attrs *attrs);

extern void
swiotlb_unmap_sg_attrs(struct device *hwdev, struct scatterlist *sgl,
         int nelems, int dir, struct dma_attrs *attrs);

extern void
swiotlb_sync_single_for_cpu(struct device *hwdev, dma_addr_t dev_addr,
       size_t size, int dir);

extern void
swiotlb_sync_sg_for_cpu(struct device *hwdev, struct scatterlist *sg,
   int nelems, int dir);

extern void
swiotlb_sync_single_for_device(struct device *hwdev, dma_addr_t dev_addr,
          size_t size, int dir);

extern void
swiotlb_sync_sg_for_device(struct device *hwdev, struct scatterlist *sg,
      int nelems, int dir);

extern void
swiotlb_sync_single_range_for_cpu(struct device *hwdev, dma_addr_t dev_addr,
      unsigned long offset, size_t size, int dir);

extern void
swiotlb_sync_single_range_for_device(struct device *hwdev, dma_addr_t dev_addr,
         unsigned long offset, size_t size,
         int dir);

extern int
swiotlb_dma_mapping_error(struct device *hwdev, dma_addr_t dma_addr);

extern int
swiotlb_dma_supported(struct device *hwdev, u64 mask);
# 5 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/swiotlb.h" 2



extern int swiotlb_force;


extern int swiotlb;
extern void pci_swiotlb_init(void);







static inline __attribute__((always_inline)) void dma_mark_clean(void *addr, size_t size) {}
# 12 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h" 2
# 1 "include/asm-generic/dma-coherent.h" 1
# 13 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h" 2

extern dma_addr_t bad_dma_address;
extern int iommu_merge;
extern struct device x86_dma_fallback_dev;
extern int panic_on_overflow;

struct dma_mapping_ops {
 int (*mapping_error)(struct device *dev,
      dma_addr_t dma_addr);
 void* (*alloc_coherent)(struct device *dev, size_t size,
    dma_addr_t *dma_handle, gfp_t gfp);
 void (*free_coherent)(struct device *dev, size_t size,
    void *vaddr, dma_addr_t dma_handle);
 dma_addr_t (*map_single)(struct device *hwdev, phys_addr_t ptr,
    size_t size, int direction);
 void (*unmap_single)(struct device *dev, dma_addr_t addr,
    size_t size, int direction);
 void (*sync_single_for_cpu)(struct device *hwdev,
    dma_addr_t dma_handle, size_t size,
    int direction);
 void (*sync_single_for_device)(struct device *hwdev,
    dma_addr_t dma_handle, size_t size,
    int direction);
 void (*sync_single_range_for_cpu)(struct device *hwdev,
    dma_addr_t dma_handle, unsigned long offset,
    size_t size, int direction);
 void (*sync_single_range_for_device)(struct device *hwdev,
    dma_addr_t dma_handle, unsigned long offset,
    size_t size, int direction);
 void (*sync_sg_for_cpu)(struct device *hwdev,
    struct scatterlist *sg, int nelems,
    int direction);
 void (*sync_sg_for_device)(struct device *hwdev,
    struct scatterlist *sg, int nelems,
    int direction);
 int (*map_sg)(struct device *hwdev, struct scatterlist *sg,
    int nents, int direction);
 void (*unmap_sg)(struct device *hwdev,
    struct scatterlist *sg, int nents,
    int direction);
 int (*dma_supported)(struct device *hwdev, u64 mask);
 int is_phys;
};

extern struct dma_mapping_ops *dma_ops;

static inline __attribute__((always_inline)) struct dma_mapping_ops *get_dma_ops(struct device *dev)
{



 if (__builtin_expect(!!(!dev), 0) || !dev->archdata.dma_ops) {
  return dma_ops;
    }
 else
  return dev->archdata.dma_ops;

}


static inline __attribute__((always_inline)) int dma_mapping_error(struct device *dev, dma_addr_t dma_addr)
{
 struct dma_mapping_ops *ops = get_dma_ops(dev);
 if (ops->mapping_error)
  return ops->mapping_error(dev, dma_addr);

 return (dma_addr == bad_dma_address);
}





extern int dma_supported(struct device *hwdev, u64 mask);
extern int dma_set_mask(struct device *dev, u64 mask);

extern void *dma_generic_alloc_coherent(struct device *dev, size_t size,
     dma_addr_t *dma_addr, gfp_t flag);

static inline __attribute__((always_inline)) dma_addr_t
dma_map_single(struct device *hwdev, void *ptr, size_t size,
        int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (98), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 return ops->map_single(hwdev, virt_to_phys(ptr), size, direction);
}

static inline __attribute__((always_inline)) void
dma_unmap_single(struct device *dev, dma_addr_t addr, size_t size,
   int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(dev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (108), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->unmap_single)
  ops->unmap_single(dev, addr, size, direction);
}

static inline __attribute__((always_inline)) int
dma_map_sg(struct device *hwdev, struct scatterlist *sg,
    int nents, int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (119), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 return ops->map_sg(hwdev, sg, nents, direction);
}

static inline __attribute__((always_inline)) void
dma_unmap_sg(struct device *hwdev, struct scatterlist *sg, int nents,
      int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (129), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->unmap_sg)
  ops->unmap_sg(hwdev, sg, nents, direction);
}

static inline __attribute__((always_inline)) void
dma_sync_single_for_cpu(struct device *hwdev, dma_addr_t dma_handle,
   size_t size, int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (140), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->sync_single_for_cpu)
  ops->sync_single_for_cpu(hwdev, dma_handle, size, direction);
 ;
}

static inline __attribute__((always_inline)) void
dma_sync_single_for_device(struct device *hwdev, dma_addr_t dma_handle,
      size_t size, int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (152), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->sync_single_for_device)
  ops->sync_single_for_device(hwdev, dma_handle, size, direction);
 ;
}

static inline __attribute__((always_inline)) void
dma_sync_single_range_for_cpu(struct device *hwdev, dma_addr_t dma_handle,
         unsigned long offset, size_t size, int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (164), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->sync_single_range_for_cpu)
  ops->sync_single_range_for_cpu(hwdev, dma_handle, offset,
            size, direction);
 ;
}

static inline __attribute__((always_inline)) void
dma_sync_single_range_for_device(struct device *hwdev, dma_addr_t dma_handle,
     unsigned long offset, size_t size,
     int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (178), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->sync_single_range_for_device)
  ops->sync_single_range_for_device(hwdev, dma_handle,
        offset, size, direction);
 ;
}

static inline __attribute__((always_inline)) void
dma_sync_sg_for_cpu(struct device *hwdev, struct scatterlist *sg,
      int nelems, int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (191), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->sync_sg_for_cpu)
  ops->sync_sg_for_cpu(hwdev, sg, nelems, direction);
 ;
}

static inline __attribute__((always_inline)) void
dma_sync_sg_for_device(struct device *hwdev, struct scatterlist *sg,
         int nelems, int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(hwdev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (203), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 if (ops->sync_sg_for_device)
  ops->sync_sg_for_device(hwdev, sg, nelems, direction);

 ;
}

static inline __attribute__((always_inline)) dma_addr_t dma_map_page(struct device *dev, struct page *page,
          size_t offset, size_t size,
          int direction)
{
 struct dma_mapping_ops *ops = get_dma_ops(dev);

 do { if (__builtin_expect(!!(!valid_dma_direction(direction)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h"), "i" (216), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 return ops->map_single(dev, ((dma_addr_t)(unsigned long)((page) - ((struct page *)(0xffffe20000000000UL))) << 12) + offset,
          size, direction);
}

static inline __attribute__((always_inline)) void dma_unmap_page(struct device *dev, dma_addr_t addr,
      size_t size, int direction)
{
 dma_unmap_single(dev, addr, size, direction);
}

static inline __attribute__((always_inline)) void
dma_cache_sync(struct device *dev, void *vaddr, size_t size,
 enum dma_data_direction dir)
{
 ;
}

static inline __attribute__((always_inline)) int dma_get_cache_alignment(void)
{


 return boot_cpu_data.x86_clflush_size;
}

static inline __attribute__((always_inline)) unsigned long dma_alloc_coherent_mask(struct device *dev,
          gfp_t gfp)
{
 unsigned long dma_mask = 0;

 dma_mask = dev->coherent_dma_mask;
 if (!dma_mask)
  dma_mask = (gfp & (( gfp_t)0x01u)) ? (((24) == 64) ? ~0ULL : ((1ULL<<(24))-1)) : (((32) == 64) ? ~0ULL : ((1ULL<<(32))-1));

 return dma_mask;
}

static inline __attribute__((always_inline)) gfp_t dma_alloc_coherent_gfp_flags(struct device *dev, gfp_t gfp)
{
 unsigned long dma_mask = dma_alloc_coherent_mask(dev, gfp);

 if (dma_mask <= (((24) == 64) ? ~0ULL : ((1ULL<<(24))-1)))
  gfp |= (( gfp_t)0x01u);

 if (dma_mask <= (((32) == 64) ? ~0ULL : ((1ULL<<(32))-1)) && !(gfp & (( gfp_t)0x01u)))
  gfp |= (( gfp_t)0x04u);

       return gfp;
}

static inline __attribute__((always_inline)) void *
dma_alloc_coherent(struct device *dev, size_t size, dma_addr_t *dma_handle,
  gfp_t gfp)
{
 struct dma_mapping_ops *ops = get_dma_ops(dev);
 void *memory;

 gfp &= ~((( gfp_t)0x01u) | (( gfp_t)0x02u) | (( gfp_t)0x04u));

 if ((0))
  return memory;

 if (!dev) {
  dev = &x86_dma_fallback_dev;
  gfp |= (( gfp_t)0x01u);
 }

 if (!is_device_dma_capable(dev))
  return ((void *)0);

 if (!ops->alloc_coherent)
  return ((void *)0);

 return ops->alloc_coherent(dev, size, dma_handle,
       dma_alloc_coherent_gfp_flags(dev, gfp));
}

static inline __attribute__((always_inline)) void dma_free_coherent(struct device *dev, size_t size,
         void *vaddr, dma_addr_t bus)
{
 struct dma_mapping_ops *ops = get_dma_ops(dev);

 ({ int __ret_warn_on = !!(({ unsigned long _flags; do { (_flags) = __raw_local_save_flags(); } while (0); raw_irqs_disabled_flags(_flags); })); if (__builtin_expect(!!(__ret_warn_on), 0)) warn_slowpath("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/dma-mapping.h", 298, ((void *)0)); __builtin_expect(!!(__ret_warn_on), 0); });

 if ((0))
  return;

 if (ops->free_coherent)
  ops->free_coherent(dev, size, vaddr, bus);
}
# 58 "include/linux/dma-mapping.h" 2
# 66 "include/linux/dma-mapping.h"
static inline __attribute__((always_inline)) u64 dma_get_mask(struct device *dev)
{
 if (dev && dev->dma_mask && *dev->dma_mask)
  return *dev->dma_mask;
 return (((32) == 64) ? ~0ULL : ((1ULL<<(32))-1));
}

extern u64 dma_get_required_mask(struct device *dev);

static inline __attribute__((always_inline)) unsigned int dma_get_max_seg_size(struct device *dev)
{
 return dev->dma_parms ? dev->dma_parms->max_segment_size : 65536;
}

static inline __attribute__((always_inline)) unsigned int dma_set_max_seg_size(struct device *dev,
      unsigned int size)
{
 if (dev->dma_parms) {
  dev->dma_parms->max_segment_size = size;
  return 0;
 } else
  return -5;
}

static inline __attribute__((always_inline)) unsigned long dma_get_seg_boundary(struct device *dev)
{
 return dev->dma_parms ?
  dev->dma_parms->segment_boundary_mask : 0xffffffff;
}

static inline __attribute__((always_inline)) int dma_set_seg_boundary(struct device *dev, unsigned long mask)
{
 if (dev->dma_parms) {
  dev->dma_parms->segment_boundary_mask = mask;
  return 0;
 } else
  return -5;
}
# 112 "include/linux/dma-mapping.h"
static inline __attribute__((always_inline)) int
dma_declare_coherent_memory(struct device *dev, dma_addr_t bus_addr,
       dma_addr_t device_addr, size_t size, int flags)
{
 return 0;
}

static inline __attribute__((always_inline)) void
dma_release_declared_memory(struct device *dev)
{
}

static inline __attribute__((always_inline)) void *
dma_mark_declared_memory_occupied(struct device *dev,
      dma_addr_t device_addr, size_t size)
{
 return ERR_PTR(-16);
}





extern void *dmam_alloc_coherent(struct device *dev, size_t size,
     dma_addr_t *dma_handle, gfp_t gfp);
extern void dmam_free_coherent(struct device *dev, size_t size, void *vaddr,
          dma_addr_t dma_handle);
extern void *dmam_alloc_noncoherent(struct device *dev, size_t size,
        dma_addr_t *dma_handle, gfp_t gfp);
extern void dmam_free_noncoherent(struct device *dev, size_t size, void *vaddr,
      dma_addr_t dma_handle);






static inline __attribute__((always_inline)) int dmam_declare_coherent_memory(struct device *dev,
    dma_addr_t bus_addr, dma_addr_t device_addr,
    size_t size, gfp_t gfp)
{
 return 0;
}

static inline __attribute__((always_inline)) void dmam_release_declared_memory(struct device *dev)
{
}



struct dma_attrs;
# 8 "include/asm-generic/pci-dma-compat.h" 2




static inline __attribute__((always_inline)) int
pci_dma_supported(struct pci_dev *hwdev, u64 mask)
{
 return dma_supported(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, mask);
}

static inline __attribute__((always_inline)) void *
pci_alloc_consistent(struct pci_dev *hwdev, size_t size,
       dma_addr_t *dma_handle)
{
 return dma_alloc_coherent(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, size, dma_handle, ((( gfp_t)0x20u)));
}

static inline __attribute__((always_inline)) void
pci_free_consistent(struct pci_dev *hwdev, size_t size,
      void *vaddr, dma_addr_t dma_handle)
{
 dma_free_coherent(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, size, vaddr, dma_handle);
}

static inline __attribute__((always_inline)) dma_addr_t
pci_map_single(struct pci_dev *hwdev, void *ptr, size_t size, int direction)
{
 return dma_map_single(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, ptr, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_unmap_single(struct pci_dev *hwdev, dma_addr_t dma_addr,
   size_t size, int direction)
{
 dma_unmap_single(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_addr, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) dma_addr_t
pci_map_page(struct pci_dev *hwdev, struct page *page,
      unsigned long offset, size_t size, int direction)
{
 return dma_map_page(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, page, offset, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_unmap_page(struct pci_dev *hwdev, dma_addr_t dma_address,
        size_t size, int direction)
{
 dma_unmap_page(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_address, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) int
pci_map_sg(struct pci_dev *hwdev, struct scatterlist *sg,
    int nents, int direction)
{
 return dma_map_sg(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nents, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_unmap_sg(struct pci_dev *hwdev, struct scatterlist *sg,
      int nents, int direction)
{
 dma_unmap_sg(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nents, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_single_for_cpu(struct pci_dev *hwdev, dma_addr_t dma_handle,
      size_t size, int direction)
{
 dma_sync_single_for_cpu(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_handle, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_single_for_device(struct pci_dev *hwdev, dma_addr_t dma_handle,
      size_t size, int direction)
{
 dma_sync_single_for_device(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_handle, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_sg_for_cpu(struct pci_dev *hwdev, struct scatterlist *sg,
  int nelems, int direction)
{
 dma_sync_sg_for_cpu(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nelems, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_sg_for_device(struct pci_dev *hwdev, struct scatterlist *sg,
  int nelems, int direction)
{
 dma_sync_sg_for_device(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nelems, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) int
pci_dma_mapping_error(struct pci_dev *pdev, dma_addr_t dma_addr)
{
 return dma_mapping_error(&pdev->dev, dma_addr);
}
# 99 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci.h" 2


# 1 "include/asm-generic/pci.h" 1
# 17 "include/asm-generic/pci.h"
static inline __attribute__((always_inline)) void
pcibios_resource_to_bus(struct pci_dev *dev, struct pci_bus_region *region,
    struct resource *res)
{
 region->start = res->start;
 region->end = res->end;
}

static inline __attribute__((always_inline)) void
pcibios_bus_to_resource(struct pci_dev *dev, struct resource *res,
   struct pci_bus_region *region)
{
 res->start = region->start;
 res->end = region->end;
}

static inline __attribute__((always_inline)) struct resource *
pcibios_select_root(struct pci_dev *pdev, struct resource *res)
{
 struct resource *root = ((void *)0);

 if (res->flags & 0x00000100)
  root = &ioport_resource;
 if (res->flags & 0x00000200)
  root = &iomem_resource;

 return root;
}




static inline __attribute__((always_inline)) int pci_get_legacy_ide_irq(struct pci_dev *dev, int channel)
{
 return channel ? 15 : 14;
}
# 102 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/pci.h" 2



static inline __attribute__((always_inline)) int __pcibus_to_node(const struct pci_bus *bus)
{
 const struct pci_sysdata *sd = bus->sysdata;

 return sd->node;
}

static inline __attribute__((always_inline)) cpumask_t __pcibus_to_cpumask(struct pci_bus *bus)
{
 return node_to_cpumask(__pcibus_to_node(bus));
}

static inline __attribute__((always_inline)) const struct cpumask *
cpumask_of_pcibus(const struct pci_bus *bus)
{
 return cpumask_of_node(__pcibus_to_node(bus));
}
# 1058 "include/linux/pci.h" 2
# 1076 "include/linux/pci.h"
static inline __attribute__((always_inline)) void *pci_get_drvdata(struct pci_dev *pdev)
{
 return dev_get_drvdata(&pdev->dev);
}

static inline __attribute__((always_inline)) void pci_set_drvdata(struct pci_dev *pdev, void *data)
{
 dev_set_drvdata(&pdev->dev, data);
}




static inline __attribute__((always_inline)) const char *pci_name(struct pci_dev *pdev)
{
 return dev_name(&pdev->dev);
}






static inline __attribute__((always_inline)) void pci_resource_to_user(const struct pci_dev *dev, int bar,
  const struct resource *rsrc, resource_size_t *start,
  resource_size_t *end)
{
 *start = rsrc->start;
 *end = rsrc->end;
}
# 1116 "include/linux/pci.h"
struct pci_fixup {
 u16 vendor, device;
 void (*hook)(struct pci_dev *dev);
};

enum pci_fixup_pass {
 pci_fixup_early,
 pci_fixup_header,
 pci_fixup_final,
 pci_fixup_enable,
 pci_fixup_resume,
 pci_fixup_suspend,
 pci_fixup_resume_early,
};
# 1158 "include/linux/pci.h"
void pci_fixup_device(enum pci_fixup_pass pass, struct pci_dev *dev);

void __attribute__((noderef, address_space(2))) *pcim_iomap(struct pci_dev *pdev, int bar, unsigned long maxlen);
void pcim_iounmap(struct pci_dev *pdev, void __attribute__((noderef, address_space(2))) *addr);
void __attribute__((noderef, address_space(2))) * const *pcim_iomap_table(struct pci_dev *pdev);
int pcim_iomap_regions(struct pci_dev *pdev, u16 mask, const char *name);
int pcim_iomap_regions_request_all(struct pci_dev *pdev, u16 mask,
       const char *name);
void pcim_iounmap_regions(struct pci_dev *pdev, u16 mask);

extern int pci_pci_problems;
# 1177 "include/linux/pci.h"
extern unsigned long pci_cardbus_io_size;
extern unsigned long pci_cardbus_mem_size;

int pcibios_add_platform_entries(struct pci_dev *dev);
void pcibios_disable_device(struct pci_dev *dev);
int pcibios_set_pcie_reset_state(struct pci_dev *dev,
     enum pcie_reset_state state);


extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) pci_mmcfg_early_init(void);
extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) pci_mmcfg_late_init(void);





int pci_ext_cfg_avail(struct pci_dev *dev);

void __attribute__((noderef, address_space(2))) *pci_ioremap_bar(struct pci_dev *pdev, int bar);
# 34 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2

# 1 "include/linux/gameport.h" 1
# 19 "include/linux/gameport.h"
struct gameport {

 void *port_data;
 char name[32];
 char phys[32];

 int io;
 int speed;
 int fuzz;

 void (*trigger)(struct gameport *);
 unsigned char (*read)(struct gameport *);
 int (*cooked_read)(struct gameport *, int *, int *);
 int (*calibrate)(struct gameport *, int *, int *);
 int (*open)(struct gameport *, int);
 void (*close)(struct gameport *);

 struct timer_list poll_timer;
 unsigned int poll_interval;
 spinlock_t timer_lock;
 unsigned int poll_cnt;
 void (*poll_handler)(struct gameport *);

 struct gameport *parent, *child;

 struct gameport_driver *drv;
 struct mutex drv_mutex;

 struct device dev;
 unsigned int registered;

 struct list_head node;
};


struct gameport_driver {

 void *private;
 char *description;

 int (*connect)(struct gameport *, struct gameport_driver *drv);
 int (*reconnect)(struct gameport *);
 void (*disconnect)(struct gameport *);

 struct device_driver driver;

 unsigned int ignore;
};


int gameport_open(struct gameport *gameport, struct gameport_driver *drv, int mode);
void gameport_close(struct gameport *gameport);



void __gameport_register_port(struct gameport *gameport, struct module *owner);
static inline __attribute__((always_inline)) void gameport_register_port(struct gameport *gameport)
{
 __gameport_register_port(gameport, (&__this_module));
}

void gameport_unregister_port(struct gameport *gameport);

void gameport_set_phys(struct gameport *gameport, const char *fmt, ...)
 __attribute__ ((format (printf, 2, 3)));
# 105 "include/linux/gameport.h"
static inline __attribute__((always_inline)) struct gameport *gameport_allocate_port(void)
{
 struct gameport *gameport = kzalloc(sizeof(struct gameport), ((( gfp_t)0x10u) | (( gfp_t)0x40u) | (( gfp_t)0x80u)));

 return gameport;
}

static inline __attribute__((always_inline)) void gameport_free_port(struct gameport *gameport)
{
 kfree(gameport);
}

static inline __attribute__((always_inline)) void gameport_set_name(struct gameport *gameport, const char *name)
{
 strlcpy(gameport->name, name, sizeof(gameport->name));
}





static inline __attribute__((always_inline)) void *gameport_get_drvdata(struct gameport *gameport)
{
 return dev_get_drvdata(&gameport->dev);
}

static inline __attribute__((always_inline)) void gameport_set_drvdata(struct gameport *gameport, void *data)
{
 dev_set_drvdata(&gameport->dev, data);
}




static inline __attribute__((always_inline)) int gameport_pin_driver(struct gameport *gameport)
{
 return mutex_lock_interruptible_nested(&gameport->drv_mutex, 0);
}

static inline __attribute__((always_inline)) void gameport_unpin_driver(struct gameport *gameport)
{
 mutex_unlock(&gameport->drv_mutex);
}

int __gameport_register_driver(struct gameport_driver *drv,
    struct module *owner, const char *mod_name);
static inline __attribute__((always_inline)) int __attribute__((warn_unused_result)) gameport_register_driver(struct gameport_driver *drv)
{
 return __gameport_register_driver(drv, (&__this_module), "ens1371");
}

void gameport_unregister_driver(struct gameport_driver *drv);
# 177 "include/linux/gameport.h"
static inline __attribute__((always_inline)) void gameport_trigger(struct gameport *gameport)
{
 if (gameport->trigger)
  gameport->trigger(gameport);
 else
  outb(0xff, gameport->io);
}

static inline __attribute__((always_inline)) unsigned char gameport_read(struct gameport *gameport)
{
 if (gameport->read)
  return gameport->read(gameport);
 else
  return inb(gameport->io);
}

static inline __attribute__((always_inline)) int gameport_cooked_read(struct gameport *gameport, int *axes, int *buttons)
{
 if (gameport->cooked_read)
  return gameport->cooked_read(gameport, axes, buttons);
 else
  return -1;
}

static inline __attribute__((always_inline)) int gameport_calibrate(struct gameport *gameport, int *axes, int *max)
{
 if (gameport->calibrate)
  return gameport->calibrate(gameport, axes, max);
 else
  return -1;
}

static inline __attribute__((always_inline)) int gameport_time(struct gameport *gameport, int time)
{
 return (time * gameport->speed) / 1000;
}

static inline __attribute__((always_inline)) void gameport_set_poll_handler(struct gameport *gameport, void (*handler)(struct gameport *))
{
 gameport->poll_handler = handler;
}

static inline __attribute__((always_inline)) void gameport_set_poll_interval(struct gameport *gameport, unsigned int msecs)
{
 gameport->poll_interval = msecs;
}

void gameport_start_polling(struct gameport *gameport);
void gameport_stop_polling(struct gameport *gameport);
# 36 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2




# 1 "include/sound/core.h" 1
# 44 "include/sound/core.h"
struct pci_dev;






typedef int snd_device_type_t;
# 67 "include/sound/core.h"
typedef int snd_device_state_t;




typedef int snd_device_cmd_t;




struct snd_device;

struct snd_device_ops {
    int (* __attribute__((noderef, address_space(2))) dev_free)(struct snd_device *dev);
    int (* __attribute__((noderef, address_space(2))) dev_register)(struct snd_device *dev);
    int (* __attribute__((noderef, address_space(2))) dev_disconnect)(struct snd_device *dev);
};

struct snd_device {
 struct list_head list;
 struct snd_card *card;
 snd_device_state_t state;
 snd_device_type_t type;
 void *device_data;
    struct snd_device_ops *__attribute__((noderef, address_space(2))) ops;
};





struct snd_monitor_file {
    struct file *__attribute__((recursive)) file;
    struct snd_monitor_file *__attribute__((recursive)) next;
 const struct file_operations *disconnected_f_op;
 struct list_head shutdown_list;
};



struct snd_card {
 int number;


 char id[16];
 char driver[16];
 char shortname[32];
 char longname[80];
 char mixername[80];
 char components[128];

 struct module *module;

    void *__attribute__((recursive)) private_data;
 void (*private_free) (struct snd_card *card);

 struct list_head devices;

 unsigned int last_numid;
 struct rw_semaphore controls_rwsem;
 rwlock_t ctl_files_rwlock;
 int controls_count;
 int user_ctl_count;
 struct list_head controls;
 struct list_head ctl_files;

 struct snd_info_entry *proc_root;
 struct snd_info_entry *proc_id;
    struct proc_dir_entry *__attribute__((recursive)) proc_root_link;

    struct snd_monitor_file *__attribute__((recursive)) files;
 struct snd_shutdown_f_ops *s_f_ops;

 spinlock_t files_lock;
 int shutdown;
 int free_on_last_close;
 wait_queue_head_t shutdown_sleep;
    struct device * __attribute__((recursive)) dev;





 unsigned int power_state;
 struct mutex power_lock;
 wait_queue_head_t power_sleep;



 struct snd_mixer_oss *mixer_oss;
 int mixer_oss_change_count;

};


static inline __attribute__((always_inline)) void snd_power_lock(struct snd_card *card)
{
 mutex_lock_nested(&card->power_lock, 0);
}

static inline __attribute__((always_inline)) void snd_power_unlock(struct snd_card *card)
{
 mutex_unlock(&card->power_lock);
}

static inline __attribute__((always_inline)) unsigned int snd_power_get_state(struct snd_card *card)
{
 return card->power_state;
}

static inline __attribute__((always_inline)) void snd_power_change_state(struct snd_card *card, unsigned int state)
{
 card->power_state = state;
 __wake_up(&card->power_sleep, (1 | 2), 1, ((void *)0));
}


int snd_power_wait(struct snd_card *card, unsigned int power_state);
# 196 "include/sound/core.h"
struct snd_minor {
 int type;
 int card;
 int device;
 const struct file_operations *f_ops;
 void *private_data;
 struct device *dev;
};


static inline __attribute__((always_inline)) struct device *snd_card_get_device_link(struct snd_card *card)
{

 return card ? card->dev : ((void *)0);



}



extern int snd_major;
extern int snd_ecards_limit;
extern struct class *sound_class;

void snd_request_card(int card);

int snd_register_device_for_dev(int type, struct snd_card *card,
    int dev,
    const struct file_operations *f_ops,
    void *private_data,
    const char *name,
    struct device *device);
# 247 "include/sound/core.h"
static inline __attribute__((always_inline)) int snd_register_device(int type, struct snd_card *card, int dev,
          const struct file_operations *f_ops,
          void *private_data,
          const char *name)
{
 return snd_register_device_for_dev(type, card, dev, f_ops,
        private_data, name,
        snd_card_get_device_link(card));
}

int snd_unregister_device(int type, struct snd_card *card, int dev);
void *snd_lookup_minor_data(unsigned int minor, int type);
int snd_add_device_sysfs_file(int type, struct snd_card *card, int dev,
         struct device_attribute *attr);


int snd_register_oss_device(int type, struct snd_card *card, int dev,
       const struct file_operations *f_ops, void *private_data,
       const char *name);
int snd_unregister_oss_device(int type, struct snd_card *card, int dev);
void *snd_lookup_oss_minor_data(unsigned int minor, int type);


int snd_minor_info_init(void);
int snd_minor_info_done(void);




int snd_minor_info_oss_init(void);
int snd_minor_info_oss_done(void);







int copy_to_user_fromio(void __attribute__((noderef, address_space(1))) *dst, const volatile void __attribute__((noderef, address_space(2))) *src, size_t count);
int copy_from_user_toio(volatile void __attribute__((noderef, address_space(2))) *dst, const void __attribute__((noderef, address_space(1))) *src, size_t count);



extern struct snd_card *snd_cards[32];
int snd_card_locked(int card);




extern int (*snd_mixer_oss_notify_callback)(struct snd_card *card, int cmd);


struct snd_card *snd_card_new(int idx, const char *id,
    struct module *module, int extra_size);
int snd_card_disconnect(struct snd_card *card);
int snd_card_free(struct snd_card *card);
int snd_card_free_when_closed(struct snd_card *card);
int snd_card_register(struct snd_card *card);
int snd_card_info_init(void);
int snd_card_info_done(void);
int snd_component_add(struct snd_card *card, const char *component);
int snd_card_file_add(struct snd_card *card, struct file *file);
int snd_card_file_remove(struct snd_card *card, struct file *file);







int snd_device_new(struct snd_card *card, snd_device_type_t type,
     void *device_data, struct snd_device_ops *ops);
int snd_device_register(struct snd_card *card, void *device_data);
int snd_device_register_all(struct snd_card *card);
int snd_device_disconnect(struct snd_card *card, void *device_data);
int snd_device_disconnect_all(struct snd_card *card);
int snd_device_free(struct snd_card *card, void *device_data);
int snd_device_free_all(struct snd_card *card, snd_device_cmd_t cmd);






void snd_dma_program(unsigned long dma, unsigned long addr, unsigned int size, unsigned short mode);
void snd_dma_disable(unsigned long dma);
unsigned int snd_dma_pointer(unsigned long dma, unsigned int size);



struct resource;
void release_and_free_resource(struct resource *res);
# 412 "include/sound/core.h"
static inline __attribute__((always_inline)) int __snd_bug_on(int cond)
{
 return 0;
}
# 446 "include/sound/core.h"
struct snd_pci_quirk {
 unsigned short subvendor;
 unsigned short subdevice;
 int value;



};
# 466 "include/sound/core.h"
const struct snd_pci_quirk *
snd_pci_quirk_lookup(struct pci_dev *pci, const struct snd_pci_quirk *list);
# 41 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 1 "include/sound/control.h" 1
# 25 "include/sound/control.h"
# 1 "include/sound/asound.h" 1
# 27 "include/sound/asound.h"
# 1 "include/linux/ioctl.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ioctl.h" 1
# 1 "include/asm-generic/ioctl.h" 1
# 73 "include/asm-generic/ioctl.h"
extern unsigned int __invalid_size_argument_for_IOC;
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/ioctl.h" 2
# 5 "include/linux/ioctl.h" 2
# 28 "include/sound/asound.h" 2
# 63 "include/sound/asound.h"
struct snd_aes_iec958 {
 unsigned char status[24];
 unsigned char subcode[147];
 unsigned char pad;
 unsigned char dig_subframe[4];
};
# 78 "include/sound/asound.h"
enum {
 SNDRV_HWDEP_IFACE_OPL2 = 0,
 SNDRV_HWDEP_IFACE_OPL3,
 SNDRV_HWDEP_IFACE_OPL4,
 SNDRV_HWDEP_IFACE_SB16CSP,
 SNDRV_HWDEP_IFACE_EMU10K1,
 SNDRV_HWDEP_IFACE_YSS225,
 SNDRV_HWDEP_IFACE_ICS2115,
 SNDRV_HWDEP_IFACE_SSCAPE,
 SNDRV_HWDEP_IFACE_VX,
 SNDRV_HWDEP_IFACE_MIXART,
 SNDRV_HWDEP_IFACE_USX2Y,
 SNDRV_HWDEP_IFACE_EMUX_WAVETABLE,
 SNDRV_HWDEP_IFACE_BLUETOOTH,
 SNDRV_HWDEP_IFACE_USX2Y_PCM,
 SNDRV_HWDEP_IFACE_PCXHR,
 SNDRV_HWDEP_IFACE_SB_RC,
 SNDRV_HWDEP_IFACE_HDA,
 SNDRV_HWDEP_IFACE_USB_STREAM,


 SNDRV_HWDEP_IFACE_LAST = SNDRV_HWDEP_IFACE_USB_STREAM
};

struct snd_hwdep_info {
 unsigned int device;
 int card;
 unsigned char id[64];
 unsigned char name[80];
 int iface;
 unsigned char reserved[64];
};


struct snd_hwdep_dsp_status {
 unsigned int version;
 unsigned char id[32];
 unsigned int num_dsps;
 unsigned int dsp_loaded;
 unsigned int chip_ready;
 unsigned char reserved[16];
};

struct snd_hwdep_dsp_image {
 unsigned int index;
 unsigned char name[64];
 unsigned char __attribute__((noderef, address_space(1))) *image;
 size_t length;
 unsigned long driver_data;
};

enum {
 SNDRV_HWDEP_IOCTL_PVERSION = (((2U) << (((0 +8)+8)+14)) | ((('H')) << (0 +8)) | (((0x00)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_HWDEP_IOCTL_INFO = (((2U) << (((0 +8)+8)+14)) | ((('H')) << (0 +8)) | (((0x01)) << 0) | (((((sizeof(struct snd_hwdep_info) == sizeof(struct snd_hwdep_info[1]) && sizeof(struct snd_hwdep_info) < (1 << 14)) ? sizeof(struct snd_hwdep_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_HWDEP_IOCTL_DSP_STATUS = (((2U) << (((0 +8)+8)+14)) | ((('H')) << (0 +8)) | (((0x02)) << 0) | (((((sizeof(struct snd_hwdep_dsp_status) == sizeof(struct snd_hwdep_dsp_status[1]) && sizeof(struct snd_hwdep_dsp_status) < (1 << 14)) ? sizeof(struct snd_hwdep_dsp_status) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_HWDEP_IOCTL_DSP_LOAD = (((1U) << (((0 +8)+8)+14)) | ((('H')) << (0 +8)) | (((0x03)) << 0) | (((((sizeof(struct snd_hwdep_dsp_image) == sizeof(struct snd_hwdep_dsp_image[1]) && sizeof(struct snd_hwdep_dsp_image) < (1 << 14)) ? sizeof(struct snd_hwdep_dsp_image) : __invalid_size_argument_for_IOC))) << ((0 +8)+8)))
};
# 144 "include/sound/asound.h"
typedef unsigned long snd_pcm_uframes_t;
typedef signed long snd_pcm_sframes_t;

enum {
 SNDRV_PCM_CLASS_GENERIC = 0,
 SNDRV_PCM_CLASS_MULTI,
 SNDRV_PCM_CLASS_MODEM,
 SNDRV_PCM_CLASS_DIGITIZER,

 SNDRV_PCM_CLASS_LAST = SNDRV_PCM_CLASS_DIGITIZER,
};

enum {
 SNDRV_PCM_SUBCLASS_GENERIC_MIX = 0,
 SNDRV_PCM_SUBCLASS_MULTI_MIX,

 SNDRV_PCM_SUBCLASS_LAST = SNDRV_PCM_SUBCLASS_MULTI_MIX,
};

enum {
 SNDRV_PCM_STREAM_PLAYBACK = 0,
 SNDRV_PCM_STREAM_CAPTURE,
 SNDRV_PCM_STREAM_LAST = SNDRV_PCM_STREAM_CAPTURE,
};

typedef int snd_pcm_access_t;







typedef int snd_pcm_format_t;
# 241 "include/sound/asound.h"
typedef int snd_pcm_subformat_t;
# 260 "include/sound/asound.h"
typedef int snd_pcm_state_t;
# 272 "include/sound/asound.h"
enum {
 SNDRV_PCM_MMAP_OFFSET_DATA = 0x00000000,
 SNDRV_PCM_MMAP_OFFSET_STATUS = 0x80000000,
 SNDRV_PCM_MMAP_OFFSET_CONTROL = 0x81000000,
};

union snd_pcm_sync_id {
 unsigned char id[16];
 unsigned short id16[8];
 unsigned int id32[4];
};

struct snd_pcm_info {
 unsigned int device;
 unsigned int subdevice;
 int stream;
 int card;
 unsigned char id[64];
 unsigned char name[80];
 unsigned char subname[32];
 int dev_class;
 int dev_subclass;
 unsigned int subdevices_count;
 unsigned int subdevices_avail;
 union snd_pcm_sync_id sync;
 unsigned char reserved[64];
};

typedef int snd_pcm_hw_param_t;
# 334 "include/sound/asound.h"
struct snd_interval {
 unsigned int min, max;
 unsigned int openmin:1,
       openmax:1,
       integer:1,
       empty:1;
};



struct snd_mask {
 u_int32_t bits[(256 +31)/32];
};

struct snd_pcm_hw_params {
 unsigned int flags;
 struct snd_mask masks[2 -
          0 + 1];
 struct snd_mask mres[5];
 struct snd_interval intervals[19 -
            8 + 1];
 struct snd_interval ires[9];
 unsigned int rmask;
 unsigned int cmask;
 unsigned int info;
 unsigned int msbits;
 unsigned int rate_num;
 unsigned int rate_den;
 snd_pcm_uframes_t fifo_size;
 unsigned char reserved[64];
};

enum {
 SNDRV_PCM_TSTAMP_NONE = 0,
 SNDRV_PCM_TSTAMP_ENABLE,
 SNDRV_PCM_TSTAMP_LAST = SNDRV_PCM_TSTAMP_ENABLE,
};

struct snd_pcm_sw_params {
 int tstamp_mode;
 unsigned int period_step;
 unsigned int sleep_min;
 snd_pcm_uframes_t avail_min;
 snd_pcm_uframes_t xfer_align;
 snd_pcm_uframes_t start_threshold;
 snd_pcm_uframes_t stop_threshold;
 snd_pcm_uframes_t silence_threshold;
 snd_pcm_uframes_t silence_size;
 snd_pcm_uframes_t boundary;
 unsigned char reserved[64];
};

struct snd_pcm_channel_info {
 unsigned int channel;
 off_t offset;
 unsigned int first;
 unsigned int step;
};

struct snd_pcm_status {
 snd_pcm_state_t state;
 struct timespec trigger_tstamp;
 struct timespec tstamp;
 snd_pcm_uframes_t appl_ptr;
 snd_pcm_uframes_t hw_ptr;
 snd_pcm_sframes_t delay;
 snd_pcm_uframes_t avail;
 snd_pcm_uframes_t avail_max;
 snd_pcm_uframes_t overrange;
 snd_pcm_state_t suspended_state;
 unsigned char reserved[60];
};

struct snd_pcm_mmap_status {
 snd_pcm_state_t state;
 int pad1;
 snd_pcm_uframes_t hw_ptr;
 struct timespec tstamp;
 snd_pcm_state_t suspended_state;
};

struct snd_pcm_mmap_control {
 snd_pcm_uframes_t appl_ptr;
 snd_pcm_uframes_t avail_min;
};





struct snd_pcm_sync_ptr {
 unsigned int flags;
 union {
  struct snd_pcm_mmap_status status;
  unsigned char reserved[64];
 } s;
 union {
  struct snd_pcm_mmap_control control;
  unsigned char reserved[64];
 } c;
};

struct snd_xferi {
 snd_pcm_sframes_t result;
 void __attribute__((noderef, address_space(1))) *buf;
 snd_pcm_uframes_t frames;
};

struct snd_xfern {
 snd_pcm_sframes_t result;
 void __attribute__((noderef, address_space(1))) * __attribute__((noderef, address_space(1))) *bufs;
 snd_pcm_uframes_t frames;
};

enum {
 SNDRV_PCM_TSTAMP_TYPE_GETTIMEOFDAY = 0,
 SNDRV_PCM_TSTAMP_TYPE_MONOTONIC,
 SNDRV_PCM_TSTAMP_TYPE_LAST = SNDRV_PCM_TSTAMP_TYPE_MONOTONIC,
};

enum {
 SNDRV_PCM_IOCTL_PVERSION = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x00)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_INFO = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x01)) << 0) | (((((sizeof(struct snd_pcm_info) == sizeof(struct snd_pcm_info[1]) && sizeof(struct snd_pcm_info) < (1 << 14)) ? sizeof(struct snd_pcm_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_TSTAMP = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x02)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_TTSTAMP = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x03)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_HW_REFINE = (((2U|1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x10)) << 0) | (((((sizeof(struct snd_pcm_hw_params) == sizeof(struct snd_pcm_hw_params[1]) && sizeof(struct snd_pcm_hw_params) < (1 << 14)) ? sizeof(struct snd_pcm_hw_params) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_HW_PARAMS = (((2U|1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x11)) << 0) | (((((sizeof(struct snd_pcm_hw_params) == sizeof(struct snd_pcm_hw_params[1]) && sizeof(struct snd_pcm_hw_params) < (1 << 14)) ? sizeof(struct snd_pcm_hw_params) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_HW_FREE = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x12)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_SW_PARAMS = (((2U|1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x13)) << 0) | (((((sizeof(struct snd_pcm_sw_params) == sizeof(struct snd_pcm_sw_params[1]) && sizeof(struct snd_pcm_sw_params) < (1 << 14)) ? sizeof(struct snd_pcm_sw_params) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_STATUS = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x20)) << 0) | (((((sizeof(struct snd_pcm_status) == sizeof(struct snd_pcm_status[1]) && sizeof(struct snd_pcm_status) < (1 << 14)) ? sizeof(struct snd_pcm_status) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_DELAY = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x21)) << 0) | (((((sizeof(snd_pcm_sframes_t) == sizeof(snd_pcm_sframes_t[1]) && sizeof(snd_pcm_sframes_t) < (1 << 14)) ? sizeof(snd_pcm_sframes_t) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_HWSYNC = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x22)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_SYNC_PTR = (((2U|1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x23)) << 0) | (((((sizeof(struct snd_pcm_sync_ptr) == sizeof(struct snd_pcm_sync_ptr[1]) && sizeof(struct snd_pcm_sync_ptr) < (1 << 14)) ? sizeof(struct snd_pcm_sync_ptr) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_CHANNEL_INFO = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x32)) << 0) | (((((sizeof(struct snd_pcm_channel_info) == sizeof(struct snd_pcm_channel_info[1]) && sizeof(struct snd_pcm_channel_info) < (1 << 14)) ? sizeof(struct snd_pcm_channel_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_PREPARE = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x40)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_RESET = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x41)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_START = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x42)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_DROP = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x43)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_DRAIN = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x44)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_PAUSE = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x45)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_REWIND = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x46)) << 0) | (((((sizeof(snd_pcm_uframes_t) == sizeof(snd_pcm_uframes_t[1]) && sizeof(snd_pcm_uframes_t) < (1 << 14)) ? sizeof(snd_pcm_uframes_t) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_RESUME = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x47)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_XRUN = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x48)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_FORWARD = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x49)) << 0) | (((((sizeof(snd_pcm_uframes_t) == sizeof(snd_pcm_uframes_t[1]) && sizeof(snd_pcm_uframes_t) < (1 << 14)) ? sizeof(snd_pcm_uframes_t) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_WRITEI_FRAMES = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x50)) << 0) | (((((sizeof(struct snd_xferi) == sizeof(struct snd_xferi[1]) && sizeof(struct snd_xferi) < (1 << 14)) ? sizeof(struct snd_xferi) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_READI_FRAMES = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x51)) << 0) | (((((sizeof(struct snd_xferi) == sizeof(struct snd_xferi[1]) && sizeof(struct snd_xferi) < (1 << 14)) ? sizeof(struct snd_xferi) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_WRITEN_FRAMES = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x52)) << 0) | (((((sizeof(struct snd_xfern) == sizeof(struct snd_xfern[1]) && sizeof(struct snd_xfern) < (1 << 14)) ? sizeof(struct snd_xfern) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_READN_FRAMES = (((2U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x53)) << 0) | (((((sizeof(struct snd_xfern) == sizeof(struct snd_xfern[1]) && sizeof(struct snd_xfern) < (1 << 14)) ? sizeof(struct snd_xfern) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_LINK = (((1U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x60)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_PCM_IOCTL_UNLINK = (((0U) << (((0 +8)+8)+14)) | ((('A')) << (0 +8)) | (((0x61)) << 0) | ((0) << ((0 +8)+8))),
};
# 501 "include/sound/asound.h"
enum {
 SNDRV_RAWMIDI_STREAM_OUTPUT = 0,
 SNDRV_RAWMIDI_STREAM_INPUT,
 SNDRV_RAWMIDI_STREAM_LAST = SNDRV_RAWMIDI_STREAM_INPUT,
};





struct snd_rawmidi_info {
 unsigned int device;
 unsigned int subdevice;
 int stream;
 int card;
 unsigned int flags;
 unsigned char id[64];
 unsigned char name[80];
 unsigned char subname[32];
 unsigned int subdevices_count;
 unsigned int subdevices_avail;
 unsigned char reserved[64];
};

struct snd_rawmidi_params {
 int stream;
 size_t buffer_size;
 size_t avail_min;
 unsigned int no_active_sensing: 1;
 unsigned char reserved[16];
};

struct snd_rawmidi_status {
 int stream;
 struct timespec tstamp;
 size_t avail;
 size_t xruns;
 unsigned char reserved[16];
};

enum {
 SNDRV_RAWMIDI_IOCTL_PVERSION = (((2U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0x00)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_RAWMIDI_IOCTL_INFO = (((2U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0x01)) << 0) | (((((sizeof(struct snd_rawmidi_info) == sizeof(struct snd_rawmidi_info[1]) && sizeof(struct snd_rawmidi_info) < (1 << 14)) ? sizeof(struct snd_rawmidi_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_RAWMIDI_IOCTL_PARAMS = (((2U|1U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0x10)) << 0) | (((((sizeof(struct snd_rawmidi_params) == sizeof(struct snd_rawmidi_params[1]) && sizeof(struct snd_rawmidi_params) < (1 << 14)) ? sizeof(struct snd_rawmidi_params) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_RAWMIDI_IOCTL_STATUS = (((2U|1U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0x20)) << 0) | (((((sizeof(struct snd_rawmidi_status) == sizeof(struct snd_rawmidi_status[1]) && sizeof(struct snd_rawmidi_status) < (1 << 14)) ? sizeof(struct snd_rawmidi_status) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_RAWMIDI_IOCTL_DROP = (((1U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0x30)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_RAWMIDI_IOCTL_DRAIN = (((1U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0x31)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
};







enum {
 SNDRV_TIMER_CLASS_NONE = -1,
 SNDRV_TIMER_CLASS_SLAVE = 0,
 SNDRV_TIMER_CLASS_GLOBAL,
 SNDRV_TIMER_CLASS_CARD,
 SNDRV_TIMER_CLASS_PCM,
 SNDRV_TIMER_CLASS_LAST = SNDRV_TIMER_CLASS_PCM,
};


enum {
 SNDRV_TIMER_SCLASS_NONE = 0,
 SNDRV_TIMER_SCLASS_APPLICATION,
 SNDRV_TIMER_SCLASS_SEQUENCER,
 SNDRV_TIMER_SCLASS_OSS_SEQUENCER,
 SNDRV_TIMER_SCLASS_LAST = SNDRV_TIMER_SCLASS_OSS_SEQUENCER,
};
# 583 "include/sound/asound.h"
struct snd_timer_id {
 int dev_class;
 int dev_sclass;
 int card;
 int device;
 int subdevice;
};

struct snd_timer_ginfo {
 struct snd_timer_id tid;
 unsigned int flags;
 int card;
 unsigned char id[64];
 unsigned char name[80];
 unsigned long reserved0;
 unsigned long resolution;
 unsigned long resolution_min;
 unsigned long resolution_max;
 unsigned int clients;
 unsigned char reserved[32];
};

struct snd_timer_gparams {
 struct snd_timer_id tid;
 unsigned long period_num;
 unsigned long period_den;
 unsigned char reserved[32];
};

struct snd_timer_gstatus {
 struct snd_timer_id tid;
 unsigned long resolution;
 unsigned long resolution_num;
 unsigned long resolution_den;
 unsigned char reserved[32];
};

struct snd_timer_select {
 struct snd_timer_id id;
 unsigned char reserved[32];
};

struct snd_timer_info {
 unsigned int flags;
 int card;
 unsigned char id[64];
 unsigned char name[80];
 unsigned long reserved0;
 unsigned long resolution;
 unsigned char reserved[64];
};





struct snd_timer_params {
 unsigned int flags;
 unsigned int ticks;
 unsigned int queue_size;
 unsigned int reserved0;
 unsigned int filter;
 unsigned char reserved[60];
};

struct snd_timer_status {
 struct timespec tstamp;
 unsigned int resolution;
 unsigned int lost;
 unsigned int overrun;
 unsigned int queue;
 unsigned char reserved[64];
};

enum {
 SNDRV_TIMER_IOCTL_PVERSION = (((2U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x00)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_NEXT_DEVICE = (((2U|1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x01)) << 0) | (((((sizeof(struct snd_timer_id) == sizeof(struct snd_timer_id[1]) && sizeof(struct snd_timer_id) < (1 << 14)) ? sizeof(struct snd_timer_id) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_TREAD = (((1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x02)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_GINFO = (((2U|1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x03)) << 0) | (((((sizeof(struct snd_timer_ginfo) == sizeof(struct snd_timer_ginfo[1]) && sizeof(struct snd_timer_ginfo) < (1 << 14)) ? sizeof(struct snd_timer_ginfo) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_GPARAMS = (((1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x04)) << 0) | (((((sizeof(struct snd_timer_gparams) == sizeof(struct snd_timer_gparams[1]) && sizeof(struct snd_timer_gparams) < (1 << 14)) ? sizeof(struct snd_timer_gparams) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_GSTATUS = (((2U|1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x05)) << 0) | (((((sizeof(struct snd_timer_gstatus) == sizeof(struct snd_timer_gstatus[1]) && sizeof(struct snd_timer_gstatus) < (1 << 14)) ? sizeof(struct snd_timer_gstatus) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_SELECT = (((1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x10)) << 0) | (((((sizeof(struct snd_timer_select) == sizeof(struct snd_timer_select[1]) && sizeof(struct snd_timer_select) < (1 << 14)) ? sizeof(struct snd_timer_select) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_INFO = (((2U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x11)) << 0) | (((((sizeof(struct snd_timer_info) == sizeof(struct snd_timer_info[1]) && sizeof(struct snd_timer_info) < (1 << 14)) ? sizeof(struct snd_timer_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_PARAMS = (((1U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x12)) << 0) | (((((sizeof(struct snd_timer_params) == sizeof(struct snd_timer_params[1]) && sizeof(struct snd_timer_params) < (1 << 14)) ? sizeof(struct snd_timer_params) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_STATUS = (((2U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0x14)) << 0) | (((((sizeof(struct snd_timer_status) == sizeof(struct snd_timer_status[1]) && sizeof(struct snd_timer_status) < (1 << 14)) ? sizeof(struct snd_timer_status) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),

 SNDRV_TIMER_IOCTL_START = (((0U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0xa0)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_STOP = (((0U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0xa1)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_CONTINUE = (((0U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0xa2)) << 0) | ((0) << ((0 +8)+8))),
 SNDRV_TIMER_IOCTL_PAUSE = (((0U) << (((0 +8)+8)+14)) | ((('T')) << (0 +8)) | (((0xa3)) << 0) | ((0) << ((0 +8)+8))),
};

struct snd_timer_read {
 unsigned int resolution;
 unsigned int ticks;
};

enum {
 SNDRV_TIMER_EVENT_RESOLUTION = 0,
 SNDRV_TIMER_EVENT_TICK,
 SNDRV_TIMER_EVENT_START,
 SNDRV_TIMER_EVENT_STOP,
 SNDRV_TIMER_EVENT_CONTINUE,
 SNDRV_TIMER_EVENT_PAUSE,
 SNDRV_TIMER_EVENT_EARLY,
 SNDRV_TIMER_EVENT_SUSPEND,
 SNDRV_TIMER_EVENT_RESUME,

 SNDRV_TIMER_EVENT_MSTART = SNDRV_TIMER_EVENT_START + 10,
 SNDRV_TIMER_EVENT_MSTOP = SNDRV_TIMER_EVENT_STOP + 10,
 SNDRV_TIMER_EVENT_MCONTINUE = SNDRV_TIMER_EVENT_CONTINUE + 10,
 SNDRV_TIMER_EVENT_MPAUSE = SNDRV_TIMER_EVENT_PAUSE + 10,
 SNDRV_TIMER_EVENT_MSUSPEND = SNDRV_TIMER_EVENT_SUSPEND + 10,
 SNDRV_TIMER_EVENT_MRESUME = SNDRV_TIMER_EVENT_RESUME + 10,
};

struct snd_timer_tread {
 int event;
 struct timespec tstamp;
 unsigned int val;
};
# 713 "include/sound/asound.h"
struct snd_ctl_card_info {
 int card;
 int pad;
 unsigned char id[16];
 unsigned char driver[16];
 unsigned char name[32];
 unsigned char longname[80];
 unsigned char reserved_[16];
 unsigned char mixername[80];
 unsigned char components[128];
};

typedef int snd_ctl_elem_type_t;
# 735 "include/sound/asound.h"
typedef int snd_ctl_elem_iface_t;
# 769 "include/sound/asound.h"
struct snd_ctl_elem_id {
 unsigned int numid;
 snd_ctl_elem_iface_t iface;
 unsigned int device;
 unsigned int subdevice;
        unsigned char name[44];
 unsigned int index;
};

struct snd_ctl_elem_list {
 unsigned int offset;
 unsigned int space;
 unsigned int used;
 unsigned int count;
 struct snd_ctl_elem_id __attribute__((noderef, address_space(1))) *pids;
 unsigned char reserved[50];
};

struct snd_ctl_elem_info {
 struct snd_ctl_elem_id id;
 snd_ctl_elem_type_t type;
 unsigned int access;
 unsigned int count;
 pid_t owner;
 union {
  struct {
   long min;
   long max;
   long step;
  } integer;
  struct {
   long long min;
   long long max;
   long long step;
  } integer64;
  struct {
   unsigned int items;
   unsigned int item;
   char name[64];
  } enumerated;
  unsigned char reserved[128];
 } value;
 union {
  unsigned short d[4];
  unsigned short *d_ptr;
 } dimen;
 unsigned char reserved[64-4*sizeof(unsigned short)];
};

struct snd_ctl_elem_value {
 struct snd_ctl_elem_id id;
 unsigned int indirect: 1;
        union {
  union {
   long value[128];
   long *value_ptr;
  } integer;
  union {
   long long value[64];
   long long *value_ptr;
  } integer64;
  union {
   unsigned int item[128];
   unsigned int *item_ptr;
  } enumerated;
  union {
   unsigned char data[512];
   unsigned char *data_ptr;
  } bytes;
  struct snd_aes_iec958 iec958;
        } value;
 struct timespec tstamp;
        unsigned char reserved[128-sizeof(struct timespec)];
};

struct snd_ctl_tlv {
        unsigned int numid;
        unsigned int length;
        unsigned int tlv[0];
};

enum {
 SNDRV_CTL_IOCTL_PVERSION = (((2U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x00)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_CARD_INFO = (((2U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x01)) << 0) | (((((sizeof(struct snd_ctl_card_info) == sizeof(struct snd_ctl_card_info[1]) && sizeof(struct snd_ctl_card_info) < (1 << 14)) ? sizeof(struct snd_ctl_card_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_LIST = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x10)) << 0) | (((((sizeof(struct snd_ctl_elem_list) == sizeof(struct snd_ctl_elem_list[1]) && sizeof(struct snd_ctl_elem_list) < (1 << 14)) ? sizeof(struct snd_ctl_elem_list) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_INFO = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x11)) << 0) | (((((sizeof(struct snd_ctl_elem_info) == sizeof(struct snd_ctl_elem_info[1]) && sizeof(struct snd_ctl_elem_info) < (1 << 14)) ? sizeof(struct snd_ctl_elem_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_READ = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x12)) << 0) | (((((sizeof(struct snd_ctl_elem_value) == sizeof(struct snd_ctl_elem_value[1]) && sizeof(struct snd_ctl_elem_value) < (1 << 14)) ? sizeof(struct snd_ctl_elem_value) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_WRITE = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x13)) << 0) | (((((sizeof(struct snd_ctl_elem_value) == sizeof(struct snd_ctl_elem_value[1]) && sizeof(struct snd_ctl_elem_value) < (1 << 14)) ? sizeof(struct snd_ctl_elem_value) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_LOCK = (((1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x14)) << 0) | (((((sizeof(struct snd_ctl_elem_id) == sizeof(struct snd_ctl_elem_id[1]) && sizeof(struct snd_ctl_elem_id) < (1 << 14)) ? sizeof(struct snd_ctl_elem_id) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_UNLOCK = (((1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x15)) << 0) | (((((sizeof(struct snd_ctl_elem_id) == sizeof(struct snd_ctl_elem_id[1]) && sizeof(struct snd_ctl_elem_id) < (1 << 14)) ? sizeof(struct snd_ctl_elem_id) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_SUBSCRIBE_EVENTS = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x16)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_ADD = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x17)) << 0) | (((((sizeof(struct snd_ctl_elem_info) == sizeof(struct snd_ctl_elem_info[1]) && sizeof(struct snd_ctl_elem_info) < (1 << 14)) ? sizeof(struct snd_ctl_elem_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_REPLACE = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x18)) << 0) | (((((sizeof(struct snd_ctl_elem_info) == sizeof(struct snd_ctl_elem_info[1]) && sizeof(struct snd_ctl_elem_info) < (1 << 14)) ? sizeof(struct snd_ctl_elem_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_ELEM_REMOVE = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x19)) << 0) | (((((sizeof(struct snd_ctl_elem_id) == sizeof(struct snd_ctl_elem_id[1]) && sizeof(struct snd_ctl_elem_id) < (1 << 14)) ? sizeof(struct snd_ctl_elem_id) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_TLV_READ = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x1a)) << 0) | (((((sizeof(struct snd_ctl_tlv) == sizeof(struct snd_ctl_tlv[1]) && sizeof(struct snd_ctl_tlv) < (1 << 14)) ? sizeof(struct snd_ctl_tlv) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_TLV_WRITE = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x1b)) << 0) | (((((sizeof(struct snd_ctl_tlv) == sizeof(struct snd_ctl_tlv[1]) && sizeof(struct snd_ctl_tlv) < (1 << 14)) ? sizeof(struct snd_ctl_tlv) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_TLV_COMMAND = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x1c)) << 0) | (((((sizeof(struct snd_ctl_tlv) == sizeof(struct snd_ctl_tlv[1]) && sizeof(struct snd_ctl_tlv) < (1 << 14)) ? sizeof(struct snd_ctl_tlv) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_HWDEP_NEXT_DEVICE = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x20)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_HWDEP_INFO = (((2U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x21)) << 0) | (((((sizeof(struct snd_hwdep_info) == sizeof(struct snd_hwdep_info[1]) && sizeof(struct snd_hwdep_info) < (1 << 14)) ? sizeof(struct snd_hwdep_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_PCM_NEXT_DEVICE = (((2U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x30)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_PCM_INFO = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x31)) << 0) | (((((sizeof(struct snd_pcm_info) == sizeof(struct snd_pcm_info[1]) && sizeof(struct snd_pcm_info) < (1 << 14)) ? sizeof(struct snd_pcm_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_PCM_PREFER_SUBDEVICE = (((1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x32)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_RAWMIDI_NEXT_DEVICE = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x40)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_RAWMIDI_INFO = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x41)) << 0) | (((((sizeof(struct snd_rawmidi_info) == sizeof(struct snd_rawmidi_info[1]) && sizeof(struct snd_rawmidi_info) < (1 << 14)) ? sizeof(struct snd_rawmidi_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_RAWMIDI_PREFER_SUBDEVICE = (((1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0x42)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_POWER = (((2U|1U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0xd0)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_CTL_IOCTL_POWER_STATE = (((2U) << (((0 +8)+8)+14)) | ((('U')) << (0 +8)) | (((0xd1)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
};





enum sndrv_ctl_event_type {
 SNDRV_CTL_EVENT_ELEM = 0,
 SNDRV_CTL_EVENT_LAST = SNDRV_CTL_EVENT_ELEM,
};







struct snd_ctl_event {
 int type;
 union {
  struct {
   unsigned int mask;
   struct snd_ctl_elem_id id;
  } elem;
                unsigned char data8[60];
        } data;
};
# 926 "include/sound/asound.h"
struct snd_xferv {
 const struct iovec *vector;
 unsigned long count;
};

enum {
 SNDRV_IOCTL_READV = (((1U) << (((0 +8)+8)+14)) | ((('K')) << (0 +8)) | (((0x00)) << 0) | (((((sizeof(struct snd_xferv) == sizeof(struct snd_xferv[1]) && sizeof(struct snd_xferv) < (1 << 14)) ? sizeof(struct snd_xferv) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
 SNDRV_IOCTL_WRITEV = (((1U) << (((0 +8)+8)+14)) | ((('K')) << (0 +8)) | (((0x01)) << 0) | (((((sizeof(struct snd_xferv) == sizeof(struct snd_xferv[1]) && sizeof(struct snd_xferv) < (1 << 14)) ? sizeof(struct snd_xferv) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))),
};
# 26 "include/sound/control.h" 2



struct snd_kcontrol;
typedef int (snd_kcontrol_info_t) (struct snd_kcontrol * kcontrol, struct snd_ctl_elem_info * uinfo);
typedef int (snd_kcontrol_get_t) (struct snd_kcontrol * kcontrol, struct snd_ctl_elem_value * ucontrol);
typedef int (snd_kcontrol_put_t) (struct snd_kcontrol * kcontrol, struct snd_ctl_elem_value * ucontrol);
typedef int (snd_kcontrol_tlv_rw_t)(struct snd_kcontrol *kcontrol,
        int op_flag,
        unsigned int size,
        unsigned int __attribute__((noderef, address_space(1))) *tlv);


struct snd_kcontrol_new {
 snd_ctl_elem_iface_t iface;
 unsigned int device;
 unsigned int subdevice;
    unsigned char *__attribute__((nullterm)) name;
 unsigned int index;
 unsigned int access;
 unsigned int count;
 snd_kcontrol_info_t *info;
 snd_kcontrol_get_t *get;
 snd_kcontrol_put_t *put;
 union {
  snd_kcontrol_tlv_rw_t *c;
  const unsigned int *p;
 } tlv;
 unsigned long private_value;
};

struct snd_kcontrol_volatile {
 struct snd_ctl_file *owner;
 pid_t owner_pid;
 unsigned int access;
};

struct snd_kcontrol {
 struct list_head list;
 struct snd_ctl_elem_id id;
 unsigned int count;
 snd_kcontrol_info_t *info;
 snd_kcontrol_get_t *get;
 snd_kcontrol_put_t *put;
 union {
  snd_kcontrol_tlv_rw_t *c;
  const unsigned int *p;
 } tlv;
 unsigned long private_value;
    struct ensoniq *__attribute__((recursive)) private_data;
 void (*private_free)(struct snd_kcontrol *kcontrol);
 struct snd_kcontrol_volatile vd[0];
};



struct snd_kctl_event {
 struct list_head list;
 struct snd_ctl_elem_id id;
 unsigned int mask;
};



struct snd_ctl_file {
 struct list_head list;
    struct snd_card *__attribute__((recursive)) card;
 pid_t pid;
 int prefer_pcm_subdevice;
 int prefer_rawmidi_subdevice;
 wait_queue_head_t change_sleep;
 spinlock_t read_lock;
 struct fasync_struct *fasync;
 int subscribed;
 struct list_head events;
};



typedef int (*snd_kctl_ioctl_func_t) (struct snd_card * card,
          struct snd_ctl_file * control,
          unsigned int cmd, unsigned long arg);

void snd_ctl_notify(struct snd_card * card, unsigned int mask, struct snd_ctl_elem_id * id);

struct snd_kcontrol *snd_ctl_new1(const struct snd_kcontrol_new * kcontrolnew, void * private_data);
void snd_ctl_free_one(struct snd_kcontrol * kcontrol);
int snd_ctl_add(struct snd_card * card, struct snd_kcontrol * kcontrol);
int snd_ctl_remove(struct snd_card * card, struct snd_kcontrol * kcontrol);
int snd_ctl_remove_id(struct snd_card * card, struct snd_ctl_elem_id *id);
int snd_ctl_rename_id(struct snd_card * card, struct snd_ctl_elem_id *src_id, struct snd_ctl_elem_id *dst_id);
struct snd_kcontrol *snd_ctl_find_numid(struct snd_card * card, unsigned int numid);
struct snd_kcontrol *snd_ctl_find_id(struct snd_card * card, struct snd_ctl_elem_id *id);

int snd_ctl_create(struct snd_card *card);

int snd_ctl_register_ioctl(snd_kctl_ioctl_func_t fcn);
int snd_ctl_unregister_ioctl(snd_kctl_ioctl_func_t fcn);

int snd_ctl_register_ioctl_compat(snd_kctl_ioctl_func_t fcn);
int snd_ctl_unregister_ioctl_compat(snd_kctl_ioctl_func_t fcn);





static inline __attribute__((always_inline)) unsigned int snd_ctl_get_ioffnum(struct snd_kcontrol *kctl, struct snd_ctl_elem_id *id)
{
 return id->numid - kctl->id.numid;
}

static inline __attribute__((always_inline)) unsigned int snd_ctl_get_ioffidx(struct snd_kcontrol *kctl, struct snd_ctl_elem_id *id)
{
 return id->index - kctl->id.index;
}

static inline __attribute__((always_inline)) unsigned int snd_ctl_get_ioff(struct snd_kcontrol *kctl, struct snd_ctl_elem_id *id)
{
 if (id->numid) {
  return snd_ctl_get_ioffnum(kctl, id);
 } else {
  return snd_ctl_get_ioffidx(kctl, id);
 }
}

static inline __attribute__((always_inline)) struct snd_ctl_elem_id *snd_ctl_build_ioff(struct snd_ctl_elem_id *dst_id,
          struct snd_kcontrol *src_kctl,
          unsigned int offset)
{
 *dst_id = src_kctl->id;
 dst_id->index += offset;
 dst_id->numid += offset;
 return dst_id;
}




int snd_ctl_boolean_mono_info(struct snd_kcontrol *kcontrol,
         struct snd_ctl_elem_info *uinfo);
int snd_ctl_boolean_stereo_info(struct snd_kcontrol *kcontrol,
    struct snd_ctl_elem_info *uinfo);




struct snd_kcontrol *snd_ctl_make_virtual_master(char *name,
       const unsigned int *tlv);
int snd_ctl_add_slave(struct snd_kcontrol *master, struct snd_kcontrol *slave);
# 42 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 1 "include/sound/pcm.h" 1
# 27 "include/sound/pcm.h"
# 1 "include/sound/memalloc.h" 1
# 27 "include/sound/memalloc.h"
struct device;




struct snd_dma_device {
 int type;
 struct device *dev;
};
# 55 "include/sound/memalloc.h"
struct snd_dma_buffer {
 struct snd_dma_device dev;
 unsigned char *area;
 dma_addr_t addr;
 size_t bytes;
    void *__attribute__((recursive)) private_data;
};




void *snd_malloc_sgbuf_pages(struct device *device,
        size_t size, struct snd_dma_buffer *dmab,
        size_t *res_size);
int snd_free_sgbuf_pages(struct snd_dma_buffer *dmab);

struct snd_sg_page {
 void *buf;
 dma_addr_t addr;
};

struct snd_sg_buf {
 int size;
 int pages;
 int tblsize;
 struct snd_sg_page *table;
 struct page **page_table;
 struct device *dev;
};




static inline __attribute__((always_inline)) unsigned int snd_sgbuf_aligned_pages(size_t size)
{
 return (size + ((1UL) << 12) - 1) >> 12;
}




static inline __attribute__((always_inline)) dma_addr_t snd_sgbuf_get_addr(struct snd_sg_buf *sgbuf, size_t offset)
{
 dma_addr_t addr = sgbuf->table[offset >> 12].addr;
 addr &= (~(((1UL) << 12)-1));
 return addr + offset % ((1UL) << 12);
}




static inline __attribute__((always_inline)) void *snd_sgbuf_get_ptr(struct snd_sg_buf *sgbuf, size_t offset)
{
 return sgbuf->table[offset >> 12].buf + offset % ((1UL) << 12);
}


int snd_dma_alloc_pages(int type, struct device *dev, size_t size,
   struct snd_dma_buffer *dmab);
int snd_dma_alloc_pages_fallback(int type, struct device *dev, size_t size,
                                 struct snd_dma_buffer *dmab);
void snd_dma_free_pages(struct snd_dma_buffer *dmab);





size_t snd_dma_get_reserved_buf(struct snd_dma_buffer *dmab, unsigned int id);
int snd_dma_reserve_buf(struct snd_dma_buffer *dmab, unsigned int id);


void *snd_malloc_pages(size_t size, gfp_t gfp_flags);
void snd_free_pages(void *ptr, size_t size);
# 28 "include/sound/pcm.h" 2
# 1 "include/sound/minors.h" 1
# 55 "include/sound/minors.h"
enum {
 SNDRV_DEVICE_TYPE_CONTROL,
 SNDRV_DEVICE_TYPE_SEQUENCER,
 SNDRV_DEVICE_TYPE_TIMER,
 SNDRV_DEVICE_TYPE_HWDEP,
 SNDRV_DEVICE_TYPE_RAWMIDI,
 SNDRV_DEVICE_TYPE_PCM_PLAYBACK,
 SNDRV_DEVICE_TYPE_PCM_CAPTURE,
};
# 29 "include/sound/pcm.h" 2
# 1 "include/linux/poll.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/poll.h" 1
# 1 "include/asm-generic/poll.h" 1
# 31 "include/asm-generic/poll.h"
struct pollfd {
 int fd;
 short events;
 short revents;
};
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/poll.h" 2
# 5 "include/linux/poll.h" 2






# 1 "include/linux/fs.h" 1
# 9 "include/linux/fs.h"
# 1 "include/linux/limits.h" 1
# 10 "include/linux/fs.h" 2
# 35 "include/linux/fs.h"
struct files_stat_struct {
 int nr_files;
 int nr_free_files;
 int max_files;
};

struct inodes_stat_t {
 int nr_inodes;
 int nr_unused;
 int dummy[5];
};
# 297 "include/linux/fs.h"
# 1 "include/linux/kdev_t.h" 1
# 21 "include/linux/kdev_t.h"
static inline __attribute__((always_inline)) int old_valid_dev(dev_t dev)
{
 return ((unsigned int) ((dev) >> 20)) < 256 && ((unsigned int) ((dev) & ((1U << 20) - 1))) < 256;
}

static inline __attribute__((always_inline)) u16 old_encode_dev(dev_t dev)
{
 return (((unsigned int) ((dev) >> 20)) << 8) | ((unsigned int) ((dev) & ((1U << 20) - 1)));
}

static inline __attribute__((always_inline)) dev_t old_decode_dev(u16 val)
{
 return ((((val >> 8) & 255) << 20) | (val & 255));
}

static inline __attribute__((always_inline)) int new_valid_dev(dev_t dev)
{
 return 1;
}

static inline __attribute__((always_inline)) u32 new_encode_dev(dev_t dev)
{
 unsigned major = ((unsigned int) ((dev) >> 20));
 unsigned minor = ((unsigned int) ((dev) & ((1U << 20) - 1)));
 return (minor & 0xff) | (major << 8) | ((minor & ~0xff) << 12);
}

static inline __attribute__((always_inline)) dev_t new_decode_dev(u32 dev)
{
 unsigned major = (dev & 0xfff00) >> 8;
 unsigned minor = (dev & 0xff) | ((dev >> 12) & 0xfff00);
 return (((major) << 20) | (minor));
}

static inline __attribute__((always_inline)) int huge_valid_dev(dev_t dev)
{
 return 1;
}

static inline __attribute__((always_inline)) u64 huge_encode_dev(dev_t dev)
{
 return new_encode_dev(dev);
}

static inline __attribute__((always_inline)) dev_t huge_decode_dev(u64 dev)
{
 return new_decode_dev(dev);
}

static inline __attribute__((always_inline)) int sysv_valid_dev(dev_t dev)
{
 return ((unsigned int) ((dev) >> 20)) < (1<<14) && ((unsigned int) ((dev) & ((1U << 20) - 1))) < (1<<18);
}

static inline __attribute__((always_inline)) u32 sysv_encode_dev(dev_t dev)
{
 return ((unsigned int) ((dev) & ((1U << 20) - 1))) | (((unsigned int) ((dev) >> 20)) << 18);
}

static inline __attribute__((always_inline)) unsigned sysv_major(u32 dev)
{
 return (dev >> 18) & 0x3fff;
}

static inline __attribute__((always_inline)) unsigned sysv_minor(u32 dev)
{
 return dev & 0x3ffff;
}
# 298 "include/linux/fs.h" 2
# 1 "include/linux/dcache.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 5 "include/linux/dcache.h" 2

# 1 "include/linux/rculist.h" 1
# 18 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void __list_add_rcu(struct list_head *new,
  struct list_head *prev, struct list_head *next)
{
 new->next = next;
 new->prev = prev;
 ({ if (!__builtin_constant_p(new) || ((new) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (prev->next) = (new); });
 next->prev = new;
}
# 43 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void list_add_rcu(struct list_head *new, struct list_head *head)
{
 __list_add_rcu(new, head, head->next);
}
# 64 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void list_add_tail_rcu(struct list_head *new,
     struct list_head *head)
{
 __list_add_rcu(new, head->prev, head);
}
# 94 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void list_del_rcu(struct list_head *entry)
{
 __list_del(entry->prev, entry->next);
 entry->prev = ((void *) 0x00200200);
}
# 120 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void hlist_del_init_rcu(struct hlist_node *n)
{
 if (!hlist_unhashed(n)) {
  __hlist_del(n);
  n->pprev = ((void *)0);
 }
}
# 136 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void list_replace_rcu(struct list_head *old,
    struct list_head *new)
{
 new->next = old->next;
 new->prev = old->prev;
 ({ if (!__builtin_constant_p(new) || ((new) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (new->prev->next) = (new); });
 new->next->prev = new;
 old->prev = ((void *) 0x00200200);
}
# 163 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void list_splice_init_rcu(struct list_head *list,
     struct list_head *head,
     void (*sync)(void))
{
 struct list_head *first = list->next;
 struct list_head *last = list->prev;
 struct list_head *at = head->next;

 if (list_empty(head))
  return;



 INIT_LIST_HEAD(list);
# 185 "include/linux/rculist.h"
 sync();
# 195 "include/linux/rculist.h"
 last->next = at;
 ({ if (!__builtin_constant_p(first) || ((first) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (head->next) = (first); });
 first->prev = head;
 at->prev = last;
}
# 257 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void hlist_del_rcu(struct hlist_node *n)
{
 __hlist_del(n);
 n->pprev = ((void *) 0x00200200);
}
# 270 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void hlist_replace_rcu(struct hlist_node *old,
     struct hlist_node *new)
{
 struct hlist_node *next = old->next;

 new->next = next;
 new->pprev = old->pprev;
 ({ if (!__builtin_constant_p(new) || ((new) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (*new->pprev) = (new); });
 if (next)
  new->next->pprev = &new->next;
 old->pprev = ((void *) 0x00200200);
}
# 302 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void hlist_add_head_rcu(struct hlist_node *n,
     struct hlist_head *h)
{
 struct hlist_node *first = h->first;

 n->next = first;
 n->pprev = &h->first;
 ({ if (!__builtin_constant_p(n) || ((n) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (h->first) = (n); });
 if (first)
  first->pprev = &n->next;
}
# 332 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void hlist_add_before_rcu(struct hlist_node *n,
     struct hlist_node *next)
{
 n->pprev = next->pprev;
 n->next = next;
 ({ if (!__builtin_constant_p(n) || ((n) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (*(n->pprev)) = (n); });
 next->pprev = &n->next;
}
# 359 "include/linux/rculist.h"
static inline __attribute__((always_inline)) void hlist_add_after_rcu(struct hlist_node *prev,
           struct hlist_node *n)
{
 n->next = prev->next;
 n->pprev = &prev->next;
 ({ if (!__builtin_constant_p(n) || ((n) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (prev->next) = (n); });
 if (n->next)
  n->next->pprev = &n->next;
}
# 7 "include/linux/dcache.h" 2




struct nameidata;
struct path;
struct vfsmount;
# 33 "include/linux/dcache.h"
struct qstr {
 unsigned int hash;
 unsigned int len;
 const unsigned char *name;
};

struct dentry_stat_t {
 int nr_dentry;
 int nr_unused;
 int age_limit;
 int want_pages;
 int dummy[2];
};
extern struct dentry_stat_t dentry_stat;






static inline __attribute__((always_inline)) unsigned long
partial_name_hash(unsigned long c, unsigned long prevhash)
{
 return (prevhash + (c << 4) + (c >> 4)) * 11;
}





static inline __attribute__((always_inline)) unsigned long end_name_hash(unsigned long hash)
{
 return (unsigned int) hash;
}


static inline __attribute__((always_inline)) unsigned int
full_name_hash(const unsigned char *name, unsigned int len)
{
 unsigned long hash = 0;
 while (len--)
  hash = partial_name_hash(*name++, hash);
 return end_name_hash(hash);
}
# 89 "include/linux/dcache.h"
struct dentry {
 atomic_t d_count;
 unsigned int d_flags;
 spinlock_t d_lock;
 int d_mounted;
 struct inode *d_inode;





 struct hlist_node d_hash;
 struct dentry *d_parent;
 struct qstr d_name;

 struct list_head d_lru;



 union {
  struct list_head d_child;
   struct rcu_head d_rcu;
 } d_u;
 struct list_head d_subdirs;
 struct list_head d_alias;
 unsigned long d_time;
 struct dentry_operations *d_op;
 struct super_block *d_sb;
 void *d_fsdata;

 unsigned char d_iname[32];
};







enum dentry_d_lock_class
{
 DENTRY_D_LOCK_NORMAL,
 DENTRY_D_LOCK_NESTED
};

struct dentry_operations {
 int (*d_revalidate)(struct dentry *, struct nameidata *);
 int (*d_hash) (struct dentry *, struct qstr *);
 int (*d_compare) (struct dentry *, struct qstr *, struct qstr *);
 int (*d_delete)(struct dentry *);
 void (*d_release)(struct dentry *);
 void (*d_iput)(struct dentry *, struct inode *);
 char *(*d_dname)(struct dentry *, char *, int);
};
# 187 "include/linux/dcache.h"
extern spinlock_t dcache_lock;
extern seqlock_t rename_lock;
# 206 "include/linux/dcache.h"
static inline __attribute__((always_inline)) void __d_drop(struct dentry *dentry)
{
 if (!(dentry->d_flags & 0x0010)) {
  dentry->d_flags |= 0x0010;
  hlist_del_rcu(&dentry->d_hash);
 }
}

static inline __attribute__((always_inline)) void d_drop(struct dentry *dentry)
{
 _spin_lock(&dcache_lock);
 _spin_lock(&dentry->d_lock);
  __d_drop(dentry);
 _spin_unlock(&dentry->d_lock);
 _spin_unlock(&dcache_lock);
}

static inline __attribute__((always_inline)) int dname_external(struct dentry *dentry)
{
 return dentry->d_name.name != dentry->d_iname;
}




extern void d_instantiate(struct dentry *, struct inode *);
extern struct dentry * d_instantiate_unique(struct dentry *, struct inode *);
extern struct dentry * d_materialise_unique(struct dentry *, struct inode *);
extern void d_delete(struct dentry *);


extern struct dentry * d_alloc(struct dentry *, const struct qstr *);
extern struct dentry * d_splice_alias(struct inode *, struct dentry *);
extern struct dentry * d_add_ci(struct dentry *, struct inode *, struct qstr *);
extern struct dentry * d_obtain_alias(struct inode *);
extern void shrink_dcache_sb(struct super_block *);
extern void shrink_dcache_parent(struct dentry *);
extern void shrink_dcache_for_umount(struct super_block *);
extern int d_invalidate(struct dentry *);


extern struct dentry * d_alloc_root(struct inode *);


extern void d_genocide(struct dentry *);

extern struct dentry *d_find_alias(struct inode *);
extern void d_prune_aliases(struct inode *);


extern int have_submounts(struct dentry *);




extern void d_rehash(struct dentry *);
# 272 "include/linux/dcache.h"
static inline __attribute__((always_inline)) void d_add(struct dentry *entry, struct inode *inode)
{
 d_instantiate(entry, inode);
 d_rehash(entry);
}
# 286 "include/linux/dcache.h"
static inline __attribute__((always_inline)) struct dentry *d_add_unique(struct dentry *entry, struct inode *inode)
{
 struct dentry *res;

 res = d_instantiate_unique(entry, inode);
 d_rehash(res != ((void *)0) ? res : entry);
 return res;
}


extern void d_move(struct dentry *, struct dentry *);
extern struct dentry *d_ancestor(struct dentry *, struct dentry *);


extern struct dentry * d_lookup(struct dentry *, struct qstr *);
extern struct dentry * __d_lookup(struct dentry *, struct qstr *);
extern struct dentry * d_hash_and_lookup(struct dentry *, struct qstr *);


extern int d_validate(struct dentry *, struct dentry *);




extern char *dynamic_dname(struct dentry *, char *, int, const char *, ...);

extern char *__d_path(const struct path *path, struct path *root, char *, int);
extern char *d_path(const struct path *, char *, int);
extern char *dentry_path(struct dentry *, char *, int);
# 331 "include/linux/dcache.h"
static inline __attribute__((always_inline)) struct dentry *dget(struct dentry *dentry)
{
 if (dentry) {
  do { if (__builtin_expect(!!(!((&dentry->d_count)->counter)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/dcache.h"), "i" (334), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
  atomic_inc(&dentry->d_count);
 }
 return dentry;
}

extern struct dentry * dget_locked(struct dentry *);
# 349 "include/linux/dcache.h"
static inline __attribute__((always_inline)) int d_unhashed(struct dentry *dentry)
{
 return (dentry->d_flags & 0x0010);
}

static inline __attribute__((always_inline)) struct dentry *dget_parent(struct dentry *dentry)
{
 struct dentry *ret;

 _spin_lock(&dentry->d_lock);
 ret = dget(dentry->d_parent);
 _spin_unlock(&dentry->d_lock);
 return ret;
}

extern void dput(struct dentry *);

static inline __attribute__((always_inline)) int d_mountpoint(struct dentry *dentry)
{
 return dentry->d_mounted;
}

extern struct vfsmount *lookup_mnt(struct vfsmount *, struct dentry *);
extern struct dentry *lookup_create(struct nameidata *nd, int is_dir);

extern int sysctl_vfs_cache_pressure;
# 299 "include/linux/fs.h" 2





# 1 "include/linux/radix-tree.h" 1
# 41 "include/linux/radix-tree.h"
static inline __attribute__((always_inline)) void *radix_tree_ptr_to_indirect(void *ptr)
{
 return (void *)((unsigned long)ptr | 1);
}

static inline __attribute__((always_inline)) void *radix_tree_indirect_to_ptr(void *ptr)
{
 return (void *)((unsigned long)ptr & ~1);
}

static inline __attribute__((always_inline)) int radix_tree_is_indirect_ptr(void *ptr)
{
 return (int)((unsigned long)ptr & 1);
}






struct radix_tree_root {
 unsigned int height;
 gfp_t gfp_mask;
 struct radix_tree_node *rnode;
};
# 137 "include/linux/radix-tree.h"
static inline __attribute__((always_inline)) void *radix_tree_deref_slot(void **pslot)
{
 void *ret = ({ typeof(*pslot) _________p1 = (*(volatile typeof(*pslot) *)&(*pslot)); do { } while (0); (_________p1); });
 if (__builtin_expect(!!(radix_tree_is_indirect_ptr(ret)), 0))
  ret = ((void *)-1UL);
 return ret;
}
# 152 "include/linux/radix-tree.h"
static inline __attribute__((always_inline)) void radix_tree_replace_slot(void **pslot, void *item)
{
 do { if (__builtin_expect(!!(radix_tree_is_indirect_ptr(item)), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/radix-tree.h"), "i" (154), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);
 ({ if (!__builtin_constant_p(item) || ((item) != ((void *)0))) __asm__ __volatile__("": : :"memory"); (*pslot) = (item); });
}

int radix_tree_insert(struct radix_tree_root *, unsigned long, void *);
void *radix_tree_lookup(struct radix_tree_root *, unsigned long);
void **radix_tree_lookup_slot(struct radix_tree_root *, unsigned long);
void *radix_tree_delete(struct radix_tree_root *, unsigned long);
unsigned int
radix_tree_gang_lookup(struct radix_tree_root *root, void **results,
   unsigned long first_index, unsigned int max_items);
unsigned int
radix_tree_gang_lookup_slot(struct radix_tree_root *root, void ***results,
   unsigned long first_index, unsigned int max_items);
unsigned long radix_tree_next_hole(struct radix_tree_root *root,
    unsigned long index, unsigned long max_scan);
int radix_tree_preload(gfp_t gfp_mask);
void radix_tree_init(void);
void *radix_tree_tag_set(struct radix_tree_root *root,
   unsigned long index, unsigned int tag);
void *radix_tree_tag_clear(struct radix_tree_root *root,
   unsigned long index, unsigned int tag);
int radix_tree_tag_get(struct radix_tree_root *root,
   unsigned long index, unsigned int tag);
unsigned int
radix_tree_gang_lookup_tag(struct radix_tree_root *root, void **results,
  unsigned long first_index, unsigned int max_items,
  unsigned int tag);
unsigned int
radix_tree_gang_lookup_tag_slot(struct radix_tree_root *root, void ***results,
  unsigned long first_index, unsigned int max_items,
  unsigned int tag);
int radix_tree_tagged(struct radix_tree_root *root, unsigned int tag);

static inline __attribute__((always_inline)) void radix_tree_preload_end(void)
{
 do { } while (0);
}
# 305 "include/linux/fs.h" 2






# 1 "include/linux/fiemap.h" 1
# 14 "include/linux/fiemap.h"
struct fiemap_extent {
 __u64 fe_logical;

 __u64 fe_physical;

 __u64 fe_length;
 __u64 fe_reserved64[2];
 __u32 fe_flags;
 __u32 fe_reserved[3];
};

struct fiemap {
 __u64 fm_start;

 __u64 fm_length;

 __u32 fm_flags;
 __u32 fm_mapped_extents;
 __u32 fm_extent_count;
 __u32 fm_reserved;
 struct fiemap_extent fm_extents[0];
};
# 312 "include/linux/fs.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 314 "include/linux/fs.h" 2


struct export_operations;
struct hd_geometry;
struct iovec;
struct nameidata;
struct kiocb;
struct pipe_inode_info;
struct poll_table_struct;
struct kstatfs;
struct vm_area_struct;
struct vfsmount;
struct cred;

extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) inode_init(void);
extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) inode_init_early(void);
extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) files_init(unsigned long);

extern struct files_stat_struct files_stat;
extern int get_max_files(void);
extern int sysctl_nr_open;
extern struct inodes_stat_t inodes_stat;
extern int leases_enable, lease_break_time;

extern int dir_notify_enable;


struct buffer_head;
typedef int (get_block_t)(struct inode *inode, sector_t iblock,
   struct buffer_head *bh_result, int create);
typedef void (dio_iodone_t)(struct kiocb *iocb, loff_t offset,
   ssize_t bytes, void *private);
# 378 "include/linux/fs.h"
struct iattr {
 unsigned int ia_valid;
 umode_t ia_mode;
 uid_t ia_uid;
 gid_t ia_gid;
 loff_t ia_size;
 struct timespec ia_atime;
 struct timespec ia_mtime;
 struct timespec ia_ctime;






 struct file *ia_file;
};




# 1 "include/linux/quota.h" 1
# 86 "include/linux/quota.h"
enum {
 QIF_BLIMITS_B = 0,
 QIF_SPACE_B,
 QIF_ILIMITS_B,
 QIF_INODES_B,
 QIF_BTIME_B,
 QIF_ITIME_B,
};
# 106 "include/linux/quota.h"
struct if_dqblk {
 __u64 dqb_bhardlimit;
 __u64 dqb_bsoftlimit;
 __u64 dqb_curspace;
 __u64 dqb_ihardlimit;
 __u64 dqb_isoftlimit;
 __u64 dqb_curinodes;
 __u64 dqb_btime;
 __u64 dqb_itime;
 __u32 dqb_valid;
};
# 127 "include/linux/quota.h"
struct if_dqinfo {
 __u64 dqi_bgrace;
 __u64 dqi_igrace;
 __u32 dqi_flags;
 __u32 dqi_valid;
};
# 149 "include/linux/quota.h"
enum {
 QUOTA_NL_C_UNSPEC,
 QUOTA_NL_C_WARNING,
 __QUOTA_NL_C_MAX,
};


enum {
 QUOTA_NL_A_UNSPEC,
 QUOTA_NL_A_QTYPE,
 QUOTA_NL_A_EXCESS_ID,
 QUOTA_NL_A_WARNING,
 QUOTA_NL_A_DEV_MAJOR,
 QUOTA_NL_A_DEV_MINOR,
 QUOTA_NL_A_CAUSED_ID,
 __QUOTA_NL_A_MAX,
};
# 176 "include/linux/quota.h"
# 1 "include/linux/dqblk_xfs.h" 1
# 50 "include/linux/dqblk_xfs.h"
typedef struct fs_disk_quota {
 __s8 d_version;
 __s8 d_flags;
 __u16 d_fieldmask;
 __u32 d_id;
 __u64 d_blk_hardlimit;
 __u64 d_blk_softlimit;
 __u64 d_ino_hardlimit;
 __u64 d_ino_softlimit;
 __u64 d_bcount;
 __u64 d_icount;
 __s32 d_itimer;

 __s32 d_btimer;
 __u16 d_iwarns;
 __u16 d_bwarns;
 __s32 d_padding2;
 __u64 d_rtb_hardlimit;
 __u64 d_rtb_softlimit;
 __u64 d_rtbcount;
 __s32 d_rtbtimer;
 __u16 d_rtbwarns;
 __s16 d_padding3;
 char d_padding4[8];
} fs_disk_quota_t;
# 137 "include/linux/dqblk_xfs.h"
typedef struct fs_qfilestat {
 __u64 qfs_ino;
 __u64 qfs_nblks;
 __u32 qfs_nextents;
} fs_qfilestat_t;

typedef struct fs_quota_stat {
 __s8 qs_version;
 __u16 qs_flags;
 __s8 qs_pad;
 fs_qfilestat_t qs_uquota;
 fs_qfilestat_t qs_gquota;
 __u32 qs_incoredqs;
 __s32 qs_btimelimit;
 __s32 qs_itimelimit;
 __s32 qs_rtbtimelimit;
 __u16 qs_bwarnlimit;
 __u16 qs_iwarnlimit;
} fs_quota_stat_t;
# 177 "include/linux/quota.h" 2
# 1 "include/linux/dqblk_v1.h" 1
# 178 "include/linux/quota.h" 2
# 1 "include/linux/dqblk_v2.h" 1







# 1 "include/linux/dqblk_qtree.h" 1
# 17 "include/linux/dqblk_qtree.h"
struct dquot;


struct qtree_fmt_operations {
 void (*mem2disk_dqblk)(void *disk, struct dquot *dquot);
 void (*disk2mem_dqblk)(struct dquot *dquot, void *disk);
 int (*is_id)(void *disk, struct dquot *dquot);
};


struct qtree_mem_dqinfo {
 struct super_block *dqi_sb;
 int dqi_type;
 unsigned int dqi_blocks;
 unsigned int dqi_free_blk;
 unsigned int dqi_free_entry;
 unsigned int dqi_blocksize_bits;
 unsigned int dqi_entry_size;
 unsigned int dqi_usable_bs;
 unsigned int dqi_qtree_depth;
 struct qtree_fmt_operations *dqi_ops;
};

int qtree_write_dquot(struct qtree_mem_dqinfo *info, struct dquot *dquot);
int qtree_read_dquot(struct qtree_mem_dqinfo *info, struct dquot *dquot);
int qtree_delete_dquot(struct qtree_mem_dqinfo *info, struct dquot *dquot);
int qtree_release_dquot(struct qtree_mem_dqinfo *info, struct dquot *dquot);
int qtree_entry_unused(struct qtree_mem_dqinfo *info, char *disk);
static inline __attribute__((always_inline)) int qtree_depth(struct qtree_mem_dqinfo *info)
{
 unsigned int epb = info->dqi_usable_bs >> 2;
 unsigned long long entries = epb;
 int i;

 for (i = 1; entries < (1ULL << 32); i++)
  entries *= epb;
 return i;
}
# 9 "include/linux/dqblk_v2.h" 2
# 179 "include/linux/quota.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 181 "include/linux/quota.h" 2

typedef __kernel_uid32_t qid_t;
typedef long long qsize_t;

extern spinlock_t dq_data_lock;
# 197 "include/linux/quota.h"
struct mem_dqblk {
 qsize_t dqb_bhardlimit;
 qsize_t dqb_bsoftlimit;
 qsize_t dqb_curspace;
 qsize_t dqb_ihardlimit;
 qsize_t dqb_isoftlimit;
 qsize_t dqb_curinodes;
 time_t dqb_btime;
 time_t dqb_itime;
};




struct quota_format_type;

struct mem_dqinfo {
 struct quota_format_type *dqi_format;
 int dqi_fmt_id;

 struct list_head dqi_dirty_list;
 unsigned long dqi_flags;
 unsigned int dqi_bgrace;
 unsigned int dqi_igrace;
 qsize_t dqi_maxblimit;
 qsize_t dqi_maxilimit;
 void *dqi_priv;
};

struct super_block;





extern void mark_info_dirty(struct super_block *sb, int type);
static inline __attribute__((always_inline)) int info_dirty(struct mem_dqinfo *info)
{
 return (__builtin_constant_p((16)) ? constant_test_bit((16), (&info->dqi_flags)) : variable_test_bit((16), (&info->dqi_flags)));
}

struct dqstats {
 int lookups;
 int drops;
 int reads;
 int writes;
 int cache_hits;
 int allocated_dquots;
 int free_dquots;
 int syncs;
};

extern struct dqstats dqstats;
# 263 "include/linux/quota.h"
struct dquot {
 struct hlist_node dq_hash;
 struct list_head dq_inuse;
 struct list_head dq_free;
 struct list_head dq_dirty;
 struct mutex dq_lock;
 atomic_t dq_count;
 wait_queue_head_t dq_wait_unused;
 struct super_block *dq_sb;
 unsigned int dq_id;
 loff_t dq_off;
 unsigned long dq_flags;
 short dq_type;
 struct mem_dqblk dq_dqb;
};







struct quota_format_ops {
 int (*check_quota_file)(struct super_block *sb, int type);
 int (*read_file_info)(struct super_block *sb, int type);
 int (*write_file_info)(struct super_block *sb, int type);
 int (*free_file_info)(struct super_block *sb, int type);
 int (*read_dqblk)(struct dquot *dquot);
 int (*commit_dqblk)(struct dquot *dquot);
 int (*release_dqblk)(struct dquot *dquot);
};


struct dquot_operations {
 int (*initialize) (struct inode *, int);
 int (*drop) (struct inode *);
 int (*alloc_space) (struct inode *, qsize_t, int);
 int (*alloc_inode) (const struct inode *, qsize_t);
 int (*free_space) (struct inode *, qsize_t);
 int (*free_inode) (const struct inode *, qsize_t);
 int (*transfer) (struct inode *, struct iattr *);
 int (*write_dquot) (struct dquot *);
 struct dquot *(*alloc_dquot)(struct super_block *, int);
 void (*destroy_dquot)(struct dquot *);
 int (*acquire_dquot) (struct dquot *);
 int (*release_dquot) (struct dquot *);
 int (*mark_dirty) (struct dquot *);
 int (*write_info) (struct super_block *, int);
};


struct quotactl_ops {
 int (*quota_on)(struct super_block *, int, int, char *, int);
 int (*quota_off)(struct super_block *, int, int);
 int (*quota_sync)(struct super_block *, int);
 int (*get_info)(struct super_block *, int, struct if_dqinfo *);
 int (*set_info)(struct super_block *, int, struct if_dqinfo *);
 int (*get_dqblk)(struct super_block *, int, qid_t, struct if_dqblk *);
 int (*set_dqblk)(struct super_block *, int, qid_t, struct if_dqblk *);
 int (*get_xstate)(struct super_block *, struct fs_quota_stat *);
 int (*set_xstate)(struct super_block *, unsigned int, int);
 int (*get_xquota)(struct super_block *, int, qid_t, struct fs_disk_quota *);
 int (*set_xquota)(struct super_block *, int, qid_t, struct fs_disk_quota *);
};

struct quota_format_type {
 int qf_fmt_id;
 struct quota_format_ops *qf_ops;
 struct module *qf_owner;
 struct quota_format_type *qf_next;
};


enum {
 _DQUOT_USAGE_ENABLED = 0,
 _DQUOT_LIMITS_ENABLED,
 _DQUOT_SUSPENDED,


 _DQUOT_STATE_FLAGS
};
# 358 "include/linux/quota.h"
static inline __attribute__((always_inline)) unsigned int dquot_state_flag(unsigned int flags, int type)
{
 if (type == 0)
  return flags;
 return flags << _DQUOT_STATE_FLAGS;
}

static inline __attribute__((always_inline)) unsigned int dquot_generic_flag(unsigned int flags, int type)
{
 if (type == 0)
  return flags;
 return flags >> _DQUOT_STATE_FLAGS;
}

struct quota_info {
 unsigned int flags;
 struct mutex dqio_mutex;
 struct mutex dqonoff_mutex;
 struct rw_semaphore dqptr_sem;
 struct inode *files[2];
 struct mem_dqinfo info[2];
 struct quota_format_ops *ops[2];
};

int register_quota_format(struct quota_format_type *fmt);
void unregister_quota_format(struct quota_format_type *fmt);

struct quota_module_name {
 int qm_fmt_id;
 char *qm_mod_name;
};
# 400 "include/linux/fs.h" 2
# 427 "include/linux/fs.h"
enum positive_aop_returns {
 AOP_WRITEPAGE_ACTIVATE = 0x80000,
 AOP_TRUNCATED_PAGE = 0x80001,
};
# 441 "include/linux/fs.h"
struct page;
struct address_space;
struct writeback_control;

struct iov_iter {
 const struct iovec *iov;
 unsigned long nr_segs;
 size_t iov_offset;
 size_t count;
};

size_t iov_iter_copy_from_user_atomic(struct page *page,
  struct iov_iter *i, unsigned long offset, size_t bytes);
size_t iov_iter_copy_from_user(struct page *page,
  struct iov_iter *i, unsigned long offset, size_t bytes);
void iov_iter_advance(struct iov_iter *i, size_t bytes);
int iov_iter_fault_in_readable(struct iov_iter *i, size_t bytes);
size_t iov_iter_single_seg_count(struct iov_iter *i);

static inline __attribute__((always_inline)) void iov_iter_init(struct iov_iter *i,
   const struct iovec *iov, unsigned long nr_segs,
   size_t count, size_t written)
{
 i->iov = iov;
 i->nr_segs = nr_segs;
 i->iov_offset = 0;
 i->count = count + written;

 iov_iter_advance(i, written);
}

static inline __attribute__((always_inline)) size_t iov_iter_count(struct iov_iter *i)
{
 return i->count;
}
# 486 "include/linux/fs.h"
typedef struct {
 size_t written;
 size_t count;
 union {
  char __attribute__((noderef, address_space(1))) *buf;
  void *data;
 } arg;
 int error;
} read_descriptor_t;

typedef int (*read_actor_t)(read_descriptor_t *, struct page *,
  unsigned long, unsigned long);

struct address_space_operations {
 int (*writepage)(struct page *page, struct writeback_control *wbc);
 int (*readpage)(struct file *, struct page *);
 void (*sync_page)(struct page *);


 int (*writepages)(struct address_space *, struct writeback_control *);


 int (*set_page_dirty)(struct page *page);

 int (*readpages)(struct file *filp, struct address_space *mapping,
   struct list_head *pages, unsigned nr_pages);

 int (*write_begin)(struct file *, struct address_space *mapping,
    loff_t pos, unsigned len, unsigned flags,
    struct page **pagep, void **fsdata);
 int (*write_end)(struct file *, struct address_space *mapping,
    loff_t pos, unsigned len, unsigned copied,
    struct page *page, void *fsdata);


 sector_t (*bmap)(struct address_space *, sector_t);
 void (*invalidatepage) (struct page *, unsigned long);
 int (*releasepage) (struct page *, gfp_t);
 ssize_t (*direct_IO)(int, struct kiocb *, const struct iovec *iov,
   loff_t offset, unsigned long nr_segs);
 int (*get_xip_mem)(struct address_space *, unsigned long, int,
      void **, unsigned long *);

 int (*migratepage) (struct address_space *,
   struct page *, struct page *);
 int (*launder_page) (struct page *);
 int (*is_partially_uptodate) (struct page *, read_descriptor_t *,
     unsigned long);
};





int pagecache_write_begin(struct file *, struct address_space *mapping,
    loff_t pos, unsigned len, unsigned flags,
    struct page **pagep, void **fsdata);

int pagecache_write_end(struct file *, struct address_space *mapping,
    loff_t pos, unsigned len, unsigned copied,
    struct page *page, void *fsdata);

struct backing_dev_info;
struct address_space {
 struct inode *host;
 struct radix_tree_root page_tree;
 spinlock_t tree_lock;
 unsigned int i_mmap_writable;
 struct prio_tree_root i_mmap;
 struct list_head i_mmap_nonlinear;
 spinlock_t i_mmap_lock;
 unsigned int truncate_count;
 unsigned long nrpages;
 unsigned long writeback_index;
 const struct address_space_operations *a_ops;
 unsigned long flags;
 struct backing_dev_info *backing_dev_info;
 spinlock_t private_lock;
 struct list_head private_list;
 struct address_space *assoc_mapping;
} __attribute__((aligned(sizeof(long))));






struct block_device {
 dev_t bd_dev;
 struct inode * bd_inode;
 struct super_block * bd_super;
 int bd_openers;
 struct mutex bd_mutex;
 struct semaphore bd_mount_sem;
 struct list_head bd_inodes;
 void * bd_holder;
 int bd_holders;

 struct list_head bd_holder_list;

 struct block_device * bd_contains;
 unsigned bd_block_size;
 struct hd_struct * bd_part;

 unsigned bd_part_count;
 int bd_invalidated;
 struct gendisk * bd_disk;
 struct list_head bd_list;
 struct backing_dev_info *bd_inode_backing_dev_info;






 unsigned long bd_private;


 int bd_fsfreeze_count;

 struct mutex bd_fsfreeze_mutex;
};
# 616 "include/linux/fs.h"
int mapping_tagged(struct address_space *mapping, int tag);




static inline __attribute__((always_inline)) int mapping_mapped(struct address_space *mapping)
{
 return !prio_tree_empty(&mapping->i_mmap) ||
  !list_empty(&mapping->i_mmap_nonlinear);
}







static inline __attribute__((always_inline)) int mapping_writably_mapped(struct address_space *mapping)
{
 return mapping->i_mmap_writable != 0;
}
# 649 "include/linux/fs.h"
struct inode {
 struct hlist_node i_hash;
 struct list_head i_list;
 struct list_head i_sb_list;
 struct list_head i_dentry;
 unsigned long i_ino;
 atomic_t i_count;
 unsigned int i_nlink;
 uid_t i_uid;
 gid_t i_gid;
 dev_t i_rdev;
 u64 i_version;
 loff_t i_size;



 struct timespec i_atime;
 struct timespec i_mtime;
 struct timespec i_ctime;
 unsigned int i_blkbits;
 blkcnt_t i_blocks;
 unsigned short i_bytes;
 umode_t i_mode;
 spinlock_t i_lock;
 struct mutex i_mutex;
 struct rw_semaphore i_alloc_sem;
 const struct inode_operations *i_op;
 const struct file_operations *i_fop;
 struct super_block *i_sb;
 struct file_lock *i_flock;
 struct address_space *i_mapping;
 struct address_space i_data;

 struct dquot *i_dquot[2];

 struct list_head i_devices;
 union {
  struct pipe_inode_info *i_pipe;
  struct block_device *i_bdev;
  struct cdev *i_cdev;
 };
 int i_cindex;

 __u32 i_generation;


 unsigned long i_dnotify_mask;
 struct dnotify_struct *i_dnotify;



 struct list_head inotify_watches;
 struct mutex inotify_mutex;


 unsigned long i_state;
 unsigned long dirtied_when;

 unsigned int i_flags;

 atomic_t i_writecount;

 void *i_security;

 void *i_private;
};
# 727 "include/linux/fs.h"
enum inode_i_mutex_lock_class
{
 I_MUTEX_NORMAL,
 I_MUTEX_PARENT,
 I_MUTEX_CHILD,
 I_MUTEX_XATTR,
 I_MUTEX_QUOTA
};

extern void inode_double_lock(struct inode *inode1, struct inode *inode2);
extern void inode_double_unlock(struct inode *inode1, struct inode *inode2);
# 749 "include/linux/fs.h"
static inline __attribute__((always_inline)) loff_t i_size_read(const struct inode *inode)
{
# 768 "include/linux/fs.h"
 return inode->i_size;

}






static inline __attribute__((always_inline)) void i_size_write(struct inode *inode, loff_t i_size)
{
# 788 "include/linux/fs.h"
 inode->i_size = i_size;

}

static inline __attribute__((always_inline)) unsigned iminor(const struct inode *inode)
{
 return ((unsigned int) ((inode->i_rdev) & ((1U << 20) - 1)));
}

static inline __attribute__((always_inline)) unsigned imajor(const struct inode *inode)
{
 return ((unsigned int) ((inode->i_rdev) >> 20));
}

extern struct block_device *I_BDEV(struct inode *inode);

struct fown_struct {
 rwlock_t lock;
 struct pid *pid;
 enum pid_type pid_type;
 uid_t uid, euid;
 int signum;
};




struct file_ra_state {
 unsigned long start;
 unsigned int size;
 unsigned int async_size;


 unsigned int ra_pages;
 int mmap_miss;
 loff_t prev_pos;
};




static inline __attribute__((always_inline)) int ra_has_index(struct file_ra_state *ra, unsigned long index)
{
 return (index >= ra->start &&
  index < ra->start + ra->size);
}




struct file {




 union {
  struct list_head fu_list;
  struct rcu_head fu_rcuhead;
 } f_u;
 struct path f_path;


 const struct file_operations *f_op;
 atomic_long_t f_count;
 unsigned int f_flags;
 fmode_t f_mode;
 loff_t f_pos;
 struct fown_struct f_owner;
 const struct cred *f_cred;
 struct file_ra_state f_ra;

 u64 f_version;

 void *f_security;


    void *__attribute__((recursive)) __attribute__((noderef, address_space(2))) private_data;



 struct list_head f_ep_links;
 spinlock_t f_ep_lock;

 struct address_space *f_mapping;



};
extern spinlock_t files_lock;
# 916 "include/linux/fs.h"
static inline __attribute__((always_inline)) void file_take_write(struct file *filp) {}
static inline __attribute__((always_inline)) void file_release_write(struct file *filp) {}
static inline __attribute__((always_inline)) void file_reset_write(struct file *filp) {}
static inline __attribute__((always_inline)) void file_check_state(struct file *filp) {}
static inline __attribute__((always_inline)) int file_check_writeable(struct file *filp)
{
 return 0;
}
# 957 "include/linux/fs.h"
typedef struct files_struct *fl_owner_t;

struct file_lock_operations {
 void (*fl_copy_lock)(struct file_lock *, struct file_lock *);
 void (*fl_release_private)(struct file_lock *);
};

struct lock_manager_operations {
 int (*fl_compare_owner)(struct file_lock *, struct file_lock *);
 void (*fl_notify)(struct file_lock *);
 int (*fl_grant)(struct file_lock *, struct file_lock *, int);
 void (*fl_copy_lock)(struct file_lock *, struct file_lock *);
 void (*fl_release_private)(struct file_lock *);
 void (*fl_break)(struct file_lock *);
 int (*fl_mylease)(struct file_lock *, struct file_lock *);
 int (*fl_change)(struct file_lock **, int);
};

struct lock_manager {
 struct list_head list;
};

void locks_start_grace(struct lock_manager *);
void locks_end_grace(struct lock_manager *);
int locks_in_grace(void);


# 1 "include/linux/nfs_fs_i.h" 1





# 1 "include/linux/nfs.h" 1
# 39 "include/linux/nfs.h"
 enum nfs_stat {
 NFS_OK = 0,
 NFSERR_PERM = 1,
 NFSERR_NOENT = 2,
 NFSERR_IO = 5,
 NFSERR_NXIO = 6,
 NFSERR_EAGAIN = 11,
 NFSERR_ACCES = 13,
 NFSERR_EXIST = 17,
 NFSERR_XDEV = 18,
 NFSERR_NODEV = 19,
 NFSERR_NOTDIR = 20,
 NFSERR_ISDIR = 21,
 NFSERR_INVAL = 22,
 NFSERR_FBIG = 27,
 NFSERR_NOSPC = 28,
 NFSERR_ROFS = 30,
 NFSERR_MLINK = 31,
 NFSERR_OPNOTSUPP = 45,
 NFSERR_NAMETOOLONG = 63,
 NFSERR_NOTEMPTY = 66,
 NFSERR_DQUOT = 69,
 NFSERR_STALE = 70,
 NFSERR_REMOTE = 71,
 NFSERR_WFLUSH = 99,
 NFSERR_BADHANDLE = 10001,
 NFSERR_NOT_SYNC = 10002,
 NFSERR_BAD_COOKIE = 10003,
 NFSERR_NOTSUPP = 10004,
 NFSERR_TOOSMALL = 10005,
 NFSERR_SERVERFAULT = 10006,
 NFSERR_BADTYPE = 10007,
 NFSERR_JUKEBOX = 10008,
 NFSERR_SAME = 10009,
 NFSERR_DENIED = 10010,
 NFSERR_EXPIRED = 10011,
 NFSERR_LOCKED = 10012,
 NFSERR_GRACE = 10013,
 NFSERR_FHEXPIRED = 10014,
 NFSERR_SHARE_DENIED = 10015,
 NFSERR_WRONGSEC = 10016,
 NFSERR_CLID_INUSE = 10017,
 NFSERR_RESOURCE = 10018,
 NFSERR_MOVED = 10019,
 NFSERR_NOFILEHANDLE = 10020,
 NFSERR_MINOR_VERS_MISMATCH = 10021,
 NFSERR_STALE_CLIENTID = 10022,
 NFSERR_STALE_STATEID = 10023,
 NFSERR_OLD_STATEID = 10024,
 NFSERR_BAD_STATEID = 10025,
 NFSERR_BAD_SEQID = 10026,
 NFSERR_NOT_SAME = 10027,
 NFSERR_LOCK_RANGE = 10028,
 NFSERR_SYMLINK = 10029,
 NFSERR_RESTOREFH = 10030,
 NFSERR_LEASE_MOVED = 10031,
 NFSERR_ATTRNOTSUPP = 10032,
 NFSERR_NO_GRACE = 10033,
 NFSERR_RECLAIM_BAD = 10034,
 NFSERR_RECLAIM_CONFLICT = 10035,
 NFSERR_BAD_XDR = 10036,
 NFSERR_LOCKS_HELD = 10037,
 NFSERR_OPENMODE = 10038,
 NFSERR_BADOWNER = 10039,
 NFSERR_BADCHAR = 10040,
 NFSERR_BADNAME = 10041,
 NFSERR_BAD_RANGE = 10042,
 NFSERR_LOCK_NOTSUPP = 10043,
 NFSERR_OP_ILLEGAL = 10044,
 NFSERR_DEADLOCK = 10045,
 NFSERR_FILE_OPEN = 10046,
 NFSERR_ADMIN_REVOKED = 10047,
 NFSERR_CB_PATH_DOWN = 10048,
 NFSERR_REPLAY_ME = 10049
};



enum nfs_ftype {
 NFNON = 0,
 NFREG = 1,
 NFDIR = 2,
 NFBLK = 3,
 NFCHR = 4,
 NFLNK = 5,
 NFSOCK = 6,
 NFBAD = 7,
 NFFIFO = 8
};


# 1 "include/linux/sunrpc/msg_prot.h" 1
# 18 "include/linux/sunrpc/msg_prot.h"
typedef u32 rpc_authflavor_t;

enum rpc_auth_flavors {
 RPC_AUTH_NULL = 0,
 RPC_AUTH_UNIX = 1,
 RPC_AUTH_SHORT = 2,
 RPC_AUTH_DES = 3,
 RPC_AUTH_KRB = 4,
 RPC_AUTH_GSS = 6,
 RPC_AUTH_MAXFLAVOR = 8,

 RPC_AUTH_GSS_KRB5 = 390003,
 RPC_AUTH_GSS_KRB5I = 390004,
 RPC_AUTH_GSS_KRB5P = 390005,
 RPC_AUTH_GSS_LKEY = 390006,
 RPC_AUTH_GSS_LKEYI = 390007,
 RPC_AUTH_GSS_LKEYP = 390008,
 RPC_AUTH_GSS_SPKM = 390009,
 RPC_AUTH_GSS_SPKMI = 390010,
 RPC_AUTH_GSS_SPKMP = 390011,
};




enum rpc_msg_type {
 RPC_CALL = 0,
 RPC_REPLY = 1
};

enum rpc_reply_stat {
 RPC_MSG_ACCEPTED = 0,
 RPC_MSG_DENIED = 1
};

enum rpc_accept_stat {
 RPC_SUCCESS = 0,
 RPC_PROG_UNAVAIL = 1,
 RPC_PROG_MISMATCH = 2,
 RPC_PROC_UNAVAIL = 3,
 RPC_GARBAGE_ARGS = 4,
 RPC_SYSTEM_ERR = 5,

 RPC_DROP_REPLY = 60000,
};

enum rpc_reject_stat {
 RPC_MISMATCH = 0,
 RPC_AUTH_ERROR = 1
};

enum rpc_auth_stat {
 RPC_AUTH_OK = 0,
 RPC_AUTH_BADCRED = 1,
 RPC_AUTH_REJECTEDCRED = 2,
 RPC_AUTH_BADVERF = 3,
 RPC_AUTH_REJECTEDVERF = 4,
 RPC_AUTH_TOOWEAK = 5,

 RPCSEC_GSS_CREDPROBLEM = 13,
 RPCSEC_GSS_CTXPROBLEM = 14
};
# 102 "include/linux/sunrpc/msg_prot.h"
typedef __be32 rpc_fraghdr;
# 131 "include/linux/nfs.h" 2






struct nfs_fh {
 unsigned short size;
 unsigned char data[128];
};





static inline __attribute__((always_inline)) int nfs_compare_fh(const struct nfs_fh *a, const struct nfs_fh *b)
{
 return a->size != b->size || memcmp(a->data, b->data, a->size) != 0;
}

static inline __attribute__((always_inline)) void nfs_copy_fh(struct nfs_fh *target, const struct nfs_fh *source)
{
 target->size = source->size;
 memcpy(target->data, source->data, source->size);
}
# 165 "include/linux/nfs.h"
enum nfs3_stable_how {
 NFS_UNSTABLE = 0,
 NFS_DATA_SYNC = 1,
 NFS_FILE_SYNC = 2
};
# 7 "include/linux/nfs_fs_i.h" 2

struct nlm_lockowner;




struct nfs_lock_info {
 u32 state;
 struct nlm_lockowner *owner;
 struct list_head list;
};

struct nfs4_lock_state;
struct nfs4_lock_info {
 struct nfs4_lock_state *owner;
};
# 985 "include/linux/fs.h" 2

struct file_lock {
 struct file_lock *fl_next;
 struct list_head fl_link;
 struct list_head fl_block;
 fl_owner_t fl_owner;
 unsigned char fl_flags;
 unsigned char fl_type;
 unsigned int fl_pid;
 struct pid *fl_nspid;
 wait_queue_head_t fl_wait;
 struct file *fl_file;
 loff_t fl_start;
 loff_t fl_end;

 struct fasync_struct * fl_fasync;
 unsigned long fl_break_time;

 struct file_lock_operations *fl_ops;
 struct lock_manager_operations *fl_lmops;
 union {
  struct nfs_lock_info nfs_fl;
  struct nfs4_lock_info nfs4_fl;
  struct {
   struct list_head link;
   int state;
  } afs;
 } fl_u;
};
# 1022 "include/linux/fs.h"
# 1 "include/linux/fcntl.h" 1



# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fcntl.h" 1
# 1 "include/asm-generic/fcntl.h" 1
# 117 "include/asm-generic/fcntl.h"
struct flock {
 short l_type;
 short l_whence;
 off_t l_start;
 off_t l_len;
 pid_t l_pid;

};
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/fcntl.h" 2
# 5 "include/linux/fcntl.h" 2
# 1023 "include/linux/fs.h" 2

extern void send_sigio(struct fown_struct *fown, int fd, int band);


extern int do_sync_mapping_range(struct address_space *mapping, loff_t offset,
   loff_t endbyte, unsigned int flags);


extern int fcntl_getlk(struct file *, struct flock __attribute__((noderef, address_space(1))) *);
extern int fcntl_setlk(unsigned int, struct file *, unsigned int,
   struct flock __attribute__((noderef, address_space(1))) *);







extern int fcntl_setlease(unsigned int fd, struct file *filp, long arg);
extern int fcntl_getlease(struct file *filp);


extern void locks_init_lock(struct file_lock *);
extern void locks_copy_lock(struct file_lock *, struct file_lock *);
extern void __locks_copy_lock(struct file_lock *, const struct file_lock *);
extern void locks_remove_posix(struct file *, fl_owner_t);
extern void locks_remove_flock(struct file *);
extern void posix_test_lock(struct file *, struct file_lock *);
extern int posix_lock_file(struct file *, struct file_lock *, struct file_lock *);
extern int posix_lock_file_wait(struct file *, struct file_lock *);
extern int posix_unblock_lock(struct file *, struct file_lock *);
extern int vfs_test_lock(struct file *, struct file_lock *);
extern int vfs_lock_file(struct file *, unsigned int, struct file_lock *, struct file_lock *);
extern int vfs_cancel_lock(struct file *filp, struct file_lock *fl);
extern int flock_lock_file_wait(struct file *filp, struct file_lock *fl);
extern int __break_lease(struct inode *inode, unsigned int flags);
extern void lease_get_mtime(struct inode *, struct timespec *time);
extern int generic_setlease(struct file *, long, struct file_lock **);
extern int vfs_setlease(struct file *, long, struct file_lock **);
extern int lease_modify(struct file_lock **, int);
extern int lock_may_read(struct inode *, loff_t start, unsigned long count);
extern int lock_may_write(struct inode *, loff_t start, unsigned long count);
# 1097 "include/linux/fs.h"
struct fasync_struct {
 int magic;
 int fa_fd;
 struct fasync_struct *fa_next;
 struct file *fa_file;
};




extern int fasync_helper(int, struct file *, int, struct fasync_struct **);

extern void kill_fasync(struct fasync_struct **, int, int);

extern void __kill_fasync(struct fasync_struct *, int, int);

extern int __f_setown(struct file *filp, struct pid *, enum pid_type, int force);
extern int f_setown(struct file *filp, unsigned long arg, int force);
extern void f_delown(struct file *filp);
extern pid_t f_getown(struct file *filp);
extern int send_sigurg(struct fown_struct *fown);
# 1127 "include/linux/fs.h"
extern struct list_head super_blocks;
extern spinlock_t sb_lock;



struct super_block {
 struct list_head s_list;
 dev_t s_dev;
 unsigned long s_blocksize;
 unsigned char s_blocksize_bits;
 unsigned char s_dirt;
 unsigned long long s_maxbytes;
 struct file_system_type *s_type;
 const struct super_operations *s_op;
 struct dquot_operations *dq_op;
  struct quotactl_ops *s_qcop;
 const struct export_operations *s_export_op;
 unsigned long s_flags;
 unsigned long s_magic;
 struct dentry *s_root;
 struct rw_semaphore s_umount;
 struct mutex s_lock;
 int s_count;
 int s_need_sync_fs;
 atomic_t s_active;

 void *s_security;

 struct xattr_handler **s_xattr;

 struct list_head s_inodes;
 struct list_head s_dirty;
 struct list_head s_io;
 struct list_head s_more_io;
 struct hlist_head s_anon;
 struct list_head s_files;

 struct list_head s_dentry_lru;
 int s_nr_dentry_unused;

 struct block_device *s_bdev;
 struct mtd_info *s_mtd;
 struct list_head s_instances;
 struct quota_info s_dquot;

 int s_frozen;
 wait_queue_head_t s_wait_unfrozen;

 char s_id[32];

 void *s_fs_info;
 fmode_t s_mode;





 struct mutex s_vfs_rename_mutex;



 u32 s_time_gran;





 char *s_subtype;





 char *s_options;




 struct list_head s_async_list;
};

extern struct timespec current_fs_time(struct super_block *sb);




enum {
 SB_UNFROZEN = 0,
 SB_FREEZE_WRITE = 1,
 SB_FREEZE_TRANS = 2,
};
# 1230 "include/linux/fs.h"
extern void lock_super(struct super_block *);
extern void unlock_super(struct super_block *);




extern int vfs_create(struct inode *, struct dentry *, int, struct nameidata *);
extern int vfs_mkdir(struct inode *, struct dentry *, int);
extern int vfs_mknod(struct inode *, struct dentry *, int, dev_t);
extern int vfs_symlink(struct inode *, struct dentry *, const char *);
extern int vfs_link(struct dentry *, struct inode *, struct dentry *);
extern int vfs_rmdir(struct inode *, struct dentry *);
extern int vfs_unlink(struct inode *, struct dentry *);
extern int vfs_rename(struct inode *, struct dentry *, struct inode *, struct dentry *);




extern void dentry_unhash(struct dentry *dentry);




extern int file_permission(struct file *, int);




struct fiemap_extent_info {
 unsigned int fi_flags;
 unsigned int fi_extents_mapped;
 unsigned int fi_extents_max;
 struct fiemap_extent *fi_extents_start;

};
int fiemap_fill_next_extent(struct fiemap_extent_info *info, u64 logical,
       u64 phys, u64 len, u32 flags);
int fiemap_check_flags(struct fiemap_extent_info *fieinfo, u32 fs_flags);
# 1288 "include/linux/fs.h"
int generic_osync_inode(struct inode *, struct address_space *, int);







typedef int (*filldir_t)(void *, const char *, int, loff_t, u64, unsigned);
struct block_device_operations;
# 1310 "include/linux/fs.h"
struct file_operations {
 struct module *owner;
 loff_t (*llseek) (struct file *, loff_t, int);
 ssize_t (*read) (struct file *, char __attribute__((noderef, address_space(1))) *, size_t, loff_t *);
 ssize_t (*write) (struct file *, const char __attribute__((noderef, address_space(1))) *, size_t, loff_t *);
 ssize_t (*aio_read) (struct kiocb *, const struct iovec *, unsigned long, loff_t);
 ssize_t (*aio_write) (struct kiocb *, const struct iovec *, unsigned long, loff_t);
 int (*readdir) (struct file *, void *, filldir_t);
 unsigned int (*poll) (struct file *, struct poll_table_struct *);
 int (*ioctl) (struct inode *, struct file *, unsigned int, unsigned long);
 long (*unlocked_ioctl) (struct file *, unsigned int, unsigned long);
 long (*compat_ioctl) (struct file *, unsigned int, unsigned long);
 int (*mmap) (struct file *, struct vm_area_struct *);
 int (*open) (struct inode *, struct file *);
 int (*flush) (struct file *, fl_owner_t id);
 int (*release) (struct inode *, struct file *);
 int (*fsync) (struct file *, struct dentry *, int datasync);
 int (*aio_fsync) (struct kiocb *, int datasync);
 int (*fasync) (int, struct file *, int);
 int (*lock) (struct file *, int, struct file_lock *);
 ssize_t (*sendpage) (struct file *, struct page *, int, size_t, loff_t *, int);
 unsigned long (*get_unmapped_area)(struct file *, unsigned long, unsigned long, unsigned long, unsigned long);
 int (*check_flags)(int);
 int (*flock) (struct file *, int, struct file_lock *);
 ssize_t (*splice_write)(struct pipe_inode_info *, struct file *, loff_t *, size_t, unsigned int);
 ssize_t (*splice_read)(struct file *, loff_t *, struct pipe_inode_info *, size_t, unsigned int);
 int (*setlease)(struct file *, long, struct file_lock **);
};

struct inode_operations {
 int (*create) (struct inode *,struct dentry *,int, struct nameidata *);
 struct dentry * (*lookup) (struct inode *,struct dentry *, struct nameidata *);
 int (*link) (struct dentry *,struct inode *,struct dentry *);
 int (*unlink) (struct inode *,struct dentry *);
 int (*symlink) (struct inode *,struct dentry *,const char *);
 int (*mkdir) (struct inode *,struct dentry *,int);
 int (*rmdir) (struct inode *,struct dentry *);
 int (*mknod) (struct inode *,struct dentry *,int,dev_t);
 int (*rename) (struct inode *, struct dentry *,
   struct inode *, struct dentry *);
 int (*readlink) (struct dentry *, char __attribute__((noderef, address_space(1))) *,int);
 void * (*follow_link) (struct dentry *, struct nameidata *);
 void (*put_link) (struct dentry *, struct nameidata *, void *);
 void (*truncate) (struct inode *);
 int (*permission) (struct inode *, int);
 int (*setattr) (struct dentry *, struct iattr *);
 int (*getattr) (struct vfsmount *mnt, struct dentry *, struct kstat *);
 int (*setxattr) (struct dentry *, const char *,const void *,size_t,int);
 ssize_t (*getxattr) (struct dentry *, const char *, void *, size_t);
 ssize_t (*listxattr) (struct dentry *, char *, size_t);
 int (*removexattr) (struct dentry *, const char *);
 void (*truncate_range)(struct inode *, loff_t, loff_t);
 long (*fallocate)(struct inode *inode, int mode, loff_t offset,
     loff_t len);
 int (*fiemap)(struct inode *, struct fiemap_extent_info *, u64 start,
        u64 len);
};

struct seq_file;

ssize_t rw_copy_check_uvector(int type, const struct iovec __attribute__((noderef, address_space(1))) * uvector,
    unsigned long nr_segs, unsigned long fast_segs,
    struct iovec *fast_pointer,
    struct iovec **ret_pointer);

extern ssize_t vfs_read(struct file *, char __attribute__((noderef, address_space(1))) *, size_t, loff_t *);
extern ssize_t vfs_write(struct file *, const char __attribute__((noderef, address_space(1))) *, size_t, loff_t *);
extern ssize_t vfs_readv(struct file *, const struct iovec __attribute__((noderef, address_space(1))) *,
  unsigned long, loff_t *);
extern ssize_t vfs_writev(struct file *, const struct iovec __attribute__((noderef, address_space(1))) *,
  unsigned long, loff_t *);

struct super_operations {
    struct inode *(*alloc_inode)(struct super_block *sb);
 void (*destroy_inode)(struct inode *);

    void (*dirty_inode) (struct inode *);
 int (*write_inode) (struct inode *, int);
 void (*drop_inode) (struct inode *);
 void (*delete_inode) (struct inode *);
 void (*put_super) (struct super_block *);
 void (*write_super) (struct super_block *);
 int (*sync_fs)(struct super_block *sb, int wait);
 int (*freeze_fs) (struct super_block *);
 int (*unfreeze_fs) (struct super_block *);
 int (*statfs) (struct dentry *, struct kstatfs *);
 int (*remount_fs) (struct super_block *, int *, char *);
 void (*clear_inode) (struct inode *);
 void (*umount_begin) (struct super_block *);

 int (*show_options)(struct seq_file *, struct vfsmount *);
 int (*show_stats)(struct seq_file *, struct vfsmount *);

 ssize_t (*quota_read)(struct super_block *, int, char *, size_t, loff_t);
 ssize_t (*quota_write)(struct super_block *, int, const char *, size_t, loff_t);

 int (*bdev_try_to_free_page)(struct super_block*, struct page*, gfp_t);
};
# 1476 "include/linux/fs.h"
extern void __mark_inode_dirty(struct inode *, int);
static inline __attribute__((always_inline)) void mark_inode_dirty(struct inode *inode)
{
 __mark_inode_dirty(inode, (1 | 2 | 4));
}

static inline __attribute__((always_inline)) void mark_inode_dirty_sync(struct inode *inode)
{
 __mark_inode_dirty(inode, 1);
}
# 1495 "include/linux/fs.h"
static inline __attribute__((always_inline)) void inc_nlink(struct inode *inode)
{
 inode->i_nlink++;
}

static inline __attribute__((always_inline)) void inode_inc_link_count(struct inode *inode)
{
 inc_nlink(inode);
 mark_inode_dirty(inode);
}
# 1517 "include/linux/fs.h"
static inline __attribute__((always_inline)) void drop_nlink(struct inode *inode)
{
 inode->i_nlink--;
}
# 1530 "include/linux/fs.h"
static inline __attribute__((always_inline)) void clear_nlink(struct inode *inode)
{
 inode->i_nlink = 0;
}

static inline __attribute__((always_inline)) void inode_dec_link_count(struct inode *inode)
{
 drop_nlink(inode);
 mark_inode_dirty(inode);
}
# 1549 "include/linux/fs.h"
static inline __attribute__((always_inline)) void inode_inc_iversion(struct inode *inode)
{
       _spin_lock(&inode->i_lock);
       inode->i_version++;
       _spin_unlock(&inode->i_lock);
}

extern void touch_atime(struct vfsmount *mnt, struct dentry *dentry);
static inline __attribute__((always_inline)) void file_accessed(struct file *file)
{
 if (!(file->f_flags & 01000000))
  touch_atime(file->f_path.mnt, file->f_path.dentry);
}

int sync_inode(struct inode *inode, struct writeback_control *wbc);

struct file_system_type {
 const char *name;
 int fs_flags;
 int (*get_sb) (struct file_system_type *, int,
         const char *, void *, struct vfsmount *);
 void (*kill_sb) (struct super_block *);
 struct module *owner;
 struct file_system_type * next;
 struct list_head fs_supers;

 struct lock_class_key s_lock_key;
 struct lock_class_key s_umount_key;

 struct lock_class_key i_lock_key;
 struct lock_class_key i_mutex_key;
 struct lock_class_key i_mutex_dir_key;
 struct lock_class_key i_alloc_sem_key;
};

extern int get_sb_bdev(struct file_system_type *fs_type,
 int flags, const char *dev_name, void *data,
 int (*fill_super)(struct super_block *, void *, int),
 struct vfsmount *mnt);
extern int get_sb_single(struct file_system_type *fs_type,
 int flags, void *data,
 int (*fill_super)(struct super_block *, void *, int),
 struct vfsmount *mnt);
extern int get_sb_nodev(struct file_system_type *fs_type,
 int flags, void *data,
 int (*fill_super)(struct super_block *, void *, int),
 struct vfsmount *mnt);
void generic_shutdown_super(struct super_block *sb);
void kill_block_super(struct super_block *sb);
void kill_anon_super(struct super_block *sb);
void kill_litter_super(struct super_block *sb);
void deactivate_super(struct super_block *sb);
int set_anon_super(struct super_block *s, void *data);
struct super_block *sget(struct file_system_type *type,
   int (*test)(struct super_block *,void *),
   int (*set)(struct super_block *,void *),
   void *data);
extern int get_sb_pseudo(struct file_system_type *, char *,
 const struct super_operations *ops, unsigned long,
 struct vfsmount *mnt);
extern int simple_set_mnt(struct vfsmount *mnt, struct super_block *sb);
int __put_super_and_need_restart(struct super_block *sb);







extern int register_filesystem(struct file_system_type *);
extern int unregister_filesystem(struct file_system_type *);
extern struct vfsmount *kern_mount_data(struct file_system_type *, void *data);

extern int may_umount_tree(struct vfsmount *);
extern int may_umount(struct vfsmount *);
extern long do_mount(char *, char *, char *, unsigned long, void *);
extern struct vfsmount *collect_mounts(struct vfsmount *, struct dentry *);
extern void drop_collected_mounts(struct vfsmount *);

extern int vfs_statfs(struct dentry *, struct kstatfs *);


extern struct kobject *fs_kobj;

extern int rw_verify_area(int, struct file *, loff_t *, size_t);





extern int locks_mandatory_locked(struct inode *);
extern int locks_mandatory_area(int, struct inode *, struct file *, loff_t, size_t);






static inline __attribute__((always_inline)) int __mandatory_lock(struct inode *ino)
{
 return (ino->i_mode & (0002000 | 00010)) == 0002000;
}






static inline __attribute__((always_inline)) int mandatory_lock(struct inode *ino)
{
 return ((ino)->i_sb->s_flags & (64)) && __mandatory_lock(ino);
}

static inline __attribute__((always_inline)) int locks_verify_locked(struct inode *inode)
{
 if (mandatory_lock(inode))
  return locks_mandatory_locked(inode);
 return 0;
}

static inline __attribute__((always_inline)) int locks_verify_truncate(struct inode *inode,
        struct file *filp,
        loff_t size)
{
 if (inode->i_flock && mandatory_lock(inode))
  return locks_mandatory_area(
   2, inode, filp,
   size < inode->i_size ? size : inode->i_size,
   (size < inode->i_size ? inode->i_size - size
    : size - inode->i_size)
  );
 return 0;
}

static inline __attribute__((always_inline)) int break_lease(struct inode *inode, unsigned int mode)
{
 if (inode->i_flock)
  return __break_lease(inode, mode);
 return 0;
}
# 1701 "include/linux/fs.h"
extern int do_truncate(struct dentry *, loff_t start, unsigned int time_attrs,
         struct file *filp);
extern long do_sys_open(int dfd, const char __attribute__((noderef, address_space(1))) *filename, int flags,
   int mode);
extern struct file *filp_open(const char *, int, int);
extern struct file * dentry_open(struct dentry *, struct vfsmount *, int,
     const struct cred *);
extern int filp_close(struct file *, fl_owner_t id);
extern char * getname(const char __attribute__((noderef, address_space(1))) *);


extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) vfs_caches_init_early(void);
extern void __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) vfs_caches_init(unsigned long);

extern struct kmem_cache *names_cachep;






extern void putname(const char *name);



extern int register_blkdev(unsigned int, const char *);
extern void unregister_blkdev(unsigned int, const char *);
extern struct block_device *bdget(dev_t);
extern void bd_set_size(struct block_device *, loff_t size);
extern void bd_forget(struct inode *inode);
extern void bdput(struct block_device *);
extern struct block_device *open_by_devnum(dev_t, fmode_t);



extern const struct file_operations def_blk_fops;
extern const struct file_operations def_chr_fops;
extern const struct file_operations bad_sock_fops;
extern const struct file_operations def_fifo_fops;

extern int ioctl_by_bdev(struct block_device *, unsigned, unsigned long);
extern int blkdev_ioctl(struct block_device *, fmode_t, unsigned, unsigned long);
extern long compat_blkdev_ioctl(struct file *, unsigned, unsigned long);
extern int blkdev_get(struct block_device *, fmode_t);
extern int blkdev_put(struct block_device *, fmode_t);
extern int bd_claim(struct block_device *, void *);
extern void bd_release(struct block_device *);

extern int bd_claim_by_disk(struct block_device *, void *, struct gendisk *);
extern void bd_release_from_disk(struct block_device *, struct gendisk *);
# 1759 "include/linux/fs.h"
extern int alloc_chrdev_region(dev_t *, unsigned, unsigned, const char *__attribute__((nullterm)));
extern int register_chrdev_region(dev_t, unsigned, const char *__attribute__((nullterm)));
extern int register_chrdev(unsigned int, const char *__attribute__((nullterm)),
      const struct file_operations *);
extern void unregister_chrdev(unsigned int, const char *__attribute__((nullterm)));
extern void unregister_chrdev_region(dev_t, unsigned);
extern void chrdev_show(struct seq_file *,off_t);







extern const char *__bdevname(dev_t, char *buffer);
extern const char *bdevname(struct block_device *bdev, char *buffer);
extern struct block_device *lookup_bdev(const char *);
extern struct block_device *open_bdev_exclusive(const char *, fmode_t, void *);
extern void close_bdev_exclusive(struct block_device *, fmode_t);
extern void blkdev_show(struct seq_file *,off_t);





extern void init_special_inode(struct inode *, umode_t, dev_t);


extern void make_bad_inode(struct inode *);
extern int is_bad_inode(struct inode *);

extern const struct file_operations read_pipefifo_fops;
extern const struct file_operations write_pipefifo_fops;
extern const struct file_operations rdwr_pipefifo_fops;

extern int fs_may_remount_ro(struct super_block *);
# 1807 "include/linux/fs.h"
extern void check_disk_size_change(struct gendisk *disk,
       struct block_device *bdev);
extern int revalidate_disk(struct gendisk *);
extern int check_disk_change(struct block_device *);
extern int __invalidate_device(struct block_device *);
extern int invalidate_partition(struct gendisk *, int);

extern int invalidate_inodes(struct super_block *);
unsigned long __invalidate_mapping_pages(struct address_space *mapping,
     unsigned long start, unsigned long end,
     bool be_atomic);
unsigned long invalidate_mapping_pages(struct address_space *mapping,
     unsigned long start, unsigned long end);

static inline __attribute__((always_inline)) unsigned long __attribute__((deprecated))
invalidate_inode_pages(struct address_space *mapping)
{
 return invalidate_mapping_pages(mapping, 0, ~0UL);
}

static inline __attribute__((always_inline)) void invalidate_remote_inode(struct inode *inode)
{
 if ((((inode->i_mode) & 00170000) == 0100000) || (((inode->i_mode) & 00170000) == 0040000) ||
     (((inode->i_mode) & 00170000) == 0120000))
  invalidate_mapping_pages(inode->i_mapping, 0, -1);
}
extern int invalidate_inode_pages2(struct address_space *mapping);
extern int invalidate_inode_pages2_range(struct address_space *mapping,
      unsigned long start, unsigned long end);
extern void generic_sync_sb_inodes(struct super_block *sb,
    struct writeback_control *wbc);
extern int write_inode_now(struct inode *, int);
extern int filemap_fdatawrite(struct address_space *);
extern int filemap_flush(struct address_space *);
extern int filemap_fdatawait(struct address_space *);
extern int filemap_write_and_wait(struct address_space *mapping);
extern int filemap_write_and_wait_range(struct address_space *mapping,
            loff_t lstart, loff_t lend);
extern int wait_on_page_writeback_range(struct address_space *mapping,
    unsigned long start, unsigned long end);
extern int __filemap_fdatawrite_range(struct address_space *mapping,
    loff_t start, loff_t end, int sync_mode);
extern int filemap_fdatawrite_range(struct address_space *mapping,
    loff_t start, loff_t end);

extern int vfs_fsync(struct file *file, struct dentry *dentry, int datasync);
extern void sync_supers(void);
extern void sync_filesystems(int wait);
extern void __fsync_super(struct super_block *sb);
extern void emergency_sync(void);
extern void emergency_remount(void);
extern int do_remount_sb(struct super_block *sb, int flags,
    void *data, int force);

extern sector_t bmap(struct inode *, sector_t);

extern int notify_change(struct dentry *, struct iattr *);
extern int inode_permission(struct inode *, int);
extern int generic_permission(struct inode *, int,
  int (*check_acl)(struct inode *, int));

static inline __attribute__((always_inline)) bool execute_ok(struct inode *inode)
{
 return (inode->i_mode & (00100|00010|00001)) || (((inode->i_mode) & 00170000) == 0040000);
}

extern int get_write_access(struct inode *);
extern int deny_write_access(struct file *);
static inline __attribute__((always_inline)) void put_write_access(struct inode * inode)
{
 atomic_dec(&inode->i_writecount);
}
static inline __attribute__((always_inline)) void allow_write_access(struct file *file)
{
 if (file)
  atomic_inc(&file->f_path.dentry->d_inode->i_writecount);
}
extern int do_pipe(int *);
extern int do_pipe_flags(int *, int);
extern struct file *create_read_pipe(struct file *f, int flags);
extern struct file *create_write_pipe(int flags);
extern void free_write_pipe(struct file *);

extern struct file *do_filp_open(int dfd, const char *pathname,
  int open_flag, int mode);
extern int may_open(struct path *, int, int);

extern int kernel_read(struct file *, unsigned long, char *, unsigned long);
extern struct file * open_exec(const char *);


extern int is_subdir(struct dentry *, struct dentry *);
extern ino_t find_inode_number(struct dentry *, struct qstr *);




extern loff_t default_llseek(struct file *file, loff_t offset, int origin);

extern loff_t vfs_llseek(struct file *file, loff_t offset, int origin);

extern struct inode * inode_init_always(struct super_block *, struct inode *);
extern void inode_init_once(struct inode *);
extern void inode_add_to_lists(struct super_block *, struct inode *);
extern void iput(struct inode *);
extern struct inode * igrab(struct inode *);
extern ino_t iunique(struct super_block *, ino_t);
extern int inode_needs_sync(struct inode *inode);
extern void generic_delete_inode(struct inode *inode);
extern void generic_drop_inode(struct inode *inode);

extern struct inode *ilookup5_nowait(struct super_block *sb,
  unsigned long hashval, int (*test)(struct inode *, void *),
  void *data);
extern struct inode *ilookup5(struct super_block *sb, unsigned long hashval,
  int (*test)(struct inode *, void *), void *data);
extern struct inode *ilookup(struct super_block *sb, unsigned long ino);

extern struct inode * iget5_locked(struct super_block *, unsigned long, int (*test)(struct inode *, void *), int (*set)(struct inode *, void *), void *);
extern struct inode * iget_locked(struct super_block *, unsigned long);
extern int insert_inode_locked4(struct inode *, unsigned long, int (*test)(struct inode *, void *), void *);
extern int insert_inode_locked(struct inode *);
extern void unlock_new_inode(struct inode *);

extern void __iget(struct inode * inode);
extern void iget_failed(struct inode *);
extern void clear_inode(struct inode *);
extern void destroy_inode(struct inode *);
extern struct inode *new_inode(struct super_block *);
extern int should_remove_suid(struct dentry *);
extern int file_remove_suid(struct file *);

extern void __insert_inode_hash(struct inode *, unsigned long hashval);
extern void remove_inode_hash(struct inode *);
static inline __attribute__((always_inline)) void insert_inode_hash(struct inode *inode) {
 __insert_inode_hash(inode, inode->i_ino);
}

extern struct file * get_empty_filp(void);
extern void file_move(struct file *f, struct list_head *list);
extern void file_kill(struct file *f);

struct bio;
extern void submit_bio(int, struct bio *);
extern int bdev_read_only(struct block_device *);

extern int set_blocksize(struct block_device *, int);
extern int sb_set_blocksize(struct super_block *, int);
extern int sb_min_blocksize(struct super_block *, int);
extern int sb_has_dirty_inodes(struct super_block *);

extern int generic_file_mmap(struct file *, struct vm_area_struct *);
extern int generic_file_readonly_mmap(struct file *, struct vm_area_struct *);
extern int file_read_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size);
int generic_write_checks(struct file *file, loff_t *pos, size_t *count, int isblk);
extern ssize_t generic_file_aio_read(struct kiocb *, const struct iovec *, unsigned long, loff_t);
extern ssize_t generic_file_aio_write(struct kiocb *, const struct iovec *, unsigned long, loff_t);
extern ssize_t generic_file_aio_write_nolock(struct kiocb *, const struct iovec *,
  unsigned long, loff_t);
extern ssize_t generic_file_direct_write(struct kiocb *, const struct iovec *,
  unsigned long *, loff_t, loff_t *, size_t, size_t);
extern ssize_t generic_file_buffered_write(struct kiocb *, const struct iovec *,
  unsigned long, loff_t, loff_t *, size_t, ssize_t);
extern ssize_t do_sync_read(struct file *filp, char __attribute__((noderef, address_space(1))) *buf, size_t len, loff_t *ppos);
extern ssize_t do_sync_write(struct file *filp, const char __attribute__((noderef, address_space(1))) *buf, size_t len, loff_t *ppos);
extern int generic_segment_checks(const struct iovec *iov,
  unsigned long *nr_segs, size_t *count, int access_flags);


extern ssize_t generic_file_splice_read(struct file *, loff_t *,
  struct pipe_inode_info *, size_t, unsigned int);
extern ssize_t generic_file_splice_write(struct pipe_inode_info *,
  struct file *, loff_t *, size_t, unsigned int);
extern ssize_t generic_file_splice_write_nolock(struct pipe_inode_info *,
  struct file *, loff_t *, size_t, unsigned int);
extern ssize_t generic_splice_sendpage(struct pipe_inode_info *pipe,
  struct file *out, loff_t *, size_t len, unsigned int flags);
extern long do_splice_direct(struct file *in, loff_t *ppos, struct file *out,
  size_t len, unsigned int flags);

extern void
file_ra_state_init(struct file_ra_state *ra, struct address_space *mapping);
extern loff_t no_llseek(struct file *file, loff_t offset, int origin);
extern loff_t generic_file_llseek(struct file *file, loff_t offset, int origin);
extern loff_t generic_file_llseek_unlocked(struct file *file, loff_t offset,
   int origin);
extern int generic_file_open(struct inode * inode, struct file * filp);
extern int nonseekable_open(struct inode * inode, struct file * filp);


extern ssize_t xip_file_read(struct file *filp, char __attribute__((noderef, address_space(1))) *buf, size_t len,
        loff_t *ppos);
extern int xip_file_mmap(struct file * file, struct vm_area_struct * vma);
extern ssize_t xip_file_write(struct file *filp, const char __attribute__((noderef, address_space(1))) *buf,
         size_t len, loff_t *ppos);
extern int xip_truncate_page(struct address_space *mapping, loff_t from);
# 2011 "include/linux/fs.h"
ssize_t __blockdev_direct_IO(int rw, struct kiocb *iocb, struct inode *inode,
 struct block_device *bdev, const struct iovec *iov, loff_t offset,
 unsigned long nr_segs, get_block_t get_block, dio_iodone_t end_io,
 int lock_type);

enum {
 DIO_LOCKING = 1,
 DIO_NO_LOCKING,
 DIO_OWN_LOCKING,
};

static inline __attribute__((always_inline)) ssize_t blockdev_direct_IO(int rw, struct kiocb *iocb,
 struct inode *inode, struct block_device *bdev, const struct iovec *iov,
 loff_t offset, unsigned long nr_segs, get_block_t get_block,
 dio_iodone_t end_io)
{
 return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
    nr_segs, get_block, end_io, DIO_LOCKING);
}

static inline __attribute__((always_inline)) ssize_t blockdev_direct_IO_no_locking(int rw, struct kiocb *iocb,
 struct inode *inode, struct block_device *bdev, const struct iovec *iov,
 loff_t offset, unsigned long nr_segs, get_block_t get_block,
 dio_iodone_t end_io)
{
 return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
    nr_segs, get_block, end_io, DIO_NO_LOCKING);
}

static inline __attribute__((always_inline)) ssize_t blockdev_direct_IO_own_locking(int rw, struct kiocb *iocb,
 struct inode *inode, struct block_device *bdev, const struct iovec *iov,
 loff_t offset, unsigned long nr_segs, get_block_t get_block,
 dio_iodone_t end_io)
{
 return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
    nr_segs, get_block, end_io, DIO_OWN_LOCKING);
}


extern const struct file_operations generic_ro_fops;



extern int vfs_readlink(struct dentry *, char __attribute__((noderef, address_space(1))) *, int, const char *);
extern int vfs_follow_link(struct nameidata *, const char *);
extern int page_readlink(struct dentry *, char __attribute__((noderef, address_space(1))) *, int);
extern void *page_follow_link_light(struct dentry *, struct nameidata *);
extern void page_put_link(struct dentry *, struct nameidata *, void *);
extern int __page_symlink(struct inode *inode, const char *symname, int len,
  int nofs);
extern int page_symlink(struct inode *inode, const char *symname, int len);
extern const struct inode_operations page_symlink_inode_operations;
extern int generic_readlink(struct dentry *, char __attribute__((noderef, address_space(1))) *, int);
extern void generic_fillattr(struct inode *, struct kstat *);
extern int vfs_getattr(struct vfsmount *, struct dentry *, struct kstat *);
void inode_add_bytes(struct inode *inode, loff_t bytes);
void inode_sub_bytes(struct inode *inode, loff_t bytes);
loff_t inode_get_bytes(struct inode *inode);
void inode_set_bytes(struct inode *inode, loff_t bytes);

extern int vfs_readdir(struct file *, filldir_t, void *);

extern int vfs_stat(char __attribute__((noderef, address_space(1))) *, struct kstat *);
extern int vfs_lstat(char __attribute__((noderef, address_space(1))) *, struct kstat *);
extern int vfs_stat_fd(int dfd, char __attribute__((noderef, address_space(1))) *, struct kstat *);
extern int vfs_lstat_fd(int dfd, char __attribute__((noderef, address_space(1))) *, struct kstat *);
extern int vfs_fstat(unsigned int, struct kstat *);

extern int do_vfs_ioctl(struct file *filp, unsigned int fd, unsigned int cmd,
      unsigned long arg);
extern int __generic_block_fiemap(struct inode *inode,
      struct fiemap_extent_info *fieinfo, u64 start,
      u64 len, get_block_t *get_block);
extern int generic_block_fiemap(struct inode *inode,
    struct fiemap_extent_info *fieinfo, u64 start,
    u64 len, get_block_t *get_block);

extern void get_filesystem(struct file_system_type *fs);
extern void put_filesystem(struct file_system_type *fs);
extern struct file_system_type *get_fs_type(const char *name);
extern struct super_block *get_super(struct block_device *);
extern struct super_block *user_get_super(dev_t);
extern void drop_super(struct super_block *sb);

extern int dcache_dir_open(struct inode *, struct file *);
extern int dcache_dir_close(struct inode *, struct file *);
extern loff_t dcache_dir_lseek(struct file *, loff_t, int);
extern int dcache_readdir(struct file *, void *, filldir_t);
extern int simple_getattr(struct vfsmount *, struct dentry *, struct kstat *);
extern int simple_statfs(struct dentry *, struct kstatfs *);
extern int simple_link(struct dentry *, struct inode *, struct dentry *);
extern int simple_unlink(struct inode *, struct dentry *);
extern int simple_rmdir(struct inode *, struct dentry *);
extern int simple_rename(struct inode *, struct dentry *, struct inode *, struct dentry *);
extern int simple_sync_file(struct file *, struct dentry *, int);
extern int simple_empty(struct dentry *);
extern int simple_readpage(struct file *file, struct page *page);
extern int simple_prepare_write(struct file *file, struct page *page,
   unsigned offset, unsigned to);
extern int simple_write_begin(struct file *file, struct address_space *mapping,
   loff_t pos, unsigned len, unsigned flags,
   struct page **pagep, void **fsdata);
extern int simple_write_end(struct file *file, struct address_space *mapping,
   loff_t pos, unsigned len, unsigned copied,
   struct page *page, void *fsdata);

extern struct dentry *simple_lookup(struct inode *, struct dentry *, struct nameidata *);
extern ssize_t generic_read_dir(struct file *, char __attribute__((noderef, address_space(1))) *, size_t, loff_t *);
extern const struct file_operations simple_dir_operations;
extern const struct inode_operations simple_dir_inode_operations;
struct tree_descr { char *name; const struct file_operations *ops; int mode; };
struct dentry *d_alloc_name(struct dentry *, const char *);
extern int simple_fill_super(struct super_block *, int, struct tree_descr *);
extern int simple_pin_fs(struct file_system_type *, struct vfsmount **mount, int *count);
extern void simple_release_fs(struct vfsmount **mount, int *count);

extern ssize_t simple_read_from_buffer(void __attribute__((noderef, address_space(1))) *to, size_t count,
   loff_t *ppos, const void *from, size_t available);


extern int buffer_migrate_page(struct address_space *,
    struct page *, struct page *);




extern int inode_change_ok(struct inode *, struct iattr *);
extern int __attribute__((warn_unused_result)) inode_setattr(struct inode *, struct iattr *);

extern void file_update_time(struct file *file);

extern int generic_show_options(struct seq_file *m, struct vfsmount *mnt);
extern void save_mount_options(struct super_block *sb, char *options);

static inline __attribute__((always_inline)) ino_t parent_ino(struct dentry *dentry)
{
 ino_t res;

 _spin_lock(&dentry->d_lock);
 res = dentry->d_parent->d_inode->i_ino;
 _spin_unlock(&dentry->d_lock);
 return res;
}







struct simple_transaction_argresp {
 ssize_t size;
 char data[0];
};



char *simple_transaction_get(struct file *file, const char __attribute__((noderef, address_space(1))) *buf,
    size_t size);
ssize_t simple_transaction_read(struct file *file, char __attribute__((noderef, address_space(1))) *buf,
    size_t size, loff_t *pos);
int simple_transaction_release(struct inode *inode, struct file *file);

static inline __attribute__((always_inline)) void simple_transaction_set(struct file *file, size_t n)
{
 struct simple_transaction_argresp *ar = file->private_data;

 do { if (__builtin_expect(!!(n > (((1UL) << 12) - sizeof(struct simple_transaction_argresp))), 0)) do { asm volatile("1:\tud2\n" ".pushsection __bug_table,\"a\"\n" "2:\t.long 1b - 2b, %c0 - 2b\n" "\t.word %c1, 0\n" "\t.org 2b+%c2\n" ".popsection" : : "i" ("include/linux/fs.h"), "i" (2178), "i" (sizeof(struct bug_entry))); for (;;) ; } while (0); } while(0);





 asm volatile("mfence":::"memory");
 ar->size = n;
}
# 2218 "include/linux/fs.h"
static inline __attribute__((always_inline)) void __attribute__((format(printf, 1, 2)))
__simple_attr_check_format(const char *fmt, ...)
{

}

int simple_attr_open(struct inode *inode, struct file *file,
       int (*get)(void *, u64 *), int (*set)(void *, u64),
       const char *fmt);
int simple_attr_release(struct inode *inode, struct file *file);
ssize_t simple_attr_read(struct file *file, char __attribute__((noderef, address_space(1))) *buf,
    size_t len, loff_t *ppos);
ssize_t simple_attr_write(struct file *file, const char __attribute__((noderef, address_space(1))) *buf,
     size_t len, loff_t *ppos);



static inline __attribute__((always_inline)) char *alloc_secdata(void)
{
 return (char *)get_zeroed_page(((( gfp_t)0x10u) | (( gfp_t)0x40u) | (( gfp_t)0x80u)));
}

static inline __attribute__((always_inline)) void free_secdata(void *secdata)
{
 free_pages(((unsigned long)secdata),0);
}
# 2254 "include/linux/fs.h"
struct ctl_table;
int proc_nr_files(struct ctl_table *table, int write, struct file *filp,
    void __attribute__((noderef, address_space(1))) *buffer, size_t *lenp, loff_t *ppos);

int get_filesystem_list(char * buf);
# 12 "include/linux/poll.h" 2

# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h" 1
# 95 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h"
struct exception_table_entry {
 unsigned long insn, fixup;
};

extern int fixup_exception(struct pt_regs *regs);
# 116 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h"
extern int __get_user_1(void);
extern int __get_user_2(void);
extern int __get_user_4(void);
extern int __get_user_8(void);
extern int __get_user_bad(void);
# 211 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h"
extern void __put_user_bad(void);





extern void __put_user_1(void);
extern void __put_user_2(void);
extern void __put_user_4(void);
extern void __put_user_8(void);
# 370 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h"
struct __large_struct { unsigned long buf[100]; };
# 452 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h"
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess_64.h" 1
# 18 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess_64.h"
__attribute__((warn_unused_result)) unsigned long
copy_user_generic(void *to, const void *from, unsigned len);

__attribute__((warn_unused_result)) unsigned long
copy_to_user(void __attribute__((noderef, address_space(1))) *to, const void *from, unsigned len);
__attribute__((warn_unused_result)) unsigned long
copy_from_user(void *to, const void __attribute__((noderef, address_space(1))) *from, unsigned len);
__attribute__((warn_unused_result)) unsigned long
copy_in_user(void __attribute__((noderef, address_space(1))) *to, const void __attribute__((noderef, address_space(1))) *from, unsigned len);

static inline __attribute__((always_inline)) __attribute__((always_inline)) __attribute__((warn_unused_result))
int __copy_from_user(void *dst, const void __attribute__((noderef, address_space(1))) *src, unsigned size)
{
 int ret = 0;

 might_fault();
 if (!__builtin_constant_p(size))
  return copy_user_generic(dst, ( void *)src, size);
 switch (size) {
 case 1:asm volatile("1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=q"(*(u8 *)dst) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u8 __attribute__((noderef, address_space(1))) *)src))), "i" (1), "0" (ret))
                                ;
  return ret;
 case 2:asm volatile("1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u16 *)dst) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u16 __attribute__((noderef, address_space(1))) *)src))), "i" (2), "0" (ret))
                                ;
  return ret;
 case 4:asm volatile("1:	mov""l"" %2,%""k""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""l"" %""k""1,%""k""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u32 *)dst) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u32 __attribute__((noderef, address_space(1))) *)src))), "i" (4), "0" (ret))
                                ;
  return ret;
 case 8:asm volatile("1:	mov""q"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""q"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u64 *)dst) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)src))), "i" (8), "0" (ret))
                               ;
  return ret;
 case 10:
  asm volatile("1:	mov""q"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""q"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u64 *)dst) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)src))), "i" (10), "0" (ret))
                                 ;
  if (__builtin_expect(!!(ret), 0))
   return ret;
  asm volatile("1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u16 *)(8 + (char *)dst)) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u16 __attribute__((noderef, address_space(1))) *)(8 + (char __attribute__((noderef, address_space(1))) *)src)))), "i" (2), "0" (ret))

                                 ;
  return ret;
 case 16:
  asm volatile("1:	mov""q"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""q"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u64 *)dst) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)src))), "i" (16), "0" (ret))
                                 ;
  if (__builtin_expect(!!(ret), 0))
   return ret;
  asm volatile("1:	mov""q"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""q"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(*(u64 *)(8 + (char *)dst)) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)(8 + (char __attribute__((noderef, address_space(1))) *)src)))), "i" (8), "0" (ret))

                                ;
  return ret;
 default:
  return copy_user_generic(dst, ( void *)src, size);
 }
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) __attribute__((warn_unused_result))
int __copy_to_user(void __attribute__((noderef, address_space(1))) *dst, const void *src, unsigned size)
{
 int ret = 0;

 might_fault();
 if (!__builtin_constant_p(size))
  return copy_user_generic(( void *)dst, src, size);
 switch (size) {
 case 1:asm volatile("1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "iq"(*(u8 *)src), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u8 __attribute__((noderef, address_space(1))) *)dst))), "i" (1), "0" (ret))
                                ;
  return ret;
 case 2:asm volatile("1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(*(u16 *)src), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u16 __attribute__((noderef, address_space(1))) *)dst))), "i" (2), "0" (ret))
                                ;
  return ret;
 case 4:asm volatile("1:	mov""l"" %""k""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(*(u32 *)src), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u32 __attribute__((noderef, address_space(1))) *)dst))), "i" (4), "0" (ret))
                                ;
  return ret;
 case 8:asm volatile("1:	mov""q"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(*(u64 *)src), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)dst))), "i" (8), "0" (ret))
                               ;
  return ret;
 case 10:
  asm volatile("1:	mov""q"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(*(u64 *)src), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)dst))), "i" (10), "0" (ret))
                                 ;
  if (__builtin_expect(!!(ret), 0))
   return ret;
  asm("":::"memory");
  asm volatile("1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(4[(u16 *)src]), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)(4 + (u16 __attribute__((noderef, address_space(1))) *)dst))), "i" (2), "0" (ret))
                                 ;
  return ret;
 case 16:
  asm volatile("1:	mov""q"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(*(u64 *)src), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)dst))), "i" (16), "0" (ret))
                                 ;
  if (__builtin_expect(!!(ret), 0))
   return ret;
  asm("":::"memory");
  asm volatile("1:	mov""q"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(1[(u64 *)src]), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)(1 + (u64 __attribute__((noderef, address_space(1))) *)dst))), "i" (8), "0" (ret))
                                ;
  return ret;
 default:
  return copy_user_generic(( void *)dst, src, size);
 }
}

static inline __attribute__((always_inline)) __attribute__((always_inline)) __attribute__((warn_unused_result))
int __copy_in_user(void __attribute__((noderef, address_space(1))) *dst, const void __attribute__((noderef, address_space(1))) *src, unsigned size)
{
 int ret = 0;

 might_fault();
 if (!__builtin_constant_p(size))
  return copy_user_generic(( void *)dst,
      ( void *)src, size);
 switch (size) {
 case 1: {
  u8 tmp;
  asm volatile("1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=q"(tmp) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u8 __attribute__((noderef, address_space(1))) *)src))), "i" (1), "0" (ret))
                                 ;
  if (__builtin_expect(!!(!ret), 1))
   asm volatile("1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "iq"(tmp), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u8 __attribute__((noderef, address_space(1))) *)dst))), "i" (1), "0" (ret))
                                  ;
  return ret;
 }
 case 2: {
  u16 tmp;
  asm volatile("1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(tmp) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u16 __attribute__((noderef, address_space(1))) *)src))), "i" (2), "0" (ret))
                                 ;
  if (__builtin_expect(!!(!ret), 1))
   asm volatile("1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(tmp), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u16 __attribute__((noderef, address_space(1))) *)dst))), "i" (2), "0" (ret))
                                  ;
  return ret;
 }

 case 4: {
  u32 tmp;
  asm volatile("1:	mov""l"" %2,%""k""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""l"" %""k""1,%""k""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(tmp) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u32 __attribute__((noderef, address_space(1))) *)src))), "i" (4), "0" (ret))
                                 ;
  if (__builtin_expect(!!(!ret), 1))
   asm volatile("1:	mov""l"" %""k""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(tmp), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u32 __attribute__((noderef, address_space(1))) *)dst))), "i" (4), "0" (ret))
                                  ;
  return ret;
 }
 case 8: {
  u64 tmp;
  asm volatile("1:	mov""q"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	xor""q"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r" (ret), "=r"(tmp) : "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)src))), "i" (8), "0" (ret))
                                ;
  if (__builtin_expect(!!(!ret), 1))
   asm volatile("1:	mov""q"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	mov %3,%0\n" "	jmp 2b\n" ".previous\n" " .section __ex_table,\"a\"\n" " " ".balign 8" " " "\n" " " ".quad" " " "1b" "," "3b" "\n" " .previous\n" : "=r"(ret) : "ir"(tmp), "m" ((*(struct __large_struct __attribute__((noderef, address_space(1))) *)((u64 __attribute__((noderef, address_space(1))) *)dst))), "i" (8), "0" (ret))
                                 ;
  return ret;
 }
 default:
  return copy_user_generic(( void *)dst,
      ( void *)src, size);
 }
}

__attribute__((warn_unused_result)) long
strncpy_from_user(char *dst, const char __attribute__((noderef, address_space(1))) *src, long count);
__attribute__((warn_unused_result)) long
__strncpy_from_user(char *dst, const char __attribute__((noderef, address_space(1))) *src, long count);
__attribute__((warn_unused_result)) long strnlen_user(const char __attribute__((noderef, address_space(1))) *str, long n);
__attribute__((warn_unused_result)) long __strnlen_user(const char __attribute__((noderef, address_space(1))) *str, long n);
__attribute__((warn_unused_result)) long strlen_user(const char __attribute__((noderef, address_space(1))) *str);
__attribute__((warn_unused_result)) unsigned long clear_user(void __attribute__((noderef, address_space(1))) *mem, unsigned long len);
__attribute__((warn_unused_result)) unsigned long __clear_user(void __attribute__((noderef, address_space(1))) *mem, unsigned long len);

__attribute__((warn_unused_result)) long __copy_from_user_inatomic(void *dst, const void __attribute__((noderef, address_space(1))) *src,
         unsigned size);

static __attribute__((warn_unused_result)) inline __attribute__((always_inline)) __attribute__((always_inline)) int
__copy_to_user_inatomic(void __attribute__((noderef, address_space(1))) *dst, const void *src, unsigned size)
{
 return copy_user_generic(( void *)dst, src, size);
}

extern long __copy_user_nocache(void *dst, const void __attribute__((noderef, address_space(1))) *src,
    unsigned size, int zerorest);

static inline __attribute__((always_inline)) int __copy_from_user_nocache(void *dst, const void __attribute__((noderef, address_space(1))) *src,
        unsigned size)
{
 do { __might_sleep("/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess_64.h", 194); _cond_resched(); } while (0);
 return __copy_user_nocache(dst, src, size, 1);
}

static inline __attribute__((always_inline)) int __copy_from_user_inatomic_nocache(void *dst,
          const void __attribute__((noderef, address_space(1))) *src,
          unsigned size)
{
 return __copy_user_nocache(dst, src, size, 0);
}

unsigned long
copy_user_handle_tail(char *to, char *from, unsigned len, unsigned zerorest);
# 453 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/uaccess.h" 2
# 14 "include/linux/poll.h" 2
# 26 "include/linux/poll.h"
struct poll_table_struct;




typedef void (*poll_queue_proc)(struct file *, wait_queue_head_t *, struct poll_table_struct *);

typedef struct poll_table_struct {
 poll_queue_proc qproc;
} poll_table;

static inline __attribute__((always_inline)) void poll_wait(struct file * filp, wait_queue_head_t * wait_address, poll_table *p)
{
 if (p && wait_address)
  p->qproc(filp, wait_address, p);
}

static inline __attribute__((always_inline)) void init_poll_funcptr(poll_table *pt, poll_queue_proc qproc)
{
 pt->qproc = qproc;
}

struct poll_table_entry {
 struct file *filp;
 wait_queue_t wait;
 wait_queue_head_t *wait_address;
};




struct poll_wqueues {
 poll_table pt;
 struct poll_table_page *table;
 struct task_struct *polling_task;
 int triggered;
 int error;
 int inline_index;
 struct poll_table_entry inline_entries[((832 - 256) / sizeof(struct poll_table_entry))];
};

extern void poll_initwait(struct poll_wqueues *pwq);
extern void poll_freewait(struct poll_wqueues *pwq);
extern int poll_schedule_timeout(struct poll_wqueues *pwq, int state,
     ktime_t *expires, unsigned long slack);

static inline __attribute__((always_inline)) int poll_schedule(struct poll_wqueues *pwq, int state)
{
 return poll_schedule_timeout(pwq, state, ((void *)0), 0);
}





typedef struct {
 unsigned long *in, *out, *ex;
 unsigned long *res_in, *res_out, *res_ex;
} fd_set_bits;
# 99 "include/linux/poll.h"
static inline __attribute__((always_inline))
int get_fd_set(unsigned long nr, void __attribute__((noderef, address_space(1))) *ufdset, unsigned long *fdset)
{
 nr = ((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long));
 if (ufdset)
  return copy_from_user(fdset, ufdset, nr) ? -14 : 0;

 memset(fdset, 0, nr);
 return 0;
}

static inline __attribute__((always_inline)) unsigned long __attribute__((warn_unused_result))
set_fd_set(unsigned long nr, void __attribute__((noderef, address_space(1))) *ufdset, unsigned long *fdset)
{
 if (ufdset)
  return __copy_to_user(ufdset, fdset, ((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)));
 return 0;
}

static inline __attribute__((always_inline))
void zero_fd_set(unsigned long nr, unsigned long *fdset)
{
 memset(fdset, 0, ((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)));
}



extern int do_select(int n, fd_set_bits *fds, struct timespec *end_time);
extern int do_sys_poll(struct pollfd __attribute__((noderef, address_space(1))) * ufds, unsigned int nfds,
         struct timespec *end_time);
extern int core_sys_select(int n, fd_set __attribute__((noderef, address_space(1))) *inp, fd_set __attribute__((noderef, address_space(1))) *outp,
      fd_set __attribute__((noderef, address_space(1))) *exp, struct timespec *end_time);

extern int poll_select_set_timeout(struct timespec *to, long sec, long nsec);
# 30 "include/sound/pcm.h" 2







# 1 "include/sound/pcm_oss.h" 1
# 25 "include/sound/pcm_oss.h"
struct snd_pcm_oss_setup {
 char *task_name;
 unsigned int disable:1,
       direct:1,
       block:1,
       nonblock:1,
       partialfrag:1,
       nosilence:1,
       buggyptr:1;
 unsigned int periods;
 unsigned int period_size;
    struct snd_pcm_oss_setup *__attribute__((recursive)) next;
};

struct snd_pcm_oss_runtime {
 unsigned params: 1,
   prepare: 1,
   trigger: 1,
   sync_trigger: 1;
 int rate;
 int format;
 unsigned int channels;
 unsigned int fragshift;
 unsigned int maxfrags;
 unsigned int subdivision;
 size_t period_bytes;
 size_t period_frames;
 size_t period_ptr;
 unsigned int periods;
 size_t buffer_bytes;
 size_t bytes;
 size_t mmap_bytes;
 char *buffer;
 size_t buffer_used;
 struct mutex params_lock;

 struct snd_pcm_plugin *plugin_first;
 struct snd_pcm_plugin *plugin_last;

 unsigned int prev_hw_ptr_interrupt;
};

struct snd_pcm_oss_file {
 struct snd_pcm_substream *streams[2];
};

struct snd_pcm_oss_substream {
 unsigned oss: 1;
 struct snd_pcm_oss_setup setup;
};

struct snd_pcm_oss_stream {
 struct snd_pcm_oss_setup *setup_list;
 struct mutex setup_mutex;

 struct snd_info_entry *proc_entry;

};

struct snd_pcm_oss {
 int reg;
 unsigned int reg_mask;
};
# 38 "include/sound/pcm.h" 2






struct snd_pcm_hardware {
 unsigned int info;
 u64 formats;
 unsigned int rates;
 unsigned int rate_min;
 unsigned int rate_max;
 unsigned int channels_min;
 unsigned int channels_max;
 size_t buffer_bytes_max;
 size_t period_bytes_min;
 size_t period_bytes_max;
 unsigned int periods_min;
 unsigned int periods_max;
 size_t fifo_size;
};

struct snd_pcm_substream;

struct snd_pcm_ops {
 int (*open)(struct snd_pcm_substream *substream);
 int (*close)(struct snd_pcm_substream *substream);
 int (*ioctl)(struct snd_pcm_substream * substream,
       unsigned int cmd, void *arg);
 int (*hw_params)(struct snd_pcm_substream *substream,
    struct snd_pcm_hw_params *params);
 int (*hw_free)(struct snd_pcm_substream *substream);
 int (*prepare)(struct snd_pcm_substream *substream);
 int (*trigger)(struct snd_pcm_substream *substream, int cmd);
 snd_pcm_uframes_t (*pointer)(struct snd_pcm_substream *substream);
 int (*copy)(struct snd_pcm_substream *substream, int channel,
      snd_pcm_uframes_t pos,
      void __attribute__((noderef, address_space(1))) *buf, snd_pcm_uframes_t count);
 int (*silence)(struct snd_pcm_substream *substream, int channel,
         snd_pcm_uframes_t pos, snd_pcm_uframes_t count);
 struct page *(*page)(struct snd_pcm_substream *substream,
        unsigned long offset);
 int (*mmap)(struct snd_pcm_substream *substream, struct vm_area_struct *vma);
 int (*ack)(struct snd_pcm_substream *substream);
};
# 199 "include/sound/pcm.h"
struct snd_pcm_file {
    struct snd_pcm_substream *__attribute__((recursive)) substream;
 int no_compat_mmap;
};

struct snd_pcm_hw_rule;
typedef int (*snd_pcm_hw_rule_func_t)(struct snd_pcm_hw_params *params,
          struct snd_pcm_hw_rule *rule);

struct snd_pcm_hw_rule {
 unsigned int cond;
 snd_pcm_hw_rule_func_t func;
 int var;
 int deps[4];
    void *__attribute__((recursive)) private;
};

struct snd_pcm_hw_constraints {
 struct snd_mask masks[2 -
    0 + 1];
 struct snd_interval intervals[19 -
        8 + 1];
 unsigned int rules_num;
 unsigned int rules_all;
    struct snd_pcm_hw_rule *__attribute__((recursive)) rules;
};

static inline __attribute__((always_inline)) struct snd_mask *constrs_mask(struct snd_pcm_hw_constraints *constrs,
         snd_pcm_hw_param_t var)
{
 return &constrs->masks[var - 0];
}

static inline __attribute__((always_inline)) struct snd_interval *constrs_interval(struct snd_pcm_hw_constraints *constrs,
          snd_pcm_hw_param_t var)
{
 return &constrs->intervals[var - 8];
}

struct snd_ratnum {
 unsigned int num;
 unsigned int den_min, den_max, den_step;
};

struct snd_ratden {
 unsigned int num_min, num_max, num_step;
 unsigned int den;
};

struct snd_pcm_hw_constraint_ratnums {
 int nrats;
 struct snd_ratnum *rats;
};

struct snd_pcm_hw_constraint_ratdens {
 int nrats;
 struct snd_ratden *rats;
};

struct snd_pcm_hw_constraint_list {
 unsigned int count;
 unsigned int *list;
 unsigned int mask;
};

struct snd_pcm_runtime {

    struct snd_pcm_substream *__attribute__((recursive)) trigger_master;
 struct timespec trigger_tstamp;
 int overrange;
 snd_pcm_uframes_t avail_max;
 snd_pcm_uframes_t hw_ptr_base;
 snd_pcm_uframes_t hw_ptr_interrupt;


 snd_pcm_access_t access;
 snd_pcm_format_t format;
 snd_pcm_subformat_t subformat;
 unsigned int rate;
 unsigned int channels;
 snd_pcm_uframes_t period_size;
 unsigned int periods;
 snd_pcm_uframes_t buffer_size;
 snd_pcm_uframes_t min_align;
 size_t byte_align;
 unsigned int frame_bits;
 unsigned int sample_bits;
 unsigned int info;
 unsigned int rate_num;
 unsigned int rate_den;


 int tstamp_mode;
   unsigned int period_step;
 snd_pcm_uframes_t start_threshold;
 snd_pcm_uframes_t stop_threshold;
 snd_pcm_uframes_t silence_threshold;

 snd_pcm_uframes_t silence_size;
 snd_pcm_uframes_t boundary;

 snd_pcm_uframes_t silence_start;
 snd_pcm_uframes_t silence_filled;

 union snd_pcm_sync_id sync;


 struct snd_pcm_mmap_status *status;
 struct snd_pcm_mmap_control *control;


 wait_queue_head_t sleep;
 struct fasync_struct *fasync;


 void *private_data;
 void (*private_free)(struct snd_pcm_runtime *runtime);


 struct snd_pcm_hardware hw;
    struct snd_pcm_hw_constraints __attribute__((recursive)) hw_constraints;


 void (*transfer_ack_begin)(struct snd_pcm_substream *substream);
 void (*transfer_ack_end)(struct snd_pcm_substream *substream);


 unsigned int timer_resolution;
 int tstamp_type;


    unsigned char * __attribute__((noderef, address_space(2))) dma_area;
    dma_addr_t __attribute__((noderef, address_space(2))) dma_addr;
    size_t __attribute__((noderef, address_space(2))) dma_bytes;

    struct snd_dma_buffer * __attribute__((noderef, address_space(2))) dma_buffer_p;



 struct snd_pcm_oss_runtime oss;

};

struct snd_pcm_group {
 spinlock_t lock;
 struct list_head substreams;
 int count;
};

struct snd_pcm_substream {
    struct snd_pcm *__attribute__((recursive)) pcm;
 struct snd_pcm_str *pstr;
    struct ensoniq *__attribute__((recursive)) private_data;
 int number;
 char name[32];
 int stream;
 char latency_id[20];
 size_t buffer_bytes_max;
    struct snd_dma_buffer __attribute__((recursive)) dma_buffer;
 unsigned int dma_buf_id;
 size_t dma_max;

 struct snd_pcm_ops *ops;

    struct snd_pcm_runtime *__attribute__((recursive)) runtime;

    struct snd_timer *__attribute__((recursive)) timer;
 unsigned timer_running: 1;
 spinlock_t timer_lock;

    struct snd_pcm_substream *__attribute__((recursive)) next;

 struct list_head link_list;
 struct snd_pcm_group self_group;
 struct snd_pcm_group *group;

    void *__attribute__((recursive)) file;
 int ref_count;
 atomic_t mmap_count;
 unsigned int f_flags;
 void (*pcm_release)(struct snd_pcm_substream *);


 struct snd_pcm_oss_substream oss;


    struct snd_info_entry *__attribute__((recursive)) proc_root;
    struct snd_info_entry *__attribute__((recursive)) proc_info_entry;
    struct snd_info_entry *__attribute__((recursive)) proc_hw_params_entry;
    struct snd_info_entry *__attribute__((recursive)) proc_sw_params_entry;
    struct snd_info_entry *__attribute__((recursive)) proc_status_entry;
    struct snd_info_entry *__attribute__((recursive)) proc_prealloc_entry;
    struct snd_info_entry *__attribute__((recursive)) proc_prealloc_max_entry;


 unsigned int hw_opened: 1;
};




struct snd_pcm_str {
 int stream;
    struct snd_pcm *__attribute__((recursive)) pcm;

 unsigned int substream_count;
 unsigned int substream_opened;
    struct snd_pcm_substream *__attribute__((recursive)) substream;


 struct snd_pcm_oss_stream oss;


    struct snd_info_entry *__attribute__((recursive)) proc_root;
    struct snd_info_entry *__attribute__((recursive)) proc_info_entry;





};

struct snd_pcm {
    struct snd_card *__attribute__((recursive)) card;
 struct list_head list;
 int device;
 unsigned int info_flags;
 unsigned short dev_class;
 unsigned short dev_subclass;
 char id[64];
 char name[80];
 struct snd_pcm_str streams[2];
 struct mutex open_mutex;
 wait_queue_head_t open_wait;
    void *__attribute__((recursive)) private_data;
 void (*private_free) (struct snd_pcm *pcm);
 struct device *dev;

 struct snd_pcm_oss oss;

};

struct snd_pcm_notify {
 int (*n_register) (struct snd_pcm * pcm);
 int (*n_disconnect) (struct snd_pcm * pcm);
 int (*n_unregister) (struct snd_pcm * pcm);
 struct list_head list;
};





extern const struct file_operations snd_pcm_f_ops[2];

int snd_pcm_new(struct snd_card *card, char *id, int device,
  int playback_count, int capture_count,
  struct snd_pcm **rpcm);
int snd_pcm_new_stream(struct snd_pcm *pcm, int stream, int substream_count);

int snd_pcm_notify(struct snd_pcm_notify *notify, int nfree);





extern rwlock_t snd_pcm_link_rwlock;

int snd_pcm_info(struct snd_pcm_substream *substream, struct snd_pcm_info *info);
int snd_pcm_info_user(struct snd_pcm_substream *substream,
        struct snd_pcm_info __attribute__((noderef, address_space(1))) *info);
int snd_pcm_status(struct snd_pcm_substream *substream,
     struct snd_pcm_status *status);
int snd_pcm_start(struct snd_pcm_substream *substream);
int snd_pcm_stop(struct snd_pcm_substream *substream, int status);
int snd_pcm_drain_done(struct snd_pcm_substream *substream);

int snd_pcm_suspend(struct snd_pcm_substream *substream);
int snd_pcm_suspend_all(struct snd_pcm *pcm);

int snd_pcm_kernel_ioctl(struct snd_pcm_substream *substream, unsigned int cmd, void *arg);
int snd_pcm_open_substream(struct snd_pcm *pcm, int stream, struct file *file,
      struct snd_pcm_substream **rsubstream);
void snd_pcm_release_substream(struct snd_pcm_substream *substream);
int snd_pcm_attach_substream(struct snd_pcm *pcm, int stream, struct file *file,
        struct snd_pcm_substream **rsubstream);
void snd_pcm_detach_substream(struct snd_pcm_substream *substream);
void snd_pcm_vma_notify_data(void *client, void *data);
int snd_pcm_mmap_data(struct snd_pcm_substream *substream, struct file *file, struct vm_area_struct *area);



static inline __attribute__((always_inline)) void div64_32(u_int64_t *n, u_int32_t div, u_int32_t *rem)
{
 *rem = *n % div;
 *n /= div;
}
# 567 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_stream_linked(struct snd_pcm_substream *substream)
{
 return substream->group != &substream->self_group;
}

static inline __attribute__((always_inline)) void snd_pcm_stream_lock(struct snd_pcm_substream *substream)
{
 _read_lock(&snd_pcm_link_rwlock);
 _spin_lock(&substream->self_group.lock);
}

static inline __attribute__((always_inline)) void snd_pcm_stream_unlock(struct snd_pcm_substream *substream)
{
 _spin_unlock(&substream->self_group.lock);
 _read_unlock(&snd_pcm_link_rwlock);
}

static inline __attribute__((always_inline)) void snd_pcm_stream_lock_irq(struct snd_pcm_substream *substream)
{
 _read_lock_irq(&snd_pcm_link_rwlock);
 _spin_lock(&substream->self_group.lock);
}

static inline __attribute__((always_inline)) void snd_pcm_stream_unlock_irq(struct snd_pcm_substream *substream)
{
 _spin_unlock(&substream->self_group.lock);
 _read_unlock_irq(&snd_pcm_link_rwlock);
}
# 611 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_running(struct snd_pcm_substream *substream)
{
 return (substream->runtime->status->state == (( snd_pcm_state_t) 3) ||
  (substream->runtime->status->state == (( snd_pcm_state_t) 5) &&
   substream->stream == SNDRV_PCM_STREAM_PLAYBACK));
}

static inline __attribute__((always_inline)) ssize_t bytes_to_samples(struct snd_pcm_runtime *runtime, ssize_t size)
{
 return size * 8 / runtime->sample_bits;
}

static inline __attribute__((always_inline)) snd_pcm_sframes_t bytes_to_frames(struct snd_pcm_runtime *runtime, ssize_t size)
{
 return size * 8 / runtime->frame_bits;
}

static inline __attribute__((always_inline)) ssize_t samples_to_bytes(struct snd_pcm_runtime *runtime, ssize_t size)
{
 return size * runtime->sample_bits / 8;
}

static inline __attribute__((always_inline)) ssize_t frames_to_bytes(struct snd_pcm_runtime *runtime, snd_pcm_sframes_t size)
{
 return size * runtime->frame_bits / 8;
}

static inline __attribute__((always_inline)) int frame_aligned(struct snd_pcm_runtime *runtime, ssize_t bytes)
{
 return bytes % runtime->byte_align == 0;
}

static inline __attribute__((always_inline)) size_t snd_pcm_lib_buffer_bytes(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 return frames_to_bytes(runtime, runtime->buffer_size);
}

static inline __attribute__((always_inline)) size_t snd_pcm_lib_period_bytes(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 return frames_to_bytes(runtime, runtime->period_size);
}




static inline __attribute__((always_inline)) snd_pcm_uframes_t snd_pcm_playback_avail(struct snd_pcm_runtime *runtime)
{
 snd_pcm_sframes_t avail = runtime->status->hw_ptr + runtime->buffer_size - runtime->control->appl_ptr;
 if (avail < 0)
  avail += runtime->boundary;
 else if ((snd_pcm_uframes_t) avail >= runtime->boundary)
  avail -= runtime->boundary;
 return avail;
}




static inline __attribute__((always_inline)) snd_pcm_uframes_t snd_pcm_capture_avail(struct snd_pcm_runtime *runtime)
{
 snd_pcm_sframes_t avail = runtime->status->hw_ptr - runtime->control->appl_ptr;
 if (avail < 0)
  avail += runtime->boundary;
 return avail;
}

static inline __attribute__((always_inline)) snd_pcm_sframes_t snd_pcm_playback_hw_avail(struct snd_pcm_runtime *runtime)
{
 return runtime->buffer_size - snd_pcm_playback_avail(runtime);
}

static inline __attribute__((always_inline)) snd_pcm_sframes_t snd_pcm_capture_hw_avail(struct snd_pcm_runtime *runtime)
{
 return runtime->buffer_size - snd_pcm_capture_avail(runtime);
}
# 697 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_playback_ready(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 return snd_pcm_playback_avail(runtime) >= runtime->control->avail_min;
}
# 711 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_capture_ready(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 return snd_pcm_capture_avail(runtime) >= runtime->control->avail_min;
}
# 726 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_playback_data(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;

 if (runtime->stop_threshold >= runtime->boundary)
  return 1;
 return snd_pcm_playback_avail(runtime) < runtime->buffer_size;
}
# 743 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_playback_empty(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 return snd_pcm_playback_avail(runtime) >= runtime->buffer_size;
}
# 757 "include/sound/pcm.h"
static inline __attribute__((always_inline)) int snd_pcm_capture_empty(struct snd_pcm_substream *substream)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 return snd_pcm_capture_avail(runtime) == 0;
}

static inline __attribute__((always_inline)) void snd_pcm_trigger_done(struct snd_pcm_substream *substream,
     struct snd_pcm_substream *master)
{
 substream->runtime->trigger_master = master;
}

static inline __attribute__((always_inline)) int hw_is_mask(int var)
{
 return var >= 0 &&
  var <= 2;
}

static inline __attribute__((always_inline)) int hw_is_interval(int var)
{
 return var >= 8 &&
  var <= 19;
}

static inline __attribute__((always_inline)) struct snd_mask *hw_param_mask(struct snd_pcm_hw_params *params,
         snd_pcm_hw_param_t var)
{
 return &params->masks[var - 0];
}

static inline __attribute__((always_inline)) struct snd_interval *hw_param_interval(struct snd_pcm_hw_params *params,
          snd_pcm_hw_param_t var)
{
 return &params->intervals[var - 8];
}

static inline __attribute__((always_inline)) const struct snd_mask *hw_param_mask_c(const struct snd_pcm_hw_params *params,
          snd_pcm_hw_param_t var)
{
 return &params->masks[var - 0];
}

static inline __attribute__((always_inline)) const struct snd_interval *hw_param_interval_c(const struct snd_pcm_hw_params *params,
           snd_pcm_hw_param_t var)
{
 return &params->intervals[var - 8];
}
# 817 "include/sound/pcm.h"
int snd_interval_refine(struct snd_interval *i, const struct snd_interval *v);
void snd_interval_mul(const struct snd_interval *a, const struct snd_interval *b, struct snd_interval *c);
void snd_interval_div(const struct snd_interval *a, const struct snd_interval *b, struct snd_interval *c);
void snd_interval_muldivk(const struct snd_interval *a, const struct snd_interval *b,
     unsigned int k, struct snd_interval *c);
void snd_interval_mulkdiv(const struct snd_interval *a, unsigned int k,
     const struct snd_interval *b, struct snd_interval *c);
int snd_interval_list(struct snd_interval *i, unsigned int count, unsigned int *list, unsigned int mask);
int snd_interval_ratnum(struct snd_interval *i,
   unsigned int rats_count, struct snd_ratnum *rats,
   unsigned int *nump, unsigned int *denp);

void _snd_pcm_hw_params_any(struct snd_pcm_hw_params *params);
void _snd_pcm_hw_param_setempty(struct snd_pcm_hw_params *params, snd_pcm_hw_param_t var);
int snd_pcm_hw_params_choose(struct snd_pcm_substream *substream, struct snd_pcm_hw_params *params);

int snd_pcm_hw_refine(struct snd_pcm_substream *substream, struct snd_pcm_hw_params *params);

int snd_pcm_hw_constraints_init(struct snd_pcm_substream *substream);
int snd_pcm_hw_constraints_complete(struct snd_pcm_substream *substream);

int snd_pcm_hw_constraint_mask(struct snd_pcm_runtime *runtime, snd_pcm_hw_param_t var,
          u_int32_t mask);
int snd_pcm_hw_constraint_mask64(struct snd_pcm_runtime *runtime, snd_pcm_hw_param_t var,
     u_int64_t mask);
int snd_pcm_hw_constraint_minmax(struct snd_pcm_runtime *runtime, snd_pcm_hw_param_t var,
     unsigned int min, unsigned int max);
int snd_pcm_hw_constraint_integer(struct snd_pcm_runtime *runtime, snd_pcm_hw_param_t var);
int snd_pcm_hw_constraint_list(struct snd_pcm_runtime *runtime,
          unsigned int cond,
          snd_pcm_hw_param_t var,
          struct snd_pcm_hw_constraint_list *l);
int snd_pcm_hw_constraint_ratnums(struct snd_pcm_runtime *runtime,
      unsigned int cond,
      snd_pcm_hw_param_t var,
      struct snd_pcm_hw_constraint_ratnums *r);
int snd_pcm_hw_constraint_ratdens(struct snd_pcm_runtime *runtime,
      unsigned int cond,
      snd_pcm_hw_param_t var,
      struct snd_pcm_hw_constraint_ratdens *r);
int snd_pcm_hw_constraint_msbits(struct snd_pcm_runtime *runtime,
     unsigned int cond,
     unsigned int width,
     unsigned int msbits);
int snd_pcm_hw_constraint_step(struct snd_pcm_runtime *runtime,
          unsigned int cond,
          snd_pcm_hw_param_t var,
          unsigned long step);
int snd_pcm_hw_constraint_pow2(struct snd_pcm_runtime *runtime,
          unsigned int cond,
          snd_pcm_hw_param_t var);
# 876 "include/sound/pcm.h"
int snd_pcm_hw_rule_add_MJR0(struct snd_pcm_runtime *runtime,
                             unsigned int cond,
                             int var,
                             snd_pcm_hw_rule_func_t func, void *private,
                             int dep);
int snd_pcm_hw_rule_add_MJR1(struct snd_pcm_runtime *runtime,
                             unsigned int cond,
                             int var,
                             snd_pcm_hw_rule_func_t func, void *private,
                             int dep, int MJR1);
int snd_pcm_hw_rule_add_MJR2(struct snd_pcm_runtime *runtime,
                             unsigned int cond,
                             int var,
                             snd_pcm_hw_rule_func_t func, void *private,
                             int dep, int MJR1, int MJR2);
int snd_pcm_hw_rule_add_MJR3(struct snd_pcm_runtime *runtime,
                             unsigned int cond,
                             int var,
                             snd_pcm_hw_rule_func_t func, void *private,
                             int dep, int MJR1, int MJR2, int MJR3);


int snd_pcm_format_signed(snd_pcm_format_t format);
int snd_pcm_format_unsigned(snd_pcm_format_t format);
int snd_pcm_format_linear(snd_pcm_format_t format);
int snd_pcm_format_little_endian(snd_pcm_format_t format);
int snd_pcm_format_big_endian(snd_pcm_format_t format);
# 918 "include/sound/pcm.h"
int snd_pcm_format_width(snd_pcm_format_t format);
int snd_pcm_format_physical_width(snd_pcm_format_t format);
ssize_t snd_pcm_format_size(snd_pcm_format_t format, size_t samples);
const unsigned char *snd_pcm_format_silence_64(snd_pcm_format_t format);
int snd_pcm_format_set_silence(snd_pcm_format_t format, void *buf, unsigned int frames);
snd_pcm_format_t snd_pcm_build_linear_format(int width, int unsignd, int big_endian);

void snd_pcm_set_ops(struct snd_pcm * pcm, int direction, struct snd_pcm_ops *ops);
void snd_pcm_set_sync(struct snd_pcm_substream *substream);
int snd_pcm_lib_interleave_len(struct snd_pcm_substream *substream);
int snd_pcm_lib_ioctl(struct snd_pcm_substream *substream,
        unsigned int cmd, void *arg);
int snd_pcm_update_hw_ptr(struct snd_pcm_substream *substream);
int snd_pcm_playback_xrun_check(struct snd_pcm_substream *substream);
int snd_pcm_capture_xrun_check(struct snd_pcm_substream *substream);
int snd_pcm_playback_xrun_asap(struct snd_pcm_substream *substream);
int snd_pcm_capture_xrun_asap(struct snd_pcm_substream *substream);
void snd_pcm_playback_silence(struct snd_pcm_substream *substream, snd_pcm_uframes_t new_hw_ptr);
void snd_pcm_period_elapsed(struct snd_pcm_substream *substream);
snd_pcm_sframes_t snd_pcm_lib_write(struct snd_pcm_substream *substream,
        const void __attribute__((noderef, address_space(1))) *buf,
        snd_pcm_uframes_t frames);
snd_pcm_sframes_t snd_pcm_lib_read(struct snd_pcm_substream *substream,
       void __attribute__((noderef, address_space(1))) *buf, snd_pcm_uframes_t frames);
snd_pcm_sframes_t snd_pcm_lib_writev(struct snd_pcm_substream *substream,
         void __attribute__((noderef, address_space(1))) **bufs, snd_pcm_uframes_t frames);
snd_pcm_sframes_t snd_pcm_lib_readv(struct snd_pcm_substream *substream,
        void __attribute__((noderef, address_space(1))) **bufs, snd_pcm_uframes_t frames);

extern const struct snd_pcm_hw_constraint_list snd_pcm_known_rates;

int snd_pcm_limit_hw_rates(struct snd_pcm_runtime *runtime);
unsigned int snd_pcm_rate_to_rate_bit(unsigned int rate);

static inline __attribute__((always_inline)) void snd_pcm_set_runtime_buffer(struct snd_pcm_substream *substream,
           struct snd_dma_buffer *bufp)
{
 struct snd_pcm_runtime *runtime = substream->runtime;
 if (bufp) {
  runtime->dma_buffer_p = bufp;
  runtime->dma_area = bufp->area;
  runtime->dma_addr = bufp->addr;
  runtime->dma_bytes = bufp->bytes;
 } else {
  runtime->dma_buffer_p = ((void *)0);
  runtime->dma_area = ((void *)0);
  runtime->dma_addr = 0;
  runtime->dma_bytes = 0;
 }
}





void snd_pcm_timer_resolution_change(struct snd_pcm_substream *substream);
void snd_pcm_timer_init(struct snd_pcm_substream *substream);
void snd_pcm_timer_done(struct snd_pcm_substream *substream);

static inline __attribute__((always_inline)) void snd_pcm_gettime(struct snd_pcm_runtime *runtime,
       struct timespec *tv)
{
 if (runtime->tstamp_type == SNDRV_PCM_TSTAMP_TYPE_MONOTONIC)
  ktime_get_ts(tv);
 else
  getnstimeofday(tv);
}





int snd_pcm_lib_preallocate_free(struct snd_pcm_substream *substream);
int snd_pcm_lib_preallocate_free_for_all(struct snd_pcm *pcm);
int snd_pcm_lib_preallocate_pages(struct snd_pcm_substream *substream,
      int type, struct device *data,
      size_t size, size_t max);
int snd_pcm_lib_preallocate_pages_for_all(struct snd_pcm *pcm,
       int type, void *data,
       size_t size, size_t max);
int snd_pcm_lib_malloc_pages(struct snd_pcm_substream *substream, size_t size);
int snd_pcm_lib_free_pages(struct snd_pcm_substream *substream);







static inline __attribute__((always_inline)) dma_addr_t
snd_pcm_sgbuf_get_addr(struct snd_pcm_substream *substream, unsigned int ofs)
{
 struct snd_sg_buf *sg = ((substream)->runtime->dma_buffer_p->private_data);
 return snd_sgbuf_get_addr(sg, ofs);
}

static inline __attribute__((always_inline)) void *
snd_pcm_sgbuf_get_ptr(struct snd_pcm_substream *substream, unsigned int ofs)
{
 struct snd_sg_buf *sg = ((substream)->runtime->dma_buffer_p->private_data);
 return snd_sgbuf_get_ptr(sg, ofs);
}

struct page *snd_pcm_sgbuf_ops_page(struct snd_pcm_substream *substream,
        unsigned long offset);
unsigned int snd_pcm_sgbuf_get_chunk_size(struct snd_pcm_substream *substream,
       unsigned int ofs, unsigned int size);


static inline __attribute__((always_inline)) void snd_pcm_mmap_data_open(struct vm_area_struct *area)
{
 struct snd_pcm_substream *substream = (struct snd_pcm_substream *)area->vm_private_data;
 atomic_inc(&substream->mmap_count);
}

static inline __attribute__((always_inline)) void snd_pcm_mmap_data_close(struct vm_area_struct *area)
{
 struct snd_pcm_substream *substream = (struct snd_pcm_substream *)area->vm_private_data;
 atomic_dec(&substream->mmap_count);
}




int snd_pcm_lib_mmap_iomem(struct snd_pcm_substream *substream, struct vm_area_struct *area);





static inline __attribute__((always_inline)) void snd_pcm_limit_isa_dma_size(int dma, size_t *max)
{
 *max = dma < 4 ? 64 * 1024 : 128 * 1024;
}
# 43 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 1 "include/sound/rawmidi.h" 1
# 32 "include/sound/rawmidi.h"
# 1 "include/sound/seq_device.h" 1
# 34 "include/sound/seq_device.h"
struct snd_seq_device {

    struct snd_card *__attribute__((recursive)) card;
 int device;
 char id[32];
 char name[80];
 int argsize;
 void *driver_data;
 int status;
    void *__attribute__((recursive)) private_data;
 void (*private_free)(struct snd_seq_device *device);
 struct list_head list;
};
# 62 "include/sound/seq_device.h"
struct snd_seq_dev_ops {
 int (*init_device)(struct snd_seq_device *dev);
 int (*free_device)(struct snd_seq_device *dev);
};




void snd_seq_device_load_drivers(void);
int snd_seq_device_new(struct snd_card *card, int device, char *id, int argsize, struct snd_seq_device **result);
int snd_seq_device_register_driver(char *id, struct snd_seq_dev_ops *entry, int argsize);
int snd_seq_device_unregister_driver(char *id);
# 33 "include/sound/rawmidi.h" 2
# 47 "include/sound/rawmidi.h"
struct snd_rawmidi;
struct snd_rawmidi_substream;
struct snd_seq_port_info;

struct snd_rawmidi_ops {
 int (*open) (struct snd_rawmidi_substream * substream);
 int (*close) (struct snd_rawmidi_substream * substream);
 void (*trigger) (struct snd_rawmidi_substream * substream, int up);
 void (*drain) (struct snd_rawmidi_substream * substream);
};

struct snd_rawmidi_global_ops {
 int (*dev_register) (struct snd_rawmidi * rmidi);
 int (*dev_unregister) (struct snd_rawmidi * rmidi);
 void (*get_port_info)(struct snd_rawmidi *rmidi, int number,
         struct snd_seq_port_info *info);
};

struct snd_rawmidi_runtime {
 unsigned int drain: 1,
       oss: 1;

 unsigned char *buffer;
 size_t buffer_size;
 size_t appl_ptr;
 size_t hw_ptr;
 size_t avail_min;
 size_t avail;
 size_t xruns;

 spinlock_t lock;
 wait_queue_head_t sleep;

 void (*event)(struct snd_rawmidi_substream *substream);

 struct tasklet_struct tasklet;

    void *__attribute__((recursive)) private_data;
 void (*private_free)(struct snd_rawmidi_substream *substream);
};

struct snd_rawmidi_substream {
 struct list_head list;
 int stream;
 int number;
 unsigned int opened: 1,
       append: 1,
       active_sensing: 1;
 int use_count;
 size_t bytes;
    struct snd_rawmidi *__attribute__((recursive)) rmidi;
 struct snd_rawmidi_str *pstr;
 char name[32];
    struct snd_rawmidi_runtime *__attribute__((recursive)) runtime;

 struct snd_rawmidi_ops *ops;
};

struct snd_rawmidi_file {
    struct snd_rawmidi *__attribute__((recursive)) rmidi;
    struct snd_rawmidi_substream *__attribute__((recursive)) input;
 struct snd_rawmidi_substream *output;
};

struct snd_rawmidi_str {
 unsigned int substream_count;
 unsigned int substream_opened;
 struct list_head substreams;
};

struct snd_rawmidi {
    struct snd_card *__attribute__((recursive)) card;
 struct list_head list;
 unsigned int device;
 unsigned int info_flags;
 char id[64];
 char name[80];


 int ossreg;


 struct snd_rawmidi_global_ops *ops;

 struct snd_rawmidi_str streams[2];

    struct ensoniq *__attribute__((recursive)) private_data;
 void (*private_free) (struct snd_rawmidi *rmidi);

 struct mutex open_mutex;
 wait_queue_head_t open_wait;

 struct snd_info_entry *dev;
    struct snd_info_entry *__attribute__((recursive)) proc_entry;


    struct snd_seq_device *__attribute__((recursive)) seq_dev;

};



int snd_rawmidi_new(struct snd_card *card, char *id, int device,
      int output_count, int input_count,
      struct snd_rawmidi **rmidi);
void snd_rawmidi_set_ops(struct snd_rawmidi *rmidi, int stream,
    struct snd_rawmidi_ops *ops);



void snd_rawmidi_receive_reset(struct snd_rawmidi_substream *substream);
int snd_rawmidi_receive(struct snd_rawmidi_substream *substream,
   const unsigned char *buffer, int count);
void snd_rawmidi_transmit_reset(struct snd_rawmidi_substream *substream);
int snd_rawmidi_transmit_empty(struct snd_rawmidi_substream *substream);
int snd_rawmidi_transmit_peek(struct snd_rawmidi_substream *substream,
         unsigned char *buffer, int count);
int snd_rawmidi_transmit_ack(struct snd_rawmidi_substream *substream, int count);
int snd_rawmidi_transmit(struct snd_rawmidi_substream *substream,
    unsigned char *buffer, int count);



int snd_rawmidi_info_select(struct snd_card *card, struct snd_rawmidi_info *info);
int snd_rawmidi_kernel_open(struct snd_card *card, int device, int subdevice,
       int mode, struct snd_rawmidi_file *rfile);
int snd_rawmidi_kernel_release(struct snd_rawmidi_file *rfile);
int snd_rawmidi_output_params(struct snd_rawmidi_substream *substream,
         struct snd_rawmidi_params *params);
int snd_rawmidi_input_params(struct snd_rawmidi_substream *substream,
        struct snd_rawmidi_params *params);
int snd_rawmidi_drop_output(struct snd_rawmidi_substream *substream);
int snd_rawmidi_drain_output(struct snd_rawmidi_substream *substream);
int snd_rawmidi_drain_input(struct snd_rawmidi_substream *substream);
long snd_rawmidi_kernel_read(struct snd_rawmidi_substream *substream,
        unsigned char *buf, long count);
long snd_rawmidi_kernel_write(struct snd_rawmidi_substream *substream,
         const unsigned char *buf, long count);
# 44 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2

# 1 "include/sound/ac97_codec.h" 1
# 31 "include/sound/ac97_codec.h"
# 1 "include/sound/pcm.h" 1
# 32 "include/sound/ac97_codec.h" 2
# 1 "include/sound/control.h" 1
# 33 "include/sound/ac97_codec.h" 2
# 1 "include/sound/info.h" 1
# 28 "include/sound/info.h"
struct snd_info_buffer {
    char *__attribute__((nullterm)) buffer;
 unsigned int curr;
 unsigned int size;
 unsigned int len;
 int stop;
 int error;
};




struct snd_info_entry;

struct snd_info_entry_text {
 void (*read)(struct snd_info_entry *entry,
       struct snd_info_buffer *buffer);
 void (*write)(struct snd_info_entry *entry,
        struct snd_info_buffer *buffer);
};

struct snd_info_entry_ops {
 int (*open)(struct snd_info_entry *entry,
      unsigned short mode, void **file_private_data);
 int (*release)(struct snd_info_entry *entry,
         unsigned short mode, void *file_private_data);
 long (*read)(struct snd_info_entry *entry, void *file_private_data,
       struct file *file, char __attribute__((noderef, address_space(1))) *buf,
       unsigned long count, unsigned long pos);
 long (*write)(struct snd_info_entry *entry, void *file_private_data,
        struct file *file, const char __attribute__((noderef, address_space(1))) *buf,
        unsigned long count, unsigned long pos);
 long long (*llseek)(struct snd_info_entry *entry,
       void *file_private_data, struct file *file,
       long long offset, int orig);
 unsigned int(*poll)(struct snd_info_entry *entry,
       void *file_private_data, struct file *file,
       poll_table *wait);
 int (*ioctl)(struct snd_info_entry *entry, void *file_private_data,
       struct file *file, unsigned int cmd, unsigned long arg);
 int (*mmap)(struct snd_info_entry *entry, void *file_private_data,
      struct inode *inode, struct file *file,
      struct vm_area_struct *vma);
};

struct snd_info_entry {
 const char *name;
 mode_t mode;
 long size;
 unsigned short content;
 union {
  struct snd_info_entry_text text;
  struct snd_info_entry_ops *ops;
 } c;
    struct snd_info_entry *__attribute__((recursive)) parent;
    struct snd_card *__attribute__((recursive)) card;
 struct module *module;

    struct ensoniq *__attribute__((recursive)) private_data;
 void (*private_free)(struct snd_info_entry *entry);
    struct proc_dir_entry *__attribute__((recursive)) p;
 struct mutex access;
 struct list_head children;
 struct list_head list;
};


int snd_info_minor_register(void);
int snd_info_minor_unregister(void);
# 105 "include/sound/info.h"
extern struct snd_info_entry *snd_seq_root;

extern struct snd_info_entry *snd_oss_root;
void snd_card_info_read_oss(struct snd_info_buffer *buffer);





int snd_iprintf(struct snd_info_buffer *buffer, char *fmt, ...)
    __attribute__ ((format (printf, 2, 3)));
int snd_info_init(void);
int snd_info_done(void);

int snd_info_get_line(struct snd_info_buffer *buffer, char *line, int len);
char *snd_info_get_str(char *dest, char *src, int len);
struct snd_info_entry *snd_info_create_module_entry(struct module *module,
            const char *name,
            struct snd_info_entry *parent);
struct snd_info_entry *snd_info_create_card_entry(struct snd_card *card,
          const char *name,
          struct snd_info_entry *parent);
void snd_info_free_entry(struct snd_info_entry *entry);
int snd_info_store_text(struct snd_info_entry *entry);
int snd_info_restore_text(struct snd_info_entry *entry);

int snd_info_card_create(struct snd_card *card);
int snd_info_card_register(struct snd_card *card);
int snd_info_card_free(struct snd_card *card);
void snd_info_card_disconnect(struct snd_card *card);
void snd_info_card_id_change(struct snd_card *card);
int snd_info_register(struct snd_info_entry *entry);


int snd_card_proc_new(struct snd_card *card, const char *name,
        struct snd_info_entry **entryp);

static inline __attribute__((always_inline)) void snd_info_set_text_ops(struct snd_info_entry *entry,
 void *private_data,
 void (*read)(struct snd_info_entry *, struct snd_info_buffer *))
{
 entry->private_data = private_data;
 entry->c.text.read = read;
}

int snd_info_check_reserved_words(const char *str);
# 198 "include/sound/info.h"
int snd_oss_info_register(int dev, int num, char *string);
# 34 "include/sound/ac97_codec.h" 2
# 416 "include/sound/ac97_codec.h"
struct snd_ac97;

struct snd_ac97_build_ops {
 int (*build_3d) (struct snd_ac97 *ac97);
 int (*build_specific) (struct snd_ac97 *ac97);
 int (*build_spdif) (struct snd_ac97 *ac97);
 int (*build_post_spdif) (struct snd_ac97 *ac97);

 void (*suspend) (struct snd_ac97 *ac97);
 void (*resume) (struct snd_ac97 *ac97);

 void (*update_jacks) (struct snd_ac97 *ac97);
};

struct snd_ac97_bus_ops {
 void (*reset) (struct snd_ac97 *ac97);
 void (*warm_reset)(struct snd_ac97 *ac97);
 void (*write) (struct snd_ac97 *ac97, unsigned short reg, unsigned short val);
 unsigned short (*read) (struct snd_ac97 *ac97, unsigned short reg);
 void (*wait) (struct snd_ac97 *ac97);
 void (*init) (struct snd_ac97 *ac97);
};

struct snd_ac97_bus {

 struct snd_ac97_bus_ops *ops;
    void *__attribute__((recursive)) private_data;
 void (*private_free) (struct snd_ac97_bus *bus);

 struct snd_card *card;
 unsigned short num;
 unsigned short no_vra: 1,
         dra: 1,
         isdin: 1;
 unsigned int clock;
 spinlock_t bus_lock;
 unsigned short used_slots[2][4];
 unsigned short pcms_count;
    struct ac97_pcm *__attribute__((recursive)) pcms;
 struct snd_ac97 *codec[4];
 struct snd_info_entry *proc;
};




struct snd_ac97_res_table {
 unsigned short reg;
 unsigned short bits;
};

struct snd_ac97_template {
 void *private_data;
 void (*private_free) (struct snd_ac97 *ac97);
 struct pci_dev *pci;
 unsigned short num;
 unsigned short addr;
 unsigned int scaps;
 const struct snd_ac97_res_table *res_table;
};

struct snd_ac97 {

 struct snd_ac97_build_ops * build_ops;
    struct ensoniq *__attribute__((recursive)) private_data;
 void (*private_free) (struct snd_ac97 *ac97);

    struct snd_ac97_bus *__attribute__((recursive)) bus;
 struct pci_dev *pci;
 struct snd_info_entry *proc;
 struct snd_info_entry *proc_regs;
 unsigned short subsystem_vendor;
 unsigned short subsystem_device;
 struct mutex reg_mutex;
 struct mutex page_mutex;
 unsigned short num;
 unsigned short addr;
 unsigned int id;
 unsigned short caps;
 unsigned short ext_id;
 unsigned short ext_mid;
 const struct snd_ac97_res_table *res_table;
 unsigned int scaps;
 unsigned int flags;
 unsigned int rates[6];
 unsigned int spdif_status;
 unsigned short regs[0x80];
 unsigned long reg_accessed[(((0x80) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))];
 union {
  struct {
   unsigned short unchained[3];
   unsigned short chained[3];
   unsigned short id[3];
   unsigned short pcmreg[3];
   unsigned short codec_cfg[3];
   unsigned char swap_mic_linein;
   unsigned char lo_as_master;
  } ad18xx;
  unsigned int dev_flags;
 } spec;

 unsigned char indep_surround;
 unsigned char channel_mode;





 struct device dev;
};




static inline __attribute__((always_inline)) int ac97_is_audio(struct snd_ac97 * ac97)
{
 return (ac97->scaps & (1<<0));
}
static inline __attribute__((always_inline)) int ac97_is_modem(struct snd_ac97 * ac97)
{
 return (ac97->scaps & (1<<1));
}
static inline __attribute__((always_inline)) int ac97_is_rev22(struct snd_ac97 * ac97)
{
 return (ac97->ext_id & 0x0c00) >= 0x0400;
}
static inline __attribute__((always_inline)) int ac97_can_amap(struct snd_ac97 * ac97)
{
 return (ac97->ext_id & 0x0200) != 0;
}
static inline __attribute__((always_inline)) int ac97_can_spdif(struct snd_ac97 * ac97)
{
 return (ac97->ext_id & 0x0004) != 0;
}



int snd_ac97_bus(struct snd_card *card, int num, struct snd_ac97_bus_ops *ops,
   void *private_data, struct snd_ac97_bus **rbus);

int snd_ac97_mixer(struct snd_ac97_bus *bus, struct snd_ac97_template *template,
     struct snd_ac97 **rac97);
const char *snd_ac97_get_short_name(struct snd_ac97 *ac97);

void snd_ac97_write(struct snd_ac97 *ac97, unsigned short reg, unsigned short value);
unsigned short snd_ac97_read(struct snd_ac97 *ac97, unsigned short reg);
void snd_ac97_write_cache(struct snd_ac97 *ac97, unsigned short reg, unsigned short value);
int snd_ac97_update(struct snd_ac97 *ac97, unsigned short reg, unsigned short value);
int snd_ac97_update_bits(struct snd_ac97 *ac97, unsigned short reg, unsigned short mask, unsigned short value);



static inline __attribute__((always_inline)) int snd_ac97_update_power(struct snd_ac97 *ac97, int reg,
     int powerup)
{
 return 0;
}


void snd_ac97_suspend(struct snd_ac97 *ac97);
void snd_ac97_resume(struct snd_ac97 *ac97);



enum {
 AC97_TUNE_DEFAULT = -1,
 AC97_TUNE_NONE = 0,
 AC97_TUNE_HP_ONLY,
 AC97_TUNE_SWAP_HP,
 AC97_TUNE_SWAP_SURROUND,
 AC97_TUNE_AD_SHARING,
 AC97_TUNE_ALC_JACK,
 AC97_TUNE_INV_EAPD,
 AC97_TUNE_MUTE_LED,
 AC97_TUNE_HP_MUTE_LED,
};

struct ac97_quirk {
 unsigned short subvendor;
 unsigned short subdevice;
 unsigned short mask;
 unsigned int codec_id;
 const char *name;
 int type;
};

int snd_ac97_tune_hardware(struct snd_ac97 *ac97, struct ac97_quirk *quirk, const char *override);
int snd_ac97_set_rate(struct snd_ac97 *ac97, int reg, unsigned int rate);





enum ac97_pcm_cfg {
 AC97_PCM_CFG_FRONT = 2,
 AC97_PCM_CFG_REAR = 10,
 AC97_PCM_CFG_LFE = 11,
 AC97_PCM_CFG_40 = 4,
 AC97_PCM_CFG_51 = 6,
 AC97_PCM_CFG_SPDIF = 20
};

struct ac97_pcm {
    struct snd_ac97_bus *__attribute__((recursive)) bus;
 unsigned int stream: 1,
       exclusive: 1,
       copy_flag: 1,
       spdif: 1;
 unsigned short aslots;
 unsigned short cur_dbl;
 unsigned int rates;
 struct {
  unsigned short slots;
  unsigned short rslots[4];
  unsigned char rate_table[4];
  struct snd_ac97 *codec[4];
 } r[2];
 unsigned long private_value;
};

int snd_ac97_pcm_assign(struct snd_ac97_bus *ac97,
   unsigned short pcms_count,
   const struct ac97_pcm *pcms);
int snd_ac97_pcm_open(struct ac97_pcm *pcm, unsigned int rate,
        enum ac97_pcm_cfg cfg, unsigned short slots);
int snd_ac97_pcm_close(struct ac97_pcm *pcm);
int snd_ac97_pcm_double_rate_rules(struct snd_pcm_runtime *runtime);


extern struct bus_type ac97_bus_type;
# 46 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2



# 1 "include/sound/initval.h" 1
# 50 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 1 "include/sound/asoundef.h" 1
# 51 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2


# 1 "include/linux/proc_fs.h" 1






# 1 "include/linux/magic.h" 1
# 8 "include/linux/proc_fs.h" 2
# 1 "/scratch/sym/annotated/linux-2.6.29-ipc-ens1371/arch/x86/include/asm/atomic.h" 1
# 9 "include/linux/proc_fs.h" 2

struct net;
struct completion;
struct mm_struct;
# 30 "include/linux/proc_fs.h"
enum {
 PROC_ROOT_INO = 1,
};
# 49 "include/linux/proc_fs.h"
typedef int (read_proc_t)(char *page, char **start, off_t off,
     int count, int *eof, void *data);
typedef int (write_proc_t)(struct file *file, const char __attribute__((noderef, address_space(1))) *buffer,
      unsigned long count, void *data);

struct proc_dir_entry {
 unsigned int low_ino;
 unsigned short namelen;
    const char *__attribute__((nullterm)) name;
 mode_t mode;
 nlink_t nlink;
 uid_t uid;
 gid_t gid;
 loff_t size;
 const struct inode_operations *proc_iops;
# 72 "include/linux/proc_fs.h"
 const struct file_operations *proc_fops;
 struct module *owner;
 struct proc_dir_entry *next, *parent, *subdir;
    void *__attribute__((recursive)) data;
 read_proc_t *read_proc;
 write_proc_t *write_proc;
 atomic_t count;
 int pde_users;
 spinlock_t pde_unload_lock;
 struct completion *pde_unload_completion;
 struct list_head pde_openers;
};

struct kcore_list {
 struct kcore_list *next;
 unsigned long addr;
 size_t size;
};

struct vmcore {
 struct list_head list;
 unsigned long long paddr;
 unsigned long long size;
 loff_t offset;
};



extern spinlock_t proc_subdir_lock;

extern void proc_root_init(void);

void proc_flush_task(struct task_struct *task);
struct dentry *proc_pid_lookup(struct inode *dir, struct dentry * dentry, struct nameidata *);
int proc_pid_readdir(struct file * filp, void * dirent, filldir_t filldir);
unsigned long task_vsize(struct mm_struct *);
int task_statm(struct mm_struct *, int *, int *, int *, int *);
void task_mem(struct seq_file *, struct mm_struct *);
void clear_refs_smap(struct mm_struct *mm);

struct proc_dir_entry *de_get(struct proc_dir_entry *de);
void de_put(struct proc_dir_entry *de);

extern struct proc_dir_entry *create_proc_entry(const char *__attribute__((nullterm)) name, mode_t mode,
      struct proc_dir_entry *parent);
struct proc_dir_entry *proc_create_data(const char *__attribute__((nullterm)) name, mode_t mode,
    struct proc_dir_entry *parent,
    const struct file_operations *proc_fops,
    void *data);
extern void remove_proc_entry(const char *__attribute__((nullterm)) name, struct proc_dir_entry *parent);

extern struct vfsmount *proc_mnt;
struct pid_namespace;
extern int proc_fill_super(struct super_block *);
extern struct inode *proc_get_inode(struct super_block *, unsigned int, struct proc_dir_entry *);
# 135 "include/linux/proc_fs.h"
extern int proc_readdir(struct file *, void *, filldir_t);
extern struct dentry *proc_lookup(struct inode *, struct dentry *, struct nameidata *);

extern int pid_ns_prepare_proc(struct pid_namespace *ns);
extern void pid_ns_release_proc(struct pid_namespace *ns);




struct tty_driver;
extern void proc_tty_init(void);
extern void proc_tty_register_driver(struct tty_driver *driver);
extern void proc_tty_unregister_driver(struct tty_driver *driver);
# 165 "include/linux/proc_fs.h"
extern struct proc_dir_entry *proc_symlink(const char * __attribute__((nullterm)),
                                           struct proc_dir_entry *, const char * __attribute__((nullterm)));
extern struct proc_dir_entry *proc_mkdir(const char *,struct proc_dir_entry *);
extern struct proc_dir_entry *proc_mkdir_mode(const char *name, mode_t mode,
   struct proc_dir_entry *parent);

static inline __attribute__((always_inline)) struct proc_dir_entry *proc_create(const char *name, mode_t mode,
 struct proc_dir_entry *parent, const struct file_operations *proc_fops)
{
 return proc_create_data(name, mode, parent, proc_fops, ((void *)0));
}

static inline __attribute__((always_inline)) struct proc_dir_entry *create_proc_read_entry(const char *name,
 mode_t mode, struct proc_dir_entry *base,
 read_proc_t *read_proc, void * data)
{
 struct proc_dir_entry *res=create_proc_entry(name,mode,base);
 if (res) {
  res->read_proc=read_proc;
  res->data=data;
 }
 return res;
}

extern struct proc_dir_entry *proc_net_fops_create(struct net *net,
 const char *name, mode_t mode, const struct file_operations *fops);
extern void proc_net_remove(struct net *net, const char *name);
extern struct proc_dir_entry *proc_net_mkdir(struct net *net, const char *name,
 struct proc_dir_entry *parent);



extern void set_mm_exe_file(struct mm_struct *mm, struct file *new_exe_file);
extern struct file *get_mm_exe_file(struct mm_struct *mm);
extern void dup_mm_exe_file(struct mm_struct *oldmm, struct mm_struct *newmm);
# 268 "include/linux/proc_fs.h"
extern void kclist_add(struct kcore_list *, void *, size_t);


union proc_op {
 int (*proc_get_link)(struct inode *, struct path *);
 int (*proc_read)(struct task_struct *task, char *page);
 int (*proc_show)(struct seq_file *m,
  struct pid_namespace *ns, struct pid *pid,
  struct task_struct *task);
};

struct ctl_table_header;
struct ctl_table;

struct proc_inode {
 struct pid *pid;
 int fd;
 union proc_op op;
 struct proc_dir_entry *pde;
 struct ctl_table_header *sysctl;
 struct ctl_table *sysctl_entry;
 struct inode vfs_inode;
};

static inline __attribute__((always_inline)) struct proc_inode *PROC_I(const struct inode *inode)
{
 return ({ const typeof( ((struct proc_inode *)0)->vfs_inode ) *__mptr = (inode); (struct proc_inode *)( (char *)__mptr - __builtin_offsetof(struct proc_inode,vfs_inode) );});
}

static inline __attribute__((always_inline)) struct proc_dir_entry *PDE(const struct inode *inode)
{
 return PROC_I(inode)->pde;
}

static inline __attribute__((always_inline)) struct net *PDE_NET(struct proc_dir_entry *pde)
{
 return pde->parent->data;
}

struct proc_maps_private {
 struct pid *pid;
 struct task_struct *task;

 struct vm_area_struct *tail_vma;

};
# 54 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2

# 1 "/scratch/sym/ipc_drivers/ens1371/modif_annots.h" 1






void MICRODRIVERS__DUMMY(void *x) {
    return;
}


void * MICRODRIVERS__MODIF_kmem_cache_zalloc
  (struct kmem_cache *a, gfp_t b) {
  return 0;
}


unsigned long MICRODRIVERS__MODIF__spin_lock_irqsave
    (spinlock_t *lock) {
    return 0;
}


void MICRODRIVERS__MODIF__spin_unlock_irqrestore
    (spinlock_t *lock, unsigned long flags) {
    return;
}


void MICRODRIVERS__MODIF_pci_disable_device
    (struct pci_dev *dev) {
    MICRODRIVERS__DUMMY((void *) dev->bus);
    MICRODRIVERS__DUMMY((void *) dev->devfn);
    return;
}


void MICRODRIVERS__MODIF_pci_set_master
    (struct pci_dev *dev) {
    MICRODRIVERS__DUMMY((void *) dev->bus);
    MICRODRIVERS__DUMMY((void *) dev->devfn);
    MICRODRIVERS__DUMMY((void *) dev->dev.bus_id);
    return;
}


void MICRODRIVERS__MODIF___release_region
    (struct resource * a, resource_size_t b, resource_size_t c) {

    return;
}


void MICRODRIVERS__MODIF___spin_lock_init
  (spinlock_t *lock, const char *name, struct lock_class_key *key) {
  return;
}


struct resource * MICRODRIVERS__MODIF___request_region
    (struct resource * a, resource_size_t start,
     resource_size_t n, const char *name) {
    return 0;
}


int MICRODRIVERS__MODIF_pci_enable_device
    (struct pci_dev *dev) {

    return 0;
}


void MICRODRIVERS__MODIF_pci_unregister_driver
    (struct pci_driver *drv) {
    MICRODRIVERS__DUMMY((void *) &drv->driver);
    return;
}


void MICRODRIVERS__MODIF__spin_unlock
    (spinlock_t *lock) {
    return;
}


void MICRODRIVERS__MODIF__spin_lock
    (spinlock_t *lock) {
    return;
}

void MICRODRIVERS__MODIF__spin_lock_bh
    (spinlock_t *lock) {
    return;
}


int MICRODRIVERS__MODIF___pci_register_driver
    (struct pci_driver *drv, struct module *owner) {
    MICRODRIVERS__DUMMY((void *) drv->driver.name);
    MICRODRIVERS__DUMMY((void *) drv->name);
    MICRODRIVERS__DUMMY((void *) drv->driver.bus);
    MICRODRIVERS__DUMMY((void *) drv->driver.owner);
    MICRODRIVERS__DUMMY((void *) &drv->dynids.lock);
    MICRODRIVERS__DUMMY((void *) &drv->dynids.list);
    MICRODRIVERS__DUMMY((void *) &drv->driver);
    MICRODRIVERS__DUMMY((void *) drv->probe);

    return 0;
}


int MICRODRIVERS__MODIF_request_irq
    (unsigned int a,
     irqreturn_t (*handler)(int, void *, struct pt_regs *),
     unsigned long b, const char * c, void * d) {
    return 0;
}


void MICRODRIVERS__MODIF__spin_unlock_bh
  (spinlock_t *lock) {
  return;
}


void MICRODRIVERS__MODIF__spin_lock_irq (spinlock_t *lock) {
    return;
}


void MICRODRIVERS__MODIF_mutex_unlock (struct mutex *lock) {
    return;
}


void MICRODRIVERS__MODIF_mutex_lock (struct mutex *lock) {
    return;
}


void MICRODRIVERS__MODIF__spin_unlock_irq (spinlock_t *lock) {
    return;
}

void MICRODRIVERS__MODIF___wake_up (wait_queue_head_t *q, unsigned int mode,
                            int nr_exclusive, void *key) {
    return;
}

void MICRODRIVERS__MODIF_pci_release_regions (struct pci_dev *pdev) {
    return;
}

void MICRODRIVERS__MODIF_free_irq (unsigned int irq, void *dev_id) {
    return;
}

int MICRODRIVERS__MODIF_pci_set_power_state (struct pci_dev *dev, pci_power_t state) {
    return 0;
}

int MICRODRIVERS__MODIF_pci_save_state (struct pci_dev *dev) {
    return 0;
}

int MICRODRIVERS__MODIF_pci_restore_state (struct pci_dev *dev) {
    return 0;
}

int MICRODRIVERS__MODIF_pci_bus_read_config_byte (struct pci_bus *bus,
                                          unsigned int devfn,
                                          int where,
                                          u8 *val) {
    return 0;
}

void MICRODRIVERS__MODIF___mutex_init (struct mutex *lock, const char *name, struct lock_class_key *key) {
    return;
}

int MICRODRIVERS__MODIF_pci_bus_read_config_word (struct pci_bus *bus,
                                          unsigned int devfn,
                                          int where,
                                          u16 *val) {
    return 0;
}

int MICRODRIVERS__MODIF_pci_request_regions (struct pci_dev *pdev,
                                     const char *res_name) {
    return 0;
}

int MICRODRIVERS__MODIF_device_create_file(struct device *dev, struct device_attribute *attr) {
    MICRODRIVERS__DUMMY((void *) attr->attr.mode);
    MICRODRIVERS__DUMMY((void *) attr->attr.name);

    return 0;
}

int MICRODRIVERS__MODIF_hrtimer_get_res(const clockid_t which_clock, struct timespec *tp) {
    MICRODRIVERS__DUMMY((void *) tp->tv_sec);
    MICRODRIVERS__DUMMY((void *) tp->tv_nsec);
    return 0;
}

struct proc_dir_entry *MICRODRIVERS__MODIF_create_proc_entry(char const *name, mode_t mode, struct proc_dir_entry *parent) {
    MICRODRIVERS__DUMMY((void *) name);
    MICRODRIVERS__DUMMY((void *) mode);
    MICRODRIVERS__DUMMY((void *) parent);
    MICRODRIVERS__DUMMY((void *) parent->name);
    return ((void *)0);
}

struct proc_dir_entry *MICRODRIVERS__MODIF_proc_symlink(const char *name,
                                                struct proc_dir_entry *parent, const char *dest) {
    MICRODRIVERS__DUMMY((void *) parent->name);
    return ((void *)0);
}

int MICRODRIVERS__MODIF_logWrite (void *addr , char *what, char *where, char *file , int line ) {
 return 0;
}

int MICRODRIVERS__MODIF_logRead (void *addr , char *what, char *where, char *file , int line ) {
 return 0;
}

int MICRODRIVERS__MODIF_logStackFrame (char *func ) {
 return 0;
}

int MICRODRIVERS__MODIF_logAlloc (void *addr , int size , char *fn, char *file , char *allocfn, int line ) {
 return 0;
}
# 56 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c" 2
# 69 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
static struct pci_dev * ens1371_dev = ((void *)0);

static const char __mod_author71[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "author" "=" "Jaroslav Kysela <perex@perex.cz>, Thomas Sailer <sailer@ife.ee.ethz.ch>";
static const char __mod_license72[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "license" "=" "GPL";






static const char __mod_description79[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "description" "=" "Ensoniq/Creative AudioPCI ES1371+";





                     ;






static int index[32] = { [0 ... (32 -1)] = -1 };
static char *id[32] = { [0 ... (32 -1)] = ((void *)0) };
static int enable[32] = { [0 ... (32 -1)] = 1 };


static int joystick_port[32];





static int spdif[32];
static int lineio[32];


static const struct kparam_array __param_arr_index = { (sizeof(index) / sizeof((index)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(index), typeof(&index[0])))]) - 1)), ((void *)0), param_set_int, param_get_int, sizeof(index[0]), index }; static int __param_perm_check_index __attribute__((unused)) = (sizeof(char[1 - 2 * !!((0444) < 0 || (0444) > 0777 || ((0444) & 2))]) - 1) + (sizeof(char[1 - 2 * !!(sizeof("") > (64 - sizeof(unsigned long)))]) - 1); static const char __param_str_index[] = "index"; static struct kernel_param const __param_index __attribute__((__used__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_index, 0444, param_array_set, param_array_get, { .arr = &__param_arr_index } }; static const char __mod_indextype107[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parmtype" "=" "index" ":" "array of " "int";
static const char __mod_index108[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parm" "=" "index" ":" "Index value for Ensoniq AudioPCI soundcard.";
static const struct kparam_array __param_arr_id = { (sizeof(id) / sizeof((id)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(id), typeof(&id[0])))]) - 1)), ((void *)0), param_set_charp, param_get_charp, sizeof(id[0]), id }; static int __param_perm_check_id __attribute__((unused)) = (sizeof(char[1 - 2 * !!((0444) < 0 || (0444) > 0777 || ((0444) & 2))]) - 1) + (sizeof(char[1 - 2 * !!(sizeof("") > (64 - sizeof(unsigned long)))]) - 1); static const char __param_str_id[] = "id"; static struct kernel_param const __param_id __attribute__((__used__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_id, 0444, param_array_set, param_array_get, { .arr = &__param_arr_id } }; static const char __mod_idtype109[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parmtype" "=" "id" ":" "array of " "charp";
static const char __mod_id110[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parm" "=" "id" ":" "ID string for Ensoniq AudioPCI soundcard.";
static const struct kparam_array __param_arr_enable = { (sizeof(enable) / sizeof((enable)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(enable), typeof(&enable[0])))]) - 1)), ((void *)0), param_set_bool, param_get_bool, sizeof(enable[0]), enable }; static int __param_perm_check_enable __attribute__((unused)) = (sizeof(char[1 - 2 * !!((0444) < 0 || (0444) > 0777 || ((0444) & 2))]) - 1) + (sizeof(char[1 - 2 * !!(sizeof("") > (64 - sizeof(unsigned long)))]) - 1); static const char __param_str_enable[] = "enable"; static struct kernel_param const __param_enable __attribute__((__used__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_enable, 0444, param_array_set, param_array_get, { .arr = &__param_arr_enable } }; static const char __mod_enabletype111[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parmtype" "=" "enable" ":" "array of " "bool";
static const char __mod_enable112[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parm" "=" "enable" ":" "Enable Ensoniq AudioPCI soundcard.";


static const struct kparam_array __param_arr_joystick_port = { (sizeof(joystick_port) / sizeof((joystick_port)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(joystick_port), typeof(&joystick_port[0])))]) - 1)), ((void *)0), param_set_int, param_get_int, sizeof(joystick_port[0]), joystick_port }; static int __param_perm_check_joystick_port __attribute__((unused)) = (sizeof(char[1 - 2 * !!((0444) < 0 || (0444) > 0777 || ((0444) & 2))]) - 1) + (sizeof(char[1 - 2 * !!(sizeof("") > (64 - sizeof(unsigned long)))]) - 1); static const char __param_str_joystick_port[] = "joystick_port"; static struct kernel_param const __param_joystick_port __attribute__((__used__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_joystick_port, 0444, param_array_set, param_array_get, { .arr = &__param_arr_joystick_port } }; static const char __mod_joystick_porttype115[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parmtype" "=" "joystick_port" ":" "array of " "int";
static const char __mod_joystick_port116[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parm" "=" "joystick_port" ":" "Joystick port address.";






static const struct kparam_array __param_arr_spdif = { (sizeof(spdif) / sizeof((spdif)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(spdif), typeof(&spdif[0])))]) - 1)), ((void *)0), param_set_int, param_get_int, sizeof(spdif[0]), spdif }; static int __param_perm_check_spdif __attribute__((unused)) = (sizeof(char[1 - 2 * !!((0444) < 0 || (0444) > 0777 || ((0444) & 2))]) - 1) + (sizeof(char[1 - 2 * !!(sizeof("") > (64 - sizeof(unsigned long)))]) - 1); static const char __param_str_spdif[] = "spdif"; static struct kernel_param const __param_spdif __attribute__((__used__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_spdif, 0444, param_array_set, param_array_get, { .arr = &__param_arr_spdif } }; static const char __mod_spdiftype123[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parmtype" "=" "spdif" ":" "array of " "int";
static const char __mod_spdif124[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parm" "=" "spdif" ":" "S/PDIF output (-1 = none, 0 = auto, 1 = force).";
static const struct kparam_array __param_arr_lineio = { (sizeof(lineio) / sizeof((lineio)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(lineio), typeof(&lineio[0])))]) - 1)), ((void *)0), param_set_int, param_get_int, sizeof(lineio[0]), lineio }; static int __param_perm_check_lineio __attribute__((unused)) = (sizeof(char[1 - 2 * !!((0444) < 0 || (0444) > 0777 || ((0444) & 2))]) - 1) + (sizeof(char[1 - 2 * !!(sizeof("") > (64 - sizeof(unsigned long)))]) - 1); static const char __param_str_lineio[] = "lineio"; static struct kernel_param const __param_lineio __attribute__((__used__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_lineio, 0444, param_array_set, param_array_get, { .arr = &__param_arr_lineio } }; static const char __mod_lineiotype125[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parmtype" "=" "lineio" ":" "array of " "int";
static const char __mod_lineio126[] __attribute__((__used__)) __attribute__((section(".modinfo"),unused)) = "parm" "=" "lineio" ":" "Line In to Rear Out (0 = auto, 1 = force).";
# 389 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
struct ensoniq {
 spinlock_t * __attribute__((noderef, address_space(2))) reg_lock;
 struct mutex * __attribute__((noderef, address_space(2))) src_mutex;

 int irq;

 unsigned long playback1size;
 unsigned long playback2size;
 unsigned long capture3size;

 unsigned long port;
 unsigned int mode;
 unsigned int uartm;

 unsigned int ctrl;
 unsigned int sctrl;
 unsigned int cssr;
 unsigned int uartc;
 unsigned int rev;

 union {

  struct {
   struct snd_ac97 *__attribute__((recursive))ac97;
  } __attribute__((recursive)) es1371;






 } u;

 struct pci_dev *pci;
 struct snd_card *__attribute__((recursive)) card;
 struct snd_pcm *__attribute__((recursive)) pcm1;
 struct snd_pcm *__attribute__((recursive)) pcm2;
 struct snd_pcm_substream *__attribute__((recursive)) playback1_substream;
 struct snd_pcm_substream *__attribute__((recursive)) playback2_substream;
 struct snd_pcm_substream *__attribute__((recursive)) capture_substream;
 unsigned int p1_dma_size;
 unsigned int p2_dma_size;
 unsigned int c_dma_size;
 unsigned int p1_period_size;
 unsigned int p2_period_size;
 unsigned int c_period_size;
 struct snd_rawmidi *__attribute__((recursive)) rmidi;
 struct snd_rawmidi_substream *midi_input;
 struct snd_rawmidi_substream *midi_output;

 unsigned int spdif;
 unsigned int spdif_default;
 unsigned int spdif_stream;






 struct gameport *gameport;

};

static irqreturn_t snd_audiopci_interrupt(int irq, void *dev_id);

static struct pci_device_id snd_audiopci_ids[] = {




 { 0x1274, 0x1371, (~0), (~0), 0, 0, 0, },
 { 0x1274, 0x5880, (~0), (~0), 0, 0, 0, },
 { 0x1102, 0x8938, (~0), (~0), 0, 0, 0, },

 { 0, }
};

extern const struct pci_device_id __mod_pci_device_table __attribute__ ((unused, alias("snd_audiopci_ids")));
# 493 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
static struct snd_ratden es1371_dac_clock = {
 .num_min = 3000 * (1 << 15),
 .num_max = 48000 * (1 << 15),
 .num_step = 3000,
 .den = 1 << 15,
};
static struct snd_pcm_hw_constraint_ratdens snd_es1371_hw_constraints_dac_clock = {
 .nrats = 1,
 .rats = &es1371_dac_clock,
};
static struct snd_ratnum es1371_adc_clock = {
 .num = 48000 << 15,
 .den_min = 32768,
 .den_max = 393216,
 .den_step = 1,
};
static struct snd_pcm_hw_constraint_ratnums snd_es1371_hw_constraints_adc_clock = {
 .nrats = 1,
 .rats = &es1371_adc_clock,
};

static const unsigned int snd_ensoniq_sample_shift[] =
 {0, 1, 1, 2};







static unsigned int snd_es1371_wait_src_ready(struct ensoniq * ensoniq)
{
 unsigned int t, r = 0;

 for (t = 0; t < 0xa000; t++) {
  r = inl(((ensoniq)->port + 0x10));
  if ((r & (1<<23)) == 0)
   return r;
  cond_resched();
 }
 printk("<3>" "wait src ready timeout 0x%lx [0x%x]\n" , ((ensoniq)->port +
 0x10), r);
 return 0;
}

static unsigned int snd_es1371_src_read(struct ensoniq * ensoniq, unsigned short reg)
{
 unsigned int temp, i, orig, r;


 temp = orig = snd_es1371_wait_src_ready(ensoniq);


 r = temp & ((1<<22) | (1<<21) |
      (1<<20) | (1<<19));
 r |= (((reg)&0x7f)<<25) | 0x10000;
 outl(r, ((ensoniq)->port + 0x10));


 temp = snd_es1371_wait_src_ready(ensoniq);

 if ((temp & 0x00870000) != 0x00010000) {

  for (i = 0; i < 0xa000; i++) {
   temp = inl(((ensoniq)->port + 0x10));
   if ((temp & 0x00870000) == 0x00010000)
    break;
  }
 }


 r = orig & ((1<<22) | (1<<21) |
     (1<<20) | (1<<19));
 r |= (((reg)&0x7f)<<25);
 outl(r, ((ensoniq)->port + 0x10));

 return temp;
}

static void snd_es1371_src_write(struct ensoniq * ensoniq,
     unsigned short reg, unsigned short data)
{
 unsigned int r;

 r = snd_es1371_wait_src_ready(ensoniq) &
     ((1<<22) | (1<<21) |
      (1<<20) | (1<<19));
 r |= (((reg)&0x7f)<<25) | (((data)&0xffff)<<0);
 outl(r | (1<<24), ((ensoniq)->port + 0x10));
}
# 613 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
static void snd_es1371_codec_write(struct snd_ac97 *ac97,
       unsigned short reg, unsigned short val)
{
 struct ensoniq *ensoniq = ac97->private_data;
 unsigned int t, x;

 mutex_lock_nested(ensoniq->src_mutex, 0);
 for (t = 0; t < 0xa000; t++) {
  if (!(inl(((ensoniq)->port + 0x14)) & (1<<30))) {

   x = snd_es1371_wait_src_ready(ensoniq);
   outl((x & ((1<<22) | (1<<21) |
              (1<<20) | (1<<19))) | 0x00010000,
        ((ensoniq)->port + 0x10));


   for (t = 0; t < 0xa000; t++) {
    if ((inl(((ensoniq)->port + 0x10)) & 0x00870000) ==
        0x00000000)
     break;
   }

   for (t = 0; t < 0xa000; t++) {
    if ((inl(((ensoniq)->port + 0x10)) & 0x00870000) ==
        0x00010000)
     break;
   }
   outl(((((reg)&0x7f)<<16)|(((val)&0xffff)<<0)), ((ensoniq)->port + 0x14));

   snd_es1371_wait_src_ready(ensoniq);
   outl(x, ((ensoniq)->port + 0x10));
   mutex_unlock(ensoniq->src_mutex);
   return;
  }
 }
 mutex_unlock(ensoniq->src_mutex);
 printk("<3>" "codec write timeout at 0x%lx [0x%x]\n" , ((ensoniq)->port +
 0x14), inl(((ensoniq)->port + 0x14)));
}

static unsigned short snd_es1371_codec_read(struct snd_ac97 *ac97,
         unsigned short reg)
{
 struct ensoniq *ensoniq = ac97->private_data;
 unsigned int t, x, fail = 0;

      __again:
 mutex_lock_nested(ensoniq->src_mutex, 0);
 for (t = 0; t < 0xa000; t++) {
  if (!(inl(((ensoniq)->port + 0x14)) & (1<<30))) {

   x = snd_es1371_wait_src_ready(ensoniq);
   outl((x & ((1<<22) | (1<<21) |
              (1<<20) | (1<<19))) | 0x00010000,
        ((ensoniq)->port + 0x10));


   for (t = 0; t < 0xa000; t++) {
    if ((inl(((ensoniq)->port + 0x10)) & 0x00870000) ==
        0x00000000)
     break;
   }

   for (t = 0; t < 0xa000; t++) {
    if ((inl(((ensoniq)->port + 0x10)) & 0x00870000) ==
        0x00010000)
     break;
   }
   outl(((((reg)&0x7f)<<16)|(1<<23)), ((ensoniq)->port + 0x14));

   snd_es1371_wait_src_ready(ensoniq);
   outl(x, ((ensoniq)->port + 0x10));

   for (t = 0; t < 0xa000; t++) {
    if (!(inl(((ensoniq)->port + 0x14)) & (1<<30)))
     break;
   }

   for (t = 0; t < 0xa000; t++) {
    if ((x = inl(((ensoniq)->port + 0x14))) & (1<<31)) {
     mutex_unlock(ensoniq->src_mutex);
     return (((x)>>0)&0xffff);
    }
   }
   mutex_unlock(ensoniq->src_mutex);
   if (++fail > 10) {
    printk("<3>" "codec read timeout (final) " "at 0x%lx, reg = 0x%x [0x%x]\n" , ((ensoniq)->port +


 0x14), reg, inl(((ensoniq)->port + 0x14)));
    return 0;
   }
   goto __again;
  }
 }
 mutex_unlock(ensoniq->src_mutex);
 printk("<3>" "es1371: codec read timeout at 0x%lx [0x%x]\n" , ((ensoniq)->port +
 0x14), inl(((ensoniq)->port + 0x14)));
 return 0;
}

static void snd_es1371_codec_wait(struct snd_ac97 *ac97)
{

 (__builtin_constant_p(750) ? ((750) > 20000 ? __bad_udelay() : __const_udelay((750) * 0x10c7ul)) : __udelay(750));
 snd_es1371_codec_read(ac97, 0x00);
 snd_es1371_codec_read(ac97, 0x7c);
 snd_es1371_codec_read(ac97, 0x7e);
 (__builtin_constant_p(750) ? ((750) > 20000 ? __bad_udelay() : __const_udelay((750) * 0x10c7ul)) : __udelay(750));

}

static void snd_es1371_adc_rate(struct ensoniq * ensoniq, unsigned int rate)
{
 unsigned int n, truncm, freq, result;
 unsigned int ret = 0;

spin:
 ret = mutex_trylock(ensoniq->src_mutex);
 if (ret != 0) {

          } else {
    goto spin;
  }



 n = rate / 3000;
 if ((1 << n) & ((1 << 15) | (1 << 13) | (1 << 11) | (1 << 9)))
  n--;
 truncm = (21 * n - 1) | 1;
 freq = ((48000UL << 15) / rate) * n;
 result = (48000UL << 15) / (freq / n);
 if (rate >= 24000) {
  if (truncm > 239)
   truncm = 239;
  snd_es1371_src_write(ensoniq, 0x78 + 0x00,
    (((239 - truncm) >> 1) << 9) | (n << 4));
 } else {
  if (truncm > 119)
   truncm = 119;
  snd_es1371_src_write(ensoniq, 0x78 + 0x00,
    0x8000 | (((119 - truncm) >> 1) << 9) | (n << 4));
 }
 snd_es1371_src_write(ensoniq, 0x78 + 0x01,
        (snd_es1371_src_read(ensoniq, 0x78 +
        0x01) & 0x00ff) |
        ((freq >> 5) & 0xfc00));
 snd_es1371_src_write(ensoniq, 0x78 + 0x03, freq & 0x7fff);
 snd_es1371_src_write(ensoniq, 0x6c, n << 8);
 snd_es1371_src_write(ensoniq, 0x6c + 1, n << 8);
 mutex_unlock(ensoniq->src_mutex);
}

static void snd_es1371_dac1_rate(struct ensoniq * ensoniq, unsigned int rate)
{
 unsigned int freq, r;

 mutex_lock_nested(ensoniq->src_mutex, 0);
 freq = ((rate << 15) + 1500) / 3000;
 r = (snd_es1371_wait_src_ready(ensoniq) & ((1<<22) |
         (1<<20) | (1<<19))) |
  (1<<21);
 outl(r, ((ensoniq)->port + 0x10));
 snd_es1371_src_write(ensoniq, 0x70 + 0x01,
        (snd_es1371_src_read(ensoniq, 0x70 +
        0x01) & 0x00ff) |
        ((freq >> 5) & 0xfc00));
 snd_es1371_src_write(ensoniq, 0x70 + 0x03, freq & 0x7fff);
 r = (snd_es1371_wait_src_ready(ensoniq) & ((1<<22) |
         (1<<20) | (1<<19)));
 outl(r, ((ensoniq)->port + 0x10));
 mutex_unlock(ensoniq->src_mutex);
}

static void snd_es1371_dac2_rate(struct ensoniq * ensoniq, unsigned int rate)
{
 unsigned int freq, r;

 mutex_lock_nested(ensoniq->src_mutex, 0);
 freq = ((rate << 15) + 1500) / 3000;
 r = (snd_es1371_wait_src_ready(ensoniq) & ((1<<22) |
         (1<<21) | (1<<19))) |
  (1<<20);
 outl(r, ((ensoniq)->port + 0x10));
 snd_es1371_src_write(ensoniq, 0x74 + 0x01,
        (snd_es1371_src_read(ensoniq, 0x74 +
        0x01) & 0x00ff) |
        ((freq >> 5) & 0xfc00));
 snd_es1371_src_write(ensoniq, 0x74 + 0x03,
        freq & 0x7fff);
 r = (snd_es1371_wait_src_ready(ensoniq) & ((1<<22) |
         (1<<21) | (1<<19)));
 outl(r, ((ensoniq)->port + 0x10));
 mutex_unlock(ensoniq->src_mutex);
}



static int snd_ensoniq_trigger(struct snd_pcm_substream *substream, int cmd)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 switch (cmd) {
 case 3:
 case 4:
 {
  unsigned int what = 0;
  struct snd_pcm_substream *s;
  for (s = ({ const typeof( ((typeof(*s) *)0)->link_list ) *__mptr = ((&substream->group->substreams)->next); (typeof(*s) *)( (char *)__mptr - __builtin_offsetof(typeof(*s),link_list) );}); __builtin_prefetch(s->link_list.next), &s->link_list != (&substream->group->substreams); s = ({ const typeof( ((typeof(*s) *)0)->link_list ) *__mptr = (s->link_list.next); (typeof(*s) *)( (char *)__mptr - __builtin_offsetof(typeof(*s),link_list) );})) {
   if (s == ensoniq->playback1_substream) {
    what |= (1<<11);
    snd_pcm_trigger_done(s, substream);
   } else if (s == ensoniq->playback2_substream) {
    what |= (1<<12);
    snd_pcm_trigger_done(s, substream);
   } else if (s == ensoniq->capture_substream)
    return -22;
  }
  _spin_lock(ensoniq->reg_lock);
  if (cmd == 3)
   ensoniq->sctrl |= what;
  else
   ensoniq->sctrl &= ~what;
  outl(ensoniq->sctrl, ((ensoniq)->port + 0x20));
  _spin_unlock(ensoniq->reg_lock);
  break;
 }
 case 1:
 case 0:
 {
  unsigned int what = 0;
  struct snd_pcm_substream *s;
  for (s = ({ const typeof( ((typeof(*s) *)0)->link_list ) *__mptr = ((&substream->group->substreams)->next); (typeof(*s) *)( (char *)__mptr - __builtin_offsetof(typeof(*s),link_list) );}); __builtin_prefetch(s->link_list.next), &s->link_list != (&substream->group->substreams); s = ({ const typeof( ((typeof(*s) *)0)->link_list ) *__mptr = (s->link_list.next); (typeof(*s) *)( (char *)__mptr - __builtin_offsetof(typeof(*s),link_list) );})) {
   if (s == ensoniq->playback1_substream) {
    what |= (1<<6);
    snd_pcm_trigger_done(s, substream);
   } else if (s == ensoniq->playback2_substream) {
    what |= (1<<5);
    snd_pcm_trigger_done(s, substream);
   } else if (s == ensoniq->capture_substream) {
    what |= (1<<4);
    snd_pcm_trigger_done(s, substream);
   }
  }
  _spin_lock(ensoniq->reg_lock);
  if (cmd == 1)
   ensoniq->ctrl |= what;
  else
   ensoniq->ctrl &= ~what;
  outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
  _spin_unlock(ensoniq->reg_lock);
  break;
 }
 default:
  return -22;
 }
 return 0;
}





static int snd_ensoniq_hw_params(struct snd_pcm_substream *substream,
     struct snd_pcm_hw_params *hw_params)
{
 return snd_pcm_lib_malloc_pages(substream, hw_param_interval((hw_params), 18)->min);
}

static int snd_ensoniq_hw_free(struct snd_pcm_substream *substream)
{
 return snd_pcm_lib_free_pages(substream);
}

static int snd_ensoniq_playback1_prepare(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 struct snd_pcm_runtime *runtime = substream->runtime;
 unsigned int mode = 0;

 ensoniq->p1_dma_size = snd_pcm_lib_buffer_bytes(substream);
 ensoniq->p1_period_size = snd_pcm_lib_period_bytes(substream);
 if (snd_pcm_format_width(runtime->format) == 16)
  mode |= 0x02;
 if (runtime->channels > 1)
  mode |= 0x01;
 _spin_lock_irq(ensoniq->reg_lock);
 ensoniq->ctrl &= ~(1<<6);


 if (runtime->rate == 48000)
  ensoniq->ctrl |= (1<<31);
 else
  ensoniq->ctrl &= ~(1<<31);

 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 outl((((0x0c)&0x0f)<<0), ((ensoniq)->port + 0x0c));
 outl(runtime->dma_addr, ((ensoniq)->port + 0x30));
 outl((ensoniq->p1_dma_size >> 2) - 1, ((ensoniq)->port + 0x34));
 ensoniq->sctrl &= ~((1<<13) | (1<<11) | (1<<7) | (0x03<<0));
 ensoniq->sctrl |= (1<<8) | (((mode)&0x03)<<0);
 outl(ensoniq->sctrl, ((ensoniq)->port + 0x20));
 outl((ensoniq->p1_period_size >> snd_ensoniq_sample_shift[mode]) - 1,
      ((ensoniq)->port + 0x24));
# 927 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 _spin_unlock_irq(ensoniq->reg_lock);

 snd_es1371_dac1_rate(ensoniq, runtime->rate);

 return 0;
}

static int snd_ensoniq_playback2_prepare(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 struct snd_pcm_runtime *runtime = substream->runtime;
 unsigned int mode = 0;

 ensoniq->p2_dma_size = snd_pcm_lib_buffer_bytes(substream);
 ensoniq->p2_period_size = snd_pcm_lib_period_bytes(substream);
 if (snd_pcm_format_width(runtime->format) == 16)
  mode |= 0x02;
 if (runtime->channels > 1)
  mode |= 0x01;
 _spin_lock_irq(ensoniq->reg_lock);
 ensoniq->ctrl &= ~(1<<5);
 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 outl((((0x0c)&0x0f)<<0), ((ensoniq)->port + 0x0c));
 outl(runtime->dma_addr, ((ensoniq)->port + 0x38));
 outl((ensoniq->p2_dma_size >> 2) - 1, ((ensoniq)->port + 0x3c));
 ensoniq->sctrl &= ~((1<<14) | (1<<12) | (1<<6) |
       (0x07<<19) | (0x07<<16) | (0x03<<2));
 ensoniq->sctrl |= (1<<9) | (((mode)&0x03)<<2) |
     (((mode & 2 ? 2 : 1)&0x07)<<19) | (((0)&0x07)<<16);
 outl(ensoniq->sctrl, ((ensoniq)->port + 0x20));
 outl((ensoniq->p2_period_size >> snd_ensoniq_sample_shift[mode]) - 1,
      ((ensoniq)->port + 0x28));







 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 _spin_unlock_irq(ensoniq->reg_lock);

 snd_es1371_dac2_rate(ensoniq, runtime->rate);

 return 0;
}

static int snd_ensoniq_capture_prepare(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 struct snd_pcm_runtime *runtime = substream->runtime;
 unsigned int mode = 0;

 ensoniq->c_dma_size = snd_pcm_lib_buffer_bytes(substream);
 ensoniq->c_period_size = snd_pcm_lib_period_bytes(substream);
 if (snd_pcm_format_width(runtime->format) == 16)
  mode |= 0x02;
 if (runtime->channels > 1)
  mode |= 0x01;
 _spin_lock_irq(ensoniq->reg_lock);
 ensoniq->ctrl &= ~(1<<4);
 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 outl((((0x0d)&0x0f)<<0), ((ensoniq)->port + 0x0c));
 outl(runtime->dma_addr, ((ensoniq)->port + 0x30));
 outl((ensoniq->c_dma_size >> 2) - 1, ((ensoniq)->port + 0x34));
 ensoniq->sctrl &= ~((1<<15) | (0x03<<4));
 ensoniq->sctrl |= (1<<10) | (((mode)&0x03)<<4);
 outl(ensoniq->sctrl, ((ensoniq)->port + 0x20));
 outl((ensoniq->c_period_size >> snd_ensoniq_sample_shift[mode]) - 1,
      ((ensoniq)->port + 0x2c));







 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 _spin_unlock_irq(ensoniq->reg_lock);

 snd_es1371_adc_rate(ensoniq, runtime->rate);

 return 0;
}

static snd_pcm_uframes_t snd_ensoniq_playback1_pointer(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 size_t ptr;

 _spin_lock(ensoniq->reg_lock);
 if (inl(((ensoniq)->port + 0x00)) & (1<<6)) {
  outl((((0x0c)&0x0f)<<0), ((ensoniq)->port + 0x0c));
  ptr = (((inl(((ensoniq)->port + 0x34)))>>14)&0x3fffc);
  ptr = bytes_to_frames(substream->runtime, ptr);
 } else {
  ptr = 0;
 }
 _spin_unlock(ensoniq->reg_lock);
 return ptr;
}

static snd_pcm_uframes_t snd_ensoniq_playback2_pointer(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 size_t ptr;

 _spin_lock(ensoniq->reg_lock);
 if (inl(((ensoniq)->port + 0x00)) & (1<<5)) {
  outl((((0x0c)&0x0f)<<0), ((ensoniq)->port + 0x0c));
  ptr = (((inl(((ensoniq)->port + 0x3c)))>>14)&0x3fffc);
  ptr = bytes_to_frames(substream->runtime, ptr);
 } else {
  ptr = 0;
 }
 _spin_unlock(ensoniq->reg_lock);
 return ptr;
}

static snd_pcm_uframes_t snd_ensoniq_capture_pointer(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 size_t ptr;

 _spin_lock(ensoniq->reg_lock);
 if (inl(((ensoniq)->port + 0x00)) & (1<<4)) {
  outl((((0x0d)&0x0f)<<0), ((ensoniq)->port + 0x0c));
  ptr = (((inl(((ensoniq)->port + 0x34)))>>14)&0x3fffc);
  ptr = bytes_to_frames(substream->runtime, ptr);
 } else {
  ptr = 0;
 }
 _spin_unlock(ensoniq->reg_lock);
 return ptr;
}

static struct snd_pcm_hardware snd_ensoniq_playback1 =
{
 .info = (0x00000001 | 0x00000100 |
     0x00010000 |
     0x00000002 |
     0x00080000 | 0x00400000),
 .formats = (1ULL << (( snd_pcm_format_t) 1)) | (1ULL << (( snd_pcm_format_t) 2)),
 .rates =

    (1<<30) | (((1<<1)|(1<<2)| (1<<3)|(1<<4)| (1<<5)|(1<<6))|(1<<7)),





 .rate_min = 4000,
 .rate_max = 48000,
 .channels_min = 1,
 .channels_max = 2,
 .buffer_bytes_max = (128*1024),
 .period_bytes_min = 64,
 .period_bytes_max = (128*1024),
 .periods_min = 1,
 .periods_max = 1024,
 .fifo_size = 0,
};

static struct snd_pcm_hardware snd_ensoniq_playback2 =
{
 .info = (0x00000001 | 0x00000100 |
     0x00010000 |
     0x00000002 | 0x00080000 |
     0x00400000),
 .formats = (1ULL << (( snd_pcm_format_t) 1)) | (1ULL << (( snd_pcm_format_t) 2)),
 .rates = (1<<30) | (((1<<1)|(1<<2)| (1<<3)|(1<<4)| (1<<5)|(1<<6))|(1<<7)),
 .rate_min = 4000,
 .rate_max = 48000,
 .channels_min = 1,
 .channels_max = 2,
 .buffer_bytes_max = (128*1024),
 .period_bytes_min = 64,
 .period_bytes_max = (128*1024),
 .periods_min = 1,
 .periods_max = 1024,
 .fifo_size = 0,
};

static struct snd_pcm_hardware snd_ensoniq_capture =
{
 .info = (0x00000001 | 0x00000100 |
     0x00010000 |
     0x00000002 | 0x00400000),
 .formats = (1ULL << (( snd_pcm_format_t) 1)) | (1ULL << (( snd_pcm_format_t) 2)),
 .rates = (1<<30) | (((1<<1)|(1<<2)| (1<<3)|(1<<4)| (1<<5)|(1<<6))|(1<<7)),
 .rate_min = 4000,
 .rate_max = 48000,
 .channels_min = 1,
 .channels_max = 2,
 .buffer_bytes_max = (128*1024),
 .period_bytes_min = 64,
 .period_bytes_max = (128*1024),
 .periods_min = 1,
 .periods_max = 1024,
 .fifo_size = 0,
};

static int snd_ensoniq_playback1_open(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 struct snd_pcm_runtime *runtime = substream->runtime;

 ensoniq->mode |= 0x0001;
 ensoniq->playback1_substream = substream;
 runtime->hw = snd_ensoniq_playback1;
 snd_pcm_set_sync(substream);
 _spin_lock_irq(ensoniq->reg_lock);
 if (ensoniq->spdif && ensoniq->playback2_substream == ((void *)0))
  ensoniq->spdif_stream = ensoniq->spdif_default;
 _spin_unlock_irq(ensoniq->reg_lock);




 snd_pcm_hw_constraint_ratdens(runtime, 0, 11,
          &snd_es1371_hw_constraints_dac_clock);

 return 0;
}

static int snd_ensoniq_playback2_open(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 struct snd_pcm_runtime *runtime = substream->runtime;

 ensoniq->mode |= 0x0002;
 ensoniq->playback2_substream = substream;
 runtime->hw = snd_ensoniq_playback2;
 snd_pcm_set_sync(substream);
 _spin_lock_irq(ensoniq->reg_lock);
 if (ensoniq->spdif && ensoniq->playback1_substream == ((void *)0))
  ensoniq->spdif_stream = ensoniq->spdif_default;
 _spin_unlock_irq(ensoniq->reg_lock);




 snd_pcm_hw_constraint_ratdens(runtime, 0, 11,
          &snd_es1371_hw_constraints_dac_clock);

 return 0;
}

static int snd_ensoniq_capture_open(struct snd_pcm_substream *substream)
{
 struct ensoniq *ensoniq = ((substream)->private_data);
 struct snd_pcm_runtime *runtime = substream->runtime;

 ensoniq->mode |= 0x0004;
 ensoniq->capture_substream = substream;
 runtime->hw = snd_ensoniq_capture;
 snd_pcm_set_sync(substream);




 snd_pcm_hw_constraint_ratnums(runtime, 0, 11,
          &snd_es1371_hw_constraints_adc_clock);

 return 0;
}

static int snd_ensoniq_playback1_close(struct snd_pcm_substream *substream) __attribute((isolate))
{

 struct ensoniq *ensoniq = substream->private_data;

 ensoniq->playback1_substream = ((void *)0);
 ensoniq->mode &= ~0x0001;
 return 0;
}

static int snd_ensoniq_playback2_close(struct snd_pcm_substream *substream)
{

 struct ensoniq *ensoniq = substream->private_data;

 ensoniq->playback2_substream = ((void *)0);
 _spin_lock_irq(ensoniq->reg_lock);



 ensoniq->mode &= ~0x0002;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_ensoniq_capture_close(struct snd_pcm_substream *substream)
{

 struct ensoniq *ensoniq = substream->private_data;

 ensoniq->capture_substream = ((void *)0);
 _spin_lock_irq(ensoniq->reg_lock);



 ensoniq->mode &= ~0x0004;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static struct snd_pcm_ops snd_ensoniq_playback1_ops = {
 .open = snd_ensoniq_playback1_open,
 .close = snd_ensoniq_playback1_close,
 .ioctl = snd_pcm_lib_ioctl,
 .hw_params = snd_ensoniq_hw_params,
 .hw_free = snd_ensoniq_hw_free,
 .prepare = snd_ensoniq_playback1_prepare,
 .trigger = snd_ensoniq_trigger,
 .pointer = snd_ensoniq_playback1_pointer,
};

static struct snd_pcm_ops snd_ensoniq_playback2_ops = {
 .open = snd_ensoniq_playback2_open,
 .close = snd_ensoniq_playback2_close,
 .ioctl = snd_pcm_lib_ioctl,
 .hw_params = snd_ensoniq_hw_params,
 .hw_free = snd_ensoniq_hw_free,
 .prepare = snd_ensoniq_playback2_prepare,
 .trigger = snd_ensoniq_trigger,
 .pointer = snd_ensoniq_playback2_pointer,
};

static struct snd_pcm_ops snd_ensoniq_capture_ops = {
 .open = snd_ensoniq_capture_open,
 .close = snd_ensoniq_capture_close,
 .ioctl = snd_pcm_lib_ioctl,
 .hw_params = snd_ensoniq_hw_params,
 .hw_free = snd_ensoniq_hw_free,
 .prepare = snd_ensoniq_capture_prepare,
 .trigger = snd_ensoniq_trigger,
 .pointer = snd_ensoniq_capture_pointer,
};

static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_pcm(struct ensoniq * ensoniq, int device,
         struct snd_pcm ** rpcm)
{
 struct snd_pcm *pcm;
 int err;

 if (rpcm)
  *rpcm = ((void *)0);



 err = snd_pcm_new(ensoniq->card, "ES1371/1", device, 1, 1, &pcm);

 if (err < 0)
  return err;




 snd_pcm_set_ops(pcm, SNDRV_PCM_STREAM_PLAYBACK, &snd_ensoniq_playback1_ops);

 snd_pcm_set_ops(pcm, SNDRV_PCM_STREAM_CAPTURE, &snd_ensoniq_capture_ops);

 pcm->private_data = ensoniq;
 pcm->info_flags = 0;



 strcpy(pcm->name, "ES1371 DAC2/ADC");

 ensoniq->pcm1 = pcm;

 snd_pcm_lib_preallocate_pages_for_all(pcm, 2,
           (&(ensoniq->pci)->dev), 64*1024, 128*1024);

 if (rpcm)
  *rpcm = pcm;
 return 0;
}

static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_pcm2(struct ensoniq * ensoniq, int device,
          struct snd_pcm ** rpcm)
{
 struct snd_pcm *pcm;
 int err;

 if (rpcm)
  *rpcm = ((void *)0);



 err = snd_pcm_new(ensoniq->card, "ES1371/2", device, 1, 0, &pcm);

 if (err < 0)
  return err;




 snd_pcm_set_ops(pcm, SNDRV_PCM_STREAM_PLAYBACK, &snd_ensoniq_playback2_ops);

 pcm->private_data = ensoniq;
 pcm->info_flags = 0;



 strcpy(pcm->name, "ES1371 DAC1");

 ensoniq->pcm2 = pcm;

 snd_pcm_lib_preallocate_pages_for_all(pcm, 2,
           (&(ensoniq->pci)->dev), 64*1024, 128*1024);

 if (rpcm)
  *rpcm = pcm;
 return 0;
}
# 1354 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
static int snd_ens1373_spdif_info(struct snd_kcontrol *kcontrol,
      struct snd_ctl_elem_info *uinfo) __attribute((isolate))
{
 uinfo->type = (( snd_ctl_elem_type_t) 5);
 uinfo->count = 1;
 return 0;
}

static int snd_ens1373_spdif_default_get(struct snd_kcontrol *kcontrol,
                                         struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 _spin_lock_irq(ensoniq->reg_lock);
 ucontrol->value.iec958.status[0] = (ensoniq->spdif_default >> 0) & 0xff;
 ucontrol->value.iec958.status[1] = (ensoniq->spdif_default >> 8) & 0xff;
 ucontrol->value.iec958.status[2] = (ensoniq->spdif_default >> 16) & 0xff;
 ucontrol->value.iec958.status[3] = (ensoniq->spdif_default >> 24) & 0xff;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_ens1373_spdif_default_put(struct snd_kcontrol *kcontrol,
                                         struct snd_ctl_elem_value *ucontrol)
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 unsigned int val;
 int change;

 val = ((u32)ucontrol->value.iec958.status[0] << 0) |
       ((u32)ucontrol->value.iec958.status[1] << 8) |
       ((u32)ucontrol->value.iec958.status[2] << 16) |
       ((u32)ucontrol->value.iec958.status[3] << 24);
 _spin_lock_irq(ensoniq->reg_lock);
 change = ensoniq->spdif_default != val;
 ensoniq->spdif_default = val;
 if (change && ensoniq->playback1_substream == ((void *)0) &&
     ensoniq->playback2_substream == ((void *)0))
  outl(val, ((ensoniq)->port + 0x1c));
 _spin_unlock_irq(ensoniq->reg_lock);
 return change;
}

static int snd_ens1373_spdif_mask_get(struct snd_kcontrol *kcontrol,
          struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{
 ucontrol->value.iec958.status[0] = 0xff;
 ucontrol->value.iec958.status[1] = 0xff;
 ucontrol->value.iec958.status[2] = 0xff;
 ucontrol->value.iec958.status[3] = 0xff;
 return 0;
}

static int snd_ens1373_spdif_stream_get(struct snd_kcontrol *kcontrol,
     struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 _spin_lock_irq(ensoniq->reg_lock);
 ucontrol->value.iec958.status[0] = (ensoniq->spdif_stream >> 0) & 0xff;
 ucontrol->value.iec958.status[1] = (ensoniq->spdif_stream >> 8) & 0xff;
 ucontrol->value.iec958.status[2] = (ensoniq->spdif_stream >> 16) & 0xff;
 ucontrol->value.iec958.status[3] = (ensoniq->spdif_stream >> 24) & 0xff;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_ens1373_spdif_stream_put(struct snd_kcontrol *kcontrol,
                                        struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 unsigned int val;
 int change;

 val = ((u32)ucontrol->value.iec958.status[0] << 0) |
       ((u32)ucontrol->value.iec958.status[1] << 8) |
       ((u32)ucontrol->value.iec958.status[2] << 16) |
       ((u32)ucontrol->value.iec958.status[3] << 24);
 _spin_lock_irq(ensoniq->reg_lock);
 change = ensoniq->spdif_stream != val;
 ensoniq->spdif_stream = val;
 if (change && (ensoniq->playback1_substream != ((void *)0) ||
         ensoniq->playback2_substream != ((void *)0)))
  outl(val, ((ensoniq)->port + 0x1c));
 _spin_unlock_irq(ensoniq->reg_lock);
 return change;
}







static int snd_es1371_spdif_get(struct snd_kcontrol *kcontrol,
    struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

  struct ensoniq *ensoniq = kcontrol->private_data;
 _spin_lock_irq(ensoniq->reg_lock);
 ucontrol->value.integer.value[0] = ensoniq->ctrl & (1<<26) ? 1 : 0;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_es1371_spdif_put(struct snd_kcontrol *kcontrol,
    struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;

 unsigned int nval1, nval2;
 int change;

 nval1 = ucontrol->value.integer.value[0] ? (1<<26) : 0;
 nval2 = ucontrol->value.integer.value[0] ? (1<<18) : 0;
 _spin_lock_irq(ensoniq->reg_lock);
 change = (ensoniq->ctrl & (1<<26)) != nval1;
 ensoniq->ctrl &= ~(1<<26);
 ensoniq->ctrl |= nval1;
 ensoniq->cssr &= ~(1<<18);
 ensoniq->cssr |= nval2;
 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 outl(ensoniq->cssr, ((ensoniq)->port + 0x04));
 _spin_unlock_irq(ensoniq->reg_lock);
 return change;
}



static struct snd_kcontrol_new snd_es1371_mixer_spdif[] __attribute__ ((__section__(".devinit.data"))) = {
 { .iface = (( snd_ctl_elem_iface_t) 2), .name = "IEC958 " "" "Playback " "Switch", .info = snd_ctl_boolean_mono_info, .get = snd_es1371_spdif_get, .put = snd_es1371_spdif_put },
 {
  .iface = (( snd_ctl_elem_iface_t) 3),
  .name = "IEC958 " "" "Playback " "Default",
  .info = snd_ens1373_spdif_info,
  .get = snd_ens1373_spdif_default_get,
  .put = snd_ens1373_spdif_default_put,
 },
 {
  .access = (1<<0),
  .iface = (( snd_ctl_elem_iface_t) 3),
  .name = "IEC958 " "" "Playback " "Mask",
  .info = snd_ens1373_spdif_info,
  .get = snd_ens1373_spdif_mask_get
 },
 {
  .iface = (( snd_ctl_elem_iface_t) 3),
  .name = "IEC958 " "" "Playback " "PCM Stream",
  .info = snd_ens1373_spdif_info,
  .get = snd_ens1373_spdif_stream_get,
  .put = snd_ens1373_spdif_stream_put
 },
};




static int snd_es1373_rear_get(struct snd_kcontrol *kcontrol,
          struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 int val = 0;

 _spin_lock_irq(ensoniq->reg_lock);
 if ((ensoniq->cssr & ((1<<27)|(1<<26)|
         (1<<24))) == (1<<26))
      val = 1;
 ucontrol->value.integer.value[0] = val;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_es1373_rear_put(struct snd_kcontrol *kcontrol,
          struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 unsigned int nval1;
 int change;

 nval1 = ucontrol->value.integer.value[0] ?
  (1<<26) : ((1<<27)|(1<<24));
 _spin_lock_irq(ensoniq->reg_lock);
 change = (ensoniq->cssr & ((1<<27)|
       (1<<26)|(1<<24))) != nval1;
 ensoniq->cssr &= ~((1<<27)|(1<<26)|(1<<24));
 ensoniq->cssr |= nval1;
 outl(ensoniq->cssr, ((ensoniq)->port + 0x04));
 _spin_unlock_irq(ensoniq->reg_lock);
 return change;
}

static struct snd_kcontrol_new snd_ens1373_rear __attribute__ ((__section__(".devinit.data"))) =
{
 .iface = (( snd_ctl_elem_iface_t) 2),
 .name = "AC97 2ch->4ch Copy Switch",
 .info = snd_ctl_boolean_mono_info,
 .get = snd_es1373_rear_get,
 .put = snd_es1373_rear_put,
};



static int snd_es1373_line_get(struct snd_kcontrol *kcontrol,
          struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 int val = 0;

 _spin_lock_irq(ensoniq->reg_lock);
 if ((ensoniq->ctrl & (0x0f<<16)) >= 4)
      val = 1;
 ucontrol->value.integer.value[0] = val;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_es1373_line_put(struct snd_kcontrol *kcontrol,
          struct snd_ctl_elem_value *ucontrol) __attribute((isolate))
{

 struct ensoniq *ensoniq = kcontrol->private_data;
 int changed;
 unsigned int ctrl;

 _spin_lock_irq(ensoniq->reg_lock);
 ctrl = ensoniq->ctrl;
 if (ucontrol->value.integer.value[0])
  ensoniq->ctrl |= (((4)&0x0f)<<16);
 else
  ensoniq->ctrl &= ~(((4)&0x0f)<<16);
 changed = (ctrl != ensoniq->ctrl);
 if (changed)
  outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 _spin_unlock_irq(ensoniq->reg_lock);
 return changed;
}

static struct snd_kcontrol_new snd_ens1373_line __attribute__ ((__section__(".devinit.data"))) =
{
 .iface = (( snd_ctl_elem_iface_t) 2),
 .name = "Line In->Rear Out Switch",
 .info = snd_ctl_boolean_mono_info,
 .get = snd_es1373_line_get,
 .put = snd_es1373_line_put,
};

static void snd_ensoniq_mixer_free_ac97(struct snd_ac97 *ac97)
{
 struct ensoniq *ensoniq = ac97->private_data;
 ensoniq->u.es1371.ac97 = ((void *)0);
}

struct es1371_quirk {
 unsigned short vid;
 unsigned short did;
 unsigned char rev;
};

static int es1371_quirk_lookup(struct ensoniq *ensoniq,
    struct es1371_quirk *list)
{
 while (list->vid != (unsigned short)(~0)) {
  if (ensoniq->pci->vendor == list->vid &&
      ensoniq->pci->device == list->did &&
      ensoniq->rev == list->rev)
   return 1;
  list++;
 }
 return 0;
}

static struct es1371_quirk es1371_spdif_present[] __attribute__ ((__section__(".devinit.data"))) = {
 { .vid = 0x1274, .did = 0x5880, .rev = 0x02 },
 { .vid = 0x1274, .did = 0x5880, .rev = 0x03 },
 { .vid = 0x1274, .did = 0x5880, .rev = 0x04 },
 { .vid = 0x1274, .did = 0x1371, .rev = 0x07 },
 { .vid = 0x1274, .did = 0x1371, .rev = 0x08 },
 { .vid = (~0), .did = (~0) }
};

static struct snd_pci_quirk ens1373_line_quirk[] __attribute__ ((__section__(".devinit.data"))) = {
 {.subvendor = (0x1274), .subdevice = (0x2000)},
 {.subvendor = (0x1458), .subdevice = (0xa000)},
 { }
};

static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_1371_mixer(struct ensoniq *ensoniq,
         int has_spdif, int has_line)
{
 struct snd_card *card = ensoniq->card;
 struct snd_ac97_bus *pbus;
 struct snd_ac97_template ac97;
 int err;
 static struct snd_ac97_bus_ops ops = {
  .write = snd_es1371_codec_write,
  .read = snd_es1371_codec_read,
  .wait = snd_es1371_codec_wait,
 };

 if ((err = snd_ac97_bus(card, 0, &ops, ((void *)0), &pbus)) < 0)
  return err;

 memset(&ac97, 0, sizeof(ac97));
 ac97.private_data = ensoniq;
 ac97.private_free = snd_ensoniq_mixer_free_ac97;
 ac97.pci = ensoniq->pci;
 ac97.scaps = (1<<0);
 if ((err = snd_ac97_mixer(pbus, &ac97, &ensoniq->u.es1371.ac97)) < 0)
  return err;
 if (has_spdif > 0 ||
     (!has_spdif && es1371_quirk_lookup(ensoniq, es1371_spdif_present))) {
  struct snd_kcontrol *kctl;
  int i, is_spdif = 0;

  ensoniq->spdif_default = ensoniq->spdif_stream =
   ((0<<3)| ((1<<7)<<8)| ((0x02|0x00)<<8)| ((2<<0)<<24));
  outl(ensoniq->spdif_default, ((ensoniq)->port + 0x1c));

  if (ensoniq->u.es1371.ac97->ext_id & 0x0004)
   is_spdif++;

  for (i = 0; i < (sizeof(snd_es1371_mixer_spdif) / sizeof((snd_es1371_mixer_spdif)[0]) + (sizeof(char[1 - 2 * !!(__builtin_types_compatible_p(typeof(snd_es1371_mixer_spdif), typeof(&snd_es1371_mixer_spdif[0])))]) - 1)); i++) {
   kctl = snd_ctl_new1(&snd_es1371_mixer_spdif[i], ensoniq);
   if (!kctl)
    return -12;
   kctl->id.index = is_spdif;
   err = snd_ctl_add(card, kctl);
   if (err < 0)
    return err;
  }
 }
 if (ensoniq->u.es1371.ac97->ext_id & 0x0080) {

  ensoniq->cssr &= ~((1<<27)|(1<<24));
  ensoniq->cssr |= (1<<26);
  err = snd_ctl_add(card, snd_ctl_new1(&snd_ens1373_rear, ensoniq));
  if (err < 0)
   return err;
 }
 if (has_line > 0 ||
     snd_pci_quirk_lookup(ensoniq->pci, ens1373_line_quirk)) {
   err = snd_ctl_add(card, snd_ctl_new1(&snd_ens1373_line,
            ensoniq));
   if (err < 0)
    return err;
 }

 return 0;
}
# 1802 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_get_joystick_port(int dev)
{
 switch (joystick_port[dev]) {
 case 0:
 case 1:
 case 0x200:
 case 0x208:
 case 0x210:
 case 0x218:
  return joystick_port[dev];

 default:
  printk("<3>" "ens1371: invalid joystick port %#x", joystick_port[dev]);
  return 0;
 }
}







static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_create_gameport(struct ensoniq *ensoniq, int dev)
{
 struct gameport *gp;
 int io_port;

 io_port = snd_ensoniq_get_joystick_port(dev);

 switch (io_port) {
 case 0:
  return -38;

 case 1:
  for (io_port = 0x200; io_port <= 0x218; io_port += 8)
   if (__request_region(&ioport_resource, (io_port), (8), ("ens137x: gameport"), 0))
    break;
  if (io_port > 0x218) {
   printk("<4>" "ens137x: no gameport ports available\n");
   return -16;
  }
  break;

 default:
  if (!__request_region(&ioport_resource, (io_port), (8), ("ens137x: gameport"), 0)) {
   printk("<4>" "ens137x: gameport io port 0x%#x in use\n",
          io_port);
   return -16;
  }
  break;
 }

 ensoniq->gameport = gp = gameport_allocate_port();
 if (!gp) {
  printk("<3>" "ens137x: cannot allocate memory for gameport\n");
  __release_region(&ioport_resource, (io_port), (8));
  return -12;
 }

 gameport_set_name(gp, "ES137x");
 gameport_set_phys(gp, "pci%s/gameport0", pci_name(ensoniq->pci));
 ((gp)->dev.parent = (&ensoniq->pci->dev));
 gp->io = io_port;

 ensoniq->ctrl |= (1<<2);

 ensoniq->ctrl &= ~(0x03<<24);
 ensoniq->ctrl |= ((((io_port - 0x200) / 8)&0x03)<<24);

 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));

 gameport_register_port(ensoniq->gameport);

 return 0;
}

static void snd_ensoniq_free_gameport(struct ensoniq *ensoniq)
{
 if (ensoniq->gameport) {
  int port = ensoniq->gameport->io;

  gameport_unregister_port(ensoniq->gameport);
  ensoniq->gameport = ((void *)0);
  ensoniq->ctrl &= ~(1<<2);
  outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
  __release_region(&ioport_resource, (port), (8));
 }
}
# 1900 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
static void snd_ensoniq_proc_read(struct snd_info_entry *entry,
      struct snd_info_buffer * __attribute__((noderef, address_space(2))) buffer) __attribute((isolate))
{
 struct ensoniq *ensoniq = entry->private_data;




 snd_iprintf(buffer, "Ensoniq AudioPCI ES1371\n\n");

 snd_iprintf(buffer, "Joystick enable  : %s\n",
      ensoniq->ctrl & (1<<2) ? "on" : "off");






 snd_iprintf(buffer, "Joystick port    : 0x%x\n",
      ((((ensoniq->ctrl)>>24)&0x03) * 8) + 0x200);

 printk ("Buffer is %s.\n", buffer);
}

static void __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_proc_init(struct ensoniq * ensoniq)
{
 struct snd_info_entry *entry;

 if (! snd_card_proc_new(ensoniq->card, "audiopci", &entry))
  snd_info_set_text_ops(entry, ensoniq, snd_ensoniq_proc_read);
}





static int snd_ensoniq_free(struct ensoniq *ensoniq)
{
 snd_ensoniq_free_gameport(ensoniq);
 if (ensoniq->irq < 0)
  goto __hw_end;




 outl(0, ((ensoniq)->port + 0x00));
 outl(0, ((ensoniq)->port + 0x20));

 if (ensoniq->irq >= 0)
  synchronize_irq(ensoniq->irq);
 pci_set_power_state(ensoniq->pci, 3);
      __hw_end:




 if (ensoniq->irq >= 0)
  free_irq(ensoniq->irq, ensoniq);
 pci_release_regions(ensoniq->pci);
 pci_disable_device(ensoniq->pci);
 kfree(ensoniq);
 return 0;
}

static int snd_ensoniq_dev_free(struct snd_device *device)
{
 struct ensoniq *ensoniq = device->device_data;
 return snd_ensoniq_free(ensoniq);
}


static struct snd_pci_quirk es1371_amplifier_hack[] __attribute__ ((__section__(".devinit.data"))) = {
 {.subvendor = (0x107b), .subdevice = (0x2150)},
 {.subvendor = (0x13bd), .subdevice = (0x100c)},
 {.subvendor = (0x1102), .subdevice = (0x5938)},
 {.subvendor = (0x1102), .subdevice = (0x8938)},
 { }
};

static struct es1371_quirk es1371_ac97_reset_hack[] = {
 { .vid = 0x1274, .did = 0x5880, .rev = 0x02 },
 { .vid = 0x1274, .did = 0x5880, .rev = 0x03 },
 { .vid = 0x1274, .did = 0x5880, .rev = 0x04 },
 { .vid = 0x1274, .did = 0x1371, .rev = 0x07 },
 { .vid = 0x1274, .did = 0x1371, .rev = 0x08 },
 { .vid = (~0), .did = (~0) }
};


static void snd_ensoniq_chip_init(struct ensoniq *ensoniq)
{

 int idx;
# 2004 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));
 outl(ensoniq->sctrl, ((ensoniq)->port + 0x20));
 outl(0, ((ensoniq)->port + 0x18));
 if (es1371_quirk_lookup(ensoniq, es1371_ac97_reset_hack)) {
     outl(ensoniq->cssr, ((ensoniq)->port + 0x04));



     (__builtin_constant_p(20) ? ((20) > 20000 ? __bad_udelay() : __const_udelay((20) * 0x10c7ul)) : __udelay(20));
 }

 outl(ensoniq->ctrl | (1<<14), ((ensoniq)->port + 0x00));
 inl(((ensoniq)->port + 0x00));
 (__builtin_constant_p(20) ? ((20) > 20000 ? __bad_udelay() : __const_udelay((20) * 0x10c7ul)) : __udelay(20));
 outl(ensoniq->ctrl, ((ensoniq)->port + 0x00));

 snd_es1371_wait_src_ready(ensoniq);
 outl((1<<22), ((ensoniq)->port + 0x10));
 for (idx = 0; idx < 0x80; idx++)
  snd_es1371_src_write(ensoniq, idx, 0);
 snd_es1371_src_write(ensoniq, 0x70 + 0x00, 16 << 4);
 snd_es1371_src_write(ensoniq, 0x70 + 0x01, 16 << 10);
 snd_es1371_src_write(ensoniq, 0x74 + 0x00, 16 << 4);
 snd_es1371_src_write(ensoniq, 0x74 + 0x01, 16 << 10);
 snd_es1371_src_write(ensoniq, 0x6c, 1 << 12);
 snd_es1371_src_write(ensoniq, 0x6c + 1, 1 << 12);
 snd_es1371_src_write(ensoniq, 0x7c, 1 << 12);
 snd_es1371_src_write(ensoniq, 0x7c + 1, 1 << 12);
 snd_es1371_src_write(ensoniq, 0x7e, 1 << 12);
 snd_es1371_src_write(ensoniq, 0x7e + 1, 1 << 12);
 snd_es1371_adc_rate(ensoniq, 22050);
 snd_es1371_dac1_rate(ensoniq, 22050);
 snd_es1371_dac2_rate(ensoniq, 22050);






 snd_es1371_wait_src_ready(ensoniq);
 outl(0, ((ensoniq)->port + 0x10));

 outl(((((0)&0x7f)<<16)|(((0)&0xffff)<<0)), ((ensoniq)->port + 0x14));

 outb(ensoniq->uartc = 0x00, ((ensoniq)->port + 0x09));
 outb(0x00, ((ensoniq)->port + 0x0a));
 outl(ensoniq->cssr, ((ensoniq)->port + 0x04));
 synchronize_irq(ensoniq->irq);
}


static int snd_ensoniq_suspend(struct pci_dev *pci, pm_message_t state)
{
 struct snd_card *card = pci_get_drvdata(pci);
 struct ensoniq *ensoniq = card->private_data;

 snd_power_change_state(card, (0x0300|0x0000));

 snd_pcm_suspend_all(ensoniq->pcm1);
 snd_pcm_suspend_all(ensoniq->pcm2);


 snd_ac97_suspend(ensoniq->u.es1371.ac97);
# 2078 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
 pci_disable_device(pci);
 pci_save_state(pci);
 pci_set_power_state(pci, pci_choose_state(pci, state));


 ens1371_checkpoint();
 ens1371_restore();


 return 0;
}

int ens1371_checkpoint()
{
 printk("ens1371_checkpoint.\n");

 if (ens1371_dev == ((void *)0))
  return;

 pci_save_state(ens1371_dev);

}

int ens1371_restore()
{
 printk ("ens1371_resume.\n");

 if (ens1371_dev == ((void *)0))
 {

  alsa_card_ens137x_exit();
 }

 struct snd_card *card = pci_get_drvdata(ens1371_dev);
 struct ensoniq *ensoniq = card->private_data;

 snd_pcm_suspend_all(ensoniq->pcm1);
 snd_pcm_suspend_all(ensoniq->pcm2);

 pci_restore_state(ens1371_dev);
 snd_ensoniq_chip_init(ensoniq);


 snd_ac97_resume(ensoniq->u.es1371.ac97);



}

static int snd_ensoniq_resume(struct pci_dev *pci)
{
 struct snd_card *card = pci_get_drvdata(pci);
 struct ensoniq *ensoniq = card->private_data;

 pci_set_power_state(pci, ((pci_power_t ) 0));
 pci_restore_state(pci);
 if (pci_enable_device(pci) < 0) {
  printk("<3>" "ENS1371" ": pci_enable_device failed, "
         "disabling device\n");
  snd_card_disconnect(card);
  return -5;
 }
 pci_set_master(pci);

 snd_ensoniq_chip_init(ensoniq);


 snd_ac97_resume(ensoniq->u.es1371.ac97);



 snd_power_change_state(card, 0x0000);
 return 0;
}



static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_create(struct snd_card *card,
         struct pci_dev *pci,
         struct ensoniq ** rensoniq)
{
 struct ensoniq *ensoniq;
 int err;
 static struct snd_device_ops ops = {
  .dev_free = snd_ensoniq_dev_free,
 };

 *rensoniq = ((void *)0);
 if ((err = pci_enable_device(pci)) < 0)
  return err;
 ensoniq = kzalloc(sizeof(*ensoniq), ((( gfp_t)0x10u) | (( gfp_t)0x40u) | (( gfp_t)0x80u)));
 if (ensoniq == ((void *)0)) {
  pci_disable_device(pci);
  return -12;
 }
 ensoniq->reg_lock = (struct spinlock_t *) kmalloc (sizeof (spinlock_t), ((( gfp_t)0x20u)));
  ensoniq->src_mutex = (struct mutex *) kmalloc (sizeof (struct mutex), ((( gfp_t)0x20u)));
 do { static struct lock_class_key __key; __spin_lock_init((ensoniq->reg_lock), "ensoniq->reg_lock", &__key); } while (0);
 do { static struct lock_class_key __key; __mutex_init((ensoniq->src_mutex), "ensoniq->src_mutex", &__key); } while (0);
 ensoniq->card = card;
 ensoniq->pci = pci;
 ens1371_dev = pci;
 ensoniq->irq = -1;
 if ((err = pci_request_regions(pci, "Ensoniq AudioPCI")) < 0) {
  kfree(ensoniq);
  pci_disable_device(pci);
  return err;
 }
 ensoniq->port = ((pci)->resource[(0)].start);
 if (request_irq(pci->irq, snd_audiopci_interrupt, 0x00000080,
   "Ensoniq AudioPCI", ensoniq)) {
  printk("<3>" "unable to grab IRQ %d\n" , pci->irq);
  snd_ensoniq_free(ensoniq);
  return -16;
 }
 ensoniq->irq = pci->irq;
# 2202 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
 pci_set_master(pci);
 ensoniq->rev = pci->revision;
# 2213 "/scratch/sym/ipc_drivers/ens1371/ens1371_annotated.c"
 ensoniq->ctrl = 0;
 ensoniq->sctrl = 0;
 ensoniq->cssr = 0;
 if (snd_pci_quirk_lookup(pci, es1371_amplifier_hack))
  ensoniq->ctrl |= (((1)&0x0f)<<16);

 if (es1371_quirk_lookup(ensoniq, es1371_ac97_reset_hack))
  ensoniq->cssr |= (1<<29);


 snd_ensoniq_chip_init(ensoniq);

 if ((err = snd_device_new(card, (( snd_device_type_t) 0x2000), ensoniq, &ops)) < 0) {
  snd_ensoniq_free(ensoniq);
  return err;
 }

 snd_ensoniq_proc_init(ensoniq);

 ((card)->dev = (&pci->dev));

 *rensoniq = ensoniq;
 return 0;
}





static void snd_ensoniq_midi_interrupt(struct ensoniq * ensoniq)
{
 struct snd_rawmidi *rmidi = ensoniq->rmidi;
 unsigned char status, mask, byte;

 if (rmidi == ((void *)0))
  return;

 _spin_lock(ensoniq->reg_lock);
 mask = ensoniq->uartm & 0x0002 ? (1<<0) : 0;
 while (mask) {
  status = inb(((ensoniq)->port + 0x09));
  if ((status & mask) == 0)
   break;
  byte = inb(((ensoniq)->port + 0x08));
  snd_rawmidi_receive(ensoniq->midi_input, &byte, 1);
 }
 _spin_unlock(ensoniq->reg_lock);


 _spin_lock(ensoniq->reg_lock);
 mask = ensoniq->uartm & 0x0001 ? (1<<1) : 0;
 while (mask) {
  status = inb(((ensoniq)->port + 0x09));
  if ((status & mask) == 0)
   break;
  if (snd_rawmidi_transmit(ensoniq->midi_output, &byte, 1) != 1) {
   ensoniq->uartc &= ~(0x03<<5);
   outb(ensoniq->uartc, ((ensoniq)->port + 0x09));
   mask &= ~(1<<1);
  } else {
   outb(byte, ((ensoniq)->port + 0x08));
  }
 }
 _spin_unlock(ensoniq->reg_lock);
}

static int snd_ensoniq_midi_input_open(struct snd_rawmidi_substream *substream)
{
 struct ensoniq *ensoniq = substream->rmidi->private_data;

 _spin_lock_irq(ensoniq->reg_lock);
 ensoniq->uartm |= 0x0002;
 ensoniq->midi_input = substream;
 if (!(ensoniq->uartm & 0x0001)) {
  outb((((3)&0x03)<<0), ((ensoniq)->port + 0x09));
  outb(ensoniq->uartc = 0, ((ensoniq)->port + 0x09));
  outl(ensoniq->ctrl |= (1<<3), ((ensoniq)->port + 0x00));
 }
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_ensoniq_midi_input_close(struct snd_rawmidi_substream *substream)
{
 struct ensoniq *ensoniq = substream->rmidi->private_data;

 _spin_lock_irq(ensoniq->reg_lock);
 if (!(ensoniq->uartm & 0x0001)) {
  outb(ensoniq->uartc = 0, ((ensoniq)->port + 0x09));
  outl(ensoniq->ctrl &= ~(1<<3), ((ensoniq)->port + 0x00));
 } else {
  outb(ensoniq->uartc &= ~(1<<7), ((ensoniq)->port + 0x09));
 }
 ensoniq->midi_input = ((void *)0);
 ensoniq->uartm &= ~0x0002;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_ensoniq_midi_output_open(struct snd_rawmidi_substream *substream)
{
 struct ensoniq *ensoniq = substream->rmidi->private_data;

 _spin_lock_irq(ensoniq->reg_lock);
 ensoniq->uartm |= 0x0001;
 ensoniq->midi_output = substream;
 if (!(ensoniq->uartm & 0x0002)) {
  outb((((3)&0x03)<<0), ((ensoniq)->port + 0x09));
  outb(ensoniq->uartc = 0, ((ensoniq)->port + 0x09));
  outl(ensoniq->ctrl |= (1<<3), ((ensoniq)->port + 0x00));
 }
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static int snd_ensoniq_midi_output_close(struct snd_rawmidi_substream *substream)
{
 struct ensoniq *ensoniq = substream->rmidi->private_data;

 _spin_lock_irq(ensoniq->reg_lock);
 if (!(ensoniq->uartm & 0x0002)) {
  outb(ensoniq->uartc = 0, ((ensoniq)->port + 0x09));
  outl(ensoniq->ctrl &= ~(1<<3), ((ensoniq)->port + 0x00));
 } else {
  outb(ensoniq->uartc &= ~(0x03<<5), ((ensoniq)->port + 0x09));
 }
 ensoniq->midi_output = ((void *)0);
 ensoniq->uartm &= ~0x0001;
 _spin_unlock_irq(ensoniq->reg_lock);
 return 0;
}

static void snd_ensoniq_midi_input_trigger(struct snd_rawmidi_substream *substream, int up)
{
 unsigned long flags;
 struct ensoniq *ensoniq = substream->rmidi->private_data;
 int idx;

 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = _spin_lock_irqsave(ensoniq->reg_lock); } while (0);
 if (up) {
  if ((ensoniq->uartc & (1<<7)) == 0) {

   for (idx = 0; idx < 32; idx++)
    inb(((ensoniq)->port + 0x08));
   ensoniq->uartc |= (1<<7);
   outb(ensoniq->uartc, ((ensoniq)->port + 0x09));
  }
 } else {
  if (ensoniq->uartc & (1<<7)) {
   ensoniq->uartc &= ~(1<<7);
   outb(ensoniq->uartc, ((ensoniq)->port + 0x09));
  }
 }
 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); _spin_unlock_irqrestore(ensoniq->reg_lock, flags); } while (0);
}

static void snd_ensoniq_midi_output_trigger(struct snd_rawmidi_substream *substream, int up ) __attribute((isolate))
{
 unsigned long flags;
 struct ensoniq *ensoniq = substream->rmidi->private_data;
 unsigned char byte;

 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); flags = _spin_lock_irqsave(ensoniq->reg_lock); } while (0);
 if (up) {
  if ((((ensoniq->uartc)>>5)&0x03) == 0) {
   ensoniq->uartc |= (((1)&0x03)<<5);

   while ((((ensoniq->uartc)>>5)&0x03) == 1 &&
          (inb(((ensoniq)->port + 0x09)) & (1<<1))) {
    if (snd_rawmidi_transmit(substream, &byte, 1) != 1) {
     ensoniq->uartc &= ~(0x03<<5);
    } else {
     outb(byte, ((ensoniq)->port + 0x08));
    }
   }
   outb(ensoniq->uartc, ((ensoniq)->port + 0x09));
  }
 } else {
  if ((((ensoniq->uartc)>>5)&0x03) == 1) {
   ensoniq->uartc &= ~(0x03<<5);
   outb(ensoniq->uartc, ((ensoniq)->port + 0x09));
  }
 }
 do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); _spin_unlock_irqrestore(ensoniq->reg_lock, flags); } while (0);
}

static struct snd_rawmidi_ops snd_ensoniq_midi_output =
{
 .open = snd_ensoniq_midi_output_open,
 .close = snd_ensoniq_midi_output_close,
 .trigger = snd_ensoniq_midi_output_trigger,
};

static struct snd_rawmidi_ops snd_ensoniq_midi_input =
{
 .open = snd_ensoniq_midi_input_open,
 .close = snd_ensoniq_midi_input_close,
 .trigger = snd_ensoniq_midi_input_trigger,
};

static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_ensoniq_midi(struct ensoniq * ensoniq, int device,
          struct snd_rawmidi **rrawmidi)
{
 struct snd_rawmidi *rmidi;
 int err;

 if (rrawmidi)
  *rrawmidi = ((void *)0);
 if ((err = snd_rawmidi_new(ensoniq->card, "ES1370/1", device, 1, 1, &rmidi)) < 0)
  return err;



 strcpy(rmidi->name, "ES1371");

 snd_rawmidi_set_ops(rmidi, SNDRV_RAWMIDI_STREAM_OUTPUT, &snd_ensoniq_midi_output);
 snd_rawmidi_set_ops(rmidi, SNDRV_RAWMIDI_STREAM_INPUT, &snd_ensoniq_midi_input);
 rmidi->info_flags |= 0x00000001 | 0x00000002 |
  0x00000004;
 rmidi->private_data = ensoniq;
 ensoniq->rmidi = rmidi;
 if (rrawmidi)
  *rrawmidi = rmidi;
 return 0;
}





static irqreturn_t snd_audiopci_interrupt(int irq, void *dev_id)
{
 struct ensoniq *ensoniq = dev_id;
 unsigned int status, sctrl;

 if (ensoniq == ((void *)0))
  return (0);

 status = inl(((ensoniq)->port + 0x04));
 if (!(status & (1<<31)))
  return (0);

 _spin_lock(ensoniq->reg_lock);
 sctrl = ensoniq->sctrl;
 if (status & (1<<2))
  sctrl &= ~(1<<8);
 if (status & (1<<1))
  sctrl &= ~(1<<9);
 if (status & (1<<0))
  sctrl &= ~(1<<10);
 outl(sctrl, ((ensoniq)->port + 0x20));
 outl(ensoniq->sctrl, ((ensoniq)->port + 0x20));
 _spin_unlock(ensoniq->reg_lock);

 if (status & (1<<3))
  snd_ensoniq_midi_interrupt(ensoniq);
 if ((status & (1<<1)) && ensoniq->playback2_substream)
  snd_pcm_period_elapsed(ensoniq->playback2_substream);
 if ((status & (1<<0)) && ensoniq->capture_substream)
  snd_pcm_period_elapsed(ensoniq->capture_substream);
 if ((status & (1<<2)) && ensoniq->playback1_substream)
  snd_pcm_period_elapsed(ensoniq->playback1_substream);
 return (1);
}

static int __attribute__ ((__section__(".devinit.text"))) __attribute__((__cold__)) snd_audiopci_probe(struct pci_dev *pci,
     const struct pci_device_id *pci_id)
{
 static int dev;
 struct snd_card *card;
 struct ensoniq *ensoniq;
 int err, pcm_devs[2];

 if (dev >= 32)
  return -19;
 if (!enable[dev]) {
  dev++;
  return -2;
 }

 card = snd_card_new(index[dev], id[dev], (&__this_module), 0);
 if (card == ((void *)0))
  return -12;

 if ((err = snd_ensoniq_create(card, pci, &ensoniq)) < 0) {
  snd_card_free(card);
  return err;
 }
 card->private_data = ensoniq;

 pcm_devs[0] = 0; pcm_devs[1] = 1;







 if ((err = snd_ensoniq_1371_mixer(ensoniq, spdif[dev], lineio[dev])) < 0) {
  snd_card_free(card);
  return err;
 }

 if ((err = snd_ensoniq_pcm(ensoniq, 0, ((void *)0))) < 0) {
  snd_card_free(card);
  return err;
 }
 if ((err = snd_ensoniq_pcm2(ensoniq, 1, ((void *)0))) < 0) {
  snd_card_free(card);
  return err;
 }
 if ((err = snd_ensoniq_midi(ensoniq, 0, ((void *)0))) < 0) {
  snd_card_free(card);
  return err;
 }

 snd_ensoniq_create_gameport(ensoniq, dev);

 strcpy(card->driver, "ENS1371");

 strcpy(card->shortname, "Ensoniq AudioPCI");
 sprintf(card->longname, "%s %s at 0x%lx, irq %i",
  card->shortname,
  card->driver,
  ensoniq->port,
  ensoniq->irq);

 if ((err = snd_card_register(card)) < 0) {
  snd_card_free(card);
  return err;
 }

 pci_set_drvdata(pci, card);
 dev++;
 return 0;
}

static void __attribute__ ((__section__(".devexit.text"))) __attribute__((__cold__)) snd_audiopci_remove(struct pci_dev *pci)
{
 snd_card_free(pci_get_drvdata(pci));
 pci_set_drvdata(pci, ((void *)0));
}

static struct pci_driver driver = {
 .name = "ENS1371",
 .id_table = snd_audiopci_ids,
 .probe = snd_audiopci_probe,
 .remove = snd_audiopci_remove,

 .suspend = snd_ensoniq_suspend,
 .resume = snd_ensoniq_resume,

};

static int __attribute__ ((__section__(".init.text"))) __attribute__((__cold__)) __attribute__((no_instrument_function)) alsa_card_ens137x_init(void)
{
 return __pci_register_driver(&driver, (&__this_module), "ens1371");
}

static void __attribute__ ((__section__(".exit.text"))) __attribute__((__cold__)) alsa_card_ens137x_exit(void)
{
 pci_unregister_driver(&driver);
}

static inline __attribute__((always_inline)) initcall_t __inittest(void) { return alsa_card_ens137x_init; } int init_module(void) __attribute__((alias("alsa_card_ens137x_init")));
static inline __attribute__((always_inline)) exitcall_t __exittest(void) { return alsa_card_ens137x_exit; } void cleanup_module(void) __attribute__((alias("alsa_card_ens137x_exit")));
