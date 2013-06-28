cmd_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.o := gcc -Wp,-MD,/scratch/sym/ipc_drivers/cmipci/../../common/.wrappers_locks.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include -Iinclude  -I/scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include -include include/linux/autoconf.h -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iarch/x86/include/asm/mach-default -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -g -Wall -Wno-attributes -Wno-unknown-pragmas -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(wrappers_locks)"  -D"KBUILD_MODNAME=KBUILD_STR(cmipci_sfi)"  -c -o /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.o /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.c

deps_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.o := \
  /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.c \
  include/linux/kernel.h \
    $(wildcard include/config/lbd.h) \
    $(wildcard include/config/preempt/voluntary.h) \
    $(wildcard include/config/debug/spinlock/sleep.h) \
    $(wildcard include/config/prove/locking.h) \
    $(wildcard include/config/printk.h) \
    $(wildcard include/config/dynamic/printk/debug.h) \
    $(wildcard include/config/numa.h) \
    $(wildcard include/config/ftrace/mcount/record.h) \
  /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stdarg.h \
  include/linux/linkage.h \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/linux/compiler-gcc4.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/linkage.h \
    $(wildcard include/config/x86/64.h) \
    $(wildcard include/config/x86/32.h) \
    $(wildcard include/config/x86/alignment/16.h) \
  include/linux/stddef.h \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/phys/addr/t/64bit.h) \
    $(wildcard include/config/64bit.h) \
  include/linux/posix_types.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/posix_types.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/posix_types_64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/types.h \
    $(wildcard include/config/highmem64g.h) \
  include/asm-generic/int-ll64.h \
  include/linux/bitops.h \
    $(wildcard include/config/generic/find/first/bit.h) \
    $(wildcard include/config/generic/find/last/bit.h) \
    $(wildcard include/config/generic/find/next/bit.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/bitops.h \
    $(wildcard include/config/x86/cmov.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/alternative.h \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/paravirt.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/asm.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/cpufeature.h \
    $(wildcard include/config/x86/invlpg.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/required-features.h \
    $(wildcard include/config/x86/minimum/cpu/family.h) \
    $(wildcard include/config/math/emulation.h) \
    $(wildcard include/config/x86/pae.h) \
    $(wildcard include/config/x86/cmpxchg64.h) \
    $(wildcard include/config/x86/use/3dnow.h) \
    $(wildcard include/config/x86/p6/nop.h) \
  include/asm-generic/bitops/sched.h \
  include/asm-generic/bitops/hweight.h \
  include/asm-generic/bitops/fls64.h \
  include/asm-generic/bitops/ext2-non-atomic.h \
  include/asm-generic/bitops/le.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/byteorder.h \
  include/linux/byteorder/little_endian.h \
  include/linux/swab.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/swab.h \
    $(wildcard include/config/x86/bswap.h) \
  include/linux/byteorder/generic.h \
  include/asm-generic/bitops/minix.h \
  include/linux/log2.h \
    $(wildcard include/config/arch/has/ilog2/u32.h) \
    $(wildcard include/config/arch/has/ilog2/u64.h) \
  include/linux/typecheck.h \
  include/linux/ratelimit.h \
  include/linux/param.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/param.h \
    $(wildcard include/config/hz.h) \
  include/linux/dynamic_printk.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/bug.h \
    $(wildcard include/config/bug.h) \
    $(wildcard include/config/debug/bugverbose.h) \
  include/asm-generic/bug.h \
    $(wildcard include/config/generic/bug.h) \
    $(wildcard include/config/generic/bug/relative/pointers.h) \
  include/linux/spinlock.h \
    $(wildcard include/config/debug/spinlock.h) \
    $(wildcard include/config/generic/lockbreak.h) \
    $(wildcard include/config/preempt.h) \
    $(wildcard include/config/debug/lock/alloc.h) \
  include/linux/preempt.h \
    $(wildcard include/config/debug/preempt.h) \
    $(wildcard include/config/preempt/tracer.h) \
    $(wildcard include/config/preempt/notifiers.h) \
  include/linux/thread_info.h \
    $(wildcard include/config/compat.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/thread_info.h \
    $(wildcard include/config/debug/stack/usage.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/page.h \
  include/linux/const.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/page_64.h \
    $(wildcard include/config/physical/start.h) \
    $(wildcard include/config/flatmem.h) \
  include/asm-generic/memory_model.h \
    $(wildcard include/config/discontigmem.h) \
    $(wildcard include/config/sparsemem/vmemmap.h) \
    $(wildcard include/config/sparsemem.h) \
  include/asm-generic/page.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/processor.h \
    $(wildcard include/config/x86/vsmp.h) \
    $(wildcard include/config/x86/ds.h) \
    $(wildcard include/config/x86/ptrace/bts.h) \
    $(wildcard include/config/x86/debugctlmsr.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/processor-flags.h \
    $(wildcard include/config/vm86.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/vm86.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/ptrace.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/ptrace-abi.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/segment.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/cache.h \
    $(wildcard include/config/x86/l1/cache/shift.h) \
  include/linux/init.h \
    $(wildcard include/config/modules.h) \
    $(wildcard include/config/hotplug.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/math_emu.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/sigcontext.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/current.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/pda.h \
    $(wildcard include/config/cc/stackprotector.h) \
  include/linux/cache.h \
    $(wildcard include/config/arch/has/cache/line/size.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/system.h \
    $(wildcard include/config/ia32/emulation.h) \
    $(wildcard include/config/x86/ppro/fence.h) \
    $(wildcard include/config/x86/oostore.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/cmpxchg.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/cmpxchg_64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/nops.h \
    $(wildcard include/config/mk7.h) \
  include/linux/irqflags.h \
    $(wildcard include/config/trace/irqflags.h) \
    $(wildcard include/config/irqsoff/tracer.h) \
    $(wildcard include/config/trace/irqflags/support.h) \
    $(wildcard include/config/x86.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/irqflags.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/percpu.h \
  include/asm-generic/percpu.h \
    $(wildcard include/config/have/setup/per/cpu/area.h) \
  include/linux/threads.h \
    $(wildcard include/config/nr/cpus.h) \
    $(wildcard include/config/base/small.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/msr.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/msr-index.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/errno.h \
  include/asm-generic/errno.h \
  include/asm-generic/errno-base.h \
  include/linux/errno.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/desc_defs.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/ds.h \
  include/linux/err.h \
  include/linux/personality.h \
  include/linux/cpumask.h \
    $(wildcard include/config/disable/obsolete/cpumask/functions.h) \
    $(wildcard include/config/hotplug/cpu.h) \
    $(wildcard include/config/cpumask/offstack.h) \
    $(wildcard include/config/debug/per/cpu/maps.h) \
  include/linux/bitmap.h \
  include/linux/string.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/string.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/string_64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/ftrace.h \
    $(wildcard include/config/function/tracer.h) \
    $(wildcard include/config/dynamic/ftrace.h) \
    $(wildcard include/config/function/graph/tracer.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/atomic.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/atomic_64.h \
  include/asm-generic/atomic.h \
  include/linux/list.h \
    $(wildcard include/config/debug/list.h) \
  include/linux/poison.h \
  include/linux/prefetch.h \
  include/linux/stringify.h \
  include/linux/bottom_half.h \
  include/linux/spinlock_types.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/spinlock_types.h \
  include/linux/lockdep.h \
    $(wildcard include/config/lockdep.h) \
    $(wildcard include/config/lock/stat.h) \
    $(wildcard include/config/generic/hardirqs.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/spinlock.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/rwlock.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/paravirt.h \
    $(wildcard include/config/x86/local/apic.h) \
    $(wildcard include/config/highpte.h) \
    $(wildcard include/config/paravirt/debug.h) \
  include/linux/spinlock_api_smp.h \

/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.o: $(deps_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.o)

$(deps_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_locks.o):
