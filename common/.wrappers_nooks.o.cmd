cmd_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.o := gcc -Wp,-MD,/scratch/sym/ipc_drivers/cmipci/../../common/.wrappers_nooks.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include -Iinclude  -I/scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include -include include/linux/autoconf.h -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iarch/x86/include/asm/mach-default -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -g -Wall -Wno-attributes -Wno-unknown-pragmas -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(wrappers_nooks)"  -D"KBUILD_MODNAME=KBUILD_STR(cmipci_sfi)"  -c -o /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.o /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.c

deps_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.o := \
  /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.c \
  include/linux/string.h \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/linux/compiler-gcc4.h \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/lbd.h) \
    $(wildcard include/config/phys/addr/t/64bit.h) \
    $(wildcard include/config/64bit.h) \
  include/linux/posix_types.h \
  include/linux/stddef.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/posix_types.h \
    $(wildcard include/config/x86/32.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/posix_types_64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/types.h \
    $(wildcard include/config/x86/64.h) \
    $(wildcard include/config/highmem64g.h) \
  include/asm-generic/int-ll64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/string.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/string_64.h \
  include/linux/gfp.h \
    $(wildcard include/config/numa.h) \
    $(wildcard include/config/zone/dma.h) \
    $(wildcard include/config/zone/dma32.h) \
    $(wildcard include/config/highmem.h) \
  include/linux/mmzone.h \
    $(wildcard include/config/force/max/zoneorder.h) \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/unevictable/lru.h) \
    $(wildcard include/config/memory/hotplug.h) \
    $(wildcard include/config/sparsemem.h) \
    $(wildcard include/config/arch/populates/node/map.h) \
    $(wildcard include/config/discontigmem.h) \
    $(wildcard include/config/flat/node/mem/map.h) \
    $(wildcard include/config/cgroup/mem/res/ctlr.h) \
    $(wildcard include/config/have/memory/present.h) \
    $(wildcard include/config/need/node/memmap/size.h) \
    $(wildcard include/config/need/multiple/nodes.h) \
    $(wildcard include/config/have/arch/early/pfn/to/nid.h) \
    $(wildcard include/config/flatmem.h) \
    $(wildcard include/config/sparsemem/extreme.h) \
    $(wildcard include/config/nodes/span/other/nodes.h) \
    $(wildcard include/config/holes/in/zone.h) \
  include/linux/spinlock.h \
    $(wildcard include/config/debug/spinlock.h) \
    $(wildcard include/config/generic/lockbreak.h) \
    $(wildcard include/config/preempt.h) \
    $(wildcard include/config/debug/lock/alloc.h) \
  include/linux/typecheck.h \
  include/linux/preempt.h \
    $(wildcard include/config/debug/preempt.h) \
    $(wildcard include/config/preempt/tracer.h) \
    $(wildcard include/config/preempt/notifiers.h) \
  include/linux/thread_info.h \
    $(wildcard include/config/compat.h) \
  include/linux/bitops.h \
    $(wildcard include/config/generic/find/first/bit.h) \
    $(wildcard include/config/generic/find/last/bit.h) \
    $(wildcard include/config/generic/find/next/bit.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/bitops.h \
    $(wildcard include/config/x86/cmov.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/alternative.h \
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
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/thread_info.h \
    $(wildcard include/config/debug/stack/usage.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/page.h \
  include/linux/const.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/page_64.h \
    $(wildcard include/config/physical/start.h) \
  include/asm-generic/memory_model.h \
    $(wildcard include/config/sparsemem/vmemmap.h) \
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
  include/linux/kernel.h \
    $(wildcard include/config/preempt/voluntary.h) \
    $(wildcard include/config/debug/spinlock/sleep.h) \
    $(wildcard include/config/prove/locking.h) \
    $(wildcard include/config/printk.h) \
    $(wildcard include/config/dynamic/printk/debug.h) \
    $(wildcard include/config/ftrace/mcount/record.h) \
  /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stdarg.h \
  include/linux/linkage.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/linkage.h \
    $(wildcard include/config/x86/alignment/16.h) \
  include/linux/log2.h \
    $(wildcard include/config/arch/has/ilog2/u32.h) \
    $(wildcard include/config/arch/has/ilog2/u64.h) \
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
  include/linux/wait.h \
  include/linux/numa.h \
    $(wildcard include/config/nodes/shift.h) \
  include/linux/seqlock.h \
  include/linux/nodemask.h \
  include/linux/pageblock-flags.h \
    $(wildcard include/config/hugetlb/page.h) \
    $(wildcard include/config/hugetlb/page/size/variable.h) \
  include/linux/bounds.h \
  include/linux/memory_hotplug.h \
    $(wildcard include/config/have/arch/nodedata/extension.h) \
    $(wildcard include/config/memory/hotremove.h) \
  include/linux/notifier.h \
  include/linux/mutex.h \
    $(wildcard include/config/debug/mutexes.h) \
  include/linux/rwsem.h \
    $(wildcard include/config/rwsem/generic/spinlock.h) \
  include/linux/rwsem-spinlock.h \
  include/linux/srcu.h \
  include/linux/topology.h \
    $(wildcard include/config/sched/smt.h) \
    $(wildcard include/config/sched/mc.h) \
  include/linux/smp.h \
    $(wildcard include/config/use/generic/smp/helpers.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/smp.h \
    $(wildcard include/config/x86/io/apic.h) \
    $(wildcard include/config/x86/32/smp.h) \
    $(wildcard include/config/x86/64/smp.h) \
    $(wildcard include/config/x86/has/boot/cpu/id.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/mpspec.h \
    $(wildcard include/config/x86/numaq.h) \
    $(wildcard include/config/mca.h) \
    $(wildcard include/config/eisa.h) \
    $(wildcard include/config/x86/mpparse.h) \
    $(wildcard include/config/acpi.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/mpspec_def.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/apic.h \
  include/linux/pm.h \
    $(wildcard include/config/pm/sleep.h) \
  include/linux/delay.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/delay.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/fixmap.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/fixmap_64.h \
    $(wildcard include/config/provide/ohci1394/dma/init.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/acpi.h \
    $(wildcard include/config/acpi/numa.h) \
  include/acpi/pdc_intel.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/numa.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/numa_64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/apicdef.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/mmu.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/vsyscall.h \
    $(wildcard include/config/generic/time.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/io_apic.h \
    $(wildcard include/config/pci.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/irq_vectors.h \
    $(wildcard include/config/x86/voyager.h) \
    $(wildcard include/config/sparse/irq.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/topology.h \
    $(wildcard include/config/x86/ht.h) \
    $(wildcard include/config/x86/64/acpi/numa.h) \
  include/asm-generic/topology.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/mmzone.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/mmzone_64.h \
    $(wildcard include/config/numa/emu.h) \
  include/linux/mmdebug.h \
    $(wildcard include/config/debug/vm.h) \
    $(wildcard include/config/debug/virtual.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/sparsemem.h \
  include/linux/mm.h \
    $(wildcard include/config/sysctl.h) \
    $(wildcard include/config/mmu.h) \
    $(wildcard include/config/stack/growsup.h) \
    $(wildcard include/config/security.h) \
    $(wildcard include/config/swap.h) \
    $(wildcard include/config/shmem.h) \
    $(wildcard include/config/proc/fs.h) \
    $(wildcard include/config/ia64.h) \
    $(wildcard include/config/debug/pagealloc.h) \
    $(wildcard include/config/hibernation.h) \
  include/linux/rbtree.h \
  include/linux/prio_tree.h \
  include/linux/debug_locks.h \
    $(wildcard include/config/debug/locking/api/selftests.h) \
  include/linux/mm_types.h \
    $(wildcard include/config/split/ptlock/cpus.h) \
    $(wildcard include/config/mm/owner.h) \
    $(wildcard include/config/mmu/notifier.h) \
  include/linux/auxvec.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/auxvec.h \
  include/linux/completion.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/pgtable.h \
    $(wildcard include/config/compat/vdso.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/pgtable_64.h \
  include/asm-generic/pgtable.h \
  include/linux/page-flags.h \
    $(wildcard include/config/pageflags/extended.h) \
    $(wildcard include/config/ia64/uncached/allocator.h) \
    $(wildcard include/config/s390.h) \
  include/linux/vmstat.h \
    $(wildcard include/config/vm/event/counters.h) \
  include/linux/percpu.h \
  include/linux/slab.h \
    $(wildcard include/config/slab/debug.h) \
    $(wildcard include/config/debug/objects.h) \
    $(wildcard include/config/slub.h) \
    $(wildcard include/config/slob.h) \
    $(wildcard include/config/debug/slab.h) \
  include/linux/slub_def.h \
    $(wildcard include/config/slub/stats.h) \
    $(wildcard include/config/slub/debug.h) \
  include/linux/workqueue.h \
  include/linux/timer.h \
    $(wildcard include/config/timer/stats.h) \
    $(wildcard include/config/debug/objects/timers.h) \
  include/linux/ktime.h \
    $(wildcard include/config/ktime/scalar.h) \
  include/linux/time.h \
  include/linux/math64.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/div64.h \
  include/asm-generic/div64.h \
  include/linux/jiffies.h \
  include/linux/timex.h \
    $(wildcard include/config/no/hz.h) \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/timex.h \
  /scratch/sym/annotated/linux-2.6.29-ipc-cmipci/arch/x86/include/asm/tsc.h \
    $(wildcard include/config/x86/tsc.h) \
  include/linux/debugobjects.h \
    $(wildcard include/config/debug/objects/free.h) \
  include/linux/kobject.h \
  include/linux/sysfs.h \
    $(wildcard include/config/sysfs.h) \
  include/linux/kref.h \
  /scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.h \
  /scratch/sym/ipc_drivers/cmipci/../../common/ud_md.h \
  /scratch/sym/ipc_drivers/cmipci/../../common/slave_master_ud_md.h \
  /scratch/sym/ipc_drivers/cmipci/../../common/full_slab_verify.h \

/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.o: $(deps_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.o)

$(deps_/scratch/sym/ipc_drivers/cmipci/../../common/wrappers_nooks.o):
