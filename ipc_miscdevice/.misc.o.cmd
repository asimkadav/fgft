cmd_/scratch/sym/ipc_miscdevice/misc.o := gcc -Wp,-MD,/scratch/sym/ipc_miscdevice/.misc.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include -Iinclude -Iinclude2 -I/scratch/sym/linux-2.6.29/include -I/scratch/sym/linux-2.6.29/arch/x86/include -include include/linux/autoconf.h   -I/scratch/sym/ipc_miscdevice -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow  -I/scratch/sym/linux-2.6.29/arch/x86/include/asm/mach-default -Iarch/x86/include/asm/mach-default -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -Wno-attributes -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(misc)"  -D"KBUILD_MODNAME=KBUILD_STR(mischelp)"  -c -o /scratch/sym/ipc_miscdevice/misc.o /scratch/sym/ipc_miscdevice/misc.c

deps_/scratch/sym/ipc_miscdevice/misc.o := \
  /scratch/sym/ipc_miscdevice/misc.c \
    $(wildcard include/config/numa.h) \
  /scratch/sym/ipc_miscdevice/common_h.h \
  /scratch/sym/ipc_miscdevice/../common/testing_ep.h \
  /scratch/sym/ipc_miscdevice/../common/uprintk.h \
  /scratch/sym/ipc_miscdevice/../common/master_md_nooks_api.h \
  /scratch/sym/ipc_miscdevice/nooks/nooks-i.h \
  /scratch/sym/linux-2.6.29/include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/lbd.h) \
    $(wildcard include/config/phys/addr/t/64bit.h) \
    $(wildcard include/config/64bit.h) \
  /scratch/sym/linux-2.6.29/include/linux/posix_types.h \
  /scratch/sym/linux-2.6.29/include/linux/stddef.h \
  /scratch/sym/linux-2.6.29/include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  /scratch/sym/linux-2.6.29/include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  /scratch/sym/linux-2.6.29/include/linux/compiler-gcc4.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/posix_types.h \
    $(wildcard include/config/x86/32.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/posix_types_64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/types.h \
    $(wildcard include/config/x86/64.h) \
    $(wildcard include/config/highmem64g.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/int-ll64.h \
  /scratch/sym/linux-2.6.29/include/linux/slab.h \
    $(wildcard include/config/slab/debug.h) \
    $(wildcard include/config/debug/objects.h) \
    $(wildcard include/config/slub.h) \
    $(wildcard include/config/slob.h) \
    $(wildcard include/config/debug/slab.h) \
  /scratch/sym/linux-2.6.29/include/linux/gfp.h \
    $(wildcard include/config/zone/dma.h) \
    $(wildcard include/config/zone/dma32.h) \
    $(wildcard include/config/highmem.h) \
  /scratch/sym/linux-2.6.29/include/linux/mmzone.h \
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
  /scratch/sym/linux-2.6.29/include/linux/spinlock.h \
    $(wildcard include/config/debug/spinlock.h) \
    $(wildcard include/config/generic/lockbreak.h) \
    $(wildcard include/config/preempt.h) \
    $(wildcard include/config/debug/lock/alloc.h) \
  /scratch/sym/linux-2.6.29/include/linux/typecheck.h \
  /scratch/sym/linux-2.6.29/include/linux/preempt.h \
    $(wildcard include/config/debug/preempt.h) \
    $(wildcard include/config/preempt/tracer.h) \
    $(wildcard include/config/preempt/notifiers.h) \
  /scratch/sym/linux-2.6.29/include/linux/thread_info.h \
    $(wildcard include/config/compat.h) \
  /scratch/sym/linux-2.6.29/include/linux/bitops.h \
    $(wildcard include/config/generic/find/first/bit.h) \
    $(wildcard include/config/generic/find/last/bit.h) \
    $(wildcard include/config/generic/find/next/bit.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/bitops.h \
    $(wildcard include/config/x86/cmov.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/alternative.h \
    $(wildcard include/config/paravirt.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/asm.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/cpufeature.h \
    $(wildcard include/config/x86/invlpg.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/required-features.h \
    $(wildcard include/config/x86/minimum/cpu/family.h) \
    $(wildcard include/config/math/emulation.h) \
    $(wildcard include/config/x86/pae.h) \
    $(wildcard include/config/x86/cmpxchg64.h) \
    $(wildcard include/config/x86/use/3dnow.h) \
    $(wildcard include/config/x86/p6/nop.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/bitops/sched.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/bitops/hweight.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/bitops/fls64.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/bitops/ext2-non-atomic.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/bitops/le.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/byteorder.h \
  /scratch/sym/linux-2.6.29/include/linux/byteorder/little_endian.h \
  /scratch/sym/linux-2.6.29/include/linux/swab.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/swab.h \
    $(wildcard include/config/x86/bswap.h) \
  /scratch/sym/linux-2.6.29/include/linux/byteorder/generic.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/bitops/minix.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/thread_info.h \
    $(wildcard include/config/debug/stack/usage.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/page.h \
  /scratch/sym/linux-2.6.29/include/linux/const.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/page_64.h \
    $(wildcard include/config/physical/start.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/memory_model.h \
    $(wildcard include/config/sparsemem/vmemmap.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/page.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/processor.h \
    $(wildcard include/config/x86/vsmp.h) \
    $(wildcard include/config/x86/ds.h) \
    $(wildcard include/config/x86/ptrace/bts.h) \
    $(wildcard include/config/x86/debugctlmsr.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/processor-flags.h \
    $(wildcard include/config/vm86.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/vm86.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ptrace.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ptrace-abi.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/segment.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/cache.h \
    $(wildcard include/config/x86/l1/cache/shift.h) \
  /scratch/sym/linux-2.6.29/include/linux/init.h \
    $(wildcard include/config/modules.h) \
    $(wildcard include/config/hotplug.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/math_emu.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/sigcontext.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/current.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/pda.h \
    $(wildcard include/config/cc/stackprotector.h) \
  /scratch/sym/linux-2.6.29/include/linux/cache.h \
    $(wildcard include/config/arch/has/cache/line/size.h) \
  /scratch/sym/linux-2.6.29/include/linux/kernel.h \
    $(wildcard include/config/preempt/voluntary.h) \
    $(wildcard include/config/debug/spinlock/sleep.h) \
    $(wildcard include/config/prove/locking.h) \
    $(wildcard include/config/printk.h) \
    $(wildcard include/config/dynamic/printk/debug.h) \
    $(wildcard include/config/ftrace/mcount/record.h) \
  /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stdarg.h \
  /scratch/sym/linux-2.6.29/include/linux/linkage.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/linkage.h \
    $(wildcard include/config/x86/alignment/16.h) \
  /scratch/sym/linux-2.6.29/include/linux/log2.h \
    $(wildcard include/config/arch/has/ilog2/u32.h) \
    $(wildcard include/config/arch/has/ilog2/u64.h) \
  /scratch/sym/linux-2.6.29/include/linux/ratelimit.h \
  /scratch/sym/linux-2.6.29/include/linux/param.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/param.h \
    $(wildcard include/config/hz.h) \
  /scratch/sym/linux-2.6.29/include/linux/dynamic_printk.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/bug.h \
    $(wildcard include/config/bug.h) \
    $(wildcard include/config/debug/bugverbose.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/bug.h \
    $(wildcard include/config/generic/bug.h) \
    $(wildcard include/config/generic/bug/relative/pointers.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/system.h \
    $(wildcard include/config/ia32/emulation.h) \
    $(wildcard include/config/x86/ppro/fence.h) \
    $(wildcard include/config/x86/oostore.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/cmpxchg.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/cmpxchg_64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/nops.h \
    $(wildcard include/config/mk7.h) \
  /scratch/sym/linux-2.6.29/include/linux/irqflags.h \
    $(wildcard include/config/trace/irqflags.h) \
    $(wildcard include/config/irqsoff/tracer.h) \
    $(wildcard include/config/trace/irqflags/support.h) \
    $(wildcard include/config/x86.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/irqflags.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/percpu.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/percpu.h \
    $(wildcard include/config/have/setup/per/cpu/area.h) \
  /scratch/sym/linux-2.6.29/include/linux/threads.h \
    $(wildcard include/config/nr/cpus.h) \
    $(wildcard include/config/base/small.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/msr.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/msr-index.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/errno.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/errno.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/errno-base.h \
  /scratch/sym/linux-2.6.29/include/linux/errno.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/desc_defs.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ds.h \
  /scratch/sym/linux-2.6.29/include/linux/err.h \
  /scratch/sym/linux-2.6.29/include/linux/personality.h \
  /scratch/sym/linux-2.6.29/include/linux/cpumask.h \
    $(wildcard include/config/disable/obsolete/cpumask/functions.h) \
    $(wildcard include/config/hotplug/cpu.h) \
    $(wildcard include/config/cpumask/offstack.h) \
    $(wildcard include/config/debug/per/cpu/maps.h) \
  /scratch/sym/linux-2.6.29/include/linux/bitmap.h \
  /scratch/sym/linux-2.6.29/include/linux/string.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/string.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/string_64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ftrace.h \
    $(wildcard include/config/function/tracer.h) \
    $(wildcard include/config/dynamic/ftrace.h) \
    $(wildcard include/config/function/graph/tracer.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/atomic.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/atomic_64.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/atomic.h \
  /scratch/sym/linux-2.6.29/include/linux/list.h \
    $(wildcard include/config/debug/list.h) \
  /scratch/sym/linux-2.6.29/include/linux/poison.h \
  /scratch/sym/linux-2.6.29/include/linux/prefetch.h \
  /scratch/sym/linux-2.6.29/include/linux/stringify.h \
  /scratch/sym/linux-2.6.29/include/linux/bottom_half.h \
  /scratch/sym/linux-2.6.29/include/linux/spinlock_types.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/spinlock_types.h \
  /scratch/sym/linux-2.6.29/include/linux/lockdep.h \
    $(wildcard include/config/lockdep.h) \
    $(wildcard include/config/lock/stat.h) \
    $(wildcard include/config/generic/hardirqs.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/spinlock.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/rwlock.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/paravirt.h \
    $(wildcard include/config/x86/local/apic.h) \
    $(wildcard include/config/highpte.h) \
    $(wildcard include/config/paravirt/debug.h) \
  /scratch/sym/linux-2.6.29/include/linux/spinlock_api_smp.h \
  /scratch/sym/linux-2.6.29/include/linux/wait.h \
  /scratch/sym/linux-2.6.29/include/linux/numa.h \
    $(wildcard include/config/nodes/shift.h) \
  /scratch/sym/linux-2.6.29/include/linux/seqlock.h \
  /scratch/sym/linux-2.6.29/include/linux/nodemask.h \
  /scratch/sym/linux-2.6.29/include/linux/pageblock-flags.h \
    $(wildcard include/config/hugetlb/page.h) \
    $(wildcard include/config/hugetlb/page/size/variable.h) \
  include/linux/bounds.h \
  /scratch/sym/linux-2.6.29/include/linux/memory_hotplug.h \
    $(wildcard include/config/have/arch/nodedata/extension.h) \
    $(wildcard include/config/memory/hotremove.h) \
  /scratch/sym/linux-2.6.29/include/linux/notifier.h \
  /scratch/sym/linux-2.6.29/include/linux/mutex.h \
    $(wildcard include/config/debug/mutexes.h) \
  /scratch/sym/linux-2.6.29/include/linux/rwsem.h \
    $(wildcard include/config/rwsem/generic/spinlock.h) \
  /scratch/sym/linux-2.6.29/include/linux/rwsem-spinlock.h \
  /scratch/sym/linux-2.6.29/include/linux/srcu.h \
  /scratch/sym/linux-2.6.29/include/linux/topology.h \
    $(wildcard include/config/sched/smt.h) \
    $(wildcard include/config/sched/mc.h) \
  /scratch/sym/linux-2.6.29/include/linux/smp.h \
    $(wildcard include/config/use/generic/smp/helpers.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/smp.h \
    $(wildcard include/config/x86/io/apic.h) \
    $(wildcard include/config/x86/32/smp.h) \
    $(wildcard include/config/x86/64/smp.h) \
    $(wildcard include/config/x86/has/boot/cpu/id.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/mpspec.h \
    $(wildcard include/config/x86/numaq.h) \
    $(wildcard include/config/mca.h) \
    $(wildcard include/config/eisa.h) \
    $(wildcard include/config/x86/mpparse.h) \
    $(wildcard include/config/acpi.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/mpspec_def.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/apic.h \
  /scratch/sym/linux-2.6.29/include/linux/pm.h \
    $(wildcard include/config/pm/sleep.h) \
  /scratch/sym/linux-2.6.29/include/linux/delay.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/delay.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/fixmap.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/fixmap_64.h \
    $(wildcard include/config/provide/ohci1394/dma/init.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/acpi.h \
    $(wildcard include/config/acpi/numa.h) \
  /scratch/sym/linux-2.6.29/include/acpi/pdc_intel.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/numa.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/numa_64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/apicdef.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/mmu.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/vsyscall.h \
    $(wildcard include/config/generic/time.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/io_apic.h \
    $(wildcard include/config/pci.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/irq_vectors.h \
    $(wildcard include/config/x86/voyager.h) \
    $(wildcard include/config/sparse/irq.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/topology.h \
    $(wildcard include/config/x86/ht.h) \
    $(wildcard include/config/x86/64/acpi/numa.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/topology.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/mmzone.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/mmzone_64.h \
    $(wildcard include/config/numa/emu.h) \
  /scratch/sym/linux-2.6.29/include/linux/mmdebug.h \
    $(wildcard include/config/debug/vm.h) \
    $(wildcard include/config/debug/virtual.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/sparsemem.h \
  /scratch/sym/linux-2.6.29/include/linux/slub_def.h \
    $(wildcard include/config/slub/stats.h) \
    $(wildcard include/config/slub/debug.h) \
  /scratch/sym/linux-2.6.29/include/linux/workqueue.h \
  /scratch/sym/linux-2.6.29/include/linux/timer.h \
    $(wildcard include/config/timer/stats.h) \
    $(wildcard include/config/debug/objects/timers.h) \
  /scratch/sym/linux-2.6.29/include/linux/ktime.h \
    $(wildcard include/config/ktime/scalar.h) \
  /scratch/sym/linux-2.6.29/include/linux/time.h \
  /scratch/sym/linux-2.6.29/include/linux/math64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/div64.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/div64.h \
  /scratch/sym/linux-2.6.29/include/linux/jiffies.h \
  /scratch/sym/linux-2.6.29/include/linux/timex.h \
    $(wildcard include/config/no/hz.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/timex.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/tsc.h \
    $(wildcard include/config/x86/tsc.h) \
  /scratch/sym/linux-2.6.29/include/linux/debugobjects.h \
    $(wildcard include/config/debug/objects/free.h) \
  /scratch/sym/linux-2.6.29/include/linux/kobject.h \
  /scratch/sym/linux-2.6.29/include/linux/sysfs.h \
    $(wildcard include/config/sysfs.h) \
  /scratch/sym/linux-2.6.29/include/linux/kref.h \
  /scratch/sym/linux-2.6.29/include/linux/module.h \
    $(wildcard include/config/modversions.h) \
    $(wildcard include/config/unused/symbols.h) \
    $(wildcard include/config/kallsyms.h) \
    $(wildcard include/config/markers.h) \
    $(wildcard include/config/tracepoints.h) \
    $(wildcard include/config/module/unload.h) \
  /scratch/sym/linux-2.6.29/include/linux/stat.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/stat.h \
  /scratch/sym/linux-2.6.29/include/linux/kmod.h \
  /scratch/sym/linux-2.6.29/include/linux/elf.h \
  /scratch/sym/linux-2.6.29/include/linux/elf-em.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/elf.h \
    $(wildcard include/config/compat/vdso.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/user.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/user_64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/auxvec.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/vdso.h \
  /scratch/sym/linux-2.6.29/include/linux/moduleparam.h \
    $(wildcard include/config/alpha.h) \
    $(wildcard include/config/ia64.h) \
    $(wildcard include/config/ppc64.h) \
  /scratch/sym/linux-2.6.29/include/linux/marker.h \
  /scratch/sym/linux-2.6.29/include/linux/tracepoint.h \
  /scratch/sym/linux-2.6.29/include/linux/rcupdate.h \
    $(wildcard include/config/classic/rcu.h) \
    $(wildcard include/config/tree/rcu.h) \
    $(wildcard include/config/preempt/rcu.h) \
  /scratch/sym/linux-2.6.29/include/linux/percpu.h \
  /scratch/sym/linux-2.6.29/include/linux/completion.h \
  /scratch/sym/linux-2.6.29/include/linux/rcuclassic.h \
    $(wildcard include/config/rcu/cpu/stall/detector.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/local.h \
    $(wildcard include/config/m386.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/module.h \
    $(wildcard include/config/m486.h) \
    $(wildcard include/config/m586.h) \
    $(wildcard include/config/m586tsc.h) \
    $(wildcard include/config/m586mmx.h) \
    $(wildcard include/config/mcore2.h) \
    $(wildcard include/config/m686.h) \
    $(wildcard include/config/mpentiumii.h) \
    $(wildcard include/config/mpentiumiii.h) \
    $(wildcard include/config/mpentiumm.h) \
    $(wildcard include/config/mpentium4.h) \
    $(wildcard include/config/mk6.h) \
    $(wildcard include/config/mk8.h) \
    $(wildcard include/config/x86/elan.h) \
    $(wildcard include/config/mcrusoe.h) \
    $(wildcard include/config/mefficeon.h) \
    $(wildcard include/config/mwinchipc6.h) \
    $(wildcard include/config/mwinchip3d.h) \
    $(wildcard include/config/mcyrixiii.h) \
    $(wildcard include/config/mviac3/2.h) \
    $(wildcard include/config/mviac7.h) \
    $(wildcard include/config/mgeodegx1.h) \
    $(wildcard include/config/mgeode/lx.h) \
    $(wildcard include/config/4kstacks.h) \
  /scratch/sym/ipc_miscdevice/list.h \
  /scratch/sym/ipc_miscdevice/errno.h \
  /scratch/sym/ipc_miscdevice/nooks/nooks-hash.h \
  /scratch/sym/ipc_miscdevice/nooks/nooks-i.h \
  /scratch/sym/ipc_miscdevice/nooks/nooks-range-query.h \
  /scratch/sym/linux-2.6.29/include/linux/sched.h \
    $(wildcard include/config/sched/debug.h) \
    $(wildcard include/config/detect/softlockup.h) \
    $(wildcard include/config/core/dump/default/elf/headers.h) \
    $(wildcard include/config/bsd/process/acct.h) \
    $(wildcard include/config/taskstats.h) \
    $(wildcard include/config/audit.h) \
    $(wildcard include/config/inotify/user.h) \
    $(wildcard include/config/epoll.h) \
    $(wildcard include/config/posix/mqueue.h) \
    $(wildcard include/config/keys.h) \
    $(wildcard include/config/user/sched.h) \
    $(wildcard include/config/schedstats.h) \
    $(wildcard include/config/task/delay/acct.h) \
    $(wildcard include/config/fair/group/sched.h) \
    $(wildcard include/config/rt/group/sched.h) \
    $(wildcard include/config/blk/dev/io/trace.h) \
    $(wildcard include/config/sysvipc.h) \
    $(wildcard include/config/auditsyscall.h) \
    $(wildcard include/config/rt/mutexes.h) \
    $(wildcard include/config/task/xacct.h) \
    $(wildcard include/config/cpusets.h) \
    $(wildcard include/config/cgroups.h) \
    $(wildcard include/config/futex.h) \
    $(wildcard include/config/fault/injection.h) \
    $(wildcard include/config/latencytop.h) \
    $(wildcard include/config/tracing.h) \
    $(wildcard include/config/have/unstable/sched/clock.h) \
    $(wildcard include/config/preempt/bkl.h) \
    $(wildcard include/config/group/sched.h) \
    $(wildcard include/config/mm/owner.h) \
  /scratch/sym/linux-2.6.29/include/linux/capability.h \
    $(wildcard include/config/security/file/capabilities.h) \
  /scratch/sym/linux-2.6.29/include/linux/rbtree.h \
  /scratch/sym/linux-2.6.29/include/linux/mm_types.h \
    $(wildcard include/config/split/ptlock/cpus.h) \
    $(wildcard include/config/mmu.h) \
    $(wildcard include/config/proc/fs.h) \
    $(wildcard include/config/mmu/notifier.h) \
  /scratch/sym/linux-2.6.29/include/linux/auxvec.h \
  /scratch/sym/linux-2.6.29/include/linux/prio_tree.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/cputime.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/cputime.h \
  /scratch/sym/linux-2.6.29/include/linux/sem.h \
  /scratch/sym/linux-2.6.29/include/linux/ipc.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ipcbuf.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/sembuf.h \
  /scratch/sym/linux-2.6.29/include/linux/signal.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/signal.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/signal.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/siginfo.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/siginfo.h \
  /scratch/sym/linux-2.6.29/include/linux/fs_struct.h \
  /scratch/sym/linux-2.6.29/include/linux/path.h \
  /scratch/sym/linux-2.6.29/include/linux/pid.h \
  /scratch/sym/linux-2.6.29/include/linux/proportions.h \
  /scratch/sym/linux-2.6.29/include/linux/percpu_counter.h \
  /scratch/sym/linux-2.6.29/include/linux/seccomp.h \
    $(wildcard include/config/seccomp.h) \
  /scratch/sym/linux-2.6.29/include/linux/rtmutex.h \
    $(wildcard include/config/debug/rt/mutexes.h) \
  /scratch/sym/linux-2.6.29/include/linux/plist.h \
    $(wildcard include/config/debug/pi/list.h) \
  /scratch/sym/linux-2.6.29/include/linux/resource.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/resource.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/resource.h \
  /scratch/sym/linux-2.6.29/include/linux/hrtimer.h \
    $(wildcard include/config/high/res/timers.h) \
  /scratch/sym/linux-2.6.29/include/linux/task_io_accounting.h \
    $(wildcard include/config/task/io/accounting.h) \
  /scratch/sym/linux-2.6.29/include/linux/latencytop.h \
  /scratch/sym/linux-2.6.29/include/linux/cred.h \
    $(wildcard include/config/security.h) \
  /scratch/sym/linux-2.6.29/include/linux/key.h \
    $(wildcard include/config/sysctl.h) \
  /scratch/sym/linux-2.6.29/include/linux/sysctl.h \
  /scratch/sym/linux-2.6.29/include/linux/aio.h \
    $(wildcard include/config/aio.h) \
  /scratch/sym/linux-2.6.29/include/linux/aio_abi.h \
  /scratch/sym/linux-2.6.29/include/linux/uio.h \
  /scratch/sym/linux-2.6.29/include/linux/fs.h \
    $(wildcard include/config/dnotify.h) \
    $(wildcard include/config/quota.h) \
    $(wildcard include/config/inotify.h) \
    $(wildcard include/config/debug/writecount.h) \
    $(wildcard include/config/file/locking.h) \
    $(wildcard include/config/block.h) \
    $(wildcard include/config/fs/xip.h) \
    $(wildcard include/config/migration.h) \
  /scratch/sym/linux-2.6.29/include/linux/limits.h \
  /scratch/sym/linux-2.6.29/include/linux/ioctl.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ioctl.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/ioctl.h \
  /scratch/sym/linux-2.6.29/include/linux/kdev_t.h \
  /scratch/sym/linux-2.6.29/include/linux/dcache.h \
  /scratch/sym/linux-2.6.29/include/linux/rculist.h \
  /scratch/sym/linux-2.6.29/include/linux/radix-tree.h \
  /scratch/sym/linux-2.6.29/include/linux/semaphore.h \
  /scratch/sym/linux-2.6.29/include/linux/fiemap.h \
  /scratch/sym/linux-2.6.29/include/linux/quota.h \
  /scratch/sym/linux-2.6.29/include/linux/dqblk_xfs.h \
  /scratch/sym/linux-2.6.29/include/linux/dqblk_v1.h \
  /scratch/sym/linux-2.6.29/include/linux/dqblk_v2.h \
  /scratch/sym/linux-2.6.29/include/linux/dqblk_qtree.h \
  /scratch/sym/linux-2.6.29/include/linux/nfs_fs_i.h \
  /scratch/sym/linux-2.6.29/include/linux/nfs.h \
  /scratch/sym/linux-2.6.29/include/linux/sunrpc/msg_prot.h \
  /scratch/sym/linux-2.6.29/include/linux/fcntl.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/fcntl.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/fcntl.h \
  /scratch/sym/linux-2.6.29/include/linux/miscdevice.h \
  /scratch/sym/linux-2.6.29/include/linux/major.h \
  /scratch/sym/linux-2.6.29/include/linux/vmalloc.h \
  /scratch/sym/linux-2.6.29/include/linux/mm.h \
    $(wildcard include/config/stack/growsup.h) \
    $(wildcard include/config/swap.h) \
    $(wildcard include/config/shmem.h) \
    $(wildcard include/config/debug/pagealloc.h) \
    $(wildcard include/config/hibernation.h) \
  /scratch/sym/linux-2.6.29/include/linux/debug_locks.h \
    $(wildcard include/config/debug/locking/api/selftests.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/pgtable.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/pgtable_64.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/pgtable.h \
  /scratch/sym/linux-2.6.29/include/linux/page-flags.h \
    $(wildcard include/config/pageflags/extended.h) \
    $(wildcard include/config/ia64/uncached/allocator.h) \
    $(wildcard include/config/s390.h) \
  /scratch/sym/linux-2.6.29/include/linux/vmstat.h \
    $(wildcard include/config/vm/event/counters.h) \
  /scratch/sym/linux-2.6.29/include/linux/uaccess.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/uaccess.h \
    $(wildcard include/config/x86/wp/works/ok.h) \
    $(wildcard include/config/x86/intel/usercopy.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/uaccess_64.h \
  /scratch/sym/linux-2.6.29/include/linux/interrupt.h \
    $(wildcard include/config/generic/irq/probe.h) \
  /scratch/sym/linux-2.6.29/include/linux/irqreturn.h \
  /scratch/sym/linux-2.6.29/include/linux/irqnr.h \
  /scratch/sym/linux-2.6.29/include/linux/hardirq.h \
    $(wildcard include/config/virt/cpu/accounting.h) \
  /scratch/sym/linux-2.6.29/include/linux/smp_lock.h \
    $(wildcard include/config/lock/kernel.h) \
  /scratch/sym/linux-2.6.29/include/linux/ftrace_irq.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/hardirq.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/hardirq_64.h \
  /scratch/sym/linux-2.6.29/include/linux/irq.h \
    $(wildcard include/config/irq/per/cpu.h) \
    $(wildcard include/config/irq/release/method.h) \
    $(wildcard include/config/intr/remap.h) \
    $(wildcard include/config/generic/pending/irq.h) \
    $(wildcard include/config/numa/migrate/irq/desc.h) \
    $(wildcard include/config/generic/hardirqs/no//do/irq.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/irq.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/irq_regs.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/irq_regs_64.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/irq_regs.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/hw_irq.h \
    $(wildcard include/config/x86/smp.h) \
  /scratch/sym/linux-2.6.29/include/linux/profile.h \
    $(wildcard include/config/profiling.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/sections.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/sections.h \
  /scratch/sym/linux-2.6.29/include/linux/io.h \
    $(wildcard include/config/has/ioport.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/io.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/io_64.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/iomap.h \
  /scratch/sym/linux-2.6.29/include/linux/dma-mapping.h \
    $(wildcard include/config/has/dma.h) \
    $(wildcard include/config/have/dma/attrs.h) \
  /scratch/sym/linux-2.6.29/include/linux/device.h \
    $(wildcard include/config/debug/devres.h) \
  /scratch/sym/linux-2.6.29/include/linux/ioport.h \
  /scratch/sym/linux-2.6.29/include/linux/klist.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/device.h \
    $(wildcard include/config/dmar.h) \
  /scratch/sym/linux-2.6.29/include/linux/pm_wakeup.h \
    $(wildcard include/config/pm.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/dma-mapping.h \
  /scratch/sym/linux-2.6.29/include/linux/scatterlist.h \
    $(wildcard include/config/debug/sg.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/scatterlist.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/swiotlb.h \
    $(wildcard include/config/swiotlb.h) \
  /scratch/sym/linux-2.6.29/include/linux/swiotlb.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/dma-coherent.h \
    $(wildcard include/config/have/generic/dma/coherent.h) \
  /scratch/sym/ipc_miscdevice/misc.h \
  /scratch/sym/ipc_miscdevice/../common/ud_md.h \
  /scratch/sym/ipc_miscdevice/../common/slave_master_ud_md.h \
  /scratch/sym/ipc_miscdevice/../common/slave_master_ud_md.h \
  /scratch/sym/ipc_miscdevice/wrappers_nooks.h \
  /scratch/sym/ipc_miscdevice/wrappers_alloc.h \
  /scratch/sym/ipc_miscdevice/wrappers_misc.h \
  /scratch/sym/ipc_miscdevice/rec_lock.h \
  /scratch/sym/ipc_miscdevice/marsh_alloc.h \
  /scratch/sym/linux-2.6.29/include/linux/pci.h \
    $(wildcard include/config/pcieaspm.h) \
    $(wildcard include/config/pci/msi.h) \
    $(wildcard include/config/pci/legacy.h) \
    $(wildcard include/config/ht/irq.h) \
    $(wildcard include/config/pci/domains.h) \
    $(wildcard include/config/pci/mmconfig.h) \
  /scratch/sym/linux-2.6.29/include/linux/pci_regs.h \
  /scratch/sym/linux-2.6.29/include/linux/mod_devicetable.h \
  /scratch/sym/linux-2.6.29/include/linux/pci_ids.h \
  /scratch/sym/linux-2.6.29/include/linux/dmapool.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/pci.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/pci_64.h \
    $(wildcard include/config/calgary/iommu.h) \
  /scratch/sym/linux-2.6.29/include/asm-generic/pci-dma-compat.h \
  /scratch/sym/linux-2.6.29/include/asm-generic/pci.h \
  /scratch/sym/linux-2.6.29/include/linux/bug.h \
  /scratch/sym/linux-2.6.29/include/linux/kallsyms.h \
  /scratch/sym/linux-2.6.29/include/linux/rtnetlink.h \
  /scratch/sym/linux-2.6.29/include/linux/netlink.h \
  /scratch/sym/linux-2.6.29/include/linux/socket.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/socket.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/sockios.h \
  /scratch/sym/linux-2.6.29/include/linux/sockios.h \
  /scratch/sym/linux-2.6.29/include/linux/skbuff.h \
    $(wildcard include/config/nf/conntrack.h) \
    $(wildcard include/config/bridge/netfilter.h) \
    $(wildcard include/config/xfrm.h) \
    $(wildcard include/config/net/sched.h) \
    $(wildcard include/config/net/cls/act.h) \
    $(wildcard include/config/ipv6/ndisc/nodetype.h) \
    $(wildcard include/config/mac80211.h) \
    $(wildcard include/config/net/dma.h) \
    $(wildcard include/config/network/secmark.h) \
  /scratch/sym/linux-2.6.29/include/linux/net.h \
  /scratch/sym/linux-2.6.29/include/linux/random.h \
  /scratch/sym/linux-2.6.29/include/linux/textsearch.h \
  /scratch/sym/linux-2.6.29/include/net/checksum.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/checksum.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/checksum_64.h \
  /scratch/sym/linux-2.6.29/include/linux/dmaengine.h \
    $(wildcard include/config/dma/engine.h) \
  /scratch/sym/linux-2.6.29/include/linux/if_link.h \
  /scratch/sym/linux-2.6.29/include/linux/if_addr.h \
  /scratch/sym/linux-2.6.29/include/linux/neighbour.h \
  /scratch/sym/linux-2.6.29/include/linux/suspend.h \
    $(wildcard include/config/frv.h) \
    $(wildcard include/config/ppc32.h) \
    $(wildcard include/config/vt.h) \
    $(wildcard include/config/vt/console.h) \
    $(wildcard include/config/suspend.h) \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/suspend.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/suspend_64.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/desc.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/ldt.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/i387.h \
  /scratch/sym/linux-2.6.29/include/linux/kernel_stat.h \
  /scratch/sym/linux-2.6.29/include/linux/regset.h \
  /scratch/sym/linux-2.6.29/arch/x86/include/asm/xsave.h \
  /scratch/sym/linux-2.6.29/include/linux/swap.h \
    $(wildcard include/config/cgroup/mem/res/ctlr/swap.h) \
  /scratch/sym/linux-2.6.29/include/linux/memcontrol.h \
    $(wildcard include/config/cgroup/mem/cont.h) \
  /scratch/sym/linux-2.6.29/include/linux/cgroup.h \
  /scratch/sym/linux-2.6.29/include/linux/cgroupstats.h \
  /scratch/sym/linux-2.6.29/include/linux/taskstats.h \
  /scratch/sym/linux-2.6.29/include/linux/prio_heap.h \
  /scratch/sym/linux-2.6.29/include/linux/node.h \
  /scratch/sym/linux-2.6.29/include/linux/sysdev.h \

/scratch/sym/ipc_miscdevice/misc.o: $(deps_/scratch/sym/ipc_miscdevice/misc.o)

$(deps_/scratch/sym/ipc_miscdevice/misc.o):
