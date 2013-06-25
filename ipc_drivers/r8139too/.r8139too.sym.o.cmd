cmd_/scratch/sym/ipc_drivers/r8139too/r8139too.sym.o := gcc -Wp,-MD,/scratch/sym/ipc_drivers/r8139too/.r8139too.sym.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include -Iinclude  -I/scratch/sym/annotated/linux-2.6.29-ipc-r8139too/arch/x86/include -include include/linux/autoconf.h -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iarch/x86/include/asm/mach-default -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -g -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -g -Wall -Wno-attributes -Wno-unknown-pragmas -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(r8139too.sym)"  -D"KBUILD_MODNAME=KBUILD_STR(r8139too_sfi)"  -c -o /scratch/sym/ipc_drivers/r8139too/r8139too.sym.o /scratch/sym/ipc_drivers/r8139too/r8139too.sym.c

deps_/scratch/sym/ipc_drivers/r8139too/r8139too.sym.o := \
  /scratch/sym/ipc_drivers/r8139too/r8139too.sym.c \
  /scratch/sym/ipc_drivers/r8139too/../../common/slave_top.h \
    $(wildcard include/config/base/small.h) \
  /scratch/sym/ipc_drivers/r8139too/../../common/slave_master_ud_md.h \
  /scratch/sym/ipc_drivers/r8139too/../../common/slave_master_ud_md_marshaling.h \
  /scratch/sym/ipc_drivers/r8139too/../../common/wrappers_nooks.h \
  /scratch/sym/ipc_drivers/r8139too/../../common/demarshbuf_free.h \

/scratch/sym/ipc_drivers/r8139too/r8139too.sym.o: $(deps_/scratch/sym/ipc_drivers/r8139too/r8139too.sym.o)

$(deps_/scratch/sym/ipc_drivers/r8139too/r8139too.sym.o):
