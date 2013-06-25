cmd_/scratch/sym/ipc_drivers/r8169/r8169.sym.o := gcc -Wp,-MD,/scratch/sym/ipc_drivers/r8169/.r8169.sym.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include -Iinclude  -I/scratch/sym/annotated/linux-2.6.29-ipc-r8169/arch/x86/include -include include/linux/autoconf.h -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iarch/x86/include/asm/mach-default -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -g -Wall -Wno-attributes -Wno-unknown-pragmas -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(r8169.sym)"  -D"KBUILD_MODNAME=KBUILD_STR(r8169_sfi)"  -c -o /scratch/sym/ipc_drivers/r8169/r8169.sym.o /scratch/sym/ipc_drivers/r8169/r8169.sym.c

deps_/scratch/sym/ipc_drivers/r8169/r8169.sym.o := \
  /scratch/sym/ipc_drivers/r8169/r8169.sym.c \
  /scratch/sym/ipc_drivers/r8169/../../common/slave_top.h \
    $(wildcard include/config/base/small.h) \
  /scratch/sym/ipc_drivers/r8169/../../common/slave_master_ud_md.h \
  /scratch/sym/ipc_drivers/r8169/../../common/slave_master_ud_md_marshaling.h \
  /scratch/sym/ipc_drivers/r8169/../../common/wrappers_nooks.h \
  /scratch/sym/ipc_drivers/r8169/../../common/demarshbuf_free.h \

/scratch/sym/ipc_drivers/r8169/r8169.sym.o: $(deps_/scratch/sym/ipc_drivers/r8169/r8169.sym.o)

$(deps_/scratch/sym/ipc_drivers/r8169/r8169.sym.o):
