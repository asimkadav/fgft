cmd_/scratch/sym/ipc_drivers/e1000/e1000.sym.o := gcc -Wp,-MD,/scratch/sym/ipc_drivers/e1000/.e1000.sym.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.4.7/include -Iinclude  -I/scratch/sym/annotated/linux-2.6.29-ipc-e1000/arch/x86/include -include include/linux/autoconf.h -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iarch/x86/include/asm/mach-default -Wframe-larger-than=2048 -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -g -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -fno-dwarf2-cfi-asm -g -Wall -Wno-attributes -Wno-unknown-pragmas -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(e1000.sym)"  -D"KBUILD_MODNAME=KBUILD_STR(e1000_sfi)"  -c -o /scratch/sym/ipc_drivers/e1000/e1000.sym.o /scratch/sym/ipc_drivers/e1000/e1000.sym.c

deps_/scratch/sym/ipc_drivers/e1000/e1000.sym.o := \
  /scratch/sym/ipc_drivers/e1000/e1000.sym.c \
  /scratch/sym/ipc_drivers/e1000/../../common/slave_top.h \
    $(wildcard include/config/base/small.h) \
  /scratch/sym/ipc_drivers/e1000/../../common/slave_master_ud_md.h \
  /scratch/sym/ipc_drivers/e1000/../../common/slave_master_ud_md_marshaling.h \
  /scratch/sym/ipc_drivers/e1000/../../common/wrappers_nooks.h \
  /scratch/sym/ipc_drivers/e1000/../../common/demarshbuf_free.h \

/scratch/sym/ipc_drivers/e1000/e1000.sym.o: $(deps_/scratch/sym/ipc_drivers/e1000/e1000.sym.o)

$(deps_/scratch/sym/ipc_drivers/e1000/e1000.sym.o):
