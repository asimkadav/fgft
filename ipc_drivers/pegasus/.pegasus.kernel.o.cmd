cmd_/scratch/sym/ipc_drivers/pegasus/pegasus.kernel.o := gcc -Wp,-MD,/scratch/sym/ipc_drivers/pegasus/.pegasus.kernel.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-redhat-linux/4.1.2/include -Iinclude  -I/scratch/sym/annotated/linux-2.6.29-ipc-pegasus/arch/x86/include -include include/linux/autoconf.h -D__KERNEL__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -m64 -mtune=generic -mno-red-zone -mcmodel=kernel -funit-at-a-time -maccumulate-outgoing-args -DCONFIG_AS_CFI=1 -DCONFIG_AS_CFI_SIGNAL_FRAME=1 -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iarch/x86/include/asm/mach-default -fno-stack-protector -fno-omit-frame-pointer -fno-optimize-sibling-calls -g -Wdeclaration-after-statement -Wno-pointer-sign -fwrapv -g -Wall -Wno-attributes -Wno-unknown-pragmas -DMODULE -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(pegasus.kernel)"  -D"KBUILD_MODNAME=KBUILD_STR(pegasus_stub)"  -c -o /scratch/sym/ipc_drivers/pegasus/pegasus.kernel.o /scratch/sym/ipc_drivers/pegasus/pegasus.kernel.c

deps_/scratch/sym/ipc_drivers/pegasus/pegasus.kernel.o := \
  /scratch/sym/ipc_drivers/pegasus/pegasus.kernel.c \
  /scratch/sym/ipc_drivers/pegasus/../../common/master_top.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/full_slab_verify.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/demarshbuf_free.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/slave_master_ud_md_marshaling.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/slave_master_ud_md.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/MJR_external_functions.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/master_md_nooks_api.h \
  /scratch/sym/ipc_drivers/pegasus/../../common/master_init_module.h \

/scratch/sym/ipc_drivers/pegasus/pegasus.kernel.o: $(deps_/scratch/sym/ipc_drivers/pegasus/pegasus.kernel.o)

$(deps_/scratch/sym/ipc_drivers/pegasus/pegasus.kernel.o):
