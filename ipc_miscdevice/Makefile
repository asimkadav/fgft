# This makefile builds both the miscellaneous device, and the custom PCI bus driver.

KERNEL_VERSION = 2.6.29-tuxedo
# DEBUG = y

#EXTRA_CFLAGS += -fno-inline -Os -g -Wall $(DEFINE_ENABLE_UPRINTK)
EXTRA_CFLAGS += -Wno-attributes 

obj-m := mischelp.o \
#	mpci-bus.o \
#	module-format.o 
#	musb-core.o \
#	musb-gadget.o \
#	musb-bulk.o \
#	musb-net.o \
#	musb-sound.o

# Misc device, provides all the glue code for communication and object tracking
mischelp-objs := misc.o
#mischelp-objs += wrappers_alloc.o wrappers_misc.o wrappers_nooks.o
mischelp-objs += wrappers_misc.o wrappers_nooks.o
mischelp-objs += md_marshaling.o rec_lock.o
#mischelp-objs += MJR_external_functions.o MJR_wrappers.o
# kernel_timer.o
mischelp-objs += nooks/nooks-hash.o nooks/nooks-pool.o nooks/nooks-kern-api.o
mischelp-objs += nooks/nooks-range-query.o
mischelp-objs += hashtable/hashtable.o hashtable/range_tracker.o hashtable/klog.o  marsh_alloc.o
#mischelp-objs += swfi/db_sym.o 

# The fake PCI bus, also provides the fake device
#mpci-bus-objs := mpci.o
#musb-bus-objs := musb.o
#musb-core-objs := core/usb.o core/hub.o core/hcd.o core/urb.o core/message.o core/driver.o \
	core/config.o core/file.o core/buffer.o core/sysfs.o core/endpoint.o \
	core/devio.o core/notify.o core/generic.o core/quirks.o
#musb-gadget-objs := gadget/dummy_hcd.o 
#musb-bulk-objs   := gadget/file_storage.o 
#musb-net-objs    := gadget/ether.o
#musb-sound-objs  := gadget/gmidi.o

# A trivial module that we load automatically when UML starts.
# The idea is to check if the reboot will give us the
# "Invalid module format" error or not.
module-format-objs := mformat.o

all:
	$(MAKE) -C ../compiled_images/$(KERNEL_VERSION) M=`pwd` modules
	#gcc $(EXTRA_FLAGS) mischelper.c -o mischelper.o
	#gcc mischelper.o -o mischelper
	gcc mischelper.c 
	rm -f mischelper
	mv a.out mischelper
	chmod 555 mischelper

clean:
	rm -f *.o *.ko *.mod.c Module.symvers .mischelp.* .misc.o.d modules.order
	rm -rf ./.tmp_versions
	rm -rf ./nooks/*.o ./nooks/\.*cmd
	rm -rf ./core/*.o ./core/\.*cmd
	rm -rf ./gadget/*.o ./gadget/\.*cmd
	rm -f \.*.cmd
	rm -f mischelper
	rm -rf ./hashtable/*.o
