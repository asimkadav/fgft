# To generate a real kernel module rather than a split
# driver, just build it here and edit the source file to
# include the appropriate #define statement (very top
# of usb_test_annotated.c)

KERNEL_VERSION = 2.6.29-uml-ipc

.PHONY: clean

obj-m := pegasus_annotated.o

all:
	make -C ../../../compiled_images/$(KERNEL_VERSION) M=`pwd` ARCH=um  modules

clean:
	rm -rf *.o *.ko \.*.o.d \.*cmd *.i .tmp_versions Module.symvers *.mod.c modules.order
	rm -f *.tmp
	rm -f output.txt
