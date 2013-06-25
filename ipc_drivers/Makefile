export USE_LLVM=false
export USE_KLEE=false
export SPEED_KLEE=false
export DRIVER_TEST=false
###############################################################################
# Choose one:
# Use -pg to enable profiling
#export OPTIMIZATION_LEVEL=-O0 -g
#export OPTIMIZATION_LEVEL=-O1 -g
export OPTIMIZATION_LEVEL=-O2 -g

# Try using O0 without -g when using LLVM if problems arise
# ens1371 seems to require this or the interpreter crashes
#export OPTIMIZATION_LEVEL=-O2
###############################################################################
# Either comment or uncomment this line:
# This feature is used for debugging CIL/DriverSlicer crashes
# --bytecode gives us stack traces if DriverSlicer crashes
#export EXTRA_CILLY_FLAGS=--bytecode --verbose
export EXTRA_CILLY_FLAGS=--verbose
###############################################################################
# BECONS
# - DriverSlicer parameter.  Are we going to generate "conservative"
#   kernel function marshaling code, or not?
# - Probably should be false usually -- only generate marshaling code
#   for functions specified in the modif annotations.
# - Set to false when setting up a new driver to get the
#   necessary "recursive" annotations.
# Conservative:  marshal EVERYTHING if the function is not annotated (modif_annots.h)
# Conservative:  BECONS=TRUE
#
# Not conservative:  marshal NOTHING if the function is not annotated.
# "Not conservative" is MUCH faster in some cases, particularly if modif_annots
# is incomplete, because we avoid generating a lot of marshaling code.
# Not conservative:  BECONS=FALSE
#
# BECONS is now set below when you comment/uncomment the desired driver
#
###############################################################################
# Do function pointer analysis.
#
# Note that this feature causes problems with the sound library in its present
# implementation.
#

# Do we need this on for any drivers?
#export DOFUNCPTR=--dofuncptr=true

# 8139too
# ca0106
# cmipci
# e1000
# ens1371
# pegasus
# tg3
# usb-audio
export DOFUNCPTR=--dofuncptr=false
###############################################################################
# Do void pointer analysis.  It may make sense to disable this
# more broadly in symdrive since we almost never want to marshal a void
# pointer even if we do know/can infer what it is.

# 8139too
# ca0106
# cmipci
# e1000
# ens1371
# pegasus
# tg3
# usb-audio
export DOVOIDPTR=--dovoidptr=false
###############################################################################
# Enable this if you want lots of debugging output.
# Disable it if you want relatively little.
export ENABLE_UPRINTK=true
#export ENABLE_UPRINTK=false
###############################################################################
# Enable or disable refactoring support
#export ENABLE_REFACTORING=true
export ENABLE_REFACTORING=false
###############################################################################
# Specify the objects that make up this driver here

# Sound modules.  Do we need these or not?
# sound/control_compat.c
# sound/hwdep_compat.c
# sound/pcm_compat.c
# sound/rawmidi_compat.c
# sound/isadma.c.bak
# sound/jack.c.bak

# Included elsewhere
# sound/ac97_patch.c
# sound/core/seq/seq_compat.c

# Not compiling yet:
# sound/core/rtctimer.c

# OPL3 for CMIPCI that we're not using?
#    sound/drivers/opl3/opl3_midi.c
#    sound/drivers/opl3/opl3_drums.c
#    sound/drivers/opl3/opl3_seq.c

#export BECONS=false
#export DRIVER_SOUND=false
#export DRIVER_NAME=r8139too
#export DRIVER_SRC=r8139too_annotated.c

#export BECONS=true
#export DRIVER_SOUND=true
#export DRIVER_NAME=ca0106
#export DRIVER_SRC=ca0106_mixer.c ca0106_main.c ca0106_proc.c ca_midi.c $(SOUND_MODULES)

#export BECONS=false
#export DRIVER_SOUND=false
#export DRIVER_NAME=cmipci
#export DRIVER_SRC=cmipci_annotated.c $(SOUND_MODULES)

#export BECONS=false
#export DRIVER_SOUND=false
#export DRIVER_NAME=r8169
#export DRIVER_SRC=r8169_annotated.c

export BECONS=false
export DRIVER_SOUND=false
export DRIVER_NAME=e1000
export DRIVER_SRC=e1000_ethtool.c e1000_main.c e1000_hw.c

#export BECONS=true
#export DRIVER_SOUND=false
#export DRIVER_NAME=forcedeth
#export DRIVER_SRC=forcedeth_annotated.c

#export BECONS=true
#export DRIVER_SOUND=false
#export DRIVER_NAME=psmousebase
#export DRIVER_SRC= psmousebase_annotated.c alps.c synaptics.c lifebook.c logips2pp.c trackpoint.c

#export BECONS=false
#export DRIVER_SOUND=true
#export DRIVER_NAME=ens1371
#export DRIVER_SRC=ens1371_annotated.c $(SOUND_MODULES)

#export BECONS=true
#export DRIVER_SOUND=false
#export DRIVER_NAME=pci_test
#export DRIVER_SRC=pci_test_annotated.c

#export BECONS=true
#export DRIVER_SOUND=false
#export DRIVER_NAME=pegasus
#export DRIVER_SRC=pegasus_annotated.c

#export BECONS=true
#export DRIVER_SOUND=false
#export DRIVER_NAME=tg3
#export DRIVER_SRC=tg3_annotated.c

#export BECONS=false
#export DRIVER_SOUND=true
#export DRIVER_NAME=usb-audio
#export DRIVER_SRC=usbaudio_annotated.c usbmidi.c usbmixer.c $(SOUND_MODULES)

#export BECONS=false
#export DRIVER_SOUND=false
#export DRIVER_NAME=usb-storage
#export DRIVER_SRC=scsiglue.c protocol.c transport.c usb.c initializers.c
#alauda.c datafab.c freecom.c isd200.c karma.c usb.c onetouch.c protocol.c sddr09.c shuttle_usbat.c  transport.c cypress_atacb.c  debug.c initializers.c  jumpshot.c option_ms.c scsiglue.c sddr55.c sierra_ms.c  option_ms.c

#export BECONS=true
#export DRIVER_SOUND=false
#export DRIVER_NAME=usb_test
#export DRIVER_SRC=usb_test_annotated.c

###############################################################################
# Specify the kernel version here
# BASE_VERSION is the source directory
# VERSION is the compiled object directory
export KERNEL_BASE_VERSION=2.6.29-ipc
export KERNEL_VERSION=2.6.29-uml-ipc
###############################################################################
# Extra compilation flags
ifeq "$(DRIVER_TEST)" "true"
    export DEFINE_DRIVER_TEST=-DDRIVER_TEST
    export DO_DRIVER_TEST=--do-symdriver-test=true
else
    export DEFINE_DRIVER_TEST=
    export DO_DRIVER_TEST=--do-symdriver-test=false
endif

ifeq "$(SPEED_KLEE)" "true"
    export DEFINE_SPEED_KLEE=-DSPEED_KLEE
else
    export DEFINE_SPEED_KLEE=
endif

ifeq "$(ENABLE_UPRINTK)" "true"
    export DEFINE_ENABLE_UPRINTK=-DENABLE_UPRINTK
else
    export DEFINE_ENABLE_UPRINTK=
endif

ifeq "$(ENABLE_REFACTORING)" "true"
    export DEFINE_ENABLE_REFACTORING=-DENABLE_REFACTORING
else
    export DEFINE_ENABLE_REFACTORING=
endif

ifeq "$(USE_KLEE)" "true"
    export DEFINE_USE_KLEE=-DUSE_KLEE
else
    export DEFINE_USE_KLEE=
endif

ifeq "$(DRIVER_SOUND)" "true"
    export DEFINE_DRIVER_SOUND=-DDRIVER_SOUND
else
    export DEFINE_DRIVER_SOUND=
endif

###############
# Test infrastructure
# Related to DEFINE_DRIVER_TEST
###############
ifeq "$(DRIVER_NAME)" "8139too"
    export DEFINE_DISABLE_8139TOO_CHECKS=-DDISABLE_8139TOO_CHECKS
else
    export DEFINE_DISABLE_8139TOO_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "ca0106"
    export DEFINE_DISABLE_CA0106_CHECKS=-DDISABLE_CA0106_CHECKS
else
    export DEFINE_DISABLE_CA0106_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "cmipci"
    export DEFINE_DISABLE_CMIPCI_CHECKS=-DDISABLE_CMIPCI_CHECKS
else
    export DEFINE_DISABLE_CMIPCI_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "e1000"
    export DEFINE_DISABLE_E1000_CHECKS=-DDISABLE_E1000_CHECKS
else
    export DEFINE_DISABLE_E1000_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "ens1371"
    export DEFINE_DISABLE_ENS1371_CHECKS=-DDISABLE_ENS1371_CHECKS
else
    export DEFINE_DISABLE_ENS1371_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "pegasus"
    export DEFINE_DISABLE_PEGASUS_CHECKS=-DDISABLE_PEGASUS_CHECKS
else
    export DEFINE_DISABLE_PEGASUS_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "tg3"
    export DEFINE_DISABLE_TG3_CHECKS=-DDISABLE_TG3_CHECKS
else
    export DEFINE_DISABLE_TG3_CHECKS=
endif

ifeq "$(DRIVER_NAME)" "usb-audio"
    export DEFINE_DISABLE_USB_AUDIO_CHECKS=-DDISABLE_USB_AUDIO_CHECKS
else
    export DEFINE_DISABLE_USB_AUDIO_CHECKS=
endif

export DEFINE_DRIVER_TESTS = \
	$(DEFINE_DISABLE_8139TOO_CHECKS) \
	$(DEFINE_DISABLE_CA0106_CHECKS) \
	$(DEFINE_DISABLE_CMIPCI_CHECKS) \
	$(DEFINE_DISABLE_E1000_CHECKS) \
	$(DEFINE_DISABLE_ENS1371_CHECKS) \
	$(DEFINE_DISABLE_PEGASUS_CHECKS) \
	$(DEFINE_DISABLE_TG3_CHECKS) \
	$(DEFINE_DISABLE_USB_AUDIO_CHECKS)

###############

export EXTRA_FLAGS=-c \
	$(OPTIMIZATION_LEVEL) \
	-Wall -Wno-attributes \
	$(DEFINE_USE_KLEE) \
        $(DEFINE_SPEED_KLEE) \
	$(DEFINE_ENABLE_UPRINTK) \
	$(DEFINE_ENABLE_REFACTORING) \
	$(DEFINE_DRIVER_TEST) \
	$(DEFINE_DRIVER_TESTS) \
        $(DEFINE_DRIVER_SOUND) \
	$(DEFINE_CUSTOM_FN_DEFAULT)

###############################################################################
# Probably don't need to change stuff below here too often
###############################################################################

export FORMAT_DIVIDER="******************************************************************************"
export OUTPUT_MERGED=output_merged.txt
export OUTPUT_ANNOTS=output_all_annots.txt
export OUTPUT_ENTRY_POINTS=output_entry_points.txt
export OUTPUT_SYM=output_sym.txt
export OUTPUT_KERN=output_kernel.txt

ifeq "$(USE_LLVM)" "true"
    # If we're using LLVM, then we need to emit LLVM bitcode
    # We also turn off linker optimizations.  That had some problems
    # in the past -- feel free to turn back on if feeling adventurous
    export UMCC=llvm-gcc -emit-llvm
    export UMLD=llvm-ld -disable-opt
    export LINKER_OPT=
else
    # Otherwise, we'll just use GCC.
    export UMCC=gcc
    export UMLD=gcc
    export LINKER_OPT=$(OPTIMIZATION_LEVEL)
endif

# We always do UM
# This is passed to the kernel build system
#export ARCH=um  //ASIM
export arch=x64

# Relative to ipc_userdaemon
export DRIVER_ROOT=../ipc_drivers/$(DRIVER_NAME)
export MISCHELP_ROOT=../ipc_miscdevice

# Kernel paths, relative to base of trees
# This is extremely complex because of the scattered locations of UML
# header files.  We need these header locations for just a few
# files, e.g. wrappers_alloc, that make use of kernel functionality in
# user mode.  Unfortunately, #including kernel files in user mode
# is generally a no-no, which is why need this hack.
BASE_KERNEL_ANNOTATED_PATH=annotated/linux-$(KERNEL_BASE_VERSION)-$(DRIVER_NAME)
BASE_KERNEL_ANNOTATED_HEADERS=$(BASE_KERNEL_ANNOTATED_PATH)/include
BASE_KERNEL_ANNOTATED_HEADERS_ARCH=$(BASE_KERNEL_ANNOTATED_PATH)/arch/um/include
BASE_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED=$(BASE_KERNEL_ANNOTATED_PATH)/arch/um/include/shared
BASE_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED_SKAS=$(BASE_KERNEL_ANNOTATED_PATH)/arch/um/include/shared/skas
BASE_KERNEL_ANNOTATED_HEADERS_ARCH_I386=$(BASE_KERNEL_ANNOTATED_PATH)/arch/um/sys-i386
BASE_KERNEL_ANNOTATED_HEADERS_ARCH_I386_SHARED=$(BASE_KERNEL_ANNOTATED_PATH)/arch/um/sys-i386/shared
BASE_KERNEL_ANNOTATED_HEADERS_ARCH_I386_ASM=$(BASE_KERNEL_ANNOTATED_PATH)/arch/um/sys-i386/asm
BASE_KERNEL_ANNOTATED_HEADERS_X86=$(BASE_KERNEL_ANNOTATED_PATH)/arch/x86/include
BASE_KERNEL_ANNOTATED_HEADERS_X86_ASM=$(BASE_KERNEL_ANNOTATED_PATH)/arch/x86/include/asm
BASE_KERNEL_COMPILED_PATH=compiled_images/$(KERNEL_VERSION)
BASE_KERNEL_COMPILED_HEADERS=$(BASE_KERNEL_COMPILED_PATH)/include
BASE_KERNEL_COMPILED_HEADERS2=$(BASE_KERNEL_COMPILED_PATH)/include2
#BASE_KERNEL_COMPILED_HEADERS_ARCH=$(BASE_KERNEL_COMPILED_PATH)/arch/um/include/shared

# Relative to ipc_drivers/driver_name
export DRIVER_KERNEL_ANNOTATED_PATH=../../$(BASE_KERNEL_ANNOTATED_PATH)
export DRIVER_KERNEL_ANNOTATED_HEADERS=../../$(BASE_KERNEL_ANNOTATED_HEADERS)
#export DRIVER_KERNEL_COMPILED_HEADERS_ARCH=../$(BASE_KERNEL_COMPILED_HEADERS_ARCH)

# Relative to ipc_userdaemon
export UD_KERNEL_ANNOTATED_PATH=../$(BASE_KERNEL_ANNOTATED_PATH)
export UD_KERNEL_ANNOTATED_HEADERS=../$(BASE_KERNEL_ANNOTATED_HEADERS)
export UD_KERNEL_ANNOTATED_HEADERS_ARCH=../$(BASE_KERNEL_ANNOTATED_HEADERS_ARCH)
export UD_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED=../$(BASE_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED)
export UD_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED_SKAS=../$(BASE_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED_SKAS)
export UD_KERNEL_ANNOTATED_HEADERS_ARCH_I386=../$(BASE_KERNEL_ANNOTATED_HEADERS_ARCH_I386)
export UD_KERNEL_ANNOTATED_HEADERS_ARCH_I386_SHARED=../$(BASE_KERNEL_ANNOTATED_HEADERS_ARCH_I386_SHARED)
export UD_KERNEL_ANNOTATED_HEADERS_ARCH_I386_ASM=../$(BASE_KERNEL_ANNOTATED_HEADERS_ARCH_I386_ASM)
export UD_KERNEL_ANNOTATED_HEADERS_X86=../$(BASE_KERNEL_ANNOTATED_HEADERS_X86)
export UD_KERNEL_ANNOTATED_HEADERS_X86_ASM=../$(BASE_KERNEL_ANNOTATED_HEADERS_X86_ASM)
export UD_KERNEL_COMPILED_PATH=../$(BASE_KERNEL_COMPILED_PATH)
export UD_KERNEL_COMPILED_HEADERS=../$(BASE_KERNEL_COMPILED_HEADERS)
export UD_KERNEL_COMPILED_HEADERS2=../$(BASE_KERNEL_COMPILED_HEADERS2)
#export UD_KERNEL_COMPILED_HEADERS_ARCH=../$(BASE_KERNEL_COMPILED_HEADERS_ARCH)

# Phony targets with no real dependencies--just call the necessary
# commands regardless.
.PHONY: clean
.PHONY: final_check
.PHONY: driver
.PHONY: all

# Compile the user-daemon and the misc device.
all: final_check

ifeq "$(DRIVER_TEST)" "true"
    OTHER_DEP += ./bin/wrappers_symtesting.o
    OTHER_DEP += ./bin/testing_state.o
ifeq "$(USE_KLEE)" "true"
# Note that this code requires both KLEE and the DRIVER_TEST framework
    OTHER_DEP += ./bin/testing_ep.o
endif
endif

ifeq "$(ENABLE_REFACTORING)" "true"
    OTHER_DEP+=./bin/refactoring.o
endif

OTHER_OBJ=$(DRIVER_ROOT)/$(DRIVER_NAME).sym.o \
	../ipc_external_functions/MJR_wrappers.o \
	../ipc_external_functions/libMJR_external_functions_llvm.o

# Some header files for the userdaemon.  This list should include
# all our headers.
OTHER_H=../common/slave_master_ud_md.h \
	../common/slave_master_ud_md_marshaling.h \
	../common/slave_master_ud_md_marshaling.c \
	../common/slave_top.h \
	../common/ud_md.h \
	../common/syscall_replacements.h \
	../common/wrappers_nooks.h \
	../common/wrappers_misc.h \
	../common/wrappers_usb.h 

# Header file directories that we use when compiling
# userspace source files that #include kernel headers
USERSPACE_KERNEL_HEADERS=-DARCH=um \
		-DMJR_IPC \
		-DKBUILD_BASENAME="\"linux2629\"" \
		-DKBUILD_MODNAME="\"driverprocess\"" \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_ARCH) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_ARCH_SHARED_SKAS) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_ARCH_I386) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_ARCH_I386_SHARED) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_ARCH_I386_ASM) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_X86) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS_X86_ASM) \
		-I$(UD_KERNEL_ANNOTATED_HEADERS) \
		-I$(UD_KERNEL_COMPILED_HEADERS) \
		-I$(UD_KERNEL_COMPILED_HEADERS2)

# Header file locations
UD_INCLUDES=-I../common -I../ipc_external_functions -I./hashtable -I./trie -I./testing

# OBJ files.  Most of these we compile without too much trouble.
./bin/host-ud.o: host-ud.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		host-ud.c \
		-o $@

./bin/process_trace.o: process_trace.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		process_trace.c \
		-o $@

./bin/refactoring.o: refactoring.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		refactoring.c \
		-o $@

./bin/syscall_replacements.o: syscall_replacements.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		syscall_replacements.c \
		-o $@

./bin/ud_marshaling.o: ud_marshaling.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		ud_marshaling.c -o $@

./bin/wrappers_nooks.o: wrappers_nooks.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		wrappers_nooks.c -o $@

./bin/wrappers_misc.o: wrappers_misc.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		wrappers_misc.c -o $@

#./bin/wrappers_sym.o: wrappers_sym.c $(OTHER_H)
#	$(UMCC) $(EXTRA_FLAGS) \
#		$(UD_INCLUDES) \
#		wrappers_sym.c -o $@

./bin/wrappers_disp_kern.o: wrappers_disp_kern.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(UD_INCLUDES) \
		wrappers_disp_kern.c -o $@

# The next three files take more effort to compile because they use
# kernel data structures.  We need to include kernel header
# files to make it work.
./bin/wrappers_usb.o: wrappers_usb.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(USERSPACE_KERNEL_HEADERS) \
		$(UD_INCLUDES) \
		wrappers_usb.c -o $@

#./bin/wrappers_alloc.o: wrappers_alloc.c $(OTHER_H)
        # ARCH=um is just to ensure headers work properly
        # __KERNEL__ is to trick the header files
        # MJR_IPC is for kernel header files:  undefs some functions
#	$(UMCC) $(EXTRA_FLAGS) \
		$(USERSPACE_KERNEL_HEADERS) \
		$(UD_INCLUDES) \
		wrappers_alloc.c -o $@

./bin/wrappers_symtesting.o: ./testing/wrappers_symtesting.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(USERSPACE_KERNEL_HEADERS) \
		$(UD_INCLUDES) \
		./testing/wrappers_symtesting.c -o $@

./bin/testing_state.o: ./testing/testing_state.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(USERSPACE_KERNEL_HEADERS) \
		$(UD_INCLUDES) \
		./testing/testing_state.c -o $@

./bin/testing_ep.o: ./testing/testing_ep.c $(OTHER_H)
	$(UMCC) $(EXTRA_FLAGS) \
		$(USERSPACE_KERNEL_HEADERS) \
		$(UD_INCLUDES) \
		./testing/testing_ep.c -o $@

./bin/hashtable_tester: ./bin/hashtable.o ./bin/hashtable_tester.o ./bin/hashtable_itr.o
	$(UMLD) $(LINKER_OPT) $? -o $@

./bin/hashtable_tester.o: ./hashtable/tester.c
	$(UMCC) $(EXTRA_FLAGS) -c $? -o $@

./bin/hashtable.o: ./hashtable/hashtable.c
	$(UMCC) $(EXTRA_FLAGS) -c $? -o $@

./bin/hashtable_itr.o: ./hashtable/hashtable_itr.c
	$(UMCC) $(EXTRA_FLAGS) -c $? -o $@

# Wrappers for the external functions.
# External functions.  These are the functions that enable communication
# with the UML kernel
external: ../ipc_external_functions/*.c $(OTHER_H)
	$(MAKE) -C ../ipc_external_functions

# Run the makefile in the driver root directory.
# Each driver may consist of a variety of different
# files, so we need to make this part of the build
# process a little more extensible.
driver: $(patsubst %.c,$(DRIVER_ROOT)/%.c,$(DRIVER_SRC)) $(OTHER_H)
	$(MAKE) -C $(DRIVER_ROOT)

# This program is necessary to improve LLVM/KLEE performance
# since parsing the trace using KLEE/LLVM is simply too slow.
# In this instance, we don't define USE_KLEE so it will use the native
# strcmp/strcpy etc functions.
./bin/process_trace: process_trace.c $(OTHER_H)
	gcc -DSTANDALONE_TRACE $(UD_INCLUDES) -g -O0 process_trace.c -o ./bin/process_trace

ifeq "$(ENABLE_REFACTORING)" "true"
./bin/refactoring:
	mkdir ./bin/refactoring

./bin/refactoring/refactoring.o: | ./bin/refactoring
./bin/refactoring/refactoring.o: refactoring.c $(OTHER_H)
	gcc -DSTANDALONE_REFACTOR \
		$(DEFINE_ENABLE_REFACTORING) \
		$(UD_INCLUDES) -O0 -g -c ./refactoring.c -o $@

./bin/refactoring/trie.o: | ./bin/refactoring
./bin/refactoring/trie.o: ./trie/trie.c $(OTHER_H)
	gcc -DSTANDALONE_REFACTOR \
		$(DEFINE_ENABLE_REFACTORING) \
		$(UD_INCLUDES) -O0 -g -c ./trie/trie.c -o $@

./bin/refactoring/process_trace.o: | ./bin/refactoring
./bin/refactoring/process_trace.o: process_trace.c $(OTHER_H)
	gcc -DSTANDALONE_REFACTOR \
		$(DEFINE_ENABLE_REFACTORING) \
		$(UD_INCLUDES) -O0 -g -c ./process_trace.c -o $@

./bin/refactoring/process_refactoring: | ./bin/refactoring
./bin/refactoring/process_refactoring: ./bin/refactoring/refactoring.o
./bin/refactoring/process_refactoring: ./bin/refactoring/trie.o
./bin/refactoring/process_refactoring: ./bin/refactoring/process_trace.o
	gcc -g -O0 $(DEFINE_ENABLE_REFACTORING) $^ -o $@
endif

# Put together a the symbolic driver
#./bin/ud$(DRIVER_NAME): $(OTHER_DEP) $(OTHER_H) driver external
#	$(UMLD) $(LINKER_OPT) $(OTHER_OBJ) $(OTHER_DEP) -o ./bin/ud$(DRIVER_NAME)
./bin/ud$(DRIVER_NAME): driver 
ifeq "$(USE_LLVM)" "true"
	cd ./bin; llvm-dis ./ud$(DRIVER_NAME).bc; cd ../
endif

final_check: miscdevice
ifeq "$(ENABLE_REFACTORING)" "true"
final_check: ./bin/refactoring/process_refactoring
endif
final_check: ./bin/ud$(DRIVER_NAME)
#final_check: ./bin/process_trace
#final_check: ./bin/hashtable_tester
#        ifeq "$(USE_LLVM)" "false"
#		@objdump -t ./bin/ud$(DRIVER_NAME) | grep UND
#        endif
	@echo "Following should list four files if build was successful.  View output.txt if not."
	@ls -lha $(DRIVER_ROOT)/$(DRIVER_NAME)-stub.ko $(DRIVER_ROOT)/$(DRIVER_NAME)-sfi.ko $(MISCHELP_ROOT)/mischelp.ko 
	#@cat $(DRIVER_ROOT)/output_sym.txt | grep FAULT  
#		#./bin/hashtable_tester
#		#./bin/ud$(DRIVER_NAME)

# If motivated, could add all the source file dependencies here
# so that we call make only if it's necessary.  But this seems like
# overkill since the miscdevice Makefile takes care of it all.
# Note that the build process here is complicated by the fact that
# the miscdevice is a kernel module.  We, therefore, must use the kernel
# build system.
miscdevice: ../ipc_miscdevice/*.[ch] \
	../ipc_miscdevice/nooks/*.c \
	$(OTHER_H)
	$(MAKE) -C $(MISCHELP_ROOT)

# Cleanup
clean:
	-rm -rf ./bin/*
	-rm -f output.txt
	-rm -f output_refactoring.bin
	-rm -f ./sound/\.*.cmd
	-rm -f ./sound/core/\.*.cmd
	-rm -f ./sound/core/seq/\.*.cmd
	-rm -f ./sound/oss/\.*.cmd
	-rm -f ./sound/pci/\.*.cmd
	-rm -f ./sound/pci/ac97/\.*.cmd
	-rm -f ./sound/drivers/mpu401/\.*.cmd
	-rm -f ./sound/drivers/opl3/\.*.cmd
	$(MAKE) -C ../ipc_external_functions clean
	$(MAKE) -C $(DRIVER_ROOT) clean
	$(MAKE) -C ../ipc_miscdevice clean
