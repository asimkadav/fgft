#!/bin/bash

#echo "Are you sure?"
#exit

./process.pl ../sound.bak/ac97_bus.c > ./ac97_bus.c
./process.pl ../sound.bak/last.c > ./last.c
./process.pl ../sound.bak/sound_core.c > ./sound_core.c
./process.pl ../sound.bak/sound_firmware.c > ./sound_firmware.c

./process.pl ../sound.bak/core/control.c > ./core/control.c
./process.pl ../sound.bak/core/control_compat.c > ./core/control_compat.c
./process.pl ../sound.bak/core/device.c > ./core/device.c
./process.pl ../sound.bak/core/hrtimer.c > ./core/hrtimer.c
./process.pl ../sound.bak/core/hwdep.c > ./core/hwdep.c
./process.pl ../sound.bak/core/hwdep_compat.c > ./core/hwdep_compat.c
./process.pl ../sound.bak/core/info.c > ./core/info.c
./process.pl ../sound.bak/core/info_oss.c > ./core/info_oss.c
./process.pl ../sound.bak/core/init.c > ./core/init.c
./process.pl ../sound.bak/core/memalloc.c > ./core/memalloc.c
./process.pl ../sound.bak/core/memory.c > ./core/memory.c
./process.pl ../sound.bak/core/misc.c > ./core/misc.c
./process.pl ../sound.bak/core/pcm.c > ./core/pcm.c
./process.pl ../sound.bak/core/pcm_compat.c > ./core/pcm_compat.c
./process.pl ../sound.bak/core/pcm_lib.c > ./core/pcm_lib.c
./process.pl ../sound.bak/core/pcm_memory.c > ./core/pcm_memory.c
./process.pl ../sound.bak/core/pcm_misc.c > ./core/pcm_misc.c
./process.pl ../sound.bak/core/pcm_native.c > ./core/pcm_native.c
./process.pl ../sound.bak/core/pcm_timer.c > ./core/pcm_timer.c
./process.pl ../sound.bak/core/rawmidi.c > ./core/rawmidi.c
./process.pl ../sound.bak/core/rawmidi_compat.c > ./core/rawmidi_compat.c
./process.pl ../sound.bak/core/rtctimer.c > ./core/rtctimer.c
./process.pl ../sound.bak/core/sgbuf.c > ./core/sgbuf.c
./process.pl ../sound.bak/core/sound.c > ./core/sound.c
./process.pl ../sound.bak/core/sound_oss.c > ./core/sound_oss.c
./process.pl ../sound.bak/core/timer.c > ./core/timer.c
./process.pl ../sound.bak/core/timer_compat.c > ./core/timer_compat.c
./process.pl ../sound.bak/core/vmaster.c > ./core/vmaster.c

./process.pl ../sound.bak/core/seq/seq.c > ./core/seq/seq.c
./process.pl ../sound.bak/core/seq/seq_clientmgr.c > ./core/seq/seq_clientmgr.c
./process.pl ../sound.bak/core/seq/seq_compat.c > ./core/seq/seq_compat.c
./process.pl ../sound.bak/core/seq/seq_device.c > ./core/seq/seq_device.c
./process.pl ../sound.bak/core/seq/seq_dummy.c > ./core/seq/seq_dummy.c
./process.pl ../sound.bak/core/seq/seq_fifo.c > ./core/seq/seq_fifo.c
./process.pl ../sound.bak/core/seq/seq_info.c > ./core/seq/seq_info.c
./process.pl ../sound.bak/core/seq/seq_lock.c > ./core/seq/seq_lock.c
./process.pl ../sound.bak/core/seq/seq_memory.c > ./core/seq/seq_memory.c
./process.pl ../sound.bak/core/seq/seq_midi.c > ./core/seq/seq_midi.c
./process.pl ../sound.bak/core/seq/seq_midi_emul.c > ./core/seq/seq_midi_emul.c
./process.pl ../sound.bak/core/seq/seq_midi_event.c > ./core/seq/seq_midi_event.c
./process.pl ../sound.bak/core/seq/seq_ports.c > ./core/seq/seq_ports.c
./process.pl ../sound.bak/core/seq/seq_prioq.c > ./core/seq/seq_prioq.c
./process.pl ../sound.bak/core/seq/seq_queue.c > ./core/seq/seq_queue.c
./process.pl ../sound.bak/core/seq/seq_system.c > ./core/seq/seq_system.c
./process.pl ../sound.bak/core/seq/seq_timer.c > ./core/seq/seq_timer.c
./process.pl ../sound.bak/core/seq/seq_virmidi.c > ./core/seq/seq_virmidi.c

./process.pl ../sound.bak/pci/ac97/ac97_codec.c > ./pci/ac97/ac97_codec.c
./process.pl ../sound.bak/pci/ac97/ac97_patch.c > ./pci/ac97/ac97_patch.c
./process.pl ../sound.bak/pci/ac97/ac97_pcm.c > ./pci/ac97/ac97_pcm.c
./process.pl ../sound.bak/pci/ac97/ac97_proc.c > ./pci/ac97/ac97_proc.c
