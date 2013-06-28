#include <linux/vmalloc.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/mm.h>
#include <linux/slab.h>
#include <linux/sched.h>
#include <asm/uaccess.h>
#include "oss/sound_firmware.h"

/* static MJR */ int do_mod_firmware_load(const char *fn, char **fp)
{
	struct file* filp;
	long l = 0;  // MJR
	char *dp;
	loff_t pos;

        // filp_close / filp_open -- we've got a problem MJR
        mprintk ("Failure calling do_mod_firmware_load"); // MJR
        exit(1); // MJR

        /*
	filp = filp_open(fn, 0, 0);
	if (IS_ERR(filp))
	{
		mprintk(KERN_INFO "Unable to load '%s'.\n", fn); // MJR
		return 0;
	}
	l = filp->f_path.dentry->d_inode->i_size;
	if (l <= 0 || l > 131072)
	{
		mprintk(KERN_INFO "Invalid firmware '%s'\n", fn); // MJR
		filp_close(filp, current->files);
		return 0;
	}
	dp = vmalloc(l);
	if (dp == NULL)
	{
		mprintk(KERN_INFO "Out of memory loading '%s'.\n", fn); // MJR
		filp_close(filp, current->files);
		return 0;
	}
	pos = 0;
	if (vfs_read(filp, dp, l, &pos) != l)
	{
		mprintk(KERN_INFO "Failed to read '%s'.\n", fn); // MJR
		vfree(dp);
		filp_close(filp, current->files);
		return 0;
	}
	filp_close(filp, current->files);
	*fp = dp;*/
	return (int) l;
}

/**
 *	mod_firmware_load - load sound driver firmware
 *	@fn: filename
 *	@fp: return for the buffer.
 *
 *	Load the firmware for a sound module (up to 128K) into a buffer.
 *	The buffer is returned in *fp. It is allocated with vmalloc so is
 *	virtually linear and not DMAable. The caller should free it with
 *	vfree when finished.
 *
 *	The length of the buffer is returned on a successful load, the
 *	value zero on a failure.
 *
 *	Caution: This API is not recommended. Firmware should be loaded via
 *	request_firmware.
 */
 
int mod_firmware_load(const char *fn, char **fp)
{
	int r;
	/*mm_segment_t*/ unsigned long fs = Sym_get_fs(); // MJR
	Sym_set_fs(Sym_get_ds()); // MJR
        mprintk ("MJR:  MAJOR PROBLEMS WITH THIS CODE %s", __FUNCTION__);
	r = do_mod_firmware_load(fn, fp);
	Sym_set_fs(fs); // MJR
	return r;
}
// EXPORT_SYMBOL(mod_firmware_load); // MJR

MODULE_LICENSE("GPL");
