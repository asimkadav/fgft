#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

static int miscdevice_fd = -1;

#define FLIP_POINTERS 0x101
#define CORRUPT_STACK 0x102
#define REALLY_CORRUPT_STACK 0x103
#define RESET_COUNTS 0x104

//
// The idea behind this test is to have the miscdevice notify the user
// daemon to call all the network ethtool functions with symbolic parameters.
// In this case, this test program has no role -- it's simply providing
// a notification.
//
/*
static void test_net(const char *param) {
    int test = atoi (param);
    fprintf (stderr, "Executing test %s/%d\n", param, test);
    
    ioctl (miscdevice_fd, TEST_NET, test);
}
*/


static void flip_pointer(const char *dereference)   {
    int nthflip = atoi(dereference);
    fprintf (stderr, "Executing flippointer %s/%d\n", dereference, nthflip);
    ioctl (miscdevice_fd, FLIP_POINTERS, nthflip);
}

static void corrupt_stack(const char *dereference)   {
    int nthflip = atoi(dereference);
    fprintf (stderr, "Executing zero stack %s/%d\n", dereference, nthflip);
    ioctl (miscdevice_fd, CORRUPT_STACK, nthflip);
}

static void reallycorrupt_stack(const char *dereference)   {
    int nthflip = atoi(dereference);
    fprintf (stderr, "Executing really corrupt stack %s/%d\n", dereference, nthflip);
    ioctl (miscdevice_fd, REALLY_CORRUPT_STACK, nthflip);
}

static void resetfi()	{
    ioctl (miscdevice_fd, RESET_COUNTS);	
}



//
// This test, in contrast to test_net, is very different.  Here, we notify
// the miscdevice to go into "symbolic entry point" mode, which means
// we want to supply a combination of symbolic and concrete parameters
// to the driver function.  For the sound driver, we want to invoke the various
// file operations.  This entails providing a concrete file object, and some
// symbolic data.
//
/*
static void test_snd(const char *param) {
    {
        int test = atoi (param);
        
        fprintf (stderr, "Executing test sound / %d\n", test);
        
        // Put misc device in symbolic invocation mode.
        // Ignoring "test" parameter
        ioctl (miscdevice_fd, TEST_SND, 0);
    }

    {
        int fd_control;
        int fd_pcm;
        int err;
        char buf[10];

        fd_control = open ("/dev/snd/controlC0", O_RDWR);
        fprintf (stderr, "Returned controlC0 open: %d %d\n", fd_control, errno);

        err = ioctl (fd_control, 0, 0);
        fprintf (stderr, "Returned controlC0 ioctl: %d %d\n", err, errno);

        err = read (fd_control, buf, 10);
        fprintf (stderr, "Returned controlC0 read: %d %d\n", err, errno);

        err = write (fd_control, buf, 10);
        fprintf (stderr, "Returned controlC0 write: %d %d\n", err, errno);

        fd_pcm = open ("/dev/snd/pcmC0D0c", O_RDWR);
        fprintf (stderr, "Returned pcmC0D0c open: %d %d\n", fd_pcm, errno);

        err = ioctl (fd_pcm, 0, 0);
        fprintf (stderr, "Returned pcmC0D0c ioctl: %d %d\n", err, errno);

        err = read (fd_pcm, buf, 10);
        fprintf (stderr, "Returned pcmC0D0c read: %d %d\n", err, errno);

        err = write (fd_pcm, buf, 10);
        fprintf (stderr, "Returned pcmC0D0c write: %d %d\n", err, errno);

        err = close (fd_pcm);
        fprintf (stderr, "Returned pcmC0D0c close: %d %d\n", err, errno);

        err = close (fd_control);
        fprintf (stderr, "Returned controlC0 close: %d %d\n", err, errno);
    }
}

*/

int main (int argc, char **argv) {
    if (argc < 3) {
        fprintf (stderr, "Usage:\n");
        fprintf (stderr, "%s <cmd> <OPTION>\n", argv[0]);
        fprintf (stderr, "CMD: flip -> Flip pointer, OPTION: which derefrence to flip.\n");  
        fprintf (stderr, "CMD: stack -> Corrupt Stack, OPTION: which derefrence to flip.\n");  
        return 1;
    }

    miscdevice_fd = open("/dev/mischelp", O_RDWR);
    if (miscdevice_fd == -1) {
        printf ("Failed to open mischelp\n");
        return 2;
    }

    if (strcmp (argv[1], "flip") == 0) {
      flip_pointer(argv[2]);
    }
	
    if (strcmp (argv[1], "stack") == 0) {
      corrupt_stack(argv[2]);
    }
	
    if (strcmp (argv[1], "stackcorrupt") == 0) {
      reallycorrupt_stack(argv[2]);
    }

    if (strcmp (argv[1], "reset") == 0) {
	resetfi();	
    }
  

    /*
    if (strcmp (argv[1], "net") == 0) {
        test_net(argv[2]);
    } else if (strcmp (argv[1], "snd") == 0) {
        test_snd(argv[2]);
    } else {
        fprintf (stderr, "Failed:  specify snd or net\n");
    }
    */

    if (miscdevice_fd != -1) {
        close (miscdevice_fd);
    }

    return 0;
}
