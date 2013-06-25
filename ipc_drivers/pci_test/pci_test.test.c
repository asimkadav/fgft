#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

char bytes[128] = "holy crap does this work?";

int main (int argc, char **argv) {
    int fd;
    
    fd = open("/dev/mjr_driver",O_RDWR);
    if (fd == -1) {
        printf ("Open misc device failed.  errno: %d\n", errno);
        return -1;
    }

    //ioctl (fd, 1, 2);
    ioctl (fd, 2, bytes);
//    write (fd, NULL, 10);

    close (fd);
    return 0;
}
