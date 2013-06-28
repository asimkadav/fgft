#ifndef UPRINTK_H
#define UPRINTK_H

// required for pegasus and 8139too
//extern int printk(const char* fmt, ...); 

#define uprintk(fmt, ...) printk (fmt, ## __VA_ARGS__)
#define mprintk(fmt, ...) printk (fmt, ## __VA_ARGS__)
#define snd_mprintk(fmt, ...) snd_printk (fmt, ## __VA_ARGS__)

#define snd_printk printk

//void printk(const char *fmt, ...);
//int printk(const char * fmt, ...);

#endif // UPRINTK_H
