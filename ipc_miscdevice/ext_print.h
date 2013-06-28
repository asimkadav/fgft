
#ifndef EXT_PRINT_H
#define EXT_PRINT_H

#ifdef IN_USER
  #ifdef ENABLE_UPRINTK
    // Only call fprintf in user mode.  Don't use this in the kernel
    // or everything will crash randomly and be a huge pain in the ass.
    #define ext_print(...) fprintf (stderr, __VA_ARGS__)
  #else
    #define ext_print(...)
  #endif

  #define terminate(x) exit(x)
#else
// The ENABLE_UPRINTK macro can only be defined
// with our own Makefile.  Thus it is never defined
// in the kernel-half of the external functions
extern int printk (const char *x, ...);
#define ext_print(x, ...) printk (x, __VA_ARGS__)
#define terminate(x) _exit(x)
#endif

#endif

