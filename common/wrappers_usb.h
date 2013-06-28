#ifndef WRAPPERS_USB_H
#define WRAPPERS_USB_H

#ifdef WRAPPERS_SYM_DRIVER_STEP1
#ifdef WRAPPERS_SYM_DRIVER_STEP2
#error Error define at most one of WRAPPERS_SYM_DRIVER_STEP1 or WRAPPERS_SYM_DRIVER_STEP2
#endif
#endif

/////////////////////////////////////////
//
// For host-ud.c only
//
void initialize_wrappers_usb (void);
void shutdown_usb (void);
/////////////////////////////////////////

/////////////////////////////////////////
// For everyone else -- symbolic drivers
/////////////////////////////////////////

void execute_completions (void);

void * Sym_usb_buffer_alloc (const char *FUNCTION,
                             unsigned int LINE,
                             void *dev,
                             unsigned long size,
                             unsigned int mem_flags,
                             unsigned long long *dma);
void Sym_usb_buffer_free (const char *FUNCTION,
                          unsigned int LINE,
                          void *dev,
                          unsigned long size,
                          void *addr,
                          unsigned long dma);

void Sym_usb_sg_wait (const char *FUNCTION,
                      unsigned int LINE,
                      void *io);
void Sym_usb_sg_cancel (const char *FUNCTION,
                        unsigned int LINE,
                        void *io);
int Sym_usb_sg_init (const char *FUNCTION,
                     unsigned int LINE,
                     void *io,
                     void *dev,
                     unsigned pipe,
                     unsigned period,
                     void *sg,
                     int nents,
                     unsigned long length,
                     unsigned int mem_flags);

void Sym_usb_kill_urb (const char *FUNCTION,
                       unsigned int LINE,
                       void *urb);
void Sym_usb_free_urb (const char *FUNCTION,
                       unsigned int LINE,
                       void *urb);
void *Sym_usb_alloc_urb (const char *FUNCTION,
                         unsigned int LINE,
                         int iso_packets,
                         unsigned int mem_flags);
int Sym_usb_unlink_urb (const char *FUNCTION,
                        unsigned int LINE,
                        void *urb);

int Sym_usb_lock_device_for_reset (const char *FUNCTION,
                                   unsigned int LINE,
                                   void *udev,
                                   const void *iface);
int Sym_usb_reset_device (const char *FUNCTION,
                          unsigned int LINE,
                          void *dev);

int Sym_usb_control_msg (const char *FUNCTION,
                         unsigned int LINE,
                         void *dev,
                         unsigned int pipe,
                         unsigned char request,
                         unsigned char requesttype,
                         unsigned short value,
                         unsigned short index,
                         void *data,
                         unsigned short size,
                         int timeout);
int Sym_usb_interrupt_msg(const char *FUNCTION,
                          unsigned int LINE,
                          void *usb_dev,
                          unsigned int pipe,
                          void *data,
                          int len,
                          int *actual_length,
                          int timeout);
int Sym_usb_bulk_msg(const char *FUNCTION,
                     unsigned int LINE,
                     void *usb_dev,
                     unsigned int pipe,
                     void *data,
                     int len,
                     int *actual_length,
                     int timeout);
int Sym_usb_submit_urb (const char *FUNCTION,
                        unsigned int LINE,
                        void *urb,
                        unsigned int mem_flags);
int Sym_usb_set_interface(const char *FUNCTION,
                          unsigned int LINE,
                          void *dev,
                          int interface,
                          int alternate);

void Sym_schedule (const char *FUNCTION,
                   unsigned int LINE);

#ifdef WRAPPERS_SYM_DRIVER_STEP2

#define usb_buffer_alloc(a, b, c, d)                            \
    Sym_usb_buffer_alloc(__FUNCTION__, __LINE__, a, b, c, d)
#define usb_buffer_free(a, b, c, d)                             \
    Sym_usb_buffer_free(__FUNCTION__, __LINE__, a, b, c, d)

#define usb_sg_wait(a)                                  \
    Sym_usb_sg_wait(__FUNCTION__, __LINE__, a)
#define usb_sg_cancel(a)                                \
    Sym_usb_sg_cancel(__FUNCTION__, __LINE__, a)
#define usb_sg_init(a, b, c, d, e, f, g, h)                             \
    Sym_usb_sg_init(__FUNCTION__, __LINE__, a, b, c, d, e, f, g, h)

#define usb_free_urb(a)                                 \
    Sym_usb_free_urb(__FUNCTION__, __LINE__, a)
#define usb_alloc_urb(a, b)                             \
    Sym_usb_alloc_urb(__FUNCTION__, __LINE__, a, b)
#define usb_kill_urb(a)                                 \
    Sym_usb_kill_urb(__FUNCTION__, __LINE__, a)
#define usb_unlink_urb(a)                               \
    Sym_usb_unlink_urb(__FUNCTION__, __LINE__, a)

/*
#define usb_lock_device_for_reset(a, b)                                 \
    Sym_usb_lock_device_for_reset(__FUNCTION__, __LINE__, a, b)
#define usb_reset_device(a)                             \
    Sym_usb_reset_device(__FUNCTION__, __LINE__, a)
*/

#define usb_control_msg(a, b, c, d, e, f, g, h, i)                      \
    Sym_usb_control_msg(__FUNCTION__, __LINE__, a, b, c, d, e, f, g, h, i)
#define usb_interrupt_msg(a, b, c, d, e, f)                             \
    Sym_usb_interrupt_msg(__FUNCTION__, __LINE__, a, b, c, d, e, f)
#define usb_bulk_msg(a, b, c, d, e, f)                          \
    Sym_usb_bulk_msg(__FUNCTION__, __LINE__, a, b, c, d, e, f)
#define usb_submit_urb(a, b)                            \
    Sym_usb_submit_urb(__FUNCTION__, __LINE__, a, b)
#define usb_set_interface(a, b, c)                              \
    Sym_usb_set_interface(__FUNCTION__, __LINE__, a, b, c)

#define schedule() \
    Sym_schedule(__FUNCTION__, __LINE__)

#endif
#endif
