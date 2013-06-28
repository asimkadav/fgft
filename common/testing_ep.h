#ifndef TESTING_EP_H
#define TESTING_EP_H

#ifdef USE_KLEE
enum TEST_CATEGORIES {
    TEST_NET,
    TEST_SND
};

struct net_device;
struct file;

enum TEST_CASE_NET {
    TEST_CASE_NET_BEGIN = 0,
    ETHTOOL_GET_SETTINGS,
    ETHTOOL_SET_SETTINGS,
    ETHTOOL_GET_DRVINFO,
    ETHTOOL_GET_REGS_LEN,
    ETHTOOL_GET_REGS,
    ETHTOOL_GET_WOL,
    ETHTOOL_SET_WOL,
    ETHTOOL_GET_MSGLEVEL,
    ETHTOOL_SET_MSGLEVEL,
    ETHTOOL_NWAY_RESET,
    ETHTOOL_GET_LINK,
    ETHTOOL_GET_EEPROM_LEN,
    ETHTOOL_GET_EEPROM,
    ETHTOOL_SET_EEPROM,
    ETHTOOL_GET_COALESCE,
    ETHTOOL_SET_COALESCE,
    ETHTOOL_GET_RINGPARAM,
    ETHTOOL_SET_RINGPARAM,
    ETHTOOL_GET_PAUSEPARAM,
    ETHTOOL_SET_PAUSEPARAM,
    ETHTOOL_GET_RX_CSUM,
    ETHTOOL_SET_RX_CSUM,
    ETHTOOL_GET_TX_CSUM,
    ETHTOOL_SET_TX_CSUM,
    ETHTOOL_GET_SG,
    ETHTOOL_SET_SG,
    ETHTOOL_GET_TSO,
    ETHTOOL_SET_TSO,
    ETHTOOL_SELF_TEST,
    ETHTOOL_GET_STRINGS,
    ETHTOOL_PHYS_ID,
    ETHTOOL_GET_ETHTOOL_STATS,
    ETHTOOL_BEGIN,
    ETHTOOL_COMPLETE,
    ETHTOOL_GET_UFO,
    ETHTOOL_SET_UFO,
    ETHTOOL_GET_FLAGS,
    ETHTOOL_SET_FLAGS,
    ETHTOOL_GET_PRIV_FLAGS,
    ETHTOOL_SET_PRIV_FLAGS,
    ETHTOOL_GET_SSET_COUNT,
    TEST_CASE_NET_END
};

void initialize_ep (void);
void ep_test_net (enum TEST_CASE_NET fn_id);
void ep_init_net (struct net_device *net_dev);
void ep_symexec_set(int new_value);
#else
#define initialize_ep()
#define ep_test_net(fn_id)
#define ep_init_net(net_dev)
#define ep_symexec_set(new_value)
#endif

#ifdef TESTING_EP_FULL
#ifdef USE_KLEE
//
// Test functions called from wrappers_symtesting
//
void ep_llseek_fop_check (const char *fn,
                          int prepost,
                          long long retval,
                          void **arg1,
                          long long *arg2,
                          int *arg3);
void ep_read_fop_check (const char *fn,
                        int prepost,
                        long retval,
                        void **arg1,
                        char **arg2,
                        unsigned long *arg3,
                        long long **arg4);
void ep_write_fop_check(const char *fn,
                        int prepost,
                        long retval,
                        void **arg1,
                        /*const*/ char **arg2,
                        unsigned long *arg3,
                        long long **arg4);
void ep_aio_read_fop_check (const char *fn,
                            int prepost,
                            long retval,
                            void **arg1,
                            /*const*/ void **arg2,
                            unsigned long *arg3,
                            long long *arg4);
void ep_aio_write_fop_check (const char *fn,
                             int prepost,
                             long retval,
                             void **arg1,
                             /*const*/ void **arg2,
                             unsigned long *arg3,
                             long long *arg4);
void ep_readdir_fop_check (const char *fn,
                           int prepost,
                           int retval,
                           void **arg1,
                           void **arg2,
                           void **arg3);
void ep_poll_fop_check (const char *fn,
                        int prepost,
                        unsigned int retval,
                        void **arg1,
                        void **arg2);
void ep_ioctl_fop_check (const char *fn,
                         int prepost,
                         int retval,
                         void **arg1,
                         void **arg2,
                         unsigned int *arg3,
                         unsigned long *arg4);
void ep_unlocked_ioctl_fop_check (const char *fn,
                                  int prepost,
                                  long retval,
                                  void **arg1,
                                  unsigned int *arg2,
                                  unsigned long *arg3);
void ep_compat_ioctl_fop_check (const char *fn,
                                int prepost,
                                long retval,
                                void **arg1,
                                unsigned int *arg2,
                                unsigned long *arg3);
void ep_mmap_fop_check (const char *fn,
                        int prepost,
                        int retval,
                        void **arg1,
                        void **arg2);
void ep_open_fop_check (const char *fn,
                        int prepost,
                        int retval,
                        void **arg1,
                        void **arg2);
void ep_flush_fop_check (const char *fn,
                         int prepost,
                         int retval,
                         void **arg1,
                         void **id);
void ep_release_fop_check (const char *fn,
                           int prepost,
                           int retval,
                           void **arg1,
                           void **arg2);
void ep_fsync_fop_check (const char *fn,
                         int prepost,
                         int retval,
                         void **arg1,
                         void **arg2,
                         int *datasync);
void ep_aio_fsync_fop_check (const char *fn,
                             int prepost,
                             int retval,
                             void **arg1,
                             int *datasync);
void ep_fasync_fop_check (const char *fn,
                          int prepost,
                          int retval,
                          int *arg1,
                          void **arg2,
                          int *arg3);
void ep_lock_fop_check (const char *fn,
                        int prepost,
                        int retval,
                        void **arg1,
                        int *arg2,
                        void **arg3);
void ep_sendpage_fop_check (const char *fn,
                            int prepost,
                            long retval,
                            void **arg1,
                            void **arg2,
                            int *arg3,
                            unsigned long *arg4,
                            long long **arg5,
                            int *arg6);
void ep_get_unmapped_area_fop_check (const char *fn,
                                     int prepost,
                                     unsigned long retval,
                                     void **arg1,
                                     unsigned long *arg2,
                                     unsigned long *arg3,
                                     unsigned long *arg4,
                                     unsigned long *arg5);
void ep_check_flags_fop_check (const char *fn,
                               int prepost,
                               int retval,
                               int *arg1);
void ep_flock_fop_check (const char *fn,
                         int prepost,
                         int retval,
                         void **arg1,
                         int *arg2,
                         void **arg3);
void ep_splice_write_fop_check (const char *fn,
                                int prepost,
                                long retval,
                                void **arg1,
                                void **arg2,
                                long long **arg3,
                                unsigned long *arg4,
                                unsigned int *arg5);
void ep_splice_read_fop_check (const char *fn,
                               int prepost,
                               long retval,
                               void **arg1,
                               long long **arg2,
                               void **arg3,
                               unsigned long *arg4,
                               unsigned int *arg5);
void ep_setlease_fop_check (const char *fn,
                            int prepost,
                            int retval,
                            void **arg1,
                            long *arg2,
                            void ***arg3);
#else // USE_KLEE

//
// Test functions called from wrappers_symtesting
//
#define ep_llseek_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_read_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4)
#define ep_write_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4)
#define ep_aio_read_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4)
#define ep_aio_write_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4)
#define ep_readdir_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_poll_fop_check(fn, prepost, retval, arg1, arg2)
#define ep_ioctl_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4)
#define ep_unlocked_ioctl_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_compat_ioctl_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_mmap_fop_check(fn, prepost, retval, arg1, arg2)
#define ep_open_fop_check(fn, prepost, retval, arg1, arg2)
#define ep_flush_fop_check(fn, prepost, retval, arg1, id)
#define ep_release_fop_check(fn, prepost, retval, arg1, arg2)
#define ep_fsync_fop_check(fn, prepost, retval, arg1, arg2, datasync)
#define ep_aio_fsync_fop_check(fn, prepost, retval, arg1, datasync)
#define ep_fasync_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_lock_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_sendpage_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4, arg5, arg6)
#define ep_get_unmapped_area_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4, arg5)
#define ep_check_flags_fop_check(fn, prepost, retval, arg1)
#define ep_flock_fop_check(fn, prepost, retval, arg1, arg2, arg3)
#define ep_splice_write_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4, arg5)
#define ep_splice_read_fop_check(fn, prepost, retval, arg1, arg2, arg3, arg4, arg5)
#define ep_setlease_fop_check( fn, prepost, retval, arg1, arg2, arg3)

#endif // USE_KLEE

#endif // TESTING_EP_FULL

#endif
