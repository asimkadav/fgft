#ifndef WRAPPERS_SYM_TESTING_H
#define WRAPPERS_SYM_TESTING_H

#ifdef WRAPPERS_SYM_DRIVER_STEP1
#ifdef WRAPPERS_SYM_DRIVER_STEP2
#error Define exactly one of WRAPPERS_SYM_DRIVER_STEP1 or WRAPPERS_SYM_DRIVER_STEP2
#endif
#endif

//
// For host-ud.c only
//
void initialize_driver_state (void);
void free_driver_state (void);

//
// For wrappers_sym.c only
//
int can_call_interrupt_handlers (void);

#ifdef WRAPPERS_SYM_DRIVER_STEP1
#endif

#ifdef WRAPPERS_SYM_DRIVER_STEP2
// Generic functions that apply to all drivers
void default_check(const char *fn,
                   int prepost);
void interrupt_check(const char *fn,
                     int prepost,
                     int retval,
                     int *irq,
                     void **dev_instance);
void init_module_check(const char *fn,
                       int prepost,
                       int retval);
void cleanup_module_check(const char *fn,
                          int prepost);

// net_device_ops structure
void ndo_init_check(const char *fn,
                    int prepost,
                    int retval,
                    void **dev);
void ndo_uninit_check(const char *fn,
                      int prepost,
                      void **dev);
void ndo_open_check(const char *fn,
                    int prepost,
                    int retval,
                    void **dev);
void ndo_stop_check(const char *fn,
                    int prepost,
                    int retval,
                    void **dev);
void ndo_start_xmit_check(const char *fn,
                          int prepost,
                          int retval,
                          void **skb,
                          void **dev);
void ndo_select_queue_check(const char *fn,
                            int prepost,
                            unsigned short retval,
                            void **dev,
                            void **skb);
void ndo_change_rx_flags_check(const char *fn,
                               int prepost,
                               void **dev,
                               int *flags);
void ndo_set_rx_mode_check(const char *fn,
                           int prepost,
                           void **dev);
void ndo_set_multicast_list_check(const char *fn,
                                  int prepost,
                                  void **dev);
void ndo_set_mac_address_check(const char *fn,
                               int prepost,
                               int retval,
                               void **dev,
                               void **addr);
void ndo_validate_addr_check(const char *fn,
                             int prepost,
                             int retval,
                             void **dev);
void ndo_do_ioctl_check(const char *fn,
                        int prepost,
                        int retval,
                        void **dev,
                        void **ifr,
                        int *cmd);
void ndo_set_config_check(const char *fn,
                          int prepost,
                          int retval,
                          void **dev,
                          void **map);
void ndo_change_mtu_check(const char *fn,
                          int prepost,
                          int retval,
                          void **dev,
                          int *new_mtu);
void ndo_neigh_setup_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev,
                           void **);
void ndo_tx_timeout_check(const char *fn,
                          int prepost,
                          void **dev);
void ndo_get_stats_check(const char *fn,
                         int prepost,
                         void *retval,
                         void **dev);
void ndo_vlan_rx_register_check(const char *fn,
                                int prepost,
                                void **dev,
                                void **grp);
void ndo_vlan_rx_add_vid_check(const char *fn,
                               int prepost,
                               void **dev,
                               unsigned short *vid);
void ndo_vlan_rx_kill_vid_check(const char *fn,
                                int prepost,
                                void **dev,
                                unsigned short *vid);
void ndo_poll_controller_check(const char *fn,
                               int *prepost,
                               void **dev);

// ethtool_ops structure
void get_settings_check(const char *fn,
                        int prepost,
                        int retval,
                        void **,
                        void **);
void set_settings_check(const char *fn,
                        int prepost,
                        int retval,
                        void **,
                        void **);
void get_drvinfo_check(const char *fn,
                       int prepost,
                       void **,
                       void **);
void get_regs_len_check(const char *fn,
                        int prepost,
                        int retval,
                        void **);
void get_regs_check(const char *fn,
                    int prepost,
                    void **,
                    void **,
                    void **);
void get_wol_check(const char *fn,
                   int prepost,
                   void **,
                   void **);
void set_wol_check(const char *fn,
                   int prepost,
                   int retval,
                   void **,
                   void **);
void get_msglevel_check(const char *fn,
                        int prepost,
                        unsigned int retval,
                        void **);
void set_msglevel_check(const char *fn,
                        int prepost,
                        void **,
                        unsigned int *);
void nway_reset_check(const char *fn,
                      int prepost,
                      int retval,
                      void **);
void get_link_check(const char *fn,
                    int prepost,
                    unsigned int retval,
                    void **);
void get_eeprom_len_check(const char *fn,
                          int prepost,
                          int retval,
                          void **);
void get_eeprom_check(const char *fn,
                      int prepost,
                      int retval,
                      void **,
                      void **,
                      unsigned char **);
void set_eeprom_check(const char *fn,
                      int prepost,
                      int retval,
                      void **,
                      void **,
                      unsigned char **);
void get_coalesce_check(const char *fn,
                        int prepost,
                        int retval,
                        void **,
                        void **);
void set_coalesce_check(const char *fn,
                        int prepost,
                        int retval,
                        void **,
                        void **);
void get_ringparam_check(const char *fn,
                         int prepost,
                         void **,
                         void **);
void set_ringparam_check(const char *fn,
                         int prepost,
                         int retval,
                         void **,
                         void **);
void get_pauseparam_check(const char *fn,
                          int prepost,
                          void **,
                          void **);
void set_pauseparam_check(const char *fn,
                          int prepost,
                          int retval,
                          void **,
                          void **);
void get_rx_csum_check(const char *fn,
                       int prepost,
                       unsigned int retval,
                       void **);
void set_rx_csum_check(const char *fn,
                       int prepost,
                       int retval,
                       void **,
                       unsigned int *);
void get_tx_csum_check(const char *fn,
                       int prepost,
                       unsigned int retval,
                       void **);
void set_tx_csum_check(const char *fn,
                       int prepost,
                       int retval,
                       void **,
                       unsigned int *);
void get_sg_check(const char *fn,
                  int prepost,
                  unsigned int retval,
                  void **);
void set_sg_check(const char *fn,
                  int prepost,
                  int retval,
                  void **,
                  unsigned int *);
void get_tso_check(const char *fn,
                   int prepost,
                   unsigned int retval,
                   void **);
void set_tso_check(const char *fn,
                   int prepost,
                   int retval,
                   void **,
                   unsigned int *);
void self_test_check(const char *fn,
                     int prepost,
                     void **,
                     void **,
                     unsigned long **);
void get_strings_check(const char *fn,
                       int prepost,
                       void **,
                       unsigned int *stringset,
                       unsigned char **);
void phys_id_check(const char *fn,
                   int prepost,
                   int retval,
                   void **,
                   unsigned int *);
void get_ethtool_stats_check(const char *fn,
                             int prepost,
                             void **,
                             void **,
                             unsigned long long **);
void begin_check(const char *fn,
                 int prepost,
                 int retval,
                 void **);
void complete_check(const char *fn,
                    int prepost,
                    void **);
void get_ufo_check(const char *fn,
                   int prepost,
                   unsigned int retval,
                   void **);
void set_ufo_check(const char *fn,
                   int prepost,
                   int retval,
                   void **,
                   unsigned int *);
void get_flags_check(const char *fn,
                     int prepost,
                     unsigned int retval,
                     void **);
void set_flags_check(const char *fn,
                     int prepost,
                     int retval,
                     void **,
                     unsigned int *);
void get_priv_flags_check(const char *fn,
                          int prepost,
                          unsigned int retval,
                          void **);
void set_priv_flags_check(const char *fn,
                          int prepost,
                          int retval,
                          void **,
                          unsigned int *);
void get_sset_count_check(const char *fn,
                          int prepost,
                          int retval,
                          void **,
                          int *);
void self_test_count_check(const char *fn,
                           int prepost,
                           int retval,
                           void **);
void get_stats_count_check(const char *fn,
                           int prepost,
                           int retval,
                           void **);
void get_rxhash_check(const char *fn,
                      int prepost,
                      int retval,
                      void **,
                      void **);
void set_rxhash_check(const char *fn,
                      int prepost,
                      int retval,
                      void **,
                      void **);

// pci_driver structures
void probe_pci_check(const char *fn,
                     int prepost,
                     int retval,
                     void **dev,
                     /*const*/ void **id);
void remove_pci_check(const char *fn,
                      int prepost,
                      void **dev);
//void suspend_pci_check(const char *fn, int prepost, int retval, void **dev, pm_message_t *state);
//void suspend_late_pci_check(const char *fn, int prepost, int retval, void **dev, pm_message_t *state);
void resume_early_pci_check(const char *fn,
                            int prepost,
                            int retval,
                            void **dev);
void resume_pci_check(const char *fn,
                      int prepost,
                      int retval,
                      void **dev);
void shutdown_pci_check(const char *fn,
                        int prepost,
                        void **dev);

// usb_driver structures
int probe_usb_check(const char *fn,
                    int prepost,
                    int retval,
                    void **intf,
                    /*const*/ void **id);
void disconnect_usb_check(const char *fn,
                          int prepost,
                          void **intf);
int ioctl_usb_check(const char *fn,
                    int prepost,
                    int retval,
                    void **intf,
                    unsigned int *code,
                    void **buf);
//int suspend_usb_check(const char *fn, int prepost, int retval, void **intf, pm_message_t *message);
int resume_usb_check(const char *fn, int prepost,
                     int retval,
                     void **intf);
int reset_resume_usb_check(const char *fn,
                           int prepost,
                           int retval,
                           void **intf);
int pre_reset_usb_check(const char *fn,
                        int prepost,
                        int retval,
                        void **intf);
int post_reset_usb_check(const char *fn,
                         int prepost,
                         int retval,
                         void **intf);

// Kernel functions
// For 8139too specifically.
void __napi_complete_check(const char *fn,
                           int prepost,
                           void **n);
void __napi_schedule_check(const char *fn,
                           int prepost,
                           void **n);
void __netdev_alloc_skb_check(const char *fn,
                              int prepost,
                              void *retval,
                              void **dev,
                              unsigned int *length,
                              unsigned int *gfp_mask);
void __netif_schedule_check(const char *fn,
                            int prepost,
                            void **q);
void __pci_register_driver_check(const char *fn,
                                 int prepost,
                                 int retval,
                                 void **arg0,
                                 void **arg1,
                                 char /*const*/ **arg2);
void bitrev32_check(const char *fn,
                    int prepost,
                    unsigned int retval,
                    unsigned int *in);
void crc32_le_check(const char *fn,
                    int prepost,
                    unsigned int retval,
                    unsigned int *crc,
                    unsigned char /*const*/ **p,
                    unsigned long *len);
void eth_type_trans_check(const char *fn,
                          int prepost,
                          unsigned short retval,
                          void **skb,
                          void **dev);
void flush_scheduled_work_check(const char *fn,
                                int prepost);
void free_netdev_check(const char *fn,
                       int prepost,
                       void **dev);
void generic_mii_ioctl_check(const char *fn,
                             int prepost,
                             int retval,
                             void **mii_if,
                             void **mii_data,
                             int *cmd,
                             unsigned int **duplex_changed);
void init_timer_check(const char *fn,
                      int prepost,
                      void **timer);
void kfree_skb_check(const char *fn,
                     int prepost,
                     void **skb);
void mii_check_media_check(const char *fn,
                           int prepost,
                           unsigned int *retval,
                           void **mii,
                           unsigned int *ok_to_print,
                           unsigned int *init_media);
void mii_ethtool_gset_check(const char *fn,
                            int prepost,
                            int retval,
                            void **mii,
                            void **ecmd);
void mii_ethtool_sset_check(const char *fn,
                            int prepost,
                            int retval,
                            void **mii,
                            void **ecmd);
void mii_link_ok_check(const char *fn,
                       int prepost,
                       int retval,
                       void **mii);
void mii_nway_restart_check(const char *fn,
                            int prepost,
                            int retval,
                            void **mii);
void msleep_check(const char *fn,
                  int prepost,
                  unsigned int *msecs);
void net_ratelimit_check(const char *fn,
                         int prepost,
                         int retval);
void netif_napi_add_check(const char *fn,
                          int prepost,
                          void **dev,
                          void **napi,
                          void **poll,
                          int *weight);
void netif_receive_skb_check(const char *fn,
                             int prepost,
                             int retval,
                             void **skb);
void pci_disable_device_check(const char *fn,
                              int prepost,
                              int retval,
                              void **dev);
void pci_enable_device_check(const char *fn,
                             int prepost,
                             int retval,
                             void **dev);
void pci_get_drvdata_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **pdev);
void pci_name_check(const char *fn,
                    int prepost,
                    char /*const*/ *retval,
                    void **pdev);
void pci_read_config_word_check(const char *fn,
                                int prepost,
                                int retval,
                                void **dev,
                                int *where,
                                unsigned short **val);
void pci_release_regions_check(const char *fn,
                               int prepost,
                               void **arg0);
void pci_request_regions_check(const char *fn,
                               int prepost,
                               int retval,
                               void **arg0,
                               char /*const*/ **arg1);
void pci_set_drvdata_check(const char *fn,
                           int prepost,
                           void **pdev,
                           void **data);
void pci_set_master_check(const char *fn,
                          int prepost,
                          void **dev);
void pci_unregister_driver_check(const char *fn,
                                 int prepost,
                                 void **arg0);
void pci_write_config_word_check(const char *fn,
                                 int prepost,
                                 int retval,
                                 void **dev,
                                 int *where,
                                 unsigned short *val);
void register_netdev_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev);
void rtnl_lock_check(const char *fn,
                     int prepost);
void rtnl_unlock_check(const char *fn,
                       int prepost);
void schedule_delayed_work_check(const char *fn,
                                 int prepost,
                                 int retval,
                                 void **work,
                                 unsigned long *delay);
void skb_put_check(const char *fn,
                   int prepost,
                   unsigned char *retval,
                   void **skb,
                   unsigned int *len);
void unregister_netdev_check(const char *fn,
                             int prepost,
                             void **dev);

// For pegasus
void __create_workqueue_key_check(const char *fn,
                                  int prepost,
                                  void *retval,
                                  char /*const*/ **name,
                                  int *singlethread,
                                  int *freezeable,
                                  int *rt,
                                  void **key,
                                  char /*const*/ **lock_name);
void __tasklet_schedule_check(const char *fn,
                              int prepost,
                              void **t);
void __wake_up_check(const char *fn,
                     int prepost,
                     void **q,
                     unsigned int *mode,
                     int *nr,
                     void **key);
void capable_check(const char *fn,
                   int prepost,
                   int retval,
                   int *cap);
void del_timer_check(const char *fn,
                     int prepost,
                     int retval,
                     void **timer);
void destroy_workqueue_check(const char *fn,
                             int prepost,
                             void **wq);
void dev_driver_string_check(const char *fn,
                             int prepost,
                             char /*const*/ *retval,
                             void **dev);
void init_waitqueue_head_check(const char *fn,
                               int prepost,
                               void **q);
void netif_carrier_off_check(const char *fn,
                             int prepost,
                             void **dev);
void netif_carrier_on_check(const char *fn,
                            int prepost,
                            void **dev);
void netif_device_attach_check(const char *fn,
                               int prepost,
                               void **dev);
void netif_device_detach_check(const char *fn,
                               int prepost,
                               void **dev);
void netif_rx_check(const char *fn,
                    int prepost,
                    int retval,
                    void **skb);
void printk_ratelimit_check(const char *fn,
                            int prepost,
                            int retval);
void queue_delayed_work_check(const char *fn,
                              int prepost,
                              int retval,
                              void **wq,
                              void **work,
                              unsigned long *delay);
void tasklet_init_check(const char *fn,
                        int prepost,
                        void **t,
                        void (**func)(unsigned long ),
                        unsigned long *data);
void tasklet_kill_check(const char *fn,
                        int prepost,
                        void **t);
void usb_deregister_check(const char *fn,
                          int prepost,
                          void **arg0);
void usb_get_dev_check(const char *fn,
                       int prepost,
                       void *retval,
                       void **dev);
void usb_put_dev_check(const char *fn,
                       int prepost,
                       void **dev);
void usb_register_driver_check(const char *fn,
                               int prepost,
                               int retval,
                               void **arg0,
                               void **arg1,
                               char /*const*/ **arg2);

// For usb-audio
void kmemdup_check(const char *fn,
                   int prepost,
                   void *retval,
                   void /*const*/ **src,
                   unsigned long *len,
                   unsigned int *gfp);
void mod_timer_check(const char *fn,
                     int prepost,
                     int retval,
                     void **timer,
                     unsigned long *expires);
void msecs_to_jiffies_check(const char *fn,
                            int prepost,
                            unsigned long *retval,
                            unsigned int *m);
void schedule_timeout_uninterruptible_check(const char *fn,
                                            int prepost,
                                            long retval,
                                            long *timeout);
void snd_card_disconnect_check(const char *fn,
                               int prepost,
                               int retval,
                               void **card);
void snd_card_free_check(const char *fn,
                         int prepost,
                         int retval,
                         void **card);
void snd_card_free_when_closed_check(const char *fn,
                                     int prepost,
                                     int retval,
                                     void **card);
void snd_card_new_check(const char *fn,
                        int prepost,
                        void *retval,
                        int *idx,
                        char /*const*/ **id,
                        void **module,
                        int *extra_size);
void snd_card_proc_new_check(const char *fn,
                             int prepost,
                             int retval,
                             void **card,
                             char /*const*/ **name,
                             void ***entryp);
void snd_card_register_check(const char *fn,
                             int prepost,
                             int retval,
                             void **card);
void snd_component_add_check(const char *fn,
                             int prepost,
                             int retval,
                             void **card,
                             char /*const*/ **component);
void snd_ctl_add_check(const char *fn,
                       int prepost,
                       int retval,
                       void **card,
                       void **kcontrol);
void snd_ctl_find_id_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **card,
                           void **id);
void snd_ctl_new1_check(const char *fn,
                        int prepost,
                        void *retval,
                        void **kcontrolnew,
                        void **private_data);
void snd_ctl_notify_check(const char *fn,
                          int prepost,
                          void **card,
                          unsigned int *mask,
                          void **id);
void snd_device_new_check(const char *fn,
                          int prepost,
                          int retval,
                          void **card,
                          int *type,
                          void **device_data,
                          void **ops);
void snd_hwdep_new_check(const char *fn,
                         int prepost,
                         int retval,
                         void **card,
                         char **id,
                         int *device,
                         void ***rhwdep);
void snd_pcm_format_physical_width_check(const char *fn,
                                         int prepost,
                                         int retval,
                                         int *format);
void snd_pcm_hw_constraint_list_check(const char *fn,
                                      int prepost,
                                      int retval,
                                      void **runtime,
                                      unsigned int *cond,
                                      int *var,
                                      void **l);
void snd_pcm_hw_constraint_minmax_check(const char *fn,
                                        int prepost,
                                        int retval,
                                        void **runtime,
                                        int *var,
                                        unsigned int *min,
                                        unsigned int *max);
void snd_pcm_hw_rule_add_check(const char *fn,
                               int prepost,
                               int retval,
                               void **runtime,
                               unsigned int *cond,
                               int *var,
                               int (**func)(void *params ,
                                            void *rule ),
                               void **private,
                               int *dep,
                               int *x,
                               int *y);
void snd_pcm_new_check(const char *fn,
                       int prepost,
                       int retval,
                       void **card,
                       char **id,
                       int *device,
                       int *playback_count,
                       int *capture_count,
                       void ***rpcm);
void snd_pcm_new_stream_check(const char *fn,
                              int prepost,
                              int retval,
                              void **pcm,
                              int *stream,
                              int *substream_count);
void snd_pcm_period_elapsed_check(const char *fn,
                                  int prepost,
                                  void **substream);
void snd_pcm_rate_to_rate_bit_check(const char *fn,
                                    int prepost,
                                    unsigned int retval,
                                    unsigned int *rate);
void snd_pcm_set_ops_check(const char *fn,
                           int prepost,
                           void **pcm,
                           int *direction,
                           void **ops);
void snd_pcm_stop_check(const char *fn,
                        int prepost,
                        int retval,
                        void **substream,
                        int *status);
void snd_rawmidi_new_check(const char *fn,
                           int prepost,
                           int retval,
                           void **card,
                           char **id,
                           int *device,
                           int *output_count,
                           int *input_count,
                           void ***rmidi);
void snd_rawmidi_receive_check(const char *fn,
                               int prepost,
                               int retval,
                               void **substream,
                               unsigned char /*const*/ **buffer,
                               int *count);
void snd_rawmidi_set_ops_check(const char *fn,
                               int prepost,
                               void **rmidi,
                               int *stream,
                               void **ops);
void snd_rawmidi_transmit_ack_check(const char *fn,
                                    int prepost,
                                    int retval,
                                    void **substream,
                                    int *count);
void snd_rawmidi_transmit_check(const char *fn,
                                int prepost,
                                int retval,
                                void **substream,
                                unsigned char **buffer,
                                int *count);
void snd_rawmidi_transmit_empty_check(const char *fn,
                                      int prepost,
                                      int retval,
                                      void **substream);
void snd_rawmidi_transmit_peek_check(const char *fn,
                                     int prepost,
                                     int retval,
                                     void **substream,
                                     unsigned char **buffer,
                                     int *count);
void usb_driver_claim_interface_check(const char *fn,
                                      int prepost,
                                      int retval,
                                      void **driver,
                                      void **iface,
                                      void **priv);
void usb_get_descriptor_check(const char *fn,
                              int prepost,
                              int retval,
                              void **dev,
                              unsigned char *desctype,
                              unsigned char *descindex,
                              void **buf,
                              int *size);
void usb_ifnum_to_if_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **dev,
                           unsigned int *ifnum);
void usb_reset_configuration_check(const char *fn,
                                   int prepost,
                                   int retval,
                                   void **dev);
void usb_set_interface_check(const char *fn,
                             int prepost,
                             int retval,
                             void **dev,
                             int *ifnum,
                             int *alternate);
void usb_string_check(const char *fn,
                      int prepost,
                      int retval,
                      void **dev,
                      int *index,
                      char **buf,
                      unsigned long *size);
void vmalloc_to_page_check(const char *fn,
                           int prepost,
                           void *retval,
                           void /*const*/ **addr);

// For E1000
void __alloc_skb_check(const char *fn,
                       int prepost,
                       void *retval,
                       unsigned int *size,
                       unsigned int *priority,
                       int *fclone,
                       int *node);
void cancel_work_sync_check(const char *fn,
                            int prepost,
                            int retval,
                            void **work);
void capable_check(const char *fn,
                   int prepost,
                   int retval,
                   int *cap);
void del_timer_check(const char *fn,
                     int prepost,
                     int retval,
                     void **timer);
void dev_close_check(const char *fn,
                     int prepost,
                     int retval,
                     void **dev);
void dev_open_check(const char *fn,
                    int prepost,
                    int retval,
                    void **dev);
void eth_type_trans_check(const char *fn,
                          int prepost,
                          unsigned short retval,
                          void **skb,
                          void **dev);
void free_netdev_check(const char *fn,
                       int prepost,
                       void **dev);
void init_timer_check(const char *fn,
                      int prepost,
                      void **timer);
void iounmap_check(const char *fn,
                   int prepost,
                   void volatile **addr);
void mmiowb_check(const char *fn,
                  int prepost);
void mod_timer_check(const char *fn,
                     int prepost,
                     int retval,
                     void **timer,
                     unsigned long *expires);
void msleep_check(const char *fn,
                  int prepost,
                  unsigned int *msecs);
void msleep_interruptible_check(const char *fn,
                                int prepost,
                                unsigned long retval,
                                unsigned int *msecs);
void napi_complete_check(const char *fn,
                         int prepost,
                         void **n);
void __napi_schedule_check(const char *fn,
                           int prepost,
                           void **n);
void netif_carrier_off_check(const char *fn,
                             int prepost,
                             void **dev);
void netif_carrier_on_check(const char *fn,
                            int prepost,
                            void **dev);
void netif_device_attach_check(const char *fn,
                               int prepost,
                               void **dev);
void netif_device_detach_check(const char *fn,
                               int prepost,
                               void **dev);
void netif_napi_add_check(const char *fn,
                          int prepost,
                          void **dev,
                          void **napi,
                          void **poll,
                          int *weight);
void netif_receive_skb_check(const char *fn,
                             int prepost,
                             int retval,
                             void **skb);
void __netif_schedule_check(const char *fn,
                            int prepost,
                            void **q);
void net_ratelimit_check(const char *fn,
                         int prepost,
                         int retval);
void pci_channel_offline_check(const char *fn,
                               int prepost,
                               int retval,
                               void **pdev);
//void pci_choose_state_check(const char *fn, int prepost, int retval, void **dev, pm_message_t *state);
void pci_clear_mwi_check(const char *fn,
                         int prepost,
                         void **dev);
void pci_disable_device_check(const char *fn,
                              int prepost,
                              int retval,
                              void **dev);
void pci_disable_msi_check(const char *fn,
                           int prepost,
                           void **dev);
void pci_dma_sync_single_for_cpu_check(const char *fn,
                                       int prepost,
                                       void **pdev,
                                       unsigned long *dma_handle,
                                       unsigned long *size,
                                       int *direction);
void pci_dma_sync_single_for_device_check(const char *fn,
                                          int prepost,
                                          void **dev,
                                          unsigned long *dma_addr,
                                          unsigned long *size,
                                          int *direction);
void pci_enable_device_check(const char *fn,
                             int prepost,
                             int retval,
                             void **dev);
void pci_enable_device_mem_check(const char *fn,
                                 int prepost,
                                 int retval,
                                 void **dev);
void pci_enable_msi_check(const char *fn,
                          int prepost,
                          int retval,
                          void **dev);
void pci_enable_wake_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev,
                           int *state,
                           int *enable);
void pci_find_capability_check(const char *fn,
                               int prepost,
                               int retval,
                               void **dev,
                               int *cap);
void pci_get_drvdata_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **pdev);
void pci_ioremap_bar_check(const char *fn,
                           int prepost,
                           unsigned long retval,
                           void **pdev,
                           int *bar);
void pci_read_config_word_check(const char *fn,
                                int prepost,
                                int retval,
                                void **dev,
                                int *where,
                                unsigned short **val);
void __pci_register_driver_check(const char *fn,
                                 int prepost,
                                 int retval,
                                 void **arg0,
                                 void **arg1,
                                 char /*const*/ **arg2);
void pci_release_selected_regions_check(const char *fn,
                                        int prepost,
                                        void **pdev,
                                        int *bars);
void pci_request_selected_regions_check(const char *fn,
                                        int prepost,
                                        int retval,
                                        void **pdev,
                                        int *bars,
                                        char /*const*/ **res_name);
void pci_select_bars_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev,
                           unsigned long *flags);
void pci_set_consistent_dma_mask_check(const char *fn,
                                       int prepost,
                                       int retval,
                                       void **dev,
                                       unsigned long *mask);
void pci_set_dma_mask_check(const char *fn,
                            int prepost,
                            int retval,
                            void **dev,
                            unsigned long *mask);
void pci_set_drvdata_check(const char *fn,
                           int prepost,
                           void **pdev,
                           void **data);
void pci_set_master_check(const char *fn,
                          int prepost,
                          void **dev);
void pci_set_mwi_check(const char *fn,
                       int prepost,
                       int retval,
                       void **dev);
void pci_set_power_state_check(const char *fn,
                               int prepost,
                               int retval,
                               void **dev,
                               int *state);
void pci_unregister_driver_check(const char *fn,
                                 int prepost,
                                 void **arg0);
void pcix_get_mmrbc_check(const char *fn,
                          int prepost,
                          int retval,
                          void **dev);
void pcix_set_mmrbc_check(const char *fn,
                          int prepost,
                          int retval,
                          void **dev,
                          int *mmrbc);
void print_hex_dump_check(const char *fn,
                          int prepost,
                          char /*const*/ **level,
                          char /*const*/ **prefix_str,
                          int *prefix_type,
                          int *rowsize,
                          int *groupsize,
                          void /*const*/ **buf,
                          unsigned long *len,
                          _Bool *ascii);
void pskb_expand_head_check(const char *fn,
                            int prepost,
                            int retval,
                            void **skb,
                            int *nhead,
                            int *ntail,
                            unsigned int *gfp_mask);
void __pskb_pull_tail_check(const char *fn,
                            int prepost,
                            unsigned char *retval,
                            void **skb,
                            int *delta);
void register_netdev_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev);
void round_jiffies_check(const char *fn,
                         int prepost,
                         unsigned long retval,
                         unsigned long *j);
void schedule_work_check(const char *fn,
                         int prepost,
                         int retval,
                         void **work);
void skb_put_check(const char *fn,
                   int prepost,
                   unsigned char *retval,
                   void **skb,
                   unsigned int *len);
void skb_trim_check(const char *fn,
                    int prepost,
                    void **skb,
                    unsigned int *len);
void __udelay_check(const char *fn,
                    int prepost,
                    unsigned long *usecs);
void unregister_netdev_check(const char *fn,
                             int prepost,
                             void **dev);

//
// For Ens1371
//
void add_wait_queue_check(const char *fn,
                          int prepost,
                          void **q,
                          void **wait);
void bus_register_check(const char *fn,
                        int prepost,
                        int retval,
                        void **bus);
void bus_unregister_check(const char *fn,
                          int prepost,
                          void **bus);
void __class_create_check(const char *fn,
                          int prepost,
                          void *retval,
                          void **owner,
                          char /*const*/ **name,
                          void **key);
void class_destroy_check(const char *fn,
                         int prepost,
                         void **cls);
void _cond_resched_check(const char *fn,
                         int prepost,
                         int retval);
void create_proc_entry_check(const char *fn,
                             int prepost,
                             void *retval,
                             char /*const*/ **name,
                             unsigned int *mode,
                             void **parent);
void del_timer_check(const char *fn,
                     int prepost,
                     int retval,
                     void **timer);
void device_create_check(const char *fn,
                         int prepost,
                         void *retval,
                         void **cls,
                         void **parent,
                         unsigned int *devt,
                         void **drvdata,
                         char /*const*/ **fmt);
void device_create_file_check(const char *fn,
                              int prepost,
                              int retval,
                              void **device,
                              void **entry);
void device_destroy_check(const char *fn,
                          int prepost,
                          void **cls,
                          unsigned int *devt);
void device_register_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev);
void device_unregister_check(const char *fn,
                             int prepost,
                             void **dev);
void dev_set_name_check(const char *fn,
                        int prepost,
                        int retval,
                        void **dev,
                        char /*const*/ **name);
void do_gettimeofday_check(const char *fn,
                           int prepost,
                           void **tv);
void fasync_helper_check(const char *fn,
                         int prepost,
                         int retval,
                         int *arg0,
                         void **arg1,
                         int *arg2,
                         void **arg3); // MJR arg3 was void **
void fget_check(const char *fn,
                int prepost,
                void *retval,
                unsigned int *fd);
void finish_wait_check(const char *fn,
                       int prepost,
                       void **q,
                       void **wait);
void fput_check(const char *fn,
                int prepost,
                void **arg0);
void getnstimeofday_check(const char *fn,
                          int prepost,
                          void **tv);
void hrtimer_cancel_check(const char *fn,
                          int prepost,
                          int retval,
                          void **timer);
// void hrtimer_forward_check(const char *fn, int prepost, u64 *retval, void **timer, ktime_t *now, ktime_t *interval);
void hrtimer_get_res_check(const char *fn,
                           int prepost,
                           int retval,
                           int *which_clock,
                           void **tp);
void hrtimer_init_check(const char *fn,
                        int prepost,
                        void **timer,
                        int *which_clock,
                        int *mode);
// void hrtimer_start_check(const char *fn, int prepost, int retval, void **timer, ktime_t *tim, enum hrtimer_mode *mode);
void init_timer_check(const char *fn,
                      int prepost,
                      void **timer);
void init_waitqueue_head_check(const char *fn,
                               int prepost,
                               void **q);
// void kernel_vmap_check(const char *fn, int prepost, void *retval, void ***pages, unsigned int *count, unsigned long *flags, pgprot_t *prot);
void kill_fasync_check(const char *fn,
                       int prepost,
                       void *arg0,
                       int *arg1,
                       int *arg2); // MJR arg0 was void **
void ktime_get_ts_check(const char *fn,
                        int prepost,
                        void **ts);
void __might_sleep_check(const char *fn,
                         int prepost,
                         char **file,
                         int *line);
void __mod_timer_check(const char *fn,
                       int prepost,
                       int retval,
                       void **timer,
                       unsigned long *expires);
void module_put_check(const char *fn,
                      int prepost,
                      void **module);
void msecs_to_jiffies_check(const char *fn,
                            int prepost,
                            unsigned long *retval,
                            unsigned int *m);
void msleep_check(const char *fn,
                  int prepost,
                  unsigned int *msecs);
void pci_disable_device_check(const char *fn,
                              int prepost,
                              int retval,
                              void **dev);
void pci_enable_device_check(const char *fn,
                             int prepost,
                             int retval,
                             void **dev);
void pci_get_drvdata_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **pdev);
void __pci_register_driver_check(const char *fn,
                                 int prepost,
                                 int retval,
                                 void **arg0,
                                 void **arg1,
                                 char /*const*/ **arg2);
void pci_release_regions_check(const char *fn,
                               int prepost,
                               void **arg0);
void pci_request_regions_check(const char *fn,
                               int prepost,
                               int retval,
                               void **arg0,
                               char /*const*/ **res_name);
void pci_set_drvdata_check(const char *fn,
                           int prepost,
                           void **pdev,
                           void **data);
void pci_set_master_check(const char *fn,
                          int prepost,
                          void **dev);
void pci_set_power_state_check(const char *fn,
                               int prepost,
                               int retval,
                               void **dev,
                               int *state);
void pci_unregister_driver_check(const char *fn,
                                 int prepost,
                                 void **arg0);
void pm_qos_add_requirement_check(const char *fn,
                                  int prepost,
                                  int retval,
                                  int *qos,
                                  char **name,
                                  int *value);
void pm_qos_remove_requirement_check(const char *fn,
                                     int prepost,
                                     int *qos,
                                     char **name);
void prepare_to_wait_check(const char *fn,
                           int prepost,
                           void **q,
                           void **wait,
                           int *state);
void proc_create_data_check(const char *fn,
                            int prepost,
                            void *retval,
                            char /*const*/ **name,
                            unsigned int *mode,
                            void **parent,
                            void /*const*/ **proc_fops,
                            void **data);
void proc_symlink_check(const char *fn,
                        int prepost,
                        void *retval,
                        char /*const*/ **arg0,
                        void **arg1,
                        char /*const*/ **arg2);
void register_chrdev_check(const char *fn,
                           int prepost,
                           int retval,
                           unsigned int *arg0,
                           char /*const*/ **arg1,
                           void /*const*/ **arg2);
void release_resource_check(const char *fn,
                            int prepost,
                            int retval,
                            void **new);
void remove_proc_entry_check(const char *fn,
                             int prepost,
                             char /*const*/ **name,
                             void **parent);
void remove_wait_queue_check(const char *fn,
                             int prepost,
                             void **q,
                             void **wait);
void request_module_check(const char *fn,
                          int prepost,
                          int retval,
                          char /*const*/ **name);
void schedule_timeout_check(const char *fn,
                            int prepost,
                            long retval,
                            long *timeout);
void schedule_timeout_uninterruptible_check(const char *fn,
                                            int prepost,
                                            long retval,
                                            long *timeout);
void simple_strtoul_check(const char *fn,
                          int prepost,
                          unsigned long retval,
                          char /*const*/ **arg0,
                          char ***arg1,
                          unsigned int *arg2);
void single_open_check(const char *fn,
                       int prepost,
                       int retval,
                       void **arg0,
                       void **arg1,
                       void **arg2); // MJR arg1 is a function pointer
void tasklet_init_check(const char *fn,
                        int prepost,
                        void **t,
                        void (**func)(unsigned long),
                        unsigned long *data);
void tasklet_kill_check(const char *fn,
                        int prepost,
                        void **t);
void __tasklet_schedule_check(const char *fn,
                              int prepost,
                              void **t);
void __udelay_check(const char *fn,
                    int prepost,
                    unsigned long *usecs);
void unregister_chrdev_check(const char *fn,
                             int prepost,
                             unsigned int *arg0,
                             char /*const*/ **arg1);
void vunmap_check(const char *fn,
                  int prepost,
                  void /*const*/ **addr);
void __wake_up_check(const char *fn,
                     int prepost,
                     void **q,
                     unsigned int *mode,
                     int *nr,
                     void **key);

// ca0106
void pci_read_config_dword_check(const char *fn,
                                 int prepost,
                                 int  retval,
                                 void **dev,
                                 int  *where,
                                 unsigned int **val);
void __request_region_check(const char *fn,
                            int prepost,
                            void *retval,
                            void **arg0,
                            unsigned long *start,
                            unsigned long *n,
                            char /*const*/ **name,
                            int *flags);

//
// CMIPCI
//
void pci_dev_present_check(const char *fn,
                           int prepost,
                           int retval,
                           /*const*/ void **ids);

//
// TG3
// 
void ethtool_op_set_tso_check(const char *fn,
                              int prepost,
                              int retval,
                              void **dev,
                              unsigned int *data);
void ethtool_op_set_tx_csum_check(const char *fn,
                                  int prepost,
                                  int retval,
                                  void **dev,
                                  unsigned int *data);
void ethtool_op_set_tx_ipv6_csum_check(const char *fn,
                                       int prepost,
                                       int retval,
                                       void **dev,
                                       unsigned int *data);
void hweight8_check(const char *fn,
                    int prepost,
                    unsigned int retval,
                    unsigned int *w);
void jiffies_to_usecs_check(const char *fn,
                            int prepost,
                            unsigned int retval,
                            unsigned long *j);
void local_bh_disable_check(const char *fn,
                            int prepost);
void local_bh_enable_check(const char *fn,
                           int prepost);
void mdiobus_alloc_check(const char *fn,
                         int prepost,
                         void *retval);
void mdiobus_free_check(const char *fn,
                        int prepost,
                        void **bus);
void mdiobus_register_check(const char *fn,
                            int prepost,
                            int retval,
                            void **bus);
void mdiobus_unregister_check(const char *fn,
                              int prepost,
                              void **bus);
void pci_dev_put_check(const char *fn,
                       int prepost,
                       void **dev);
void pcie_set_readrq_check(const char *fn,
                           int prepost,
                           int retval,
                           void **dev,
                           int *rq);
void pci_get_device_check(const char *fn,
                          int prepost,
                          void *retval,
                          unsigned int *vendor,
                          unsigned int *device,
                          void **from);
void pci_get_slot_check(const char *fn,
                        int prepost,
                        void *retval,
                        void **bus,
                        unsigned int *devfn);
void pci_pme_capable_check(const char *fn,
                           int prepost,
                           _Bool retval,
                           void **dev,
                           int *state);
void pci_restore_state_check(const char *fn,
                             int prepost,
                             int retval,
                             void **dev);
void pci_save_state_check(const char *fn,
                          int prepost,
                          int retval,
                          void **dev);
void pci_target_state_check(const char *fn,
                            int prepost,
                            int retval,
                            void **dev);
void phy_connect_check(const char *fn,
                       int prepost,
                       void *retval,
                       void **dev,
                       char **bus_id /*const*/,
                       void **handler,
                       unsigned int *flags,
                       int *interface); // MJR handler is an fn ptr
void phy_disconnect_check(const char *fn,
                          int prepost,
                          void **phydev);
void phy_ethtool_gset_check(const char *fn,
                            int prepost,
                            int retval,
                            void **phydev,
                            void **cmd);
void phy_ethtool_sset_check(const char *fn,
                            int prepost,
                            int retval,
                            void **phydev,
                            void **cmd);
void phy_mii_ioctl_check(const char *fn,
                         int prepost,
                         int retval,
                         void **phydev,
                         void **mii_data,
                         int *cmd);
void phy_start_aneg_check(const char *fn,
                          int prepost,
                          int retval,
                          void **phydev);
void phy_start_check(const char *fn,
                     int prepost,
                     void **phydev);
void phy_stop_check(const char *fn,
                    int prepost,
                    void **phydev);
void release_firmware_check(const char *fn,
                            int prepost,
                            void **fw /*const*/);
void request_firmware_check(const char *fn,
                            int prepost,
                            int retval,
                            void ***fw /*const */,
                            char **name /*const*/,
                            void **device);
void skb_copy_check(const char *fn,
                    int prepost,
                    void *retval,
                    void ***skb /*const*/,
                    unsigned *priority);
void skb_copy_expand_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **skb /*const*/,
                           int *newheadroom,
                           int *newtailroom,
                           unsigned *priority);
void skb_gso_segment_check(const char *fn,
                           int prepost,
                           void *retval,
                           void **skb,
                           int *features);
void usecs_to_jiffies_check(const char *fn,
                            int prepost,
                            unsigned long retval,
                            unsigned int *u);

//
// Sound library
//

//
// file_operations struct
//
void llseek_fop_check (const char *fn,
                       int prepost,
                       long long retval,
                       void **arg1,
                       long long *arg2,
                       int *arg3);
void read_fop_check (const char *fn,
                     int prepost,
                     long retval,
                     void **arg1,
                     char **arg2,
                     unsigned long *arg3,
                     long long **arg4);
void write_fop_check (const char *fn,
                      int prepost,
                      long retval,
                      void **arg1,
                      /*const*/ char **arg2,
                      unsigned long *arg3,
                      long long **arg4);
void aio_read_fop_check (const char *fn,
                         int prepost,
                         long retval,
                         void **arg1,
                         /*const*/ void **arg2,
                         unsigned long *arg3,
                         long long *arg4);
void aio_write_fop_check (const char *fn,
                          int prepost,
                          long retval,
                          void **arg1,
                          /*const*/ void **arg2,
                          unsigned long *arg3,
                          long long *arg4);
void readdir_fop_check (const char *fn,
                        int prepost,
                        int retval,
                        void **arg1,
                        void **arg2,
                        void **arg3);
void poll_fop_check (const char *fn,
                     int prepost,
                     unsigned int retval,
                     void **arg1,
                     void **arg2);
void ioctl_fop_check (const char *fn,
                      int prepost,
                      int retval,
                      void **arg1,
                      void **arg2,
                      unsigned int *arg3,
                      unsigned long *arg4);
void unlocked_ioctl_fop_check (const char *fn,
                               int prepost,
                               long retval,
                               void **arg1,
                               unsigned int *arg2,
                               unsigned long *arg3);
void compat_ioctl_fop_check (const char *fn,
                             int prepost,
                             long retval,
                             void **arg1,
                             unsigned int *arg2,
                             unsigned long *arg3);
void mmap_fop_check (const char *fn,
                     int prepost,
                     int retval,
                     void **arg1,
                     void **arg2);
void open_fop_check (const char *fn,
                     int prepost,
                     int retval,
                     void **arg1,
                     void **arg2);
void flush_fop_check (const char *fn,
                      int prepost,
                      int retval,
                      void **arg1,
                      void **id);
void release_fop_check (const char *fn,
                        int prepost,
                        int retval,
                        void **arg1,
                        void **arg2);
void fsync_fop_check (const char *fn,
                      int prepost,
                      int retval,
                      void **arg1,
                      void **arg2,
                      int *datasync);
void aio_fsync_fop_check (const char *fn,
                          int prepost,
                          int retval,
                          void **arg1,
                          int *datasync);
void fasync_fop_check (const char *fn,
                       int prepost,
                       int retval,
                       int *arg1,
                       void **arg2,
                       int *arg3);
void lock_fop_check (const char *fn,
                     int prepost,
                     int retval,
                     void **arg1,
                     int *arg2,
                     void **arg3);
void sendpage_fop_check (const char *fn,
                         int prepost,
                         long retval,
                         void **arg1,
                         void **arg2,
                         int *arg3,
                         unsigned long *arg4,
                         long long **arg5,
                         int *arg6);
void get_unmapped_area_fop_check (const char *fn,
                                  int prepost,
                                  unsigned long retval,
                                  void **arg1,
                                  unsigned long *arg2,
                                  unsigned long *arg3,
                                  unsigned long *arg4,
                                  unsigned long *arg5);
void check_flags_fop_check (const char *fn,
                            int prepost,
                            int retval,
                            int *arg1);
void flock_fop_check (const char *fn,
                      int prepost,
                      int retval,
                      void **arg1,
                      int *arg2,
                      void **arg3);
void splice_write_fop_check (const char *fn,
                             int prepost,
                             long retval,
                             void **arg1,
                             void **arg2,
                             long long **arg3,
                             unsigned long *arg4,
                             unsigned int *arg5);
void splice_read_fop_check (const char *fn,
                            int prepost,
                            long retval,
                            void **arg1,
                            long long **arg2,
                            void **arg3,
                            unsigned long *arg4,
                            unsigned int *arg5);
void setlease_fop_check (const char *fn,
                         int prepost,
                         int retval,
                         void **arg1,
                         long *arg2,
                         void ***arg3);

//
// struct vm_operations_struct
//
void open_vmop_check (const char *fn,
                      int prepost,
                      void **area);
void close_vmop_check (const char *fn,
                       int prepost,
                       void **area);
void fault_vmop_check (const char *fn,
                       int prepost,
                       int retval,
                       void **vma,
                       void **vmf);
void page_mkwrite_vmop_check (const char *fn,
                              int prepost,
                              int retval,
                              void **vma,
                              void **page);
void access_vmop_check (const char *fn,
                        int prepost,
                        int retval,
                        void **vma,
                        unsigned long *addr,
                        void **buf,
                        int *len,
                        int *write);

//
// struct bus_type
//
void match_bustype_check (const char *fn,
                          int prepost,
                          int retval,
                          void **dev,
                          void **drv);
void uevent_bustype_check (const char *fn,
                           int prepost,
                           int retval,
                           void **dev,
                           void **env);
void probe_bustype_check (const char *fn,
                          int prepost,
                          int retval,
                          void **dev);
void remove_bustype_check (const char *fn,
                           int prepost,
                           int retval,
                           void **dev);
void shutdown_bustype_check (const char *fn,
                             int prepost,
                             void **dev);
/*
void suspend_bustype_check (const char *fn,
                            int prepost,
                            int retval,
                            void **dev,
                            pm_message_t *state);
void suspend_late_bustype_check (const char *fn,
                                 int prepost,
                                 int retval,
                                 void **dev,
                                 pm_message_t *state);
*/
void resume_early_bustype_check (const char *fn,
                                 int prepost,
                                 int retval,
                                 void **dev);
void resume_bustype_check (const char *fn,
                           int prepost,
                           int retval,
                           void **dev);

//
// Custom ones we added. These are special because we have wrappers for these
// already, see for example wrappers_usb.c
//
void usb_alloc_urb_check(const char *fn,
                         int prepost,
                         void *retval,
                         int *iso_packets,
                         unsigned *mem_flags);
void usb_free_urb_check(const char *fn,
                        int prepost,
                        void **urb);
void usb_control_msg_check(const char *fn,
                           int prepost,
                           void **dev,
                           unsigned int *pipe,
                           unsigned short *request,
                           unsigned char *requesttype,
                           unsigned short *value,
                           unsigned short *index,
                           void **data,
                           unsigned short *size,
                           int *timeout);
void usb_submit_urb_check(const char *fn,
                          int prepost,
                          void **urb,
                          unsigned int *mem_flags);

//
// Allocation wrappers
//
void __get_free_pages_check(const char *fn,
                            int prepost,
                            unsigned long retval,
                            unsigned int *gfp_mask,
                            unsigned int *order);
void free_pages_check(const char *fn,
                      int prepost,
                      unsigned long *addr,
                      unsigned int *order);

void dma_alloc_coherent_check(const char *fn,
                              int prepost,
                              void *retval,
                              void **dev,
                              unsigned long *size,
                              unsigned long long **dma_handle,
                              unsigned int *flag);
void dma_free_coherent_check(const char *fn,
                             int prepost,
                             void **dev,
                             unsigned long *size,
                             void **vaddr,
                             unsigned long long *dma_handle);

void pci_alloc_consistent_check(const char *fn,
                                int prepost,
                                void *retval,
                                void **dev,
                                unsigned long *size,
                                unsigned long long **dma);
void pci_free_consistent_check(const char *fn,
                               int prepost,
                               void **pdev,
                               unsigned long *size,
                               void **cpu_addr,
                               unsigned long long *dma_addr);

void pci_map_single_check(const char *fn,
                          int prepost,
                          unsigned long long retval,
                          void **pdev,
                          void **cpu_addr,
                          unsigned long *size,
                          int *dir);
void pci_unmap_single_check(const char *fn,
                            int prepost,
                            void **pdev,
                            unsigned long long *dma_addr,
                            unsigned long *size,
                            int *direction);

void pci_map_page_check(const char *fn,
                        int prepost,
                        unsigned long long retval,
                        void **pdev,
                        void **page,
                        unsigned long *offset,
                        unsigned long *size,
                        int *dir);
void pci_unmap_page_check(const char *fn,
                          int prepost,
                          void **pdev,
                          unsigned long long *dma_addr,
                          unsigned long *size,
                          int *direction);

void skb_dma_map_check(const char *fn,
                       int prepost,
                       void **dev,
                       void **skb,
                       int *dir);
void skb_dma_unmap_check(const char *fn,
                         int prepost,
                         void **dev,
                         void **skb,
                         int *dir);

void alloc_etherdev_mq_check(const char *fn,
                             int prepost,
                             void *retval,
                             int *sizeof_priv,
                             unsigned int *queue_count);

void __kmalloc_check(const char *fn,
                     int prepost,
                     void *retval,
                     unsigned long *arg0,
                     unsigned int *arg1);
void kfree_check(const char *fn,
                 int prepost,
                 /*const*/ void **arg0);

void vmalloc_check(const char *fn,
                   int prepost,
                   void *retval,
                   unsigned long *arg0);
void vfree_check(const char *fn,
                 int prepost,
                 /*const*/ void **arg0);

void dev_alloc_skb_check(const char *fn,
                         int prepost,
                         void *retval,
                         unsigned int *length);
void dev_kfree_skb_any_check(const char *fn,            
                             int prepost,
                             void **skb);

void copy_to_user_check(const char *fn,
                        int prepost,
                        int retval,
                        void **to,
                        void /*const*/ **from,
                        int *n);
void copy_from_user_check(const char *fn,
                          int prepost,
                          int retval,
                          void **to,
                          void /*const*/ **from,
                          int *n);

//
// Locking / unlocking wrappers
//
void __spin_lock_init_check(const char *fn,
                            int prepost,
                            void **lock,
                            /*const*/ char **name,
                            void **key);
void spin_lock_init_check(const char *fn,
                          int prepost,
                          void **lock);
void _spin_lock_check(const char *fn,
                      int prepost,
                      void **lock);
void spin_lock_check(const char *fn,
                     int prepost,
                     void **lock);
void _spin_lock_bh_check(const char *fn,
                         int prepost,
                         void **lock);
void spin_lock_bh_check(const char *fn,
                        int prepost,
                        void **lock);
void _spin_lock_irqsave_check(const char *fn,
                              int prepost,
                              void **lock);
void spin_lock_irqsave_check(const char *fn,
                             int prepost,
                             void **lock,
                             unsigned long *flags);
void _spin_lock_irq_check(const char *fn,
                          int prepost,
                          void **lock);
void spin_lock_irq_check(const char *fn,
                         int prepost,
                         void **lock);
void _spin_unlock_check(const char *fn,
                        int prepost,
                        void **lock);
void spin_unlock_check(const char *fn,
                       int prepost,
                       void **lock);
void _spin_unlock_bh_check(const char *fn,
                           int prepost,
                           void **lock);
void spin_unlock_bh_check(const char *fn,
                          int prepost,
                          void **lock);
void _spin_unlock_irqrestore_check(const char *fn,
                                   int prepost,
                                   void **lock,
                                   unsigned long *flags);
void spin_unlock_irqrestore_check(const char *fn,
                                  int prepost,
                                  void **lock,
                                  unsigned long *flags);
void _spin_unlock_irq_check(const char *fn,
                            int prepost,
                            void **lock);
void spin_unlock_irq_check(const char *fn,
                           int prepost,
                           void **lock);
void spin_trylock_check(const char *fn,
                        int prepost,
                        void **lock);

void __mutex_init_check(const char *fn,
                        int prepost,
                        void **lock,
                        /*const*/ char **name,
                        void **key);
void mutex_init_check(const char *fn,
                      int prepost,
                      void **lock);
void mutex_lock_check(const char *fn,
                      int prepost,
                      void **lock);
void mutex_lock_interruptible_check(const char *fn,
                                    int prepost,
                                    void **lock);
void mutex_unlock_check(const char *fn,
                        int prepost,
                        void **lock);

void __init_rwsem_check(const char *fn,
                        int prepost,
                        void **sem,
                        /*const*/ char **name,
                        void **key);
void up_write_check(const char *fn,
                    int prepost,
                    void **sem);
void up_read_check(const char *fn,
                   int prepost,
                   void **sem);
void down_write_check(const char *fn,
                      int prepost,
                      void **sem);
void down_read_check(const char *fn,
                     int prepost,
                     void **sem);

void __rwlock_init_check(const char *fn,
                         int prepost,
                         void **lock,
                         /*const*/ char **name,
                         void **key);
void _write_lock_irq_check(const char *fn,
                           int prepost,
                           void **lock);
void _write_unlock_irq_check(const char *fn,
                             int prepost,
                             void **lock);
void _read_lock_irq_check(const char *fn,
                          int prepost,
                          void **lock);
void _read_unlock_irq_check(const char *fn,
                            int prepost,
                            void **lock);
void _read_lock_check(const char *fn,
                      int prepost,
                      void **lock);
void _read_unlock_check(const char *fn,
                        int prepost,
                        void **lock);
void _write_lock_irqsave_check(const char *fn,
                               int prepost,
                               void **lock);
void _write_unlock_irqrestore_check(const char *fn,
                                    int prepost,
                                    void **lock,
                                    unsigned long *flags);
void _read_lock_irqsave_check(const char *fn,
                              int prepost,
                              void **lock);
void _read_unlock_irqrestore_check(const char *fn,
                                   int prepost,
                                   void **lock,
                                   unsigned long *flags);

void test_driver_function_check (const char *fn,
                                 int prepost,
                                 int retval,
                                 int **x);
void test_driver_function2_check (const char *fn,
                                  int prepost,
                                  unsigned char *x);
void misc_register_check(const char *fn,
                         int prepost,
                         int retval,
                         void **misc);
void misc_deregister_check(const char *fn,
                           int prepost,
                           int retval,
                           void **misc);

#endif

#endif
