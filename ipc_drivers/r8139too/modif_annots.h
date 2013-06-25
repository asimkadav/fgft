#define MODIFANNOT(X)  MICRODRIVERS__MODIF_ ## X
#define MODIFIES(var)  MICRODRIVERS__DUMMY((unsigned long) var) /* MJR: Was void * */
#define READS(var)     MICRODRIVERS__DUMMY((void *) var)
#define MODIFIES_ADDROF(var) MICRODRIVERS__DUMMY((unsigned long) &var) /* MJR: Was void */

void MICRODRIVERS__DUMMY(unsigned long x) { /* MJR Was void */
  return;
}

// Does not modify anything.
const char *MODIFANNOT(dev_driver_string) (struct device *dev) {
    return 0;
}

int MODIFANNOT(pci_bus_write_config_word) (struct pci_bus *bus, unsigned int devfn, int where, u16 val) {
    return 0;
}

/* DONE */
/*
void MODIFANNOT(_spin_lock) (spinlock_t *lock) {
    return;
}
*/

/* DONE */
/*
void MODIFANNOT(__spin_lock_init)
  (spinlock_t *lock, const char *name, struct lock_class_key *key) {
  return;
}
*/

// DONE
/*
void MODIFANNOT(_spin_lock_irq) (spinlock_t *lock) {
    return;
}
*/

/* DONE */
/*
void MODIFANNOT(_spin_lock_irqsave) (spinlock_t *lock) {
    return;
}
*/

/* DONE */
/*
void MODIFANNOT(_spin_unlock)
  (spinlock_t *lock) {
  return;
}
*/

/*
void MODIFANNOT(_spin_unlock_irq) (spinlock_t *lock) {
    return;
}
*/

/* DONE */
/*
void MODIFANNOT(_spin_unlock_irqrestore)
  (spinlock_t *lock, unsigned long flags) {
  return;
}
*/

/* TODO */
struct net_device * MODIFANNOT(alloc_etherdev_mq)
    (int sizeof_priv) {
    struct net_device *dev;
    MODIFIES(dev->change_mtu);
    MODIFIES(dev->set_mac_address);
    MODIFIES(dev->type);
    MODIFIES(dev->hard_header_len);
    MODIFIES(dev->mtu);
    MODIFIES(dev->addr_len);
    MODIFIES(dev->tx_queue_len);
    MODIFIES(dev->flags);
    MODIFIES(dev->broadcast);
    MODIFIES(dev->name);
    MODIFIES(dev->_tx);
    // MODIFIES(dev->padded); // It does modify this, but we deal
    // with it ourselves in our wrapper.
    return dev;
}

/* DONE */
struct sk_buff * MODIFANNOT(__alloc_skb)
    (unsigned int size, gfp_t priority, int fclone) {
    struct sk_buff *skb;
    MODIFIES(skb->truesize);
    MODIFIES(skb->head);
    MODIFIES(skb->data);
    MODIFIES(skb->tail);
    MODIFIES(skb->end);
    return skb;
}

/* DONE */
void MODIFANNOT(dev_kfree_skb_any)
    (struct sk_buff *skb) {
    return;
}

/* DONE */
void * MODIFANNOT(dma_alloc_coherent)
    (struct device *dev, size_t size,
     dma_addr_t *dma_handle, gfp_t flag) {
    //MODIFIES(dev->dma_mem);
    MODIFIES(dev->coherent_dma_mask);
    return 0;
}

/* DONE */
void MODIFANNOT(dma_free_coherent)
    (struct device *dev, size_t size, void *vaddr, dma_addr_t dma_handle) {
    //MODIFIES(dev->dma_mem);
    return;
}

void MODIFANNOT(_memcpy_fromio)
 	(void *to , void const volatile   *from , unsigned int len ) {   

	MODIFIES(from);
	return;
 }


/* DONE */
__be16 MODIFANNOT(eth_type_trans)
    (struct sk_buff *skb, struct net_device *dev) {
    MODIFIES(skb->pkt_type);
    MODIFIES(skb->data);
    MODIFIES(skb->len);
    MODIFIES(skb->data);
    return 0;
}

/* DONE */
void MODIFANNOT(free_netdev)
    (struct net_device *dev) {
    MODIFIES(dev->reg_state);
    //MODIFIES(dev->padded); // We deal with padded in the wrapper
    return;
}

/* TODO */
int MODIFANNOT(generic_mii_ioctl)
    (struct mii_if_info *mii_if,
     struct mii_ioctl_data *mii_data, int cmd,
     unsigned int *duplex_changed) {
    MODIFIES(mii_if->phy_id);
    MODIFIES(mii_if->advertising);
    MODIFIES(mii_if->phy_id_mask);
    MODIFIES(mii_if->reg_num_mask);
    MODIFIES(mii_if->full_duplex);
    MODIFIES(mii_if->force_media);
    MODIFIES(mii_if->supports_gmii);
    MODIFIES(mii_if->mdio_read);
    MODIFIES(mii_if->mdio_write);
    return 0;
}

/* TODO */
void MODIFANNOT(kfree_skb)
    (struct sk_buff *skb) {
    return;
}

/* DONE */
unsigned int MODIFANNOT(mii_check_media)
    (struct mii_if_info *mii,
     unsigned int ok_to_print,
     unsigned int init_media)
{
    MODIFIES(mii->full_duplex);
    MODIFIES(mii->advertising);
    MODIFIES(mii->dev->state);
    
    MODIFIES(mii->mdio_read);
    MODIFIES(mii->mdio_write);
    MODIFIES(mii->phy_id);
    return 0;
}

/* DONE */
int MODIFANNOT(mii_ethtool_gset)
    (struct mii_if_info *mii, struct ethtool_cmd *ecmd) {
    MODIFIES(mii->dev);
    MODIFIES(mii->phy_id);
    MODIFIES(mii->mdio_read);
    MODIFIES(mii->mdio_write);
    /* TODO For ethtool_cmd */
    return 0;
}

/* TODO */
int MODIFANNOT(mii_ethtool_sset)
    (struct mii_if_info *mii, struct ethtool_cmd *ecmd) {
    MODIFIES(mii->advertising);
    MODIFIES(mii->force_media);
    return 0;
}

/* DONE */
int MODIFANNOT(mii_nway_restart)
    (struct mii_if_info *mii) {
    MODIFIES(mii->mdio_read);
    MODIFIES(mii->mdio_write);
    MODIFIES(mii->dev);
    MODIFIES(mii->phy_id);
    return 0;
}

/* TODO */
void MODIFANNOT(netif_carrier_off)
    (struct net_device *dev) {
    MODIFIES(dev->state);
    return;
}

void MODIFANNOT(netif_device_attach)
    (struct net_device *dev) {
    MODIFIES(dev->state);
    return;
}

void MODIFANNOT(netif_device_detach)
    (struct net_device *dev) {
    MODIFIES(dev->state);
    return;
}

int MODIFANNOT(netif_receive_skb) (struct sk_buff *skb)
{
    return 0;
}

/* TODO */
int MODIFANNOT(netif_rx)
    (struct sk_buff *skb) {
    return 0;
}

void MODIFANNOT(__netif_rx_schedule)
    (struct net_device *dev)
{
    return;
}

/* DONE */
void MODIFANNOT(__netif_schedule)
    (struct net_device *dev) {
    MODIFIES(dev->state);
    return;
}

/* TODO */
void *MODIFANNOT(page_address) (struct page *page) {
    return 0;
}

/* TODO */
int MODIFANNOT(pci_bus_read_config_byte)
    (struct pci_bus *bus,
     unsigned int devfn,
     int where,
     u8 *val)
{
    MODIFIES(val);
    return 0;
}

/* TODO */
int MODIFANNOT(pci_bus_read_config_word)
    (struct pci_bus *bus,
     unsigned int devfn,
     int where,
     u16 *val)
{
    MODIFIES(val);
    return 0;
}

pci_power_t MODIFANNOT(pci_choose_state)
    (struct pci_dev *dev,
     pm_message_t state)
{
    return 0;
}

/* TODO */
void MODIFANNOT(pci_clear_mwi) (struct pci_dev *dev) {
    return;
}

/* DONE */
void MODIFANNOT(pci_disable_device)
    (struct pci_dev *dev) {
    return;
}

/* TODO */
int MODIFANNOT(pci_enable_device)
    (struct pci_dev *dev) {
    return 0;
}

/* TODO */
int MODIFANNOT(pci_enable_wake)
    (struct pci_dev *dev,
     pci_power_t state,
     int enable)
{
    return 0;
}

/* TODO */
int MODIFANNOT(__pci_register_driver)
    (struct pci_driver *drv, struct module *owner) {
    MODIFIES(drv->driver.name);
    MODIFIES(drv->name);
    MODIFIES(drv->driver.bus);
    MODIFIES(drv->driver.owner);
    MODIFIES_ADDROF(drv->dynids.lock);
    MODIFIES_ADDROF(drv->dynids.list);
    MODIFIES_ADDROF(drv->driver);
    MODIFIES(drv->probe);

    return 0;
}

/* TODO */
void MODIFANNOT(pci_release_regions)
    (struct pci_dev *pdev)
{
    return;
}

/* TODO */
int MODIFANNOT(pci_request_regions)
    (struct pci_dev *pdev,
     const char *res_name)
{
    MODIFIES(pdev->resource);
    return 0;
}

/* TODO */
int MODIFANNOT(pci_restore_state)
    (struct pci_dev *dev)
{
    return 0;
}

/* TODO */
int MODIFANNOT(pci_save_state)
    (struct pci_dev *dev)
{
    return 0;
}

/* TODO */
int MODIFANNOT(pci_set_consistent_dma_mask)
    (struct pci_dev *dev, u64 mask)
{
    return 0;
}

/* TODO */
int MODIFANNOT(pci_set_dma_mask)
    (struct pci_dev *dev, u64 mask)
{
    return 0;
}

/* DONE */
void MODIFANNOT(pci_set_master)
    (struct pci_dev *dev) {
    return;
}

/* TODO */
int MODIFANNOT(pci_set_mwi) (struct pci_dev *dev)
{
    return 0;
}

/* TODO */
int  MODIFANNOT(pci_set_power_state)
    (struct pci_dev *dev,
     pci_power_t state)
{
    return 0;
}

/* TODO */
void MODIFANNOT(pci_unregister_driver)
    (struct pci_driver *drv) {
    MODIFIES_ADDROF(drv->driver);
    return;
}

/*int MODIFANNOT(register_netdev)
    (struct net_device *dev) {
    MODIFIES(dev->name);
    MODIFANNOT(register_netdevice)(dev);
    return 0;
*///}


/*int MODIFANNOT(register_netdevice)
    (struct net_device *dev)
{
    MODIFIES_ADDROF(dev->queue_lock);
    MODIFIES_ADDROF(dev->_xmit_lock);
    MODIFIES(dev->xmit_lock_owner);
    MODIFIES(dev->iflink);
    MODIFIES(dev->init);
    MODIFIES(dev->name);
    MODIFIES(dev->ifindex);
    MODIFIES(dev->features);
    MODIFIES(dev->rebuild_header);
    MODIFIES(dev->reg_state);
    MODIFIES(dev->next);
    MODIFIES_ADDROF(dev->name_hlist);
    MODIFIES_ADDROF(dev->index_hlist);
    return 0;
*///}

/* DONE */
/*int MODIFANNOT(request_irq)
    (unsigned int a,
     irqreturn_t (*handler)(int, void *, struct pt_regs *),
     unsigned long b, const char * c, void * d) {
    return 0;
    }*/

/* TODO */
void MODIFANNOT(skb_over_panic)
    (struct sk_buff *skb, int len, void *here) {
    return;
}

/* DONE */
void MODIFANNOT(unregister_netdev)
    (struct net_device *dev) {
    MODIFIES(dev->reg_state);
    MODIFIES_ADDROF(dev->name_hlist);
    MODIFIES_ADDROF(dev->index_hlist);
    MODIFIES(dev->name);
    MODIFIES(dev->uninit);
    MODIFIES(dev->mc_list);
    MODIFIES(dev->mc_count);
    return;
}

void MODIFANNOT(free_irq)
    (unsigned int irq , struct net_device *dev )
{
    return;
}

void MODIFANNOT(cancel_rearming_delayed_work)
    (struct work_struct *work)
{
}

int MODIFANNOT(schedule_delayed_work)
    (struct delayed_work *work,
     unsigned long delay)
{
    MODIFIES(work->work.data.counter);
    MODIFIES(work->work.entry.next);
    MODIFIES(work->work.entry.prev);
    MODIFIES(work->work.func);
    MODIFIES(work->timer.entry.next);
    MODIFIES(work->timer.entry.prev);
    MODIFIES(work->timer.expires);
    MODIFIES(work->timer.function);
    MODIFIES(work->timer.data);
    return 0;
}

void MODIFANNOT(init_timer)(struct timer_list *timer)
{
    // These are marshaled, but it saves modifying timer.h (see e1000)
    /*MODIFIES(timer->entry.next);
      MODIFIES(timer->base);*/
}

/*
void MODIFANNOT(_spin_unlock_bh)(spinlock_t *lock)
{
}

void MODIFANNOT(_spin_lock_bh)(spinlock_t *lock)
{
}
*/

int MODIFANNOT(mii_link_ok) (struct mii_if_info *mii)
{
    MODIFIES(mii->mdio_read);
    MODIFIES(mii->dev);
    MODIFIES(mii->phy_id);
}

void MODIFANNOT(skb_copy_and_csum_dev)
    (const struct sk_buff *skb,
     u8 *to)
{
	
}

void MODIFANNOT(netif_stop_queue) (struct net_device * dev)
{
  MODIFIES(dev->dev.parent); 
  return;	
}

