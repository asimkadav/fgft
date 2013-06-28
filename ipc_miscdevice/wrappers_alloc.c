#include "../common/slave_master_ud_md_marshaling.h"
#include "../common/demarshbuf_free.h"
#include "../common/MJR_external_functions.h"

#include "misc.h"
#include "wrappers_alloc.h"

#include <linux/netdevice.h>
#include <linux/etherdevice.h>

#include "common_h.h"

extern pnooks_hash_table_t g_nooks_table;

// Wrapper function prototypes.
static void __get_free_pages_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void alloc_etherdev_mq_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void dev_alloc_skb_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void __netdev_alloc_skb_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void dev_kfree_skb_any_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void kfree_skb_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void copy_from_user_wrapper(void *_buf_ , struct marshret_struct *_ret_ );
static void copy_to_user_wrapper(void *_buf_ , struct marshret_struct *_ret_ );
static void current_thread_info_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void virt_to_page_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void INIT_WORK_wrapper(void *_buf_, struct marshret_struct *_ret_);
static void INIT_DELAYED_WORK_wrapper(void *_buf_, struct marshret_struct *_ret_);
    
//
// Returns 0 if the call is handled, 1 if not.
//
int disp_wrapper (struct req_args *arg) {
    static struct marshret_struct retval;
    int not_handled;

    not_handled = 0; // Assume we'll handle it unless shown otherwise
    retval.buf = 0;

    // This is unsigned long by default but we get weird relocation otherwise R_X86_64_32S
    switch (arg->function_id) {
        case WRAPPERS_ALLOC_GET_FREE_PAGES:
            __get_free_pages_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_ALLOC_ETHERDEV_MQ:
            alloc_etherdev_mq_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_DEV_ALLOC_SKB:
            dev_alloc_skb_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_NETDEV_ALLOC_SKB:
            __netdev_alloc_skb_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_DEV_KFREE_SKB_ANY:
            dev_kfree_skb_any_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_KFREE_SKB:
            kfree_skb_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_COPY_FROM_USER:
            copy_from_user_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_COPY_TO_USER:
            copy_to_user_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_CURRENT_THREAD_INFO:
            current_thread_info_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_VIRT_TO_PAGE:
            virt_to_page_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_INIT_WORK:
            INIT_WORK_wrapper (arg->data, &retval);
            break;
        case WRAPPERS_ALLOC_INIT_DELAYED_WORK:
            INIT_DELAYED_WORK_wrapper (arg->data, &retval);
            break;            
        default:
            not_handled = 1; // Not handled, it must be a normal function
            break;
    }

    if (not_handled == 0) {
        arg->data = retval.buf;
        arg->length = retval.len;
    }

    return not_handled;
}

#if 0
static void __get_free_pages_wrapper(void *_buf_, struct marshret_struct *_ret_) {
    int _off_;
    unsigned long retval;
    gfp_t gfp_mask;
    unsigned int order;
    
    _off_ = 0;
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(gfp_t ), (void *)(& gfp_mask));
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned int ), (void *)(& order));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    retval = __get_free_pages(gfp_mask, order);
    _off_ = 0;
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval), sizeof(unsigned long ));
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void alloc_etherdev_mq_wrapper(void *_buf_, struct marshret_struct *_ret_ )
{
    int _off_;
    int sizeof_priv ;
    unsigned int queue_count ;
    struct net_device *retval6 ;
    int idx_arr17 ;

    _off_ = 0;
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(int ), (void *)(& sizeof_priv));
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned int ), (void *)(& queue_count));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    retval6 = 0U;
    retval6 = (struct net_device *)alloc_etherdev_mq(sizeof_priv, queue_count);
    _off_ = 0;
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& sizeof_priv), sizeof(int ));
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& queue_count), sizeof(unsigned int ));
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & retval6);
    if (retval6 != 0U) {
        idx_arr17 = 0;
        while (idx_arr17 < 16) {
            fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->name[idx_arr17]),
                                          sizeof(char ));
            idx_arr17 ++;
        }
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & retval6->/*__annonCompField10.*/change_mtu);
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & retval6->/*__annonCompField10.*/set_mac_address);
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & retval6->_tx);
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->tx_queue_len),
                                      sizeof(unsigned long ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->type), sizeof(unsigned short ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->addr_len), sizeof(unsigned char ));
        idx_arr17 = 0;
        while (idx_arr17 < 32) {
            fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->broadcast[idx_arr17]),
                                          sizeof(unsigned char ));
            idx_arr17 ++;
        }
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->flags), sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->mtu), sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->hard_header_len),
                                      sizeof(unsigned short ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval6->num_tx_queues),
                                      sizeof(unsigned int ));
    }
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void dev_alloc_skb_wrapper(void *_buf_, struct marshret_struct *_ret_)
{
    int _off_ ;
    unsigned int length ;
    struct sk_buff *_retval_ ;
    unsigned long arraylen___MARSH_WRAP__dev_alloc_skb_retval_data6 ;
    unsigned long idx210 ;
    int btfld8 ;

    _off_ = 0;
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned int ), (void *)(& length));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    _retval_ = 0U;
    _retval_ = (struct sk_buff *)dev_alloc_skb(length);
    _off_ = 0;
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& length), sizeof(unsigned int ));
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_);
    if (_retval_ != 0U) {
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->protocol),
                                      sizeof(unsigned short ));

        // MJR Copied from netdev_alloc_skb ------------------------------
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->tail),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->end),
                                      sizeof(unsigned int ));
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_->head);
        if (_retval_->head != 0U) {
            // MJR See driver process demarshaling code for explanation.
            arraylen___MARSH_WRAP__dev_alloc_skb_retval_data6 = _retval_->end + sizeof(struct skb_shared_info); // MJR MAJOR HACK.
            fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& arraylen___MARSH_WRAP__dev_alloc_skb_retval_data6), sizeof(int ));
            
            idx210 = 0;
            while (idx210 < arraylen___MARSH_WRAP__dev_alloc_skb_retval_data6) {
                fill_marshbuf(__func__, &_buf_, & _off_, (void *)(_retval_->head + idx210),
                                              sizeof(unsigned char ));
                idx210 ++;
            }
        }
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_->data);
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->truesize),
                                      sizeof(unsigned int ));
        // MJR <-----------------------------

        btfld8 = _retval_->pkt_type;
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& btfld8), sizeof(unsigned char ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->len),
                                      sizeof(unsigned int ));
    }
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void __netdev_alloc_skb_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{
    int _off_ ;
    struct net_device * __attribute__((__noderef__, __address_space__(2))) dev ;
    unsigned int length ;
    gfp_t gfp_mask ;
    struct sk_buff *_retval_ ;
    int btfld8 ;
    int arraylen___MARSH_WRAP____netdev_alloc_skb_retval_head9 ;
    int idx210 ;

    _off_ = 0;
    dev = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& dev), sizeof(struct net_device ));
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned int ), (void *)(& length));
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned int ), (void *)(& gfp_mask));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    _retval_ = 0U;
    _retval_ = (struct sk_buff *)__netdev_alloc_skb(dev, length, gfp_mask);
    _off_ = 0;
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & dev);
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& length),
                                  sizeof(unsigned int ));
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& gfp_mask),
                                  sizeof(unsigned int ));
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_);
    if (_retval_ != 0U) {
        btfld8 = _retval_->pkt_type;
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& btfld8),
                                      sizeof(unsigned char ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->tail),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->transport_header),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->csum),
                                      sizeof(unsigned int ));
        btfld8 = _retval_->ip_summed;
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& btfld8),
                                      sizeof(unsigned char ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->data_len),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->len),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->network_header),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->end),
                                      sizeof(unsigned int ));
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->protocol),
                                      sizeof(unsigned short ));
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_->head);
        if (_retval_->head != 0U) {
            // MJR See driver process demarshaling code for explanation.
            arraylen___MARSH_WRAP____netdev_alloc_skb_retval_head9 = _retval_->end + sizeof(struct skb_shared_info); // MJR MAJOR HACK TODO
            fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& arraylen___MARSH_WRAP____netdev_alloc_skb_retval_head9), sizeof(int ));
            idx210 = 0;
            while (idx210 < arraylen___MARSH_WRAP____netdev_alloc_skb_retval_head9) {
                fill_marshbuf(__func__, &_buf_, & _off_, (void *)(_retval_->head + idx210),
                                              sizeof(unsigned char ));
                idx210 ++;
            }
        }
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_->data);
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_->truesize),
                                      sizeof(unsigned int ));
    }
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void dev_kfree_skb_any_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{
    int _off_ ;
    struct sk_buff *skb ;

    _off_ = 0;
    skb = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& skb), sizeof(struct sk_buff ));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    dev_kfree_skb_any(skb);
    _off_ = 0;
    //fill_marshbuf_ptr(__func__, &_buf_, & _off_, & skb);
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void kfree_skb_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{ int _off_ ;
    struct sk_buff *skb ;

    _off_ = 0;
    skb = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& skb), sizeof(struct sk_buff ));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    kfree_skb(skb);
    _off_ = 0;
    //fill_marshbuf_ptr(__func__, &_buf_, & _off_, & skb);
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}  

static void copy_from_user_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{
    int _off_ ;
    void *to, *tempto;
    int idx08 ;
    void *from ;
    int n ;
    int _retval_ ;

    _off_ = 0;

    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(int ), (void *)(& n));

    to = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& to), -1);
    from = 0U;
    //fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& from), -1);
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned long ), (void *)(& from));
    
    tempto = kmalloc (n, GFP_KERNEL);
    if (tempto ==  NULL) {
        panic ("Out of memory in %s\n", __func__);
    }
    
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    _retval_ = (int )copy_from_user(tempto, from, n);
    
    _off_ = 0;
    //fill_marshbuf(__func__, &_buf_, & _off_, (void *)(&n), sizeof(int ));
    //fill_marshbuf_ptr(__func__, &_buf_, & _off_, & to);
    if (tempto != 0U) {
        idx08 = 0;
        while (idx08 < n) {
            fill_marshbuf(__func__, &_buf_, & _off_, &((char *)tempto)[idx08],
                                          sizeof(char const   ));
            idx08 ++;
        }
    }

    //fill_marshbuf_ptr(__func__, &_buf_, & _off_, & from);
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_),
                                  sizeof(int ));
    _ret_->buf = _buf_;
    _ret_->len = _off_;
    kfree (tempto);
}

static void copy_to_user_wrapper(void *_buf_ , struct marshret_struct *_ret_ ) 
{
    int _off_ ;
    void *to ;
    void *from ;
    int n ;
    int _retval_ ;
    void *tempfrom;

    _off_ = 0;
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(int ), (void *)(& n));
    
    to = 0U;
    //fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& to), -1);
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned long ), (void *)(& to));
    from = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& from), -1);

    tempfrom = kmalloc (n, GFP_KERNEL);
    if (tempfrom ==  NULL) {
        panic ("Out of memory in %s\n", __func__);
    }
    memcpy (tempfrom, &((char *) _buf_)[_off_], n);

    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    _retval_ = (int )copy_to_user(to, tempfrom, n);
    _off_ = 0;

    //fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& n), sizeof(int ));
    //fill_marshbuf_ptr(__func__, &_buf_, & _off_, & to);
    //fill_marshbuf_ptr(__func__, &_buf_, & _off_, & from);
    fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& _retval_), sizeof(int ));
    _ret_->buf = _buf_;
    _ret_->len = _off_;

    kfree(tempfrom);
}

//
// Copied definition from current_thread_info
// Alternate approach:  use separate arch/um headers for linux-2.6.29-ipc
// and for the annotated directory.
//
// One set would have to include the modified version of current_thread_info
// to support all the sound library crap in the driver process.  The other
// would retain the original implementation.
//
// We just do it this way since having two copies of the arch header files
// would be difficult to maintain.
//
// TODO AGAIN:  I'm an idiot.  Just use MJR_IPC preprocessor definition to separate
// these two definitions.
//
static void current_thread_info_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{
    int _off_ ;
    struct thread_info *_retval_ ;

    _off_ = 0;
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    _retval_ = 0U;
    _retval_ = (struct thread_info *)current_thread_info();
    _off_ = 0;
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_);
    if (_retval_ != 0U) {
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, & _retval_->task);
        if (_retval_->task != 0U) {
            fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& (_retval_->task)->pid),
                                          sizeof(int ));
        }
    }
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void virt_to_page_wrapper(void *_buf_, struct marshret_struct *_ret_) {
    int _off_;
    unsigned long user_vaddr;
    void *kernel_vaddr;
    struct page *retval;
    
    
    _off_ = 0;
    fetch_marshbuf(__func__, _buf_, & _off_, sizeof(unsigned long ), (void *)(& user_vaddr));
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }

    // Translate the user pointer
    nooks_ot_lookup_kern_no_create(g_nooks_table, (void *) user_vaddr, (void **) &kernel_vaddr);

    if (kernel_vaddr == NULL) {
        panic ("Failure to translate %p to %p\n", (void *) user_vaddr, (void *) kernel_vaddr);
    }
    
    retval = virt_to_page(kernel_vaddr);

    // MJR: HACK.  The user-mode code doesn't know where this struct page *
    // comes from so it never marshals it?  UNKNOWN EXPLANATION. If
    // we get "bad_page" errors look into this line and whether it should be here.
    get_page (retval);
    
    _off_ = 0;
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & retval);
    if (retval != 0U) {
        fill_marshbuf(__func__, &_buf_, & _off_, (void *)(& retval->_count.counter),
                                      sizeof(long ));
    }
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void INIT_WORK_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{
    int _off_ ;
    struct work_struct * __attribute__((__noderef__, __address_space__(2))) w ;
    void * __attribute__((__noderef__, __address_space__(2))) func ;

    _off_ = 0;
    w = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& w), sizeof(struct work_struct ));
    func = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& func), -2);
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    INIT_WORK(w, func);
    _off_ = 0;
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & w);
    if (w != NULL) {
        fill_marshbuf_ptr(__func__, &_buf_, & _off_, (void **)(& w->func));
    }
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & func);
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

static void INIT_DELAYED_WORK_wrapper(void *_buf_ , struct marshret_struct *_ret_ )
{
    int _off_ ;
    struct delayed_work *w ;
    void *func ;
    void *tmp_lv6 ;
    void *tmp_fld7 ;

    _off_ = 0;
    w = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& w), sizeof(struct delayed_work ));
    func = 0U;
    fetch_marshbuf_ptr(__func__, _buf_, & _off_, (void **)(& func), -2);
    if (w != 0U) {
        tmp_fld7 = & w->timer;
        tmp_lv6 = w;
        nooks_ot_storeoffset_kern(g_nooks_table, w, tmp_fld7 - tmp_lv6, sizeof(struct timer_list));
        tmp_fld7 = & w->work;
        tmp_lv6 = w;
        nooks_ot_storeoffset_kern(g_nooks_table, w, tmp_fld7 - tmp_lv6, sizeof(struct work_struct));
    }
    
    if (_buf_ != 0U) {
        DEMARSHBUF_FREE2(_buf_);
    }
    INIT_DELAYED_WORK(w, func);
    _off_ = 0;
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & w);
    fill_marshbuf_ptr(__func__, &_buf_, & _off_, & func);
    _ret_->buf = _buf_;
    _ret_->len = _off_;
}

#endif
