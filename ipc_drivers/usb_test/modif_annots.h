#define MODIFANNOT(X)  MICRODRIVERS__MODIF_ ## X
#define MODIFIES(var)  MICRODRIVERS__DUMMY((unsigned long) var) /* MJR: Was void * */
#define READS(var)     MICRODRIVERS__DUMMY((void *) var)
#define MODIFIES_ADDROF(var) MICRODRIVERS__DUMMY((unsigned long) &var) /* MJR: Was void */

void MICRODRIVERS__DUMMY(unsigned long x) { /* MJR Was void */
  return;
}


int  MODIFANNOT(usb_register)(struct usb_driver * driver) {
    /* usb_register generated automatically */
    // MODIFANNOT(usb_register_driver)(driver, & __this_module, "musb_core");
    READS(driver);
    return 0;
}

int  MODIFANNOT(usb_register_driver)(struct usb_driver * new_driver,
                                     struct module * owner,
                                     char const   * mod_name) {
    /* usb_register_driver generated automatically */
    // MODIFANNOT(INIT_LIST_HEAD)(& new_driver->dynids.list);
    // MODIFANNOT(__spin_lock_init)(& new_driver->dynids.lock, "&new_driver->dynids.lock", & __key___2);
    // MODIFANNOT(driver_register)(& new_driver->drvwrap.driver);
    // MODIFANNOT(printk)("<3>%s: error %d registering interface \tdriver %s\n", usbcore_name, retval, new_driver->nam);
    // MODIFANNOT(printk)("<6>%s: registered new interface driver %s\n", usbcore_name, new_driver->name);
    // MODIFANNOT(usb_create_newid_file)(new_driver);
    // MODIFANNOT(usb_disabled)(void);
    // MODIFANNOT(usbfs_update_special)(void);
    MODIFIES(new_driver->drvwrap.driver.bus);
    MODIFIES(new_driver->drvwrap.driver.mod_name);
    MODIFIES(new_driver->drvwrap.driver.name);
    MODIFIES(new_driver->drvwrap.driver.owner);
    MODIFIES(new_driver->drvwrap.driver.probe);
    MODIFIES(new_driver->drvwrap.driver.remove);
    MODIFIES(new_driver->drvwrap.for_devices);
    READS(mod_name);
    READS(new_driver);
    READS(new_driver->name);
    READS(owner);
    return 0;
}
