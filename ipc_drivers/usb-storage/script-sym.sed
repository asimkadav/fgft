# Comment out the extra stuff
/^struct marshret_struct {/{
N
N
N
N
N
N
N
N
s/.*/#include \"..\/..\/common\/slave_top.h\" \/\/ Matt E1/
}

# Fix __mod_usb_device_table
s/struct usb_device_id  const  __mod_usb_device_table/extern struct usb_device_id  const  __mod_usb_device_table \/\* Matt E2 \*\//

s/int ( \/\* missing proto \*\/  uprintk/\/\/ Matt E3 uprintk proto/
