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

# Fix __mod_pci_device_table
# s/struct pci_device_id  const  __mod_pci_device_table/extern struct pci_device_id  const  __mod_pci_device_table/

s/int ( \/\* missing proto \*\/  uprintk/\/\/ uprintk proto/
