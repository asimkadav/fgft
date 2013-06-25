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

s/int ( \/\* missing proto \*\/  uprintk/\/\/ Matt E3 uprintk proto/
s/struct module __this_module/extern struct module __this_module/

