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
N
s/.*/#include \"..\/..\/common\/master_top.h\" \/\/ Matt E1/
}

# Add the master_middle.h header file.
# /^void MICRODRIVERS__DUMMY/{
/^int MJR_middle  ;/{
s/\(.*\)/\#include \"..\/..\/common\/master_middle.h\" \/\/Matt E2 \
    \1/
}

# Add the appropriate header file to init_module
/^int init_module/{
N
s/\(int init_module.*\n{ \)/\1 \
\#include \"..\/..\/common\/master_init_module.h\" \/\/Matt E3 \
 /
}

s/extern int ( \/\* missing proto \*\/  uprintk/\/\/ uprintk proto/
