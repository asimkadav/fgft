// This file is included in both the u-driver and k-driver

#ifndef USER_KERN_SHARED_H
#define USER_KERN_SHARED_H

struct marshret_struct {
    void *buf;
    int len;
};

// Request arguments
struct req_args {
    unsigned long function_id; // ID of the function for the dispatch function
    int length;
    void *data;
};

// User/kernel control transfer
int disp_kern (char *function_name, struct req_args *ret_data);
int disp_user (char *function_name, struct req_args *req_data);
void dispatch_user_request (char *function_name, struct req_args *misc_fnargs);


// Driver initialization in the master
void register_miscfn (void *fnptr, char function_id_map[][128], int function_id_map_len);
void register_functions (void);
void register_globals (void);

// Monitor execution
void record_function (const char *fn);

#endif
