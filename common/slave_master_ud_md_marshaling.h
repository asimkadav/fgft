#ifndef MARSHALING_H
#define MARSHALING_H

void fill_marshbuf (const char *function_name,
                    void **marshbuf,
                    int *marshoff,
                    const void *tocopy,
                    int sztocopy);
void fill_marshbuf_ptr (const char *function_name,
                        void **marshbuf,
                        int *marshoff,
                        const void *tocopy);
void fetch_marshbuf (const char *function_name,
                     void *marshbuf,
                     int *marshoff,
                     int sztoread,
                     void *result);
void *fetch_marshbuf_ptr (const char *function_name,
                          void *marshbuf,
                          int *marshoff,
                          void **result,
                          int sizeof_struct,
			  int real_size);

#define MAX_MARSH_BUFF_SIZE 100000
//static void * marshbuf_pointers[100];
//static int marshbuf_index =0;
//static spinlock_t marshbuf_index_lock=NULL;

extern void odft_init_range_hashtable(void);
extern int odft_insert_range_hash (const char *, void *, int, int);
extern int odft_check_range_hash (const char *, void *);


#endif
