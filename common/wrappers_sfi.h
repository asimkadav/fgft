#include <linux/slab.h>
#include <linux/kernel.h>
#include <linux/signal.h>

extern void odft_init_range_hashtable(void);
extern int odft_insert_range_hash (const char *, void *, int);
extern int odft_check_range_hash (const char *, void *);
extern int odft_delete_range_hash (const char *, void *);
extern int odft_insert_klog_hash(const char *, void *, int, void *, int);
extern void rollback_klog(void);
extern void * odft_check_klog_hash (const char*, int);
extern void * odft_delete_klog_hash (const char*, int);


int logRead(void *addr , char *what, char *where,  char *file , int line);
int logWrite(void *addr , char *what,char *where,  char *file , int line);
void logStackFrame(char *func );
int logStackVar(char *func );
int logAlloc(void *addr , int size , char *fn, char *file , char *allocfn, int line );
void logFree(void *addr , char *fn, char *file , char *allocfn, int line );

void heapifyfree (void * addr);
void *heapifymalloc(int sizetoalloc);
void self_destruct(void);
void odft_record_failure(void);

#define ODFT_INJECT_FAULTS 0

#ifdef ODFT_INJECT_FAULTS
extern int odft_pointer_to_flip(void);
static int pointer_dereferenced = 0;

extern int odft_stack_to_corrupt(void);
static int stack_corrupted = 0;

extern int odft_stack_to_really_corrupt(void);
static int stack_really_corrupted = 0;
#endif

#define MAX_MARSH_BUFF_SIZE 100000
//extern void * marshbuf_pointers[1000];
// //extern int marshbuf_index;
extern void * getmarshbuftop(void);
