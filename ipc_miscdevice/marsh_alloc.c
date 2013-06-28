#include <linux/kernel.h>
#include <linux/slab.h>


// Memory management
#define NUM_BUFFERS 20 
#define BUFFER_SIZE (1024 * 32)

static char allocated_buffers[NUM_BUFFERS];
static char memory_buffers[NUM_BUFFERS][BUFFER_SIZE];


static int MJR_alloc_index (void) {
    int i;
    for (i = 0; i < NUM_BUFFERS; i++) {
        if (allocated_buffers[i] == 0) {
            allocated_buffers[i] = 1;
            return i;
        }
    }

    printk ("Failed to find a free buffer in %s!\n", __func__);
    panic("FAIL");
}

void *MJR_extern_alloc (int length) {
    int i;
    void *retval = NULL;

    if (length > BUFFER_SIZE) {
        printk ("Requested too large a buffer!  Length: %d\r\n", length);
        panic("out of mem fail");
    }

    if (length < 0) {
        length = BUFFER_SIZE;
    }

    i = MJR_alloc_index ();
    //printk ("Allocated buffer %d at addr %p\r\n", i, &memory_buffers[i]);
    retval = &memory_buffers[i];

    return retval;
}

static void MJR_clear_state (void) {
    int i;

    for (i = 0; i < NUM_BUFFERS; i++) {
        allocated_buffers[i] = 0;
        memset (memory_buffers[i], 0, BUFFER_SIZE);
    }
}

void MJR_extern_free (void *data) {
    int i;

    if (data == NULL) {
        return;
    }
    
    for (i = 0; i < NUM_BUFFERS; i++) {
        if (data == &memory_buffers[i]) {
            //printk ("Free'd memory at index %d address %p\r\n", i, data);
            allocated_buffers[i] = 0;
            return;
        }
    }

    printk ("Failed to free memory at address %p\r\n", data);
    for (i = 0; i < NUM_BUFFERS; i++) {
        printk ("Addresses include: %p\r\n", &memory_buffers[i]);
    }
    panic("fail");
}  

 
