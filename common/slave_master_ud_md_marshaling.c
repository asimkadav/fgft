#include "full_slab_verify.h"

//#define MARSH_PRINT

//void MJR_extern_free(void *);
void * MJR_extern_alloc(int x);
#define ALLOC(x) MJR_extern_alloc(x) 
/*

void * ALLOC(size_t size) {
    void * temp = NULL;
    static spinlock_t marshbuf;
    if (size == 0)  {
#ifdef MARSH_PRINT    
        printk ("Allocating %d bytes.\n", MAX_MARSH_BUFF_SIZE);
#endif
        temp = kmalloc (MAX_MARSH_BUFF_SIZE, GFP_ATOMIC);
        //spin_lock_irq(&marshbuf);
        //marshbuf_pointers[marshbuf_index] = temp;
        //marshbuf_index++;
        //spin_unlock_irq(&marshbuf);

        //printk ("ALLOC: Allocated memory %p of %d size at the %dth allocation.\n",temp, marshbuf_index, marshbuf_index);
        return temp;
    }

    printk ("THIS IS NEVER TO BE CALLED>\n");
    return NULL;


}
*/



//#define FREE(x) MJR_free(x)

/*---------------------------------------------------------------------------*/
/* Code that is used during marshaling */
/*---------------------------------------------------------------------------*/
/*
static void validate_pointer (const void *ptr) { 
    return;
    if (
        (
            ptr >= ((void *) -4095) ||
            ptr <= ((void *)  4095) ||
            ptr == ((void *) 0x6b6b6b6b6b6b6b6b)
         )
        || 
	
	ptr == 0x0
        )
    {
        printk ("%s: validation failed, pointer is %p\n", __func__, ptr);
        TERMINATE();
    }


}

*/

/* fill_marshbuf: fills the buffer 'marshbuf' at offset 'marshoff' with the 
 * contents of 'tocopy'. The number of bytes of data contained in tocopy is
 * stored in sztocopy
 */
void fill_marshbuf (const char *function_name,
                    void **marshbuf,
                    int *marshoff,
                    const void *tocopy, int sztocopy) 
{
    //full_slab_verify();
    if (function_name == NULL) {
        PRINT("%s:  function_name is NULL!\n", __FUNCTION__);
        TERMINATE();
    }

    if (*marshbuf == NULL) {
        *marshbuf = ALLOC(0);
        //printk ("Verifying slab again..\n");
        //full_slab_verify();
    }

    if (MAX_MARSH_BUFF_SIZE < *marshoff)    {
        printk ("Incorrect marshalling  - let's panic.\n");
        panic ("Marsh buffer too small.\n");
    }

#ifdef MARSH_PRINT
    printk ("In fill_marshbuf for %s, *marshbuf %p *marshoff %d, tocopy %p, size %d", function_name,
                *marshbuf, *marshoff, tocopy, sztocopy);
#endif		
    //full_slab_verify();

    // Trivial case
    if (tocopy == 0U) {
        *marshoff += sztocopy;
    }

#ifdef MARSH_PRINT
    printk ("%s copying data of size %d to %p, returning %p\n", __FUNCTION__, sztocopy, *marshbuf + *marshoff, *marshbuf);
#endif
    memcpy (
        *marshbuf + *marshoff,
        tocopy,
        sztocopy);

    *marshoff += sztocopy;
#ifdef MARSH_PRINT    	
    printk ("marsh off %d sztocopy %d.\n", *marshoff, sztocopy); 
    //full_slab_verify();
    printk ("\nDone with fill_marshbuf : new offset is %p %d.\n", *marshbuf + *marshoff, sztocopy);
#endif    
}

/* fill_marshbuf_ptr: fills the buffer 'marshbuf' at offset 'marshoff' with
 * the contents of 'tocopy'. In this case, 'tocopy' contains a pointer 
 * Here, the contents of the void * location are suposed to be the pointer.
 * That is, the void * is not the pointer itself.
 */
void fill_marshbuf_ptr (const char *function_name,
                        void **marshbuf,
                        int *marshoff,
                        const void *tocopy)
{
    int ptrsz = sizeof (void *);
#ifdef MARSH_PRINT    
    printk ("In fill_marshbuf_ptr for marshbuff %p, marshoff %p, functions name %s and tocopy %p.\n", marshbuf, marshoff, function_name, tocopy);
#endif
    if (tocopy == 0)
    {
        PRINT ("fill_marshbuf_ptr: Don't pass a NULL value.  Things are going fail.\n");
        TERMINATE();
    }
    
    //validate_pointer (tocopy);
    //full_slab_verify();
    // The mouse device copies a size_t as a void * (sigh!)
    //validate_pointer (*(const void **) tocopy);
    fill_marshbuf (function_name, marshbuf, marshoff, tocopy, ptrsz);
}

/*---------------------------------------------------------------------------*/
/* Code that is used during demarshaling */
/*---------------------------------------------------------------------------*/

/* fetch_marshbuf: reads off the buffer 'marshbuf' at the offset 'marshoff'.
 * It reads 'sztoread' bytes from this offset and stores the result in the
 * void * 'result' blob. Assumes that sufficient memory has already been 
 * allocated in the 'result' blob and so does no memory mgmt. on its own.
 */
void fetch_marshbuf (const char *function_name,
                     void *marshbuf,
                     int *marshoff,
                     int sztoread,
                     void *result)
{
    void *currptr;
    if (function_name == NULL) {
        PRINT("%s:  function_name is NULL!\n", __FUNCTION__);
        TERMINATE();
    }


    if (result == 0U) {
        PRINT ("%s: You passed me a NULL result\n", __FUNCTION__);
        //*marshoff += sztoread;
        TERMINATE();
        //return;
    }

    if (marshbuf == NULL) {
        PRINT ("%s: You passed me a NULL buffer!  Things will fail now.\n", __FUNCTION__);
        TERMINATE();
    }

    //PRINT ("%s copying data to %p\n", __FUNCTION__, result);
    
    currptr = marshbuf + *marshoff;
#ifdef MARSH_PRINT    
    printk ("fetch: %s copying data %p to %p of size %d from %p, returning %p\n", __FUNCTION__, * (unsigned long *)currptr, result, sztoread, currptr, marshbuf);
#endif    
    memcpy(
        result,
        currptr,
        sztoread);

	

    // This should only be done during on our way to SFI. (why?)
    odft_insert_range_hash(function_name, result, sztoread, 2);

#ifdef MARSH_PRINT    
    printk ("INSERTING %p %d in hashtable.\n", result, sztoread); 
    //full_slab_verify();
#endif
    *marshoff += sztoread;
    return;
}

/* fetch_marshbuf_ptr: reads a pointer value from marshbuf. To get the
 * value of the pointer, you will have to dereference 'result'
 * sizeof_struct: The size of the structure being pointed to.
 *
 * Returns the untranslated pointer in case it's needed, as in the case
 * of marshaling wrapper functions.  The automatically generated
 * marshaling code won't use this.
 *
 * If sizeof_struct == -1, then this does not allocate memory and does not
 * perform the pointer translation.  This is useful for iomem in user
 * mode, since we don't want to allocate memory in that case.
 *
 * TODO we really need a mechanism for generating generic "identifiers"
 * rather than memory addresses to properly handle void * crap.
 * As it is, we are forced to allocate a 1-byte "object" to represent a
 * a void * in the table.
 */

void *fetch_marshbuf_ptr (const char *function_name,
                          void *marshbuf,
                          int *marshoff,
                          void **result,
                          int sizeof_struct,
			  int real_size)
{
    int ptrsz = sizeof (void *);
    void *temporary_pointer = 0;

    *result = NULL; // No harm in doing this.

    fetch_marshbuf (function_name, marshbuf, marshoff, ptrsz, &temporary_pointer);

    // The untranslated pointer should be valid.
    //validate_pointer (temporary_pointer);

    switch (sizeof_struct) {
        case -2:
            // In this case, we do the translation, but we don't allocate
            // memory.  If there is no mapping already present, NOOKS_OT_LOOKUP
            // will crash/panic the process so be sure the mapping exists
            // already :)
	    printk ("THis IS NOT USED *****.\n");
            NOOKS_OT_LOOKUP(temporary_pointer, result, -1);
            // NOOKS_OT_LOOKUP resolves to nooks_ot_lookup_kern
            break;
        case -1:
            // No translation
            // printk ("++++++Doing no translation for %s for pointer %p.\n\n", function_name, temporary_pointer);
            *result = temporary_pointer;
            break;
        default:
            // Translate and allocate if necessary
            NOOKS_OT_LOOKUP(temporary_pointer, result, sizeof_struct);
	    real_size = 8;
            break;
    }

    //printk ("**********fetch_marshbuf_ptr: Fn Name: %s and pointer %p and size %d for fetch.", function_name, result, sizeof_struct);
    // This will always be a pointer.
    //
    // FIXME 
    if (sizeof_struct < 0)     {
	    if (real_size < 64)	{
		    real_size = 64;
	    }
        odft_insert_range_hash(function_name, result, 64, 1);
        odft_insert_range_hash(function_name, *result, real_size, 1);
        //printk ("INSERTING %p %d.\n", *result, real_size);  
	 //We give I/O address spave a leeway of 64 byes
    }
    else    {
        // This only gets the pointer, so we just cannot give a free reign to access
	// lots of bytes. Explicitly everything needs to be marshalled in
	
	odft_insert_range_hash(function_name, result, real_size, 2);
    	//printk ("INSERTING %p(contains %p) %d in hashtable.\n", result, *result, sizeof_struct);
    }

    return temporary_pointer;
}
/*
int is_safe_hash_lookup(const char *function_name, void * addr )   {
  if (scan_hash (function_name, addr)  == 0)
      return 0;
      else return 1;
 }
*/

