#ifndef DEMARSHBUF_FREE_H
#define DEMARSHBUF_FREE_H

extern void MJR_extern_free(const void *);

/* Corresponds with kernel reply.  */
 #define DEMARSHBUF_FREE(x) do { MJR_extern_free(x); x = 0; } while (0)
 /* Corresponds to call kernel (i.e. __MARSH_WRAP__). */
 #define DEMARSHBUF_FREE2(x) do { MJR_extern_free(x); x = 0; } while (0)
#endif


/*

void kfree(const void *);

 #define DEMARSHBUF_FREE(x) do { kfree(x); x = 0; } while (0)
 #define DEMARSHBUF_FREE2(x) do { kfree(x); x = 0; } while (0)
#endif

*/
