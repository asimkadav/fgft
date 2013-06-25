#define MODIFANNOT(X)  MICRODRIVERS__MODIF_ ## X
#define MODIFIES(var)  MICRODRIVERS__DUMMY((unsigned long) var) /* MJR: Was void * */
#define READS(var)     MICRODRIVERS__DUMMY((void *) var)
#define MODIFIES_ADDROF(var) MICRODRIVERS__DUMMY((unsigned long) &var) /* MJR: Was void */

void MICRODRIVERS__DUMMY(unsigned long x) { /* MJR Was void */
  return;
}

/*
int MODIFANNOT(test_driver_function_nonexistent) (struct blah *b)
{
    MODIFIES(b->blah_field);
}

*/
