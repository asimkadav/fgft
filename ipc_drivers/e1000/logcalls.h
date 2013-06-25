void logRead(void *addr , char *what , char *file , int line)__attribute__((isolate)) ;
void logWrite(void *addr , char *what , char *file , int line)__attribute__((isolate)) ;
void logStackFrame(char *func )__attribute__((isolate)) ;
void logAlloc(void *addr , int size , char *file , int line )__attribute__((isolate)) ;
void logFree(void *addr , char *file , int line )__attribute__((isolate)) ;


void logStackFrame(char *func )__attribute__((isolate)) {
    return;
}

void logWrite(void *addr , char *what , char *file , int line ) {
    return;  
 }
void logRead(void *addr , char *what , char *file , int line ) {
    return;  
 }
