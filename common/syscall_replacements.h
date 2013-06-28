#ifndef SYSCALL_REPLACEMENTS_H
#define SYSCALL_REPLACEMENTS_H

// We define this regardless because there is no native version
unsigned long MJR_strlcat(char *dest, const char *src, unsigned long count);
unsigned long MJR_strlcpy(char *dest, const char *src, unsigned long size);
char* MJR_itoa(int value, char* result, int base);

#ifndef USE_KLEE

// Need to include <string.h> in order to use these then

#define MJR_memset(s, c, count) memset(s, c, count)
#define MJR_memcpy(dest, src, count) memcpy(dest, src, count)
#define MJR_strtok(s, delim) strtok(s, delim)
#define MJR_strcpy(dest, src) strcpy(dest, src)
#define MJR_strcmp(cs, ct) strcmp(cs, ct)
#define MJR_strncmp(cs, ct, count) strncmp(cs, ct, count)
#define MJR_strlen(s) strlen(s)
#define MJR_strcat(dest, src) strcat(dest, src)

#else

void *MJR_memset(void *s, int c, unsigned long count);
void *MJR_memcpy(void *dest, const void *src, unsigned long count);
char *MJR_strtok(char *s, const char *delim);
char *MJR_strcpy(char *dest, const char *src);
int MJR_strcmp(const char *cs, const char *ct);
int MJR_strncmp(const char *cs, const char *ct, unsigned long count);
unsigned long MJR_strlen(const char *s);
char *MJR_strcat(char *dest, const char *src);

#endif

#endif
