extern func memcpy(dest : void*, src : void*, size : size_t) : void*
extern func memmove(dest : void*, src : void*, size : size_t) : void*
extern func memcmp(s1 : void*, s2 : void*) : int
extern func memset(s : void*, c : int, size : size_t) : void*

extern func strcpy(dest : String, src : String) : String
extern func strncpy(dest : String, src : String, n : size_t) : String
extern func strcat(s1 : String, s2 : String) : String
extern func strncat(s1 : String, s2 : String, n : size_t) : String
extern func strcmp(s1 : String, s2 : String) : int
extern func strncmp(s1 : String, s2 : String, n : size_t) : int
extern func strlen(s : String) : size_t

extern func strerror(errnum : int) : String

