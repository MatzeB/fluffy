extern func calloc(nmemb : size_t, size : size_t) : void*
extern func malloc(size : size_t) : void*
extern func realloc(ptr : void*, size : size_t) : void*
extern func free(ptr : void*)

extern func abort()
extern func atexit(function : (func() : void)* )
extern func exit(status : int)

extern func getenc(name : String) : String
extern func system(string : String) : int
extern func abs(j : int) : int
extern func llabs(k : long) : long

extern func atof(nptr : String) : double
extern func atoi(nptr : String) : int
extern func atoll(nptr : String) : long

extern func rand() : int
extern func srand(seed : unsigned int)

