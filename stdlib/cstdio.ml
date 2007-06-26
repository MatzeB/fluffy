typealias FILE   <- void

extern var stdout : FILE*
extern var stderr : FILE*
extern var stdin  : FILE*

extern func fputc(c : int, stream : FILE* ) : int
extern func fputs(s : String, stream : FILE* ) : int
extern func fopen(path : String, mode : String) : FILE*
extern func fclose(stream : FILE* ) : int
extern func fflush(stream : FILE* ) : int
extern func fread(ptr : void*, size : size_t, nmemb : size_t, stream : FILE*) : size_t
extern func fwrite(ptr : void*, size : size_t, nmemb : size_t, stream : FILE*) : size_t
extern func ungetc(c : int, stream : FILE*) : int

extern func puts(s : String) : int
extern func putchar(c : int) : int
