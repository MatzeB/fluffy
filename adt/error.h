// placeholder file...

void abort(void);

static inline __attribute__((noreturn))
void panic(const char *msg)
{ abort(); }
