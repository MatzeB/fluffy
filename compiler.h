#ifndef COMPILER_H
#define COMPILER_H

#if defined __GNUC__ && __GNUC__ >= 4
#define WARN_UNUSED	        __attribute__((warn_unused_result))
#else
#define WARN_UNUSED
#endif

#endif
