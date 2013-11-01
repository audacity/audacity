#include "stdarg.h"
#include "stdio.h"

#ifdef __linux__
#define _vsnprintf vsnprintf
#elif defined(__MACH__)
#define _vsnprintf vsnprintf
#else
#include "crtdbg.h"
#endif

void trace(char *format, ...)
{
    char msg[256];
    va_list args;
    va_start(args, format);
    _vsnprintf(msg, 256, format, args);
    va_end(args);

#if defined(_DEBUG) && !defined(__linux__)
    _CrtDbgReport(_CRT_WARN, NULL, NULL, NULL, msg);
#else
    printf(msg);
#endif
}
