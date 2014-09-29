// trace.cpp -- debugging print function
//
// (I think this was created to provide a generic print function
// for use in non-command-line Windows applications where printf
// does not work. Currently, it is not used, but kept around for
// possible debugging needs. -RBD)

#include "stdarg.h"
#include "stdio.h"
#include "crtdbg.h"


void trace(char *format, ...)
{
    char msg[256];
    va_list args;
    va_start(args, format);
    _vsnprintf_s(msg, 256, _TRUNCATE, format, args);
    va_end(args);
#ifdef _DEBUG
    _CrtDbgReport(_CRT_WARN, NULL, NULL, NULL, msg);
#else
    printf(msg);
#endif
}
