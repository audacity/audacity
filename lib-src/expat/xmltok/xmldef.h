/*
Copyright (c) 1998, 1999 Thai Open Source Software Center Ltd
See the file copying.txt for copying permission.
*/

#include <string.h>

#ifdef XML_WINLIB

#define WIN32_LEAN_AND_MEAN
#define STRICT
#include <windows.h>

#define malloc(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define calloc(x, y) HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, (x)*(y))
#define free(x) HeapFree(GetProcessHeap(), 0, (x))
#define realloc(x, y) HeapReAlloc(GetProcessHeap(), 0, x, y)
#define abort() /* as nothing */

#else /* not XML_WINLIB */

#include <stdlib.h>

#endif /* not XML_WINLIB */

/* This file can be used for any definitions needed in
particular environments. */

/* Mozilla specific defines */

#ifdef MOZILLA_CLIENT

#include "nspr.h"
#define malloc(x) PR_Malloc((size_t)(x))
#define realloc(x, y) PR_Realloc((x), (size_t)(y))
#define calloc(x, y) PR_Calloc((x),(y))
#define free(x) PR_Free(x)
#if PR_BYTES_PER_INT != 4
#define int int32
#endif

/* Enable Unicode string processing in expat. */
#ifndef XML_UNICODE
#define XML_UNICODE
#endif

/* Enable external parameter entity parsing in expat */
#ifndef XML_DTD
#define XML_DTD 1
#endif

#endif /* MOZILLA_CLIENT */
