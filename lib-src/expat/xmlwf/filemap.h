/* Copyright (c) 1998, 1999 Thai Open Source Software Center Ltd
   See the file COPYING for copying permission.
*/

#include <limits.h>  /* INT_MAX */
#include <stddef.h>


/* The following limit (for XML_Parse's int len) derives from
 * this loop in xmparse.c:
 *
 *    do {
 *      bufferSize = (int) (2U * (unsigned) bufferSize);
 *    } while (bufferSize < neededSize && bufferSize > 0);
 */
#define XML_MAX_CHUNK_LEN  (INT_MAX / 2 + 1)


#ifdef XML_UNICODE
int filemap(const wchar_t *name,
            void (*processor)(const void *, size_t,
                              const wchar_t *, void *arg),
            void *arg);
#else
int filemap(const char *name,
            void (*processor)(const void *, size_t,
                              const char *, void *arg),
            void *arg);
#endif
