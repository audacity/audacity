#ifndef __TWOLAME_WINUTIL_H
#define __TWOLAME_WINUTIL_H

/*
 * Don't define this as a function to avoid conflicts
 * with other libraries which already define this.
 */
#define lrintf(x) ((int)((x)+0.5))

#endif
