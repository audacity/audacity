/* sndsystem.h -- system-specific definitions */
/* 
  NOTES: you need a different sndswitches.h for each implementation, so
this is a separate file.  Things you need to define here:

1) Either UNIX, WIN32, or MACINTOSH should be defined.

2) Either the following function declaration:
        void snd_fail(char *msg);
or
        #define snd_fail(msg) ...

3) typedef FASTFLOAT to be either a double or a float, whichever
computes faster (PowerPCs are faster at double math than float math)

4) typedef MEMFLOAT to be how you would store a sample in memory
(this should normally be float)

5) min() must be defined (either a macro or a function)

6) max() must be defined (either a macro or a function)

*/

#define UNIX

#define snd_fail(msg) xlfail(msg)
#define snd_warn(msg) errputstr(msg)
typedef double FASTFLOAT;
typedef float MEMFLOAT;

/* avoid conflict with Windows */
#ifndef max

/* min(n, sizeof(long)) doesn't work on RS6K without this: 
 * (I never tracked down what min() was called and what was wrong.)
 */
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#endif
