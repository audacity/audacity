/* stk path initialization */

#include "stdlib.h"
#include "string.h"
// #include "instr.h"
#include "Stk.h"
#include "stkinit.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "xlisp.h"

#ifdef __cplusplus
}
#endif

using namespace Nyq;

const char *rawwave_path = NULL;

extern "C" void stk_init()
{
    /* wherever the sinewave.raw file is, that will become 
     * the rawwave_path for STK 
     */
    char filename[32];
    strcpy(filename, "rawwaves");
    filename[8] = os_pathchar;
    filename[9] = '\0';
    strcat(filename, "sinewave.raw");
    /* find_in_xlisp_path returns const char *, but we're going to
     * alter it to get just the path, so we have to coerce out the
     * const attribute
     */
    char *path = (char *) find_in_xlisp_path(filename);
    if (!path) {
        errputstr("\nERROR: Could not find sinewave.raw in rawwaves. Something is wrong with the installation or configuration.\n\n");
        rawwave_path = "";
        return;
    }
    /* remove sinewave.raw to get just the path */
    path[strlen(path) - 12] = '\0'; 
    rawwave_path = strcpy((char *) malloc(strlen(path) + 1), path); /* keep a copy */
    /* note: rawwave_path is allocated but never freed */
   Stk::setRawwavePath(path); // PJM
}


