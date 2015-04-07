#include "xlisp.h"
#include "xlispfns.h"


void run_xlisp()
{
    xlisp_main_init(0,NULL);
    xlisp_main();
    /* clean up */
    xlisp_wrapup();
}
