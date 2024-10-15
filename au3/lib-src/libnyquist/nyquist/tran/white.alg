(WHITE-ALG
(NAME "white")
(ARGUMENTS ("time_type" "t0") ("rate_type" "sr") ("time_type" "d"))
(STATE )
(TERMINATE (AFTER "d"))
(INNER-LOOP "output = (sample_type) (rand() * rand_scale - 1.0)")
(SAMPLE-RATE "sr")
(SUPPORT-HEADER "
/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03 rbd all systems now use rand(), based on DM's modifications
 */

#include <stdlib.h>
#include <math.h>

/* rand returns from 0 to RAND_MAX. Scale and offset
 * to get range from -1 to +1
 */
#define rand_scale (2.0/RAND_MAX)

")
)

