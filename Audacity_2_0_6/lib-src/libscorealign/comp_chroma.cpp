/* comp_chroma.cpp -- compute chroma distance
 *
 * 2008 RBD
 */

#include <string.h>
#include <math.h>
#include <fstream>
#include <algorithm>
#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"
#include "gen_chroma.h"
#include  "comp_chroma.h"
using namespace std;

#ifdef min
#undef min
#endif
#define min(x,y) ((x)<(y)?(x):(y))

#define SILENCE_DISTANCE 16.0

/*				GEN_DIST
 *
 * This function generates the Euclidean distance for points i
 * and j in two chroma vectors for use with dynamic time warping of 
 * the chroma vectors.
 */
float Scorealign::gen_dist(int i, int j) 
{
    const float MAX = 12.0;
    assert(i < file0_frames);
    assert(j < file1_frames);
    float *cv0 = AREF1(chrom_energy0, i);
    float *cv1 = AREF1(chrom_energy1, j);
    if (cv0[CHROMA_BIN_COUNT] != cv1[CHROMA_BIN_COUNT]) {
        // silent frames are a (large) constant distance from non-silent frames
        return SILENCE_DISTANCE;
    }
    /* calculate the Euclidean distance between these vectors */
    float sum = 0;
    for (int k = 0; k < CHROMA_BIN_COUNT; k++) {
        float diff = cv0[k] - cv1[k];
        sum += diff * diff ;
    }
    // place a ceiling (MAX) on distance
    return min(sqrt(sum), MAX);
}
