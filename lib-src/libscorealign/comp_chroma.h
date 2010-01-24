#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>
#include	<math.h>

#define SILENCE_THRESHOLD 0.001
#define SILENCE_DISTANCE 16.0

/*				NORM_CHROMA
 *
 * This function normalizes the chroma for each frame of the
 * chrom_energy to mean 0 and std. dev. 1.
 */
void norm_chroma( int len, float *chrom_energy );

/*				GEN_DIST
 *
 * This function generates the Euclidean distance for points i
 * and j in two chroma vectors for use with dynamic time warping of 
 * the chroma vectors.
 */
float gen_dist(int i, int j, float *chrom_energy1, 
		       float *chrom_energy2 );
