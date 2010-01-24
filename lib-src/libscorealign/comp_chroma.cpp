
#include <fstream>
#include "allegro.h"
#include "audioreader.h"
#include "gen_chroma.h"
#include  "comp_chroma.h"

using namespace std;

/*				NORM_CHROMA
 *
 * This function normalizes the chroma for each frame of the
 * chrom_energy to mean 0 and std. dev. 1. But if this is a
 * "silent frame", set the 13th element to 1.
 */
void norm_chroma( int len, float *chrom_energy ) {

  float avg = 0;
  float dev = 0;
  float sum = 0;

  for( int i = 0; i < len; i++ ) {

    /* Calculate avg for this frame */
    sum = 0;
    for ( int j = 0; j < 12; j++ )
        sum += AREF2(chrom_energy, i, j);
    avg = sum / 12.0;

	/* Silence detection: */ 
	float silence = 0.0F;
	if (avg < SILENCE_THRESHOLD) { /* assume silent */
		silence = 1.0F;
	}
    AREF2(chrom_energy, i, 12) = silence;
	
	// printf("avg at %g: %g\n", i * 0.25, avg);

    /* Normalize this frame to avg. 0 */
    for ( int j = 0; j < 12; j++ )
        AREF2(chrom_energy, i, j) -= avg;

    /* Calculate std. dev. for this frame */
    sum = 0;
    for ( int j = 0; j < 12; j++ ) {
        float x = AREF2(chrom_energy, i, j);
        sum += x * x;
    }
    dev = sqrt( sum / 12.0 );
	if (dev == 0.0) dev = 1.0F; /* don't divide by zero */

    /* Normalize this frame to std. dev. 1*/
    for ( int j = 0; j < 12; j++ )
        AREF2(chrom_energy, i, j) /= dev;
  }
}

/* Returns the minimum of two values */
double min2( double x, double y ) {
    return (x < y ? x : y);
}

/*				GEN_DIST
 *
 * This function generates the Euclidean distance for points i
 * and j in two chroma vectors for use with dynamic time warping of 
 * the chroma vectors.
 */
float gen_dist( int i, int j, float *chrom_energy1, 
	       float *chrom_energy2 ) {

  float sum = 0;
  float MAX = 12.0;

  if (AREF2(chrom_energy1, i, CHROMA_BIN_COUNT) !=
	  AREF2(chrom_energy2, j, CHROMA_BIN_COUNT)) {
      //printf("gd%g ", SILENCE_DISTANCE); // print result
      return SILENCE_DISTANCE;
  }
  /* Determine the distance between these vectors 
     chroma1[i] and chroma2[j] to return */
  for (int k = 0; k < 12; k++) {
      float x = AREF2(chrom_energy1, i, k);
      float y = AREF2(chrom_energy2, j, k);
      float diff = x - y;

      sum += diff*diff ;
  }
  sum = min2( sqrt( sum ), MAX );
  //printf("gd%g ", sum); // print the result
  return sum;
}
