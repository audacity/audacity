#define MYRECIPLN2	1.442695040888963407359924681001892137426	// 1.0/log(2)

/* some useful conversions between a number and its power of 2 */
#define LOG2(a)	(MYRECIPLN2*log(a))	// floating point logarithm base 2
#define POW2(m) ((unsigned long) 1 << (m))	// integer power of 2 for m<32

/*******************************************************************
lower level fft stuff called by routines in fftext.c and fft2d.c
*******************************************************************/

void fftCosInit(long M, float *Utbl);
/* Compute Utbl, the cosine table for ffts	*/
/* of size (pow(2,M)/4 +1)	*/
/* INPUTS */
/* M = log2 of fft size	*/
/* OUTPUTS */
/* *Utbl = cosine table	*/

void fftBRInit(long M, short *BRLow);
/* Compute BRLow, the bit reversed table for ffts	*/
/* of size pow(2,M/2 -1)	*/
/* INPUTS */
/* M = log2 of fft size	*/
/* OUTPUTS */
/* *BRLow = bit reversed counter table	*/

void ffts1(float *ioptr, long M, long Rows, float *Utbl, short *BRLow);
/* Compute in-place complex fft on the rows of the input array	*/
/* INPUTS */
/* *ioptr = input data array	*/
/* M = log2 of fft size	(ex M=10 for 1024 point fft) */
/* Rows = number of rows in ioptr array (use Rows of 1 if ioptr is a 1 dimensional array)	*/
/* *Utbl = cosine table	*/
/* *BRLow = bit reversed counter table	*/
/* OUTPUTS */
/* *ioptr = output data array	*/

void iffts1(float *ioptr, long M, long Rows, float *Utbl, short *BRLow);
/* Compute in-place inverse complex fft on the rows of the input array	*/
/* INPUTS */
/* *ioptr = input data array	*/
/* M = log2 of fft size	*/
/* Rows = number of rows in ioptr array (use Rows of 1 if ioptr is a 1 dimensional array)	*/
/* *Utbl = cosine table	*/
/* *BRLow = bit reversed counter table	*/
/* OUTPUTS */
/* *ioptr = output data array	*/

void rffts1(float *ioptr, long M, long Rows, float *Utbl, short *BRLow);
/* Compute in-place real fft on the rows of the input array	*/
/* The result is the complex spectra of the positive frequencies */
/* except the location for the first complex number contains the real */
/* values for DC and Nyquest */
/* INPUTS */
/* *ioptr = real input data array	*/
/* M = log2 of fft size	*/
/* Rows = number of rows in ioptr array (use Rows of 1 if ioptr is a 1 dimensional array)	*/
/* *Utbl = cosine table	*/
/* *BRLow = bit reversed counter table	*/
/* OUTPUTS */
/* *ioptr = output data array	in the following order */
/* Re(x[0]), Re(x[N/2]), Re(x[1]), Im(x[1]), Re(x[2]), Im(x[2]), ... Re(x[N/2-1]), Im(x[N/2-1]). */


void riffts1(float *ioptr, long M, long Rows, float *Utbl, short *BRLow);
/* Compute in-place real ifft on the rows of the input array	*/
/* data order as from rffts1 */
/* INPUTS */
/* *ioptr = input data array in the following order	*/
/* M = log2 of fft size	*/
/* Re(x[0]), Re(x[N/2]), Re(x[1]), Im(x[1]), Re(x[2]), Im(x[2]), ... Re(x[N/2-1]), Im(x[N/2-1]). */
/* Rows = number of rows in ioptr array (use Rows of 1 if ioptr is a 1 dimensional array)	*/
/* *Utbl = cosine table	*/
/* *BRLow = bit reversed counter table	*/
/* OUTPUTS */
/* *ioptr = real output data array	*/
