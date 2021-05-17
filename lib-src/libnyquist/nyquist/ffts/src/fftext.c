/*******************************************************************
	This file extends the fftlib with calls to maintain the cosine and bit reversed tables
	for you (including mallocs and free's).  Call the init routine for each fft size you will
	be using.  Then you can call the fft routines below which will make the fftlib library 
	call with the appropriate tables passed.  When you are done with all fft's you can call 
	fftfree to release the storage for the tables.  Note that you can call fftinit repeatedly
	with the same size, the extra calls will be ignored. So, you could make a macro to call
	fftInit every time you call ffts. For example you could have someting like:
	#define FFT(a,n) if(!fftInit(roundtol(LOG2(n)))) ffts(a,roundtol(LOG2(n)),1);else printf("fft error\n");
*******************************************************************/
#include <stdint.h>
#include <stdlib.h>
#include "fftlib.h"
#include "matlib.h"
#include "fftext.h"

// pointers to storage of Utbl's and  BRLow's
static float *UtblArray[8*sizeof(long)] = {0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
									0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0};
static short *BRLowArray[8*sizeof(long)/2]  = {0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0};

int fftInit(long M){
// malloc and init cosine and bit reversed tables for a given size fft, ifft, rfft, rifft
/* INPUTS */
/* M = log2 of fft size	(ex M=10 for 1024 point fft) */
/* OUTPUTS */
/* private cosine and bit reversed tables	*/

int theError = 1;
/*** I did NOT test cases with M>27 ***/
if ((M >= 0) && (M < 8*sizeof(long))){
	theError = 0;
	if (UtblArray[M] == 0){	// have we not inited this size fft yet?
		// init cos table
		UtblArray[M] = (float *) malloc( (POW2(M)/4+1)*sizeof(float) );
		if (UtblArray[M] == 0)
			theError = 2;
		else{
			fftCosInit(M, UtblArray[M]);
		}
		if (M > 1){
			if (BRLowArray[M/2] == 0){	// init bit reversed table for cmplx fft
				// coercion avoids compiler warning about 32-bit shift:
				BRLowArray[M/2] = (short *) malloc( (int64_t) POW2(M/2-1)*sizeof(short) );
				if (BRLowArray[M/2] == 0)
					theError = 2;
				else{
					fftBRInit(M, BRLowArray[M/2]);
				}
			}
		}
		if (M > 2){
			if (BRLowArray[(M-1)/2] == 0){	// init bit reversed table for real fft
				// coercion avoids compiler warning about 32-bit shift:
				BRLowArray[(M-1)/2] = (short *) malloc( (int64_t) POW2((M-1)/2-1)*sizeof(short) );
				if (BRLowArray[(M-1)/2] == 0)
					theError = 2;
				else{
					fftBRInit(M-1, BRLowArray[(M-1)/2]);
				}
			}
		}
	}
};
return theError;
}

void fftFree(){
// release storage for all private cosine and bit reversed tables
long i1;
for (i1=8*sizeof(long)/2-1; i1>=0; i1--){
	if (BRLowArray[i1] != 0){
		free(BRLowArray[i1]);
		BRLowArray[i1] = 0;
	};
};
for (i1=8*sizeof(long)-1; i1>=0; i1--){
	if (UtblArray[i1] != 0){
		free(UtblArray[i1]);
		UtblArray[i1] = 0;
	};
};
}

/*************************************************
 The following calls are easier than calling to fftlib directly.
 Just make sure fftlib has been called for each M first.
**************************************************/

void ffts(float *data, long M, long Rows){
/* Compute in-place complex fft on the rows of the input array	*/
/* INPUTS */
/* *ioptr = input data array	*/
/* M = log2 of fft size	(ex M=10 for 1024 point fft) */
/* Rows = number of rows in ioptr array (use 1 for Rows for a single fft)	*/
/* OUTPUTS */
/* *ioptr = output data array	*/
	ffts1(data, M, Rows, UtblArray[M], BRLowArray[M/2]);
}

void iffts(float *data, long M, long Rows){
/* Compute in-place inverse complex fft on the rows of the input array	*/
/* INPUTS */
/* *ioptr = input data array	*/
/* M = log2 of fft size	(ex M=10 for 1024 point fft) */
/* Rows = number of rows in ioptr array (use 1 for Rows for a single fft)	*/
/* OUTPUTS */
/* *ioptr = output data array	*/
	iffts1(data, M, Rows, UtblArray[M], BRLowArray[M/2]);
}

void rffts(float *data, long M, long Rows){
/* Compute in-place real fft on the rows of the input array	*/
/* The result is the complex spectra of the positive frequencies */
/* except the location for the first complex number contains the real */
/* values for DC and Nyquest */
/* See rspectprod for multiplying two of these spectra together- ex. for fast convolution */
/* INPUTS */
/* *ioptr = real input data array	*/
/* M = log2 of fft size	(ex M=10 for 1024 point fft) */
/* Rows = number of rows in ioptr array (use 1 for Rows for a single fft)	*/
/* OUTPUTS */
/* *ioptr = output data array	in the following order */
/* Re(x[0]), Re(x[N/2]), Re(x[1]), Im(x[1]), Re(x[2]), Im(x[2]), ... Re(x[N/2-1]), Im(x[N/2-1]). */
	rffts1(data, M, Rows, UtblArray[M], BRLowArray[(M-1)/2]);
}

void riffts(float *data, long M, long Rows){
/* Compute in-place real ifft on the rows of the input array	*/
/* data order as from rffts */
/* INPUTS */
/* *ioptr = input data array in the following order	*/
/* M = log2 of fft size	(ex M=10 for 1024 point fft) */
/* Re(x[0]), Re(x[N/2]), Re(x[1]), Im(x[1]), Re(x[2]), Im(x[2]), ... Re(x[N/2-1]), Im(x[N/2-1]). */
/* Rows = number of rows in ioptr array (use 1 for Rows for a single fft)	*/
/* OUTPUTS */
/* *ioptr = real output data array	*/
	riffts1(data, M, Rows, UtblArray[M], BRLowArray[(M-1)/2]);
}

void rspectprod(float *data1, float *data2, float *outdata, long N){
// When multiplying a pair of spectra from rfft care must be taken to multiply the
// two real values seperately from the complex ones. This routine does it correctly.
// the result can be stored in-place over one of the inputs
/* INPUTS */
/* *data1 = input data array	first spectra */
/* *data2 = input data array	second spectra */
/* N = fft input size for both data1 and data2 */
/* OUTPUTS */
/* *outdata = output data array spectra */
if(N>1){
	outdata[0] = data1[0] * data2[0];	// multiply the zero freq values
	outdata[1] = data1[1] * data2[1];	// multiply the nyquest freq values
	cvprod(data1 + 2, data2 + 2, outdata + 2, N/2-1);	// multiply the other positive freq values
}
else{
	outdata[0] = data1[0] * data2[0];
}
}
