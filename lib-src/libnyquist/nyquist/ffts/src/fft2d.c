/*******************************************************************
	This file extends the fftlib with 2d and 3d complex fft's and
	2d real fft's.  All fft's return results in-place.  Temporary buffers
	for transposing columns are maintained privately via calls to
	fft2dInit, fft2dFree, fft3dInit, and fft3dFree.
	Note that you can call fft2dInit and fft3dInit repeatedly
	with the same sizes, the extra calls will be ignored.
	So, you could make a macro to call fft2dInit every time you call fft2d.
	*** Warning *** fft2dFree and fft3dFree also call fftFree
	so you must re-init all 1d fft sizes you are going to continue using
*******************************************************************/
#include <stdlib.h>
#include <fp.h>
#include "fftlib.h"
#include "fftext.h"
#include "matlib.h"
#include "dxpose.h"
#include "fft2d.h"
	// use trick of using a real double transpose in place of a complex transpose if it fits
#define cxpose(a,b,c,d,e,f) (2*sizeof(float)==sizeof(xdouble)) ? dxpose((xdouble *)(a), b, (xdouble *)(c), d, e, f) : cxpose(a,b,c,d,e,f);
	// for this trick to work you must NOT replace the xdouble declarations in
	// dxpose with float declarations.


	// pointers for temporary storage for four columns
static float *Array2d[8*sizeof(long)] = {0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
									0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0};
int fft2dInit(long M2, long M){
	// init for fft2d, ifft2d, rfft2d, and rifft2d
	// malloc storage for 4 columns of 2d ffts then call fftinit for both row and column ffts sizes
/* INPUTS */
/* M = log2 of number of columns */
/* M2 = log2 of number of rows */
/*       of 2d matrix to be fourier transformed */
/* OUTPUTS */
/* private storage for columns of 2d ffts	*/
/* calls fftInit for cosine and bit reversed tables	*/
int theError = 1;
if ((M2 >= 0) && (M2 < 8*sizeof(long))){
	theError = 0;
	if (Array2d[M2] == 0){
		Array2d[M2] = (float *) malloc( 4*2*POW2(M2)*sizeof(float) );
		if (Array2d[M2] == 0)
			theError = 2;
		else{
			theError = fftInit(M2);
		}
	}
	if (theError == 0)
		theError = fftInit(M);
}
return theError;
}

void fft2dFree(){
// free storage for columns of 2d ffts and call fftFree to free all BRLow and Utbl storage
long i1;
for (i1=8*sizeof(long)-1; i1>=0; i1--){
	if (Array2d[i1] != 0){
		free(Array2d[i1]);
		Array2d[i1] = 0;
	};
};
fftFree();
}

void fft2d(float *data, long M2, long M){
/* Compute 2D complex fft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/
long i1;
if((M2>0)&&(M>0)){
	ffts(data, M, POW2(M2));
	if (M>2)
		for (i1=0; i1<POW2(M); i1+=4){
			cxpose(data + i1*2, POW2(M), Array2d[M2], POW2(M2), POW2(M2), 4);
			ffts(Array2d[M2], M2, 4);
			cxpose(Array2d[M2], POW2(M2), data + i1*2, POW2(M), 4, POW2(M2));
		}
	else{
		cxpose(data, POW2(M), Array2d[M2], POW2(M2), POW2(M2), POW2(M));
		ffts(Array2d[M2], M2, POW2(M));
		cxpose(Array2d[M2], POW2(M2), data, POW2(M), POW2(M), POW2(M2));
	}
}
else
	ffts(data, M2+M, 1);
}

void ifft2d(float *data, long M2, long M){
/* Compute 2D complex ifft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/
long i1;
if((M2>0)&&(M>0)){
	iffts(data, M, POW2(M2));
	if (M>2)
		for (i1=0; i1<POW2(M); i1+=4){
			cxpose(data + i1*2, POW2(M), Array2d[M2], POW2(M2), POW2(M2), 4);
			iffts(Array2d[M2], M2, 4);
			cxpose(Array2d[M2], POW2(M2), data + i1*2, POW2(M), 4, POW2(M2));
		}
	else{
		cxpose(data, POW2(M), Array2d[M2], POW2(M2), POW2(M2), POW2(M));
		iffts(Array2d[M2], M2, POW2(M));
		cxpose(Array2d[M2], POW2(M2), data, POW2(M), POW2(M), POW2(M2));
	}
}
else
	iffts(data, M2+M, 1);
}

int fft3dInit(long L, long M2, long M){
	// init for fft3d, ifft3d
	// malloc storage for 4 columns and 4 pages of 3d ffts
	// then call fftinit for page, row and column ffts sizes
//* M = log2 of number of columns */
/* M2 = log2 of number of rows */
/* L = log2 of number of pages */
/*       of 3d matrix to be fourier transformed */
/* OUTPUTS */
/* private storage for columns and pages of 3d ffts	*/
/* calls fftInit for cosine and bit reversed tables	*/
int theError = 1;
if ((L >= 0) && (L < 8*sizeof(long))){
	theError = 0;
	if (Array2d[L] == 0){
		Array2d[L] = (float *) malloc( 4*2*POW2(L)*sizeof(float) );
		if (Array2d[L] == 0)
			theError = 2;
		else{
			theError = fftInit(L);
		}
	}
	if (theError == 0){
		if (Array2d[M2] == 0){
			Array2d[M2] = (float *) malloc( 4*2*POW2(M2)*sizeof(float) );
			if (Array2d[M2] == 0)
				theError = 2;
			else{
				theError = fftInit(M2);
			}
		}
	}
	if (theError == 0)
		theError = fftInit(M);
}
return theError;
}

void fft3dFree(){
// free storage for columns of all 2d&3d ffts and call fftFree to free all BRLow and Utbl storage
fft2dFree();
}

void fft3d(float *data, long M3, long M2, long M){
/* Compute 2D complex fft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M3 = log2 of fft size number of pages */
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/
long i1;
long i2;
const long N = POW2(M);
const long N2 = POW2(M2);
const long N3 = POW2(M3);
if((M3>0)&&(M2>0)&&(M>0)){
	ffts(data, M, N3*N2);
	if (M>2)
		for (i2=0; i2<N3; i2++){
			for (i1=0; i1<N; i1+=4){
				cxpose(data + i2*2*POW2(M2+M) + i1*2, N, Array2d[M2], N2, N2, 4);
				ffts(Array2d[M2], M2, 4);
				cxpose(Array2d[M2], N2, data + i2*2*POW2(M2+M) + i1*2, N, 4, N2);
			}
		}
	else{
		for (i2=0; i2<N3; i2++){
			cxpose(data + i2*2*POW2(M2+M), N, Array2d[M2], N2, N2, N);
			ffts(Array2d[M2], M2, N);
			cxpose(Array2d[M2], N2, data + i2*2*POW2(M2+M), N, N, N2);
		}
	}
	if ((M2+M)>2)
		for (i1=0; i1<POW2(M2+M); i1+=4){
			cxpose(data + i1*2, POW2(M2+M), Array2d[M3], N3, N3, 4);
			ffts(Array2d[M3], M3, 4);
			cxpose(Array2d[M3], N3, data + i1*2, POW2(M2+M), 4, N3);
		}
	else{
		cxpose(data, POW2(M2+M), Array2d[M3], N3, N3, POW2(M2+M));
		ffts(Array2d[M3], M3, POW2(M2+M));
		cxpose(Array2d[M3], N3, data, POW2(M2+M), POW2(M2+M), N3);
	}
}
else
	if(M3==0) fft2d(data, M2, M);
	else
		if(M2==0) fft2d(data, M3, M);
		else
			if(M==0) fft2d(data, M3, M2);
}

void ifft3d(float *data, long M3, long M2, long M){
/* Compute 2D complex ifft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M3 = log2 of fft size number of pages */
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/
long i1;
long i2;
const long N = POW2(M);
const long N2 = POW2(M2);
const long N3 = POW2(M3);
if((M3>0)&&(M2>0)&&(M>0)){
	iffts(data, M, N3*N2);
	if (M>2)
		for (i2=0; i2<N3; i2++){
			for (i1=0; i1<N; i1+=4){
				cxpose(data + i2*2*POW2(M2+M) + i1*2, N, Array2d[M2], N2, N2, 4);
				iffts(Array2d[M2], M2, 4);
				cxpose(Array2d[M2], N2, data + i2*2*POW2(M2+M) + i1*2, N, 4, N2);
			}
		}
	else{
		for (i2=0; i2<N3; i2++){
			cxpose(data + i2*2*POW2(M2+M), N, Array2d[M2], N2, N2, N);
			iffts(Array2d[M2], M2, N);
			cxpose(Array2d[M2], N2, data + i2*2*POW2(M2+M), N, N, N2);
		}
	}
	if ((M2+M)>2)
		for (i1=0; i1<POW2(M2+M); i1+=4){
			cxpose(data + i1*2, POW2(M2+M), Array2d[M3], N3, N3, 4);
			iffts(Array2d[M3], M3, 4);
			cxpose(Array2d[M3], N3, data + i1*2, POW2(M2+M), 4, N3);
		}
	else{
		cxpose(data, POW2(M2+M), Array2d[M3], N3, N3, POW2(M2+M));
		iffts(Array2d[M3], M3, POW2(M2+M));
		cxpose(Array2d[M3], N3, data, POW2(M2+M), POW2(M2+M), N3);
	}
}
else
	if(M3==0) ifft2d(data, M2, M);
	else
		if(M2==0) ifft2d(data, M3, M);
		else
			if(M==0) ifft2d(data, M3, M2);
}

void rfft2d(float *data, long M2, long M){
/* Compute 2D real fft and return results in-place	*/
/* First performs real fft on rows using size from M to compute positive frequencies */
/* then performs transform on columns using size from M2 to compute wavenumbers */
/* If you think of the result as a complex pow(2,M2) by pow(2,M-1) matrix */
/* then the first column contains the positive wavenumber spectra of DC frequency */
/* followed by the positive wavenumber spectra of the nyquest frequency */
/* since these are two positive wavenumber spectra the first complex value */
/* of each is really the real values for the zero and nyquest wavenumber packed together */
/* All other columns contain the positive and negative wavenumber spectra of a positive frequency */
/* See rspect2dprod for multiplying two of these spectra together- ex. for fast convolution */
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows in */
/* M = log2 of fft size number of columns in */
/* OUTPUTS */
/* *data = output data array	*/
long i1;
if((M2>0)&&(M>0)){
	rffts(data, M, POW2(M2));
	if (M==1){
		cxpose(data, POW2(M)/2, Array2d[M2]+POW2(M2)*2, POW2(M2), POW2(M2), 1);
		xpose(Array2d[M2]+POW2(M2)*2, 2, Array2d[M2], POW2(M2), POW2(M2), 2);
		rffts(Array2d[M2], M2, 2);
		cxpose(Array2d[M2], POW2(M2), data, POW2(M)/2, 1, POW2(M2));
	}
	else if (M==2){
		cxpose(data, POW2(M)/2, Array2d[M2]+POW2(M2)*2, POW2(M2), POW2(M2), 1);
		xpose(Array2d[M2]+POW2(M2)*2, 2, Array2d[M2], POW2(M2), POW2(M2), 2);
		rffts(Array2d[M2], M2, 2);
		cxpose(Array2d[M2], POW2(M2), data, POW2(M)/2, 1, POW2(M2));

		cxpose(data + 2, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 1);
		ffts(Array2d[M2], M2, 1);
		cxpose(Array2d[M2], POW2(M2), data + 2, POW2(M)/2, 1, POW2(M2));
	}
	else{
		cxpose(data, POW2(M)/2, Array2d[M2]+POW2(M2)*2, POW2(M2), POW2(M2), 1);
		xpose(Array2d[M2]+POW2(M2)*2, 2, Array2d[M2], POW2(M2), POW2(M2), 2);
		rffts(Array2d[M2], M2, 2);
		cxpose(Array2d[M2], POW2(M2), data, POW2(M)/2, 1, POW2(M2));

		cxpose(data + 2, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 3);
		ffts(Array2d[M2], M2, 3);
		cxpose(Array2d[M2], POW2(M2), data + 2, POW2(M)/2, 3, POW2(M2));
		for (i1=4; i1<POW2(M)/2; i1+=4){
			cxpose(data + i1*2, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 4);
			ffts(Array2d[M2], M2, 4);
			cxpose(Array2d[M2], POW2(M2), data + i1*2, POW2(M)/2, 4, POW2(M2));
		}
	}
}
else
	rffts(data, M2+M, 1);
}

void rifft2d(float *data, long M2, long M){
/* Compute 2D real ifft and return results in-place	*/
/* The input must be in the order as outout from rfft2d */
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows out */
/* M = log2 of fft size number of columns out */
/* OUTPUTS */
/* *data = output data array	*/
long i1;
if((M2>0)&&(M>0)){
	if (M==1){
		cxpose(data, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 1);
		riffts(Array2d[M2], M2, 2);
		xpose(Array2d[M2], POW2(M2), Array2d[M2]+POW2(M2)*2, 2, 2, POW2(M2));
		cxpose(Array2d[M2]+POW2(M2)*2, POW2(M2), data, POW2(M)/2, 1, POW2(M2));
	}
	else if (M==2){
		cxpose(data, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 1);
		riffts(Array2d[M2], M2, 2);
		xpose(Array2d[M2], POW2(M2), Array2d[M2]+POW2(M2)*2, 2, 2, POW2(M2)); 
		cxpose(Array2d[M2]+POW2(M2)*2, POW2(M2), data, POW2(M)/2, 1, POW2(M2));

		cxpose(data + 2, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 1);
		iffts(Array2d[M2], M2, 1);
		cxpose(Array2d[M2], POW2(M2), data + 2, POW2(M)/2, 1, POW2(M2));
	}
	else{
		cxpose(data, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 1);
		riffts(Array2d[M2], M2, 2);
		xpose(Array2d[M2], POW2(M2), Array2d[M2]+POW2(M2)*2, 2, 2, POW2(M2));
		cxpose(Array2d[M2]+POW2(M2)*2, POW2(M2), data, POW2(M)/2, 1, POW2(M2));

		cxpose(data + 2, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 3);
		iffts(Array2d[M2], M2, 3);
		cxpose(Array2d[M2], POW2(M2), data + 2, POW2(M)/2, 3, POW2(M2));
		for (i1=4; i1<POW2(M)/2; i1+=4){
			cxpose(data + i1*2, POW2(M)/2, Array2d[M2], POW2(M2), POW2(M2), 4);
			iffts(Array2d[M2], M2, 4);
			cxpose(Array2d[M2], POW2(M2), data + i1*2, POW2(M)/2, 4, POW2(M2));
		}
	}
	riffts(data, M, POW2(M2));
}
else
	riffts(data, M2+M, 1);
}

void rspect2dprod(float *data1, float *data2, float *outdata, long N2, long N1){
// When multiplying a pair of 2d spectra from rfft2d care must be taken to multiply the
// four real values seperately from the complex ones. This routine does it correctly.
// the result can be stored in-place over one of the inputs
/* *data1 = input data array	first spectra */
/* *data2 = input data array	second spectra */
/* N2 = fft size number of rows into rfft2d for both data1 and data2 */
/* N1 = fft size number of columns into rfft2d for both data1 and data2 */
long N = N2 * N1/2;		// number of "complex" numbers in spectra

if( (N2 > 1) && (N1>1)){
	outdata[0] = data1[0] * data2[0];	// multiply the zero freq, zero wavenumber values
	outdata[1] = data1[1] * data2[1];	// multiply the zero freq, nyquest wavenumber values

	cvprod(data1 + 2, data2 + 2, outdata + 2, N/2-1);

	outdata[N] = data1[N] * data2[N];		// multiply the nyquest freq, zero wavenumber values
	outdata[N+1] = data1[N+1] * data2[N+1];	// multiply the nyquest freq, nyquest wavenumber values

	cvprod(data1 + N+2, data2 + N+2, outdata + N+2, N/2-1);
}
else{	// really 1D rfft spectra
	N = N2 * N1;	// one of these is a 1
	if(N>1){
		outdata[0] = data1[0] * data2[0];	// multiply the zero freq values
		outdata[1] = data1[1] * data2[1];	// multiply the nyquest freq values
		cvprod(data1 + 2, data2 + 2, outdata + 2, N/2-1);	// multiply the other positive freq values
	}
	else{
		outdata[0] = data1[0] * data2[0];
	}
}
}