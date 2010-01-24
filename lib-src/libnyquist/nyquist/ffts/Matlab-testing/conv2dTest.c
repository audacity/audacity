/*  A program to test fast 2d real convolution	*/

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"
#include "fft2d.h"

#if macintosh
#include <timer.h>
#endif

#define	BIPRAND(a) (2.0/(RAND_MAX+1.0)*a-1.0)
//#define	BIPRAND(a) round(100*(2.0/(RAND_MAX+1.0)*a-1.0))

void main(){
long 	N = 256;	/* the number of cols in 2d ffts, must be power of 2 */
long 	N2 = 64;	/* the number of rows in 2d ffts, must be power of 2 */
long 	kernSize = 53;	/* kernal cols must be less than N */
long 	kernSize2 = 29;	/* kernal rows must be less than N2*/
long 	dataSize = N-kernSize+1;	/* data cols */
long 	dataSize2 = N2-kernSize2+1;	/* data rows */
float	*a;
float	*b;
long 	i1;
long 	i2;
long 	TheErr;
long		M;
long		M2;

FILE *fdataout;				/* output file */

unsigned int	randseed = 777;
int		rannum;
#if macintosh
	UnsignedWide 		TheTime1;
	Microseconds(&TheTime1);
	randseed = TheTime1.lo;
#endif

printf(" %6d  Byte Floats \n", sizeof(a[0]));
printf(" randseed = %10u\n", randseed);

srand(randseed);
M = roundtol(LOG2(N));
N = POW2(M);
M2 = roundtol(LOG2(N2));
N2 = POW2(M2);

printf("fft size = %6d X%6d,  ", N2, N);

if ((dataSize <= 0)||(dataSize2 <= 0)) TheErr = 22;
else TheErr = 0;

if(!TheErr){
	TheErr = fft2dInit(M2, M);
}

a = (float *) calloc(N2*N,sizeof(float) );	// calloc to zero pad data to fill N to N2
if (a == 0) TheErr = 2;
if(!TheErr){
	b = (float *) calloc(N2*N,sizeof(float) );	// calloc to zero pad data to fill N to N2
	if (b == 0) TheErr = 2;
}
if(!TheErr){
	fdataout = fopen("conv2ddat.c2d", "wb");
	if (fdataout == NULL) TheErr = -50;
}
if(!TheErr){

		/*  write sizes to fdataout */
	fwrite(&dataSize, sizeof(dataSize), 1, fdataout);
	fwrite(&dataSize2, sizeof(dataSize2), 1, fdataout);
	fwrite(&kernSize, sizeof(kernSize), 1, fdataout);
	fwrite(&kernSize2, sizeof(kernSize2), 1, fdataout);
		/*  set up a simple test case and write to fdataout */
	for (i2=0; i2<dataSize2; i2++){
		for (i1=0; i1<dataSize; i1++){
			rannum = rand();
			a[i2*N+i1] = BIPRAND(rannum);
		}
		fwrite(&a[i2*N], dataSize*sizeof(float), 1, fdataout);
	}
	for (i2=0; i2<kernSize2; i2++){
		for (i1=0; i1<kernSize; i1++){
			rannum = rand();
			b[i2*N+i1] = BIPRAND(rannum);
		}
		fwrite(&b[i2*N], kernSize*sizeof(float), 1, fdataout);
	}

	/* fast 2d convolution of zero padded sequences */
	rfft2d(a, M2, M);
	rfft2d(b, M2, M);
	rspect2dprod(a, b, a, N2, N);
	rifft2d(a, M2, M);

	/* write out answer */
	fwrite(a, N2*N*sizeof(float), 1, fdataout);

	fclose(fdataout);

	free(b);
	free(a);
	fft2dFree();
}
else{
	if(TheErr==2)	printf(" out of memory \n");
	else	printf(" error \n");
	fft2dFree();
}
printf(" Done. \n");
return;
}
