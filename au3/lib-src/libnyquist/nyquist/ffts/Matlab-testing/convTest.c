/*  A program to test fast 1d real convolution	*/

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"

#if macintosh
#include <timer.h>
#endif


#define	BIPRAND(a) (2.0/(RAND_MAX+1.0)*a-1.0)
//#define	BIPRAND(a) round(100*(2.0/(RAND_MAX+1.0)*a-1.0))

void main(){
const long N2 = 2;		/* the number ffts to test */
long 	N = 2048;		/* size of FFTs, must be power of 2 */
long 	kernSize = 1003;	/* kernal size must be less than N */
long 	dataSize = N-kernSize+1;	/* data size */
float	*a;
float	*b;
long 	i1;
long 	i2;
long 	TheErr;
long		M;

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

printf("fft size = %6d,  ",  N);

if (dataSize <= 0) TheErr = 22;
else TheErr = 0;

if(!TheErr){
	TheErr = fftInit(M);
}

a = (float *) calloc(N2*N,sizeof(float) );	// calloc to zero pad data to fill N
if (a == 0) TheErr = 2;
if(!TheErr){
	b = (float *) calloc(N2*N,sizeof(float) );	// calloc to zero pad data to fill N
	if (b == 0) TheErr = 2;
}
if(!TheErr){
	fdataout = fopen("convdat.cnv", "wb");
	if (fdataout == NULL) TheErr = -50;
}
if(!TheErr){

		/*  write sizes to fdataout */
	fwrite(&dataSize, sizeof(dataSize), 1, fdataout);
	fwrite(&kernSize, sizeof(kernSize), 1, fdataout);
	fwrite(&N2, sizeof(N2), 1, fdataout);
		/*  set up a simple test case and write to fdataout */
	for (i2=0; i2<N2; i2++){
		for (i1=0; i1<dataSize; i1++){
			rannum = rand();
			a[i2*N+i1] = BIPRAND(rannum);
		}
		fwrite(&a[i2*N], dataSize*sizeof(float), 1, fdataout);
	}
	for (i2=0; i2<N2; i2++){
		for (i1=0; i1<kernSize; i1++){
			rannum = rand();
			b[i2*N+i1] = BIPRAND(rannum);
		}
		fwrite(&b[i2*N], kernSize*sizeof(float), 1, fdataout);
	}


	/* fast convolution of zero padded sequences */
	rffts(a, M, N2);
	rffts(b, M, N2);
	for (i2=0; i2<N2*N; i2+=N){
		rspectprod(&a[i2], &b[i2], &a[i2], N);
	}
	riffts(a, M, N2);

	/* write out answer */
	fwrite(a, N2*N*sizeof(float), 1, fdataout);

	fclose(fdataout);

	free(b);
	free(a);
	fftFree();
}
else{
	if(TheErr==2)	printf(" out of memory \n");
	else	printf(" error \n");
	fftFree();
}
printf(" Done. \n");
return;
}
